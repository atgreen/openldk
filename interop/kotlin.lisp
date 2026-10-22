;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: OPENLDK.KOTLIN; Base: 10 -*-
;;;
;;; Copyright (C) 2026  Anthony Green <green@moxielogic.com>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later WITH Classpath-exception-2.0
;;;
;;; This file is part of OpenLDK.

;;; OpenLDK is free software; you can redistribute it and/or modify it
;;; under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 3, or (at your
;;; option) any later version.

;;; OpenLDK is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; General Public License for more details.

;;; You should have received a copy of the GNU General Public License
;;; along with OpenLDK; see the file COPYING.  If not, please see
;;; <http://www.gnu.org/licenses/>.

;;; Linking this library statically or dynamically with other modules is
;;; making a combined work based on this library.  Thus, the terms and
;;; conditions of the GNU General Public License cover the whole
;;; combination.

;;; As a special exception, the copyright holders of this library give
;;; you permission to link this library with independent modules to
;;; produce an executable, regardless of the license terms of these
;;; independent modules, and to copy and distribute the resulting
;;; executable under terms of your choice, provided that you also
;;; meet, for each linked independent module, the terms and conditions
;;; of the license of that module.  An independent module is a module
;;; which is not derived from or based on this library.  If you modify
;;; this library, you may extend this exception to your version of the
;;; library, but you are not obligated to do so.  If you do not wish
;;; to do so, delete this exception statement from your version.

;;; Kotlin from Common Lisp: the Kotlin compiler running on OpenLDK in the
;;; same SBCL process.  Compile Kotlin source, call its functions, pass Lisp
;;; functions to it, and convert values both ways.
;;;
;;;   (kotlin:startup)
;;;   (kotlin:eval "listOf(1, 2, 3).map { it * 2 }")    => (2 4 6)
;;;   (kotlin:compile "fun fact(n: Int): Long = if (n < 2) 1 else n * fact(n - 1)")
;;;                                                      => "Kotlin1Kt"
;;;   (kotlin:call "Kotlin1Kt" "fact" 20)               => 2432902008176640000
;;;   (kotlin:compile "fun twice(f: (Int) -> Int, x: Int) = f(f(x))")
;;;   (kotlin:call "Kotlin2Kt" "twice" (lambda (x) (* x 3)) 2)   => 18
;;;
;;; Each COMPILE is a source file of its own, compiled into a directory on
;;; the classpath; its top-level functions are static methods of its file
;;; class, returned.  The compiler takes OpenLDK minutes to warm up;
;;; `make sbcl-kotlin` saves an SBCL where it has.
;;;
;;; Values (TO-LISP and FROM-LISP):
;;;
;;;   Kotlin                        Lisp
;;;   Int, Long, Short, Byte        integer
;;;   Double, Float                 double-float, single-float
;;;   String, Char, Boolean         string, character, T and NIL (write :FALSE)
;;;   List, Set (any Collection)    list (FROM-LISP: a List)
;;;   Array                         vector (FROM-LISP of a vector: a List)
;;;   Map                           hash-table, test EQUAL (both ways)
;;;   Pair                          list of two
;;;   null, Unit                    NIL (FROM-LISP of NIL: null)
;;;   functions, other objects      left as is: call a function with FUNCALL
;;;
;;; Lisp functions become Kotlin functions of up to three arguments, (Any?)
;;; -> Any? and so on, for their arity.

(defpackage #:openldk.kotlin
  (:use #:cl #:openldk.interop)
  (:nicknames #:kotlin)
  (:shadow #:eval #:funcall #:compile)
  (:export #:startup
           #:default-classpath
           #:*output-directory*
           #:compile
           #:eval
           #:call
           #:funcall
           #:to-lisp
           #:from-lisp))

(in-package #:openldk.kotlin)

(defvar *started* nil)

(defvar *output-directory* nil
  "The directory COMPILE writes classes to, on the classpath.")

(defvar *source-directory* nil)

(defvar *counter* 0)

(defvar *classes* nil
  "Alist of keyword -> CLOS class name, for TYPEP.")

(defvar *stdlib* nil "The kotlin-stdlib jar compiled code links against.")

(defun %class (key)
  (cdr (assoc key *classes*)))

(defun default-classpath ()
  "The Kotlin compiler's jars, from CLASSPATH, `make sbcl-kotlin`, or
downloads (see FIND-CLASSPATH)."
  (find-classpath "org/jetbrains/kotlin/cli/jvm/K2JVMCompiler"
                  '(("lib/kotlin/kotlin-stdlib-2.2.20.jar" "kotlin-2.2.20/kotlin-stdlib-2.2.20.jar"
                     "https://repo1.maven.org/maven2/org/jetbrains/kotlin/kotlin-stdlib/2.2.20/kotlin-stdlib-2.2.20.jar"
                     "8836ccffd3585fadda9901244b20d42901d2f3cd581058d8434e2ffabcf3a3e7")
                    ("lib/kotlin/kotlin-compiler-embeddable-2.2.20.jar" "kotlin-2.2.20/kotlin-compiler-embeddable-2.2.20.jar"
                     "https://repo1.maven.org/maven2/org/jetbrains/kotlin/kotlin-compiler-embeddable/2.2.20/kotlin-compiler-embeddable-2.2.20.jar"
                     "1c6c3f810bfe6a41abcbc34b3a2ef639f1e3f4aeed3a953a4b0974eea4f41889")
                    ("lib/kotlin/kotlin-reflect-2.2.20.jar" "kotlin-2.2.20/kotlin-reflect-2.2.20.jar"
                     "https://repo1.maven.org/maven2/org/jetbrains/kotlin/kotlin-reflect/2.2.20/kotlin-reflect-2.2.20.jar"
                     "8209083a4a7c4e476d9842b078308fa96a96f07a6bd99f05f3586ee13289ab26")
                    ("lib/kotlin/kotlin-script-runtime-2.2.20.jar" "kotlin-2.2.20/kotlin-script-runtime-2.2.20.jar"
                     "https://repo1.maven.org/maven2/org/jetbrains/kotlin/kotlin-script-runtime/2.2.20/kotlin-script-runtime-2.2.20.jar"
                     "5c8bd5dd7ae1ea2eeb2778fdbeaa19a562184975f6cab8a0bb6779e4190731ae")
                    ("lib/kotlin/kotlin-daemon-embeddable-2.2.20.jar" "kotlin-2.2.20/kotlin-daemon-embeddable-2.2.20.jar"
                     "https://repo1.maven.org/maven2/org/jetbrains/kotlin/kotlin-daemon-embeddable/2.2.20/kotlin-daemon-embeddable-2.2.20.jar"
                     "7c5c8cd2f8beadd3283239bb9ca219a0cafa1af02682b407b181e1179718f2f7")
                    ("lib/kotlin/kotlinx-coroutines-core-jvm-1.8.0.jar" "kotlin-2.2.20/kotlinx-coroutines-core-jvm-1.8.0.jar"
                     "https://repo1.maven.org/maven2/org/jetbrains/kotlinx/kotlinx-coroutines-core-jvm/1.8.0/kotlinx-coroutines-core-jvm-1.8.0.jar"
                     "9860906a1937490bf5f3b06d2f0e10ef451e65b95b269f22daf68a3d1f5065c5")
                    ("lib/kotlin/trove4j-1.0.20200330.jar" "kotlin-2.2.20/trove4j-1.0.20200330.jar"
                     "https://repo1.maven.org/maven2/org/jetbrains/intellij/deps/trove4j/1.0.20200330/trove4j-1.0.20200330.jar"
                     "c5fd725bffab51846bf3c77db1383c60aaaebfe1b7fe2f00d23fe1b7df0a439d")
                    ("lib/kotlin/annotations-13.0.jar" "kotlin-2.2.20/annotations-13.0.jar"
                     "https://repo1.maven.org/maven2/org/jetbrains/annotations/13.0/annotations-13.0.jar"
                     "ace2a10dc8e2d5fd34925ecac03e4988b2c0f851650c94b8cef49ba1bd111478"))))

;;; Lisp functions as Kotlin functions

;; kotlin.jvm.internal.Lambda is the superclass of Kotlin's own lambdas;
;; each class also implements kotlin.jvm.functions.FunctionN.
(defclass lisp-function ()
  ((function :initarg :function :reader lisp-function)))

(defun %install-lisp-functions ()
  (let ((lambda-class (java-class-symbol "kotlin.jvm.internal.Lambda")))
    (dotimes (arity 4)
      (let ((name (intern (format nil "LISP-FUNCTION-~D" arity) :openldk.kotlin))
            (params (loop for i below arity collect (intern (format nil "A~D" i) :openldk.kotlin))))
        (cl:eval `(defclass ,name (lisp-function ,lambda-class
                                   ,(java-class-symbol (format nil "kotlin.jvm.functions.Function~D" arity)))
                    ()))
        (cl:eval `(defmethod ,(intern (format nil "invoke(~{~A~})Ljava/lang/Object;"
                                              (make-list arity :initial-element "Ljava/lang/Object;"))
                                      :openldk)
                      ((self ,name) ,@params)
                    (call-from-java
                     (lambda ()
                       (from-lisp (cl:funcall (lisp-function self)
                                              ,@(mapcar (lambda (p) `(to-lisp ,p)) params)))))))))))

(defun %arity (function)
  (let ((lambda-list (sb-introspect:function-lambda-list function)))
    (or (position-if (lambda (x) (member x lambda-list-keywords)) lambda-list)
        (length lambda-list))))

(defun %kotlin-function (function)
  (let ((arity (%arity function)))
    (unless (<= 0 arity 3)
      (error "A Lisp function becomes a Kotlin function of up to three arguments, not ~A." arity))
    (let ((object (make-instance (intern (format nil "LISP-FUNCTION-~D" arity) :openldk.kotlin)
                                 :function function)))
      ;; Lambda(int arity), the constructor Kotlin's lambdas call.
      (java-call "<init>(I)V" object arity)
      object)))

;;; Startup

(defun startup (&key (classpath (default-classpath)))
  "Start the Kotlin compiler from the jars in CLASSPATH (by default, those
DEFAULT-CLASSPATH finds).  Calling it again does nothing."
  (unless *started*
    (dolist (jar classpath)
      (unless (probe-file jar)
        (error "~A is missing." jar)))
    (let ((base (uiop:ensure-directory-pathname
                 (merge-pathnames (format nil "openldk-kotlin-~D/" (sb-posix:getpid))
                                  (uiop:temporary-directory)))))
      (setf *output-directory* (merge-pathnames "classes/" base)
            *source-directory* (merge-pathnames "src/" base))
      (ensure-directories-exist *output-directory*)
      (ensure-directories-exist *source-directory*))
    ;; Compiled code links against the standard library.
    (setf *stdlib* (or (find-if (lambda (entry) (search "kotlin-stdlib" entry)) classpath)
                       (first classpath)))
    (start-runtime :classpath (append classpath (list (namestring *output-directory*))))
    (setf *classes*
          (loop for (key . name) in '((:boolean . "java.lang.Boolean")
                                      (:character . "java.lang.Character")
                                      (:string . "java.lang.String")
                                      (:double . "java.lang.Double")
                                      (:float . "java.lang.Float")
                                      (:number . "java.lang.Number")
                                      (:big-integer . "java.math.BigInteger")
                                      (:unit . "kotlin.Unit")
                                      (:pair . "kotlin.Pair")
                                      (:map . "java.util.Map")
                                      (:collection . "java.util.Collection"))
                collect (cons key (java-class-symbol name))))
    (%install-lisp-functions)
    (setf *started* t))
  t)

(defun %ensure-started ()
  (unless *started* (startup)))

;;; Values

(defun %descriptor-types (descriptor)
  "The parameter type descriptors of the method DESCRIPTOR, and its return
type's."
  (let ((types '())
        (i 1))
    (loop until (char= (char descriptor i) #\))
          do (let ((start i))
               (loop while (char= (char descriptor i) #\[) do (incf i))
               (if (char= (char descriptor i) #\L)
                   (setf i (1+ (position #\; descriptor :start i)))
                   (incf i))
               (push (subseq descriptor start i) types)))
    (values (nreverse types) (subseq descriptor (1+ i)))))

(defun %argument (value type)
  "VALUE as a Kotlin method parameter of the descriptor TYPE: primitives as
OpenLDK passes them, references through FROM-LISP."
  (case (char type 0)
    ((#\I #\J #\S #\B) (values (round value)))
    ((#\D) (coerce value 'double-float))
    ((#\F) (coerce value 'single-float))
    ((#\Z) (if (and value (not (eq value :false))) 1 0))
    ((#\C) (char-code value))
    (t (from-lisp value))))

(defun %result (value type)
  (case (char type 0)
    ((#\V) nil)
    ((#\Z) (eql value 1))
    ((#\C) (if (characterp value) value (code-char value)))
    ((#\I #\J #\S #\B #\D #\F) value)
    (t (to-lisp value))))

(defun from-lisp (value)
  "The Kotlin value for the Lisp VALUE; see the table at the top of this file."
  (%ensure-started)
  (cond
    ((null value) nil)
    ((eq value t) (java-static-field "java.lang.Boolean" "TRUE"))
    ((eq value :false) (java-static-field "java.lang.Boolean" "FALSE"))
    ((typep value '(signed-byte 32)) (java-static "java.lang.Integer" "valueOf(I)" value))
    ((typep value '(signed-byte 64)) (java-static "java.lang.Long" "valueOf(J)" value))
    ((integerp value) (java-new "java.math.BigInteger" "(Ljava/lang/String;)" (jstring (princ-to-string value))))
    ((typep value 'single-float) (java-static "java.lang.Float" "valueOf(F)" value))
    ((realp value) (java-double value))
    ((stringp value) (jstring value))
    ((characterp value) (java-static "java.lang.Character" "valueOf(C)" (char-code value)))
    ((symbolp value) (jstring (string-downcase (symbol-name value))))
    ((or (consp value) (vectorp value))
     (let ((list (java-new "java.util.ArrayList" "()")))
       (map nil (lambda (element) (java-call "add(Ljava/lang/Object;)" list (from-lisp element))) value)
       list))
    ((hash-table-p value)
     (let ((map (java-new "java.util.LinkedHashMap" "()")))
       (maphash (lambda (k v)
                  (java-call "put(Ljava/lang/Object;Ljava/lang/Object;)" map (from-lisp k) (from-lisp v)))
                value)
       map))
    ((functionp value) (%kotlin-function value))
    (t value)))

(defun to-lisp (object)
  "The Lisp value for the Kotlin OBJECT; see the table at the top of this file."
  (flet ((is (key) (typep object (%class key))))
    (cond
      ((or (null object) (is :unit)) nil)
      ((is :boolean) (eql 1 (java-call "booleanValue()" object)))
      ((is :character)
       (let ((char (java-call "charValue()" object)))
         (if (characterp char) char (code-char char))))
      ((is :string) (lstring object))
      ((is :double) (java-call "doubleValue()" object))
      ((is :float) (java-call "floatValue()" object))
      ((is :big-integer) (parse-integer (lstring (java-call "toString()" object))))
      ((is :number) (java-call "longValue()" object))
      ((is :pair) (list (to-lisp (java-call "getFirst()" object))
                        (to-lisp (java-call "getSecond()" object))))
      ((is :map)
       (let ((table (make-hash-table :test #'equal)))
         (loop with iterator = (java-call "iterator()" (java-call "entrySet()" object))
               while (eql 1 (java-call "hasNext()" iterator))
               do (let ((entry (java-call "next()" iterator)))
                    (setf (gethash (to-lisp (java-call "getKey()" entry)) table)
                          (to-lisp (java-call "getValue()" entry)))))
         table))
      ((is :collection)
       (loop with iterator = (java-call "iterator()" object)
             while (eql 1 (java-call "hasNext()" iterator))
             collect (to-lisp (java-call "next()" iterator))))
      ((typep object 'openldk::java-array) (map 'vector #'to-lisp (java-array-list object)))
      (t object))))

;;; Compiling

(defun compile (source &key name)
  "Compile the Kotlin SOURCE (a string) as a file of its own, NAME.kt (by
default KotlinN.kt), and return the name of the class holding its
top-level functions (\"KotlinNKt\").  Signals FOREIGN-ERROR with the
compiler's messages when it doesn't compile."
  (%ensure-started)
  (let* ((name (or name (format nil "Kotlin~D" (incf *counter*))))
         (file (merge-pathnames (format nil "~A.kt" name) *source-directory*)))
    (with-open-file (out file :direction :output :if-exists :supersede)
      (write-string source out))
    (let* ((messages (java-new "java.io.ByteArrayOutputStream" "()"))
           (code (with-foreign-errors ("Kotlin")
                   (java-call "exec(Ljava/io/PrintStream;[Ljava/lang/String;)"
                              (java-new "org.jetbrains.kotlin.cli.jvm.K2JVMCompiler" "()")
                              (java-new "java.io.PrintStream" "(Ljava/io/OutputStream;)" messages)
                              (make-java-array-of
                               "java.lang.String"
                               (mapcar #'jstring
                                       (list "-no-stdlib" "-no-reflect" "-nowarn"
                                             "-cp" (format nil "~A:~A" *stdlib* (namestring *output-directory*))
                                             "-d" (namestring *output-directory*)
                                             (namestring file))))))))
      (unless (string= "OK" (lstring (java-call "name()" code)))
        (error 'foreign-error :language "Kotlin"
                              :message (string-trim '(#\Newline #\Space)
                                                    (lstring (java-call "toString()" messages)))))
      (format nil "~:@(~A~)~AKt" (subseq name 0 1) (subseq name 1)))))

(defun %static-method (class-name function arity)
  "The full name and descriptor of the static method FUNCTION of ARITY
arguments in the class CLASS-NAME, and the class."
  (let* ((class (java-load class-name))
         (method (find-if (lambda (m)
                            (and (string= function (slot-value m 'openldk::name))
                                 (logtest #x8 (openldk::access-flags m))
                                 (= arity (length (%descriptor-types (slot-value m 'openldk::descriptor))))))
                          (openldk::methods class))))
    (unless method
      (error "Kotlin: no function ~A of ~D argument~:P in ~A" function arity class-name))
    (values method class)))

(defun call (class-name function &rest args)
  "Call the top-level FUNCTION (a string) of the Kotlin file class
CLASS-NAME, as COMPILE returns it, with ARGS; return its value as Lisp data."
  (%ensure-started)
  (multiple-value-bind (method class) (%static-method class-name function (length args))
    (let ((descriptor (slot-value method 'openldk::descriptor)))
      (multiple-value-bind (types return-type) (%descriptor-types descriptor)
        (%result
         (with-foreign-errors ("Kotlin")
           (apply (symbol-function
                   (openldk::static-method-symbol
                    (format nil "~A.~A~A" (openldk::name class) function descriptor)
                    (openldk::ldk-class-package class)))
                  (mapcar #'%argument args types)))
         return-type)))))

(defun eval (expression)
  "Evaluate the Kotlin EXPRESSION (a string, which can be a block of
statements ending in an expression); return its value as Lisp data."
  (call (compile (format nil "fun lispEval(): Any? = run {~%~A~%}~%" expression)) "lispEval"))

(defun funcall (function &rest args)
  "Call the Kotlin function object FUNCTION with ARGS, converted with
FROM-LISP; return the result as Lisp data."
  (%ensure-started)
  (to-lisp (with-foreign-errors ("Kotlin")
             (apply #'java-call
                    (format nil "invoke(~{~A~})Ljava/lang/Object;"
                            (make-list (length args) :initial-element "Ljava/lang/Object;"))
                    function (mapcar #'from-lisp args)))))

;;; #@kotlin{ ... }
;;;
;;; After (openldk.interop:enable-source-syntax), #@kotlin{ ... } holds
;;; Kotlin source -- statements ending in an expression -- and evaluates it
;;; where it appears; $name and $(form) splice in Lisp values, as parameters
;;; typed by their values (Int, Long, Double, String, List<Any?>, ...):
;;;
;;;   (let ((n 4)) #@kotlin{ (1..$n).map { it * it } })     => (1 4 9 16)
;;;   #@kotlin{ $(list "a" "b").joinToString("+") }          => "a+b"
;;;
;;; Each block becomes a Kotlin function, compiled once for each set of
;;; parameter types it is called with.

(defvar *sites* (make-hash-table :test #'equal)
  "(source . parameter types) -> the class of the compiled block.")

(defun %kotlin-type (value)
  "The Kotlin type of a parameter VALUE is passed as."
  (cond
    ((or (eq value t) (eq value :false)) "Boolean")
    ((null value) "Any?")
    ((typep value '(signed-byte 32)) "Int")
    ((typep value '(signed-byte 64)) "Long")
    ((integerp value) "java.math.BigInteger")
    ((typep value 'single-float) "Float")
    ((realp value) "Double")
    ((stringp value) "String")
    ((characterp value) "Char")
    ((or (consp value) (vectorp value)) "List<Any?>")
    ((hash-table-p value) "Map<Any?, Any?>")
    ((functionp value)
     (format nil "(~{~A~^, ~}) -> Any?" (make-list (%arity value) :initial-element "Any?")))
    ((symbolp value) "String")
    (t "Any?")))

(defun %eval-site (source names values)
  (%ensure-started)
  (let* ((types (mapcar #'%kotlin-type values))
         (key (cons source types))
         (class (or (gethash key *sites*)
                    (setf (gethash key *sites*)
                          (compile (format nil "fun lispSite(~{~A~^, ~}): Any? = run {~%~A~%}~%"
                                           (mapcar (lambda (n type) (format nil "~A: ~A" n type))
                                                   names types)
                                           source))))))
    (apply #'call class "lispSite" values)))

(define-source-syntax '("kotlin" "kt")
  (lambda (source names forms)
    `(%eval-site ,source ',names (list ,@forms))))
