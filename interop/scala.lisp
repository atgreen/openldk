;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: OPENLDK.SCALA; Base: 10 -*-
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

;;; Scala from Common Lisp: Scala 2.13 running on OpenLDK in the same SBCL
;;; process.  Evaluate Scala code with the Scala REPL's script engine, call
;;; Scala functions, define Lisp functions Scala can call, and convert
;;; values both ways.
;;;
;;;   (scala:startup)
;;;   (scala:eval "List(1, 2, 3).map(_ * 2)")          => (2 4 6)
;;;   (scala:define "square" (lambda (x) (* x x)))
;;;   (scala:eval "(1 to 4).map(square).toList")        => (1 4 9 16)
;;;   (scala:funcall (scala:eval-raw "(a: Int, b: Int) => a * b") 6 7)  => 42
;;;
;;; Starting the compiler takes OpenLDK minutes, as it compiles the Scala
;;; compiler's Java bytecode; `make sbcl-scala` saves an SBCL where it has.
;;;
;;; Values (TO-LISP and FROM-LISP):
;;;
;;;   Scala                        Lisp
;;;   Int, Long, Short, Byte       integer
;;;   BigInt                       integer (FROM-LISP: beyond a Long)
;;;   Double, Float                double-float, single-float
;;;   String, Char, Boolean        string, character, T and NIL (write :FALSE)
;;;   List, other Seqs, Sets       list (FROM-LISP: a List)
;;;   Vector, Array                vector (FROM-LISP: a Vector)
;;;   Map                          hash-table, test EQUAL (both ways)
;;;   Option                       None is NIL, Some(x) is x
;;;   Unit                         NIL
;;;   TupleN                       list
;;;   functions, other objects     left as is: call a function with FUNCALL
;;;
;;; Lisp functions become Scala functions of type (Any*) => Any for their
;;; arity: (Any) => Any for one argument, (Any, Any) => Any for two, and so
;;; on up to three; DEFINE takes the arity from the function's lambda list.

(defpackage #:openldk.scala
  (:use #:cl #:openldk.interop)
  (:nicknames #:scala)
  (:shadow #:eval #:funcall)
  (:export #:enable-syntax
           #:defun-scala
           #:startup
           #:default-classpath
           #:*engine*
           #:eval
           #:eval-raw
           #:funcall
           #:define
           #:to-lisp
           #:from-lisp))

(in-package #:openldk.scala)

(defvar *started* nil)

(defvar *engine* nil
  "The Scala REPL's script engine (scala.tools.nsc.interpreter.shell.Scripted)
code is evaluated in, once STARTUP has run.")

(defvar *classes* nil
  "Alist of keyword -> CLOS class name, for TYPEP.")

(defvar *nil* nil "Scala's Nil.")
(defvar *none* nil "Scala's None.")
(defvar *unit* nil "Scala's (), scala.runtime.BoxedUnit.UNIT.")

(defvar *binding-counter* 0)

(defvar *bridge* nil
  "A java.util.Map in the REPL, LispBridge.values, through which DEFINE
hands Scala its values.")

(defun %class (key)
  (cdr (assoc key *classes*)))

(defun default-classpath ()
  "Scala's jars, from CLASSPATH, `make sbcl-scala`, or downloads (see FIND-CLASSPATH)."
  (find-classpath "scala/tools/nsc/Main"
                  '(("lib/scala/scala-library-2.13.16.jar" "scala-2.13.16/scala-library-2.13.16.jar"
                     "https://repo1.maven.org/maven2/org/scala-lang/scala-library/2.13.16/scala-library-2.13.16.jar"
                     "1ebb2b6f9e4eb4022497c19b1e1e825019c08514f962aaac197145f88ed730f1")
                    ("lib/scala/scala-reflect-2.13.16.jar" "scala-2.13.16/scala-reflect-2.13.16.jar"
                     "https://repo1.maven.org/maven2/org/scala-lang/scala-reflect/2.13.16/scala-reflect-2.13.16.jar"
                     "fb49ccd9cac7464486ab993cda20a3c1569d8ef26f052e897577ad2a4970fb1d")
                    ("lib/scala/scala-compiler-2.13.16.jar" "scala-2.13.16/scala-compiler-2.13.16.jar"
                     "https://repo1.maven.org/maven2/org/scala-lang/scala-compiler/2.13.16/scala-compiler-2.13.16.jar"
                     "f59982714591e321ba9c087af2c8666e2f5fb92b11a0cef72c2c5e9b342152d3"))))

;;; Lisp functions as Scala functions

;; Scala's AbstractFunctionN implement FunctionN but for apply, so they
;; supply andThen, compose, tupled and so on.
(define-java-subclass lisp-function-0 "scala.runtime.AbstractFunction0"
    ((function :initarg :function :reader lisp-function))
  ("apply()" (self)
   (call-from-java (lambda () (from-lisp (cl:funcall (lisp-function self)))))))

(define-java-subclass lisp-function-1 "scala.runtime.AbstractFunction1"
    ((function :initarg :function :reader lisp-function))
  ("apply(Ljava/lang/Object;)" (self a)
   (call-from-java (lambda () (from-lisp (cl:funcall (lisp-function self) (to-lisp a)))))))

(define-java-subclass lisp-function-2 "scala.runtime.AbstractFunction2"
    ((function :initarg :function :reader lisp-function))
  ("apply(Ljava/lang/Object;Ljava/lang/Object;)" (self a b)
   (call-from-java (lambda ()
                     (from-lisp (cl:funcall (lisp-function self) (to-lisp a) (to-lisp b)))))))

(define-java-subclass lisp-function-3 "scala.runtime.AbstractFunction3"
    ((function :initarg :function :reader lisp-function))
  ("apply(Ljava/lang/Object;Ljava/lang/Object;Ljava/lang/Object;)" (self a b c)
   (call-from-java (lambda ()
                     (from-lisp (cl:funcall (lisp-function self)
                                            (to-lisp a) (to-lisp b) (to-lisp c)))))))

(defun %arity (function)
  "The number of required arguments FUNCTION takes, from its lambda list."
  (let ((lambda-list (sb-introspect:function-lambda-list function)))
    (or (position-if (lambda (x) (member x lambda-list-keywords)) lambda-list)
        (length lambda-list))))

(defun %scala-function (function &optional (arity (%arity function)))
  (unless (<= 0 arity 3)
    (error "A Lisp function becomes a Scala function of up to three arguments, not ~A." arity))
  (make-java-subclass-instance
   (nth arity '(lisp-function-0 lisp-function-1 lisp-function-2 lisp-function-3))
   :function function))

;;; Startup

(defun startup (&key (classpath (default-classpath)))
  "Start Scala's compiler and REPL engine from the jars in CLASSPATH (by
default, the ones `make sbcl-scala` downloads).  Calling it again does
nothing."
  (unless *started*
    (dolist (jar classpath)
      (unless (probe-file jar)
        (error "~A is missing." jar)))
    (start-runtime :classpath classpath)
    (setf *classes*
          (loop for (key . name) in '((:boolean . "java.lang.Boolean")
                                      (:character . "java.lang.Character")
                                      (:double . "java.lang.Double")
                                      (:float . "java.lang.Float")
                                      (:number . "java.lang.Number")
                                      (:string . "java.lang.String")
                                      (:big-int . "scala.math.BigInt")
                                      (:unit . "scala.runtime.BoxedUnit")
                                      (:option . "scala.Option")
                                      (:map . "scala.collection.Map")
                                      (:vector . "scala.collection.immutable.Vector")
                                      (:iterable . "scala.collection.Iterable")
                                      (:product . "scala.Product")
                                      (:function . "scala.Function")
                                      (:function-0 . "scala.Function0")
                                      (:function-1 . "scala.Function1")
                                      (:function-2 . "scala.Function2")
                                      (:function-3 . "scala.Function3"))
                collect (cons key (ignore-errors (java-class-symbol name)))))
    (setf *nil* (java-static-field "scala.collection.immutable.Nil$" "MODULE$")
          *none* (java-static-field "scala.None$" "MODULE$")
          *unit* (java-static-field "scala.runtime.BoxedUnit" "UNIT"))
    (with-foreign-errors ("Scala")
      ;; The REPL compiles against the classpath OpenLDK runs with.
      (let ((settings (java-new "scala.tools.nsc.Settings" "()")))
        (java-call "value_$eq(Ljava/lang/Object;)"
                   (java-call "usejavacp()" settings)
                   (java-static-field "java.lang.Boolean" "TRUE"))
        (setf *engine*
              (java-static "scala.tools.nsc.interpreter.shell.Scripted"
                           "apply(Ljavax/script/ScriptEngineFactory;Lscala/tools/nsc/Settings;Ljava/io/PrintWriter;)"
                           (java-new "scala.tools.nsc.interpreter.shell.Scripted$Factory" "()")
                           settings
                           (java-new "java.io.PrintWriter" "(Ljava/io/OutputStream;Z)"
                                     (java-static-field "java.lang.System" "out") 1)))))
    (dolist (class '(lisp-function-0 lisp-function-1 lisp-function-2 lisp-function-3))
      (cl:funcall (intern (format nil "INSTALL-~A" class) :openldk.scala)))
    (with-foreign-errors ("Scala")
      (%engine-eval "object LispBridge { val values = new java.util.concurrent.ConcurrentHashMap[String, Any]() }")
      (setf *bridge* (%engine-eval "LispBridge.values")))
    (setf *started* t))
  t)

(defun %ensure-started ()
  (unless *started* (startup)))

;;; Values

(defun %iterate (iterable)
  "The elements of the Scala collection ITERABLE, as a list."
  (loop with iterator = (java-call "iterator()" iterable)
        while (eql 1 (java-call "hasNext()" iterator))
        collect (java-call "next()" iterator)))

(defun %big-int (integer)
  (java-static "scala.math.BigInt" "apply(Ljava/lang/String;)" (jstring (princ-to-string integer))))

(defun %list (elements)
  "A Scala List of the (already converted) ELEMENTS."
  (let ((list *nil*))
    (dolist (element (reverse elements) list)
      (setf list (java-new "scala.collection.immutable.$colon$colon"
                           "(Ljava/lang/Object;Lscala/collection/immutable/List;)"
                           element list)))))

(defun %java-list (elements)
  (let ((list (java-new "java.util.ArrayList" "()")))
    (dolist (element elements list)
      (java-call "add(Ljava/lang/Object;)" list element))))

(defun from-lisp (value)
  "The Scala value for the Lisp VALUE; see the table at the top of this file."
  (%ensure-started)
  (cond
    ((null value) *nil*)
    ((eq value t) (java-static-field "java.lang.Boolean" "TRUE"))
    ((eq value :false) (java-static-field "java.lang.Boolean" "FALSE"))
    ((typep value '(signed-byte 32)) (java-static "java.lang.Integer" "valueOf(I)" value))
    ((typep value '(signed-byte 64)) (java-static "java.lang.Long" "valueOf(J)" value))
    ((integerp value) (%big-int value))
    ((rationalp value) (java-double value))
    ((typep value 'single-float) (java-static "java.lang.Float" "valueOf(F)" value))
    ((floatp value) (java-double value))
    ((stringp value) (jstring value))
    ((characterp value) (java-static "java.lang.Character" "valueOf(C)" (char-code value)))
    ((symbolp value) (jstring (string-downcase (symbol-name value))))
    ((consp value) (%list (mapcar #'from-lisp value)))
    ((vectorp value)
     (java-call "toVector()"
                (java-static "scala.jdk.javaapi.CollectionConverters" "asScala(Ljava/util/List;)"
                             (%java-list (map 'list #'from-lisp value)))))
    ((hash-table-p value)
     (java-static "scala.collection.immutable.Map$" "from(Lscala/collection/IterableOnce;)"
                  (%list (loop for k being the hash-keys of value using (hash-value v)
                               collect (java-new "scala.Tuple2" "(Ljava/lang/Object;Ljava/lang/Object;)"
                                                 (from-lisp k) (from-lisp v))))))
    ((typep value 'foreign-function) (foreign-function-object value))
    ((functionp value) (%scala-function value))
    (t value)))

(defun to-lisp (object)
  "The Lisp value for the Scala OBJECT; see the table at the top of this file."
  (flet ((is (key)
           (let ((class (%class key)))
             (and class (typep object class)))))
    (cond
      ((or (null object) (is :unit)) nil)
      ((is :boolean) (eql 1 (java-call "booleanValue()" object)))
      ((is :character)
       (let ((char (java-call "charValue()" object)))
         (if (characterp char) char (code-char char))))
      ((is :string) (lstring object))
      ((is :double) (java-call "doubleValue()" object))
      ((is :float) (java-call "floatValue()" object))
      ((is :big-int) (parse-integer (lstring (java-call "toString()" object))))
      ((is :number) (java-call "longValue()" object))
      ((is :option)
       (if (eql 1 (java-call "isEmpty()" object)) nil (to-lisp (java-call "get()" object))))
      ((is :map)
       (let ((table (make-hash-table :test #'equal)))
         (dolist (entry (%iterate object) table)
           (setf (gethash (to-lisp (java-call "_1()" entry)) table)
                 (to-lisp (java-call "_2()" entry))))))
      ((is :vector) (map 'vector #'to-lisp (%iterate object)))
      ((is :iterable) (mapcar #'to-lisp (%iterate object)))
      ((typep object 'openldk::java-array) (map 'vector #'to-lisp (java-array-list object)))
      ((and (is :product) (%tuple-p object))
       (loop for i below (java-call "productArity()" object)
             collect (to-lisp (java-call "productElement(I)" object i))))
      ((or (is :function-0) (is :function-1) (is :function-2) (is :function-3)) (make-foreign-function object "Scala" #'funcall))
      (t object))))

(defun %tuple-p (object)
  (let ((name (symbol-name (class-name (class-of object)))))
    (and (> (length name) 11) (string= "scala/Tuple" name :end2 11))))

;;; Evaluating

(defun %engine-eval (source)
  (java-call "eval(Ljava/lang/String;)" *engine* (jstring source)))

(defun eval-raw (source)
  "Evaluate the Scala code SOURCE (a string) and return its value
unconverted, so a function can be passed to FUNCALL."
  (%ensure-started)
  (with-foreign-errors ("Scala")
    (%engine-eval source)))

(defun eval (source)
  "Evaluate the Scala code SOURCE (a string); return its value as Lisp data."
  (to-lisp (eval-raw source)))

;;; Functions and definitions

(defun funcall (function &rest args)
  "Call the Scala FUNCTION (a FunctionN object, or the name of a value the
REPL binds to one) with ARGS, converted with FROM-LISP; return the result
as Lisp data."
  (%ensure-started)
  (let ((object (if (stringp function) (eval-raw function) (foreign-object function)))
        (args (mapcar #'from-lisp args)))
    (to-lisp
     (with-foreign-errors ("Scala")
       (apply #'java-call
              (format nil "apply(~{~A~})" (make-list (length args) :initial-element "Ljava/lang/Object;"))
              object args)))))

(defun define (name value &key type arity)
  "Bind NAME (a string) in the Scala REPL to VALUE, converted with FROM-LISP,
as a val of TYPE (a string of Scala; by default Any, or (Any, ...) => Any
for a function).  A function becomes a Scala function of ARITY arguments,
by default the number its lambda list requires."
  (%ensure-started)
  (let* ((arity (and (and (functionp value) (not (typep value 'foreign-function))) (or arity (%arity value))))
         (object (if arity (%scala-function value arity) (from-lisp value)))
         (type (or type
                   (if arity
                       (format nil "(~{~A~^, ~}) => Any" (make-list arity :initial-element "Any"))
                       "Any")))
         (key (format nil "binding~A" (incf *binding-counter*))))
    (with-foreign-errors ("Scala")
      (java-call "put(Ljava/lang/Object;Ljava/lang/Object;)" *bridge* (jstring key) object)
      (%engine-eval (format nil "val ~A = LispBridge.values.remove(\"~A\").asInstanceOf[~A]"
                            name key type)))
    name))

;;; #@scala{ ... }
;;;
;;; After (openldk.interop:enable-source-syntax), #@scala{ ... } holds Scala
;;; source and evaluates it where it appears; $name and $(form) splice in
;;; Lisp values, as parameters typed by their values (Int, Long, Double,
;;; String, List[Any], ...), so they need no casts:
;;;
;;;   (let ((xs '(5 3 8))) #@scala{ $xs.map { case n: Int => n * 2 }.sum })  => 32
;;;   #@scala{ (1 to $(+ 2 3)).toList }                                 => (1 2 3 4 5)
;;;
;;; Each block becomes a Scala method, compiled once for each set of
;;; parameter types it is called with.

(defvar *sites* (make-hash-table :test #'equal)
  "(source . parameter types) -> the compiled block, a Scala function.")

(defvar *site-counter* 0)

(defun %scala-type (value)
  "The Scala type FROM-LISP gives VALUE."
  (cond
    ((or (eq value t) (eq value :false)) "Boolean")
    ((typep value '(signed-byte 32)) "Int")
    ((typep value '(signed-byte 64)) "Long")
    ((integerp value) "BigInt")
    ((typep value 'single-float) "Float")
    ((realp value) "Double")
    ((stringp value) "String")
    ((characterp value) "Char")
    ((listp value) "List[Any]")
    ((vectorp value) "Vector[Any]")
    ((hash-table-p value) "Map[Any, Any]")
    ((typep value 'foreign-function) "Any")
    ((functionp value)
     (format nil "(~{~A~^, ~}) => Any" (make-list (%arity value) :initial-element "Any")))
    ((symbolp value) "String")
    (t "Any")))

(defun %eval-site (source names values)
  (%ensure-started)
  (let* ((types (mapcar #'%scala-type values))
         (key (cons source types))
         (function
           (or (gethash key *sites*)
               (setf (gethash key *sites*)
                     (let ((name (format nil "lispSite~D" (incf *site-counter*))))
                       (with-foreign-errors ("Scala")
                         (%engine-eval (format nil "def ~A(~{~A~^, ~}): Any = {~%~A~%}" name
                                               (mapcar (lambda (n type) (format nil "~A: ~A" n type))
                                                       names types)
                                               source))
                         (%engine-eval (format nil "~A _" name))))))))
    (to-lisp
     (with-foreign-errors ("Scala")
       (apply #'java-call
              (format nil "apply(~{~A~})" (make-list (length values) :initial-element "Ljava/lang/Object;"))
              function (mapcar #'from-lisp values))))))

(defparameter *static-scala-types*
  '(((signed-byte 32) . "Int") ((signed-byte 64) . "Long") (fixnum . "Long") (integer . "BigInt")
    (double-float . "Double") (single-float . "Float") (string . "String") (character . "Char")
    (boolean . "Boolean") (list . "List[Any]") (vector . "Vector[Any]") (hash-table . "Map[Any, Any]")))

(defparameter *scala-type-descriptors*
  '(("Int" . "I") ("Long" . "J") ("Double" . "D") ("Float" . "F") ("Char" . "C") ("Boolean" . "Z")
    ("String" . "Ljava/lang/String;") ("BigInt" . "Lscala/math/BigInt;")
    ("List[Any]" . "Lscala/collection/immutable/List;") ("Vector[Any]" . "Lscala/collection/immutable/Vector;")
    ("Map[Any, Any]" . "Lscala/collection/immutable/Map;")))

(defun %scala-argument (value type)
  "VALUE as a parameter of the Scala TYPE: primitives as OpenLDK passes
them, references through FROM-LISP."
  (cond ((member type '("Int" "Long") :test #'string=) (values (round value)))
        ((string= type "Double") (coerce value 'double-float))
        ((string= type "Float") (coerce value 'single-float))
        ((string= type "Char") (char-code value))
        ((string= type "Boolean") (if (and value (not (eq value :false))) 1 0))
        (t (from-lisp value))))

(defmacro %scala-site (source names &rest forms)
  "A #@scala{ } block: precompiled with scalac if its splices' types are
static and Scala is running now, else compiled the first time it runs."
  (let ((types (mapcar (lambda (form) (openldk.interop::%static-splice-type form *static-scala-types*)) forms)))
    (if (and *started* (every #'identity types))
        (let* ((name (openldk.interop::%site-name "LispSite" source types))
               (base (uiop:ensure-directory-pathname
                      (merge-pathnames (format nil "openldk-scala-~D/~A/" (sb-posix:getpid) name)
                                       (uiop:temporary-directory))))
               (directory (merge-pathnames "classes/" base))
               (file (merge-pathnames (format nil "~A.scala" name) base)))
          (uiop:delete-directory-tree base :validate t :if-does-not-exist :ignore)
          (ensure-directories-exist directory)
          (with-open-file (out file :direction :output :if-exists :supersede)
            (format out "object ~A {~%  def lispSite(~{~A~^, ~}): Any = {~%~A~%  }~%}~%" name
                    (mapcar (lambda (n type) (format nil "~A: ~A" n type)) names types)
                    source))
          (compile-scala-files (list file) directory)
          `(%precompiled-site ,name
                              ,(format nil "lispSite(~{~A~})Ljava/lang/Object;"
                                       (mapcar (lambda (type) (cdr (assoc type *scala-type-descriptors* :test #'string=)))
                                               types))
                              ',types
                              ',(openldk.interop::%class-files-in directory)
                              (list ,@forms)))
        `(%eval-site ,source ',names (list ,@forms)))))

(defun %precompiled-site (class method types classes values)
  (openldk.interop::%define-precompiled-classes classes)
  (to-lisp (with-foreign-errors ("Scala")
             (apply (symbol-function
                     (openldk::static-method-symbol
                      (format nil "~A.~A" class method)
                      (openldk::loader-package openldk::*app-ldk-class-loader*)))
                    (mapcar #'%scala-argument values types)))))

(define-source-syntax '("scala" "sc")
  (lambda (source names forms)
    `(%scala-site ,source ,names ,@forms)))

;;; (:scala-file "name") ASDF components: compiled with scalac

(defun compile-scala-files (sources output-directory &key (classpath ""))
  "Compile the Scala SOURCES (pathnames) into OUTPUT-DIRECTORY against
CLASSPATH (a colon-separated string) and the classpath OpenLDK runs with;
signal FOREIGN-ERROR if they don't compile."
  (%ensure-started)
  (unless (eql 1 (with-foreign-errors ("Scala")
                   (java-static "scala.tools.nsc.Main" "process([Ljava/lang/String;)Z"
                                (make-java-array-of
                                 "java.lang.String"
                                 (mapcar #'jstring
                                         (append (list "-usejavacp" "-nowarn"
                                                       "-classpath" classpath
                                                       "-d" (namestring output-directory))
                                                 (mapcar #'namestring sources)))))))
    (error 'foreign-error :language "Scala"
                          :message (format nil "~{~A~^, ~} did not compile (scalac's messages are above)"
                                           (mapcar #'namestring sources))))
  output-directory)

(openldk.interop::define-foreign-file-type scala-file openldk.interop::compiled-foreign-file "scala"
  (openldk.interop:compile-foreign-file (component source output)
    (declare (ignore component))
    (compile-scala-files (list source) output :classpath (openldk.interop::%compiled-classpath))))

;;; Definitions with typed arguments and a directly embedded native body.

(defun enable-syntax (&optional (readtable *readtable*))
  "Enable source blocks and DEFUN-SCALA native bodies in READTABLE."
  (enable-source-syntax readtable))

(defmacro defun-scala (name parameters source &optional captures forms return-type)
  "Define NAME with (NAME TYPE) parameters and a native body, compiled at load time."
  (openldk.interop::%expand-typed-function-definition
   name parameters source captures forms return-type '%make-native-function))

(openldk.interop::%register-function-syntax 'defun-scala :scala)

(defun %make-native-function (name names types source captures values return-type)
  (let* ((declarations (mapcar (lambda (n type) (format nil "~A: ~A" n type)) names types))
         (definition (format nil "{ def ~A(~{~A~^, ~})~A = {~%~A~%}; ~A _ }"
                             name declarations
                             (if return-type (format nil ": ~A" return-type) "") source name))
         (factory (if captures
                      (format nil "(~{~A~^, ~}) => ~A"
                              (mapcar (lambda (n v) (format nil "~A: ~A" n (%scala-type v)))
                                      captures values)
                              definition)
                      definition)))
    (let ((function (eval-raw factory)))
      (when captures
        (setf function
              (with-foreign-errors ("Scala")
                (apply #'java-call
                       (format nil "apply(~{~A~})"
                               (make-list (length values) :initial-element "Ljava/lang/Object;"))
                       function (mapcar #'from-lisp values)))))
      (make-foreign-function
       function "Scala"
       (lambda (object &rest args)
         (to-lisp
          (with-foreign-errors ("Scala")
            (apply #'java-call
                   (format nil "apply(~{~A~})"
                           (make-list (length types) :initial-element "Ljava/lang/Object;"))
                   object (mapcar #'%native-argument args types)))))))))

(defun %native-argument (value type)
  "Box a primitive according to its declared type, independent of VALUE's size."
  (let ((entry (assoc type '(("Int" "java.lang.Integer" "I")
                              ("Long" "java.lang.Long" "J")
                              ("Short" "java.lang.Short" "S")
                              ("Byte" "java.lang.Byte" "B")
                              ("Double" "java.lang.Double" "D")
                              ("Float" "java.lang.Float" "F")
                              ("Char" "java.lang.Character" "C")
                              ("Boolean" "java.lang.Boolean" "Z"))
                      :test #'string=)))
    (if entry
        (java-static (second entry) (format nil "valueOf(~A)" (third entry))
                     (if (member type '("Short" "Byte") :test #'string=)
                         (round value)
                         (%scala-argument value type)))
        (from-lisp value))))
