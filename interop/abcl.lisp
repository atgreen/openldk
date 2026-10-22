;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: OPENLDK.ABCL; Base: 10 -*-
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

;;; Kawa Scheme from Common Lisp: Kawa running on OpenLDK in the same SBCL
;;; process.  Evaluate Scheme, call Scheme procedures, define Lisp
;;; functions Scheme can call, and convert values both ways.
;;;
;;;   (kawa:startup)
;;; ABCL (Armed Bear Common Lisp) from SBCL: ABCL running on OpenLDK in
;;; the same process.  Evaluate ABCL code, call its functions, define SBCL
;;; functions ABCL can call, and convert values both ways.  ABCL is a
;;; separate Lisp, with its own packages, symbols and heap objects.
;;;
;;;   (abcl:startup)
;;;   (abcl:eval '(lisp-implementation-type))          => "Armed Bear Common Lisp"
;;;   (abcl:define 'square (lambda (x) (* x x)))
;;;   (abcl:eval '(mapcar 'square '(1 2 3)))            => (1 4 9)
;;;
;;; Values (TO-LISP and FROM-LISP):
;;;
;;;   ABCL                       SBCL
;;;   integer, ratio             integer, ratio
;;;   single-float, double-float single-float, double-float
;;;   string, character          string, character
;;;   NIL, T                     NIL, T
;;;   keyword, CL symbol         the same symbol
;;;   other symbol               symbol of the same name: in *PACKAGE* for
;;;                              ABCL's CL-USER, otherwise in the SBCL package
;;;                              of the same name if there is one
;;;   cons, list                 cons, list
;;;   simple-vector              simple-vector
;;;   function, other objects    left as is: call a function with FUNCALL
;;;
;;; Going the other way, a symbol goes to ABCL's package of the same name if
;;; ABCL has one, otherwise to CL-USER.

(defpackage #:openldk.abcl
  (:use #:cl #:openldk.interop)
  (:nicknames #:abcl)
  (:shadow #:eval #:load #:funcall)
  (:export #:startup
           #:default-classpath
           #:eval
           #:eval-string
           #:load
           #:lookup
           #:funcall
           #:define
           #:to-lisp
           #:from-lisp))

(in-package #:openldk.abcl)

(defvar *started* nil)

(defvar *nil* nil "ABCL's NIL.")
(defvar *t* nil "ABCL's T.")
(defvar *package-cl* nil)
(defvar *package-cl-user* nil)
(defvar *package-keyword* nil)
(defvar *call* nil "ABCL's OPENLDK-INTEROP::CALL, which returns errors.")
(defvar *error-marker* nil)
(defvar *classes* nil
  "Alist of keyword -> CLOS class name, for TYPEP.")

(defun %class (key)
  (cdr (assoc key *classes*)))

(defun default-classpath ()
  "ABCL's jar, from CLASSPATH, `make abcl`, or a download (see FIND-CLASSPATH)."
  (find-classpath "org/armedbear/lisp/Lisp"
                  '(("lib/abcl/abcl.jar" "abcl-1.9.2/abcl.jar"
                     "https://repo1.maven.org/maven2/org/abcl/abcl/1.9.2/abcl-1.9.2.jar"
                     "51d0dafac1cea85fc2896b5cead62ce529569c6c02962f854a72b9484a6e45d7"))))

;;; SBCL functions as ABCL functions

;; Primitive's fixed-arity execute methods all call execute(LispObject[]).
(define-java-subclass lisp-function "org.armedbear.lisp.Primitive"
    ((function :initarg :function :reader lisp-function-function))
  ("execute([Lorg/armedbear/lisp/LispObject;)" (self args)
   (call-from-java
    (lambda ()
      (from-lisp (apply (lisp-function-function self)
                        (mapcar #'to-lisp (java-array-list args))))))))

(defun %lisp-function (function name)
  (let ((object (make-java-subclass-instance 'lisp-function :function function)))
    (when name
      (java-call "setLambdaName(Lorg/armedbear/lisp/LispObject;)" object name))
    object))

;;; Startup

(defun startup (&key (classpath (default-classpath)))
  "Start ABCL from its jar in CLASSPATH (by default, found or downloaded
by DEFAULT-CLASSPATH).  Calling it again does nothing."
  (unless *started*
    (dolist (jar classpath)
      (unless (probe-file jar)
        (error "~A is missing." jar)))
    (start-runtime :classpath classpath)
    (with-foreign-errors ("ABCL")
      ;; As ABCL's main does, which loads more of the system (PRINT-OBJECT)
      ;; than createInstance; its REPL runs only if we call run().
      (java-static "org.armedbear.lisp.Interpreter" "createDefaultInstance([Ljava/lang/String;)"
                   (make-java-array-of "java.lang.String"
                                       (list (jstring "--noinform") (jstring "--noinit")))))
    ;; After the interpreter: these classes need ABCL booted to initialize.
    (setf *classes*
          (loop for (key . name) in '((:fixnum . "org.armedbear.lisp.Fixnum")
                                      (:bignum . "org.armedbear.lisp.Bignum")
                                      (:ratio . "org.armedbear.lisp.Ratio")
                                      (:single-float . "org.armedbear.lisp.SingleFloat")
                                      (:double-float . "org.armedbear.lisp.DoubleFloat")
                                      (:string . "org.armedbear.lisp.AbstractString")
                                      (:character . "org.armedbear.lisp.LispCharacter")
                                      (:symbol . "org.armedbear.lisp.Symbol")
                                      (:cons . "org.armedbear.lisp.Cons")
                                      (:simple-vector . "org.armedbear.lisp.SimpleVector"))
                collect (cons key (java-class-symbol name))))
    (setf *nil* (java-static-field "org.armedbear.lisp.Lisp" "NIL")
          *t* (java-static-field "org.armedbear.lisp.Lisp" "T")
          *package-cl* (java-static-field "org.armedbear.lisp.Lisp" "PACKAGE_CL")
          *package-cl-user* (java-static-field "org.armedbear.lisp.Lisp" "PACKAGE_CL_USER")
          *package-keyword* (java-static-field "org.armedbear.lisp.Lisp" "PACKAGE_KEYWORD"))
    (install-lisp-function)
    ;; Helpers in ABCL, in a package of their own.  CALL returns an error
    ;; as (%ERROR . condition), so that ABCL unwinds it itself, restoring
    ;; its special bindings, rather than a Java exception from SBCL code.
    (java-static "org.armedbear.lisp.Lisp" "eval(Lorg/armedbear/lisp/LispObject;)"
                 (%read "(progn
                     (defpackage :openldk-interop (:use :cl))
                     (let ((*package* (find-package :openldk-interop)))
                       (eval (read-from-string
                              \"(progn
                                  (defun call (function &rest args)
                                    (handler-case (apply function args)
                                      (serious-condition (condition) (cons '%error condition))))
                                  (defun eval-string (source)
                                    (with-input-from-string (in source)
                                      (let ((eof (list nil)) (value nil))
                                        (loop (let ((form (read in nil eof)))
                                                (when (eq form eof) (return value))
                                                (setq value (eval form))))))))\"))))"))
    (let ((package (%package "OPENLDK-INTEROP")))
      (setf *call* (java-call "getSymbolFunctionOrDie()" (%intern "CALL" package))
            *error-marker* (%intern "%ERROR" package)))
    (setf *started* t))
  t)

(defun %ensure-started ()
  (unless *started* (startup)))

;;; Values

(defun %package (name)
  "ABCL's package named NAME, or NIL."
  (java-static "org.armedbear.lisp.Packages" "findPackage(Ljava/lang/String;)" (jstring name)))

(defun %intern (name package)
  (java-call "intern(Ljava/lang/String;)" package (jstring name)))

(defun %symbol (symbol)
  "The ABCL symbol for the SBCL SYMBOL."
  (let ((name (symbol-name symbol))
        (package (symbol-package symbol)))
    (cond
      ((keywordp symbol) (%intern name *package-keyword*))
      ((eq package (find-package :cl)) (%intern name *package-cl*))
      (t (%intern name (or (and package (%package (package-name package)))
                           *package-cl-user*))))))

(defun %big-integer (integer)
  (java-new "java.math.BigInteger" "(Ljava/lang/String;)" (jstring (princ-to-string integer))))

(defun %integer (integer)
  (if (typep integer '(signed-byte 32))
      (java-static "org.armedbear.lisp.Fixnum" "getInstance(I)" integer)
      (java-static "org.armedbear.lisp.Bignum" "getInstance(Ljava/lang/String;I)"
                   (jstring (princ-to-string integer)) 10)))

(defun from-lisp (value)
  "The ABCL value for the SBCL VALUE; see the table at the top of this file."
  (%ensure-started)
  (cond
    ((null value) *nil*)
    ((eq value t) *t*)
    ((symbolp value) (%symbol value))
    ((integerp value) (%integer value))
    ((rationalp value)
     (java-new "org.armedbear.lisp.Ratio" "(Ljava/math/BigInteger;Ljava/math/BigInteger;)"
               (%big-integer (numerator value)) (%big-integer (denominator value))))
    ((typep value 'single-float)
     (java-new "org.armedbear.lisp.SingleFloat" "(F)" value))
    ((floatp value)
     (java-static "org.armedbear.lisp.DoubleFloat" "getInstance(D)" (coerce value 'double-float)))
    ((stringp value) (java-new "org.armedbear.lisp.SimpleString" "(Ljava/lang/String;)" (jstring value)))
    ((characterp value)
     (java-static "org.armedbear.lisp.LispCharacter" "getInstance(C)" (char-code value)))
    ((consp value)
     (java-new "org.armedbear.lisp.Cons" "(Lorg/armedbear/lisp/LispObject;Lorg/armedbear/lisp/LispObject;)"
               (from-lisp (car value)) (from-lisp (cdr value))))
    ((vectorp value)
     (java-new "org.armedbear.lisp.SimpleVector" "([Lorg/armedbear/lisp/LispObject;)"
               (make-java-array-of "org.armedbear.lisp.LispObject" (map 'list #'from-lisp value))))
    ((functionp value) (%lisp-function value nil))
    (t value)))

(defun %lisp-symbol (object)
  (let* ((name (lstring (java-call "getName()" object)))
         (abcl-package (java-call "getPackage()" object))
         (package-name (and (not (eq abcl-package *nil*))
                            (lstring (java-call "getName()" abcl-package)))))
    (cond
      ((eq object *nil*) nil)
      ((eq object *t*) t)
      ((equal package-name "KEYWORD") (intern name :keyword))
      ((and (equal package-name "COMMON-LISP")
            (nth-value 1 (find-symbol name :cl)))
       (find-symbol name :cl))
      ((or (null package-name) (equal package-name "COMMON-LISP-USER")
           (not (find-package package-name)))
       (intern name))
      (t (intern name package-name)))))

(defun to-lisp (object)
  "The SBCL value for the ABCL OBJECT; see the table at the top of this file."
  (flet ((is (key) (typep object (%class key))))
    (cond
      ((or (null object) (eq object *nil*)) nil)
      ((is :symbol) (%lisp-symbol object))
      ((is :fixnum) (java-call "intValue()" object))
      ((is :bignum) (parse-integer (lstring (java-call "princToString()" object))))
      ((is :ratio)
       (/ (parse-integer (lstring (java-call "toString()" (java-call "numerator()" object))))
          (parse-integer (lstring (java-call "toString()" (java-call "denominator()" object))))))
      ((is :single-float) (java-call "floatValue()" object))
      ((is :double-float) (java-call "doubleValue()" object))
      ((is :string) (lstring (java-call "getStringValue()" object)))
      ((is :character)
       (let ((char (openldk::%java-slot-value object 'openldk::|value|)))
         (if (characterp char) char (code-char char))))
      ((is :cons)
       (cons (to-lisp (java-call "car()" object)) (to-lisp (java-call "cdr()" object))))
      ((is :simple-vector)
       (coerce (loop for i below (java-call "length()" object)
                     collect (to-lisp (java-call "AREF(I)" object i)))
               'simple-vector))
      (t object))))

;;; Evaluating

(defun %call (function &rest args)
  "Call the ABCL FUNCTION with the ABCL objects ARGS, through CALL; signal
an ABCL error as FOREIGN-ERROR.  Return the ABCL result."
  (let ((result (with-foreign-errors ("ABCL")
                  (java-call "execute([Lorg/armedbear/lisp/LispObject;)" *call*
                             (make-java-array-of "org.armedbear.lisp.LispObject"
                                                 (cons function args))))))
    (if (and (typep result (%class :cons))
             (eq (java-call "car()" result) *error-marker*))
        (let ((condition (java-call "cdr()" result)))
          (error 'foreign-error :language "ABCL"
                                :message (lstring (java-call "princToString()" condition))
                                :object condition))
        result)))

(defun %cl-function (name)
  (java-call "getSymbolFunctionOrDie()" (%intern name *package-cl*)))

(defun %eval (abcl-form)
  (to-lisp (%call (%cl-function "EVAL") abcl-form)))

(defun %read (source)
  "Read SOURCE with ABCL's READ-FROM-STRING, in its current package."
  (let ((function (%cl-function "READ-FROM-STRING"))
        (string (java-new "org.armedbear.lisp.SimpleString" "(Ljava/lang/String;)"
                          (jstring source))))
    (if *call*
        (%call function string)
        ;; While STARTUP defines CALL.
        (with-foreign-errors ("ABCL")
          (java-call "execute(Lorg/armedbear/lisp/LispObject;)" function string)))))

(defun eval (form)
  "Evaluate FORM in ABCL: an SBCL s-expression, converted with FROM-LISP,
or a string of ABCL source.  Return the (primary) value as SBCL data."
  (%ensure-started)
  (if (stringp form)
      (eval-string form)
      (%eval (from-lisp form))))

(defun eval-string (source)
  "Read and evaluate, one by one, the forms in the string SOURCE, with
ABCL's reader in its current package (CL-USER unless a form changes it);
return the last value."
  (%ensure-started)
  (to-lisp (%call (java-call "getSymbolFunctionOrDie()"
                             (%intern "EVAL-STRING" (%package "OPENLDK-INTEROP")))
                  (from-lisp source))))

(defun load (file)
  "Load the ABCL source or fasl FILE."
  (eval (list 'cl:load (namestring file)))
  t)

;;; Functions and definitions

(defun %abcl-symbol (name)
  "The ABCL symbol NAME (a string, read by ABCL's reader, or an SBCL symbol)."
  (if (stringp name) (%read name) (from-lisp name)))

(defun lookup (name)
  "The function ABCL's symbol NAME (a string or symbol) names, unconverted."
  (%ensure-started)
  (%call (%cl-function "FDEFINITION") (%abcl-symbol name)))

(defun funcall (function &rest args)
  "Call the ABCL FUNCTION (a function object, or the name of one) with
ARGS, converted with FROM-LISP; return the result as SBCL data."
  (%ensure-started)
  (let ((function (if (or (stringp function) (symbolp function))
                      (lookup function)
                      function)))
    (to-lisp (apply #'%call function (mapcar #'from-lisp args)))))

(defun define (name value)
  "Make the ABCL symbol NAME (a string or symbol) name VALUE: as a function
if VALUE is an SBCL function, otherwise as a special variable bound to it,
converted with FROM-LISP."
  (%ensure-started)
  (let ((symbol (%abcl-symbol name)))
    (with-foreign-errors ("ABCL")
      (if (functionp value)
          (java-call "setSymbolFunction(Lorg/armedbear/lisp/LispObject;)" symbol
                     (%lisp-function value symbol))
          (progn
            (java-call "setSpecial(Z)" symbol 1)
            (java-call "setSymbolValue(Lorg/armedbear/lisp/LispObject;)" symbol
                       (from-lisp value)))))
    name))

;;; #@abcl{ ... }
;;;
;;; After (openldk.interop:enable-source-syntax), #@abcl{ ... } holds ABCL
;;; source, read by ABCL's reader in its current package, and evaluates it
;;; where it appears; $name and $(form) splice in SBCL values:
;;;
;;;   (let ((xs '(3 1 2))) #@abcl{ (sort (copy-list $xs) #'<) })   => (1 2 3)
;;;
;;; Each block becomes an ABCL function of the spliced values, compiled once
;;; and then called.

(defvar *sites* (make-hash-table :test #'equal)
  "Source -> the ABCL function the block's source became.")

(defun %eval-site (source names values)
  (%ensure-started)
  (apply #'funcall
         (or (gethash source *sites*)
             (setf (gethash source *sites*)
                   (eval-string (format nil "(lambda (~{~A~^ ~})~%~A~%)" names source))))
         values))

(define-source-syntax '("abcl")
  (lambda (source names forms)
    `(%eval-site ,source ',names (list ,@forms)))
  :reader #'openldk.interop::%read-lisp-family-source-and-names)
