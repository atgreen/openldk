;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: OPENLDK.GROOVY; Base: 10 -*-
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

;;; Groovy from Common Lisp: Groovy 4 running on OpenLDK in the same SBCL
;;; process.  Evaluate Groovy, call Groovy closures, define Lisp functions
;;; Groovy can call, and convert values both ways.
;;;
;;;   (groovy:startup)
;;;   (groovy:eval "[3, 1, 2].sort().collect { it * 10 }")       => (10 20 30)
;;;   (groovy:define "square" (lambda (x) (* x x)))
;;;   (groovy:eval "(1..4).collect { square(it) }.sum()")        => 30
;;;   (groovy:funcall (groovy:eval "{ a, b -> a * b }") 6 7)     => 42
;;;
;;; Code runs in one GroovyShell, whose binding holds the variables a script
;;; assigns without def (total = 10) and those DEFINE binds.
;;;
;;; Values (TO-LISP and FROM-LISP):
;;;
;;;   Groovy                          Lisp
;;;   Integer, Long, BigInteger       integer
;;;   BigDecimal (1.5)                rational, exactly (FROM-LISP: of a ratio)
;;;   Double, Float                   double-float, single-float
;;;   String, GString, Character      string, character
;;;   Boolean                         T and NIL (write :FALSE)
;;;   List, Range, Set                list (FROM-LISP: an ArrayList)
;;;   Map                             hash-table, test EQUAL (both ways)
;;;   null                            NIL
;;;   closures, other objects         left as is: call a closure with FUNCALL

(defpackage #:openldk.groovy
  (:use #:cl #:openldk.interop)
  (:nicknames #:groovy)
  (:shadow #:eval #:funcall)
  (:export #:startup
           #:default-classpath
           #:*shell*
           #:eval
           #:eval-raw
           #:define
           #:lookup
           #:funcall
           #:to-lisp
           #:from-lisp))

(in-package #:openldk.groovy)

(defvar *started* nil)

(defvar *shell* nil "The groovy.lang.GroovyShell code runs in.")

(defvar *binding* nil "The shell's groovy.lang.Binding.")

(defvar *lisp-function-class* nil
  "The <class> of LispFunction, the Groovy closure class Lisp functions
become.")

(defvar *classes* nil
  "Alist of keyword -> CLOS class name, for TYPEP.")

(defun %class (key)
  (cdr (assoc key *classes*)))

(defun default-classpath ()
  "Groovy's jar, as `make sbcl-groovy` downloads it."
  (list (namestring (asdf:system-relative-pathname "openldk" "lib/groovy/groovy-4.0.27.jar"))))

;;; Lisp functions as Groovy closures
;;;
;;; Groovy calls a closure's doCall methods, which it finds by reflection on
;;; the closure's class: a Java class.  So Groovy compiles the closure class
;;; itself, statically, to pass its arguments to a java.util.function.Function
;;; -- an interface call, which OpenLDK dispatches to the Lisp object below.

(defparameter *lisp-function-source*
  "import groovy.transform.CompileStatic
   @CompileStatic
   class LispFunction extends Closure<Object> {
     final java.util.function.Function<Object[], Object> fn
     LispFunction(java.util.function.Function<Object[], Object> fn) { super(null); this.fn = fn }
     Object doCall(Object... args) { fn.apply(args) }
   }
   LispFunction")

(defclass lisp-function ()
  ((function :initarg :function :reader lisp-function)))

(defun %install-lisp-function ()
  (cl:eval `(defclass lisp-function-object (lisp-function ,(java-class-symbol "java.lang.Object")
                                            ,(java-class-symbol "java.util.function.Function"))
              ()))
  (cl:eval `(defmethod ,(intern "apply(Ljava/lang/Object;)Ljava/lang/Object;" :openldk)
                ((self lisp-function-object) arguments)
              (call-from-java
               (lambda ()
                 (from-lisp (apply (lisp-function self)
                                   (mapcar #'to-lisp (java-array-list arguments)))))))))

(defun %groovy-closure (function)
  (let ((closure (openldk::%make-java-instance (openldk::name *lisp-function-class*)
                                               (openldk::ldk-loader *lisp-function-class*))))
    (java-call "<init>(Ljava/util/function/Function;)V" closure
               (make-instance 'lisp-function-object :function function))
    closure))

;;; Startup

(defun startup (&key (classpath (default-classpath)))
  "Start Groovy from its jar in CLASSPATH (by default, the one `make
sbcl-groovy` downloads).  Calling it again does nothing."
  (unless *started*
    (dolist (jar classpath)
      (unless (probe-file jar)
        (error "~A is missing; run `make sbcl-groovy` first." jar)))
    (start-runtime :classpath classpath)
    (setf *classes*
          (loop for (key . name) in '((:boolean . "java.lang.Boolean")
                                      (:character . "java.lang.Character")
                                      (:char-sequence . "java.lang.CharSequence")
                                      (:double . "java.lang.Double")
                                      (:float . "java.lang.Float")
                                      (:big-integer . "java.math.BigInteger")
                                      (:big-decimal . "java.math.BigDecimal")
                                      (:number . "java.lang.Number")
                                      (:map . "java.util.Map")
                                      (:collection . "java.util.Collection")
                                      (:closure . "groovy.lang.Closure"))
                collect (cons key (java-class-symbol name))))
    (with-foreign-errors ("Groovy")
      (setf *binding* (java-new "groovy.lang.Binding" "()")
            *shell* (java-new "groovy.lang.GroovyShell" "(Lgroovy/lang/Binding;)" *binding*))
      (setf *lisp-function-class*
            (openldk::get-ldk-class-for-java-class (%evaluate *lisp-function-source*))))
    (%install-lisp-function)
    (setf *started* t))
  t)

(defun %ensure-started ()
  (unless *started* (startup)))

;;; Values

(defun %integer (object)
  (parse-integer (lstring (java-call "toString()" object))))

(defun %java-list (elements)
  (let ((list (java-new "java.util.ArrayList" "()")))
    (map nil (lambda (element) (java-call "add(Ljava/lang/Object;)" list (from-lisp element))) elements)
    list))

(defun %terminating-p (ratio)
  "True when RATIO has a finite decimal expansion."
  (let ((d (denominator ratio)))
    (loop while (evenp d) do (setf d (/ d 2)))
    (loop while (zerop (mod d 5)) do (setf d (/ d 5)))
    (= d 1)))

(defun from-lisp (value)
  "The Groovy value for the Lisp VALUE; see the table at the top of this file."
  (%ensure-started)
  (cond
    ((null value) nil)
    ((eq value t) (java-static-field "java.lang.Boolean" "TRUE"))
    ((eq value :false) (java-static-field "java.lang.Boolean" "FALSE"))
    ((typep value '(signed-byte 32)) (java-static "java.lang.Integer" "valueOf(I)" value))
    ((typep value '(signed-byte 64)) (java-static "java.lang.Long" "valueOf(J)" value))
    ((integerp value) (java-new "java.math.BigInteger" "(Ljava/lang/String;)" (jstring (princ-to-string value))))
    ((and (rationalp value) (%terminating-p value))
     (java-call "divide(Ljava/math/BigDecimal;)"
                (java-new "java.math.BigDecimal" "(Ljava/lang/String;)" (jstring (princ-to-string (numerator value))))
                (java-new "java.math.BigDecimal" "(Ljava/lang/String;)" (jstring (princ-to-string (denominator value))))))
    ((typep value 'single-float) (java-static "java.lang.Float" "valueOf(F)" value))
    ((realp value) (java-double value))
    ((stringp value) (jstring value))
    ((characterp value) (java-static "java.lang.Character" "valueOf(C)" (char-code value)))
    ((symbolp value) (jstring (string-downcase (symbol-name value))))
    ((or (consp value) (vectorp value)) (%java-list value))
    ((hash-table-p value)
     (let ((map (java-new "java.util.LinkedHashMap" "()")))
       (maphash (lambda (k v)
                  (java-call "put(Ljava/lang/Object;Ljava/lang/Object;)" map (from-lisp k) (from-lisp v)))
                value)
       map))
    ((functionp value) (%groovy-closure value))
    (t value)))

(defun to-lisp (object)
  "The Lisp value for the Groovy OBJECT; see the table at the top of this file."
  (flet ((is (key) (typep object (%class key))))
    (cond
      ((null object) nil)
      ((is :boolean) (eql 1 (java-call "booleanValue()" object)))
      ((is :character)
       (let ((char (java-call "charValue()" object)))
         (if (characterp char) char (code-char char))))
      ((is :char-sequence) (lstring (java-call "toString()" object)))
      ((is :double) (java-call "doubleValue()" object))
      ((is :float) (java-call "floatValue()" object))
      ((is :big-integer) (%integer object))
      ((is :big-decimal)
       (* (%integer (java-call "unscaledValue()" object))
          (expt 10 (- (java-call "scale()" object)))))
      ((is :number) (java-call "longValue()" object))
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

;;; Evaluating

(defun %evaluate (source)
  (java-call "evaluate(Ljava/lang/String;)" *shell* (jstring source)))

(defun eval-raw (source)
  "Evaluate the Groovy SOURCE (a string) and return its value unconverted,
so a closure can be passed to FUNCALL."
  (%ensure-started)
  (with-foreign-errors ("Groovy")
    (%evaluate source)))

(defun eval (source)
  "Evaluate the Groovy SOURCE (a string); return its value as Lisp data."
  (to-lisp (eval-raw source)))

(defun define (name value)
  "Bind NAME (a string) in the shell's binding to VALUE, converted with
FROM-LISP; a Lisp function becomes a Groovy closure."
  (%ensure-started)
  (java-call "setVariable(Ljava/lang/String;Ljava/lang/Object;)" *binding* (jstring name) (from-lisp value))
  name)

(defun lookup (name)
  "The value the shell's binding gives NAME (a string), unconverted."
  (%ensure-started)
  (with-foreign-errors ("Groovy")
    (java-call "getVariable(Ljava/lang/String;)" *binding* (jstring name))))

(defun funcall (closure &rest args)
  "Call the Groovy CLOSURE (a closure, or the name of a binding holding one)
with ARGS, converted with FROM-LISP; return the result as Lisp data."
  (%ensure-started)
  (let ((closure (if (stringp closure) (lookup closure) closure)))
    (to-lisp (with-foreign-errors ("Groovy")
               (java-call "call([Ljava/lang/Object;)" closure
                          (make-java-array-of "java.lang.Object" (mapcar #'from-lisp args)))))))

;;; #@groovy{ ... }
;;;
;;; After (openldk.interop:enable-source-syntax), #@groovy{ ... } (or
;;; #@gv{ ... }) holds Groovy source and evaluates it where it appears; $name
;;; and $(form) splice in Lisp values, outside Groovy's strings:
;;;
;;;   (let ((xs '(3 1 2))) #@groovy{ $xs.sort().collect { it * 10 } })  => (10 20 30)
;;;
;;; Each block becomes a Groovy closure of the spliced values, compiled once
;;; and then called.

(defvar *sites* (make-hash-table :test #'equal)
  "Source -> the closure the block's source became.")

(defun %eval-site (source names values)
  (%ensure-started)
  (apply #'funcall
         (or (gethash source *sites*)
             (setf (gethash source *sites*)
                   (eval-raw (format nil "{ ~{~A~^, ~} -> ~%~A~%}" names source))))
         values))

(define-source-syntax '("groovy" "gv")
  (lambda (source names forms)
    `(%eval-site ,source ',names (list ,@forms)))
  ;; Groovy's '...' are strings, not characters.
  :reader (lambda (stream)
            (openldk.interop::%read-braced-source-and-names stream :string-quotes '(#\" #\'))))
