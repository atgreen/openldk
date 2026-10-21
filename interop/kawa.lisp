;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: OPENLDK.KAWA; Base: 10 -*-
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
;;;   (kawa:eval '(+ 1 2))                              => 3
;;;   (kawa:define "square" (lambda (x) (* x x)))
;;;   (kawa:eval '(map square '(1 2 3)))                => (1 4 9)
;;;
;;; Values (TO-LISP and FROM-LISP):
;;;
;;;   Kawa                       Lisp
;;;   integer, rational          integer, ratio
;;;   real                       double-float
;;;   string                     string
;;;   character                  character
;;;   symbol                     symbol, upcased, interned in *PACKAGE* (foo <-> FOO)
;;;   keyword (foo:)             keyword (:FOO)
;;;   list, pair, '()            list, cons, NIL
;;;   vector                     simple-vector
;;;   #t, #f                     T, NIL (write :FALSE for #f)
;;;   procedure, other objects   left as is: call a procedure with FUNCALL
;;;
;;; NIL goes to Scheme as '(), which Kawa (like R7RS) treats as true; define
;;; a Lisp predicate with :PREDICATE T to turn its NIL into #f.

(defpackage #:openldk.kawa
  (:use #:cl #:openldk.interop)
  (:nicknames #:kawa)
  (:shadow #:eval #:load #:funcall)
  (:export #:startup
           #:default-classpath
           #:*environment*
           #:eval
           #:eval-string
           #:load
           #:lookup
           #:funcall
           #:define
           #:to-lisp
           #:from-lisp))

(in-package #:openldk.kawa)

(defvar *started* nil)

(defvar *environment* nil
  "The Kawa environment code is evaluated in, once STARTUP has run.")

(defvar *empty* nil "Kawa's '(), gnu.lists.LList.Empty.")
(defvar *true* nil)
(defvar *false* nil)
(defvar *classes* nil
  "Alist of keyword -> CLOS class name, for TYPEP.")

(defun %class (key)
  (cdr (assoc key *classes*)))

(defun default-classpath ()
  "Kawa's jar, as `make kawa` downloads it."
  (list (namestring (asdf:system-relative-pathname "openldk" "lib/kawa-3.1.1.jar"))))

;;; Lisp functions as Scheme procedures

;; ProcedureN's constructor sets up the method handle Kawa's compiled code
;; calls a procedure through, which ends in applyN.
(define-java-subclass lisp-procedure "gnu.mapping.ProcedureN"
    ((function :initarg :function :reader lisp-procedure-function)
     (predicate :initarg :predicate :initform nil :reader lisp-procedure-predicate))
  ("applyN([Ljava/lang/Object;)" (self args)
   (call-from-java
    (lambda ()
      (let ((result (apply (lisp-procedure-function self)
                           (mapcar #'to-lisp (java-array-list args)))))
        (if (and (null result) (lisp-procedure-predicate self))
            *false*
            (from-lisp result)))))))

(defun %lisp-procedure (function name &key predicate)
  (let ((procedure (make-java-subclass-instance 'lisp-procedure
                                                :function function
                                                :predicate predicate)))
    (when name
      (ignore-errors (java-call "setName(Ljava/lang/String;)" procedure (jstring name))))
    procedure))

;;; Startup

(defun startup (&key (classpath (default-classpath)))
  "Start Kawa from its jar in CLASSPATH (by default, the one `make kawa`
downloads).  Calling it again does nothing."
  (unless *started*
    (dolist (jar classpath)
      (unless (probe-file jar)
        (error "~A is missing; run `make kawa` first." jar)))
    (start-runtime :classpath classpath)
    (setf *classes*
          (loop for (key . name) in '((:boolean . "java.lang.Boolean")
                                      (:pair . "gnu.lists.Pair")
                                      (:empty . "gnu.lists.EmptyList")
                                      (:int-num . "gnu.math.IntNum")
                                      (:rat-num . "gnu.math.RatNum")
                                      (:real-num . "gnu.math.RealNum")
                                      (:number . "java.lang.Number")
                                      (:char . "gnu.text.Char")
                                      (:character . "java.lang.Character")
                                      (:char-sequence . "java.lang.CharSequence")
                                      (:symbol . "gnu.mapping.Symbol")
                                      (:keyword . "gnu.expr.Keyword")
                                      (:f-vector . "gnu.lists.FVector")
                                      (:procedure . "gnu.mapping.Procedure"))
                collect (cons key (java-class-symbol name))))
    (with-foreign-errors ("Kawa")
      (let ((scheme (java-static "kawa.standard.Scheme" "getInstance()")))
        (java-static "kawa.standard.Scheme" "registerEnvironment()")
        (java-static "gnu.expr.Language" "setDefaults(Lgnu/expr/Language;)" scheme)
        ;; The user environment inherits the standard one, which is locked:
        ;; definitions go here, and can shadow built-ins (square, say).
        (setf *environment* (java-static "gnu.mapping.Environment" "user()"))))
    (setf *empty* (java-static-field "gnu.lists.LList" "Empty")
          *true* (java-static-field "java.lang.Boolean" "TRUE")
          *false* (java-static-field "java.lang.Boolean" "FALSE"))
    (install-lisp-procedure)
    (setf *started* t))
  t)

(defun %ensure-started ()
  (unless *started* (startup)))

;;; Values

(defun %name (symbol)
  (let ((name (symbol-name symbol)))
    (if (string= name (string-upcase name)) (string-downcase name) name)))

(defun %lisp-name (name)
  (if (string= name (string-downcase name)) (string-upcase name) name))

(defun %symbol (name)
  "The Kawa symbol named NAME (a string)."
  (java-static "gnu.mapping.Symbol" "valueOf(Ljava/lang/String;)" (jstring name)))

(defun %int-num (integer)
  (if (typep integer '(signed-byte 64))
      (java-static "gnu.math.IntNum" "make(J)" integer)
      (java-static "gnu.math.IntNum" "valueOf(Ljava/lang/String;)"
                   (jstring (princ-to-string integer)))))

(defun from-lisp (value)
  "The Kawa value for the Lisp VALUE; see the table at the top of this file."
  (%ensure-started)
  (cond
    ((null value) *empty*)
    ((eq value t) *true*)
    ((eq value :false) *false*)
    ((integerp value) (%int-num value))
    ((rationalp value)
     (java-static "gnu.math.RatNum" "make(Lgnu/math/IntNum;Lgnu/math/IntNum;)"
                  (%int-num (numerator value)) (%int-num (denominator value))))
    ((floatp value) (java-static "gnu.math.DFloNum" "make(D)" (coerce value 'double-float)))
    ((stringp value) (jstring value))
    ((characterp value) (java-static "gnu.text.Char" "make(I)" (char-code value)))
    ((keywordp value)
     (java-static "gnu.expr.Keyword" "make(Ljava/lang/String;)" (jstring (%name value))))
    ((symbolp value) (%symbol (%name value)))
    ((consp value)
     (java-new "gnu.lists.Pair" "(Ljava/lang/Object;Ljava/lang/Object;)"
               (from-lisp (car value)) (from-lisp (cdr value))))
    ((vectorp value)
     (java-new "gnu.lists.FVector" "([Ljava/lang/Object;)"
               (make-java-array-of "java.lang.Object" (map 'list #'from-lisp value))))
    ((functionp value) (%lisp-procedure value nil))
    (t value)))

(defun %string-of (object)
  (lstring (java-call "toString()" object)))

(defun to-lisp (object)
  "The Lisp value for the Kawa OBJECT; see the table at the top of this file."
  (flet ((is (key) (typep object (%class key))))
    (cond
      ((or (null object) (is :empty)) nil)
      ((is :boolean) (eql 1 (java-call "booleanValue()" object)))
      ((is :pair)
       (cons (to-lisp (java-call "getCar()" object))
             (to-lisp (java-call "getCdr()" object))))
      ((is :int-num) (parse-integer (%string-of object)))
      ((is :rat-num)
       (/ (parse-integer (%string-of (java-call "numerator()" object)))
          (parse-integer (%string-of (java-call "denominator()" object)))))
      ((is :real-num) (java-call "doubleValue()" object))
      ((is :number)
       (let ((double (java-call "doubleValue()" object)))
         (if (= double (ffloor double)) (java-call "longValue()" object) double)))
      ((is :char) (code-char (java-call "intValue()" object)))
      ((is :character)
       (let ((char (java-call "charValue()" object)))
         (if (characterp char) char (code-char char))))
      ((is :char-sequence) (%string-of object))
      ((is :keyword) (intern (%lisp-name (lstring (java-call "getName()" object))) :keyword))
      ((is :symbol) (intern (%lisp-name (lstring (java-call "getName()" object)))))
      ((is :f-vector)
       (map 'vector #'to-lisp (java-array-list (java-call "toArray()" object))))
      (t object))))

;;; Evaluating

(defun eval (form)
  "Evaluate FORM, a Lisp s-expression for a Scheme form (symbols are
lowercased), or a string of Scheme source; return the value as Lisp data."
  (%ensure-started)
  (if (stringp form)
      (eval-string form)
      (to-lisp (with-foreign-errors ("Kawa")
                 (java-static "kawa.standard.Scheme"
                              "eval(Ljava/lang/Object;Lgnu/mapping/Environment;)"
                              (from-lisp form) *environment*)))))

(defun eval-string (source)
  "Evaluate the Scheme source in the string SOURCE; return the last value."
  (%ensure-started)
  (to-lisp (with-foreign-errors ("Kawa")
             (java-static "kawa.standard.Scheme"
                          "eval(Ljava/lang/String;Lgnu/mapping/Environment;)"
                          (jstring source) *environment*))))

(defun load (file)
  "Load the Scheme source FILE."
  (eval (list 'load (namestring file)))
  t)

;;; Procedures and definitions

(defun lookup (name)
  "The value Scheme binds to NAME (a string or symbol), unconverted, so a
procedure can be passed to FUNCALL."
  (%ensure-started)
  (with-foreign-errors ("Kawa")
    (java-call "get(Lgnu/mapping/Symbol;)" *environment*
               (%symbol (if (stringp name) name (%name name))))))

(defun funcall (procedure &rest args)
  "Call the Scheme PROCEDURE (a procedure object, or the name of one) with
ARGS, converted with FROM-LISP; return the result as Lisp data.  A name
that isn't bound to a procedure object (Kawa's list is a type) is called as
Scheme code would call it."
  (%ensure-started)
  (let ((object (if (or (stringp procedure) (symbolp procedure))
                    (lookup procedure)
                    procedure)))
    (if (typep object (%class :procedure))
        (to-lisp (with-foreign-errors ("Kawa")
                   (java-call "applyN([Ljava/lang/Object;)" object
                              (make-java-array-of "java.lang.Object" (mapcar #'from-lisp args)))))
        (to-lisp (with-foreign-errors ("Kawa")
                   (java-static "kawa.standard.Scheme"
                                "eval(Ljava/lang/Object;Lgnu/mapping/Environment;)"
                                (java-new "gnu.lists.Pair" "(Ljava/lang/Object;Ljava/lang/Object;)"
                                          (if (stringp procedure)
                                              (%symbol procedure)
                                              (from-lisp procedure))
                                          (from-lisp (mapcar (lambda (arg) (list 'quote arg)) args)))
                                *environment*))))))

(defun define (name value &key predicate)
  "Bind NAME (a string or symbol) in Kawa's environment to VALUE, converted
with FROM-LISP.  A function becomes a Scheme procedure; with PREDICATE, its
NIL result is #f rather than '()."
  (%ensure-started)
  (let* ((scheme-name (if (stringp name) name (%name name)))
         (value (if (functionp value)
                    (%lisp-procedure value scheme-name :predicate predicate)
                    (from-lisp value))))
    (with-foreign-errors ("Kawa")
      (java-call "define(Lgnu/mapping/Symbol;Ljava/lang/Object;Ljava/lang/Object;)"
                 *environment* (%symbol scheme-name) nil value))
    name))
