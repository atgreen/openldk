;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: OPENLDK.CLOJURE; Base: 10 -*-
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

;;; Clojure from Common Lisp: Clojure running on OpenLDK in the same SBCL
;;; process.  Evaluate Clojure, call Clojure functions, define Lisp
;;; functions Clojure can call, and convert values both ways.
;;;
;;;   (clojure:startup)                       ; minutes cold; see docs/INTEROP.md
;;;   (clojure:eval '(+ 1 2))                 => 3
;;;   (clojure:eval '(map inc #(1 2 3)))      => (2 3 4)
;;;   (clojure:define "square" (lambda (x) (* x x)))
;;;   (clojure:eval '(mapv square (range 4))) => #(0 1 4 9)
;;;
;;; Values (TO-LISP and FROM-LISP):
;;;
;;;   Clojure                    Lisp
;;;   nil                        NIL
;;;   true, false                T, NIL (write :FALSE for false)
;;;   long, BigInt, Ratio        integer, ratio
;;;   double                     double-float
;;;   string, character          string, character
;;;   keyword                    keyword (:foo <-> :FOO)
;;;   symbol                     symbol, upcased, interned in *PACKAGE* (foo <-> FOO)
;;;   list, seq                  list
;;;   vector                     simple-vector
;;;   map                        hash-table (EQUAL); Lisp hash tables go to maps
;;;   set                        list
;;;   function, var, other       left as is: call a function with FUNCALL
;;;
;;; A lazy seq is realized when converted, so don't convert an infinite one.

(defpackage #:openldk.clojure
  (:use #:cl #:openldk.interop)
  (:nicknames #:clojure)
  (:shadow #:eval #:load #:funcall #:require)
  (:export #:startup
           #:default-classpath
           #:eval
           #:eval-string
           #:load
           #:require
           #:lookup
           #:funcall
           #:define
           #:ex-data
           #:enable-syntax
           #:to-lisp
           #:from-lisp))

(in-package #:openldk.clojure)

(defvar *started* nil)

;; Clojure functions STARTUP looks up, and the classes TO-LISP checks for.
(defvar *eval-fn* nil)
(defvar *load-string-fn* nil)
(defvar *resolve-fn* nil)
(defvar *intern-fn* nil)
(defvar *require-fn* nil)
(defvar *ex-data-fn* nil)
(defvar *classes* nil
  "Alist of keyword -> CLOS class name, for TYPEP.")

(defun %class (key)
  (cdr (assoc key *classes*)))

(defun default-classpath ()
  "Clojure's jars, from CLASSPATH, `make clojure`, or downloads (see FIND-CLASSPATH)."
  (find-classpath "clojure/lang/RT"
                  '(("lib/clojure/clojure-1.12.4.jar" "clojure-1.12.4/clojure-1.12.4.jar"
                     "https://repo1.maven.org/maven2/org/clojure/clojure/1.12.4/clojure-1.12.4.jar"
                     "4b81e9ba6da38c45d9cc58023c674062b8c9f0714f33ff00ded22e6a949da177")
                    ("lib/clojure/spec.alpha-0.5.238.jar" "clojure-1.12.4/spec.alpha-0.5.238.jar"
                     "https://repo1.maven.org/maven2/org/clojure/spec.alpha/0.5.238/spec.alpha-0.5.238.jar"
                     "94cd99b6ea639641f37af4860a643b6ed399ee5a8be5d717cff0b663c8d75077")
                    ("lib/clojure/core.specs.alpha-0.4.74.jar" "clojure-1.12.4/core.specs.alpha-0.4.74.jar"
                     "https://repo1.maven.org/maven2/org/clojure/core.specs.alpha/0.4.74/core.specs.alpha-0.4.74.jar"
                     "eb73ac08cf49ba840c88ba67beef11336ca554333d9408808d78946e0feb9ddb"))))

(defun %var (namespace name)
  (java-static "clojure.java.api.Clojure" "var(Ljava/lang/Object;Ljava/lang/Object;)"
               (jstring namespace) (jstring name)))

(defun %invoke (fn &rest args)
  "Call the Clojure function FN (an IFn) with Clojure values ARGS."
  (with-foreign-errors ("Clojure")
    (java-call "applyTo(Lclojure/lang/ISeq;)" fn (%seq-of args))))

;;; Lisp functions as Clojure functions

(defmacro %define-lisp-fn-class ()
  "LISP-FN extends clojure.lang.AFn; each invoke arity, and applyTo for
more arguments, calls the Lisp function."
  (flet ((invoke (arity)
           (let ((args (loop for i below arity collect (intern (format nil "A~D" i)))))
             `(,(format nil "invoke(~{~A~})" (make-list arity :initial-element "Ljava/lang/Object;"))
               (self ,@args)
               (%call-lisp-fn self (list ,@args))))))
    `(define-java-subclass lisp-fn "clojure.lang.AFn"
         ((function :initarg :function :reader lisp-fn-function))
       ;; IFn's fixed arities go up to 20; more arguments come through applyTo.
       ,@(loop for arity from 0 to 20 collect (invoke arity))
       ("applyTo(Lclojure/lang/ISeq;)" (self args)
        (%call-lisp-fn self (%seq-list args))))))

(defun %call-lisp-fn (self clojure-args)
  (call-from-java
   (lambda ()
     (from-lisp (apply (lisp-fn-function self) (mapcar #'to-lisp clojure-args))))))

(%define-lisp-fn-class)

;;; Startup

(defun startup (&key (classpath (default-classpath)))
  "Start Clojure from the jars in CLASSPATH (by default, found or
downloaded by DEFAULT-CLASSPATH).  Loading clojure.core takes minutes cold;
`make sbcl-clojure` builds an SBCL with Clojure already started.  Calling
it again does nothing."
  (unless *started*
    (dolist (jar classpath)
      (unless (probe-file jar)
        (error "~A is missing." jar)))
    (start-runtime :classpath classpath)
    (setf *classes*
          (loop for (key . name) in '((:boolean . "java.lang.Boolean")
                                      (:long . "java.lang.Long")
                                      (:integer . "java.lang.Integer")
                                      (:short . "java.lang.Short")
                                      (:byte . "java.lang.Byte")
                                      (:double . "java.lang.Double")
                                      (:float . "java.lang.Float")
                                      (:big-integer . "java.math.BigInteger")
                                      (:big-decimal . "java.math.BigDecimal")
                                      (:string . "java.lang.String")
                                      (:character . "java.lang.Character")
                                      (:big-int . "clojure.lang.BigInt")
                                      (:ratio . "clojure.lang.Ratio")
                                      (:keyword . "clojure.lang.Keyword")
                                      (:symbol . "clojure.lang.Symbol")
                                      (:vector . "clojure.lang.IPersistentVector")
                                      (:map . "clojure.lang.IPersistentMap")
                                      (:set . "clojure.lang.IPersistentSet")
                                      (:sequential . "clojure.lang.Sequential")
                                      (:seq . "clojure.lang.ISeq")
                                      (:fn . "clojure.lang.Fn"))
                collect (cons key (java-class-symbol name))))
    (install-lisp-fn)
    ;; Loading clojure.java.api.Clojure initializes the runtime (RT),
    ;; which loads clojure.core.
    (with-foreign-errors ("Clojure")
      (setf *eval-fn* (%var "clojure.core" "eval")
            *load-string-fn* (%var "clojure.core" "load-string")
            *resolve-fn* (%var "clojure.core" "resolve")
            *intern-fn* (%var "clojure.core" "intern")
            *require-fn* (%var "clojure.core" "require")
            *ex-data-fn* (%var "clojure.core" "ex-data")))
    ;; Evaluate in the user namespace: *ns*'s root binding is clojure.core,
    ;; and there is no REPL to bind it.
    (setf *started* t)
    (eval-string "(.bindRoot #'clojure.core/*ns* (clojure.core/create-ns 'user))
                  (clojure.core/refer-clojure)"))
  t)

(defun %ensure-started ()
  (unless *started* (startup)))

;;; Values

(defun %name (symbol)
  "The Clojure name for the Lisp SYMBOL: lowercased if all upper case."
  (let ((name (symbol-name symbol)))
    (if (string= name (string-upcase name)) (string-downcase name) name)))

(defun %lisp-name (name)
  (if (string= name (string-downcase name)) (string-upcase name) name))

(defun %object-array (list)
  (make-java-array-of "java.lang.Object" list))

(defun %seq-of (list)
  "A Clojure seq of the Clojure values in LIST, or nil if it is empty."
  (when list
    (java-static "clojure.lang.RT" "seq(Ljava/lang/Object;)" (%object-array list))))

(defun %seq-list (seqable)
  "The elements of the Clojure SEQABLE, as a Lisp list (unconverted)."
  (loop for s = (java-static "clojure.lang.RT" "seq(Ljava/lang/Object;)" seqable)
          then (java-call "next()" s)
        while s
        collect (java-call "first()" s)))

(defun %big-integer (integer)
  (java-new "java.math.BigInteger" "(Ljava/lang/String;)" (jstring (princ-to-string integer))))

(defun from-lisp (value)
  "The Clojure value for the Lisp VALUE; see the table at the top of this file."
  (%ensure-started)
  (cond
    ((null value) nil)
    ((eq value t) (java-static-field "java.lang.Boolean" "TRUE"))
    ((eq value :false) (java-static-field "java.lang.Boolean" "FALSE"))
    ((integerp value)
     (if (typep value '(signed-byte 64))
         (java-static "java.lang.Long" "valueOf(J)" value)
         (java-static "clojure.lang.BigInt" "fromBigInteger(Ljava/math/BigInteger;)"
                      (%big-integer value))))
    ((rationalp value)
     (java-new "clojure.lang.Ratio" "(Ljava/math/BigInteger;Ljava/math/BigInteger;)"
               (%big-integer (numerator value)) (%big-integer (denominator value))))
    ((floatp value) (java-double value))
    ((stringp value) (jstring value))
    ((characterp value) (java-static "java.lang.Character" "valueOf(C)" (char-code value)))
    ((keywordp value)
     (java-static "clojure.lang.Keyword" "intern(Ljava/lang/String;)" (jstring (%name value))))
    ((symbolp value)
     (java-static "clojure.lang.Symbol" "intern(Ljava/lang/String;)" (jstring (%name value))))
    ((consp value)
     (unless (listp (cdr (last value)))
       (error "Clojure has no dotted pairs: ~S" value))
     (java-static "clojure.lang.PersistentList" "create(Ljava/util/List;)"
                  (java-static "java.util.Arrays" "asList([Ljava/lang/Object;)"
                               (%object-array (mapcar #'from-lisp value)))))
    ((vectorp value)
     (java-static "clojure.lang.PersistentVector" "create([Ljava/lang/Object;)"
                  (%object-array (map 'list #'from-lisp value))))
    ((hash-table-p value)
     (java-static "clojure.lang.PersistentHashMap" "create([Ljava/lang/Object;)"
                  (%object-array (loop for k being the hash-keys of value using (hash-value v)
                                       collect (from-lisp k) collect (from-lisp v)))))
    ((typep value 'foreign-function) (foreign-function-object value))
    ((functionp value)
     (make-java-subclass-instance 'lisp-fn :function value))
    (t value)))

(defun %parse-integer (object)
  (parse-integer (lstring (java-call "toString()" object))))

(defun to-lisp (object)
  "The Lisp value for the Clojure OBJECT; see the table at the top of this file."
  (flet ((is (key) (typep object (%class key))))
    (cond
      ((null object) nil)
      ((is :boolean) (eql 1 (java-call "booleanValue()" object)))
      ((or (is :long) (is :integer) (is :short) (is :byte))
       (java-call "longValue()" object))
      ((or (is :double) (is :float)) (java-call "doubleValue()" object))
      ((or (is :big-int) (is :big-integer)) (%parse-integer object))
      ((is :ratio)
       (/ (%parse-integer (slot-value object (intern "numerator" :openldk)))
          (%parse-integer (slot-value object (intern "denominator" :openldk)))))
      ((is :big-decimal) (java-call "doubleValue()" object))
      ((is :string) (lstring object))
      ((is :character)
       (let ((char (java-call "charValue()" object)))
         (if (characterp char) char (code-char char))))
      ((is :keyword)
       (intern (%lisp-name (subseq (lstring (java-call "toString()" object)) 1)) :keyword))
      ((is :symbol) (intern (%lisp-name (lstring (java-call "toString()" object)))))
      ((is :map)
       (let ((table (make-hash-table :test #'equal)))
         (dolist (entry (%seq-list object) table)
           (setf (gethash (to-lisp (java-call "key()" entry)) table)
                 (to-lisp (java-call "val()" entry))))))
      ((is :vector) (map 'vector #'to-lisp (%seq-list object)))
      ((or (is :set) (is :sequential) (is :seq)) (mapcar #'to-lisp (%seq-list object)))
      ((is :fn) (make-foreign-function object "Clojure" #'funcall))
      (t object))))

;;; Evaluating

(defun eval (form)
  "Evaluate FORM, a Lisp s-expression for a Clojure form (symbols are
lowercased; Lisp vectors are Clojure vectors and hash tables maps), or a
string of Clojure source, in the user namespace; return the value as Lisp
data."
  (%ensure-started)
  (if (stringp form)
      (eval-string form)
      (to-lisp (%invoke *eval-fn* (from-lisp form)))))

(defun eval-string (source)
  "Evaluate every form in the string SOURCE; return the last value."
  (%ensure-started)
  (to-lisp (%invoke *load-string-fn* (jstring source))))

(defun load (file)
  "Load the Clojure source FILE."
  (%ensure-started)
  (%invoke (%var "clojure.core" "load-file") (jstring (namestring file)))
  t)

(defun require (namespace)
  "Load the Clojure NAMESPACE (a string or symbol), as (require 'namespace)."
  (%ensure-started)
  (%invoke *require-fn* (from-lisp (if (stringp namespace)
                                       (intern (%lisp-name namespace))
                                       namespace)))
  namespace)

;;; Functions and definitions

(defun lookup (name)
  "The var NAME (a string or symbol, \"ns/name\" or resolved in the user
namespace) names, unconverted, so it can be passed to FUNCALL."
  (%ensure-started)
  (let* ((name (if (stringp name) name (%name name)))
         (slash (position #\/ name)))
    (or (if slash
            (with-foreign-errors ("Clojure")
              (%var (subseq name 0 slash) (subseq name (1+ slash))))
            (%invoke *resolve-fn* (java-static "clojure.lang.Symbol"
                                               "intern(Ljava/lang/String;)" (jstring name))))
        (error "Clojure has no var named ~A" name))))

(defun funcall (fn &rest args)
  "Call the Clojure function FN (a function or var, or the name of one) with
ARGS, converted with FROM-LISP; return the result as Lisp data."
  (%ensure-started)
  (let ((fn (if (or (stringp fn) (symbolp fn)) (lookup fn) (foreign-object fn))))
    (to-lisp (apply #'%invoke fn (mapcar #'from-lisp args)))))

(defun define (name value)
  "Define NAME (a string or symbol) in the user namespace as VALUE,
converted with FROM-LISP; a Lisp function becomes a Clojure function."
  (%ensure-started)
  (%invoke *intern-fn*
           (java-static "clojure.lang.Symbol" "intern(Ljava/lang/String;)" (jstring "user"))
           (java-static "clojure.lang.Symbol" "intern(Ljava/lang/String;)"
                        (jstring (if (stringp name) name (%name name))))
           (from-lisp value))
  name)

(defun ex-data (condition)
  "The ex-data map of the Clojure exception behind CONDITION, a
FOREIGN-ERROR, or of its first cause that has one, as Lisp data."
  (loop for throwable = (foreign-error-object condition)
          then (java-call "getCause()" throwable)
        while throwable
        do (let ((data (%invoke *ex-data-fn* throwable)))
             (when data (return (to-lisp data))))))

;;; Clojure syntax in Lisp source
;;;
;;; After (clojure:enable-syntax), #[ ... ] holds Clojure source, written
;;; as Clojure writes it -- vectors, maps, sets, #() functions, @, \c
;;; characters -- and evaluates it, returning the last value as Lisp data:
;;;
;;;   #[ (->> (range 10) (filter even?) (map #(* % %))) ]   => (0 4 16 36 64)
;;;
;;; $name and $(form) splice in Lisp values, passed as objects (not
;;; printed), so lexical variables and Lisp functions work too:
;;;
;;;   (let ((xs '(3 1 2))) #[ (sort > $xs) ])                => (3 2 1)
;;;   #[ (map $#'1+ [1 2 3]) ]                                => (2 3 4)
;;;
;;; In a backquoted macro template, $,x splices in the macro argument's
;;; value: (defmacro clj-max (form) `#[ (apply max $,form) ])

(defvar *lisp-value-counter* 0)

(defun %read-clojure-source (stream &optional (close #\]))
  "Read Clojure source from STREAM up to the CLOSE (] or }) that closes it.
Return the source, with each $ splice replaced by the name of a Clojure
var, and an alist of those names and the Lisp forms whose values they hold."
  (multiple-value-bind (source names forms)
      (openldk.interop::%read-lisp-family-source
       stream close (lambda (i)
                      (declare (ignore i))
                      (format nil "lisp-value-~D" (incf *lisp-value-counter*))))
    (values source (mapcar #'cons names forms))))

(defun %eval-with-lisp-values (source names values)
  "Bind each of NAMES to its Lisp value of VALUES in the user namespace,
then evaluate the Clojure SOURCE, which refers to them."
  (mapc #'define names values)
  (eval-string source))

(defun %read-clojure-syntax (stream subchar arg)
  (declare (ignore subchar arg))
  (multiple-value-bind (source splices) (%read-clojure-source stream)
    `(%eval-with-lisp-values ,source ',(mapcar #'car splices) (list ,@(mapcar #'cdr splices)))))

(define-source-syntax '("clojure" "clj")
  (lambda (source names forms)
    `(%eval-with-lisp-values ,source ',names (list ,@forms)))
  :reader (lambda (stream)
            (multiple-value-bind (source splices) (%read-clojure-source stream #\})
              (values source (mapcar #'car splices) (mapcar #'cdr splices)))))

(defun enable-syntax (&optional (readtable *readtable*))
  "Make #[ ... ] read Clojure source in READTABLE (by default, the current
one), evaluating it where it appears; see above."
  (set-dispatch-macro-character #\# #\[ #'%read-clojure-syntax readtable)
  readtable)
