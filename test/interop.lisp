;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: OPENLDK-INTEROP-TESTS; Base: 10 -*-
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

;;;; Tests for openldk/interop, the shared core of the language systems
;;;; (FiveAM).  `make check-interop` runs them.
(defpackage :openldk-interop-tests
  (:use :cl :fiveam :openldk.interop))

(in-package :openldk-interop-tests)

(def-suite :openldk-interop-tests
  :description "The shared core: Java classes defined in Lisp, ...")

(in-suite :openldk-interop-tests)

(defvar *reader-evaluations* 0)

(test native-definition-reader
  (let ((openldk.interop::*function-syntaxes* nil)
        (*readtable* (copy-readtable nil)))
    (openldk.interop::%register-function-syntax 'defun-reader-probe :clojure)
    (enable-source-syntax)
    (let ((reader (get-macro-character (code-char 40))))
      (enable-source-syntax)
      (is (eq reader (get-macro-character (code-char 40)))))
    (dolist (text '("()" "(a . b)" "(a (b . c) nil)" "'(a b)"
                    "(defun-reader-probe! 1)" "(|defun-reader-probe| 1)"))
      (is (equal (let ((*readtable* (copy-readtable nil))) (read-from-string text))
                 (read-from-string text))))
    (is (equal '(a 3 4 5) (eval (read-from-string "`(a ,(+ 1 2) ,@(list 4 5))"))))
    (let ((cycle (read-from-string "#1=(a . #1#)")))
      (is (eq cycle (cdr cycle))))
    (let ((*reader-evaluations* 0))
      (is (equal '(1) (read-from-string "(#.(incf *reader-evaluations*))")))
      (is (= 1 *reader-evaluations*)))
    (let ((definition (read-from-string
                       (format nil "(; head comment~%#| nested #| ) |# comment |#~%defun-reader-probe add (a b) (let [sum (+ a b)] {:sum sum}))"))))
      (is (eq 'defun-reader-probe (first definition)))
      (is (equal '(a b) (third definition)))
      (is (search "[sum (+ a b)]" (fourth definition))))
    (is (eq :kept (read-from-string "#+(or) (defun-reader-probe ignored (a) #(str %) {:a a}) :kept")))))

(define-java-class greeter (:implements ("java.util.function.Function"))
    ((greeting :initform "hello") (count :initform 0))
  ("apply(Ljava/lang/Object;)Ljava/lang/Object;" (self name)
    (incf count)
    (jstring (format nil "~A, ~A (~D)" greeting (lstring name) count)))
  ("toString()Ljava/lang/String;" (self)
    (jstring (format nil "#<greeter ~S>" greeting))))

(define-java-class failing-runnable (:implements ("java.lang.Runnable")) ()
  ("run()V" (self) (error "from Lisp")))

(test define-java-class
  (let* ((g (make-java-object 'greeter :greeting "hi"))
         (class (java-call "getClass()" g)))
    (is (string= "lisp.openldk_interop_tests.greeter" (lstring (java-call "getName()" class))))
    (is (string= "hi, lisp (1)" (lstring (java-call "apply(Ljava/lang/Object;)" g (jstring "lisp")))))
    (is (string= "hi, java (2)"
                 (lstring (java-call "get()" (java-call "map(Ljava/util/function/Function;)"
                                                        (java-static "java.util.Optional" "of(Ljava/lang/Object;)"
                                                                     (jstring "java"))
                                                        g))))
        "Java code calls the Lisp method")
    (let ((method (java-call "getMethod(Ljava/lang/String;[Ljava/lang/Class;)" class (jstring "apply")
                             (make-java-array-of "java.lang.Class"
                                                 (list (openldk::java-class (java-load "java.lang.Object")))))))
      (is (string= "hi, reflection (3)"
                   (lstring (java-call "invoke(Ljava/lang/Object;[Ljava/lang/Object;)" method g
                                       (make-java-array-of "java.lang.Object" (list (jstring "reflection"))))))
          "reflection finds and calls it"))
    (is (string= "#<greeter \"hi\">" (lstring (java-static "java.lang.String" "valueOf(Ljava/lang/Object;)" g))))
    (is (eql 1 (java-call "isInstance(Ljava/lang/Object;)"
                          (openldk::java-class (java-load "java.util.function.Function")) g))
        "an instance of its interface")
    (is (string= "hello, x (1)" (lstring (java-call "apply(Ljava/lang/Object;)" (make-java-object 'greeter) (jstring "x"))))
        "initforms")))

(test define-java-class-errors
  (signals simple-error
    (with-foreign-errors ("Java")
      (java-call "run()V" (make-java-object 'failing-runnable))))
  (signals simple-error
    (with-foreign-errors ("Java")
      (java-call "join()" (java-static "java.util.concurrent.CompletableFuture" "runAsync(Ljava/lang/Runnable;)"
                                       (make-java-object 'failing-runnable)))))
  (signals error (make-java-object 'no-such-class)))

(define-java-class divider (:implements ("java.util.function.Function")) ()
  ("apply(Ljava/lang/Object;)Ljava/lang/Object;" (self x)
    ;; X is null: a division by zero at run time.
    (/ 1 (if x 1 0))))

(test typed-foreign-errors
  (flet ((parse (s) (with-foreign-errors ("Java")
                      (java-static "java.lang.Integer" "parseInt(Ljava/lang/String;)" (jstring s)))))
    (is (= 42 (parse "42")))
    (is (typep (handler-case (parse "x") (error (e) e)) 'foreign-number-format-error))
    (is (typep (handler-case (parse "x") (error (e) e)) 'parse-error) "a standard PARSE-ERROR too")
    (is (typep (handler-case (parse "x") (error (e) e)) 'foreign-illegal-argument-error) "its Java superclass's too")
    (is (string= "java.lang.NumberFormatException"
                 (handler-case (parse "x") (foreign-error (e) (foreign-error-java-class e)))))
    (is (= 7 (handler-bind ((foreign-error (lambda (e) (declare (ignore e)) (use-value 7))))
               (parse "x")))
        "USE-VALUE returns a value instead")
    (let ((tries 0))
      (is (= 3 (handler-bind ((foreign-error (lambda (e) (declare (ignore e))
                                               (when (< (incf tries) 3) (invoke-restart 'retry)))))
                 (with-foreign-errors ("Java")
                   (java-static "java.lang.Integer" "parseInt(Ljava/lang/String;)"
                                (jstring (if (< tries 2) "x" "3"))))))
          "RETRY makes the call again")))
  (is (typep (handler-case (with-foreign-errors ("Java")
                             (java-static "java.lang.Math" "floorDiv(II)" 1 0))
               (error (e) e))
             'arithmetic-error)))

(test lisp-errors-thrown-into-java-by-type
  (let ((exception (handler-case (java-call "apply(Ljava/lang/Object;)" (make-java-object 'divider) nil)
                     (openldk::|condition-java/lang/Throwable| (c) (slot-value c 'openldk::|objref|)))))
    (is (java-instance-p exception "java.lang.ArithmeticException")
        "a DIVISION-BY-ZERO is thrown into Java as an ArithmeticException"))
  (is (typep (handler-case (with-foreign-errors ("Java")
                             (java-call "apply(Ljava/lang/Object;)" (make-java-object 'divider) nil))
               (error (e) e))
             'division-by-zero)
      "and comes back as the original condition"))

(test java-sequences
  (let* ((list (java-new "java.util.ArrayList" "()")))
    (dotimes (i 5) (java-call "add(Ljava/lang/Object;)Z" list (jstring (format nil "s~D" i))))
    (let ((seq (java-sequence list :convert #'lstring :unconvert #'jstring)))
      (is (= 5 (length seq)))
      (is (string= "s3" (elt seq 3)))
      (is (equal '("s0" "s1" "s2" "s3" "s4") (map 'list #'identity seq)))
      (is (= 2 (position "s2" seq :test #'string=)))
      (is (equalp #("s1" "s2") (subseq seq 1 3)))
      (setf (elt seq 0) "changed")
      (is (string= "changed" (lstring (java-call "get(I)Ljava/lang/Object;" list 0)))
          "(SETF ELT) writes through to the Java list"))))

(test java-iterators
  (let ((list (java-call "toList()Ljava/util/List;"
                         (java-call "boxed()Ljava/util/stream/Stream;"
                                    (java-static "java.util.stream.IntStream" "range(II)" 0 10)))))
    (is (equal '(0 1 2) (java-take 3 list (lambda (n) (java-call "intValue()I" n)))))
    (let ((sum 0))
      (do-java-iterator (n list (lambda (n) (java-call "intValue()I" n)))
        (incf sum n))
      (is (= 45 sum)))))

(test lisp-collection-views
  (let* ((data (vector 1 2 3))
         (view (lisp-list-view data :convert (lambda (n) (java-static "java.lang.Integer" "valueOf(I)" n)))))
    (is (= 3 (java-call "size()I" view)))
    (is (= 2 (java-call "intValue()I" (java-call "get(I)Ljava/lang/Object;" view 1))))
    (setf (aref data 1) 20)
    (is (= 20 (java-call "intValue()I" (java-call "get(I)Ljava/lang/Object;" view 1)))
        "a view, not a copy")
    (is (string= "[1, 20, 3]" (lstring (java-call "toString()Ljava/lang/String;" view)))
        "Java's own AbstractList methods work over it"))
  (let* ((table (make-hash-table :test #'equal)))
    (setf (gethash "a" table) "one")
    (let ((view (lisp-map-view table :convert #'jstring :unconvert #'lstring)))
      (is (string= "one" (lstring (java-call "get(Ljava/lang/Object;)Ljava/lang/Object;" view (jstring "a")))))
      (is (eql 1 (java-call "containsKey(Ljava/lang/Object;)Z" view (jstring "a"))))
      (is (= 1 (java-call "size()I" view))))))

(define-java-class label () ((text :initform "label"))
  ("toString()Ljava/lang/String;" (self) (jstring text)))

(define-java-method :around ("lisp.openldk_interop_tests.label" "toString()Ljava/lang/String;") (self)
  (jstring (concatenate 'string "[" (lstring (call-next-method)) "]")))

(test define-java-method
  (is (string= "[price]"
               (lstring (java-static "java.lang.String" "valueOf(Ljava/lang/Object;)"
                                     (make-java-object 'label :text "price"))))
      "an :around method wraps the Java method, called from Java (String.valueOf)"))

(test typed-native-definition-reader
  (let ((openldk.interop::*function-syntaxes* nil)
        (*readtable* (copy-readtable nil)))
    (openldk.interop::%register-function-syntax 'defun-typed-probe :kotlin)
    (enable-source-syntax)
    (let ((form (read-from-string
                 "(defun-typed-probe add ((a :int) (b :int)) :returns :int
                   listOf(a, b).map { it + 1 }.sum() /* ) */)")))
      (is (eq 'defun-typed-probe (first form)))
      (is (equal '((a :int) (b :int)) (third form)))
      (is (search "listOf(a, b).map { it + 1 }.sum()" (fourth form)))
      (is (eq :int (seventh form))))
    (is (eq :kept (read-from-string
                   "#+(or) (defun-typed-probe ignored ((n :int)) :returns :int n + 1) :kept")))
    (let ((form (read-from-string "(defun-typed-probe capture ((n :int)) n * $factor)")))
      (is (equal '("lispArg1") (fifth form)))
      (is (equal '(factor) (sixth form)))))
  (dolist (parameters '((n) ((n)) ((n :int) (n :long)) ((nil :int)) ((n :unknown))))
    (signals error
      (openldk.interop::%expand-typed-function-definition
       'probe parameters "n + 1" nil nil nil 'identity))))

(test lisp-family-multiple-vector-delimiters
  ;; Read the real source text and then a following CL form. In particular,
  ;; the first inner ] must not terminate the enclosing source block.
  (dolist (close '(#\] #\} #\)))
    (dolist (source '("[1 2] [3 4]"
                      "[[1 2] [3 [4 5]]]"
                      "(let [[a b] [1 2] [c d] [3 4]] [[a b] [c d]])"
                      "[\"[[]]\" \\[ \\]] [3 4]"
                      "{:a [[1 2] [3 4]] :b [[] []]}"))
      (with-input-from-string (stream (format nil "~A~C :after" source close))
        (multiple-value-bind (actual names forms)
            (openldk.interop::%read-lisp-family-source stream close #'identity)
          (is (string= source actual))
          (is (null names))
          (is (null forms))
          (is (eq :after (read stream))))))))

(test scheme-dispatch-directives-are-not-comments
  (with-input-from-string (stream "#!fold-case [1 2]) :after")
    (is (string= "#!fold-case [1 2]"
                 (openldk.interop::%read-lisp-family-source stream #\) #'identity
                                                           :escaped-symbols t)))
    (is (eq :after (read stream)))))

(defun %generated-form-contains (form symbol)
  (or (eq form symbol)
      (and (consp form) (or (%generated-form-contains (car form) symbol)
                           (%generated-form-contains (cdr form) symbol)))))

(test guarded-instance-inline-native-changes
  (let ((directory (asdf:system-relative-pathname :openldk ".cache/generated-code-test/")))
    (ensure-directories-exist directory)
    (uiop:run-program
     (list (namestring (merge-pathnames "bin/javac" (uiop:ensure-directory-pathname (uiop:getenv "JAVA_HOME"))))
           "--release" "25" "-d" (namestring directory)
           (namestring (asdf:system-relative-pathname :openldk "test/standards/InstanceInlineProbe.java")))
     :output t :error-output t)
    (openldk.interop:add-to-classpath (namestring directory)))
  (let* ((object (java-new "InstanceInlineProbe" "<init>()V"))
         (child (java-new "InstanceInlineProbe$Child" "<init>()V"))
         (class (class-of object)) (child-class (class-of child))
         (add (fdefinition 'openldk::|add(II)I|))
         (subtract (fdefinition 'openldk::|subtract(II)I|))
         (types (list class (find-class t) (find-class t))))
    (let ((original (symbol-function 'openldk::%build-method-definition))
          (definitions nil)
          (openldk::*fasl-cache-directory* nil))
      (unwind-protect
           (progn
             (setf (symbol-function 'openldk::%build-method-definition)
                   (lambda (&rest arguments)
                     (let ((form (apply original arguments)))
                       (when (string= "InstanceInlineProbe" (openldk::name (first arguments)))
                         (push form definitions))
                       form)))
             (is (= 45 (java-call "loop(I)I" object 10)))
             (is (= -45 (java-call "privateLoop(I)I" object 10)))
             (is (some (lambda (form) (%generated-form-contains form 'openldk::%inline-static-target-p)) definitions))
             (is (some (lambda (form) (%generated-form-contains form 'openldk::%inline-private-target-p)) definitions)))
        (setf (symbol-function 'openldk::%build-method-definition) original)))
    (dolist (signature '("pass(I)I" "privatePass(I)I"))
      (is (= 42 (java-call signature object 42)))
      (dolist (opaque '(t nil "opaque native value"))
        (is (eq opaque (java-call signature object opaque)))))
    (let ((old-add (find-method add nil types))
          (old-subtract (find-method subtract nil types)))
      (flet ((method (owner result &optional qualifiers)
               (make-instance 'standard-method :lambda-list '(object a b)
                              :qualifiers qualifiers :specializers (list owner (find-class t) (find-class t))
                              :function (lambda (args next) (declare (ignore args next)) result))))
        (unwind-protect
             (progn
               (add-method add (method class 77))
               (is (= 77 (java-call "loop(I)I" object 10)))
               (add-method add old-add)
               (is (= 45 (java-call "loop(I)I" object 10)))
               (add-method subtract (method class 7))
               (is (= 7 (java-call "privateLoop(I)I" object 10)))
               (add-method subtract old-subtract)
               (is (= -45 (java-call "privateLoop(I)I" object 10)))
               (let ((around (method class 123 '(:around))))
                 (unwind-protect
                      (progn (add-method add around) (is (= 123 (java-call "loop(I)I" object 10))))
                   (remove-method add around)))
               (is (= 45 (java-call "loop(I)I" object 10)))
               (let ((specific-add (method child-class 99))
                     (specific-subtract (method child-class 88)))
                 (unwind-protect
                      (progn
                        (add-method add specific-add) (add-method subtract specific-subtract)
                        (is (= 99 (java-call "loop(I)I" child 10)))
                        (is (= 45 (java-call "loop(I)I" object 10)))
                        ;; Private selection remains on the defining class.
                        (is (= -45 (java-call "privateLoop(I)I" child 10))))
                   (remove-method add specific-add) (remove-method subtract specific-subtract)))
               (setf (fdefinition 'openldk::|add(II)I|) (lambda (object a b) (declare (ignore object a b)) 55))
               (is (= 55 (java-call "loop(I)I" object 10))))
          (setf (fdefinition 'openldk::|add(II)I|) add)
          (unless (eq add (sb-mop:method-generic-function old-add)) (add-method add old-add))
          (unless (eq subtract (sb-mop:method-generic-function old-subtract)) (add-method subtract old-subtract)))))))

(test primitive-call-results-retain-native-fallback
  (let ((directory (asdf:system-relative-pathname :openldk ".cache/generated-code-test/")))
    (ensure-directories-exist directory)
    (uiop:run-program
     (list (namestring (merge-pathnames "bin/javac" (uiop:ensure-directory-pathname (uiop:getenv "JAVA_HOME"))))
           "--release" "25" "-d" (namestring directory)
           (namestring (asdf:system-relative-pathname :openldk "test/standards/PrimitiveResultProbe.java")))
     :output t :error-output t)
    (openldk.interop:add-to-classpath (namestring directory)))
  (let ((openldk::*enable-primitive-result-arithmetic* nil)
        (openldk::*fasl-cache-directory* nil))
    (is (= -6 (java-static "PrimitiveResultProbe" "afterIntBaseline(I)I" 7)))
    (java-static "PrimitiveResultProbe" "orderBaseline()I"))
  (let ((openldk::*enable-primitive-result-arithmetic* t)
        (openldk::*fasl-cache-directory* nil))
    (is (= -6 (java-static "PrimitiveResultProbe" "afterInt(I)I" 7)))
    (java-static "PrimitiveResultProbe" "order()I"))
  (let* ((class (java-load "PrimitiveResultProbe"))
         (symbol (openldk::static-method-symbol "PrimitiveResultProbe.chooseInt(I)I" (openldk::ldk-class-package class)))
         (original (fdefinition symbol))
         (events nil))
    (labels ((outcome (method &rest arguments)
               (handler-case (list :value (apply #'java-static "PrimitiveResultProbe" method arguments))
                 (error (condition) (list :error (class-name (class-of condition)))))))
      (unwind-protect
           (progn
             (dolist (value (list 42 (ash 1 80) "opaque native value" t nil))
               (setf (fdefinition symbol) (lambda (x) (declare (ignore x)) value))
               (is (eq value (java-static "PrimitiveResultProbe" "passInt(I)I" 7)))
               (is (equal (outcome "afterIntBaseline(I)I" 7) (outcome "afterInt(I)I" 7))))
             (setf (fdefinition symbol)
                   (lambda (x) (push x events) (if (= x 1) :opaque x)))
             (let ((before nil) (after nil))
               (java-static "PrimitiveResultProbe" "clearEffects()V")
               (setf events nil before (outcome "orderBaseline()I"))
               (is (eq :error (first before)))
               (is (equal '(2 1) events))
               (is (= 12 (java-static "PrimitiveResultProbe" "effectCounter()I")))
               (java-static "PrimitiveResultProbe" "clearEffects()V")
               (setf events nil after (outcome "order()I"))
               (is (equal before after))
               (is (equal '(2 1) events))
               (is (= 12 (java-static "PrimitiveResultProbe" "effectCounter()I")))))
        (setf (fdefinition symbol) original)))))
