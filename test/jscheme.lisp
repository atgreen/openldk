;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: OPENLDK-JSCHEME-TESTS; Base: 10 -*-
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

;;;; Tests for openldk/jscheme: JScheme from Common Lisp (FiveAM).
;;;;
;;;; Needs JScheme's classes, built by `make jscheme`.  Run with
;;;; `make check-jscheme' or (asdf:test-system "openldk/jscheme").
(defpackage :openldk-jscheme-tests
  (:use :cl :fiveam))

(in-package :openldk-jscheme-tests)

(def-suite :openldk-jscheme-tests
  :description "Using JScheme from Common Lisp.")

(in-suite :openldk-jscheme-tests)

(test startup
  (is (not (null (jscheme:startup))))
  (is (eq (jscheme:startup) jscheme:*interpreter*) "STARTUP twice gives the same interpreter"))

(test eval-forms-and-strings
  (is (= 5 (jscheme:eval '(+ 2 3))))
  (is (= 0.25d0 (jscheme:eval '(/ 1 4))))
  (is (= 3628800 (jscheme:eval "(define (fact n) (if (< n 2) 1 (* n (fact (- n 1))))) (fact 10)"))))

(test values-to-lisp
  (is (equal '(1 "two" #\3 four t nil)
             (jscheme:eval '(list 1 "two" #\3 'four t :false))))
  (is (equalp #(1 2 3) (jscheme:eval '(vector 1 2 3))))
  (is (equal '(1 . 2) (jscheme:eval '(cons 1 2))))
  (is (null (jscheme:eval ''()))))

(test values-round-trip
  (dolist (value '(42 -7 2.5d0 "a string" #\x some-symbol (1 (2 3) "four") (a . b)))
    (is (equalp value (jscheme:to-lisp (jscheme:from-lisp value))))))

(test call-scheme-from-lisp
  (jscheme:eval '(define (add3 a b c) (+ a b c)))
  (is (= 6 (jscheme:funcall "add3" 1 2 3)))
  (is (= 6 (jscheme:funcall 'add3 1 2 3)))
  (is (string= "foobar" (jscheme:funcall (jscheme:lookup 'string-append) "foo" "bar"))))

(test call-lisp-from-scheme
  (jscheme:define "square" (lambda (x) (* x x)))
  (is (equal '(1 4 9) (jscheme:eval '(map square '(1 2 3)))))
  (jscheme:define 'lisp-upcase #'string-upcase)
  (is (string= "SHOUT" (jscheme:eval '(lisp-upcase "shout"))))
  (jscheme:define 'make-pair (lambda (a b) (cons a b)))
  (is (equal '(1 . 2) (jscheme:eval '(make-pair 1 2)))))

(test predicates
  (jscheme:define "small?" (lambda (x) (< x 10)) :predicate t)
  (is (eq 'big (jscheme:eval '(if (small? 50) 'small 'big))))
  (is (eq 'small (jscheme:eval '(if (small? 5) 'small 'big))))
  ;; Without :PREDICATE, NIL is '(), which Scheme treats as true.
  (jscheme:define "nil-returner" (lambda (x) (declare (ignore x)) nil))
  (is (eq 'true (jscheme:eval '(if (nil-returner 0) 'true 'false)))))

(test scheme-errors
  (signals openldk.interop:foreign-error (jscheme:eval '(no-such-procedure 1)))
  (handler-case (jscheme:eval '(no-such-procedure 1))
    (openldk.interop:foreign-error (e)
      (is (string= "JScheme" (openldk.interop:foreign-error-language e)))
      (is (search "no-such-procedure" (openldk.interop:foreign-error-message e))))))

(test lisp-errors-in-callbacks
  (jscheme:define "fails" (lambda () (error "from Lisp: ~A" 42)))
  (handler-case (progn (jscheme:eval '(fails)) (fail "no error"))
    (simple-error (e) (is (string= "from Lisp: 42" (princ-to-string e)))))
  (jscheme:define "type-fails" (lambda (x) (+ x "a")))
  (signals type-error (jscheme:eval '(type-fails 1))))

(test load-file
  (let ((file (uiop:with-temporary-file (:stream s :pathname p :keep t :type "scm")
                (write-string "(define loaded-value 99)" s)
                p)))
    (unwind-protect
         (progn (jscheme:load file)
                (is (= 99 (jscheme:eval 'loaded-value))))
      (delete-file file))))

(test foreign-functions-are-lisp-functions
  (let ((f (jscheme:eval-string "(lambda (x) (* x x))")))
    (is (typep f 'openldk.interop:foreign-function))
    (is (= 49 (funcall f 7)))
    (is (equal '(1 4 9) (mapcar f '(1 2 3))))
    (is (equal '(3 2 1) (sort (list 1 2 3) #'> :key f)))
    (jscheme:define "lispy" f)
    (is (equal '(1 4 9) (jscheme:eval '(map lispy '(1 2 3)))) "passed back, it is the original function")))

;;; #@jscheme{ ... } source syntax, in a readtable of this file's own
(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf *readtable* (openldk.interop:enable-source-syntax (copy-readtable))))

(test jscheme-syntax
  (let ((xs '(1 2 3)))
    (is (equal '(1 4 9) #@jscheme{ (map (lambda (x) (* x x)) $xs) })))
  (is (string= "a } b" #@jscheme{ (string-append "a" " } " "b") ; a } in a comment
                       }))
  (dotimes (i 3)
    (is (= (* i 10) #@jscheme{ (* $i 10) }) "evaluated once, called again"))
  (is (equal '(2 3) #@jscheme{ (map $#'1+ '(1 2)) })))

(defun-jscheme native-jscheme-add (a b)
  (if #t (vector (+ a b) a b) #f))

(jscheme:defun-jscheme native-jscheme-factorial (n)
  (if (< n 2) 1 (* n (native-jscheme-factorial (- n 1)))))

(test native-jscheme-definitions
  (is (equalp #(7 3 4) (native-jscheme-add 3 4)))
  (is (equalp #(7 3 4) (funcall #'native-jscheme-add 3 4)))
  (is (= 720 (native-jscheme-factorial 6)))
  (is (equal '(1 1 2 6) (mapcar #'native-jscheme-factorial '(0 1 2 3))))
  (signals program-error (funcall (symbol-function 'native-jscheme-add) 1)))

(test native-jscheme-created-once
  (let ((*readtable* (jscheme:enable-syntax (copy-readtable)))
        (*package* (find-package :openldk-jscheme-tests))
        (calls 0) (original (symbol-function 'jscheme:eval-string)))
    (unwind-protect
         (progn
           (setf (symbol-function 'jscheme:eval-string)
                 (lambda (source) (incf calls) (funcall original source)))
           (eval (read-from-string "(defun-jscheme native-once (a b) (+ a b))"))
           (is (= 1 calls))
           (dotimes (i 5) (is (= (+ i 2) (funcall 'native-once i 2))))
           (is (= 1 calls)))
      (setf (symbol-function 'jscheme:eval-string) original)
      (fmakunbound 'native-once))))
