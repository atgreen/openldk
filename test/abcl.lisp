;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: OPENLDK-ABCL-TESTS; Base: 10 -*-
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

;;;; Tests for openldk/abcl: ABCL from SBCL (FiveAM).
;;;;
;;;; Needs ABCL's jar, downloaded by `make abcl`.  `make check-abcl` runs
;;;; these in the sbcl-abcl image, where ABCL is already started.
(defpackage :openldk-abcl-tests
  (:use :cl :fiveam))

(in-package :openldk-abcl-tests)

(def-suite :openldk-abcl-tests
  :description "Using ABCL from SBCL.")

(in-suite :openldk-abcl-tests)

(test startup
  (is (eq t (abcl:startup)))
  (is (string= "Armed Bear Common Lisp" (abcl:eval '(lisp-implementation-type)))))

(test eval-forms-and-strings
  (is (= 3 (abcl:eval '(+ 1 2))))
  (is (equal '(1 4 9) (abcl:eval '(mapcar (lambda (x) (* x x)) '(1 2 3)))))
  (is (= 15511210043330985984000000
         (abcl:eval "(defun fact (n) (if (< n 2) 1 (* n (fact (1- n))))) (fact 25)")))
  (is (= 1 (abcl:eval "(defpackage :abcl-test-pkg (:use :cl)) (in-package :abcl-test-pkg)
                      (defun one () 1) (in-package :cl-user) (abcl-test-pkg::one)"))
      "forms read one at a time")
  (is (= 7 (abcl:eval "(java:jstatic \"max\" \"java.lang.Math\" 3 7)")))
  (is (= 7 (abcl:eval '(progn (defclass pt () ((x :initarg :x :reader px)))
                              (px (make-instance 'pt :x 7)))))))

(test values-to-lisp
  (is (equalp (list 1 "two" #\3 :four 1/3 2.5d0 1.5 (expt 2 100) #(1 2) nil t)
             (abcl:eval '(list 1 "two" #\3 :four 1/3 2.5d0 1.5 (expt 2 100) #(1 2) nil t))))
  (is (eq 'car (abcl:eval ''car)) "CL symbols are SBCL's")
  (let ((*package* (find-package :openldk-abcl-tests)))
    (is (eq 'openldk-abcl-tests::some-symbol (abcl:eval ''some-symbol))
        "a CL-USER symbol comes back in *PACKAGE*")))

(test values-round-trip
  (dolist (value (list 1 -7 (expt 2 70) 3/4 2.5d0 1.5 "a string" #\x :kw 'car
                       '(1 (2 3) "four") #(1 2 3)))
    (is (equalp value (abcl:to-lisp (abcl:from-lisp value))))))

(test call-abcl-from-lisp
  (abcl:eval '(defun add3 (a b c) (+ a b c)))
  (is (= 6 (abcl:funcall "add3" 1 2 3)))
  (is (= 6 (abcl:funcall 'add3 1 2 3)))
  (is (equal '(1 2) (abcl:funcall 'list 1 2)))
  (is (string= "AB" (abcl:funcall (abcl:lookup "string-upcase") "ab"))))

(test call-lisp-from-abcl
  (abcl:define 'square (lambda (x) (* x x)))
  (is (equal '(1 4 9) (abcl:eval '(mapcar 'square '(1 2 3)))))
  (abcl:define "add-many" (lambda (&rest xs) (apply #'+ xs)))
  (is (= 66 (abcl:eval '(add-many 1 2 3 4 5 6 7 8 9 10 11))) "more than 8 arguments")
  (is (= 0 (abcl:eval '(add-many))))
  (abcl:define '*limit* 10)
  (is (= 20 (abcl:eval '(* *limit* 2)))))

(test abcl-errors
  (handler-case (progn (abcl:eval '(no-such-function 1)) (fail "no error"))
    (openldk.interop:foreign-error (e)
      (is (string= "ABCL" (openldk.interop:foreign-error-language e)))
      (is (search "NO-SUCH-FUNCTION" (openldk.interop:foreign-error-message e)))))
  (signals openldk.interop:foreign-error (abcl:eval '(error "boom ~a" 42)))
  (signals openldk.interop:foreign-error (abcl:eval "(car"))
  (signals openldk.interop:foreign-error (abcl:lookup "no-such-function"))
  (is (string= "x" (abcl:eval '(handler-case (error "x") (error (e) (princ-to-string e)))))
      "errors ABCL handles itself")
  (abcl:define '*z* 0)
  (signals openldk.interop:foreign-error (abcl:eval '(let ((*z* 1)) (error "inside"))))
  (is (= 0 (abcl:eval '*z*)) "ABCL's bindings unwound"))

(test lisp-errors-in-callbacks
  (abcl:define 'lisp-fail (lambda () (error "from SBCL")))
  (handler-case (progn (abcl:eval '(lisp-fail)) (fail "no error"))
    (simple-error (e) (is (string= "from SBCL" (princ-to-string e)))))
  (abcl:define 'lisp-type-error (lambda (x) (+ x 1)))
  (signals type-error (abcl:eval '(lisp-type-error "one")))
  (abcl:define '*w* 0)
  (signals simple-error (abcl:eval '(let ((*w* 1)) (lisp-fail))))
  (is (= 0 (abcl:eval '*w*)) "ABCL's bindings unwound"))

(test foreign-functions-are-lisp-functions
  (let ((f (abcl:eval-string "(lambda (x) (* x x))")))
    (is (typep f 'openldk.interop:foreign-function))
    (is (= 49 (funcall f 7)))
    (is (equal '(1 4 9) (mapcar f '(1 2 3))))
    (is (equal '(3 2 1) (sort (list 1 2 3) #'> :key f)))
    (abcl:define "LISPY" f)
    (is (equal '(1 4 9) (abcl:eval-string "(mapcar lispy (list 1 2 3))")) "passed back, it is the original function")))
