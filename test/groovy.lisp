;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: OPENLDK-GROOVY-TESTS; Base: 10 -*-
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

;;;; Tests for openldk/groovy: Groovy from Common Lisp (FiveAM).
;;;;
;;;; Needs Groovy's jar, downloaded by `make sbcl-groovy`.  `make
;;;; check-groovy` runs these in the sbcl-groovy image, where Groovy is
;;;; already started.
(defpackage :openldk-groovy-tests
  (:use :cl :fiveam))

(in-package :openldk-groovy-tests)

(def-suite :openldk-groovy-tests
  :description "Using Groovy from Common Lisp.")

(in-suite :openldk-groovy-tests)

(test startup
  (is (eq t (groovy:startup))))

(test eval
  (is (= 3 (groovy:eval "1 + 2")))
  (is (equal '(10 20 30) (groovy:eval "[3, 1, 2].sort().collect { it * 10 }")))
  (is (= 2432902008176640000 (groovy:eval "def f(n) { n < 2 ? 1G : n * f(n - 1) }; f(20)")))
  (groovy:eval "total = 10")
  (is (= 20 (groovy:eval "total * 2")) "binding variables persist"))

(test values-to-lisp
  (is (equal '("s" #\c t nil) (groovy:eval "['s', 'c' as char, true, false]")))
  (is (= 5/2 (groovy:eval "1.5 + 1")) "BigDecimal, exactly")
  (is (= 2.5d0 (groovy:eval "1.25d * 2")))
  (is (= (expt 2 100) (groovy:eval "2G ** 100")))
  (is (string= "x = 3" (groovy:eval "def x = 3; \"x = $x\"")) "a GString")
  (is (equal '(1 2 3 4) (groovy:eval "(1..4).toList()")))
  (is (null (groovy:eval "null")))
  (let ((table (groovy:eval "[a: 1, b: 2]")))
    (is (= 2 (hash-table-count table)))
    (is (= 2 (gethash "b" table)))))

(test values-round-trip
  (dolist (value (list 1 -7 (expt 2 40) (expt 2 70) 3/4 2.5d0 "a string" #\x '(1 (2 3) "four")))
    (is (equalp value (groovy:to-lisp (groovy:from-lisp value))))))

(test call-groovy-from-lisp
  (is (= 42 (groovy:funcall (groovy:eval-raw "{ a, b -> a * b }") 6 7)))
  (groovy:eval "inc = { it + 1 }")
  (is (= 11 (groovy:funcall "inc" 10))))

(test call-lisp-from-groovy
  (groovy:define "square" (lambda (x) (* x x)))
  (is (= 30 (groovy:eval "(1..4).collect { square(it) }.sum()")))
  (is (equal '(1 4 9) (groovy:eval "[1, 2, 3].collect(square)")))
  (groovy:define "greeting" "hello")
  (is (string= "HELLO" (groovy:eval "greeting.toUpperCase()"))))

(test errors
  (signals openldk.interop:foreign-error (groovy:eval "1 +"))
  (signals openldk.interop:foreign-error (groovy:eval "throw new IllegalStateException('boom')"))
  (signals error (groovy:eval "square('x')")
    "a Lisp error in a Lisp function Groovy called"))

(test foreign-functions-are-lisp-functions
  (let ((f (groovy:eval "{ x -> x * x }")))
    (is (typep f 'openldk.interop:foreign-function))
    (is (= 49 (funcall f 7)))
    (is (equal '(1 4 9) (mapcar f '(1 2 3))))
    (is (equal '(3 2 1) (sort (list 1 2 3) #'> :key f)))
    (groovy:define "lispy" f)
    (is (equal '(1 4 9) (groovy:eval "[1, 2, 3].collect(lispy)")) "passed back, it is the original function")))

;;; #@groovy{ ... } source syntax, in a readtable of this file's own
(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf *readtable* (openldk.interop:enable-source-syntax (copy-readtable))))

(test groovy-syntax
  (let ((xs '(3 1 2)))
    (is (equal '(10 20 30) #@groovy{ $xs.sort().collect { it * 10 } })))
  (is (string= "a } b" #@gv{ 'a' + ' } ' + "b" }) "a } in strings")
  (dotimes (i 3)
    (is (= (* i i) #@gv{ $i * $i }) "compiled once, called again"))
  (is (equal '(2 3) #@gv{ [1, 2].collect { $#'1+(it) } }))
  (let ((n 3))
    (is (string= "6" #@gv{
                       import java.math.BigInteger
                       BigInteger.valueOf($n).shiftLeft(1).toString()
                     })
        "a block's imports")))
