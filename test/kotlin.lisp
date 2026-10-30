;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: OPENLDK-KOTLIN-TESTS; Base: 10 -*-
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

;;;; Tests for openldk/kotlin: Kotlin from Common Lisp (FiveAM).
;;;;
;;;; Needs the Kotlin compiler's jars, downloaded by `make sbcl-kotlin`.
;;;; `make check-kotlin` runs these in the sbcl-kotlin image, where the
;;;; compiler is already warm.
(defpackage :openldk-kotlin-tests
  (:use :cl :fiveam))

(in-package :openldk-kotlin-tests)

(def-suite :openldk-kotlin-tests
  :description "Using Kotlin from Common Lisp.")

(in-suite :openldk-kotlin-tests)

(test startup
  (is (eq t (kotlin:startup))))

(test eval
  (is (= 3 (kotlin:eval "1 + 2")))
  (is (equal '(2 4 6) (kotlin:eval "listOf(1, 2, 3).map { it * 2 }")))
  (is (string= "a-b" (kotlin:eval "val xs = listOf(\"a\", \"b\")
xs.joinToString(\"-\")")))
  ;; Compiled once, run on every EVAL.
  (let ((first (kotlin:eval "System.nanoTime()")))
    (is (/= first (kotlin:eval "System.nanoTime()")))))

(test values-to-lisp
  (is (equal '("s" #\c t nil) (kotlin:eval "listOf(\"s\", 'c', true, false)")))
  (is (= 2.5d0 (kotlin:eval "1.25 * 2")))
  (is (= 10000000000 (kotlin:eval "10000000000L")))
  (is (= (expt 2 100) (kotlin:eval "java.math.BigInteger.TWO.pow(100)")))
  (is (equal '(1 "a") (kotlin:eval "Pair(1, \"a\")")))
  (is (equalp #(1 2 3) (kotlin:eval "arrayOf(1, 2, 3)")))
  (is (null (kotlin:eval "null")))
  (is (null (kotlin:eval "Unit")))
  (let ((table (kotlin:eval "mapOf(\"a\" to 1, \"b\" to 2)")))
    (is (= 2 (hash-table-count table)))
    (is (= 2 (gethash "b" table)))))

(test values-round-trip
  (dolist (value (list 1 -7 (expt 2 40) (expt 2 70) 2.5d0 "a string" #\x '(1 (2 3) "four")))
    (is (equalp value (kotlin:to-lisp (kotlin:from-lisp value))))))

(test compile-and-call
  (let ((class (kotlin:compile "fun fact(n: Int): Long = if (n < 2) 1 else n * fact(n - 1)
fun greet(name: String) = \"hello, $name\"
fun isEven(n: Int) = n % 2 == 0")))
    (is (= 2432902008176640000 (kotlin:call class "fact" 20)))
    (is (string= "hello, lisp" (kotlin:call class "greet" "lisp")))
    (is (eq t (kotlin:call class "isEven" 4)))
    (is (null (kotlin:call class "isEven" 3)))))

(test call-lisp-from-kotlin
  (let ((class (kotlin:compile "fun twice(f: (Int) -> Int, x: Int) = f(f(x))
fun applyAll(f: (Any?) -> Any?, xs: List<Any?>) = xs.map(f)")))
    (is (= 18 (kotlin:call class "twice" (lambda (x) (* x 3)) 2)))
    (is (equal '(2 3 4) (kotlin:call class "applyAll" #'1+ '(1 2 3))))))

(test call-kotlin-function-from-lisp
  (let ((times (kotlin:eval "{ a: Int, b: Int -> a * b }")))
    (is (= 42 (kotlin:funcall times 6 7)))))

(test errors
  (signals openldk.interop:foreign-error (kotlin:compile "fun broken( = 1"))
  (signals openldk.interop:foreign-error (kotlin:eval "error(\"boom\")")))

(test foreign-functions-are-lisp-functions
  (let ((f (kotlin:eval "{ x: Int -> x * x }")))
    (is (typep f 'openldk.interop:foreign-function))
    (is (= 49 (funcall f 7)))
    (is (equal '(1 4 9) (mapcar f '(1 2 3))))
    (is (equal '(3 2 1) (sort (list 1 2 3) #'> :key f)))))

;;; #@kotlin{ ... } source syntax, in a readtable of this file's own
(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf *readtable* (openldk.interop:enable-source-syntax (copy-readtable))))

(test kotlin-syntax
  (let ((n 4))
    (is (equal '(1 4 9 16) #@kotlin{ (1..$n).map { it * it } })))
  (is (string= "a+b" #@kt{ $(list "a" "b").joinToString("+") }))
  (dotimes (i 3)
    (is (= (* i i) #@kt{ $i * $i }) "compiled once, called again"))
  (is (= 18 #@kt{ val f = $(lambda (x) (* x 3)); f(f(2)) as Int })))

(test precompiled-kotlin-syntax
  ;; Typed splices: compiled with this file, when Kotlin runs at COMPILE-FILE.
  (let ((n 7))
    (is (= 14 #@kt{ $(the (signed-byte 32) n) * 2 + $(the double-float 0.5d0).toInt() }))))

(test data-class-fields
  (let ((class (kotlin:compile "data class Point(val x: Int, val label: String)
fun origin() = Point(0, \"origin\")")))
    (is (equal '(("x" . 0) ("label" . "origin"))
               (openldk.interop:java-fields (kotlin:call class "origin") :convert #'kotlin:to-lisp)))))

(test blocks-use-earlier-kotlin-classes
  ;; Compiled classes land in a directory on the next compile's classpath:
  ;; Kotlin finds a class there, and a top-level function through the
  ;; module's META-INF/*.kotlin_module.
  (kotlin:compile "package earlier
class Point(val x: Int, val y: Int)
fun origin(): Point = Point(3, 4)")
  (is (= 7 (kotlin:eval "earlier.origin().x + earlier.Point(0, 4).y"))))

(test coroutines
  ;; async is an extension on CoroutineScope, so it needs an import.
  (is (= 42 (kotlin:call (kotlin:compile "import kotlinx.coroutines.*
fun product(): Int = runBlocking {
    val a = async { 6 }
    val b = async { 7 }
    a.await() * b.await()
}")
                         "product"))))

;;; Typed native definitions use this file's private source readtable.
(defun-kotlin native_add ((a :int) (b :int))
  a + b)

(defun-kotlin native_greet ((name :string))
  name.uppercase())

(defun-kotlin native_factorial ((n :int)) :returns :int
  if (n < 2) 1 else n * native_factorial(n - 1))

(defun-kotlin native_zero ()
  42)

(defun-kotlin native_literal ()
  "a ) } // /* \$literal")

(test native-definitions
  (is (= 7 (native_add 3 4)))
  (is (= 42 (funcall #'native_add 20 22)))
  (is (equal '(2 3 4) (mapcar (lambda (n) (native_add n 1)) '(1 2 3))))
  (is (string= "HELLO" (native_greet "hello")))
  (is (= 120 (native_factorial 5)))
  (is (= 42 (native_zero)))
  (is (string= "a ) } // /* $literal" (native_literal)))
  (signals program-error (funcall #'native_add 1)))

(test native-capture-once
  (let ((factor 3))
    (defun-kotlin native_times ((n :int))
      n * $factor)
    (setf factor 9)
    (is (= 21 (native_times 7)))))

(test native-compile-once
  (let* ((original (symbol-function 'kotlin:compile))
         (count 0)
         (*package* (find-package :openldk-kotlin-tests))
         (*readtable* (openldk.interop:enable-source-syntax (copy-readtable nil))))
    (unwind-protect
         (progn
           (setf (symbol-function 'kotlin:compile)
                 (lambda (&rest args) (incf count) (apply original args)))
           (eval (read-from-string "(defun-kotlin native_once ((n :int)) n + 1)"))
           (is (= 1 count))
           (is (= 5 (native_once 4)))
           (is (= 6 (native_once 5)))
           (is (= 1 count)))
      (setf (symbol-function 'kotlin:compile) original))))

(test native-type-conversions
  (defun-kotlin native_long ((n :long)) n + 1L)
  (defun-kotlin native_double ((n :double)) n * 2.0)
  (defun-kotlin native_boolean ((flag :boolean)) !flag)
  (defun-kotlin native_list ((xs "List<Int>")) xs.sum())
  (is (= 10000000001 (native_long 10000000000)))
  (is (= 5.0d0 (native_double 2.5d0)))
  (is (= 4.0d0 (native_double 2)))
  (is (= 2 (native_long 1)))
  (is (null (native_boolean t)))
  (is (eq t (native_boolean :false)))
  (is (eq t (native_boolean nil)))
  (is (= 6 (native_list '(1 2 3)))))

(test native-compile-file
  (let* ((source (merge-pathnames (format nil "openldk-kotlin-definition-~A.lisp" (gensym))
                                 (uiop:temporary-directory)))
         (fasl (compile-file-pathname source))
         (calls 0)
         (original (symbol-function 'kotlin:compile)))
    (unwind-protect
         (progn
           (with-open-file (out source :direction :output :if-exists :supersede)
             (format out "(in-package :openldk-kotlin-tests)~%(eval-when (:compile-toplevel :load-toplevel :execute) (setf *readtable* (kotlin:enable-syntax (copy-readtable))))~%(defun-kotlin native_fasl ((x :int)) x + 1)~%"))
           (setf (symbol-function 'kotlin:compile)
                 (lambda (&rest args) (incf calls) (apply original args)))
           (is (not (nth-value 2 (compile-file source))))
           (is (= 0 calls))
           (let ((*readtable* (copy-readtable nil))) (load fasl))
           (is (= 1 calls))
           (is (= 42 (funcall 'native_fasl 41)))
           (is (= 1 calls)))
      (setf (symbol-function 'kotlin:compile) original)
      (when (fboundp 'native_fasl) (fmakunbound 'native_fasl))
      (dolist (path (list source fasl)) (when (probe-file path) (delete-file path))))))

(test native-captured-recursion
  (let ((factor 2))
    (defun-kotlin native_recursive_capture ((n :int)) :returns :int
      if (n < 1) $factor else n * native_recursive_capture(n - 1))
    (is (= 48 (native_recursive_capture 4)))))

(test native-captures-converted-once
  (let ((xs (list 1 2 3)))
    (defun-kotlin native_captured_list ()
      $xs.size)
    (setf (cdr xs) nil)
    (is (= 3 (native_captured_list)))))
