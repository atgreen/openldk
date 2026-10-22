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
xs.joinToString(\"-\")"))))

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
