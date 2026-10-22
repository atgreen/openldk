;;;; Tests for openldk/scala: Scala from Common Lisp (FiveAM).
;;;;
;;;; Needs Scala's jars, downloaded by `make sbcl-scala`.  `make check-scala`
;;;; runs these in the sbcl-scala image, where Scala is already started.

(defpackage :openldk-scala-tests
  (:use :cl :fiveam))

(in-package :openldk-scala-tests)

(def-suite :openldk-scala-tests
  :description "Using Scala from Common Lisp.")

(in-suite :openldk-scala-tests)

(test startup
  (is (eq t (scala:startup))))

(test eval
  (is (= 3 (scala:eval "1 + 2")))
  (is (equal '(2 4 6) (scala:eval "List(1, 2, 3).map(_ * 2)")))
  (is (= 120 (scala:eval "def fact(n: Int): Int = if (n < 2) 1 else n * fact(n - 1); fact(5)")))
  (is (= 24 (scala:eval "fact(4)")) "definitions persist")
  (is (= 7 (scala:eval "math.max(3, 7)"))))

(test values-to-lisp
  (is (equal '("s" #\c t nil) (scala:eval "List(\"s\", 'c', true, false)")))
  (is (= 2.5d0 (scala:eval "1.25 * 2")))
  (is (= (expt 2 100) (scala:eval "BigInt(2).pow(100)")))
  (is (= 10000000000 (scala:eval "10000000000L")))
  (is (equalp #(1 2 3) (scala:eval "Vector(1, 2, 3)")))
  (is (equalp #(1 2 3) (scala:eval "Array(1, 2, 3)")))
  (is (equal '(1 "a") (scala:eval "(1, \"a\")")))
  (is (= 5 (scala:eval "Some(5)")))
  (is (null (scala:eval "None")))
  (is (null (scala:eval "()")))
  (let ((table (scala:eval "Map(\"a\" -> 1, \"b\" -> 2)")))
    (is (= 2 (hash-table-count table)))
    (is (= 2 (gethash "b" table)))))

(test values-round-trip
  (dolist (value (list 1 -7 (expt 2 40) (expt 2 70) 2.5d0 "a string" #\x
                       '(1 (2 3) "four") #(1 2 3)))
    (is (equalp value (scala:to-lisp (scala:from-lisp value))))))

(test call-scala-from-lisp
  (is (= 42 (scala:funcall (scala:eval-raw "(a: Int, b: Int) => a * b") 6 7)))
  (scala:eval "val inc = (n: Int) => n + 1")
  (is (= 11 (scala:funcall "inc" 10))))

(test classes-across-evaluations
  ;; A case class from one evaluation, used in a lambda in the next: the
  ;; lambda's invokedynamic names the class, which only the REPL's loader
  ;; defines.
  (scala:eval "sealed trait Suit
               case class Heart(rank: Int) extends Suit
               case class Spade(rank: Int) extends Suit")
  (is (equal '("H2" "S3" "H5")
             (scala:eval "List(2, 3, 5).zipWithIndex.map {
                            case (r, i) => if (i % 2 == 0) Heart(r) else Spade(r)
                          }.map { case Heart(r) => s\"H$r\"; case Spade(r) => s\"S$r\" }"))))

(test call-lisp-from-scala
  (scala:define "square" (lambda (x) (* x x)))
  (is (equal '(1 4 9 16) (scala:eval "(1 to 4).map(square).toList")))
  (scala:define "plus" #'+ :arity 2)
  (is (= 10 (scala:eval "List(1, 2, 3, 4).reduce(plus)")))
  (scala:define "greeting" "hello" :type "String")
  (is (string= "HELLO" (scala:eval "greeting.toUpperCase"))))

(test errors
  (signals openldk.interop:foreign-error (scala:eval "1 +"))
  (signals openldk.interop:foreign-error (scala:eval "throw new RuntimeException(\"boom\")")))

(test foreign-functions-are-lisp-functions
  (let ((f (scala:eval "(x: Int) => x * x")))
    (is (typep f 'openldk.interop:foreign-function))
    (is (= 49 (funcall f 7)))
    (is (equal '(1 4 9) (mapcar f '(1 2 3))))
    (is (equal '(3 2 1) (sort (list 1 2 3) #'> :key f)))
    (scala:define "lispy" f :type "Int => Int")
    (is (equal '(1 4 9) (scala:eval "List(1, 2, 3).map(lispy)")) "passed back, it is the original function")))

;;; #@scala{ ... } source syntax, in a readtable of this file's own
(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf *readtable* (openldk.interop:enable-source-syntax (copy-readtable))))

(test scala-syntax
  (let ((xs '(5 3 8)))
    (is (= 32 #@scala{ $xs.map { case n: Int => n * 2 }.sum })))
  (is (equal '(1 2 3 4 5) #@sc{ (1 to $(+ 2 3)).toList }))
  (is (string= "6 } x" #@sc{ val n = 6; s"$n } x" }) "a } in a string")
  (dotimes (i 3)
    (is (= (* i i) #@sc{ $i * $i }) "compiled once, called again"))
  (is (equal '(2 3 4) #@sc{ List(1, 2, 3).map(n => $#'1+(n)) })))

(test precompiled-scala-syntax
  ;; Typed splices: compiled with this file, when Scala runs at COMPILE-FILE.
  (let ((n 7))
    (is (= 14 #@sc{ $(the (signed-byte 32) n) * 2 }))))
