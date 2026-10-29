;;;; Tests for openldk/kawa: Kawa Scheme from Common Lisp (FiveAM).
;;;;
;;;; Needs Kawa's jar, downloaded by `make kawa`.  `make check-kawa` runs
;;;; these in the sbcl-kawa image, where Kawa is already started.

(defpackage :openldk-kawa-tests
  (:use :cl :fiveam))

(in-package :openldk-kawa-tests)

(def-suite :openldk-kawa-tests
  :description "Using Kawa Scheme from Common Lisp.")

(in-suite :openldk-kawa-tests)

(test startup
  (is (eq t (kawa:startup))))

(test eval-forms-and-strings
  (is (= 3 (kawa:eval '(+ 1 2))))
  (is (equal '(1 4 9) (kawa:eval '(map (lambda (x) (* x x)) '(1 2 3)))))
  (is (= 15511210043330985984000000
         (kawa:eval "(define (fact n) (if (< n 2) 1 (* n (fact (- n 1))))) (fact 25)")))
  (is (= 7 (kawa:eval "(java.lang.Math:max 3 7)")) "colon notation, in a string"))

(test values-to-lisp
  (is (equal '("s" #\c sym t nil) (kawa:eval '(list "s" #\c (quote sym) t :false))))
  (is (= 1/3 (kawa:eval '(/ 1 3))))
  (is (= 3.0d0 (kawa:eval '(* 1.5 2))))
  (is (= (expt 2 100) (kawa:eval '(expt 2 100))))
  (is (equalp #(1 2 3) (kawa:eval '(vector 1 2 3))))
  (is (equal '(1 . 2) (kawa:eval '(cons 1 2))))
  (is (null (kawa:eval ''()))))

(test values-round-trip
  (dolist (value (list 1 -7 (expt 2 70) 3/4 2.5d0 "a string" #\x :kw 'a-symbol
                       '(1 (2 3) "four") #(1 2 3)))
    (is (equalp value (kawa:to-lisp (kawa:from-lisp value))))))

(test call-kawa-from-lisp
  (kawa:eval '(define (add3 a b c) (+ a b c)))
  (is (= 6 (kawa:funcall "add3" 1 2 3)))
  (is (string= "foobar" (kawa:funcall 'string-append "foo" "bar")))
  (is (equal '(1 2) (kawa:funcall "list" 1 2)) "list is a type, not a procedure object")
  (is (string= "ab" (kawa:funcall (kawa:lookup "string-append") "a" "b"))))

(test call-lisp-from-kawa
  (kawa:define "square" (lambda (x) (* x x)))
  (is (equal '(1 4 9) (kawa:eval '(map square '(1 2 3)))))
  (kawa:define 'add-many (lambda (&rest xs) (apply #'+ xs)))
  (is (= 28 (kawa:eval '(add-many 1 2 3 4 5 6 7))))
  (is (= 0 (kawa:eval '(add-many))))
  (is (= 6 (kawa:eval '(apply add-many '(1 2 3)))))
  (kawa:define "small?" (lambda (x) (< x 10)) :predicate t)
  (is (eq 'big (kawa:eval '(if (small? 50) 'small 'big)))))

(test control
  (is (eq 'caught (kawa:eval "(guard (e (#t 'caught)) (raise 'sym))")))
  (is (string= "x" (kawa:eval "(guard (e ((error-object? e) (error-object-message e))) (error \"x\"))")))
  (is (= 42 (kawa:eval '(call-with-current-continuation (lambda (k) (+ 1 (k 42)))))))
  (is (eq 'handled
          (kawa:eval '(call/cc
                       (lambda (k)
                         (with-exception-handler
                          (lambda (e) (k 'handled))
                          (lambda () (raise 'oops)))))))))

(test kawa-errors
  (handler-case (progn (kawa:eval '(no-such-proc 1)) (fail "no error"))
    (openldk.interop:foreign-error (e)
      (is (string= "Kawa" (openldk.interop:foreign-error-language e)))
      (is (search "no-such-proc" (openldk.interop:foreign-error-message e)))))
  (handler-case (progn (kawa:eval '(error "boom" 42)) (fail "no error"))
    (openldk.interop:foreign-error (e)
      (is (search "boom" (openldk.interop:foreign-error-message e))))))

(test lisp-errors-in-callbacks
  (kawa:define "lisp-fail" (lambda () (error "from Lisp")))
  (handler-case (progn (kawa:eval '(lisp-fail)) (fail "no error"))
    (simple-error (e) (is (string= "from Lisp" (princ-to-string e)))))
  (kawa:define "lisp-type-error" (lambda (x) (+ x 1)))
  (signals type-error (kawa:eval '(lisp-type-error "one"))))

(test foreign-functions-are-lisp-functions
  (let ((f (kawa:eval-string "(lambda (x) (* x x))")))
    (is (typep f 'openldk.interop:foreign-function))
    (is (= 49 (funcall f 7)))
    (is (equal '(1 4 9) (mapcar f '(1 2 3))))
    (is (equal '(3 2 1) (sort (list 1 2 3) #'> :key f)))
    (kawa:define "lispy" f)
    (is (equal '(1 4 9) (kawa:eval '(map lispy '(1 2 3)))) "passed back, it is the original function")))

;;; #@kawa{ ... } source syntax, in a readtable of this file's own
(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf *readtable* (openldk.interop:enable-source-syntax (copy-readtable))))

(test kawa-syntax
  (let ((xs '(1 2 3)))
    (is (equal '(1 4 9) #@kawa{ (map (lambda (x) (* x x)) $xs) })))
  (is (string= "a } b" #@kawa{ (string-append "a" " } " "b") ; a } in a comment
                       }))
  (dotimes (i 3)
    (is (= (* i 10) #@kawa{ (* $i 10) }) "evaluated once, called again"))
  (is (equal '(2 3) #@kawa{ (map $#'1+ '(1 2)) })))

(defun-kawa native-kawa-add (a b)
  #| A closing ) in a #| nested |# comment. |#
  (if #t (vector (+ a b) a b) #f))

(kawa:defun-kawa native-kawa-factorial (n)
  (if (< n 2) 1 (* n (native-kawa-factorial (- n 1)))))

(test native-kawa-definitions
  (is (equalp #(7 3 4) (native-kawa-add 3 4)))
  (is (equalp #(7 3 4) (funcall #'native-kawa-add 3 4)))
  (is (= 720 (native-kawa-factorial 6)))
  (is (equal '(1 1 2 6) (mapcar #'native-kawa-factorial '(0 1 2 3))))
  (signals program-error (funcall (symbol-function 'native-kawa-add) 1)))

(test native-kawa-compiles-once
  (let ((*readtable* (kawa:enable-syntax (copy-readtable)))
        (*package* (find-package :openldk-kawa-tests))
        (calls 0) (original (symbol-function 'kawa:eval-string)))
    (unwind-protect
         (progn
           (setf (symbol-function 'kawa:eval-string)
                 (lambda (source) (incf calls) (funcall original source)))
           (eval (read-from-string "(defun-kawa native-once (a b) (+ a b))"))
           (is (= 1 calls))
           (dotimes (i 5) (is (= (+ i 2) (funcall 'native-once i 2))))
           (is (= 1 calls)))
      (setf (symbol-function 'kawa:eval-string) original)
      (fmakunbound 'native-once))))
