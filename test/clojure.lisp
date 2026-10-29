;;;; Tests for openldk/clojure: Clojure from Common Lisp (FiveAM).
;;;;
;;;; Needs Clojure's jars, downloaded by `make clojure`.  Starting Clojure
;;;; takes minutes, so `make check-clojure` runs these in the sbcl-clojure
;;;; image, where it's already started.

(defpackage :openldk-clojure-tests
  (:use :cl :fiveam))

(in-package :openldk-clojure-tests)

(def-suite :openldk-clojure-tests
  :description "Using Clojure from Common Lisp.")

(in-suite :openldk-clojure-tests)

(test startup
  (is (eq t (clojure:startup))))

(test eval-forms-and-strings
  (is (= 3 (clojure:eval '(+ 1 2))))
  (is (equal '(2 3 4) (clojure:eval '(map inc #(1 2 3)))))
  (is (equalp #(0 1 2 3) (clojure:eval '(vec (range 4)))))
  (is (= 15511210043330985984000000
         (clojure:eval "(defn fact [n] (if (< n 2) 1 (*' n (fact (dec n))))) (fact 25)"))))

(test values-to-lisp
  (is (equal '("s" #\c :kw sym nil t nil)
             (clojure:eval '(list "s" #\c :kw (quote sym) nil t :false))))
  (is (= 1/3 (clojure:eval '(/ 1 3))))
  (is (= 3.0d0 (clojure:eval '(* 1.5 2))))
  (let ((map (clojure:eval '(hash-map :a 1 :b "two"))))
    (is (= 1 (gethash :a map)))
    (is (string= "two" (gethash :b map))))
  (is (equal '(1 2 3) (sort (clojure:eval '(set #(1 2 2 3))) #'<))))

(test values-round-trip
  (dolist (value (list 1 -7 (expt 2 70) 3/4 2.5d0 "a string" #\x :kw 'a-symbol
                       '(1 (2 3) "four") #(1 2 3)))
    (is (equalp value (clojure:to-lisp (clojure:from-lisp value)))))
  (let ((table (make-hash-table :test #'equal)))
    (setf (gethash :a table) 1)
    (is (= 1 (gethash :a (clojure:to-lisp (clojure:from-lisp table)))))))

(test call-clojure-from-lisp
  (clojure:eval '(defn add3 #(a b c) (+ a b c)))
  (is (= 6 (clojure:funcall "add3" 1 2 3)))
  (is (string= "HI" (clojure:funcall 'clojure.string/upper-case "hi")))
  (is (string= "a1:b" (clojure:funcall (clojure:lookup "clojure.core/str") "a" 1 :b))))

(test call-lisp-from-clojure
  (clojure:define "square" (lambda (x) (* x x)))
  (is (equalp #(0 1 4 9) (clojure:eval '(mapv square (range 4)))))
  (clojure:define 'add-many (lambda (&rest xs) (apply #'+ xs)))
  (is (= 55 (clojure:eval '(add-many 1 2 3 4 5 6 7 8 9 10))) "a fixed arity above 8")
  (is (= 210 (clojure:eval '(apply add-many (range 21)))) "more than 20 arguments, via applyTo")
  (is (= 0 (clojure:eval '(add-many))) "no arguments"))

(test require-namespaces
  (clojure:require "clojure.set")
  (is (equal '(1 2) (sort (clojure:eval '(clojure.set/union (hash-set 1) (hash-set 2))) #'<))))

(test case-sensitive-names
  (is (string= "caught: oops"
               (clojure:eval '(try (throw (|Exception.| "oops"))
                               (catch |Exception| e (str "caught: " (|.getMessage| e))))))))

(test clojure-errors
  (handler-case (progn (clojure:eval '(no-such-fn 1)) (fail "no error"))
    (openldk.interop:foreign-error (e)
      (is (string= "Clojure" (openldk.interop:foreign-error-language e)))
      (is (search "no-such-fn" (openldk.interop:foreign-error-message e)))))
  (handler-case (progn (clojure:eval '(throw (ex-info "boom" (hash-map :code 42)))) (fail "no error"))
    (openldk.interop:foreign-error (e)
      (is (= 42 (gethash :code (clojure:ex-data e)))))))

(test lisp-errors-in-callbacks
  (clojure:define "lisp-fail" (lambda () (error "from Lisp")))
  (handler-case (progn (clojure:eval '(lisp-fail)) (fail "no error"))
    (simple-error (e) (is (string= "from Lisp" (princ-to-string e)))))
  (is (string= "caught: from Lisp"
               (clojure:eval '(try (lisp-fail)
                               (catch |Exception| e (str "caught: " (|.getMessage| e))))))
      "Clojure code can catch a Lisp error as an Exception"))

(test load-file
  (let ((file (uiop:with-temporary-file (:stream s :pathname p :keep t :type "clj")
                (write-string "(def loaded-value 99)" s)
                p)))
    (unwind-protect
         (progn (clojure:load file)
                (is (= 99 (clojure:eval 'loaded-value))))
      (delete-file file))))

;;; #[ ... ] Clojure syntax, in a readtable of this file's own
(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf *readtable* (clojure:enable-syntax (copy-readtable))))

(test clojure-syntax
  (is (equal '(0 4 16 36 64) #[ (->> (range 10) (filter even?) (map #(* % %))) ]))
  (is (equalp #(1 2 3) #[ (conj [1 2] 3) ]))
  (let ((xs '(3 1 2)))
    (is (equal '(3 2 1) #[ (sort > $xs) ])))
  (is (equal '(2 3 4) #[ (map $#'1+ [1 2 3]) ]))
  (is (string= "3 ]" #[ (str $(+ 1 2) \space "]") ; a ] in a comment
                       ]))
  (is (= 2 (gethash :b #[ {:a 1 :b 2} ]))))

(defmacro clj-max (form) `#[ (apply max $,form) ])

(test clojure-syntax-in-macros
  (is (= 9 (clj-max (list 3 9 4))))
  (let ((n 5))
    (is (= 25 (clj-max (list n (* n n)))))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf *readtable* (openldk.interop:enable-source-syntax *readtable*)))

(test clojure-source-syntax
  (is (equal '(2 4) #@clj{ (map #(* 2 %) [1 2]) }))
  (let ((xs '(1 2)))
    (is (= 2 (gethash :b #@clojure{ (zipmap [:a :b] $xs) ; } in a comment
                         })))))

(test foreign-functions-are-lisp-functions
  (let ((f (clojure:eval-string "(fn [x] (* x x))")))
    (is (typep f 'openldk.interop:foreign-function))
    (is (= 49 (funcall f 7)))
    (is (equal '(1 4 9) (mapcar f '(1 2 3))))
    (is (equal '(3 2 1) (sort (list 1 2 3) #'> :key f)))
    (clojure:define "lispy" f)
    (is (equal '(1 4 9) (clojure:eval '(map lispy (list 1 2 3)))) "passed back, it is the original function")))

(defun clojure-countdown (n)
  "A #[ ] block that calls back into Lisp, which runs the block again."
  #[ (if (pos? $n) (+ $n ($#'clojure-countdown (dec $n))) 0) ])

(test clojure-syntax-is-reentrant
  (is (= 55 (clojure-countdown 10)) "each level of the recursion keeps its own N"))

(defun-clojure native-clojure-add (a b)
  (let [sum (+ a b)] {:sum sum :values [a b]}))

(clojure:defun-clojure native-clojure-factorial (n)
  (if (< n 2) 1 (* n (native-clojure-factorial (dec n)))))

(defun-clojure native-clojure-case (s)
  (.toUpperCase s))

(test native-clojure-definitions
  (is (= 7 (gethash :sum (native-clojure-add 3 4))))
  (is (equalp #(3 4) (gethash :values (funcall #'native-clojure-add 3 4))))
  (is (= 720 (native-clojure-factorial 6)))
  (is (string= "HELLO" (native-clojure-case "hello")))
  (is (equal '("A" "B") (mapcar #'native-clojure-case '("a" "b"))))
  (signals program-error (funcall (symbol-function 'native-clojure-add) 1)))

(test native-clojure-compiles-once
  (let* ((*readtable* (clojure:enable-syntax (copy-readtable)))
         (*package* (find-package :openldk-clojure-tests))
         (calls 0)
         (original (symbol-function 'clojure:eval-string)))
    (unwind-protect
         (progn
           (setf (symbol-function 'clojure:eval-string)
                 (lambda (source) (incf calls) (funcall original source)))
           (eval (read-from-string "(defun-clojure native-once (a b) (+ a b))"))
           (is (= 1 calls))
           (dotimes (i 5) (is (= (+ i 2) (funcall 'native-once i 2))))
           (is (= 1 calls)))
      (setf (symbol-function 'clojure:eval-string) original)
      (fmakunbound 'native-once))))

(test native-clojure-lexical-capture
  (let ((factor 3))
    (defun-clojure native-scaled (n) (* n $factor))
    (is (= 21 (native-scaled 7)))
    (setf factor 9)
    (is (= 21 (native-scaled 7)) "the foreign closure captures the value at definition time")))

(test native-clojure-compile-file
  (let* ((source (merge-pathnames (format nil "openldk-definition-~A.lisp" (gensym))
                                  (uiop:temporary-directory)))
         (fasl (compile-file-pathname source))
         (calls 0)
         (original (symbol-function 'clojure:eval-string)))
    (unwind-protect
         (progn
           (with-open-file (out source :direction :output :if-exists :supersede)
             (format out "(in-package :openldk-clojure-tests)~%(eval-when (:compile-toplevel :load-toplevel :execute) (setf *readtable* (clojure:enable-syntax (copy-readtable))))~%(defun-clojure native-fasl (x) (inc x))~%"))
           (setf (symbol-function 'clojure:eval-string)
                 (lambda (text) (incf calls) (funcall original text)))
           (is (not (nth-value 2 (compile-file source))))
           (is (= 0 calls) "COMPILE-FILE does not run the foreign compiler")
           (let ((*readtable* (copy-readtable nil))) (load fasl))
           (is (= 1 calls) "FASL load compiles the native function")
           (is (= 42 (funcall 'native-fasl 41)))
           (is (= 1 calls) "ordinary calls invoke the retained function"))
      (setf (symbol-function 'clojure:eval-string) original)
      (when (fboundp 'native-fasl) (fmakunbound 'native-fasl))
      (dolist (path (list source fasl)) (when (probe-file path) (delete-file path))))))
