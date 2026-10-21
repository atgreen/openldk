;;; Build ./kawa, a Kawa Scheme image whose Java code OpenLDK has already
;;; compiled.  Run through `make kawa`, which downloads the Kawa jar first.
;;; See docs/IMAGES.md.

(in-package :openldk)

;; One REPL session fed through stdin.  Each form pulls in code the image
;; should not have to compile at runtime: the reader and printer, Kawa's
;; compiler (it compiles every REPL form to bytecode), procedure definition
;; and calls, closures, named let, lists, vectors, strings, characters,
;; numbers of each kind, apply, assoc, continuations and output.
(defparameter *kawa-warmup-input* "
(+ 2 3)
(define (fact n) (if (< n 2) 1 (* n (fact (- n 1)))))
(fact 30)
(define (make-adder n) (lambda (x) (+ x n)))
((make-adder 3) 4)
(map (lambda (x) (* x x)) '(1 2 3 4))
(let loop ((i 0) (acc '())) (if (= i 5) (reverse acc) (loop (+ i 1) (cons i acc))))
(vector-ref (vector 1 2 3) 1)
(string-append \"hello\" \", \" (number->string 42))
(string-upcase \"scheme\")
(list->string (list #\\a #\\b #\\c))
(exact->inexact 1/3)
(apply + '(1 2 3 4))
(assoc 'b '((a 1) (b 2)))
(call-with-current-continuation (lambda (k) (+ 1 (k 10))))
(display \"warm\")
(newline)
")

(dump-warm-image "kawa" "kawa.repl"
                 :classpath "lib/kawa-3.1.1.jar"
                 :warmups (list (list :input *kawa-warmup-input*))
                 :timeout 1800)
