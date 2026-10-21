;;; Build ./jscheme, a JScheme image whose Java code OpenLDK has already
;;; compiled.  Run through `make jscheme`, which downloads Peter Norvig's
;;; JScheme sources and compiles them first.  See docs/IMAGES.md.

(in-package :openldk)

;; Two runs: Norvig's R4RS test suite, which exercises most of the
;; interpreter, then a short REPL session through stdin, for the reader,
;; printer and REPL loop.
(defparameter *jscheme-warmup-input* "
(+ 2 3)
(define (fact n) (if (< n 2) 1 (* n (fact (- n 1)))))
(fact 20)
(map (lambda (x) (* x x)) '(1 2 3 4))
(string-append \"hello\" \", \" \"world\")
")

(dump-warm-image "jscheme" "jscheme.Scheme"
                 ;; Absolute: the suite runs in its own directory (it reopens
                 ;; r4rstest.scm by name).
                 :classpath (namestring (merge-pathnames "lib/jscheme/classes/"))
                 :warmups (list (list :args (list "r4rstest.scm") :input ""
                                      :directory "lib/jscheme/")
                                (list :input *jscheme-warmup-input*))
                 :timeout 600)
