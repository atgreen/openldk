;;;; The Mad Hatter's Polyglot Tea Party
;;;;
;;;; Seven languages share one table -- one SBCL process, one heap -- and
;;;; pass the teapot around.  Common Lisp hosts; Clojure, Kawa Scheme,
;;;; JScheme, Scala, Kotlin, Groovy and ABCL are guests, all running as Java
;;;; bytecode on OpenLDK.  Each guest shows off a trick it is known for, and
;;;; values travel between them with no serialization at all: Clojure's
;;;; primes become Scala's playing cards, and a question Kotlin asks is
;;;; answered by Lisp, which asks Clojure.
;;;;
;;;; Run it in the SBCL `make sbcl-polyglot` builds, where every guest has
;;;; already arrived:
;;;;
;;;;   ./sbcl-polyglot --load examples/tea-party.lisp
;;;;
;;;; In a plain SBCL (sbcl --dynamic-space-size 32768 --load ...) the guests
;;;; start cold, compiling their Java bytecode to Lisp as they arrive: the
;;;; party takes a while to assemble.

(require :asdf)
(push (uiop:getcwd) asdf:*central-registry*)
(asdf:load-system "openldk/polyglot")

(defpackage #:tea-party
  (:use #:cl))

(in-package #:tea-party)

;; #[ ... ] is Clojure and #@LANGUAGE{ ... } any guest's language, written
;; as the language writes it; $x splices in a Lisp value.
(setf *readtable* (polyglot:enable-syntax (copy-readtable)))

;;; Party manners

(defvar *guests* '())

(defmacro guest (name (&key arrives) &body courses)
  "A guest's turn at the table.  A guest who fails to arrive is noted with
regret, and the party goes on without them."
  `(progn
     (format t "~2&~A~%~A~%" ,name (make-string (length ,name) :initial-element #\~))
     (handler-case
         (progn
           (format t "  (~A arrives...)~%" ,name)
           ,arrives
           (push ,name *guests*)
           ,@courses)
       (error (e)
         (format t "  ~A could not stay: ~A~%" ,name e)))))

(defun say (control &rest args)
  (format t "~&  ~?~%" control args))

(define-condition spilled-tea (error)
  ((guest :initarg :guest :reader guest-of))
  (:report (lambda (c s) (format s "~A spilled the tea" (guest-of c)))))

(defun pour-for (guest)
  "Pour GUEST a cup.  Someone always spills; the host keeps clean cups."
  (restart-case
      (if (string= guest "the Dormouse")
          (error 'spilled-tea :guest guest)
          (format nil "a fine cup for ~A" guest))
    (clean-cup ()
      :report "Fetch a clean cup and pour again."
      (format nil "a clean cup for ~A (the old one is in the teapot)" guest))))

;;; The table is laid

(format t "~&The Mad Hatter's Polyglot Tea Party~%")
(format t "===================================~%")
(format t "~%Place cards, numbered the old-fashioned way:~%")
(loop for name in '("the Hatter (Common Lisp)" "the Dormouse (Clojure)"
                    "the Cheshire Cat (Kawa)" "the March Hare (JScheme)"
                    "the Queen of Hearts (Scala)" "Alice (Kotlin)"
                    "the Caterpillar (Groovy)" "the Mock Turtle (ABCL)")
      for seat from 1
      do (format t "  ~@R. ~A~%" seat name))

;;; Clojure, the Dormouse: lazy, immutable, and awake only when asked

(defvar *primes* '())

(guest "The Dormouse (Clojure)" (:arrives (clojure:startup))
  ;; An infinite lazy sieve: only the primes we take are ever computed.
  (setf *primes* (clojure:eval-string
                  "(defn primes []
                     (letfn [(sieve [s]
                               (lazy-seq (cons (first s)
                                               (sieve (remove #(zero? (mod % (first s))) (rest s))))))]
                       (sieve (iterate inc 2))))
                   (take 8 (primes))"))
  (say "mumbles an infinite list of primes, but only says the first ~R: ~A"
       (length *primes*) *primes*)
  ;; An atom, swapped like a teacup at a crowded table.
  (say "refills ~A cups, one atomic swap! at a time"
       (clojure:eval-string "(let [cups (atom 0)] (dotimes [_ 7] (swap! cups inc)) @cups)"))
  (let ((guests '("Hatter" "Hare" "Dormouse" "Alice")))
    (say "seats the guests by name length, Clojure-style: ~A"
         #[ (vec (sort-by (juxt count identity) $guests)) ]))
  (say "finds the commonest letter in \"twinkle twinkle little bat\": ~A"
       (clojure:eval-string
        "(->> \"twinkle twinkle little bat\" (remove #{\\space}) frequencies (apply max-key val) key)")))

;;; Kawa, the Cheshire Cat: continuations, exact numbers, and endless tails

(guest "The Cheshire Cat (Kawa)" (:arrives (kawa:startup))
  ;; call/cc: the cat vanishes part by part, escaping with only its grin.
  (say "vanishes, leaving only its ~A"
       (string-downcase
        (kawa:eval-string
         "(call-with-current-continuation
            (lambda (vanish)
              (for-each (lambda (part) (if (eq? part 'grin) (vanish part)))
                        '(tail body whiskers grin ears))
              'nothing-at-all))")))
  ;; Exact rationals: the cake is shared without a crumb of rounding.
  (say "shares a cake: 1/3 + 1/4 + 1/6 of it is exactly ~A of the cake"
       (kawa:eval '(+ 1/3 1/4 1/6)))
  ;; Proper tail calls: a million recursive steps, and the stack never grows.
  (say "counts down a million steps, tail-recursively, to ~(~A~)"
       (kawa:eval-string
        "(define (count-down n) (if (= n 0) 'tea-time (count-down (- n 1))))
         (count-down 1000000)")))

;;; JScheme, the March Hare: a tiny Scheme that calls back into Lisp

(guest "The March Hare (JScheme)" (:arrives (jscheme:startup))
  ;; A Lisp predicate, called from a Scheme loop.
  (jscheme:define "unbirthday?" (lambda (day) (/= day 1)) :predicate t)
  (say "counts the unbirthdays in a year (asking Lisp about each day): ~A"
       (jscheme:eval-string
        "(let loop ((day 1) (count 0))
           (if (> day 365) count
               (loop (+ day 1) (if (unbirthday? day) (+ count 1) count))))"))
  (say "and pours: ~A" (pour-for "the March Hare")))

;;; Scala, the Queen of Hearts: case classes and pattern matching

(guest "The Queen of Hearts (Scala)" (:arrives (scala:startup))
  ;; The Dormouse's primes become her playing cards.
  (scala:eval "sealed trait Card
               case class Heart(rank: Int) extends Card
               case class Spade(rank: Int) extends Card
               def verdict(card: Card): String = card match {
                 case Heart(r) if r > 10 => s\"the $r of hearts: off with its head!\"
                 case Heart(r)           => s\"the $r of hearts: pardoned\"
                 case Spade(r)           => s\"the $r of spades: paint it red!\"
               }")
  (dolist (sentence (scala:eval
                     (format nil "List(~{~A~^, ~}).zipWithIndex.map {
                                    case (rank, i) => if (i % 2 == 0) Heart(rank) else Spade(rank)
                                  }.map(verdict)"
                             (or *primes* '(2 3 5 7 11 13)))))
    (say "rules ~A" sentence))
  ;; A Lisp function, called as a Scala function.
  (scala:define "pour" (lambda (cups) (format nil "~R cup~:P" cups)))
  (say "demands ~A"
       (scala:eval "(1 to 3).map(n => pour(n)).mkString(\", \", \", then \", \" of tea\")"))
  ;; Her temper, caught as a Lisp condition.
  (handler-case (scala:eval "throw new IllegalStateException(\"Off with your head!\")")
    (openldk.interop:foreign-error (e)
      (say "loses her temper, and Lisp catches it: ~A"
           (openldk.interop:foreign-error-message e)))))

;;; Kotlin, Alice: data classes, null safety, and a question for the table

(guest "Alice (Kotlin)" (:arrives (kotlin:startup))
  (let ((wonderland
          (kotlin:compile
           "data class Alice(val height: Int, val mood: String)
            fun drinkMe(a: Alice) = a.copy(height = a.height / 10, mood = \"curious\")
            fun eatMe(a: Alice) = a.copy(height = a.height * 10, mood = \"curiouser and curiouser\")
            fun wine(offered: String?) = offered ?: \"there isn't any\"
            fun adventure(ask: (Any?) -> Any?): String {
                val alice = Alice(140, \"bored\")
                val small = drinkMe(alice)
                val tall = eatMe(small)
                return \"shrinks from ${alice.height}cm to ${small.height}cm, \" +
                       \"grows back to ${tall.height}cm (${tall.mood}); \" +
                       \"asks for wine: ${wine(null)}; \" +
                       \"and the Caterpillar's question comes back as: ${ask(\"who are you?\")}\"
            }")))
    ;; Kotlin calls a Lisp function, which asks Clojure to shout.
    (say "~A"
         (kotlin:call wonderland "adventure"
                      (lambda (question)
                        (clojure:eval-string
                         (format nil "(clojure.string/upper-case ~S)" question)))))))

;;; Groovy, the Caterpillar: closures, ranges, and exact decimals

(guest "The Caterpillar (Groovy)" (:arrives (groovy:startup))
  (say "asks \"Who are YOU?\" in ~A"
       #@gv{ ['who', 'are', 'you?'].collect { it.toUpperCase() }.join(' ') })
  ;; Groovy's decimals are exact: 0.1 + 0.2 really is 0.3, and Lisp agrees.
  (say "adds 0.1 and 0.2 and gets exactly ~A" (groovy:eval "0.1 + 0.2"))
  ;; A Lisp function, called as a Groovy closure.
  (groovy:define "puff" (lambda (n) (format nil "~R smoke ring~:P" n)))
  (say "blows ~A" (groovy:eval "(1..3).collect { puff(it) }.join(', then ')")))

;;; ABCL, the Mock Turtle: a Common Lisp that lives inside the JVM

(guest "The Mock Turtle (ABCL)" (:arrives (abcl:startup))
  ;; Common Lisp, evaluated by another Common Lisp, running as Java bytecode.
  (say "sighs over the lessons: ~{~A~^, ~}"
       #@abcl{ (mapcar #'string-capitalize '("reeling" "writhing" "uglification")) })
  ;; ABCL calls Java directly.
  (say "and asks the JVM which is larger, 3 or 7: ~A"
       (abcl:eval-string "(jstatic \"max\" \"java.lang.Math\" 3 7)")))

;;; The host clears up

(format t "~2&The Hatter (Common Lisp)~%~A~%" (make-string 24 :initial-element #\~))
(handler-bind ((spilled-tea
                 (lambda (c)
                   (say "notices that ~A, and reaches for a restart" c)
                   (invoke-restart 'clean-cup))))
  (say "pours ~A" (pour-for "the Dormouse")))
(say "counts ~R guest~:P at the table: ~{~A~^, ~}"
     (length *guests*) (reverse *guests*))
(say "and declares it ~R o'clock, as it always is: time for tea." 6)
(format t "~%")

;;; The Hatter's riddle: one form, six languages
;;;
;;; Read it from the inside out.  Clojure sieves the first ten primes out
;;; of an infinite lazy sequence; Kawa adds up their reciprocals as an exact
;;; fraction (9920878441/6469693230); JScheme sums the digits of its
;;; numerator and denominator in a named let; Scala pattern-matches that
;;; pair; Kotlin checks the answer fits on a clock face; and Common Lisp
;;; writes it out in Roman numerals.

(format t "~&  The Hatter's riddle, answered by everyone at once: ~@R~%"
        (kotlin:funcall                                    ; Kotlin: takeIf, and the Elvis operator
         (kotlin:eval "{ n: Any? -> (n as Number).toInt().takeIf { it in 1..3999 } ?: 1 }")
         (scala:eval                                       ; Scala: a match on a tuple, with a guard
          (format nil "(~{~D~^, ~}) match { case (a, b) if a > b => a * b; case (a, b) => a + b }"
                  (mapcar #'round
                          (jscheme:eval                    ; JScheme: digit sums in a named let
                           `(map (lambda (n)
                                   (let loop ((n n) (sum 0))
                                     (if (< n 1) sum (loop (quotient n 10) (+ sum (remainder n 10))))))
                                 (quote ,(let ((q (kawa:eval ; Kawa: exact rational arithmetic
                                                   `(apply + (map (lambda (p) (/ 1 p))
                                                                  (quote ,(clojure:eval-string ; Clojure: a lazy sieve
                                                                           "(take 10 (filter (fn [n] (not-any? #(zero? (rem n %)) (range 2 n)))
                                                                                             (iterate inc 2)))")))))))
                                           (list (numerator q) (denominator q)))))))))))
