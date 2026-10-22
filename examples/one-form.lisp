;;;; One form, seven languages
;;;;
;;;; A single Lisp expression in which every JVM language OpenLDK runs does
;;;; one thing it is known for, written in its own syntax, and hands its
;;;; result to the next.  Read it from the inside out:
;;;;
;;;;   Clojure  takes ten primes from an infinite lazy sequence
;;;;            (2 3 5 7 11 13 17 19 23 29)
;;;;   Groovy   squares them with the spread-dot operator and sums: 2397
;;;;   Kawa     adds one with Java's Long.sum, by colon notation, and
;;;;            divides exactly by the first of 6, 5, 4, 3, 2 that doesn't
;;;;            divide it evenly, escaping the search with call/cc: 1199/3,
;;;;            returned as its numerator and denominator
;;;;   JScheme  sums each one's digits in a named let: (20 3)
;;;;   Scala    matches the pair against a list pattern with a guard: 60
;;;;   Kotlin   pours it into a local data class, copies it, and sweetens it
;;;;            with when and the Elvis operator: 61
;;;;   ABCL     writes it with FORMAT, in Roman numerals and in English
;;;;
;;;; Each block is source in its language, as the language writes it.
;;;; $#@lang{ ... } (or $#[ ... ] for Clojure) splices the value of the
;;;; block inside into the language around it, passed as an object.
;;;;
;;;;   ./sbcl-polyglot --load examples/one-form.lisp
;;;;
;;;; prints: LXI (sixty-one)

(require :asdf)
(push (uiop:getcwd) asdf:*central-registry*)
(asdf:load-system "openldk/polyglot")
(polyglot:startup :verbose nil)
(setf *readtable* (polyglot:enable-syntax (copy-readtable)))

(format t "~&~A~%"
  #@abcl{                                ; ABCL: FORMAT's Roman numerals and English
    (let ((n $#@kt{                      // Kotlin: a local data class, copy, when, ?:
               data class Cup(val tea: Int, val sugar: Int? = null)
               val cup = Cup($#@sc{     // Scala: a list pattern with a guard
                               $#@jscheme{   ; JScheme: digit sums in a named let
                                 (map (lambda (n)
                                        (let loop ((n n) (sum 0))
                                          (if (< n 1)
                                              sum
                                              (loop (quotient n 10) (+ sum (remainder n 10))))))
                                      $#@kawa{   ; Kawa: Java by colon notation; call/cc; exact division
                                        (let* ((m (java.lang.Long:sum
                                                   $#@gv{   // Groovy: the spread-dot operator
                                                     $#[ (->> (iterate inc 2)   ; Clojure: lazy and infinite
                                                              (filter (fn [n] (not-any? #(zero? (rem n %))
                                                                                        (range 2 n))))
                                                              (take 10)) ]*.power(2).sum()
                                                   }
                                                   1))
                                               (d (call-with-current-continuation
                                                    (lambda (found)
                                                      (for-each (lambda (d)
                                                                  (if (not (= 0 (remainder m d))) (found d)))
                                                                '(6 5 4 3 2))
                                                      1)))
                                               (r (/ m d)))
                                          (list (numerator r) (denominator r)))
                                      })
                               } match {
                                 case List(a: Int, b: Int) if a > b => a * b
                                 case List(a: Int, b: Int) => a + b
                               }
                             }).copy(sugar = null)
               when { cup.tea > 40 -> cup.tea + (cup.sugar ?: 1); else -> cup.tea }
             }))
      (format nil "~@R (~R)" n n))
  })
