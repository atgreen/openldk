;;;; The OpenLDK polyglot showcase
;;;;
;;;; A small bookshop, run from Lisp.  The catalog is Kotlin (catalog.kt,
;;;; compiled by ASDF against a Maven library); Scala, Clojure, Groovy, Kawa,
;;;; JScheme and ABCL join in through #@LANGUAGE{ ... } blocks, written in
;;;; their own syntax, all in this one SBCL process.  Each section shows one
;;;; feature; (polyglot-showcase:run) runs them all.

(defpackage #:polyglot-showcase
  (:use #:cl #:openldk.interop)
  (:nicknames #:showcase)
  (:export #:run))

(in-package #:polyglot-showcase)

;;; Every language running when this file compiles, so typed #@kt{ } blocks
;;; compile with it (see WITH-TAX), and their source syntax turned on.
(eval-when (:compile-toplevel :load-toplevel :execute)
  (polyglot:startup :verbose nil)
  (setf *readtable* (polyglot:enable-syntax (copy-readtable))))

(defun section (title)
  (format t "~2&~A~%~A~%" title (make-string (length title) :initial-element #\-)))

(defun say (control &rest arguments)
  (format t "~&  ~?~%" control arguments))

;;; The catalog: a java.util.List of Kotlin Book data-class instances.

(defun books ()
  (java-static "CatalogKt" "catalog()"))

(defun field (object name)
  "The field NAME of a Java OBJECT (a Kotlin data class), as Lisp data."
  (kotlin:to-lisp (cdr (assoc name (java-fields object) :test #'string=))))

;;; 1. ASDF, Maven, and a Kotlin source file

(defun show-the-build ()
  (section "1. Built by ASDF: a Maven library and a Kotlin file")
  (dolist (component (asdf:component-children (asdf:find-system "polyglot-showcase")))
    (say "~(~A~) ~S" (class-name (class-of component)) (asdf:component-name component)))
  (say "commons-text, from Lisp: the initials of ~S are ~S"
       (field (elt (java-sequence (books)) 0) "title")
       (lstring (java-static "org.apache.commons.text.WordUtils" "initials(Ljava/lang/String;)"
                             (jstring (field (elt (java-sequence (books)) 0) "title"))))))

;;; 2. Records: a Kotlin data class's fields

(defun show-records ()
  (section "2. Records: a Kotlin data class, as an alist")
  (say "~S" (java-fields (elt (java-sequence (books)) 0) :convert #'kotlin:to-lisp)))

;;; 3. Collections without copying

(defun show-collections ()
  (section "3. A Kotlin List, as a Lisp sequence")
  (let ((catalog (java-sequence (books))))
    (say "LENGTH: ~D books, read through the java.util.List" (length catalog))
    (say "FIND by year 1871: ~A" (field (find 1871 catalog :key (lambda (b) (field b "year"))) "title"))
    (say "REDUCE over the prices: ~,2F" (reduce #'+ catalog :key (lambda (b) (field b "price"))))
    (say "POSITION of the cheapest: ~D"
         (let ((cheapest (reduce #'min catalog :key (lambda (b) (field b "price")))))
           (position cheapest catalog :key (lambda (b) (field b "price")))))))

;;; 4. Foreign functions are Lisp functions

(defun show-foreign-functions ()
  (section "4. Scala and Clojure functions, called by MAPCAR and SORT")
  (let ((with-vat #@sc{ (price: Double) => BigDecimal(price * 1.2).setScale(2, BigDecimal.RoundingMode.HALF_UP).toDouble })
        (word-count (clojure:eval-string "(fn [s] (count (clojure.string/split s #\" \")))"))
        (prices (map 'list (lambda (b) (field b "price")) (java-sequence (books))))
        (titles (map 'list (lambda (b) (field b "title")) (java-sequence (books)))))
    (say "a Scala function, mapped over the prices with VAT: ~S" (mapcar with-vat prices))
    (say "titles sorted by a Clojure function's word count:")
    (dolist (title (sort titles #'< :key word-count))
      (say "  ~D  ~A" (funcall word-count title) title))))

;;; 5. A Java class defined in Lisp

(define-java-class half-price (:implements ("java.util.function.Function"))
    ((uses :initform 0))
  ("apply(Ljava/lang/Object;)Ljava/lang/Object;" (self price)
    (incf uses)
    (java-double (/ (if (numberp price) price (java-call "doubleValue()D" price)) 2))))

(defun show-java-class ()
  (section "5. A Java class written in Lisp, used by Kotlin and Groovy")
  (let ((sale (make-java-object 'half-price))
        (book (elt (java-sequence (books)) 3)))
    (say "its class: ~A" (lstring (java-call "getName()" (java-call "getClass()" sale))))
    (say "Kotlin calls it as a java.util.function.Function: ~A costs ~,2F on sale"
         (field book "title") (kotlin:call "CatalogKt" "discounted" book sale))
    (say "Groovy calls it by reflection: ~A" #@gv{ "half of 10.0 is " + $sale.apply(10.0) })))

;;; 6. Errors: typed conditions, restarts, and Lisp errors in Java

(defun show-errors ()
  (section "6. Errors across languages")
  (say "Kotlin's NumberFormatException is a ~(~A~); USE-VALUE answers 0: ~A"
       'foreign-number-format-error
       (handler-bind ((foreign-number-format-error (lambda (c) (declare (ignore c)) (use-value 0))))
         #@kt{ "forty-two".toInt() }))
  (say "Scala's ArithmeticException is an ARITHMETIC-ERROR: ~A"
       (handler-case #@sc{ val zero = 0; 1 / zero }
         (arithmetic-error (e) (foreign-error-java-class e))))
  (say "and a Lisp error in a function Kotlin called: ~A"
       (kotlin:call "CatalogKt" "guarded" (lambda () (/ 1 (random 1))))))

;;; 7. A precompiled block

(defun with-tax (cents)
  "CENTS plus 20%: a #@kt{ } block whose splice has a static type, so it
was compiled with this file and its classes are in the fasl."
  (declare (type (signed-byte 32) cents))
  #@kt{ $(the (signed-byte 32) cents) * 6 / 5 })

(defun show-precompiled ()
  (section "7. A Kotlin block compiled with this file")
  (say "1000 cents with tax: ~D (no Kotlin compiler ran for it just now)" (with-tax 1000)))

;;; 8. Lisp and Clojure, recursing through each other

(defun fib (n)
  "Fibonacci numbers by a #[ ] Clojure block that calls back into Lisp:
each level of the recursion crosses between the languages."
  #[ (if (< $n 2) $n (+ ($#'fib (- $n 1)) ($#'fib (- $n 2)))) ])

(defun show-recursion ()
  (section "8. Lisp and Clojure, in one recursion")
  (say "(fib 15) => ~D, crossing between SBCL and Clojure 1973 times" (fib 15)))

;;; 9. Coroutines

(defun show-coroutines ()
  (section "9. Kotlin coroutines")
  (say "the catalog with 20% tax, priced by concurrent coroutines: ~,2F"
       (kotlin:call "CatalogKt" "totalWithTax" (books) 0.2d0)))

;;; 10. Macros writing foreign code

(defmacro groovy-shout (string)
  `#@gv{ ($,string).toUpperCase() + '!' })

(defmacro scala-reverse (string)
  `#@sc{ ($,string).reverse })

(defun show-macros ()
  (section "10. Lisp macros whose expansions are Groovy and Scala")
  (say "~A" (groovy-shout (format nil "~R books" (length (java-sequence (books))))))
  (say "~A" (scala-reverse "stressed")))

;;; 11. Lisp data, handed to Java without copying

(defun show-lisp-views ()
  (section "11. A Lisp vector, read by Groovy as a java.util.List")
  (let* ((order (vector (cons "Through The Looking-glass" 8.25) (cons "The Hunting Of The Snark" 5.0)))
         (lines (lisp-list-view order :convert (lambda (line)
                                                 (groovy:from-lisp
                                                  (let ((table (make-hash-table :test #'equal)))
                                                    (setf (gethash "title" table) (car line)
                                                          (gethash "price" table) (cdr line))
                                                    table))))))
    (format t "~A~%"
            #@gv{ def rows = $lines
                  def total = rows.sum { it.price }
                  (rows.collect { "  ${it.title.padRight(32, '.')} ${String.format('%6.2f', it.price)}" }
                   + "  ${'Total'.padRight(32, '.')} ${String.format('%6.2f', total)}").join('\n') })))

;;; 12. Six more languages, briefly

(defun show-languages ()
  (section "12. The rest of the table")
  (let ((prices (map 'list (lambda (b) (round (field b "price"))) (java-sequence (books)))))
    (say "Kawa, escaping a search with call/cc: the first book under 6 costs ~A"
         #@kawa{ (call-with-current-continuation
                   (lambda (found)
                     (for-each (lambda (p) (if (< p 6) (found p))) $prices)
                     #f)) })
    (say "Kawa's exact rationals: ~A among 3 people is ~A each"
         (reduce #'+ prices) #@kawa{ (/ (apply + $prices) 3) })
    (say "JScheme reverses a string: ~A"
         #@jscheme{ (list->string (reverse (string->list "a man a plan"))) })
    (say "ABCL, a Common Lisp in the JVM, in words: ~A"
         #@abcl{ (format nil "~R" (length $prices)) })
    (say "ABCL calls Java itself: Math.max(3, 7) = ~A" #@abcl{ (jstatic "max" "java.lang.Math" 3 7) })
    (say "Groovy's decimals are exact: 0.1 + 0.2 = ~A" (groovy:eval "0.1 + 0.2"))))

(defun run ()
  "Run the showcase."
  (format t "~&The OpenLDK polyglot showcase~%=============================~%")
  (show-the-build)
  (show-records)
  (show-collections)
  (show-foreign-functions)
  (show-java-class)
  (show-errors)
  (show-precompiled)
  (show-recursion)
  (show-coroutines)
  (show-macros)
  (show-lisp-views)
  (show-languages)
  (terpri)
  t)
