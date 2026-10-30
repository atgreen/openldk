;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER; Base: 10 -*-
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

;;;; One form, seven languages: the macro version
;;;;
;;;; examples/one-form.lisp nests seven languages' source in one form.
;;;; Here each language's step is a Lisp macro whose template is that
;;;; language's source, as the language writes it, with $,x splicing in the
;;;; value of the macro's argument form.  The seven steps then compose like
;;;; any Lisp functions, into one form that reads as a pipeline:
;;;;
;;;;   (abcl-words (kotlin-sweeten (scala-judge (jscheme-digit-sums
;;;;     (kawa-exact-fraction (groovy-sum-of-squares (clojure-primes 10)))))))
;;;;
;;;;   Clojure  takes N primes from an infinite lazy sequence
;;;;   Groovy   squares them with the spread-dot operator and sums
;;;;   Kawa     adds one with Java's Long.sum, by colon notation, and divides
;;;;            exactly by the first of 6, 5, 4, 3, 2 that doesn't divide it
;;;;            evenly, escaping the search with call/cc; it returns the
;;;;            numerator and denominator
;;;;   JScheme  sums each one's digits in a named let
;;;;   Scala    matches the pair against a list pattern with a guard
;;;;   Kotlin   pours it into a local data class, copies it, and sweetens it
;;;;            with when and the Elvis operator
;;;;   ABCL     writes it with FORMAT, in Roman numerals and in English
;;;;
;;;; Each macro's block is compiled once, the first time it runs, and then
;;;; called with the spliced values.
;;;;
;;;;   ./sbcl-polyglot --load examples/one-form-macros.lisp
;;;;
;;;; prints: LXI (sixty-one)
(require :asdf)
(push (uiop:getcwd) asdf:*central-registry*)
(asdf:load-system "openldk/polyglot")
(polyglot:startup :verbose nil)
(setf *readtable* (polyglot:enable-syntax (copy-readtable)))

(defmacro clojure-primes (n)
  "The first N primes, from an infinite lazy sequence."
  `#[ (->> (iterate inc 2)
           (filter (fn [k] (not-any? #(zero? (rem k %)) (range 2 k))))
           (take $,n)) ])

(defmacro groovy-sum-of-squares (numbers)
  "The sum of the squares of NUMBERS, by Groovy's spread-dot operator."
  `#@gv{ ($,numbers)*.power(2).sum() })

(defmacro kawa-exact-fraction (n)
  "N + 1, by Java's Long.sum in Kawa's colon notation, divided exactly by
the first of 6, 5, 4, 3, 2 that doesn't divide it evenly -- the search
escaping through a continuation, call/cc -- as numerator and denominator."
  `#@kawa{ (let* ((m (java.lang.Long:sum $,n 1))
                  (d (call-with-current-continuation
                       (lambda (found)
                         (for-each (lambda (d) (if (not (= 0 (remainder m d))) (found d)))
                                   '(6 5 4 3 2))
                         1)))
                  (r (/ m d)))
             (list (numerator r) (denominator r))) })

(defmacro jscheme-digit-sums (numbers)
  "The digit sum of each of NUMBERS, in a named let."
  `#@jscheme{ (map (lambda (n)
                     (let loop ((n n) (sum 0))
                       (if (< n 1) sum (loop (quotient n 10) (+ sum (remainder n 10))))))
                   $,numbers) })

(defmacro scala-judge (pair)
  "A two-element list, matched against a list pattern with a guard."
  `#@sc{ $,pair match {
           case List(a: Int, b: Int) if a > b => a * b
           case List(a: Int, b: Int) => a + b
         } })

(defmacro kotlin-sweeten (tea)
  "TEA poured into a local data class, copied, and sweetened when strong."
  `#@kt{ data class Cup(val tea: Int, val sugar: Int? = null)
         val cup = Cup($,tea).copy(sugar = null)
         when { cup.tea > 40 -> cup.tea + (cup.sugar ?: 1); else -> cup.tea } })

(defmacro abcl-words (n)
  "N in Roman numerals and in English, by ABCL's FORMAT.  N's form is
spliced once and bound in ABCL, so it runs once."
  `#@abcl{ (let ((n $,n)) (format nil "~@R (~R)" n n)) })

;;; The one form

(format t "~&~A~%"
        (abcl-words
         (kotlin-sweeten
          (scala-judge
           (jscheme-digit-sums
            (kawa-exact-fraction
             (groovy-sum-of-squares
              (clojure-primes 10))))))))
