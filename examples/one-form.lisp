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
