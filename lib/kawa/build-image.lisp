;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: OPENLDK; Base: 10 -*-
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
