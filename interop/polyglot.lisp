;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: OPENLDK.POLYGLOT; Base: 10 -*-
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

;;; Every JVM language OpenLDK runs, in one SBCL: Clojure, Kawa, JScheme,
;;; Scala, Kotlin, Groovy and ABCL, each with its own system (see
;;; docs/INTEROP.md), loaded together, with one call to start them and one
;;; to turn on their source syntax.
;;;
;;;   (asdf:load-system "openldk/polyglot")
;;;   (polyglot:startup)               ; already done in `make sbcl-polyglot`
;;;   (polyglot:enable-syntax)
;;;   #@kt{ (1..$(clojure:eval '(count [1 2 3]))).sum() }   => 6

(defpackage #:openldk.polyglot
  (:use #:cl)
  (:nicknames #:polyglot)
  (:export #:*languages*
           #:startup
           #:warm-up
           #:enable-syntax
           #:languages))

(in-package #:openldk.polyglot)

(defparameter *languages*
  '((:clojure . "OPENLDK.CLOJURE") (:kawa . "OPENLDK.KAWA") (:jscheme . "OPENLDK.JSCHEME")
    (:scala . "OPENLDK.SCALA") (:kotlin . "OPENLDK.KOTLIN") (:groovy . "OPENLDK.GROOVY")
    (:abcl . "OPENLDK.ABCL"))
  "Each language and the package of its system.")

(defun languages ()
  "The languages, as keywords."
  (mapcar #'car *languages*))

(defun startup (&key (languages (languages)) (verbose t))
  "Start each of LANGUAGES (by default, all of them).  A language that fails
to start is reported and skipped; return those that started."
  (loop for language in languages
        for package = (cdr (assoc language *languages*))
        when (progn
               (when verbose (format t "~&; starting ~(~A~)...~%" language))
               (handler-case (progn (uiop:symbol-call package "STARTUP") t)
                 (error (e)
                   (when verbose (format t "~&; ~(~A~) did not start: ~A~%" language e))
                   nil)))
          collect language))

(defparameter *warm-up*
  '((:clojure "EVAL-STRING" "(mapv inc (range 5)) (into {} [[:a 1]])")
    (:kawa "EVAL-STRING" "(map (lambda (x) (* x x)) (list 1 2 3))")
    (:jscheme "EVAL-STRING" "(map (lambda (x) (* x x)) (list 1 2 3))")
    (:scala "EVAL" "List(1, 2, 3).map(_ * 2).sum")
    (:kotlin "EVAL" "listOf(1, 2, 3).map { it * 2 }.sum()")
    (:groovy "EVAL" "[3, 1, 2].sort().collect { it * 2 }")
    (:abcl "EVAL-STRING" "(mapcar (function 1+) (list 1 2 3))"))
  "For each language, a function of its package and code that exercises
its common paths, so an image saved after WARM-UP has them compiled.")

(defun warm-up (&optional (languages (languages)))
  "Run a little code in each of LANGUAGES (started ones), so it is compiled
before an image is saved.  A failure is reported, not signalled."
  (dolist (language languages languages)
    (destructuring-bind (function code) (cdr (assoc language *warm-up*))
      (handler-case (uiop:symbol-call (cdr (assoc language *languages*)) function code)
        (error (e)
          (format t "~&; warming up ~(~A~) failed: ~A~%" language e))))))

(defun enable-syntax (&optional (readtable *readtable*))
  "Turn on #@LANGUAGE{ ... } for every language, and Clojure's #[ ... ], in
READTABLE (by default, the current one); return it."
  (openldk.interop:enable-source-syntax readtable)
  (clojure:enable-syntax readtable))
