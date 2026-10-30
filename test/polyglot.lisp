;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: OPENLDK-POLYGLOT-TESTS; Base: 10 -*-
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

;;;; Tests for openldk/polyglot: every language in one SBCL, and foreign
;;;; source files as ASDF components (FiveAM).  `make check-polyglot` runs
;;;; them in sbcl-polyglot.
(defpackage :openldk-polyglot-tests
  (:use :cl :fiveam))

(in-package :openldk-polyglot-tests)

(def-suite :openldk-polyglot-tests
  :description "Every JVM language at once, and ASDF components in each.")

(in-suite :openldk-polyglot-tests)

(test every-language-started
  (is (equal (polyglot:languages) (polyglot:startup :verbose nil))))

(test asdf-components
  (let ((asdf:*central-registry*
          (cons (asdf:system-relative-pathname "openldk" "test/polyglot-demo/")
                asdf:*central-registry*)))
    (asdf:load-system "polyglot-demo" :force '("polyglot-demo")))
  (is (= 14 (openldk.interop:java-static "Stats" "sumOfSquares(I)" 3)) "Scala")
  (is (< 3.14 (kotlin:call "ShapesKt" "circleArea" 1d0) 3.15) "Kotlin")
  (is (= 15 (kotlin:call "ShapesKt" "squaresPlusOne" 3)) "Kotlin, calling Scala")
  (is (string= "Hello, Lisp!"
               (openldk.interop:lstring
                (openldk.interop:java-static "Greet" "hello(Ljava/lang/String;)" (openldk.interop:jstring "lisp"))))
      "Groovy")
  (is (string= "HI!" (clojure:funcall "words/shout" "hi")) "Clojure")
  (is (= 2432902008176640000 (kawa:funcall "kawa-fact" 20)) "Kawa")
  (is (= 55 (jscheme:funcall "jfib" 10)) "JScheme")
  (is (= 3 (abcl:funcall "TOWER-HEIGHT")) "ABCL")
  (is (search (string #\Newline) (kotlin:call "ShapesKt" "wrapped" "a library from Maven, wrapped"))
      "Kotlin, calling a Maven dependency")
  (is (string= "Hello"
               (openldk.interop:lstring
                (openldk.interop:java-static "org.apache.commons.text.WordUtils" "capitalize(Ljava/lang/String;)"
                                             (openldk.interop:jstring "hello"))))
      "Lisp, calling it"))
