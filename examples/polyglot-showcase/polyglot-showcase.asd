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

;;;; The OpenLDK polyglot showcase: an ASDF system with a Java library from
;;;; Maven Central, a Kotlin source file compiled against it, and a Lisp
;;;; program that uses them and six other JVM languages in one process.
;;;; See README.md.
(asdf:defsystem "polyglot-showcase"
  :description "Every OpenLDK polyglot feature, in one small bookshop"
  :defsystem-depends-on ("openldk/polyglot")
  :serial t
  :components ((:maven-jar "commons-lang3"          ; a Java library, from Maven Central,
                :coordinates "org.apache.commons:commons-lang3:3.14.0")
               (:maven-jar "commons-text"           ; checked against Maven's SHA-1
                :coordinates "org.apache.commons:commons-text:1.12.0")
               (:kotlin-file "catalog")             ; compiled by kotlinc, against commons-text
               (:file "showcase")))                 ; Lisp, with six languages inline
