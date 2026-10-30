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

;;;
;;;
;;; This file is part of OpenLDK.

;; No local optimize policy: inherit openldk's (speed 3) (safety 1)
;; (debug 1).  A (debug 3) declaim here — and especially
;; sb-ext:restrict-compiler-policy — would leak into every system
;; compiled in the same session, including the openldk runtime whose
;; JIT policy gets baked into the dumped image.
(asdf:defsystem "javacl"
  :description "Preloaded javac executable built on OpenLDK"
  :author "Anthony Green <green@moxielogic.com>"
  :license "GPL3+Classpath Exception"
  :version "1"
  :depends-on ("openldk")
  :components ((:file "src/javac"))
  :build-operation "program-op"
  :build-pathname "javacl"
  ;; Build-time entry point constructs the image; it embeds runtime toplevel in make-javac-image.
  :entry-point "javacl:make-javac-image")

#+sb-core-compression
(defmethod asdf:perform ((o asdf:image-op) (c asdf:system))
  (uiop:dump-image (asdf:output-file o c) :executable t :compression t))
