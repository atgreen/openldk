;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: OPENLDK; Base: 10 -*-
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later WITH Classpath-exception-2.0
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
