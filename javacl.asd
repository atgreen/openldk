;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: OPENLDK; Base: 10 -*-
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later WITH Classpath-exception-2.0
;;;
;;; This file is part of OpenLDK.

(declaim (optimize (speed 0) (space 0) (debug 3)))

#+sbcl
(sb-ext:restrict-compiler-policy 'debug 3)

(asdf:defsystem "javacl"
  :description "Preloaded javac executable built on OpenLDK"
  :author "Anthony Green <green@moxielogic.com>"
  :license "GPL3+Classpath Exception"
  :version "1"
  :depends-on ("openldk")
  :components ((:file "src/javac"))
  :build-operation "program-op"
  :build-pathname "javacl"
  ;; Runtime entry point should run javac, not re-dump the image.
  :entry-point "openldk:javac-main-wrapper")

#+sb-core-compression
(defmethod asdf:perform ((o asdf:image-op) (c asdf:system))
  (uiop:dump-image (asdf:output-file o c) :executable t :compression t))
