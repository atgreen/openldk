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
  :serial t
  :components ((:file "src/package")
               (:file "src/global-state")
               (:file "src/debug")
               (:file "src/monitor")
               (:file "src/context")
               (:file "src/bootstrap")
               (:file "src/strings")
               (:file "src/arrays")
               (:file "src/math")
               (:file "src/opcodes")
               (:file "src/ir")
               (:file "src/bc-to-ir")
               (:file "src/classpath")
               (:file "src/basic-block")
               (:file "src/codegen")
               (:file "src/descriptors")
               (:file "src/classfile")
               (:file "src/native")
               (:file "src/streams")
               (:file "src/zip")
               (:file "src/reflection")
               (:file "src/openldk"))
  :around-compile
  "(lambda (thunk)
     (cl-annot:enable-annot-syntax)
     (funcall thunk))"
  :depends-on (:cl-annot :whereiseveryone.command-line-args :flexi-streams
               :alexandria :serapeum
               :zip :str :defclass-std :fast-io :bitio :pathname-utils
               :cl-store :trivial-backtrace :fset :bordeaux-threads
               :float-features :local-time :closer-mop :slynk
               :file-attributes :trivial-garbage :precise-time
               :trivial-gray-streams :cl-murmurhash)
  :build-operation "program-op"
  :build-pathname "javacl"
  :entry-point "openldk:make-javac-image")

#+sb-core-compression
(defmethod asdf:perform ((o asdf:image-op) (c asdf:system))
  (uiop:dump-image (asdf:output-file o c) :executable t :compression t))
