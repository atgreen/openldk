;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: OPENLDK; Base: 10 -*-
;;;
;;; Copyright (C) 2023, 2024, 2025  Anthony Green <green@moxielogic.com>
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

(declaim (optimize (speed 0) (space 0) (debug 3)))
(sb-ext:restrict-compiler-policy 'debug 3)

(asdf:defsystem #:openldk
  :description "Java in Common Lisp"
  :author "Anthony Green <green@moxielogic.com>"
  :license "GPL3+Classpath Exception"
  :version "1"
  :serial t
  :components ((:file "src/package")
							 (:file "src/debug")
							 (:file "src/monitor")
							 (:file "src/context")
							 (:file "src/bootstrap")
               (:file "src/strings")
							 (:file "src/opcodes")
							 (:file "src/ir")
							 (:file "src/bc-to-ir")
							 (:file "src/classpath")
							 (:file "src/basic-block")
							 (:file "src/codegen")
							 (:file "src/descriptors")
							 (:file "src/classfile")
							 (:file "src/native")
							 (:file "src/openldk"))
  :around-compile
  "(lambda (thunk)
     (cl-annot:enable-annot-syntax)
     (funcall thunk))"
  :depends-on (:cl-annot :whereiseveryone.command-line-args :flexi-streams :zip :str
                         :defclass-std :fast-io :bitio :pathname-utils :cl-store :trivial-backtrace
                         :fset :bordeaux-threads :float-features :local-time :closer-mop
               :slynk :file-attributes :trivial-garbage)
  :build-operation "program-op"
  :build-pathname "openldk"
  :entry-point "openldk:make-image")

#+sb-core-compression
(defmethod asdf:perform ((o asdf:image-op) (c asdf:system))
  (uiop:dump-image (asdf:output-file o c) :executable t :compression t))
