;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: JICL; Base: 10 -*-
;;;
;;; Copyright (C) 2023  Anthony Green <green@moxielogic.com>
;;;
;;; This program is free software: you can redistribute it and/or
;;; modify it under the terms of the GNU Affero General Public License
;;; as published by the Free Software Foundation, either version 3 of
;;; the License, or (at your option) any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; Affero General Public License for more details.
;;;
;;; You should have received a copy of the GNU Affero General Public
;;; License along with this program.  If not, see
;;; <http://www.gnu.org/licenses/>.

(asdf:defsystem #:openldk
  :description "Java in Common Lisp"
  :author "Anthony Green <green@moxielogic.com>"
  :license "GPL3+Classpath Exception"
  :version "1"
  :serial t
  :components ((:file "package")
               (:file "opcodes")
               (:file "bootstrap")
               (:file "bytecode-branches")
               (:file "native")
               (:file "context")
               (:file "classfile")
               (:file "descriptors")
               (:file "ssa")
               (:file "bytecode-to-ssa")
               (:file "codegen")
               (:file "openldk"))
  :depends-on (:bitio :fast-io :unix-opts :split-sequence :flexi-streams :cl-containers :closer-mop)
  :build-operation "program-op"
  :build-pathname "openldk"
  :entry-point "openldk:main")

#+sb-core-compression
(defmethod asdf:perform ((o asdf:image-op) (c asdf:system))
  (uiop:dump-image (asdf:output-file o c) :executable t :compression t))
