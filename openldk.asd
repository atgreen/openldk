;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: OPENLDK; Base: 10 -*-
;;;
;;; Copyright (C) 2023, 2024, 2025  Anthony Green <green@moxielogic.com>
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

;; (safety 1) keeps runtime type/bounds checks so Lisp-level faults in
;; JIT-compiled Java code surface as conditions we can translate into
;; Java exceptions; (debug 1) preserves enough frame info for the
;; caller-class checks and backtraces that security and diagnostics
;; rely on.  This policy also governs runtime JIT compilation, since
;; the proclamation is baked into the dumped images.
;;
;; It is proclaimed around each file's compilation, not by a DECLAIM
;; here: LOAD rebinds the policy, so a proclamation in this file was
;; undone once ASDF had loaded it, and OpenLDK and the code it JIT-compiles
;; were built at the default (speed 1).  At speed 3 the Spring Boot
;; sample starts 11% faster from the code cache.
(defun call-with-openldk-policy (thunk)
  "Compile THUNK with the optimization policy used by the runtime and JIT."
  (proclaim '(optimize (speed 3) (safety 1) (debug 1)))
  (funcall thunk))

(asdf:defsystem "openldk"
  :description "Java in Common Lisp"
  :around-compile call-with-openldk-policy
  :author "Anthony Green <green@moxielogic.com>"
  :license "GPL3+Classpath Exception"
  :version "1"
  :serial t
  :components ((:file "src/package")
               (:file "src/conditions")
               (:file "src/fetch")
               (:file "src/global-state")
               (:file "src/memory")
               (:file "src/fiber")
               (:file "src/java-gf")
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
               (:file "src/jrt")
               (:file "src/basic-block")
               (:file "src/codegen")
               (:file "src/descriptors")
               (:file "src/classfile")
               (:file "src/verifier")
               (:file "src/interpreter")
               ;; JDK native-method implementations, split by domain;
               ;; loaded in this order (chunks of the former src/native.lisp).
               (:file "src/native/support")
               (:file "src/native/frames")
               (:file "src/native/bytearray")
               (:file "src/native/threads")
               (:file "src/native/management")
               (:file "src/native/jsr292")
               (:file "src/native/modules")
               (:file "src/native/unsafe")
               (:file "src/native/reflection")
               (:file "src/native/system")
               (:file "src/native/files")
               (:file "src/native/constantpool")
               (:file "src/native/methodhandles")
               (:file "src/native/unsafe-tail")
               (:file "src/native/process")
               (:file "src/native/net")
               (:file "src/native/ordering")
               (:file "src/native/graphics")
               (:file "src/native/fonts")
               (:file "src/native/references")
               (:file "src/zlib-native")
               (:file "src/url")
               (:file "src/streams")
               (:file "src/zip")
               (:file "src/reflection")
               (:file "src/optimizer")
               (:file "src/fasl-cache")
               (:file "src/aot")
               (:file "src/openldk")
               (:file "src/jni")
               (:file "src/cli"))
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
               :trivial-gray-streams :cl-murmurhash :sb-md5)
  :build-operation "program-op"
  :build-pathname "openldk"
  :entry-point "openldk:make-image"
  :in-order-to ((test-op (test-op "openldk/tests"))))

;; Unit tests for OpenLDK's pure helpers (FiveAM). Run with
;; (asdf:test-system :openldk). Kept in a secondary system so the main runtime
;; has no test-time dependencies.
(defsystem "openldk/tests"
  :description "Unit tests for OpenLDK."
  :depends-on ("openldk" "fiveam")
  :pathname "test"
  :components ((:file "tests"))
  :perform (test-op (o c)
             (unless (uiop:symbol-call :fiveam '#:run! :openldk-tests)
               (error "OpenLDK unit tests failed."))))

#+sb-core-compression
(defmethod asdf:perform ((o asdf:image-op) (c asdf:system))
  (uiop:dump-image (asdf:output-file o c) :executable t :compression t))

;;; Using other JVM languages from Lisp, on OpenLDK (see docs/INTEROP.md).

(asdf:defsystem "openldk/interop"
  :description "Shared core of OpenLDK's language systems: Java calls, values, callbacks, errors"
  :author "Anthony Green <green@moxielogic.com>"
  :license "GPL3+Classpath Exception"
  :depends-on ("openldk")
  :components ((:file "interop/core"))
  :in-order-to ((test-op (test-op "openldk/interop/tests"))))

(asdf:defsystem "openldk/interop/tests"
  :description "Tests for openldk/interop"
  :depends-on ("openldk/interop" "fiveam")
  :pathname "test"
  :components ((:file "interop"))
  :perform (test-op (o c)
             (unless (uiop:symbol-call :fiveam '#:run! :openldk-interop-tests)
               (error "openldk/interop tests failed"))))

(asdf:defsystem "openldk/jscheme"
  :description "Use Peter Norvig's JScheme from Common Lisp"
  :author "Anthony Green <green@moxielogic.com>"
  :license "GPL3+Classpath Exception"
  :depends-on ("openldk/interop")
  :components ((:file "interop/jscheme"))
  :in-order-to ((test-op (test-op "openldk/jscheme/tests"))))

(asdf:defsystem "openldk/jscheme/tests"
  :description "Tests for openldk/jscheme; need `make jscheme`"
  :depends-on ("openldk/jscheme" "fiveam")
  :pathname "test"
  :components ((:file "jscheme"))
  :perform (test-op (o c)
             (unless (uiop:symbol-call :fiveam '#:run! :openldk-jscheme-tests)
               (error "openldk/jscheme tests failed"))))

(asdf:defsystem "openldk/clojure"
  :description "Use Clojure from Common Lisp"
  :author "Anthony Green <green@moxielogic.com>"
  :license "GPL3+Classpath Exception"
  :depends-on ("openldk/interop")
  :components ((:file "interop/clojure"))
  :in-order-to ((test-op (test-op "openldk/clojure/tests"))))

(asdf:defsystem "openldk/clojure/tests"
  :description "Tests for openldk/clojure; need `make clojure`"
  :depends-on ("openldk/clojure" "fiveam")
  :pathname "test"
  :components ((:file "clojure"))
  :perform (test-op (o c)
             (unless (uiop:symbol-call :fiveam '#:run! :openldk-clojure-tests)
               (error "openldk/clojure tests failed"))))

(asdf:defsystem "openldk/scala"
  :description "Use Scala from Common Lisp"
  :author "Anthony Green <green@moxielogic.com>"
  :license "GPL3+Classpath Exception"
  :depends-on ("openldk/interop" "sb-introspect")
  :components ((:file "interop/scala"))
  :in-order-to ((test-op (test-op "openldk/scala/tests"))))

(asdf:defsystem "openldk/scala/tests"
  :description "Tests for openldk/scala; need `make sbcl-scala`"
  :depends-on ("openldk/scala" "fiveam")
  :pathname "test"
  :components ((:file "scala"))
  :perform (test-op (o c)
             (unless (uiop:symbol-call :fiveam '#:run! :openldk-scala-tests)
               (error "openldk/scala tests failed"))))

(asdf:defsystem "openldk/kotlin"
  :description "Use Kotlin from Common Lisp"
  :author "Anthony Green <green@moxielogic.com>"
  :license "GPL3+Classpath Exception"
  :depends-on ("openldk/interop" "sb-introspect")
  :components ((:file "interop/kotlin"))
  :in-order-to ((test-op (test-op "openldk/kotlin/tests"))))

(asdf:defsystem "openldk/kotlin/tests"
  :description "Tests for openldk/kotlin; need `make sbcl-kotlin`"
  :depends-on ("openldk/kotlin" "fiveam")
  :pathname "test"
  :components ((:file "kotlin"))
  :perform (test-op (o c)
             (unless (uiop:symbol-call :fiveam '#:run! :openldk-kotlin-tests)
               (error "openldk/kotlin tests failed"))))

(asdf:defsystem "openldk/groovy"
  :description "Use Groovy from Common Lisp"
  :author "Anthony Green <green@moxielogic.com>"
  :license "GPL3+Classpath Exception"
  :depends-on ("openldk/interop")
  :components ((:file "interop/groovy"))
  :in-order-to ((test-op (test-op "openldk/groovy/tests"))))

(asdf:defsystem "openldk/groovy/tests"
  :description "Tests for openldk/groovy; need `make sbcl-groovy`"
  :depends-on ("openldk/groovy" "fiveam")
  :pathname "test"
  :components ((:file "groovy"))
  :perform (test-op (o c)
             (unless (uiop:symbol-call :fiveam '#:run! :openldk-groovy-tests)
               (error "openldk/groovy tests failed"))))

(asdf:defsystem "openldk/polyglot"
  :description "Every JVM language OpenLDK runs, in one SBCL"
  :author "Anthony Green <green@moxielogic.com>"
  :license "GPL3+Classpath Exception"
  :depends-on ("openldk/clojure" "openldk/kawa" "openldk/jscheme" "openldk/scala"
               "openldk/kotlin" "openldk/groovy" "openldk/abcl")
  :components ((:file "interop/polyglot"))
  :in-order-to ((test-op (test-op "openldk/polyglot/tests"))))

(asdf:defsystem "openldk/polyglot/tests"
  :description "Tests for openldk/polyglot; need `make sbcl-polyglot`"
  :depends-on ("openldk/polyglot" "fiveam")
  :pathname "test"
  :components ((:file "polyglot"))
  :perform (test-op (o c)
             (unless (uiop:symbol-call :fiveam '#:run! :openldk-polyglot-tests)
               (error "openldk/polyglot tests failed"))))

(asdf:defsystem "openldk/kawa"
  :description "Use Kawa Scheme from Common Lisp"
  :author "Anthony Green <green@moxielogic.com>"
  :license "GPL3+Classpath Exception"
  :depends-on ("openldk/interop")
  :components ((:file "interop/kawa"))
  :in-order-to ((test-op (test-op "openldk/kawa/tests"))))

(asdf:defsystem "openldk/kawa/tests"
  :description "Tests for openldk/kawa; need `make kawa`"
  :depends-on ("openldk/kawa" "fiveam")
  :pathname "test"
  :components ((:file "kawa"))
  :perform (test-op (o c)
             (unless (uiop:symbol-call :fiveam '#:run! :openldk-kawa-tests)
               (error "openldk/kawa tests failed"))))


(asdf:defsystem "openldk/abcl"
  :description "Use ABCL (Armed Bear Common Lisp) from SBCL"
  :author "Anthony Green <green@moxielogic.com>"
  :license "GPL3+Classpath Exception"
  :depends-on ("openldk/interop")
  :components ((:file "interop/abcl"))
  :in-order-to ((test-op (test-op "openldk/abcl/tests"))))

(asdf:defsystem "openldk/abcl/tests"
  :description "Tests for openldk/abcl; need `make abcl`"
  :depends-on ("openldk/abcl" "fiveam")
  :pathname "test"
  :components ((:file "abcl"))
  :perform (test-op (o c)
             (unless (uiop:symbol-call :fiveam '#:run! :openldk-abcl-tests)
               (error "openldk/abcl tests failed"))))
