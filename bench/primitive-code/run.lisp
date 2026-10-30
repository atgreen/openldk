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

;;; From the repository root (JAVA_HOME must name a JDK 25):
;;; CL_SOURCE_REGISTRY="$PWD//:" LDK_CODE_CACHE=off sbcl --dynamic-space-size 8192 \
;;;   --script bench/primitive-code/run.lisp
(require :asdf)
(asdf:load-system :openldk/interop)

(let ((directory (asdf:system-relative-pathname :openldk ".cache/bench/primitive-code/")))
  (ensure-directories-exist directory)
  (uiop:run-program
   (list (namestring (merge-pathnames "bin/javac" (uiop:ensure-directory-pathname (uiop:getenv "JAVA_HOME"))))
         "--release" "25" "-d" (namestring directory)
         (namestring (asdf:system-relative-pathname :openldk "bench/primitive-code/PrimitiveCodeBench.java")))
   :output t :error-output t)
  (openldk.interop:start-runtime :classpath (list directory)))

(defun primitive-bench-entry (class optimized)
  ;; Separate classes ensure lazy compilation observes each configuration.
  ;; Disable disk caching even when the caller omitted LDK_CODE_CACHE=off.
  (let ((openldk::*enable-word-longs* optimized)
        (openldk::*enable-primitive-entries* optimized)
        (openldk::*enable-primitive-result-arithmetic* optimized)
        (openldk::*fasl-cache-directory* nil))
    (openldk.interop:java-static class "loop(JI)J" 1 100)
    (symbol-function
     (openldk::static-method-symbol
      (concatenate 'string class ".loop(JI)J")
      (openldk::ldk-class-package (openldk.interop:java-load class))))))

(let ((baseline (primitive-bench-entry "PrimitiveCodeBaseline" nil))
      (optimized (primitive-bench-entry "PrimitiveCodeOptimized" t)))
  (dolist (initial '(0 1 -1 4611686018427387904 9223372036854775807 -9223372036854775808))
    (dolist (count '(-1 0 1 2 63 1000))
      (assert (= (funcall baseline initial count) (funcall optimized initial count)))))
  (let ((expected (funcall baseline 1 10000000)))
    (dolist (entry (list (cons :baseline baseline) (cons :optimized optimized)))
      (dotimes (trial 3)
        (sb-ext:gc :full t)
        (let* ((before (sb-ext:get-bytes-consed))
               (start (get-internal-real-time))
               (result (funcall (cdr entry) 1 10000000))
               (seconds (/ (- (get-internal-real-time) start) internal-time-units-per-second))
               (bytes (- (sb-ext:get-bytes-consed) before)))
          (assert (= result expected))
          (when (eql (car entry) :optimized) (assert (< bytes 65536)))
          (format t "~&~A trial=~D seconds=~,6F bytes=~D checksum=~D~%"
                  (car entry) trial seconds bytes result))))))
