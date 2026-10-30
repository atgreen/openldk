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

;;; CL_SOURCE_REGISTRY="$PWD//:" LDK_CODE_CACHE=off sbcl --dynamic-space-size 8192 \
;;;   --script bench/primitive-results/run.lisp
(require :asdf)
(asdf:load-system :openldk/interop)

(let* ((directory (asdf:system-relative-pathname :openldk ".cache/bench/primitive-results/"))
       (source (uiop:read-file-string (asdf:system-relative-pathname :openldk "test/standards/PrimitiveResultProbe.java")))
       (paths nil))
  (ensure-directories-exist directory)
  (dolist (suffix '("Baseline" "Optimized"))
    (let* ((name (concatenate 'string "PrimitiveResultProbe" suffix))
           (path (merge-pathnames (concatenate 'string name ".java") directory)))
      (with-open-file (out path :direction :output :if-exists :supersede)
        (write-string (str:replace-all "PrimitiveResultProbe" name source) out))
      (push (namestring path) paths)))
  (uiop:run-program
   (append (list (namestring (merge-pathnames "bin/javac" (uiop:ensure-directory-pathname (uiop:getenv "JAVA_HOME"))))
                 "--release" "25" "-d" (namestring directory)) paths)
   :output t :error-output t)
  (openldk.interop:start-runtime :classpath (list directory)))

(defun result-bench-entry (signature arguments optimized)
  (let* ((class (if optimized "PrimitiveResultProbeOptimized" "PrimitiveResultProbeBaseline"))
         (openldk::*enable-primitive-result-arithmetic* optimized)
         (openldk::*fasl-cache-directory* nil))
    (apply #'openldk.interop:java-static class signature arguments)
    (symbol-function (openldk::static-method-symbol
                      (concatenate 'string class "." signature)
                      (openldk::ldk-class-package (openldk.interop:java-load class))))))

(dolist (spec '(("ints(II)I" (-2147483648 -2147483648))
                ("longs(JJ)J" (9223372036854775807 4611686018427387905))
                ("floats(FF)F" (1.5f0 -2.25f0))
                ("doubles(DD)D" (1.5d0 -2.25d0))))
  (destructuring-bind (signature arguments) spec
    (let* ((baseline (result-bench-entry signature arguments nil))
           (optimized (result-bench-entry signature arguments t))
           (expected (apply baseline arguments))
           (baseline-bytes nil))
      (assert (= expected (apply optimized arguments)))
      (dolist (entry (list (cons :baseline baseline) (cons :optimized optimized)))
        (dotimes (trial 3)
          (sb-ext:gc :full t)
          (let ((before (sb-ext:get-bytes-consed)) (start (get-internal-real-time)))
            (dotimes (i 100000) (assert (= expected (apply (cdr entry) arguments))))
            (let ((bytes (- (sb-ext:get-bytes-consed) before)))
              (if (eql (car entry) :baseline)
                  (setf baseline-bytes (if baseline-bytes (min baseline-bytes bytes) bytes))
                  (progn
                    ;; Ordinary Lisp double/long return values can need boxes;
                    ;; specialization must not add per-call boxing.
                    (assert (<= bytes (+ baseline-bytes 262144)))
                    (when (member signature '("ints(II)I" "floats(FF)F") :test #'string=)
                      (assert (< bytes 262144)))))
              (format t "~&~A ~S trial=~D seconds=~,6F bytes=~D checksum=~S~%"
                      signature (car entry) trial (/ (- (get-internal-real-time) start) internal-time-units-per-second)
                      bytes expected))))))))
