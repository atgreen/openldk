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

;;; From the repository root, JAVA_HOME pointing to JDK 25:
;;; CL_SOURCE_REGISTRY="$PWD//:" LDK_CODE_CACHE=off sbcl --dynamic-space-size 8192 \
;;;   --script bench/generated-code/run.lisp
(require :asdf)
(asdf:load-system :openldk/interop)

(let* ((directory (asdf:system-relative-pathname :openldk ".cache/bench/generated-code/"))
       (sources nil))
  (ensure-directories-exist directory)
  ;; Distinct class identities ensure each configuration compiles its own
  ;; methods. Reuse the differential probes rather than handwritten Lisp.
  (dolist (probe '("InlineLeafProbe" "ArrayLoopProbe" "InstanceInlineProbe"))
    (dolist (suffix '("Baseline" "Optimized"))
      (let* ((name (concatenate 'string probe suffix))
             (path (merge-pathnames (concatenate 'string name ".java") directory)))
        (with-open-file (out path :direction :output :if-exists :supersede)
          (write-string (str:replace-all probe name
                         (uiop:read-file-string
                          (asdf:system-relative-pathname :openldk (format nil "test/standards/~A.java" probe)))) out))
        (push (namestring path) sources))))
  (uiop:run-program
   (append (list (namestring (merge-pathnames "bin/javac" (uiop:ensure-directory-pathname (uiop:getenv "JAVA_HOME"))))
                 "--release" "25" "-d" (namestring directory)) sources)
   :output t :error-output t)
  (openldk.interop:start-runtime :classpath (list directory)))

(defun generated-bench-entry (class signature arguments optimized)
  (let ((openldk::*enable-small-method-inlining* optimized)
        (openldk::*enable-fast-array-loops* optimized)
        (openldk::*fasl-cache-directory* nil))
    (apply #'openldk.interop:java-static class signature arguments)
    (symbol-function
     (openldk::static-method-symbol
      (concatenate 'string class "." signature)
      (openldk::ldk-class-package (openldk.interop:java-load class))))))

(defun generated-bench-instance-entry (class signature arguments optimized)
  (let ((openldk::*enable-small-method-inlining* optimized)
        (openldk::*enable-fast-array-loops* optimized)
        (openldk::*fasl-cache-directory* nil))
    (let* ((object (openldk.interop:java-new class "<init>()V"))
           (symbol (intern signature :openldk)))
      (apply #'openldk.interop:java-call signature object arguments)
      (values (openldk::%compute-java-effective-method (fdefinition symbol) (class-of object))
              object))))

(let ((integers (openldk::make-java-array-default :data (make-array 100000 :element-type '(signed-byte 32) :initial-element 1)))
      (longs (openldk::make-java-array-default :data (make-array 100000 :element-type '(signed-byte 64) :initial-element 9223372036854775807)))
      (doubles (openldk::make-java-array-default :data (make-array 100000 :element-type 'double-float :initial-element 0.5d0))))
  (dolist (spec (list (list "InlineLeafProbe" "loop(I)I" '(1000000) 10)
                     (list "ArrayLoopProbe" "sum([I)I" (list integers) 100)
                     (list "ArrayLoopProbe" "longs([J)J" (list longs) 100)
                     (list "ArrayLoopProbe" "dot([D[D)D" (list doubles doubles) 100)
                     (list "InstanceInlineProbe" "loop(I)I" '(1000000) 10)
                     (list "InstanceInlineProbe" "privateLoop(I)I" '(1000000) 10)))
    (destructuring-bind (probe signature arguments repeats) spec
      (let* ((instance (string= probe "InstanceInlineProbe"))
             (baseline-values (multiple-value-list
                               (funcall (if instance #'generated-bench-instance-entry #'generated-bench-entry)
                                        (concatenate 'string probe "Baseline") signature arguments nil)))
             (optimized-values (multiple-value-list
                                (funcall (if instance #'generated-bench-instance-entry #'generated-bench-entry)
                                         (concatenate 'string probe "Optimized") signature arguments t)))
             (baseline (first baseline-values)) (optimized (first optimized-values))
             (baseline-args (if instance (cons (second baseline-values) arguments) arguments))
             (optimized-args (if instance (cons (second optimized-values) arguments) arguments))
             (expected (apply baseline baseline-args)))
        (assert (= expected (apply optimized optimized-args)))
        (dolist (entry (list (list :baseline baseline baseline-args) (list :optimized optimized optimized-args)))
          (dotimes (trial 3)
            (sb-ext:gc :full t)
            (let ((before (sb-ext:get-bytes-consed)) (start (get-internal-real-time)))
              (dotimes (i repeats) (assert (= expected (apply (second entry) (third entry)))))
              (let ((bytes (- (sb-ext:get-bytes-consed) before))
                    (seconds (/ (- (get-internal-real-time) start) internal-time-units-per-second)))
                ;; Allow fixed runtime/GC accounting noise, but reject per-element boxing.
                (when (eql (first entry) :optimized) (assert (< bytes 262144)))
                (format t "~&~A.~A ~S trial=~D seconds=~,6F bytes=~D checksum=~S~%"
                        probe signature (first entry) trial seconds bytes expected)))))))))
