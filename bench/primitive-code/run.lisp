;;; SPDX-License-Identifier: GPL-3.0-or-later WITH Classpath-exception-2.0
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
