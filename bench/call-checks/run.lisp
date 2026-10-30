;;; SPDX-License-Identifier: GPL-3.0-or-later WITH Classpath-exception-2.0
;;; CL_SOURCE_REGISTRY="$PWD//:" LDK_CODE_CACHE=off sbcl --dynamic-space-size 8192 \
;;;   --script bench/call-checks/run.lisp
(require :asdf)
(asdf:load-system :openldk/interop)
(let* ((directory (asdf:system-relative-pathname :openldk ".cache/bench/call-checks/"))
       (source (uiop:read-file-string (asdf:system-relative-pathname :openldk "test/standards/InlineLeafProbe.java")))
       (paths nil))
  (ensure-directories-exist directory)
  (dolist (name '("CallCheckBaseline" "CallCheckOptimized"))
    (let ((path (merge-pathnames (concatenate 'string name ".java") directory)))
      (with-open-file (out path :direction :output :if-exists :supersede)
        (write-string (str:replace-all "InlineLeafProbe" name source) out))
      (push (namestring path) paths)))
  (uiop:run-program (append (list (namestring (merge-pathnames "bin/javac" (uiop:ensure-directory-pathname (uiop:getenv "JAVA_HOME"))))
                                "--release" "25" "-d" (namestring directory)) paths)
                    :output t :error-output t)
  (openldk.interop:start-runtime :classpath (list directory)))

(dolist (optimized '(nil t))
  (let* ((name (if optimized "CallCheckOptimized" "CallCheckBaseline"))
         (openldk::*enable-redundant-linkage-elision* optimized)
         (openldk::*fasl-cache-directory* nil)
         (class (openldk.interop:java-load name)))
    ;; Inlining, receiver caching and primitive arithmetic stay enabled in
    ;; both versions. Only redundant linkage/access/initialization checks differ.
    (openldk.interop:java-static name "loop(I)I" 1000000)
    (let ((function (symbol-function (openldk::static-method-symbol
                                     (concatenate 'string name ".loop(I)I") (openldk::ldk-class-package class)))))
      (dotimes (trial 3)
        (sb-ext:gc :full t)
        (let ((start (get-internal-real-time)) (before (sb-ext:get-bytes-consed)))
          (dotimes (i 10) (assert (= 1783293664 (funcall function 1000000))))
          (let ((bytes (- (sb-ext:get-bytes-consed) before)))
            (assert (< bytes 65536))
            (format t "~&call checks optimized=~S trial=~D seconds=~,6F bytes=~D checksum=1783293664~%"
                    optimized trial (/ (- (get-internal-real-time) start) internal-time-units-per-second) bytes)))))))
