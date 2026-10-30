;;; SPDX-License-Identifier: GPL-3.0-or-later WITH Classpath-exception-2.0
;;; From the repository root, with JAVA_HOME pointing to a JDK 25:
;;; CL_SOURCE_REGISTRY="$PWD//:" LDK_CODE_CACHE=off sbcl --dynamic-space-size 8192 \
;;;   --script bench/method-handles/run.lisp
(require :asdf)
(asdf:load-system :openldk/interop)

(let ((directory (asdf:system-relative-pathname :openldk ".cache/bench/method-handles/")))
  (ensure-directories-exist directory)
  (uiop:run-program
   (list (namestring (merge-pathnames "bin/javac" (uiop:ensure-directory-pathname (uiop:getenv "JAVA_HOME"))))
         "--release" "25" "-d" (namestring directory)
         (namestring (asdf:system-relative-pathname :openldk "test/standards/MethodHandleGraphProbe.java")))
   :output t :error-output t)
  (openldk.interop:start-runtime :classpath (list directory)))
(openldk.interop:java-static "MethodHandleGraphProbe" "main([Ljava/lang/String;)"
                            (openldk.interop:make-java-array-of "java.lang.String" nil))

(let ((handle (openldk.interop:java-static-field "MethodHandleGraphProbe" "bench")))
  (dolist (compiled '(nil t))
    (let ((openldk::*enable-compiled-lambda-forms* compiled))
      (dotimes (i 100) (assert (= 130 (openldk::%mh-invoke handle '(123)))))
      (dotimes (trial 3)
        (sb-ext:gc :full t)
        (let ((before (sb-ext:get-bytes-consed))
              (start (get-internal-real-time))
              (checksum 0))
          (dotimes (i 10000) (incf checksum (openldk::%mh-invoke handle '(123))))
          (let ((bytes (- (sb-ext:get-bytes-consed) before))
                (seconds (/ (- (get-internal-real-time) start) internal-time-units-per-second)))
            (assert (= 1300000 checksum))
            (when compiled (assert (< bytes 65536)))
            (format t "~&compiled=~S trial=~D seconds=~,6F bytes=~D checksum=~D~%"
                    compiled trial seconds bytes checksum)))))))
