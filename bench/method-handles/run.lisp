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
