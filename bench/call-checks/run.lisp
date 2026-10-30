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
