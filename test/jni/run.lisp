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

;;; Development runner; the integration target normally uses the saved image.
(require :asdf)
(asdf:load-system :openldk)
(openldk:initialize)
(defvar *jni-lisp-override-called* nil)
(when (uiop:getenv "JNI_LISP_OVERRIDE")
  ;; A pre-existing CL native must win over the exported C++ implementation.
  (setf (symbol-function (intern "JNIProbe.cpp(I)I" :openldk))
        (lambda (n) (setf *jni-lisp-override-called* t) (* n 7))))
(when (uiop:getenv "JNI_TRACE")
  (setf openldk::*debug-exceptions* t)
  (trace openldk::%jni-system-load openldk::%jni-load-library))
(handler-case (let ((main (or (uiop:getenv "JNI_MAIN") "JNIProbe")))
                (openldk::main main
                              (unless (string= main "ZstdProbe")
                                (list (namestring (truename ".cache/jni/test/libjniprobe.so"))
                                      (namestring (truename ".cache/jni/test/"))))
                              :classpath (or (uiop:getenv "JNI_CLASSPATH") ".cache/jni/test")))
  (openldk::|condition-java/lang/Throwable| (condition)
    (openldk::%print-java-stack-trace (slot-value condition 'openldk::|objref|) :stream *error-output*)
    (error condition)))
(openldk::%await-nondaemon-java-threads)

(when (uiop:getenv "JNI_LISP_OVERRIDE") (assert *jni-lisp-override-called*))
(maphash (lambda (id reference)
           (declare (ignore id))
           (assert (not (eql :local (openldk::jni-reference-kind reference)))))
         openldk::*jni-references*)
(maphash (lambda (id state)
           (declare (ignore id))
           (assert (= 1 (length (openldk::jni-state-frames state))))
           (assert (null (openldk::jni-state-monitors state))))
         openldk::*jni-states*)
(assert (handler-case (progn (openldk::%jni-check-save) nil) (error () t)))
(format t "JNI reference lifetime, CL precedence, and image guard passed~%")
