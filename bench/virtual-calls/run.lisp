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

;;; From the repository root:
;;; CL_SOURCE_REGISTRY="$PWD//:" sbcl --dynamic-space-size 8192 \
;;;   --script bench/virtual-calls/run.lisp
(require :asdf)
(asdf:load-system :openldk)
(defclass pic-bench-object () ())
(let* ((object (make-instance 'pic-bench-object))
       (gf (ensure-generic-function 'pic-bench-call :generic-function-class 'openldk::java-generic-function :lambda-list '(object)))
       (method (make-instance 'standard-method :lambda-list '(object) :specializers (list (find-class 'pic-bench-object))
                             :function (lambda (args next) (declare (ignore args next)) 7)))
       (body (lambda (object) (declare (ignore object)) 7))
       (site (openldk::make-java-virtual-site :name 'pic-bench-call)))
  (add-method gf method)
  (setf (gethash method openldk::*method-bodies*) body)
  (let ((baseline (compile nil '(lambda (gf object site)
                                 (declare (ignore site) (optimize (speed 3) (safety 1)))
                                 (let ((sum 0)) (declare (fixnum sum))
                                   (dotimes (i 1000000 sum) (incf sum (the fixnum (funcall gf object))))))))
        (cached (compile nil '(lambda (gf object site)
                               (declare (ignore gf) (optimize (speed 3) (safety 1)))
                               (let ((sum 0)) (declare (fixnum sum))
                                 (dotimes (i 1000000 sum) (incf sum (the fixnum (openldk::%invoke-virtual-at site object)))))))))
    (dolist (entry (list (cons :baseline baseline) (cons :cached cached)))
      (funcall (cdr entry) gf object site)
      (dotimes (trial 3)
        (let* ((start (get-internal-real-time)) (bytes (sb-ext:get-bytes-consed))
               (result (funcall (cdr entry) gf object site)))
          (assert (= 7000000 result))
          (assert (< (- (sb-ext:get-bytes-consed) bytes) 65536))
          (format t "~&PIC ~S trial=~D seconds=~,6F bytes=~D result=~D~%" (car entry) trial
                  (/ (- (get-internal-real-time) start) internal-time-units-per-second)
                  (- (sb-ext:get-bytes-consed) bytes) result))))))
