;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: OPENLDK; Base: 10 -*-
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

;;; Run from the repository root with make bench-remainder (Linux/libm).
(require :asdf)
(push (uiop:getcwd) asdf:*central-registry*)
(asdf:load-system :openldk)
(load (asdf:system-relative-pathname :openldk "test/remainder/reference.lisp"))
(in-package :openldk)

(sb-int:set-floating-point-modes :traps nil :rounding-mode :nearest :fast-mode nil)
(sb-alien:load-shared-object "libm.so.6")

(declaim (inline %libm-frem %libm-drem))
(defun %libm-frem (x y)
  (declare (type single-float x y))
  (sb-alien:alien-funcall (sb-alien:extern-alien "fmodf" (function single-float single-float single-float)) x y))
(defun %libm-drem (x y)
  (declare (type double-float x y))
  (sb-alien:alien-funcall (sb-alien:extern-alien "fmod" (function double-float double-float double-float)) x y))

(defmacro %define-remainder-benchmark (name type expression)
  `(defun ,name (left right repeats)
     (declare (type (simple-array ,type (*)) left right)
              (type (integer 1 100000) repeats)
              (optimize (speed 3)))
     (let ((checksum 0))
       (declare (type (unsigned-byte 32) checksum))
       (dotimes (repeat repeats)
         (dotimes (i (length left))
           (let* ((x (aref left i)) (y (aref right i))
                  (result (the ,type ,expression))
                  (bits ,(if (eql type 'single-float)
                             '(sb-kernel:single-float-bits result)
                             '(logxor (sb-kernel:double-float-high-bits result)
                                      (sb-kernel:double-float-low-bits result)))))
             (setf checksum (logand #xffffffff (+ checksum bits))))))
       checksum)))

(%define-remainder-benchmark %bench-single-lisp single-float (%java-frem x y))
(%define-remainder-benchmark %bench-double-lisp double-float (%java-drem x y))
(%define-remainder-benchmark %bench-single-libm single-float (%libm-frem x y))
(%define-remainder-benchmark %bench-double-libm double-float (%libm-drem x y))
(%define-remainder-benchmark %bench-single-rational single-float (%remainder-reference x y))
(%define-remainder-benchmark %bench-double-rational double-float (%remainder-reference x y))

(defun %remainder-benchmark-data (type workload)
  (let ((left (make-array 1024 :element-type type))
        (right (make-array 1024 :element-type type)))
    (dotimes (i 1024)
      (multiple-value-bind (x y)
          (ecase workload
            (:near (values (+ 1.5d0 (/ (mod (* i 17) 499) 1000d0))
                           (+ 1d0 (/ (mod (* i 31) 251) 1000d0))))
            (:moderate (values (+ 100000d0 (* i 13.25d0)) (+ 0.125d0 (/ (mod (* i 19) 1001) 8d0))))
            (:wide
             (if (eql type 'single-float)
                 (values
                  (float-features:bits-single-float
                   (logior (ash (+ 180 (mod i 74)) 23) (1+ (mod (* i 7919) #x7fffff))))
                  (float-features:bits-single-float
                   (logior (ash (1+ (mod i 20)) 23) (1+ (mod (* i 3571) #x7fffff)))))
                 (values
                  (float-features:bits-double-float
                   (logior (ash (+ 1700 (mod i 347)) 52) (1+ (* i 7919))))
                  (float-features:bits-double-float
                   (logior (ash (1+ (mod i 100)) 52) (1+ (* i 3571)))))))
            (:subnormal
             (if (eql type 'single-float)
                 (values (float-features:bits-single-float (+ #x700000 i))
                         (float-features:bits-single-float (+ 17 (mod (* i 31) 1001))))
                 (values (float-features:bits-double-float (+ #xf000000000000 i))
                         (float-features:bits-double-float (+ 17 (mod (* i 31) 1001)))))))
        (setf (aref left i) (coerce x type) (aref right i) (coerce y type))))
    (values left right)))

(defun %measure-remainder (function left right repeats)
  (funcall function left right 32)
  (let ((times nil) (allocations nil) (checksum nil)
        (calls (* repeats (length left))))
    (dotimes (trial 3)
      (sb-ext:gc :full t)
      (let ((bytes (sb-ext:get-bytes-consed))
            (start (get-internal-real-time)))
        (setf checksum (funcall function left right repeats))
        (push (/ (* 1d9 (- (get-internal-real-time) start))
                 internal-time-units-per-second calls) times)
        (push (/ (- (sb-ext:get-bytes-consed) bytes) (float calls 1d0)) allocations)))
    (values (second (sort times #'<)) (second (sort allocations #'<)) checksum)))

(format t "~&~A ~A, ~A ~A~%" (lisp-implementation-type) (lisp-implementation-version)
        (machine-type) (machine-version))
(format t "~&Lisp vs exact oracle: ~D pairs~%" (%check-remainder-functions))
(format t "~&libm vs exact oracle: ~D pairs~%" (%check-remainder-functions 100000 #'%libm-frem #'%libm-drem))
(format t "~&Median of 3 trials; preallocated typed inputs, checksum retained.~%")
(format t "~&format,workload,implementation,ns/op,bytes/op,checksum~%")
(dolist (type '(single-float double-float))
  (dolist (workload '(:near :moderate :wide :subnormal))
    (multiple-value-bind (left right) (%remainder-benchmark-data type workload)
      (let ((functions (if (eql type 'single-float)
                           (list #'%bench-single-lisp #'%bench-single-libm #'%bench-single-rational)
                           (list #'%bench-double-lisp #'%bench-double-libm #'%bench-double-rational)))
            (expected nil))
        (loop for function in functions
              for name in '(lisp libm rational)
              do (let ((checksum (funcall function left right 1)))
                   (if expected (assert (= expected checksum)) (setf expected checksum)))
                 (multiple-value-bind (time bytes checksum)
                     (%measure-remainder function left right (if (eql name 'rational) 32 2048))
                   (format t "~(~A,~A,~A~),~,2F,~,2F,~X~%" type workload name time bytes checksum)
                   (finish-output)))))))
(ensure-directories-exist ".cache/remainder-test/")
(with-open-file (*standard-output* ".cache/remainder-test/disassembly.txt" :direction :output :if-exists :supersede)
  (disassemble #'%bench-single-lisp)
  (disassemble #'%bench-double-lisp))
