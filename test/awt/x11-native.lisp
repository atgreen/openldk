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

(in-package :cl-user)

;; The caller initializes OpenLDK and compiles X11Probe into this classpath.
(openldk::main "X11Probe" '("colors")
               :classpath (or (uiop:getenv "LDK_X11_PROBE_DIR") ".cache/awt-test"))
(let ((display openldk::*x11-display*)
      (sentinel (bordeaux-threads:make-thread (lambda () (loop (sleep 1)))
                                             :name "X11 save preflight sentinel"))
      (rejected nil))
  (unwind-protect
       (progn
         (handler-case (openldk::%reset-runtime-for-save)
           (error (condition)
             (if (search "open X11 display" (princ-to-string condition))
                 (setf rejected t)
                 (error condition))))
         (assert rejected () "Saving with an open display was not rejected")
         (assert (eq display openldk::*x11-display*) () "Display was changed by rejected save")
         (assert (bordeaux-threads:thread-alive-p sentinel) () "Save killed a thread before rejecting X11 state")
         (assert (plusp (openldk::%libc "XDisplayWidth" sb-alien:int
                          ((* t) display) (sb-alien:int 0))) () "Display no longer works"))
    (when (bordeaux-threads:thread-alive-p sentinel)
      (bordeaux-threads:destroy-thread sentinel))))
(format t "~&X11 save preflight preserves live displays and threads~%")

;; The callback deliberately fails inside Xlib. The registered native must
;; deliver the condition only after Xlib has returned to its Lisp caller.
(multiple-value-bind (wrapper address)
    (openldk::%x11-layout-buffer "sun/awt/X11/XWindowAttributes")
  (declare (ignore wrapper))
  (let ((original (fdefinition 'openldk::%x11-java-static))
        (received nil))
    (unwind-protect
         (progn
           (setf (fdefinition 'openldk::%x11-java-static)
                 (lambda (owner signature &rest arguments)
                   (if (string= signature "globalErrorHandler(JJ)I")
                       (error "Deferred X11 callback sentinel")
                       (apply original owner signature arguments))))
           (handler-case
               (funcall (gethash "sun/awt/X11/XlibWrapper.XGetWindowAttributes(JJJ)I"
                                 openldk::*native-overrides*)
                        (openldk::%x11-address openldk::*x11-display*) 0 address)
             (error (condition)
               (if (search "Deferred X11 callback sentinel" (princ-to-string condition))
                   (setf received t)
                   (error condition))))
           (assert received () "Callback condition was lost")
           (assert (null (gethash (bordeaux-threads:current-thread) openldk::*x11-callback-errors*))
                   () "Callback condition was not cleared")
           (assert (plusp (funcall (gethash "sun/awt/X11/XlibWrapper.DisplayWidth(JJ)J"
                                           openldk::*native-overrides*)
                                  (openldk::%x11-address openldk::*x11-display*) 0))
                   () "Native display query failed after callback recovery")
           ;; Another native caller must acquire Xlib's display mutex. An
           ;; unwound C callback could otherwise leave the first thread owning it.
           (let ((worker (sb-thread:make-thread
                          (lambda ()
                            (openldk::%libc "XSync" sb-alien:int
                              ((* t) openldk::*x11-display*) (sb-alien:int 0))
                            t)
                          :name "X11 callback mutex recovery")))
             (assert (eq t (sb-thread:join-thread worker :timeout 5 :default :timed-out))
                     () "Xlib display mutex was not released after callback failure")))
      (setf (fdefinition 'openldk::%x11-java-static) original)
      (openldk::%free-unsafe-memory address))))
(format t "~&X11 callback errors return safely through C~%")

;; Shared subimages retain their parent's stride and an adjusted data offset.
;; Compare the CL rendering snapshot with Java's public conversion, including
;; transparent ARGB samples and the forced opaque alpha of integer RGB.
(openldk::classload "java/awt/image/BufferedImage")
(let ((checks 0))
  (dolist (mode '(1 2 3))
    (let ((type (min mode 2))
          (image (openldk::%make-java-instance "java/awt/image/BufferedImage")))
      (openldk::%x11-java-instance image "<init>(III)V" 13 11 type)
      (dotimes (y 11)
        (dotimes (x 13)
          (openldk::|setRGB(III)V| image x y
           (openldk::%as-int (logand #xffffffff (+ (* #x173b59a7 x) (* #x2d851f4b y)))))))
      (when (= mode 3) (openldk::|coerceData(Z)V| image 1))
      (dolist (source (list image (openldk::|getSubimage(IIII)Ljava/awt/image/BufferedImage;| image 3 2 6 5)))
        (let ((width (openldk::|getWidth()I| source))
              (height (openldk::|getHeight()I| source)))
          (dolist (rectangle (list (list 0 0 width height) '(1 1 3 2) '(0 0 0 0)))
            (destructuring-bind (x y w h) rectangle
              (let ((expected (openldk::java-array-data
                               (openldk::|getRGB(IIII[III)[I| source x y w h nil 0 w)))
                    (actual (openldk::%graphics-image-pixels source x y w h)))
                (assert (equalp expected actual) () "Packed raster snapshot mismatch")
                (incf checks))))))))
  (format t "~&Packed RGB/ARGB snapshots and shared subimages: ~D checks passed~%" checks))

;; The optimized snapshot should allocate its packed output, rather than
;; Java color-conversion temporaries for every pixel. Warm helpers first so
;; this measures the copy rather than initial JIT compilation.
(let ((image (openldk::%make-java-instance "java/awt/image/BufferedImage")))
  (openldk::%x11-java-instance image "<init>(III)V" 512 512 1)
  (openldk::%graphics-image-pixels image 0 0 1 1)
  (let* ((before (sb-ext:get-bytes-consed))
         (pixels (openldk::%graphics-image-pixels image 0 0 512 512))
         (allocated (- (sb-ext:get-bytes-consed) before)))
    (assert (= (length pixels) (* 512 512)))
    (assert (< allocated (+ (* 16 512 512) 65536)) ()
            "Packed snapshot allocated ~D bytes" allocated)
    (format t "~&Packed 512x512 snapshot allocated ~D bytes~%" allocated)))
