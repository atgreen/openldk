;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: OPENLDK; Base: 10 -*-
;;;
;;; Copyright (C) 2024, 2025  Anthony Green <green@moxielogic.com>
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

;;; Remaining JDK 21 internal-Unsafe native entry points.

(in-package :openldk)

;;; Remaining native entry points on JDK 21's internal Unsafe.  These are
;;; uncommon in the game itself, but are part of the transitive NIO/atomic
;;; surface and should fail neither linkage nor lazy compilation.

(defmethod |getUncompressedObject(J)|
    ((unsafe |jdk/internal/misc/Unsafe|) address)
  (declare (ignore unsafe))
  (gethash address *unsafe-reference-memory-table*))

(defmethod |writeback0(J)|
    ((unsafe |jdk/internal/misc/Unsafe|) address)
  (declare (ignore unsafe address))
  nil)

(defmethod |writebackPreSync0()| ((unsafe |jdk/internal/misc/Unsafe|))
  (declare (ignore unsafe))
  nil)

(defmethod |writebackPostSync0()| ((unsafe |jdk/internal/misc/Unsafe|))
  (declare (ignore unsafe))
  nil)

(defmethod |defineClass0(Ljava/lang/String;[BIILjava/lang/ClassLoader;Ljava/security/ProtectionDomain;)|
    ((unsafe |jdk/internal/misc/Unsafe|) class-name data offset length
     class-loader protection-domain)
  (declare (ignore unsafe protection-domain))
  (let* ((ldk-loader (get-ldk-loader-for-java-loader class-loader))
         (stream (make-instance 'byte-array-input-stream
                                :array data :start offset :end (+ offset length)))
         (result (%classload-from-stream
                  (substitute #\/ #\. (lstring class-name))
                  stream class-loader ldk-loader)))
    (unless result
      (let ((exception (%make-java-instance "java/lang/NoClassDefFoundError")))
        (|<init>(Ljava/lang/String;)| exception class-name)
        (error (%lisp-condition exception))))
    (java-class result)))

(defmethod |allocateInstance(Ljava/lang/Class;)|
    ((unsafe |jdk/internal/misc/Unsafe|) class)
  (declare (ignore unsafe))
  (let* ((bin-name (substitute #\/ #\.
                               (lstring (slot-value class '|name|))))
         (pkg (class-package bin-name)))
    (make-instance (intern bin-name pkg))))

(defmethod |throwException(Ljava/lang/Throwable;)|
    ((unsafe |jdk/internal/misc/Unsafe|) throwable)
  (declare (ignore unsafe))
  (error (%lisp-condition throwable)))

(defun %unsafe-read-storage-byte (object offset)
  (if object
      (%unsafe-array-read-byte object offset)
      (sb-sys:sap-ref-8 (sb-sys:int-sap offset) 0)))

(defun %unsafe-write-storage-byte (object offset value)
  (if object
      (%unsafe-array-write-byte object offset value)
      (setf (sb-sys:sap-ref-8 (sb-sys:int-sap offset) 0)
            (logand value #xff))))

(defmethod |copySwapMemory0(Ljava/lang/Object;JLjava/lang/Object;JJJ)|
    ((unsafe |jdk/internal/misc/Unsafe|)
     source source-offset destination destination-offset bytes element-size)
  (declare (ignore unsafe))
  (unless (and (plusp element-size) (zerop (mod bytes element-size)))
    (internal-error "Invalid copySwapMemory size ~D for ~D-byte elements"
           bytes element-size))
  (let ((snapshot (make-array bytes :element-type '(unsigned-byte 8))))
    (dotimes (index bytes)
      (setf (aref snapshot index)
            (%unsafe-read-storage-byte source (+ source-offset index))))
    (loop for base from 0 below bytes by element-size
          do (dotimes (index element-size)
               (%unsafe-write-storage-byte
                destination (+ destination-offset base index)
                (aref snapshot (+ base (- element-size index 1)))))))
  nil)

(defmethod |getLoadAverage0([DI)|
    ((unsafe |jdk/internal/misc/Unsafe|) load-averages count)
  (declare (ignore unsafe load-averages count))
  -1)
