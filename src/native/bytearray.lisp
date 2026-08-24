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

;;; jdk.internal.util.ByteArray/ByteArrayLittleEndian overrides.

(in-package :openldk)

;;; jdk.internal.util.ByteArray / ByteArrayLittleEndian read and write
;;; primitives out of byte arrays through VarHandle byte-array views, whose
;;; polymorphic get/set invocations OpenLDK does not implement.  Override
;;; the (bytecode) accessors themselves instead -- they are the JDK's
;;; standard byte-order plumbing (DataInputStream, ObjectStreams, UUID...).

(defun %byte-array-load (array index nbytes big-endian-p)
  "Read NBYTES from Java byte ARRAY at INDEX as an unsigned integer."
  (let ((data (java-array-data array))
        (result 0))
    (dotimes (i nbytes result)
      (let ((b (logand (aref data (+ index (if big-endian-p i (- nbytes 1 i)))) #xff)))
        (setf result (logior (ash result 8) b))))))

(defun %byte-array-store (array index nbytes big-endian-p value)
  "Write the low NBYTES of VALUE into Java byte ARRAY at INDEX."
  (let ((data (java-array-data array))
        (v (ldb (byte (* 8 nbytes) 0) value)))
    (dotimes (i nbytes)
      (let* ((shift (* 8 (if big-endian-p (- nbytes 1 i) i)))
             (b (ldb (byte 8 shift) v)))
        (setf (aref data (+ index i)) (if (> b 127) (- b 256) b))))))

(defun %signed-of-width (v bits)
  (if (logbitp (1- bits) v) (- v (ash 1 bits)) v))

(dolist (spec '(("jdk/internal/util/ByteArray" . t)
                ("jdk/internal/util/ByteArrayLittleEndian" . nil)))
  (destructuring-bind (class . be) spec
    (flet ((reg (sig fn)
             (setf (gethash (format nil "~A.~A" class sig) *native-overrides*) fn)))
      (reg "getBoolean([BI)Z" (lambda (a i) (if (zerop (%byte-array-load a i 1 be)) 0 1)))
      (reg "getChar([BI)C" (lambda (a i) (code-char (%byte-array-load a i 2 be))))
      (reg "getShort([BI)S" (lambda (a i) (%signed-of-width (%byte-array-load a i 2 be) 16)))
      (reg "getUnsignedShort([BI)I" (lambda (a i) (%byte-array-load a i 2 be)))
      (reg "getInt([BI)I" (lambda (a i) (%signed-of-width (%byte-array-load a i 4 be) 32)))
      (reg "getLong([BI)J" (lambda (a i) (%signed-of-width (%byte-array-load a i 8 be) 64)))
      (reg "getFloat([BI)F" (lambda (a i) (float-features:bits-single-float (%byte-array-load a i 4 be))))
      (reg "getFloatRaw([BI)F" (lambda (a i) (float-features:bits-single-float (%byte-array-load a i 4 be))))
      (reg "getDouble([BI)D" (lambda (a i) (float-features:bits-double-float (%byte-array-load a i 8 be))))
      (reg "getDoubleRaw([BI)D" (lambda (a i) (float-features:bits-double-float (%byte-array-load a i 8 be))))
      (reg "setBoolean([BIZ)V" (lambda (a i v) (%byte-array-store a i 1 be (if (or (eql v 0) (null v)) 0 1)) nil))
      (reg "setChar([BIC)V" (lambda (a i v) (%byte-array-store a i 2 be (if (characterp v) (char-code v) v)) nil))
      (reg "setShort([BIS)V" (lambda (a i v) (%byte-array-store a i 2 be v) nil))
      (reg "setUnsignedShort([BII)V" (lambda (a i v) (%byte-array-store a i 2 be v) nil))
      (reg "setInt([BII)V" (lambda (a i v) (%byte-array-store a i 4 be v) nil))
      (reg "setLong([BIJ)V" (lambda (a i v) (%byte-array-store a i 8 be v) nil))
      (reg "setFloat([BIF)V" (lambda (a i v)
                               (%byte-array-store a i 4 be (float-features:single-float-bits (coerce v 'single-float)))
                               nil))
      (reg "setFloatRaw([BIF)V" (lambda (a i v)
                                  (%byte-array-store a i 4 be (float-features:single-float-bits (coerce v 'single-float)))
                                  nil))
      (reg "setDouble([BID)V" (lambda (a i v)
                                (%byte-array-store a i 8 be (float-features:double-float-bits (coerce v 'double-float)))
                                nil))
      (reg "setDoubleRaw([BID)V" (lambda (a i v)
                                   (%byte-array-store a i 8 be (float-features:double-float-bits (coerce v 'double-float)))
                                   nil)))))

(defmethod |java/util/TimeZone.getSystemTimeZoneID(Ljava/lang/String;)| (arg)
  "Return the zoneinfo ID of the system default timezone (e.g.
\"America/Toronto\"): $TZ if set, else the /etc/localtime symlink target.
The previous implementation returned an abbreviation like \"EDT\", which
TimeZone.getTimeZone does not recognize, silently defaulting to GMT."
  (declare (ignore arg))
  (jstring
   (or (let ((tz (uiop:getenv "TZ")))
         (and tz (plusp (length tz)) (string-left-trim ":" tz)))
       (ignore-errors
         (let* ((link (sb-posix:readlink "/etc/localtime"))
                (pos (search "zoneinfo/" link)))
           (when pos
             (subseq link (+ pos (length "zoneinfo/"))))))
       "GMT")))

(defmethod |length()| ((str string))
  (length (lstring str)))

(defmethod |java/util/TimeZone.getSystemGMTOffsetID()| ()
  (jstring (local-time:format-timestring nil (local-time:now) :format '(:gmt-offset))))

(defvar *field-offset-table* (make-hash-table :test #'equal))

(defun %register-field-offset (field)
  "Return an aligned synthetic Unsafe offset and associate it with FIELD."
  ;; JDK byte/boolean atomic operations align an object offset down to the
  ;; containing four-byte word before calling the integer CAS primitives.
  ;; OpenLDK models fields as individual CLOS slots rather than packed memory,
  ;; so give every synthetic field a word-aligned identity.
  (let ((offset (logand
                 (unsigned-to-signed-integer
                  (cl-murmurhash:murmurhash (sxhash field)))
                 -8)))
    (setf (gethash offset *field-offset-table*) field)
    offset))

(defmethod |objectFieldOffset(Ljava/lang/reflect/Field;)| ((unsafe |sun/misc/Unsafe|) field)
  (declare (ignore unsafe))
  (%register-field-offset field))

(defun |java/lang/Class$Atomic.objectFieldOffset([Ljava/lang/reflect/Field;Ljava/lang/String;)| (field name)
  (declare (ignore field)
           (ignore name))
  (unimplemented "java/lang/Class$Atomic.objectFieldOffset"))

(defmethod |staticFieldBase(Ljava/lang/reflect/Field;)| ((unsafe |sun/misc/Unsafe|) field)
  (declare (ignore unsafe)
           (ignore field))
  nil)

(defmethod |staticFieldOffset(Ljava/lang/reflect/Field;)| ((unsafe |sun/misc/Unsafe|) field)
  (declare (ignore unsafe))
  (%register-field-offset field))

(defmethod |staticFieldOffset0(Ljava/lang/reflect/Field;)| ((unsafe |jdk/internal/misc/Unsafe|) field)
  (declare (ignore unsafe))
  (%register-field-offset field))

(defmethod |staticFieldBase0(Ljava/lang/reflect/Field;)| ((unsafe |jdk/internal/misc/Unsafe|) field)
  (declare (ignore unsafe field))
  nil)

(defmethod |objectFieldOffset0(Ljava/lang/reflect/Field;)| ((unsafe |jdk/internal/misc/Unsafe|) field)
  (declare (ignore unsafe))
  (%register-field-offset field))

(defun %stringize-array (array)
  "Convert an array of characters and integers (ASCII values) into a string."
  (coerce
   (map 'list
        (lambda (x)
          (if (integerp x)
              (code-char x) ;; Convert integer to character
              x))           ;; Keep character as is
        (if array (java-array-data array) nil))
   'string))

(defmethod print-object ((str |java/lang/String|) out)
  (print-unreadable-object (str out :type t)
    (format out "~S" (%stringize-array (slot-value str '|value|)))))

(defmethod print-object ((class |java/lang/Class|) out)
  (print-unreadable-object (class out :type t)
    (format out "~A" (slot-value class '|name|))))

(defmethod |java/lang/Thread.registerNatives()| ()
  ;; JNI binding registration -- a no-op here, like the other registerNatives.
  nil)

