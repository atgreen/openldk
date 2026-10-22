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

;;; Unsafe, VarHandle views, raw memory, CAS, and park/unpark.

(in-package :openldk)

;;; JDK 9+ jdk/internal/misc/Unsafe native methods
;;; These mirror the sun/misc/Unsafe methods but are on the new class.

(defmethod |arrayBaseOffset0(Ljava/lang/Class;)I| ((unsafe |jdk/internal/misc/Unsafe|) array)
  0)

(defun %unsafe-array-index-scale (array-class)
  "Return the byte width of ARRAY-CLASS elements for Unsafe array access."
  (let ((name (lstring (slot-value array-class '|name|))))
    (if (and (plusp (length name)) (char= (char name 0) #\[))
        (case (and (> (length name) 1) (char name 1))
          ((#\B #\Z) 1)
          ((#\C #\S) 2)
          ((#\F #\I) 4)
          ((#\D #\J) 8)
          (otherwise 8))
        1)))

(defun %data-element-scale (data)
  "Width in bytes of an element of the java-array DATA vector."
  (typecase data
    ((simple-array (signed-byte 8) (*)) 1)
    ((or (simple-array (unsigned-byte 16) (*)) (simple-array (signed-byte 16) (*))) 2)
    ((or (simple-array (signed-byte 32) (*)) (simple-array single-float (*))) 4)
    (t 8)))

(defun %unsafe-array-element-scale (array)
  "Return the storage width of ARRAY's component type in bytes."
  (%data-element-scale (java-array-data array)))

;;; A primitive array's data is a specialized vector laid out exactly as
;;; the JVM lays the array out (little-endian, IEEE floats, UTF-16 chars),
;;; so Unsafe byte-offset access reads and writes that memory directly.

(defun %primitive-data-p (data)
  (not (simple-vector-p data)))

(defun %check-array-bytes (data byte-offset byte-count)
  "Throw ArrayIndexOutOfBoundsException unless BYTE-COUNT bytes at
BYTE-OFFSET lie within DATA."
  (unless (<= 0 byte-offset (- (* (length data) (%data-element-scale data)) byte-count))
    (%throw-array-index-error byte-offset)))

(defun %data-ref-bits (data byte-offset byte-count)
  "Unsigned BYTE-COUNT-byte little-endian value at BYTE-OFFSET in primitive DATA."
  (%check-array-bytes data byte-offset byte-count)
  (sb-sys:with-pinned-objects (data)
    (let ((sap (sb-sys:vector-sap data)))
      (case byte-count
        (1 (sb-sys:sap-ref-8 sap byte-offset))
        (2 (sb-sys:sap-ref-16 sap byte-offset))
        (4 (sb-sys:sap-ref-32 sap byte-offset))
        (8 (sb-sys:sap-ref-64 sap byte-offset))
        (t (loop for i below byte-count
                 sum (ash (sb-sys:sap-ref-8 sap (+ byte-offset i)) (* i 8))))))))

(defun %data-set-bits (data byte-offset byte-count bits)
  "Store the low BYTE-COUNT bytes of BITS, little-endian, at BYTE-OFFSET in primitive DATA."
  (%check-array-bytes data byte-offset byte-count)
  (sb-sys:with-pinned-objects (data)
    (let ((sap (sb-sys:vector-sap data)))
      (case byte-count
        (1 (setf (sb-sys:sap-ref-8 sap byte-offset) (ldb (byte 8 0) bits)))
        (2 (setf (sb-sys:sap-ref-16 sap byte-offset) (ldb (byte 16 0) bits)))
        (4 (setf (sb-sys:sap-ref-32 sap byte-offset) (ldb (byte 32 0) bits)))
        (8 (setf (sb-sys:sap-ref-64 sap byte-offset) (ldb (byte 64 0) bits)))
        (t (dotimes (i byte-count)
             (setf (sb-sys:sap-ref-8 sap (+ byte-offset i))
                   (ldb (byte 8 (* i 8)) bits)))))))
  nil)

(defun %unsafe-array-read-byte (array byte-offset)
  "Read one byte from ARRAY's primitive in-memory representation."
  (let ((data (java-array-data array)))
    (if (%primitive-data-p data)
        (%data-ref-bits data byte-offset 1)
        ;; Reference arrays have no byte representation; treat elements
        ;; as 8-byte integers, as before typed arrays.
        (multiple-value-bind (index byte) (truncate byte-offset 8)
          (ldb (byte 8 (* 8 byte)) (aref data index))))))

(defun %unsafe-array-write-byte (array byte-offset byte)
  "Write BYTE into ARRAY's primitive in-memory representation."
  (let ((data (java-array-data array)))
    (if (%primitive-data-p data)
        (%data-set-bits data byte-offset 1 byte)
        (multiple-value-bind (index offset) (truncate byte-offset 8)
          (setf (aref data index)
                (sb-c::mask-signed-field
                 64 (dpb byte (byte 8 (* 8 offset)) (aref data index))))))))

(defun %unsafe-array-index (array byte-offset)
  "Translate Unsafe BYTE-OFFSET into ARRAY's element index."
  (multiple-value-bind (index remainder)
      (truncate byte-offset (%unsafe-array-element-scale array))
    (unless (zerop remainder)
      (internal-error "Unaligned Unsafe array offset ~D" byte-offset))
    index))

;;; Unsafe.getXUnaligned(Object, long) reads multi-byte values out of byte
;;; arrays at arbitrary byte offsets (CodePointTrie, ICU, vectorized string
;;; ops).  The Java implementations decompose into VarHandle/getByte tricks
;;; our Unsafe emulation can't serve, so override them wholesale.  Results
;;; are native (little-endian) order, as the JDK's 3-arg big-endian variants
;;; expect; char results are returned as integers (bit-manipulation code
;;; consumes them arithmetically).

(defun %unsafe-get-unaligned (obj offset nbytes signedp)
  (if (typep obj 'java-array)
      (let ((v (%unsafe-read-array-bits obj offset nbytes)))
        (if signedp
            (%signed-of-width v (* 8 nbytes))
            v))
      (let ((v (%unsafe-read-native-bits offset nbytes)))
        (if signedp
            (%signed-of-width v (* 8 nbytes))
            v))))

(dolist (spec '(("getCharUnaligned(Ljava/lang/Object;J)C" 2 nil)
                ("getShortUnaligned(Ljava/lang/Object;J)S" 2 t)
                ("getIntUnaligned(Ljava/lang/Object;J)I" 4 t)
                ("getLongUnaligned(Ljava/lang/Object;J)J" 8 t)))
  (destructuring-bind (sig nbytes signedp) spec
    (setf (gethash (format nil "jdk/internal/misc/Unsafe.~A" sig) *native-overrides*)
          (lambda (unsafe obj offset)
            (declare (ignore unsafe))
            (%unsafe-get-unaligned obj offset nbytes signedp)))))

;;; VarHandle byte-array views (MethodHandles.byteArrayViewVarHandle):
;;; polymorphic get/set invocations on VarHandleByteArrayAs*$ArrayHandle
;;; instances are intrinsified in HotSpot, so there is no bytecode to
;;; compile -- implement them as CLOS methods.  Stub defclasses let the
;;; methods be defined at load time; the real classload later redefines
;;; the classes in place (same identity), preserving the methods.

(defun %byte-view-be-p (handle)
  (and (slot-exists-p handle '|be|)
       (ignore-errors (slot-boundp handle '|be|))
       (let ((v (slot-value handle '|be|)))
         (and v (not (eql v 0))))))

(defun %byte-view-get (handle array index nbytes kind)
  (let ((v (%byte-array-load array index nbytes (%byte-view-be-p handle))))
    (ecase kind
      (:char (code-char v))
      (:signed (%signed-of-width v (* 8 nbytes)))
      (:float (float-features:bits-single-float v))
      (:double (float-features:bits-double-float v)))))

(defun %byte-view-set (handle array index nbytes kind value)
  (%byte-array-store array index nbytes (%byte-view-be-p handle)
                     (ecase kind
                       (:char (if (characterp value) (char-code value) value))
                       (:signed value)
                       (:float (float-features:single-float-bits
                                (coerce value 'single-float)))
                       (:double (float-features:double-float-bits
                                 (coerce value 'double-float)))))
  nil)

(dolist (spec '(("Shorts" 2 :signed "S")
                ("Chars" 2 :char "C")
                ("Ints" 4 :signed "I")
                ("Longs" 8 :signed "J")
                ("Floats" 4 :float "F")
                ("Doubles" 8 :double "D")))
  (destructuring-bind (view nbytes kind jtype) spec
    (let ((class-sym (intern (format nil "java/lang/invoke/VarHandleByteArrayAs~A$ArrayHandle" view)
                             :openldk)))
      (eval `(defclass ,class-sym () ()))
      (dolist (getter '("get" "getVolatile" "getAcquire" "getOpaque"))
        (eval `(defmethod ,(intern (format nil "~A([BI)" getter) :openldk)
                   ((this ,class-sym) array index)
                 (%byte-view-get this array index ,nbytes ,kind))))
      (dolist (setter '("set" "setVolatile" "setRelease" "setOpaque"))
        (eval `(defmethod ,(intern (format nil "~A([BI~A)" setter jtype) :openldk)
                   ((this ,class-sym) array index value)
                 (%byte-view-set this array index ,nbytes ,kind value)))))))

;;; VarHandle array-element views (MethodHandles.arrayElementVarHandle),
;;; same stub-class technique.  All read-modify-write operations serialize
;;; on *cas-lock*, like the Unsafe CAS emulation.

(dolist (spec '(("References" "[Ljava/lang/Object;" "Ljava/lang/Object;" eq nil)
                ("Ints" "[I" "I" eql 32)
                ("Longs" "[J" "J" eql 64)))
  (destructuring-bind (family arr-sig val-sig test bits) spec
    (let ((class-sym (intern (format nil "java/lang/invoke/VarHandle~A$Array" family)
                             :openldk)))
      (eval `(defclass ,class-sym () ()))
      (dolist (getter '("get" "getVolatile" "getAcquire" "getOpaque"))
        (eval `(defmethod ,(intern (format nil "~A(~AI)" getter arr-sig) :openldk)
                   ((this ,class-sym) array index)
                 (declare (ignore this))
                 (jaref array index))))
      (dolist (setter '("set" "setVolatile" "setRelease" "setOpaque"))
        (eval `(defmethod ,(intern (format nil "~A(~AI~A)" setter arr-sig val-sig) :openldk)
                   ((this ,class-sym) array index value)
                 (declare (ignore this))
                 (setf (jaref array index) value)
                 nil)))
      (dolist (cas '("compareAndSet" "weakCompareAndSet" "weakCompareAndSetPlain"
                     "weakCompareAndSetAcquire" "weakCompareAndSetRelease"))
        (eval `(defmethod ,(intern (format nil "~A(~AI~A~A)" cas arr-sig val-sig val-sig) :openldk)
                   ((this ,class-sym) array index expected new-value)
                 (declare (ignore this))
                 (bordeaux-threads:with-recursive-lock-held (*cas-lock*)
                   (if (,test (jaref array index) expected)
                       (progn (setf (jaref array index) new-value) 1)
                       0)))))
      (dolist (cae '("compareAndExchange" "compareAndExchangeAcquire" "compareAndExchangeRelease"))
        (eval `(defmethod ,(intern (format nil "~A(~AI~A~A)" cae arr-sig val-sig val-sig) :openldk)
                   ((this ,class-sym) array index expected new-value)
                 (declare (ignore this))
                 (bordeaux-threads:with-recursive-lock-held (*cas-lock*)
                   (let ((old (jaref array index)))
                     (when (,test old expected)
                       (setf (jaref array index) new-value))
                     old)))))
      (eval `(defmethod ,(intern (format nil "getAndSet(~AI~A)" arr-sig val-sig) :openldk)
                 ((this ,class-sym) array index value)
               (declare (ignore this))
               (bordeaux-threads:with-recursive-lock-held (*cas-lock*)
                 (let ((old (jaref array index)))
                   (setf (jaref array index) value)
                   old))))
      (when bits
        (eval `(defmethod ,(intern (format nil "getAndAdd(~AI~A)" arr-sig val-sig) :openldk)
                   ((this ,class-sym) array index delta)
                 (declare (ignore this))
                 (bordeaux-threads:with-recursive-lock-held (*cas-lock*)
                   (let ((old (jaref array index)))
                     (setf (jaref array index)
                           (%signed-of-width (ldb (byte ,bits 0) (+ old delta)) ,bits))
                     old))))))))

;;; Common storage model for jdk.internal.misc.Unsafe.  Older native support
;;; below grew around sun.misc.Unsafe, but Java 9+ NIO and atomics call the
;;; internal Unsafe class directly.  Keep one implementation for fields,
;;; primitive/reference arrays, and raw addresses so all access modes agree.

(defvar *unsafe-reference-memory-table* (make-hash-table))

(defun %unsafe-slot-key (offset)
  "Return the CLOS slot symbol for the field registered at Unsafe OFFSET,
or NIL if no field is registered there."
  (let ((field (gethash offset *field-offset-table*)))
    (when field
      (intern (mangle-field-name (lstring (slot-value field '|name|)))
              :openldk))))

(defun %unsafe-static-storage (field &key (errorp t))
  "Return the static storage object (+static-CLASSNAME+) for FIELD's
declaring class.  With ERRORP NIL, return NIL instead of signalling when
the storage object does not (yet) exist."
  (let* ((clazz (slot-value field '|clazz|))
         (bin-name (substitute #\/ #\. (lstring (slot-value clazz '|name|))))
         (pkg (%java-class-package clazz))
         (name (format nil "+static-~A+" bin-name)))
    (if errorp
        (symbol-value (intern name pkg))
        (let ((sym (find-symbol name pkg)))
          (when (and sym (boundp sym))
            (symbol-value sym))))))

(defun %unsafe-field-owner-and-key (object offset)
  (let ((field (gethash offset *field-offset-table*)))
    (when field
      (values (or object (%unsafe-static-storage field))
              (%unsafe-slot-key offset)
              t))))

(defun %unsafe-read-native-bits (address byte-count)
  (let ((sap (sb-sys:int-sap address)))
    (loop for index below byte-count
          sum (ash (sb-sys:sap-ref-8 sap index) (* index 8)))))

(defun %unsafe-write-native-bits (address byte-count bits)
  (let ((sap (sb-sys:int-sap address)))
    (dotimes (index byte-count)
      (setf (sb-sys:sap-ref-8 sap index)
            (ldb (byte 8 (* index 8)) bits))))
  nil)

(defun %unsafe-read-array-bits (array offset byte-count)
  (let ((data (java-array-data array)))
    (if (%primitive-data-p data)
        (%data-ref-bits data offset byte-count)
        (loop for index below byte-count
              sum (ash (%unsafe-array-read-byte array (+ offset index))
                       (* index 8))))))

(defun %unsafe-write-array-bits (array offset byte-count bits)
  (let ((data (java-array-data array)))
    (if (%primitive-data-p data)
        (%data-set-bits data offset byte-count bits)
        (dotimes (index byte-count)
          (%unsafe-array-write-byte array (+ offset index)
                                    (ldb (byte 8 (* index 8)) bits)))))
  nil)

(defun %unsafe-normalize-scalar (value kind)
  (ecase kind
    (:boolean (if (or (null value) (eql value 0)) 0 1))
    (:byte (%unsigned-to-signed-byte (logand value #xff)))
    (:short (unsigned-to-signed-short (logand value #xffff)))
    (:char (if (characterp value)
               value
               (code-char (logand value #xffff))))
    (:int (unsigned-to-signed-integer (logand value #xffffffff)))
    (:long (unsigned-to-signed-long (logand value #xffffffffffffffff)))
    (:float (coerce value 'single-float))
    (:double (coerce value 'double-float))))

(defun %unsafe-bits-to-scalar (bits kind)
  (ecase kind
    (:boolean (if (zerop (logand bits #xff)) 0 1))
    (:byte (%unsigned-to-signed-byte (logand bits #xff)))
    (:short (unsigned-to-signed-short (logand bits #xffff)))
    (:char (code-char (logand bits #xffff)))
    (:int (unsigned-to-signed-integer (logand bits #xffffffff)))
    (:long (unsigned-to-signed-long (logand bits #xffffffffffffffff)))
    (:float (float-features:bits-single-float (logand bits #xffffffff)))
    (:double (float-features:bits-double-float
              (logand bits #xffffffffffffffff)))))

(defun %unsafe-scalar-to-bits (value kind)
  (ecase kind
    (:boolean (if (or (null value) (eql value 0)) 0 1))
    (:byte (logand value #xff))
    (:short (logand value #xffff))
    (:char (logand (if (characterp value) (char-code value) value) #xffff))
    (:int (logand value #xffffffff))
    (:long (logand value #xffffffffffffffff))
    (:float (float-features:single-float-bits
             (coerce value 'single-float)))
    (:double (float-features:double-float-bits
              (coerce value 'double-float)))))

(defun %unsafe-scalar-byte-count (kind)
  (ecase kind
    ((:boolean :byte) 1)
    ((:short :char) 2)
    ((:int :float) 4)
    ((:long :double) 8)))

(defun %unsafe-get-scalar (object offset kind)
  (let ((byte-count (%unsafe-scalar-byte-count kind)))
    (cond
      ((typep object 'java-array)
       (%unsafe-bits-to-scalar
        (%unsafe-read-array-bits object offset byte-count) kind))
      (t
       (multiple-value-bind (owner key field-p)
           (%unsafe-field-owner-and-key object offset)
         (cond
           (field-p
            (%unsafe-normalize-scalar (slot-value owner key) kind))
           ((null object)
            (%unsafe-bits-to-scalar
             (%unsafe-read-native-bits offset byte-count) kind))
           (t
            (internal-error "Unsafe ~A read has no field at offset ~D on ~A"
                   kind offset object))))))))

(defun %unsafe-put-scalar (object offset value kind)
  (let* ((byte-count (%unsafe-scalar-byte-count kind))
         (normalized (%unsafe-normalize-scalar value kind)))
    (cond
      ((typep object 'java-array)
       (%unsafe-write-array-bits object offset byte-count
                                 (%unsafe-scalar-to-bits normalized kind)))
      (t
       (multiple-value-bind (owner key field-p)
           (%unsafe-field-owner-and-key object offset)
         (cond
           (field-p (setf (slot-value owner key) normalized))
           ((null object)
            (%unsafe-write-native-bits
             offset byte-count (%unsafe-scalar-to-bits normalized kind)))
           (t
            (internal-error "Unsafe ~A write has no field at offset ~D on ~A"
                   kind offset object)))))))
  nil)

(defun %unsafe-get-reference (object offset)
  (cond
    ((typep object 'java-array)
     (jaref object (%unsafe-array-index object offset)))
    (t
     (multiple-value-bind (owner key field-p)
         (%unsafe-field-owner-and-key object offset)
       (cond
         (field-p (slot-value owner key))
         ((null object) (gethash offset *unsafe-reference-memory-table*))
         (t (internal-error "Unsafe reference read has no field at offset ~D on ~A"
                   offset object)))))))

(defun %unsafe-put-reference (object offset value)
  (cond
    ((typep object 'java-array)
     (setf (jaref object (%unsafe-array-index object offset)) value))
    (t
     (multiple-value-bind (owner key field-p)
         (%unsafe-field-owner-and-key object offset)
       (cond
         (field-p (setf (slot-value owner key) value))
         ((null object)
          (setf (gethash offset *unsafe-reference-memory-table*) value))
         (t (internal-error "Unsafe reference write has no field at offset ~D on ~A"
                   offset object))))))
  nil)

(defmacro %define-internal-unsafe-scalar-accessors (stem descriptor kind)
  ;; OpenLDK's Lisp entry-point names contain only the JVM parameter
  ;; descriptor.  The return descriptor is deliberately not part of the
  ;; symbol (matching every other native method in this file).
  (let ((get (intern (format nil "get~A(Ljava/lang/Object;J)"
                             stem) :openldk))
        (put (intern (format nil "put~A(Ljava/lang/Object;J~A)"
                             stem descriptor) :openldk))
        (get-volatile
          (intern (format nil "get~AVolatile(Ljava/lang/Object;J)"
                          stem) :openldk))
        (put-volatile
          (intern (format nil "put~AVolatile(Ljava/lang/Object;J~A)"
                          stem descriptor) :openldk)))
    `(progn
       (defmethod ,get ((unsafe |jdk/internal/misc/Unsafe|) object offset)
         (declare (ignore unsafe))
         (%unsafe-get-scalar object offset ,kind))
       (defmethod ,put ((unsafe |jdk/internal/misc/Unsafe|) object offset value)
         (declare (ignore unsafe))
         (%unsafe-put-scalar object offset value ,kind))
       (defmethod ,get-volatile
           ((unsafe |jdk/internal/misc/Unsafe|) object offset)
         (declare (ignore unsafe))
         (%unsafe-get-scalar object offset ,kind))
       (defmethod ,put-volatile
           ((unsafe |jdk/internal/misc/Unsafe|) object offset value)
         (declare (ignore unsafe))
         (%unsafe-put-scalar object offset value ,kind)))))

(%define-internal-unsafe-scalar-accessors "Boolean" "Z" :boolean)
(%define-internal-unsafe-scalar-accessors "Byte" "B" :byte)
(%define-internal-unsafe-scalar-accessors "Short" "S" :short)
(%define-internal-unsafe-scalar-accessors "Char" "C" :char)
(%define-internal-unsafe-scalar-accessors "Int" "I" :int)
(%define-internal-unsafe-scalar-accessors "Long" "J" :long)
(%define-internal-unsafe-scalar-accessors "Float" "F" :float)
(%define-internal-unsafe-scalar-accessors "Double" "D" :double)

(defmethod |getReference(Ljava/lang/Object;J)Ljava/lang/Object;|
    ((unsafe |jdk/internal/misc/Unsafe|) object offset)
  (declare (ignore unsafe))
  (%unsafe-get-reference object offset))

(defmethod |putReference(Ljava/lang/Object;JLjava/lang/Object;)V|
    ((unsafe |jdk/internal/misc/Unsafe|) object offset value)
  (declare (ignore unsafe))
  (%unsafe-put-reference object offset value))

(defmethod |getReferenceVolatile(Ljava/lang/Object;J)Ljava/lang/Object;|
    ((unsafe |jdk/internal/misc/Unsafe|) object offset)
  (declare (ignore unsafe))
  (%unsafe-get-reference object offset))

(defmethod |putReferenceVolatile(Ljava/lang/Object;JLjava/lang/Object;)V|
    ((unsafe |jdk/internal/misc/Unsafe|) object offset value)
  (declare (ignore unsafe))
  (%unsafe-put-reference object offset value))

(defun %unsafe-compare-and-exchange-scalar
    (object offset expected replacement kind)
  (bordeaux-threads:with-recursive-lock-held (*cas-lock*)
    (let ((old (%unsafe-get-scalar object offset kind)))
      (when (eql old (%unsafe-normalize-scalar expected kind))
        (%unsafe-put-scalar object offset replacement kind))
      old)))

(defun %unsafe-compare-and-exchange-reference
    (object offset expected replacement)
  (bordeaux-threads:with-recursive-lock-held (*cas-lock*)
    (let ((old (%unsafe-get-reference object offset)))
      (when (eq old expected)
        (%unsafe-put-reference object offset replacement))
      old)))

(defmethod |compareAndSetInt(Ljava/lang/Object;JII)Z|
    ((unsafe |jdk/internal/misc/Unsafe|) object offset expected replacement)
  (declare (ignore unsafe))
  (if (eql expected (%unsafe-compare-and-exchange-scalar
                     object offset expected replacement :int))
      1 0))

(defmethod |compareAndExchangeInt(Ljava/lang/Object;JII)I|
    ((unsafe |jdk/internal/misc/Unsafe|) object offset expected replacement)
  (declare (ignore unsafe))
  (%unsafe-compare-and-exchange-scalar
   object offset expected replacement :int))

(defmethod |compareAndSetLong(Ljava/lang/Object;JJJ)Z|
    ((unsafe |jdk/internal/misc/Unsafe|) object offset expected replacement)
  (declare (ignore unsafe))
  (if (eql expected (%unsafe-compare-and-exchange-scalar
                     object offset expected replacement :long))
      1 0))

(defmethod |compareAndExchangeLong(Ljava/lang/Object;JJJ)J|
    ((unsafe |jdk/internal/misc/Unsafe|) object offset expected replacement)
  (declare (ignore unsafe))
  (%unsafe-compare-and-exchange-scalar
   object offset expected replacement :long))

(defmethod |compareAndSetReference(Ljava/lang/Object;JLjava/lang/Object;Ljava/lang/Object;)Z|
    ((unsafe |jdk/internal/misc/Unsafe|) object offset expected replacement)
  (declare (ignore unsafe))
  (if (eq expected (%unsafe-compare-and-exchange-reference
                    object offset expected replacement))
      1 0))

(defmethod |compareAndExchangeReference(Ljava/lang/Object;JLjava/lang/Object;Ljava/lang/Object;)Ljava/lang/Object;|
    ((unsafe |jdk/internal/misc/Unsafe|) object offset expected replacement)
  (declare (ignore unsafe))
  (%unsafe-compare-and-exchange-reference
   object offset expected replacement))

;;; java.util.concurrent.atomic.AtomicReference implements its atomics through a
;;; VarHandle on its `value` field (VALUE.compareAndSet(this, ...)).  OpenLDK
;;; doesn't lower field VarHandles, so those polymorphic calls have no
;;; implementation -- clojure.lang.RT.<clinit> hits this and fails to
;;; initialize (ldk-uyv).  Override the AtomicReference methods (bytecode, so
;;; installed via *native-overrides* before the JIT compiles their VarHandle
;;; bodies) to operate on the `value` slot directly, serialized by one lock
;;; (identity compare = JVM ==).
(defvar *atomic-field-lock* (bordeaux-threads:make-lock "atomic-field"))

(let ((cas (lambda (this expected update)
             (bordeaux-threads:with-lock-held (*atomic-field-lock*)
               (if (eq (slot-value this '|value|) expected)
                   (progn (setf (slot-value this '|value|) update) 1)
                   0)))))
  (dolist (sig '("compareAndSet(Ljava/lang/Object;Ljava/lang/Object;)Z"
                 "weakCompareAndSet(Ljava/lang/Object;Ljava/lang/Object;)Z"
                 "weakCompareAndSetPlain(Ljava/lang/Object;Ljava/lang/Object;)Z"
                 "weakCompareAndSetAcquire(Ljava/lang/Object;Ljava/lang/Object;)Z"
                 "weakCompareAndSetRelease(Ljava/lang/Object;Ljava/lang/Object;)Z"
                 "weakCompareAndSetVolatile(Ljava/lang/Object;Ljava/lang/Object;)Z"))
    (setf (gethash (concatenate 'string
                                "java/util/concurrent/atomic/AtomicReference." sig)
                   *native-overrides*)
          cas)))

(setf (gethash "java/util/concurrent/atomic/AtomicReference.getAndSet(Ljava/lang/Object;)Ljava/lang/Object;"
               *native-overrides*)
      (lambda (this update)
        (bordeaux-threads:with-lock-held (*atomic-field-lock*)
          (prog1 (slot-value this '|value|)
            (setf (slot-value this '|value|) update)))))

(setf (gethash "java/util/concurrent/atomic/AtomicReference.compareAndExchange(Ljava/lang/Object;Ljava/lang/Object;)Ljava/lang/Object;"
               *native-overrides*)
      (lambda (this expected update)
        (bordeaux-threads:with-lock-held (*atomic-field-lock*)
          (let ((current (slot-value this '|value|)))
            (when (eq current expected)
              (setf (slot-value this '|value|) update))
            current))))

(defmethod |fullFence()V| ((unsafe |jdk/internal/misc/Unsafe|))
  (declare (ignore unsafe))
  nil)

(defmethod |arrayIndexScale0(Ljava/lang/Class;)I| ((unsafe |jdk/internal/misc/Unsafe|) array)
  (declare (ignore unsafe))
  (%unsafe-array-index-scale array))

(defmethod |addressSize0()| ((unsafe |jdk/internal/misc/Unsafe|))
  8)

(defclass %synthetic-field ()
  ((|name| :initarg :name :accessor %sf-name)
   (|clazz| :initarg :clazz :accessor %sf-clazz))
  (:documentation "Lightweight stand-in for java/lang/reflect/Field used by objectFieldOffset1."))

(defmethod |objectFieldOffset1(Ljava/lang/Class;Ljava/lang/String;)J| ((unsafe |jdk/internal/misc/Unsafe|) clazz field-name)
  "JDK 9+ objectFieldOffset by class and field name.
   Creates a synthetic field descriptor and registers it, same as the JDK 8 path."
  (declare (ignore unsafe))
  (let* ((field-str (lstring field-name))
         (f (make-instance '%synthetic-field
                           :name (ijstring field-str)
                           :clazz clazz)))
    (%register-field-offset f)))

;;; JDK 9+ native Unsafe memory methods (0-suffixed variants).
;;; In JDK 17, the public Unsafe methods (allocateMemory, copyMemory, etc.)
;;; are bytecoded wrappers that delegate to private native 0-suffixed methods.
;;; We provide defmethods for the native methods directly.

;; NativeBuffers.copyCStringToNativeBuffer: static override that properly
;; allocates native memory and copies the byte array, bypassing the broken
;; Unsafe.allocateMemory bytecoded wrapper chain.
;; Override both NativeBuffers.copyCStringToNativeBuffer and
;; UnixNativeDispatcher.copyToNativeBuffer to properly allocate native memory.

(defun %make-native-buffer-from-bytes (byte-array)
  "Create a NativeBuffer with properly allocated native memory from a Java byte array."
  (let* ((data (java-array-data byte-array))
         (len (length data))
         (mem (sb-alien:make-alien sb-alien:char (1+ len)))
         (ptr (sb-sys:sap-int (sb-alien:alien-sap mem)))
         (sap (sb-alien:alien-sap mem)))
    (setf (gethash ptr *unsafe-memory-table*) (cons mem (1+ len)))
    ;; Copy bytes to native memory.  Java bytes are signed; SAP-REF-8 stores
    ;; unsigned, so mask (a UTF-8 path byte >= #x80 is stored negative).
    (loop for i below len
          do (setf (sb-sys:sap-ref-8 sap i) (logand (aref data i) #xFF)))
    ;; Null terminate
    (setf (sb-sys:sap-ref-8 sap len) 0)
    ;; Ensure the NativeBuffer class is loaded before creating instances
    (classload "sun/nio/fs/NativeBuffer")
    ;; Create and return a NativeBuffer
    (let ((buffer (%make-java-instance "sun/nio/fs/NativeBuffer")))
      (when (slot-exists-p buffer '|address|)
        (setf (slot-value buffer '|address|) ptr))
      (when (slot-exists-p buffer '|size|)
        (setf (slot-value buffer '|size|) (1+ len)))
      buffer)))

(setf (gethash "sun/nio/fs/NativeBuffers.copyCStringToNativeBuffer([B)Lsun/nio/fs/NativeBuffer;" *native-overrides*)
      (lambda (cstr) (%make-native-buffer-from-bytes cstr)))

(setf (gethash "sun/nio/fs/UnixNativeDispatcher.copyToNativeBuffer(Lsun/nio/fs/UnixPath;)Lsun/nio/fs/NativeBuffer;" *native-overrides*)
      (lambda (path)
        (let ((byte-array (slot-value path '|path|)))
          (%make-native-buffer-from-bytes byte-array))))

;;; NativeBuffer.free() calls cleanable.clean(), but the Cleaner
;;; infrastructure (CleanerFactory/CleanerImpl) requires daemon threads
;;; that may not work in OpenLDK.  Bypass the Cleanable and free the
;;; native memory directly via *unsafe-memory-table*.
(setf (gethash "sun/nio/fs/NativeBuffer.free()V" *native-overrides*)
      (lambda (this)
        (let ((address (slot-value this '|address|)))
          (when (/= address 0)
            (when-let ((entry (gethash address *unsafe-memory-table*)))
              (%free-unsafe-entry entry)
              (remhash address *unsafe-memory-table*))
            (setf (slot-value this '|address|) 0)))))

(defun %free-unsafe-entry (entry)
  "Free the memory of the *UNSAFE-MEMORY-TABLE* ENTRY, (alien . size); a
block restored after an image restart (:restored . size) is never freed."
  (when (typep (car entry) 'sb-alien:alien)
    (ignore-errors (sb-alien:free-alien (car entry)))))

(defmethod |allocateMemory0(J)J| ((unsafe |jdk/internal/misc/Unsafe|) size)
  (let* ((mem (sb-alien:make-alien sb-alien:char size))
         (ptr (sb-sys:sap-int (sb-alien:alien-sap mem))))
    (setf (gethash ptr *unsafe-memory-table*) (cons mem size))
    ptr))

(defmethod |freeMemory0(J)V| ((unsafe |jdk/internal/misc/Unsafe|) address)
  (when-let (entry (gethash address *unsafe-memory-table*))
    (%free-unsafe-entry entry)
    (remhash address *unsafe-memory-table*)))

(defmethod |putLong0(JJ)| ((unsafe |jdk/internal/misc/Unsafe|) address value)
  (setf (sb-sys:signed-sap-ref-64 (sb-sys:int-sap address) 0) value))

(defmethod |getByte0(J)| ((unsafe |jdk/internal/misc/Unsafe|) address)
  (sb-sys:sap-ref-8 (sb-sys:int-sap address) 0))

(defmethod |putByte0(JB)| ((unsafe |jdk/internal/misc/Unsafe|) address value)
  (setf (sb-sys:sap-ref-8 (sb-sys:int-sap address) 0) (logand value #xFF)))

(defmethod |copyMemory0(Ljava/lang/Object;JLjava/lang/Object;JJ)V| ((unsafe |jdk/internal/misc/Unsafe|) source source-offset dest dest-offset length)
  (declare (ignore unsafe))
  (cond
    ;; Native memory → Java array (common for ICU data loading)
    ((and (null source) dest)
     (let ((sap (sb-sys:int-sap source-offset)))
       (loop for i below length
             do (%unsafe-array-write-byte
                 dest (+ dest-offset i) (sb-sys:sap-ref-8 sap i)))))
    ;; Java array → Java array
    ((and source dest)
     (let ((bytes (make-array length :element-type '(unsigned-byte 8))))
       (loop for i below length
             do (setf (aref bytes i)
                      (%unsafe-array-read-byte source (+ source-offset i))))
       (loop for i below length
             do (%unsafe-array-write-byte dest (+ dest-offset i)
                                          (aref bytes i)))))
    ;; Native memory → native memory
    ((and (null source) (null dest))
     (let ((src-sap (sb-sys:int-sap source-offset))
           (dst-sap (sb-sys:int-sap dest-offset)))
       (loop for i below length
             do (setf (sb-sys:sap-ref-8 dst-sap i)
                      (sb-sys:sap-ref-8 src-sap i)))))
    ;; Java array → native memory
    (t
     (let ((dst-sap (sb-sys:int-sap dest-offset)))
       (loop for i below length
             do (setf (sb-sys:sap-ref-8 dst-sap i)
                      (%unsafe-array-read-byte source
                                               (+ source-offset i))))))))

(defmethod |reallocateMemory0(JJ)J| ((unsafe |jdk/internal/misc/Unsafe|) address new-size)
  (if (zerop address)
      ;; Zero address means fresh allocation (realloc(NULL, size) == malloc(size))
      (|allocateMemory0(J)J| unsafe new-size)
      ;; Allocate new block, copy old data, free old block
      (let* ((new-mem (sb-alien:make-alien sb-alien:char new-size))
             (new-ptr (sb-sys:sap-int (sb-alien:alien-sap new-mem)))
             (new-sap (sb-alien:alien-sap new-mem))
             (old-entry (gethash address *unsafe-memory-table*)))
        (when old-entry
          (let* ((old-size (cdr old-entry))
                 (old-sap (sb-sys:int-sap address))
                 (copy-size (min old-size new-size)))
            (loop for i below copy-size
                  do (setf (sb-sys:sap-ref-8 new-sap i)
                           (sb-sys:sap-ref-8 old-sap i)))
            (%free-unsafe-entry old-entry)
            (remhash address *unsafe-memory-table*)))
        (setf (gethash new-ptr *unsafe-memory-table*) (cons new-mem new-size))
        new-ptr)))

(defmethod |setMemory0(Ljava/lang/Object;JJB)V| ((unsafe |jdk/internal/misc/Unsafe|) obj offset bytes value)
  (if (null obj)
      ;; Direct memory — use memset for bulk operations
      (progn
        (sb-alien:alien-funcall
         (sb-alien:extern-alien "memset"
                                (function (* t) (* t) sb-alien:int sb-alien:unsigned-long))
         (sb-sys:int-sap offset)
         (logand value #xFF)
         bytes)
        nil)
      ;; Array memory
      (loop for i below bytes
            do (setf (jaref obj (+ offset i)) (logand value #xFF)))))

;;; Unsafe park/unpark — used by LockSupport and virtual thread infrastructure.

;;; A platform thread's park state: LockSupport's permit, and the mutex and
;;; condition variable a parked thread waits on.  PARKED is true while the
;;; thread waits, so interrupt0 wakes it rather than throwing into it.
(defstruct (park-state (:constructor make-park-state ()))
  (mutex (sb-thread:make-mutex :name "park"))
  (waitqueue (sb-thread:make-waitqueue :name "park"))
  (permit nil)
  (parked nil))

(defvar *park-states* (make-hash-table :test #'eq :weakness :key :synchronized t)
  "java.lang.Thread -> its PARK-STATE.")

(defun %park-state (java-thread)
  (or (gethash java-thread *park-states*)
      (sb-ext:with-locked-hash-table (*park-states*)
        (or (gethash java-thread *park-states*)
            (setf (gethash java-thread *park-states*) (make-park-state))))))

(defun %unpark-java-thread (java-thread)
  "Give JAVA-THREAD the permit and wake it if it is parked.  Return true if
it was parked."
  (let ((state (%park-state java-thread)))
    (sb-thread:with-mutex ((park-state-mutex state))
      (setf (park-state-permit state) t)
      (sb-thread:condition-broadcast (park-state-waitqueue state))
      (park-state-parked state))))

(defun %park-timeout (is-absolute time)
  "Seconds to wait for park(IS-ABSOLUTE, TIME): NIL for no limit, or a
number, zero or less if the deadline has passed.  TIME is nanoseconds from
now (0 for no limit), or with IS-ABSOLUTE, a deadline in epoch ms."
  (cond
    ((and is-absolute (not (eql is-absolute 0)))
     (/ (- time (|java/lang/System.currentTimeMillis()J|)) 1000))
    ((> time 0) (/ time 1000000000))
    (t nil)))

(defmethod |park(ZJ)V| ((unsafe |jdk/internal/misc/Unsafe|) is-absolute time)
  "Park the current thread until it is unparked, interrupted, or the time
runs out (LockSupport.park)."
  (declare (ignore unsafe))
  #+sb-fiber
  (when (in-fiber-p)
    (let ((fiber (sb-thread:current-fiber)))
      ;; Check and consume permit
      (when (gethash fiber *fiber-park-flags*)
        (remhash fiber *fiber-park-flags*)
        (return-from |park(ZJ)V|))
      ;; Park with predicate: wake when permit is set
      (if (> time 0)
          (sb-thread:fiber-park (lambda () (gethash fiber *fiber-park-flags*))
                                :timeout (/ time 1000000000.0d0))
          (sb-thread:fiber-park (lambda () (gethash fiber *fiber-park-flags*))))
      ;; Consume permit after waking
      (remhash fiber *fiber-park-flags*))
    (return-from |park(ZJ)V|))
  ;; Platform thread: consume the permit, or wait for one.
  (let* ((java-thread (|java/lang/Thread.currentThread()Ljava/lang/Thread;|))
         (state (%park-state java-thread)))
    (sb-thread:with-mutex ((park-state-mutex state))
      (unless (or (park-state-permit state)
                  (gethash java-thread *thread-interrupted*))
        (let ((timeout (%park-timeout is-absolute time)))
          (when (or (null timeout) (plusp timeout))
            (setf (park-state-parked state) t)
            (unwind-protect
                 (sb-thread:condition-wait (park-state-waitqueue state)
                                           (park-state-mutex state)
                                           :timeout timeout)
              (setf (park-state-parked state) nil)))))
      (setf (park-state-permit state) nil))
    nil))

(defmethod |unpark(Ljava/lang/Object;)V| ((unsafe |jdk/internal/misc/Unsafe|) thread)
  "Unpark a thread by granting a permit."
  (declare (ignore unsafe))
  #+sb-fiber
  (let ((fiber (gethash thread *java-to-fibers*)))
    (when fiber
      ;; Set the permit flag — the fiber's park predicate will see this
      (setf (gethash fiber *fiber-park-flags*) t)
      (return-from |unpark(Ljava/lang/Object;)V|)))
  ;; Platform thread
  (when thread
    (%unpark-java-thread thread))
  nil)

;;; JDK 9+ renamed Unsafe CAS and accessor methods.
;;; These delegate to the existing sun/misc/Unsafe implementations via inheritance.

(defmethod |compareAndSetInt(Ljava/lang/Object;JII)Z| ((unsafe |sun/misc/Unsafe|) obj field-id expected-value new-value)
  (|compareAndSwapInt(Ljava/lang/Object;JII)Z| unsafe obj field-id expected-value new-value))

(defmethod |compareAndSetLong(Ljava/lang/Object;JJJ)Z| ((unsafe |sun/misc/Unsafe|) obj field-id expected-value new-value)
  (|compareAndSwapLong(Ljava/lang/Object;JJJ)Z| unsafe obj field-id expected-value new-value))

(defmethod |compareAndSetReference(Ljava/lang/Object;JLjava/lang/Object;Ljava/lang/Object;)Z| ((unsafe |sun/misc/Unsafe|) obj field-id expected-value new-value)
  (|compareAndSwapObject(Ljava/lang/Object;JLjava/lang/Object;Ljava/lang/Object;)Z| unsafe obj field-id expected-value new-value))

(defmethod |compareAndSetObject(Ljava/lang/Object;JLjava/lang/Object;Ljava/lang/Object;)| ((unsafe |sun/misc/Unsafe|) obj field-id expected-value new-value)
  (|compareAndSwapObject(Ljava/lang/Object;JLjava/lang/Object;Ljava/lang/Object;)Z| unsafe obj field-id expected-value new-value))

;;; Byte/boolean CAS — the JDK's Java implementation reads a surrounding int
;;; from raw memory, which doesn't work with CLOS slots.  Override both
;;; sun/misc/Unsafe and jdk/internal/misc/Unsafe (the JDK 21 bytecode calls
;;; the latter directly).
(defun %cas-byte-field (obj offset expected new-val)
  "CAS a byte/boolean field using CLOS slots instead of raw memory."
  (bordeaux-threads:with-recursive-lock-held (*cas-lock*)
    (let* ((key (%unsafe-slot-key offset))
           (current (slot-value obj key)))
      (if (eql current expected)
          (progn (setf (slot-value obj key) new-val) 1)
          0))))

(defun %cae-byte-field (obj offset expected new-val)
  "Compare-and-exchange a byte/boolean field using CLOS slots."
  (bordeaux-threads:with-recursive-lock-held (*cas-lock*)
    (let* ((key (%unsafe-slot-key offset))
           (current (slot-value obj key)))
      (when (eql current expected)
        (setf (slot-value obj key) new-val))
      current)))

(defmethod |compareAndSetByte(Ljava/lang/Object;JBB)Z| ((unsafe |sun/misc/Unsafe|) obj offset expected new-val)
  (declare (ignore unsafe))
  (%cas-byte-field obj offset expected new-val))

(defmethod |compareAndExchangeByte(Ljava/lang/Object;JBB)B| ((unsafe |sun/misc/Unsafe|) obj offset expected new-val)
  (declare (ignore unsafe))
  (%cae-byte-field obj offset expected new-val))

(defmethod |compareAndSetBoolean(Ljava/lang/Object;JZZ)Z| ((unsafe |sun/misc/Unsafe|) obj offset expected new-val)
  (declare (ignore unsafe))
  (%cas-byte-field obj offset expected new-val))

;; Override jdk/internal/misc/Unsafe bytecode methods via *native-overrides*
(setf (gethash "jdk/internal/misc/Unsafe.compareAndSetByte(Ljava/lang/Object;JBB)Z" *native-overrides*)
      (lambda (unsafe obj offset expected new-val)
        (declare (ignore unsafe))
        (%cas-byte-field obj offset expected new-val)))

(setf (gethash "jdk/internal/misc/Unsafe.compareAndExchangeByte(Ljava/lang/Object;JBB)B" *native-overrides*)
      (lambda (unsafe obj offset expected new-val)
        (declare (ignore unsafe))
        (%cae-byte-field obj offset expected new-val)))

(setf (gethash "jdk/internal/misc/Unsafe.compareAndSetBoolean(Ljava/lang/Object;JZZ)Z" *native-overrides*)
      (lambda (unsafe obj offset expected new-val)
        (declare (ignore unsafe))
        (%cas-byte-field obj offset expected new-val)))

;;; ArraysSupport.vectorizedMismatch — the JDK implementation reads arrays
;;; as raw longs via Unsafe.getLongUnaligned, which doesn't work with CLOS
;;; arrays.  Compare elements directly instead.
(setf (gethash "jdk/internal/util/ArraysSupport.vectorizedMismatch(Ljava/lang/Object;JLjava/lang/Object;JII)I" *native-overrides*)
      (lambda (a a-offset b b-offset length log2-scale)
        (declare (ignore log2-scale))
        ;; a-offset/b-offset are byte offsets; for OpenLDK ARRAY_*_BASE_OFFSET=0,
        ;; so they are element indices (for byte arrays).
        ;; Compare element by element and return first mismatch index,
        ;; or ~length (= -(length+1)) meaning "all matched".
        (let ((a-data (slot-value a 'data))
              (b-data (slot-value b 'data)))
          (dotimes (i length (lognot length))
            (unless (eql (aref a-data (+ a-offset i))
                         (aref b-data (+ b-offset i)))
              (return i))))))

;;; Reflective constructor invocation — JDK 21 uses
;;; DirectConstructorHandleAccessor$NativeAccessor.newInstance0 (native).
(defun %java-class-to-type-descriptor (java-class)
  "Convert a java.lang.Class to its JVM type descriptor string."
  (let ((name (lstring (slot-value java-class '|name|))))
    (cond
      ((string= name "int")     "I")
      ((string= name "long")    "J")
      ((string= name "byte")    "B")
      ((string= name "boolean") "Z")
      ((string= name "char")    "C")
      ((string= name "short")   "S")
      ((string= name "float")   "F")
      ((string= name "double")  "D")
      ((string= name "void")    "V")
      ((char= (char name 0) #\[) (substitute #\/ #\. name))
      (t (format nil "L~A;" (substitute #\/ #\. name))))))

(defun %build-init-descriptor (param-types-array)
  "Build the parameter portion of a constructor descriptor like (Ljava/lang/String;I)
from a Class[] array.  Note: OpenLDK method symbols omit the return type."
  (if (or (null param-types-array)
          (zerop (java-array-length param-types-array)))
      "()"
      (with-output-to-string (s)
        (write-char #\( s)
        (dotimes (i (java-array-length param-types-array))
          (write-string (%java-class-to-type-descriptor (jaref param-types-array i)) s))
        (write-char #\) s))))

;; NOTE: DirectConstructorHandleAccessor$NativeAccessor.newInstance0 is defined
;; below (delegating to NativeConstructorAccessorImpl.newInstance0); the earlier
;; standalone copy that lived here was a duplicate and has been removed.

;;; PreviewFeatures.isPreviewEnabled — always false (we don't support --enable-preview).
(defun |jdk/internal/misc/PreviewFeatures.isPreviewEnabled()Z| ()
  0)

;;; ProcessImpl.init — static native initializer for process spawning.
;;; On a real JVM this sets up signal handling for child processes.
;;; We stub it as a no-op; forkAndExec is handled separately.
(defun |java/lang/ProcessImpl.init()V| ()
  nil)

;;; NOTE: In JDK 17, the compiled bytecode methods getObjectVolatile/putObject/etc.
;;; delegate to getReference/putReference/etc. (native methods).
;;; So the native "Reference" variants must contain the actual implementation,
;;; NOT delegate back to the "Object" variants (which would cause infinite recursion).
;;; The implementations are defined later, after the Object variants.

(defmethod |hashCode()I| (obj)
  (|java/lang/System.identityHashCode(Ljava/lang/Object;)I| obj))

(defclass %array-base-offset ()
  ((array :initarg :array)))

(defmethod |arrayBaseOffset(Ljava/lang/Class;)I| ((unsafe |sun/misc/Unsafe|) array)
  0)
;  (make-instance '%array-base-offset :array array))

(defmethod |arrayIndexScale(Ljava/lang/Class;)I| ((unsafe |sun/misc/Unsafe|) array)
  (declare (ignore unsafe))
  (%unsafe-array-index-scale array))

(defmethod |addressSize()I| ((unsafe |sun/misc/Unsafe|))
  8)

(defmethod |isArray()Z| ((class |java/lang/Class|))
  (let ((name-string (lstring (slot-value class '|name|))))
    (if (eq #\[ (char name-string 0))
        1
        0)))

(defmethod |getComponentType()Ljava/lang/Class;| ((class |java/lang/Class|))
  (let ((cn (lstring (slot-value class '|name|))))
    (if (eq #\[ (char cn 0))
        (let ((ct (or (and (slot-exists-p class '|componentType|)
                           (slot-value class '|componentType|))
                      (%bin-type-name-to-class (subseq cn 1)))))
          (unless ct
            (error (format nil "ERROR: can't determine component type for ~A" cn)))
          ct)
        nil)))

;; JDK 15+: Class.isHidden() — no classes in OpenLDK are hidden
(defmethod |isHidden()Z| ((class |java/lang/Class|))
  0)

;; JDK 21: Class.getClassAccessFlagsRaw0() — return raw access flags
(defmethod |getClassAccessFlagsRaw0()I| ((this |java/lang/Class|))
  (let ((ldk-class (get-ldk-class-for-java-class this)))
    (if ldk-class
        (slot-value ldk-class 'access-flags)
        0)))

;; JDK 21: Class.getClassFileVersion0() — return class file major version
(defmethod |getClassFileVersion0()I| ((this |java/lang/Class|))
  (let ((ldk-class (get-ldk-class-for-java-class this)))
    (if (and ldk-class (slot-boundp ldk-class 'major-version)
             (slot-value ldk-class 'major-version))
        (slot-value ldk-class 'major-version)
        65))) ;; Default to JDK 21 class file version

;; Module support stubs.
;; We provide a single shared unnamed Module so that Class.getModule()
;; never returns nil and Module.isNamed() returns false.
(defvar *unnamed-module* nil)

(defun %get-unnamed-module ()
  (or *unnamed-module*
      (handler-case
          (progn
            (classload "java/lang/Module")
            (let ((m (%make-java-instance "java/lang/Module")))
              (when (slot-exists-p m '|name|)
                (setf (slot-value m '|name|) nil))
              (setf *unnamed-module* m)))
        (condition () nil))))

;; :around ensures this runs even when bytecoded getModule() is compiled later
(defmethod |getModule()Ljava/lang/Module;| :around ((class |java/lang/Class|))
  (let ((result (call-next-method)))
    (or result (%get-unnamed-module))))

;; JDK 9+: JavaLangAccess.defineUnnamedModule — return a fresh unnamed Module
(defmethod |defineUnnamedModule(Ljava/lang/ClassLoader;)Ljava/lang/Module;| (this classloader)
  (declare (ignore this classloader))
  (%get-unnamed-module))

;; JDK 9+: JavaLangAccess.addEnableNativeAccess — identity stub
(defmethod |addEnableNativeAccess(Ljava/lang/Module;)Ljava/lang/Module;| (this module)
  (declare (ignore this))
  module)

(defun |jdk/internal/loader/BootLoader.getSystemPackageLocation(Ljava/lang/String;)Ljava/lang/String;| (name)
  "Location of a boot-loader (system) package: everything OpenLDK loads
from the jimage lives in the runtime image, so report jrt:.  A non-null
result is what lets Class.getPackage() define Package objects for
java.* classes.  Only a package the runtime image has gets one: for any
other name the JVM answers null, and a made-up Package for it misleads
callers (Kawa resolved unknown names as java.lang.Package objects).
The runtime image is the jimage or, in a JDK that has them, the jmods.
Without either on the classpath, every name is reported, as before."
  (when (%runtime-image-package-p (substitute #\/ #\. (lstring name)))
    (jstring "jrt:/java.base")))

;; JDK 9+: BootLoader native — associate unnamed module with boot loader (no-op)
(defun |jdk/internal/loader/BootLoader.setBootLoaderUnnamedModule0(Ljava/lang/Module;)V| (module)
  (declare (ignore module))
  nil)

;; JDK 9+: Module.defineModule0 registers a module (name, packages, openness)
;; with the VM's module graph.  OpenLDK runs everything as one unnamed module
;; and enforces no module boundaries, so registration is a no-op.  Reached via
;; java.lang.reflect.Proxy, which defines a dynamic module for each proxy class.
(defun |java/lang/Module.defineModule0(Ljava/lang/Module;ZLjava/lang/String;Ljava/lang/String;[Ljava/lang/Object;)V|
    (module is-open version location packages)
  (declare (ignore module is-open version location packages))
  nil)

;; The remaining Module graph natives (reads/exports edges) are likewise no-ops:
;; OpenLDK enforces no module boundaries, so all reads/exports already succeed.
(defun |java/lang/Module.addReads0(Ljava/lang/Module;Ljava/lang/Module;)V| (from to)
  (declare (ignore from to))
  nil)

(defun |java/lang/Module.addExports0(Ljava/lang/Module;Ljava/lang/String;Ljava/lang/Module;)V| (from pkg to)
  (declare (ignore from pkg to))
  nil)

(defun |java/lang/Module.addExportsToAll0(Ljava/lang/Module;Ljava/lang/String;)V| (from pkg)
  (declare (ignore from pkg))
  nil)

(defun |java/lang/Module.addExportsToAllUnnamed0(Ljava/lang/Module;Ljava/lang/String;)V| (from pkg)
  (declare (ignore from pkg))
  nil)

;; BootLoader$PackageHelper.findModule resolves a package location back to its
;; java.base Module via ModuleLayer.boot().findModule(name).  OpenLDK only runs
;; System.initPhase1 (VM.initLevel 1), so the boot module layer is never built
;; and ModuleLayer.boot() is null -- Modules.findLoadedModule then NPEs.  Return
;; null (module not found), which routes definePackage() down its manifest/URL
;; fallback (BuiltinClassLoader.defineOrCheckPackage) to produce a module-less
;; Package -- enough for Class.getPackage() to work on java.* classes.
(setf (gethash "jdk/internal/loader/BootLoader$PackageHelper.findModule(Ljava/lang/String;)Ljava/lang/Module;"
               *native-overrides*)
      (lambda (location) (declare (ignore location)) nil))

;; Return our boot-class-loader for getSystemClassLoader() so it's never nil
(defun |java/lang/ClassLoader.getSystemClassLoader()Ljava/lang/ClassLoader;| ()
  *boot-class-loader*)
(setf (gethash "java/lang/ClassLoader.getSystemClassLoader()Ljava/lang/ClassLoader;" *native-overrides*)
      #'|java/lang/ClassLoader.getSystemClassLoader()Ljava/lang/ClassLoader;|)

;; JDK 17: registerAsParallelCapable always succeeds (avoids InternalError in <clinit>)
(defun |java/lang/ClassLoader.registerAsParallelCapable()Z| ()
  1)
(setf (gethash "java/lang/ClassLoader.registerAsParallelCapable()Z" *native-overrides*)
      #'|java/lang/ClassLoader.registerAsParallelCapable()Z|)

;; JDK 16+: Reference.refersTo0 — native method for weak reference checking
;; Used by ThreadLocalMap.getEntry() to match WeakReference keys.
(defmethod |refersTo0(Ljava/lang/Object;)Z| ((this |java/lang/ref/Reference|) obj)
  (if (eq (slot-value this '|referent|) obj) 1 0))

;; JDK 9+: Reflection.getClassAccessFlags — return class modifiers
(defun |jdk/internal/reflect/Reflection.getClassAccessFlags(Ljava/lang/Class;)I| (java-class)
  (let ((ldk-class (get-ldk-class-for-java-class java-class)))
    (if (and ldk-class (slot-boundp ldk-class 'access-flags))
        (access-flags ldk-class)
        ;; Default: public (0x0001)
        1)))

;; JDK 9+: JavaLangReflectAccess.getExecutableSharedParameterTypes — fallback
;; for when langReflectAccess on ReflectionFactory is nil (AccessibleObject
;; <clinit> hasn't run yet).  Delegates to the Executable's parameterTypes field.
(defmethod |getExecutableSharedParameterTypes(Ljava/lang/reflect/Executable;)[Ljava/lang/Class;| (this exec)
  (declare (ignore this))
  (when (and exec (slot-exists-p exec '|parameterTypes|))
    (slot-value exec '|parameterTypes|)))

;; JDK 9+: findBootstrapClassOrNull — called via JavaLangAccess interface.
;; Delegates to the same logic as findBootstrapClass.
(defmethod |findBootstrapClassOrNull(Ljava/lang/String;)Ljava/lang/Class;| (this name)
  (declare (ignore this))
  (handler-case
      (let ((ldk-class (classload (substitute #\/ #\. (lstring name)))))
        (java-class ldk-class))
    (condition (c)
      (declare (ignore c))
      nil)))

;; JDK 9+: createOrGetClassLoaderValueMap — called via JavaLangAccess interface.
;; Returns (and lazily creates) the ConcurrentHashMap on ClassLoader.classLoaderValueMap.
(defmethod |createOrGetClassLoaderValueMap(Ljava/lang/ClassLoader;)Ljava/util/concurrent/ConcurrentHashMap;| (this classloader)
  (declare (ignore this))
  (when (and classloader (slot-exists-p classloader '|classLoaderValueMap|))
    (let ((existing (slot-value classloader '|classLoaderValueMap|)))
      (when existing
        (return-from |createOrGetClassLoaderValueMap(Ljava/lang/ClassLoader;)Ljava/util/concurrent/ConcurrentHashMap;| existing))))
  ;; Create a new ConcurrentHashMap and store it on the classloader
  (classload "java/util/concurrent/ConcurrentHashMap")
  (let ((map (%make-java-instance "java/util/concurrent/ConcurrentHashMap")))
    (when (and classloader (slot-exists-p classloader '|classLoaderValueMap|))
      (setf (slot-value classloader '|classLoaderValueMap|) map))
    map))

;; Fallback for any object (including nil) — unnamed modules return false
(defmethod |isNamed()Z| (module)
  (declare (ignore module))
  0)

(defmethod |isPrimitive()Z| ((class |java/lang/Class|))
  (let ((name-string (lstring (slot-value class '|name|))))
    (if (null (find name-string '("boolean"
                                  "char"
                                  "byte"
                                  "short"
                                  "int"
                                  "long"
                                  "float"
                                  "double"
                                  "void")
                    :test #'equal))
        0
        1)))

(defun get-ldk-class-for-java-class (java-class)
  "Get the <class> object for a java.lang.Class, using the correct loader."
  (let* ((fq-name (lstring (slot-value java-class '|name|)))
         (java-loader (slot-value java-class '|classLoader|))
         (ldk-loader (get-ldk-loader-for-java-loader java-loader)))
    (%get-ldk-class-by-fq-name fq-name t ldk-loader)))

(defun %java-class-package (java-class)
  "The Lisp package of the class JAVA-CLASS (a java.lang.Class) stands for,
found through its class loader: looking the class up by name alone can find
a same-named class from another loader."
  (let ((ldk-class (get-ldk-class-for-java-class java-class)))
    (if ldk-class
        (ldk-class-package ldk-class)
        (class-package (substitute #\/ #\. (lstring (slot-value java-class '|name|)))))))

(defun %class-interface-p (this)
  (if (and (eq 0 (|isPrimitive()Z| this))
           (eq 0 (|isArray()Z| this))
           (let ((lclass (get-ldk-class-for-java-class this)))
             (and lclass (interface-p lclass))))
      1
      0))

(defmethod |isInterface()Z| ((this |java/lang/Class|))
  (%class-interface-p this))

;; Like getModifiers(), JDK 25's Class.isInterface() reads the `modifiers` field
;; directly, which is 0 for the pre-baked bootstrap classes (e.g. Comparable) --
;; so the primary defmethod is clobbered by the JIT-compiled field read and
;; reports interfaces as non-interfaces (breaking Clojure deftype: "only
;; interfaces are supported, had: java.lang.Comparable"). An :around survives.
(defmethod |isInterface()Z| :around ((this |java/lang/Class|))
  (%class-interface-p this))

(defclass/std <constant-pool> (|java/lang/Object|)
  ((ldk-class)))

(defmethod |getConstantPool()Ljdk/internal/reflect/ConstantPool;| ((this |java/lang/Class|))
  (let ((cp-class-bin "jdk/internal/reflect/ConstantPool")
        (cp-class-fq "jdk.internal.reflect.ConstantPool"))
    (unless (%get-ldk-class-by-fq-name cp-class-fq t)
      (%clinit (classload cp-class-bin)))
    (let ((ldk-class (get-ldk-class-for-java-class this)))
      (when ldk-class
        (let ((cp (%make-java-instance cp-class-bin)))
          (setf (slot-value cp '|constantPoolOop|)
                (make-instance '<constant-pool> :ldk-class ldk-class))
          cp)))))

(defmethod |getDeclaredConstructors0(Z)[Ljava/lang/reflect/Constructor;| ((this |java/lang/Class|) public-only)
  (unwind-protect
       (progn
         (when *debug-trace*
           (format t "~&~V@A trace: entering java/lang/Class.getDeclaredConstructors0(Z)~%" (incf *call-nesting-level* 1) "*"))
         (unless (%get-ldk-class-by-bin-name "java/lang/reflect/Constructor")
           (|java/lang/Class.forName0(Ljava/lang/String;ZLjava/lang/ClassLoader;Ljava/lang/Class;)Ljava/lang/Class;| (jstring "java/lang/reflect/Constructor") nil nil nil))

         ;; Get the lclass for THIS (use correct loader)
         (let ((lclass (get-ldk-class-for-java-class this))
               (*descriptor-loader* (%class-java-loader this)))
           (make-java-array
            :component-class (%get-java-class-by-fq-name "java.lang.reflect.Constructor")
            :initial-contents
            (coerce (append (when lclass
                              (loop for method across (or (methods lclass) #())
                                    when (and (starts-with? "<init>" (name method))
                                             (or (zerop public-only)
                                                 (not (zerop (logand #x1 (access-flags method))))))
                                    #|
                                    Class<?>[] parameterTypes,
                                    Class<?>[] checkedExceptions,
                                    int modifiers,
                                    int slot,
                                    String signature,
                                    byte[] annotations,
                                    byte[] parameterAnnotations
                                    |#
                                    collect (let ((c (%make-java-instance "java/lang/reflect/Constructor"))
                                                  (pt (%get-parameter-types (descriptor method))))
                                              (|<init>(Ljava/lang/Class;[Ljava/lang/Class;[Ljava/lang/Class;IILjava/lang/String;[B[B)V|
                                               c this
                                               (make-java-array :component-class (%get-java-class-by-fq-name "java.lang.Class")
                                                                :initial-contents pt)
                                               (%checked-exception-classes method lclass)
                                               (access-flags method) 0
                                               nil  ; generic signature (not yet supported)
                                               (gethash "RuntimeVisibleAnnotations" (attributes method))
                                               (or (gethash "RuntimeVisibleParameterAnnotations" (attributes method))
                                                   (make-java-array
                                                    :component-class (%get-java-class-by-fq-name "byte")
                                                    :initial-contents (cons (length pt) (make-list (* 2 (length pt)) :initial-element 0)))))
                                              c))))
                    'vector))))
    (when *debug-trace*
      (incf *call-nesting-level* -1))))

(defmethod |getDeclaredClasses0()[Ljava/lang/Class;| ((this |java/lang/Class|))
  "The member classes THIS declares: the InnerClasses entries whose outer
class is THIS, loaded if need be."
  (let* ((lclass (and (eq 0 (|isArray()Z| this))
                      (eq 0 (|isPrimitive()Z| this))
                      (get-ldk-class-for-java-class this)))
         (cp (and lclass (constant-pool lclass)))
         (java-classes
           (when lclass
             (flet ((class-name-at (index)
                      (slot-value (aref cp (index (aref cp index))) 'value)))
               (loop for ic in (gethash "InnerClasses" (attributes lclass))
                     for outer = (outer-class-info-index ic)
                     when (and (not (zerop outer))
                               (string= (class-name-at outer) (name lclass)))
                       append (let ((c (%java-class-loading
                                        (class-name-at (inner-class-info-index ic)) lclass)))
                                (and c (list c))))))))
    (make-java-array
     :component-class (%get-java-class-by-fq-name "java.lang.Class")
     :initial-contents (coerce java-classes 'vector))))

(defun %checked-exception-classes (method lclass)
  "Class[] of METHOD's declared checked exceptions, from its Exceptions
attribute (empty when absent).  Feeds the checkedExceptions argument of
Method/Constructor so reflection reports them (e.g. Method.toString()'s
\"throws\" clause)."
  (let ((cp (constant-pool lclass)))
    (make-java-array
     :component-class (%get-java-class-by-fq-name "java.lang.Class")
     :initial-contents
     (loop for idx in (gethash "Exceptions" (attributes method))
           for class-ref = (aref cp idx)
           for cname = (slot-value (aref cp (index class-ref)) 'value)
           ;; Load the exception type on demand; skip if it can't be resolved
           ;; rather than erroring out of the whole reflective lookup.
           for cls = (progn (ignore-errors (classload cname))
                            (%get-java-class-by-bin-name cname t))
           when cls collect cls))))

(defmethod |getDeclaredMethods0(Z)[Ljava/lang/reflect/Method;| ((this |java/lang/Class|) public-only)
  (unwind-protect
       (progn
         (when *debug-trace*
           (format t "~&~V@A trace: entering java/lang/Class.getDeclaredMethods(Z)~%" (incf *call-nesting-level* 1) "*"))
         (unless (gethash "java/lang/reflect/Method" *ldk-classes-by-bin-name*)
           (|java/lang/Class.forName0(Ljava/lang/String;ZLjava/lang/ClassLoader;Ljava/lang/Class;)Ljava/lang/Class;| (jstring "java/lang/reflect/Method") nil nil nil))

         ;; Get the lclass for THIS (use correct loader)
         (let ((lclass (get-ldk-class-for-java-class this))
               (*descriptor-loader* (%class-java-loader this)))
           (make-java-array
            :component-class (%get-java-class-by-fq-name "java.lang.reflect.Method")
            :initial-contents (coerce (append (when lclass
                                                (loop for method across (or (methods lclass) #())
                                                      when (and (not (starts-with? "<init>" (name method)))
                                                                (or (zerop public-only)
                                                                    (not (zerop (logand #x1 (access-flags method))))))
                                                      #|
                                                      Method(Class<?> declaringClass,
                                                      String name,
                                                      Class<?>[] parameterTypes,
                                                      Class<?> returnType,
                                                      Class<?>[] checkedExceptions,
                                                      int modifiers,
                                                      int slot,
                                                      String signature,
                                                      byte[] annotations,
                                                      byte[] parameterAnnotations,
                                                      byte[] annotationDefault)
                                                      |#

                                                      collect
                                                      (let ((c (make-instance
                                                                '|java/lang/reflect/Method|))
                                                            (pt (%get-parameter-types
                                                                 (descriptor method))))
                                                        (let ((init-fn
                                                               (function
                                                                |<init>(Ljava/lang/Class;Ljava/lang/String;[Ljava/lang/Class;Ljava/lang/Class;[Ljava/lang/Class;IILjava/lang/String;[B[B[B)V|))) ; lint:suppress
                                                          (funcall init-fn c this
                                                                   (ijstring (name method))
                                                                   (make-java-array
                                                                    :component-class
                                                                    (%get-java-class-by-fq-name "java.lang.Class")
                                                                    :initial-contents pt)
                                                                   (%get-return-type (descriptor method))
                                                                   (%checked-exception-classes method lclass)
                                                                   (access-flags method) 0
                                                                   nil  ; generic signature (not yet supported)
                                                                   (gethash "RuntimeVisibleAnnotations"
                                                                            (attributes method))
                                                                   (or (gethash "RuntimeVisibleParameterAnnotations"
                                                                                (attributes method))
                                                                       (make-java-array
                                                                        :component-class
                                                                        (%get-java-class-by-fq-name "byte")
                                                                        :initial-contents
                                                                        (cons (length pt)
                                                                              (make-list (* 2 (length pt))
                                                                                         :initial-element 0))))
                                                                   (gethash "AnnotationDefault"
                                                                            (attributes method)))
                                                          c)))))
                                      'vector))))
         (when *debug-trace*
           (incf *call-nesting-level* -1))))


