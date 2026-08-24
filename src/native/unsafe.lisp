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

(defmethod |arrayBaseOffset0(Ljava/lang/Class;)| ((unsafe |jdk/internal/misc/Unsafe|) array)
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

(defun %unsafe-array-element-scale (array)
  "Return the storage width of ARRAY's component type in bytes."
  (let ((name (%unsafe-array-component-name array)))
    (cond
      ((member name '("byte" "boolean") :test #'string=) 1)
      ((member name '("char" "short") :test #'string=) 2)
      ((member name '("float" "int") :test #'string=) 4)
      ((member name '("double" "long") :test #'string=) 8)
      (t 8))))

(defun %unsafe-array-component-name (array)
  "Return ARRAY's component class name as a Lisp string."
  (lstring (slot-value (%array-component-class array) '|name|)))

(defun %unsafe-array-value-bits (array index)
  "Return ARRAY element INDEX as its unsigned in-memory bit pattern."
  (let ((name (%unsafe-array-component-name array))
        (value (jaref array index)))
    (cond
      ((string= name "float")
       (float-features:single-float-bits (coerce value 'single-float)))
      ((string= name "double")
       (float-features:double-float-bits (coerce value 'double-float)))
      ((string= name "char")
       (if (characterp value) (char-code value) value))
      (t (logand value
                 (1- (ash 1 (* 8 (%unsafe-array-element-scale array)))))))))

(defun %unsafe-array-read-byte (array byte-offset)
  "Read one byte from ARRAY's primitive in-memory representation."
  (let* ((scale (%unsafe-array-element-scale array))
         (index (truncate byte-offset scale))
         (shift (* 8 (mod byte-offset scale))))
    (ldb (byte 8 shift) (%unsafe-array-value-bits array index))))

(defun %unsafe-array-write-byte (array byte-offset byte)
  "Write BYTE into ARRAY's primitive in-memory representation."
  (let* ((name (%unsafe-array-component-name array))
         (scale (%unsafe-array-element-scale array))
         (index (truncate byte-offset scale))
         (shift (* 8 (mod byte-offset scale)))
         (bit-count (* 8 scale))
         (value-mask (1- (ash 1 bit-count)))
         (byte-mask (ash #xff shift))
         (old-bits (%unsafe-array-value-bits array index))
         (new-bits (logand value-mask
                           (logior (logand old-bits (lognot byte-mask))
                                   (ash (logand byte #xff) shift))))
         (new-value
           (cond
             ((string= name "float")
              (float-features:bits-single-float new-bits))
             ((string= name "double")
              (float-features:bits-double-float new-bits))
             ((string= name "byte") (%unsigned-to-signed-byte new-bits))
             ((string= name "short") (unsigned-to-signed-short new-bits))
             ((string= name "int") (unsigned-to-signed-integer new-bits))
             ((string= name "long") (unsigned-to-signed-long new-bits))
             ((string= name "boolean") (if (zerop new-bits) 0 1))
             (t new-bits))))
    (setf (jaref array index) new-value)))

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
      (let ((data (java-array-data obj))
            (v 0))
        (dotimes (i nbytes)
          (setf v (logior v (ash (logand (aref data (+ offset i)) #xff) (* 8 i)))))
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
         (pkg (class-package bin-name))
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
  (loop for index below byte-count
        sum (ash (%unsafe-array-read-byte array (+ offset index))
                 (* index 8))))

(defun %unsafe-write-array-bits (array offset byte-count bits)
  (dotimes (index byte-count)
    (%unsafe-array-write-byte array (+ offset index)
                              (ldb (byte 8 (* index 8)) bits)))
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

(defmethod |getReference(Ljava/lang/Object;J)|
    ((unsafe |jdk/internal/misc/Unsafe|) object offset)
  (declare (ignore unsafe))
  (%unsafe-get-reference object offset))

(defmethod |putReference(Ljava/lang/Object;JLjava/lang/Object;)|
    ((unsafe |jdk/internal/misc/Unsafe|) object offset value)
  (declare (ignore unsafe))
  (%unsafe-put-reference object offset value))

(defmethod |getReferenceVolatile(Ljava/lang/Object;J)|
    ((unsafe |jdk/internal/misc/Unsafe|) object offset)
  (declare (ignore unsafe))
  (%unsafe-get-reference object offset))

(defmethod |putReferenceVolatile(Ljava/lang/Object;JLjava/lang/Object;)|
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

(defmethod |compareAndSetInt(Ljava/lang/Object;JII)|
    ((unsafe |jdk/internal/misc/Unsafe|) object offset expected replacement)
  (declare (ignore unsafe))
  (if (eql expected (%unsafe-compare-and-exchange-scalar
                     object offset expected replacement :int))
      1 0))

(defmethod |compareAndExchangeInt(Ljava/lang/Object;JII)|
    ((unsafe |jdk/internal/misc/Unsafe|) object offset expected replacement)
  (declare (ignore unsafe))
  (%unsafe-compare-and-exchange-scalar
   object offset expected replacement :int))

(defmethod |compareAndSetLong(Ljava/lang/Object;JJJ)|
    ((unsafe |jdk/internal/misc/Unsafe|) object offset expected replacement)
  (declare (ignore unsafe))
  (if (eql expected (%unsafe-compare-and-exchange-scalar
                     object offset expected replacement :long))
      1 0))

(defmethod |compareAndExchangeLong(Ljava/lang/Object;JJJ)|
    ((unsafe |jdk/internal/misc/Unsafe|) object offset expected replacement)
  (declare (ignore unsafe))
  (%unsafe-compare-and-exchange-scalar
   object offset expected replacement :long))

(defmethod |compareAndSetReference(Ljava/lang/Object;JLjava/lang/Object;Ljava/lang/Object;)|
    ((unsafe |jdk/internal/misc/Unsafe|) object offset expected replacement)
  (declare (ignore unsafe))
  (if (eq expected (%unsafe-compare-and-exchange-reference
                    object offset expected replacement))
      1 0))

(defmethod |compareAndExchangeReference(Ljava/lang/Object;JLjava/lang/Object;Ljava/lang/Object;)|
    ((unsafe |jdk/internal/misc/Unsafe|) object offset expected replacement)
  (declare (ignore unsafe))
  (%unsafe-compare-and-exchange-reference
   object offset expected replacement))

(defmethod |fullFence()| ((unsafe |jdk/internal/misc/Unsafe|))
  (declare (ignore unsafe))
  nil)

(defmethod |arrayIndexScale0(Ljava/lang/Class;)| ((unsafe |jdk/internal/misc/Unsafe|) array)
  (declare (ignore unsafe))
  (%unsafe-array-index-scale array))

(defmethod |addressSize0()| ((unsafe |jdk/internal/misc/Unsafe|))
  8)

(defclass %synthetic-field ()
  ((|name| :initarg :name :accessor %sf-name)
   (|clazz| :initarg :clazz :accessor %sf-clazz))
  (:documentation "Lightweight stand-in for java/lang/reflect/Field used by objectFieldOffset1."))

(defmethod |objectFieldOffset1(Ljava/lang/Class;Ljava/lang/String;)| ((unsafe |jdk/internal/misc/Unsafe|) clazz field-name)
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
    ;; Copy bytes to native memory
    (loop for i below len
          do (setf (sb-sys:sap-ref-8 sap i) (aref data i)))
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
              (handler-case
                  (sb-alien:free-alien (car entry))
                (error () nil))  ; stale pointer from image save — ignore
              (remhash address *unsafe-memory-table*))
            (setf (slot-value this '|address|) 0)))))

(defmethod |allocateMemory0(J)| ((unsafe |jdk/internal/misc/Unsafe|) size)
  (let* ((mem (sb-alien:make-alien sb-alien:char size))
         (ptr (sb-sys:sap-int (sb-alien:alien-sap mem))))
    (setf (gethash ptr *unsafe-memory-table*) (cons mem size))
    ptr))

(defmethod |freeMemory0(J)| ((unsafe |jdk/internal/misc/Unsafe|) address)
  (when-let (entry (gethash address *unsafe-memory-table*))
    (sb-alien:free-alien (car entry))
    (remhash address *unsafe-memory-table*)))

(defmethod |putLong0(JJ)| ((unsafe |jdk/internal/misc/Unsafe|) address value)
  (setf (sb-sys:signed-sap-ref-64 (sb-sys:int-sap address) 0) value))

(defmethod |getByte0(J)| ((unsafe |jdk/internal/misc/Unsafe|) address)
  (sb-sys:sap-ref-8 (sb-sys:int-sap address) 0))

(defmethod |putByte0(JB)| ((unsafe |jdk/internal/misc/Unsafe|) address value)
  (setf (sb-sys:sap-ref-8 (sb-sys:int-sap address) 0) (logand value #xFF)))

(defmethod |copyMemory0(Ljava/lang/Object;JLjava/lang/Object;JJ)| ((unsafe |jdk/internal/misc/Unsafe|) source source-offset dest dest-offset length)
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

(defmethod |reallocateMemory0(JJ)| ((unsafe |jdk/internal/misc/Unsafe|) address new-size)
  (if (zerop address)
      ;; Zero address means fresh allocation (realloc(NULL, size) == malloc(size))
      (|allocateMemory0(J)| unsafe new-size)
      ;; Allocate new block, copy old data, free old block
      (let* ((new-mem (sb-alien:make-alien sb-alien:char new-size))
             (new-ptr (sb-sys:sap-int (sb-alien:alien-sap new-mem)))
             (new-sap (sb-alien:alien-sap new-mem))
             (old-entry (gethash address *unsafe-memory-table*)))
        (when old-entry
          (let* ((old-alien (car old-entry))
                 (old-size (cdr old-entry))
                 (old-sap (sb-alien:alien-sap old-alien))
                 (copy-size (min old-size new-size)))
            (loop for i below copy-size
                  do (setf (sb-sys:sap-ref-8 new-sap i)
                           (sb-sys:sap-ref-8 old-sap i)))
            (sb-alien:free-alien old-alien)
            (remhash address *unsafe-memory-table*)))
        (setf (gethash new-ptr *unsafe-memory-table*) (cons new-mem new-size))
        new-ptr)))

(defmethod |setMemory0(Ljava/lang/Object;JJB)| ((unsafe |jdk/internal/misc/Unsafe|) obj offset bytes value)
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

(defmethod |park(ZJ)| ((unsafe |jdk/internal/misc/Unsafe|) is-absolute time)
  "Park the current thread. If time > 0, park with timeout."
  (declare (ignore unsafe is-absolute))
  #+sb-fiber
  (when (in-fiber-p)
    (let ((fiber (sb-thread:current-fiber)))
      ;; Check and consume permit
      (when (gethash fiber *fiber-park-flags*)
        (remhash fiber *fiber-park-flags*)
        (return-from |park(ZJ)|))
      ;; Park with predicate: wake when permit is set
      (if (> time 0)
          (sb-thread:fiber-park (lambda () (gethash fiber *fiber-park-flags*))
                                :timeout (/ time 1000000000.0d0))
          (sb-thread:fiber-park (lambda () (gethash fiber *fiber-park-flags*))))
      ;; Consume permit after waking
      (remhash fiber *fiber-park-flags*))
    (return-from |park(ZJ)|))
  ;; Platform thread fallback
  (when (> time 0)
    (sleep (/ time 1000000000.0d0))))

(defmethod |unpark(Ljava/lang/Object;)| ((unsafe |jdk/internal/misc/Unsafe|) thread)
  "Unpark a thread by granting a permit."
  (declare (ignore unsafe))
  #+sb-fiber
  (let ((fiber (gethash thread *java-to-fibers*)))
    (when fiber
      ;; Set the permit flag — the fiber's park predicate will see this
      (setf (gethash fiber *fiber-park-flags*) t)
      (return-from |unpark(Ljava/lang/Object;)|)))
  ;; Platform thread fallback — no-op (Lisp threads don't support park/unpark natively)
  thread
  nil)

;;; JDK 9+ renamed Unsafe CAS and accessor methods.
;;; These delegate to the existing sun/misc/Unsafe implementations via inheritance.

(defmethod |compareAndSetInt(Ljava/lang/Object;JII)| ((unsafe |sun/misc/Unsafe|) obj field-id expected-value new-value)
  (|compareAndSwapInt(Ljava/lang/Object;JII)| unsafe obj field-id expected-value new-value))

(defmethod |compareAndSetLong(Ljava/lang/Object;JJJ)| ((unsafe |sun/misc/Unsafe|) obj field-id expected-value new-value)
  (|compareAndSwapLong(Ljava/lang/Object;JJJ)| unsafe obj field-id expected-value new-value))

(defmethod |compareAndSetReference(Ljava/lang/Object;JLjava/lang/Object;Ljava/lang/Object;)| ((unsafe |sun/misc/Unsafe|) obj field-id expected-value new-value)
  (|compareAndSwapObject(Ljava/lang/Object;JLjava/lang/Object;Ljava/lang/Object;)| unsafe obj field-id expected-value new-value))

(defmethod |compareAndSetObject(Ljava/lang/Object;JLjava/lang/Object;Ljava/lang/Object;)| ((unsafe |sun/misc/Unsafe|) obj field-id expected-value new-value)
  (|compareAndSwapObject(Ljava/lang/Object;JLjava/lang/Object;Ljava/lang/Object;)| unsafe obj field-id expected-value new-value))

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

(defmethod |compareAndSetByte(Ljava/lang/Object;JBB)| ((unsafe |sun/misc/Unsafe|) obj offset expected new-val)
  (declare (ignore unsafe))
  (%cas-byte-field obj offset expected new-val))

(defmethod |compareAndExchangeByte(Ljava/lang/Object;JBB)| ((unsafe |sun/misc/Unsafe|) obj offset expected new-val)
  (declare (ignore unsafe))
  (%cae-byte-field obj offset expected new-val))

(defmethod |compareAndSetBoolean(Ljava/lang/Object;JZZ)| ((unsafe |sun/misc/Unsafe|) obj offset expected new-val)
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
(defun |jdk/internal/misc/PreviewFeatures.isPreviewEnabled()| ()
  0)

;;; ProcessImpl.init — static native initializer for process spawning.
;;; On a real JVM this sets up signal handling for child processes.
;;; We stub it as a no-op; forkAndExec is handled separately.
(defun |java/lang/ProcessImpl.init()| ()
  nil)

;;; NOTE: In JDK 17, the compiled bytecode methods getObjectVolatile/putObject/etc.
;;; delegate to getReference/putReference/etc. (native methods).
;;; So the native "Reference" variants must contain the actual implementation,
;;; NOT delegate back to the "Object" variants (which would cause infinite recursion).
;;; The implementations are defined later, after the Object variants.

(defmethod |hashCode()| (obj)
  (|java/lang/System.identityHashCode(Ljava/lang/Object;)| obj))

(defclass %array-base-offset ()
  ((array :initarg :array)))

(defmethod |arrayBaseOffset(Ljava/lang/Class;)| ((unsafe |sun/misc/Unsafe|) array)
  0)
;  (make-instance '%array-base-offset :array array))

(defmethod |arrayIndexScale(Ljava/lang/Class;)| ((unsafe |sun/misc/Unsafe|) array)
  (declare (ignore unsafe))
  (%unsafe-array-index-scale array))

(defmethod |addressSize()| ((unsafe |sun/misc/Unsafe|))
  8)

(defmethod |isArray()| ((class |java/lang/Class|))
  (let ((name-string (lstring (slot-value class '|name|))))
    (if (eq #\[ (char name-string 0))
        1
        0)))

(defmethod |getComponentType()| ((class |java/lang/Class|))
  (let ((cn (lstring (slot-value class '|name|))))
    (if (eq #\[ (char cn 0))
        (let ((ct (%bin-type-name-to-class (subseq cn 1))))
          (unless ct
            (error (format nil "ERROR: can't determine component type for ~A" cn)))
          ct)
        nil)))

;; JDK 15+: Class.isHidden() — no classes in OpenLDK are hidden
(defmethod |isHidden()| ((class |java/lang/Class|))
  0)

;; JDK 21: Class.getClassAccessFlagsRaw0() — return raw access flags
(defmethod |getClassAccessFlagsRaw0()| ((this |java/lang/Class|))
  (let ((ldk-class (get-ldk-class-for-java-class this)))
    (if ldk-class
        (slot-value ldk-class 'access-flags)
        0)))

;; JDK 21: Class.getClassFileVersion0() — return class file major version
(defmethod |getClassFileVersion0()| ((this |java/lang/Class|))
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
(defmethod |getModule()| :around ((class |java/lang/Class|))
  (let ((result (call-next-method)))
    (or result (%get-unnamed-module))))

;; JDK 9+: JavaLangAccess.defineUnnamedModule — return a fresh unnamed Module
(defmethod |defineUnnamedModule(Ljava/lang/ClassLoader;)| (this classloader)
  (declare (ignore this classloader))
  (%get-unnamed-module))

;; JDK 9+: JavaLangAccess.addEnableNativeAccess — identity stub
(defmethod |addEnableNativeAccess(Ljava/lang/Module;)| (this module)
  (declare (ignore this))
  module)

;; JDK 9+: BootLoader native — associate unnamed module with boot loader (no-op)
(defun |jdk/internal/loader/BootLoader.setBootLoaderUnnamedModule0(Ljava/lang/Module;)| (module)
  (declare (ignore module))
  nil)

;; Return our boot-class-loader for getSystemClassLoader() so it's never nil
(defun |java/lang/ClassLoader.getSystemClassLoader()| ()
  *boot-class-loader*)
(setf (gethash "java/lang/ClassLoader.getSystemClassLoader()Ljava/lang/ClassLoader;" *native-overrides*)
      #'|java/lang/ClassLoader.getSystemClassLoader()|)

;; JDK 17: registerAsParallelCapable always succeeds (avoids InternalError in <clinit>)
(defun |java/lang/ClassLoader.registerAsParallelCapable()| ()
  1)
(setf (gethash "java/lang/ClassLoader.registerAsParallelCapable()Z" *native-overrides*)
      #'|java/lang/ClassLoader.registerAsParallelCapable()|)

;; JDK 16+: Reference.refersTo0 — native method for weak reference checking
;; Used by ThreadLocalMap.getEntry() to match WeakReference keys.
(defmethod |refersTo0(Ljava/lang/Object;)| ((this |java/lang/ref/Reference|) obj)
  (if (eq (slot-value this '|referent|) obj) 1 0))

;; JDK 9+: Reflection.getClassAccessFlags — return class modifiers
(defun |jdk/internal/reflect/Reflection.getClassAccessFlags(Ljava/lang/Class;)| (java-class)
  (let ((ldk-class (get-ldk-class-for-java-class java-class)))
    (if (and ldk-class (slot-boundp ldk-class 'access-flags))
        (access-flags ldk-class)
        ;; Default: public (0x0001)
        1)))

;; JDK 9+: JavaLangReflectAccess.getExecutableSharedParameterTypes — fallback
;; for when langReflectAccess on ReflectionFactory is nil (AccessibleObject
;; <clinit> hasn't run yet).  Delegates to the Executable's parameterTypes field.
(defmethod |getExecutableSharedParameterTypes(Ljava/lang/reflect/Executable;)| (this exec)
  (declare (ignore this))
  (when (and exec (slot-exists-p exec '|parameterTypes|))
    (slot-value exec '|parameterTypes|)))

;; JDK 9+: findBootstrapClassOrNull — called via JavaLangAccess interface.
;; Delegates to the same logic as findBootstrapClass.
(defmethod |findBootstrapClassOrNull(Ljava/lang/String;)| (this name)
  (declare (ignore this))
  (handler-case
      (let ((ldk-class (classload (substitute #\/ #\. (lstring name)))))
        (java-class ldk-class))
    (condition (c)
      (declare (ignore c))
      nil)))

;; JDK 9+: createOrGetClassLoaderValueMap — called via JavaLangAccess interface.
;; Returns (and lazily creates) the ConcurrentHashMap on ClassLoader.classLoaderValueMap.
(defmethod |createOrGetClassLoaderValueMap(Ljava/lang/ClassLoader;)| (this classloader)
  (declare (ignore this))
  (when (and classloader (slot-exists-p classloader '|classLoaderValueMap|))
    (let ((existing (slot-value classloader '|classLoaderValueMap|)))
      (when existing
        (return-from |createOrGetClassLoaderValueMap(Ljava/lang/ClassLoader;)| existing))))
  ;; Create a new ConcurrentHashMap and store it on the classloader
  (classload "java/util/concurrent/ConcurrentHashMap")
  (let ((map (%make-java-instance "java/util/concurrent/ConcurrentHashMap")))
    (when (and classloader (slot-exists-p classloader '|classLoaderValueMap|))
      (setf (slot-value classloader '|classLoaderValueMap|) map))
    map))

;; Fallback for any object (including nil) — unnamed modules return false
(defmethod |isNamed()| (module)
  (declare (ignore module))
  0)

(defmethod |isPrimitive()| ((class |java/lang/Class|))
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

(defmethod |isInterface()| ((this |java/lang/Class|))
  (if (and (eq 0 (|isPrimitive()| this))
           (let ((lclass (get-ldk-class-for-java-class this)))
             (and lclass (interface-p lclass))))
      1
      0))

(defclass/std <constant-pool> (|java/lang/Object|)
  ((ldk-class)))

(defmethod |getConstantPool()| ((this |java/lang/Class|))
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

(defmethod |getDeclaredConstructors0(Z)| ((this |java/lang/Class|) public-only)
  (unwind-protect
       (progn
         (when *debug-trace*
           (format t "~&~V@A trace: entering java/lang/Class.getDeclaredConstructors0(Z)~%" (incf *call-nesting-level* 1) "*"))
         (unless (%get-ldk-class-by-bin-name "java/lang/reflect/Constructor")
           (|java/lang/Class.forName0(Ljava/lang/String;ZLjava/lang/ClassLoader;Ljava/lang/Class;)| (jstring "java/lang/reflect/Constructor") nil nil nil))

         ;; Get the lclass for THIS (use correct loader)
         (let ((lclass (get-ldk-class-for-java-class this)))
           (make-java-array
            :component-class (%get-java-class-by-fq-name "java.lang.reflect.Constructor")
            :initial-contents
            (coerce (append (when lclass
                              (loop for method across (methods lclass)
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
                                              (|<init>(Ljava/lang/Class;[Ljava/lang/Class;[Ljava/lang/Class;IILjava/lang/String;[B[B)|
                                               c this
                                               (make-java-array :component-class (%get-java-class-by-fq-name "java.lang.Class")
                                                                :initial-contents pt)
                                               (make-java-array
                                                :component-class (%get-java-class-by-fq-name "java.lang.Class")
                                                :size 0)
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

(defmethod |getDeclaredClasses0()| ((this |java/lang/Class|))
  (let ((lclass (get-ldk-class-for-java-class this)))
    ;; FIXME: need to get all inner classes
    (let ((java-classes
            (when lclass
              (loop for iclass in (inner-classes lclass)
                    for c = (%get-java-class-by-bin-name (value iclass) t)
                    when c
                      collect c))))
      (make-java-array
       :component-class (%get-java-class-by-fq-name "java.lang.Class")
       :initial-contents (coerce java-classes 'vector)))))

(defmethod |getDeclaredMethods0(Z)| ((this |java/lang/Class|) public-only)
  (unwind-protect
       (progn
         (when *debug-trace*
           (format t "~&~V@A trace: entering java/lang/Class.getDeclaredMethods(Z)~%" (incf *call-nesting-level* 1) "*"))
         (unless (gethash "java/lang/reflect/Method" *ldk-classes-by-bin-name*)
           (|java/lang/Class.forName0(Ljava/lang/String;ZLjava/lang/ClassLoader;Ljava/lang/Class;)| (jstring "java/lang/reflect/Method") nil nil nil))

         ;; Get the lclass for THIS (use correct loader)
         (let ((lclass (get-ldk-class-for-java-class this)))
           (make-java-array
            :component-class (%get-java-class-by-fq-name "java.lang.reflect.Method")
            :initial-contents (coerce (append (when lclass
                                                (loop for method across (methods lclass)
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
                                                                |<init>(Ljava/lang/Class;Ljava/lang/String;[Ljava/lang/Class;Ljava/lang/Class;[Ljava/lang/Class;IILjava/lang/String;[B[B[B)|))) ; lint:suppress
                                                          (funcall init-fn c this
                                                                   (ijstring (name method))
                                                                   (make-java-array
                                                                    :component-class
                                                                    (%get-java-class-by-fq-name "java.lang.Class")
                                                                    :initial-contents pt)
                                                                   (%get-return-type (descriptor method))
                                                                   (make-java-array
                                                                    :component-class
                                                                    (%get-java-class-by-fq-name "java.lang.Class")
                                                                    :size 0)
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


