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

;;; ------------------------------------------------------------------
;;; VarHandle signature-polymorphic op dispatch (ldk-304)
;;;
;;; The bytecode compiler (src/bc-to-ir.lisp %transpile-virtual-call) routes
;;; every invokevirtual on java/lang/invoke/VarHandle.<op> to a call
;;;   (%VARHANDLE-OP-<op> <receiver-varhandle> . <coordinates-and-values>)
;;; VarHandle access methods are signature-polymorphic: the call-site descriptor
;;; carries the erased coordinate/value types, so there is no single declared
;;; method to dispatch on (unlike, say, AtomicReference.compareAndSet, which is a
;;; real method overridden elsewhere).  OpenLDK does not interpret VarForm/
;;; LambdaForm, so we implement the access modes directly against the handle.
;;;
;;; Handle kinds are distinguished by runtime class name:
;;;   *$FieldInstanceReadWrite/ReadOnly  -> instance field: the first coordinate
;;;       is the receiver object; operate on its CLOS slot (fieldOffset ->
;;;       %unsafe-slot-key).
;;;   *$FieldStaticReadWrite/ReadOnly    -> static field: no receiver coordinate;
;;;       operate on the class's +static-...+ storage.
;;;   LazyInitializingVarHandle          -> wraps a real handle in its `target'.
;;;   VarHandle*$Array                   -> array-element view (array,index,...).
;;;   VarHandleByteArrayAs*$ArrayHandle  -> byte-array view (array,index,...).
;;; Read-modify-write ops serialize on *cas-lock*; equality is EQL, which is
;;; JVM == for references (object identity) and value equality for the boxed
;;; primitives OpenLDK keeps in slots.  This subsumes the array/byte-view
;;; VarHandle CLOS methods in unsafe.lisp (now reached through here instead).

(defun %vh-classname (vh)
  (symbol-name (class-name (class-of vh))))

(defun %vh-byteview-spec (name)
  "For a VarHandleByteArrayAs*$ArrayHandle class NAME return (values nbytes kind)."
  (cond ((search "AsShorts"  name) (values 2 :signed))
        ((search "AsChars"   name) (values 2 :char))
        ((search "AsInts"    name) (values 4 :signed))
        ((search "AsLongs"   name) (values 8 :signed))
        ((search "AsFloats"  name) (values 4 :float))
        ((search "AsDoubles" name) (values 8 :double))
        (t (values nil nil))))

(defun %vh-field-place (vh)
  "Resolve a field VarHandle VH to (values kind storage slot) where KIND is
:instance (STORAGE nil, caller supplies the receiver) or :static (STORAGE the
+static-...+ object).  Returns NIL unless VH is a field handle."
  (let ((name (%vh-classname vh)))
    (cond
      ((search "LazyInitializing" name)
       (let ((target (and (slot-exists-p vh '|target|)
                          (slot-boundp vh '|target|)
                          (slot-value vh '|target|))))
         (if target
             (%vh-field-place target)
             (internal-error "VarHandle: uninitialized LazyInitializingVarHandle target (static-field VarHandle unsupported) - ldk-304"))))
      ((search "FieldInstance" name)
       (values :instance nil (%unsafe-slot-key (slot-value vh '|fieldOffset|))))
      ((search "FieldStatic" name)
       (let ((offset (slot-value vh '|fieldOffset|)))
         (values :static
                 (%unsafe-static-storage (gethash offset *field-offset-table*))
                 (%unsafe-slot-key offset))))
      (t nil))))

(defun %vh-run-op (category read write values)
  "Apply access-mode CATEGORY over a location abstracted by the thunks READ
(no args -> current value) and WRITE (one arg -> store it).  VALUES holds the
op's value arguments (expected/new/delta as applicable)."
  (declare (type function read write))
  (ecase category
    (:read  (funcall read))
    (:write (funcall write (first values)) nil)
    (:cas   (bordeaux-threads:with-recursive-lock-held (*cas-lock*)
              (if (eql (funcall read) (first values))
                  (progn (funcall write (second values)) 1)
                  0)))
    (:cae   (bordeaux-threads:with-recursive-lock-held (*cas-lock*)
              (let ((old (funcall read)))
                (when (eql old (first values)) (funcall write (second values)))
                old)))
    (:getset (bordeaux-threads:with-recursive-lock-held (*cas-lock*)
               (prog1 (funcall read) (funcall write (first values)))))
    (:getadd (bordeaux-threads:with-recursive-lock-held (*cas-lock*)
               (let ((old (funcall read)))
                 (funcall write (+ old (first values)))
                 old)))
    (:getor  (bordeaux-threads:with-recursive-lock-held (*cas-lock*)
               (let ((old (funcall read)))
                 (funcall write (logior old (first values)))
                 old)))
    (:getand (bordeaux-threads:with-recursive-lock-held (*cas-lock*)
               (let ((old (funcall read)))
                 (funcall write (logand old (first values)))
                 old)))
    (:getxor (bordeaux-threads:with-recursive-lock-held (*cas-lock*)
               (let ((old (funcall read)))
                 (funcall write (logxor old (first values)))
                 old)))))

(defun %vh-array-bits (array)
  "Signed width in bits of ARRAY's primitive element type, or NIL for references."
  (let ((nm (ignore-errors (lstring (slot-value (%array-component-class array) '|name|)))))
    (cond ((equal nm "int") 32) ((equal nm "long") 64)
          ((equal nm "short") 16) ((equal nm "byte") 8)
          (t nil))))

(defun %varhandle-invoke (category vh args)
  (let ((name (%vh-classname vh)))
    (cond
      ;; byte-array view (VarHandleByteArrayAs*$ArrayHandle): check BEFORE the
      ;; array-element view, since its class name also contains "$Array".  Only
      ;; get/set are meaningful, matching the prior byte-view support.
      ((search "ByteArrayAs" name)
       (multiple-value-bind (nbytes kind) (%vh-byteview-spec name)
         (let ((array (first args)) (index (second args)) (values (cddr args)))
           (case category
             (:read  (%byte-view-get vh array index nbytes kind))
             (:write (%byte-view-set vh array index nbytes kind (first values)) nil)
             (otherwise
              (internal-error "VarHandle byte-array view: ~A unsupported (ldk-304)" category))))))
      ;; array-element view (VarHandle*$Array): coordinates are (array index . values)
      ((search "$Array" name)
       (let* ((array (first args)) (index (second args)) (values (cddr args))
              (bits (%vh-array-bits array)))
         (%vh-run-op category
                     (lambda () (jaref array index))
                     (lambda (v) (setf (jaref array index)
                                       (if (and bits (integerp v))
                                           (%signed-of-width (ldb (byte bits 0) v) bits)
                                           v)))
                     values)))
      ;; field handle (instance or static)
      (t
       (multiple-value-bind (fkind storage slot) (%vh-field-place vh)
         (unless slot
           (internal-error "VarHandle: unsupported handle class ~A (ldk-304)" name))
         (multiple-value-bind (obj values)
             (ecase fkind
               (:instance (values (first args) (rest args)))
               (:static   (values storage args)))
           (%vh-run-op category
                       (lambda () (slot-value obj slot))
                       (lambda (v) (setf (slot-value obj slot) v))
                       values)))))))

;; Register %VARHANDLE-OP-<op> for each access mode the compiler may emit.
;; The op name is embedded in the function symbol (bc-to-ir builds it), so no
;; op string needs to be threaded through as an argument.
(dolist (entry '((:read   "get" "getVolatile" "getAcquire" "getOpaque")
                 (:write  "set" "setVolatile" "setRelease" "setOpaque" "setPlain")
                 (:cas    "compareAndSet" "weakCompareAndSet" "weakCompareAndSetPlain"
                          "weakCompareAndSetAcquire" "weakCompareAndSetRelease")
                 (:cae    "compareAndExchange" "compareAndExchangeAcquire"
                          "compareAndExchangeRelease")
                 (:getset "getAndSet" "getAndSetAcquire" "getAndSetRelease")
                 (:getadd "getAndAdd" "getAndAddAcquire" "getAndAddRelease")
                 (:getor  "getAndBitwiseOr" "getAndBitwiseOrAcquire" "getAndBitwiseOrRelease")
                 (:getand "getAndBitwiseAnd" "getAndBitwiseAndAcquire" "getAndBitwiseAndRelease")
                 (:getxor "getAndBitwiseXor" "getAndBitwiseXorAcquire" "getAndBitwiseXorRelease")))
  (destructuring-bind (category &rest ops) entry
    (dolist (op ops)
      (let ((cat category))
        (setf (symbol-function (intern (format nil "%VARHANDLE-OP-~A" op) :openldk))
              (lambda (vh &rest args) (%varhandle-invoke cat vh args)))))))
