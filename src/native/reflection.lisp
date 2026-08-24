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

;;; Reflective invocation, CDS, Reference/Finalizer stubs.

(in-package :openldk)

;;; --- Reflective method/constructor invocation (JDK 9+: jdk/internal/reflect) ---

(defun |jdk/internal/reflect/NativeMethodAccessorImpl.invoke0(Ljava/lang/reflect/Method;Ljava/lang/Object;[Ljava/lang/Object;)|
    (method obj args)
  "Native reflective method invocation for JDK 9+."
  (let* ((method-name (lstring (slot-value method '|name|)))
         (declaring-class (slot-value method '|clazz|))
         (param-types (slot-value method '|parameterTypes|))
         (return-type (slot-value method '|returnType|))
         (modifiers (slot-value method '|modifiers|))
         (is-static (not (zerop (logand modifiers #x0008))))
         (descriptor (%build-method-descriptor return-type param-types))
         (lispized (lispize-method-name (format nil "~A~A" method-name descriptor))))
    (let* ((class-name (substitute #\/ #\. (lstring (slot-value declaring-class '|name|))))
           (fn-name (if is-static
                        (format nil "~A.~A" class-name lispized)
                        lispized))
           (pkg (if is-static
                    (class-package class-name)
                    (find-package :openldk)))
           (sym (find-symbol fn-name pkg)))
      (unless (and sym (fboundp sym))
        (internal-error "Reflective invoke0: method ~A not found (class=~A, static=~A)"
               fn-name class-name is-static))
      (let ((lisp-args (when args
                         (coerce (java-array-data args) 'list))))
        (if is-static
            (apply (symbol-function sym) lisp-args)
            (apply (symbol-function sym) obj lisp-args))))))

;; JDK 21 uses DirectMethodHandleAccessor$NativeAccessor instead of NativeMethodAccessorImpl
(defun |jdk/internal/reflect/DirectMethodHandleAccessor$NativeAccessor.invoke0(Ljava/lang/reflect/Method;Ljava/lang/Object;[Ljava/lang/Object;)|
    (method obj args)
  "Native reflective method invocation for JDK 21."
  (|jdk/internal/reflect/NativeMethodAccessorImpl.invoke0(Ljava/lang/reflect/Method;Ljava/lang/Object;[Ljava/lang/Object;)|
   method obj args))

(defun |jdk/internal/reflect/NativeConstructorAccessorImpl.newInstance0(Ljava/lang/reflect/Constructor;[Ljava/lang/Object;)|
    (constructor args)
  "Native reflective constructor invocation for JDK 9+."
  (let* ((declaring-class (slot-value constructor '|clazz|))
         (param-types (slot-value constructor '|parameterTypes|))
         (class-name (substitute #\/ #\. (lstring (slot-value declaring-class '|name|))))
         (descriptor (format nil "(~{~A~})V"
                             (when param-types
                               (map 'list #'%class->descriptor-string (java-array-data param-types)))))
         (lispized (lispize-method-name (format nil "<init>~A" descriptor)))
         (pkg (find-package :openldk))
         (sym (find-symbol lispized pkg)))
    (classload class-name)
    (let ((instance (%make-java-instance class-name))
          (lisp-args (when args
                       (coerce (java-array-data args) 'list))))
      (if (and sym (fboundp sym))
          (apply (symbol-function sym) instance lisp-args)
          (internal-error "Reflective newInstance0: constructor ~A not found for class ~A"
                 lispized class-name))
      instance)))

;; JDK 21 routes Constructor.newInstance through this accessor class.
(defun |jdk/internal/reflect/DirectConstructorHandleAccessor$NativeAccessor.newInstance0(Ljava/lang/reflect/Constructor;[Ljava/lang/Object;)|
    (constructor args)
  "Invoke a constructor through the JDK 21 direct reflection accessor."
  (|jdk/internal/reflect/NativeConstructorAccessorImpl.newInstance0(Ljava/lang/reflect/Constructor;[Ljava/lang/Object;)|
   constructor args))

;; Guard against null dispatch on gnu.bytecode.Type methods.
;; Kawa's PrimProcedure sometimes has a null retType, causing isVoid() and
;; getRawType() to be called on nil.
(defmethod |isVoid()| ((obj null))
  0)

(defmethod |getRawType()| ((obj null))
  nil)

(defmethod |isCompatibleWithValue(Lgnu/bytecode/Type;)| ((obj null) other)
  -1)

(defmethod |isCompatibleWithValue(Lgnu/bytecode/Type;)| (obj (other null))
  -1)

;; Fix: Type.make(Class) can fail for primitive types when AbstractWeakHashTable
;; lookups fail (e.g. due to hash collisions or GC clearing weak references).
;; Fall back to looking up the well-known primitive Type static fields directly.
(defmethod |getTypeFor(Ljava/lang/Class;)| :around (self jclass)
  (or (handler-case (call-next-method)
        (error () nil))
      (let* ((name (lstring (slot-value jclass '|name|)))
             (pkg (class-package "gnu/bytecode/Type"))
             (static-sym (find-symbol "+static-gnu/bytecode/Type+" pkg))
             (static-holder (when (and static-sym (boundp static-sym))
                              (symbol-value static-sym)))
             (field-name (cond
                           ((string= name "void")    "voidType")
                           ((string= name "int")     "intType")
                           ((string= name "boolean") "booleanType")
                           ((string= name "byte")    "byteType")
                           ((string= name "short")   "shortType")
                           ((string= name "long")    "longType")
                           ((string= name "float")   "floatType")
                           ((string= name "double")  "doubleType")
                           ((string= name "char")    "charType"))))
        (when (and field-name static-holder)
          (let ((field-sym (find-symbol field-name :openldk)))
            (when (and field-sym (slot-exists-p static-holder field-sym))
              (slot-value static-holder field-sym)))))))


;; Generic type methods - return non-generic types until full generics support is implemented.
;; These must be defmethod without class specializer since java/lang/reflect/Method
;; doesn't exist at compile time. The native-override-p check prevents the bytecode
;; versions from being compiled, so these are the only definitions.
(defmethod |getGenericReturnType()| (method)
  "Return the return type. Generic type information is not yet supported."
  (slot-value method '|returnType|))

(defmethod |getGenericParameterTypes()| (method)
  "Return the parameter types. Generic type information is not yet supported."
  (let ((pt (slot-value method '|parameterTypes|)))
    (or pt (make-java-array :component-class (%get-java-class-by-fq-name "java.lang.Class")
                            :size 0))))

(defmethod |getDeclaredFields0(Z)| ((this |java/lang/Class|) public-only)
  (unwind-protect
       (progn
         (when *debug-trace*
           (format t "~&~V@A trace: entering java/lang/Class.getDeclaredFields0(Z)~%" (incf *call-nesting-level* 1) "*"))
         (unless (gethash "java/lang/reflect/Field" *ldk-classes-by-bin-name*)
           (|java/lang/Class.forName0(Ljava/lang/String;ZLjava/lang/ClassLoader;Ljava/lang/Class;)| (jstring "java/lang/reflect/Field") nil nil nil))

         ;; Get the lclass for THIS (use correct loader)
         (let ((lclass (get-ldk-class-for-java-class this)))
           (labels ((get-fields (lclass)
                      (when lclass
                        (append (loop for field across (fields lclass)
                                      ;; When public-only is 1 (true), skip non-public fields
                                      when (or (zerop public-only)
                                               (not (zerop (logand #x1 (access-flags field)))))
                                      collect (let ((f (%make-java-instance "java/lang/reflect/Field")))
                                                ;; JDK 17: Field(Class, String, Class, int, boolean, int, String, byte[])
                                                (|<init>(Ljava/lang/Class;Ljava/lang/String;Ljava/lang/Class;IZILjava/lang/String;[B)|
                                                 f this (ijstring (name field))
                                                 (let ((cn (slot-value field 'descriptor)))
                                                   (if (eq (char cn 0) #\L)
                                                       ;; Object types: strip L prefix and ; suffix, use lazy lookup
                                                       ;; to avoid triggering class loading during image build.
                                                       (let ((cn (subseq cn 1 (1- (length cn)))))
                                                         (or (%get-java-class-by-bin-name cn t)
                                                             (let ((njc (%make-java-instance "java/lang/Class")))
                                                               (setf (slot-value njc '|name|) (ijstring (substitute #\. #\/ cn)))
                                                               (setf (gethash (substitute #\. #\/ cn) *java-classes-by-fq-name*) njc)
                                                               (setf (gethash cn *java-classes-by-bin-name*) njc))))
                                                       ;; Primitives (I, J, Z, etc.) and arrays ([I, [Ljava/lang/String;, etc.)
                                                       (%bin-type-name-to-class cn)))
                                                 (access-flags field)
                                                 nil  ; trustedFinal
                                                 0    ; slot
                                                 nil nil)
                                                f))
                                (when (super lclass)
                                  (get-fields (%get-ldk-class-by-bin-name (super lclass) t)))))))

             (make-java-array
              :component-class (%get-java-class-by-fq-name "java.lang.reflect.Field")
              :initial-contents (coerce (get-fields lclass) 'vector)))))
    (when *debug-trace*
      (incf *call-nesting-level* -1))))

(defun |sun/misc/VM.initialize()| ()
  nil)

(defun |jdk/internal/misc/VM.initialize()| ()
  ;; JDK 9+ version
  nil)

;; JDK 17: SharedSecrets.getJavaLangAccess().getEnumConstantsShared(Class)
;; getJavaLangAccess() returns nil in our VM, so this is dispatched as a plain
;; function call with nil as 'this' and the enum Class as the argument.
(defmethod |getEnumConstantsShared(Ljava/lang/Class;)| (this enum-class)
  (declare (ignore this))
  ;; Call the enum's values() static method to get its constants array
  (let* ((ldk-class (get-ldk-class-for-java-class enum-class)))
    (when ldk-class
      (let ((values-fn-name (format nil "~A.values()" (name ldk-class))))
        (let ((fn (find-symbol values-fn-name :openldk)))
          (when (and fn (fboundp fn))
            (funcall fn)))))))

;;; JDK 9+ Class Data Sharing stubs
(defun |jdk/internal/misc/CDS.isDumpingClassList0()| () 0)
(defun |jdk/internal/misc/CDS.isDumpingArchive0()| () 0)
(defun |jdk/internal/misc/CDS.isSharingEnabled0()| () 0)
;; JDK 25: the individual CDS flag natives were folded into a single bitmask.
;; Returning 0 means no CDS features are active, which is what we want.
(defun |jdk/internal/misc/CDS.getCDSConfigStatus()| () 0)
(defun |jdk/internal/misc/CDS.initializeFromArchive(Ljava/lang/Class;)| (class)
  (declare (ignore class)) nil)
(defun |jdk/internal/misc/CDS.defineArchivedModules(Ljava/lang/ClassLoader;Ljava/lang/ClassLoader;)| (a b)
  (declare (ignore a b)) nil)
(defun |jdk/internal/misc/CDS.getRandomSeedForDumping()| () 0)

;;; JDK 9+ Reference handling native stubs
(defun |java/lang/ref/Reference.waitForReferencePendingList()| () nil)
(defun |java/lang/ref/Reference.getAndClearReferencePendingList()| () nil)
(defun |java/lang/ref/Reference.hasReferencePendingList()| () 0)

;;; OpenLDK runs no reference-processing threads (SBCL's GC handles memory),
;;; so Reference's clinit never registers a JavaLangRefAccess.  initPhase1
;;; calls SharedSecrets.getJavaLangRefAccess().startThreads() and Bits
;;; calls .waitForReferenceProcessing(), so hand out a no-op stub instead
;;; of the null that made initPhase1 NPE and abort mid-way.

(defclass %java-lang-ref-access-stub (|java/lang/Object|) ())

(defvar *java-lang-ref-access-stub* (make-instance '%java-lang-ref-access-stub))

(defmethod |startThreads()| ((this %java-lang-ref-access-stub))
  nil)

(defmethod |runFinalization()| ((this %java-lang-ref-access-stub))
  nil)

(defmethod |waitForReferenceProcessing()| ((this %java-lang-ref-access-stub))
  0)

(setf (gethash "jdk/internal/access/SharedSecrets.getJavaLangRefAccess()Ljdk/internal/access/JavaLangRefAccess;"
               *native-overrides*)
      (lambda () *java-lang-ref-access-stub*))

(defmethod |clear0()| ((this |java/lang/ref/Reference|))
  "Native clear0: set the referent to null."
  (when (slot-exists-p this '|referent|)
    (setf (slot-value this '|referent|) nil)))

;;; Finalizer native stubs
(defun |java/lang/ref/Finalizer.isFinalizationEnabled()| ()
  "JDK 21: returns whether finalization is enabled. Always true for OpenLDK."
  1)

(defun |java/lang/ref/Finalizer.reportComplete(Ljava/lang/Object;)| (obj)
  (declare (ignore obj)) nil)

(defmethod |compareAndSwapObject(Ljava/lang/Object;JLjava/lang/Object;Ljava/lang/Object;)| ((unsafe |sun/misc/Unsafe|) obj field-id expected-value new-value)
  (bordeaux-threads:with-recursive-lock-held (*cas-lock*)
  (cond
    ((typep obj 'java-array)
     (let ((index (%unsafe-array-index obj field-id)))
       (if (equal (jaref obj index) expected-value)
         (progn
           (setf (jaref obj index) new-value)
           1)
           0)))
    ((null obj)
     ;; Unsafe represents a static field as a null base plus a synthetic
     ;; offset in OpenLDK.  Go through the ordinary accessors so that the
     ;; declaring class's static storage object is selected.
     (if (equal (|getObject(Ljava/lang/Object;J)| unsafe obj field-id)
                expected-value)
         (progn
           (|putObject(Ljava/lang/Object;JLjava/lang/Object;)|
            unsafe obj field-id new-value)
           1)
         0))
    (t
     (let ((key (%unsafe-slot-key field-id)))
       (if (equal (slot-value obj key) expected-value)
           (progn
             (setf (slot-value obj key) new-value)
             1)
           0))))))

(defmethod |compareAndSwapInt(Ljava/lang/Object;JII)| ((unsafe |sun/misc/Unsafe|) obj field-id expected-value new-value)
  (bordeaux-threads:with-recursive-lock-held (*cas-lock*)
  ;; FIXME: use atomics package
  (if (typep obj 'java-array)
      (let ((index (%unsafe-array-index obj field-id)))
        (if (equal (jaref obj index) expected-value)
            (progn
              (setf (jaref obj index) new-value)
              1)
            0))
      (if (null obj)
      (if (equal (|getInt(Ljava/lang/Object;J)| unsafe obj field-id)
                 expected-value)
          (progn
            (|putInt(Ljava/lang/Object;JI)| unsafe obj field-id new-value)
            1)
          0)
      (let ((key (%unsafe-slot-key field-id)))
        (if (equal (slot-value obj key) expected-value)
            (progn
              (setf (slot-value obj key) new-value)
              1)
            0))))))

(defmethod |compareAndSwapLong(Ljava/lang/Object;JJJ)| ((unsafe |sun/misc/Unsafe|) obj field-id expected-value new-value)
  (bordeaux-threads:with-recursive-lock-held (*cas-lock*)
  (if (typep obj 'java-array)
      (let ((index (%unsafe-array-index obj field-id)))
        (if (equal (jaref obj index) expected-value)
            (progn
              (setf (jaref obj index) new-value)
              1)
            0))
      (if (null obj)
      (if (equal (|getLong(Ljava/lang/Object;J)| unsafe obj field-id)
                 expected-value)
          (progn
            (|putLong(Ljava/lang/Object;JJ)| unsafe obj field-id new-value)
            1)
          0)
          (|compareAndSwapInt(Ljava/lang/Object;JII)|
           unsafe obj field-id expected-value new-value)))))

(defmethod |getObjectVolatile(Ljava/lang/Object;J)| ((unsafe |sun/misc/Unsafe|) obj l)
  (cond
    ((typep obj 'java-array)
     (jaref obj (%unsafe-array-index obj l)))
    ((typep obj '|java/lang/Object|)
     (slot-value obj (%unsafe-slot-key l)))
    ((null obj)
     ;; FIXME: check that the field is STATIC
     (slot-value (%unsafe-static-storage (gethash l *field-offset-table*))
                 (%unsafe-slot-key l)))
    (t (internal-error "unrecognized object type in getObjectVolatile: ~A" obj))))

(defmethod |putObjectVolatile(Ljava/lang/Object;JLjava/lang/Object;)| ((unsafe |sun/misc/Unsafe|) obj l value)
  (cond
    ((typep obj 'java-array)
     (setf (jaref obj (%unsafe-array-index obj l)) value))
    ((typep obj '|java/lang/Object|)
     (setf (slot-value obj (%unsafe-slot-key l)) value))
    ((null obj)
     ;; Static field access
     (setf (slot-value (%unsafe-static-storage (gethash l *field-offset-table*))
                       (%unsafe-slot-key l))
           value))
    (t (internal-error "unrecognized object type in putObjectVolatile: ~A" obj))))

;; getObject - same as getObjectVolatile for OpenLDK (no volatile semantics needed in Lisp)
(defmethod |getObject(Ljava/lang/Object;J)| ((unsafe |sun/misc/Unsafe|) obj l)
  (cond
    ((typep obj 'java-array)
     (jaref obj (%unsafe-array-index obj l)))
    ((typep obj '|java/lang/Object|)
     (slot-value obj (%unsafe-slot-key l)))
    ((null obj)
     ;; FIXME: check that the field is STATIC
     (let ((field (gethash l *field-offset-table*)))
       (when field
         (let ((static-obj (%unsafe-static-storage field :errorp nil))
               (key (%unsafe-slot-key l)))
           (when (and static-obj (slot-boundp static-obj key))
             (slot-value static-obj key))))))
    (t (internal-error "unrecognized object type in getObject: ~A" obj))))

(defmethod |getLongVolatile(Ljava/lang/Object;J)| ((unsafe |sun/misc/Unsafe|) obj l)
  (cond
    ((typep obj 'java-array)
     (jaref obj (%unsafe-array-index obj l)))
    ((typep obj '|java/lang/Object|)
     (slot-value obj (%unsafe-slot-key l)))
    (t (internal-error "unrecognized object type in getLongVolatile: ~A" obj))))

(defmethod |putObject(Ljava/lang/Object;JLjava/lang/Object;)| ((unsafe |sun/misc/Unsafe|) obj l value)
  (cond
    ((typep obj 'java-array)
     (setf (jaref obj (%unsafe-array-index obj l)) value))
    ((typep obj '|java/lang/Object|)
     (setf (slot-value obj (%unsafe-slot-key l)) value))
    ((null obj)
     ;; Static field access
     (setf (slot-value (%unsafe-static-storage (gethash l *field-offset-table*))
                       (%unsafe-slot-key l))
           value))
    (t (internal-error "unrecognized object type in putObject: ~A" obj))))

(defmethod |putOrderedObject(Ljava/lang/Object;JLjava/lang/Object;)| ((unsafe |sun/misc/Unsafe|) obj l value)
  (|putObject(Ljava/lang/Object;JLjava/lang/Object;)| unsafe obj l value))

;;; JDK 9+ renamed native methods -- contain actual implementations to avoid
;;; infinite recursion with compiled bytecode that delegates old→new names.

(defmethod |getReference(Ljava/lang/Object;J)| ((unsafe |sun/misc/Unsafe|) obj l)
  (cond
    ((typep obj 'java-array) (jaref obj (%unsafe-array-index obj l)))
    ((typep obj '|java/lang/Object|)
     (slot-value obj (%unsafe-slot-key l)))
    ((null obj)
     (let ((field (gethash l *field-offset-table*)))
       (when field
         (let ((static-obj (%unsafe-static-storage field :errorp nil))
               (key (%unsafe-slot-key l)))
           (when (and static-obj (slot-boundp static-obj key))
             (slot-value static-obj key))))))
    (t nil)))

(defmethod |putReference(Ljava/lang/Object;JLjava/lang/Object;)| ((unsafe |sun/misc/Unsafe|) obj l value)
  (cond
    ((typep obj 'java-array)
     (setf (jaref obj (%unsafe-array-index obj l)) value))
    ((typep obj '|java/lang/Object|)
     (setf (slot-value obj (%unsafe-slot-key l)) value))
    ((null obj)
     (setf (slot-value (%unsafe-static-storage (gethash l *field-offset-table*))
                       (%unsafe-slot-key l))
           value))
    (t nil)))

(defmethod |getReferenceVolatile(Ljava/lang/Object;J)| ((unsafe |sun/misc/Unsafe|) obj l)
  (|getReference(Ljava/lang/Object;J)| unsafe obj l))

(defmethod |putReferenceVolatile(Ljava/lang/Object;JLjava/lang/Object;)| ((unsafe |sun/misc/Unsafe|) obj l value)
  (|putReference(Ljava/lang/Object;JLjava/lang/Object;)| unsafe obj l value))

(defmethod |putReferenceRelease(Ljava/lang/Object;JLjava/lang/Object;)| ((unsafe |sun/misc/Unsafe|) obj l value)
  (|putReference(Ljava/lang/Object;JLjava/lang/Object;)| unsafe obj l value))

(defmethod |getReferenceAcquire(Ljava/lang/Object;J)| ((unsafe |sun/misc/Unsafe|) obj l)
  (|getReference(Ljava/lang/Object;J)| unsafe obj l))

(defun |java/security/AccessController.getStackAccessControlContext()| ()
  ;; NIL means "privileged context"; fine with the SecurityManager gone.
  nil)

(defun |java/security/AccessController.ensureMaterializedForStackWalk(Ljava/lang/Object;)| (obj)
  "JDK 17 no-op: prevents JIT from optimizing away references during stack walks."
  (declare (ignore obj))
  nil)

;; ---------------------------------------------------------------------------
;; JDK 25: SystemProps$Raw native methods for System.initPhase1()
;; platformProperties() returns a String[FIXED_LENGTH] indexed by the _NDX
;; constants declared in jdk.internal.util.SystemProps.Raw. The index order
;; changed relative to JDK 17/21 (file.encoding was replaced by native.encoding,
;; and stdin/stdout/stderr encodings were added), so FIXED_LENGTH is now 40.
;; vmProperties() returns String[] of key-value pairs (like -D properties).

