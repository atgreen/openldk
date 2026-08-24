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

;;; ConstantPool natives and assorted JDK internals.

(in-package :openldk)

;;; JDK 9+ ConstantPool native methods (jdk/internal/reflect/ConstantPool)
(defmethod |getUTF8At0(Ljava/lang/Object;I)| ((this |jdk/internal/reflect/ConstantPool|) cp index)
  (let* ((cp (constant-pool (ldk-class cp)))
         (s (format nil "~A" (emit (aref cp index) cp))))
    (jstring s)))

(defmethod |getIntAt0(Ljava/lang/Object;I)| ((this |jdk/internal/reflect/ConstantPool|) cp index)
  (let* ((cp (constant-pool (ldk-class cp)))
         (i (slot-value (aref cp index) 'value)))
    i))

(defmethod |getLongAt0(Ljava/lang/Object;I)| ((this |jdk/internal/reflect/ConstantPool|) cp index)
  (let* ((cp (constant-pool (ldk-class cp)))
         (v (slot-value (aref cp index) 'value)))
    v))

(defmethod |getFloatAt0(Ljava/lang/Object;I)| ((this |jdk/internal/reflect/ConstantPool|) cp index)
  (let* ((cp (constant-pool (ldk-class cp)))
         (v (slot-value (aref cp index) 'value)))
    (coerce v 'single-float)))

(defmethod |getDoubleAt0(Ljava/lang/Object;I)| ((this |jdk/internal/reflect/ConstantPool|) cp index)
  (let* ((cp (constant-pool (ldk-class cp)))
         (v (slot-value (aref cp index) 'value)))
    (coerce v 'double-float)))

(defmethod |getSize0(Ljava/lang/Object;)| ((this |jdk/internal/reflect/ConstantPool|) cp)
  (length (constant-pool (ldk-class cp))))

(defmethod |getTagAt0(Ljava/lang/Object;I)| ((this |jdk/internal/reflect/ConstantPool|) cp index)
  (let* ((pool (constant-pool (ldk-class cp)))
         (entry (aref pool index)))
    (etypecase entry
      (ir-string-literal 1)   ; CONSTANT_Utf8 stored as ir-string-literal
      (constant-int 3)
      (constant-float 4)
      (constant-long 5)
      (constant-double 6)
      (constant-class-reference 7)
      (constant-string-reference 8)
      (constant-field-reference 9)
      (constant-interface-method-reference 11)
      (constant-method-reference 10)
      (constant-name-and-type-descriptor 12)
      (constant-method-handle 15)
      (constant-method-type 16)
      (constant-invoke-dynamic 18)
      (constant-dynamic 17)
      (constant-module-reference 19)
      (constant-package-reference 20)
      (null 0))))

(defclass byte-array-input-stream (trivial-gray-streams:fundamental-binary-input-stream)
  ((array :initarg :array :reader stream-array)
   (start :initarg :start :reader stream-start)
   (end   :initarg :end   :reader stream-end)
   (pos   :initform 0     :accessor stream-pos)))

(defmethod trivial-gray-streams:stream-read-byte ((stream byte-array-input-stream))
  ;; Reads the next byte or returns (values NIL T) on EOF
  (with-slots (array start end pos) stream
    (let ((index (+ start pos)))
      (if (>= index end)
          (values nil t)  ; indicates EOF
          (prog1 (%signed-to-unsigned-byte (jaref array index))
            (incf pos))))))

(defmethod common-lisp:stream-element-type ((stream byte-array-input-stream))
  '(unsigned-byte 8))

(defun |java/lang/reflect/Proxy.defineClass0(Ljava/lang/ClassLoader;Ljava/lang/String;[BII)|
    (class-loader class-name data offset length)
  (let* ((ldk-loader (get-ldk-loader-for-java-loader class-loader))
         (stream (make-instance 'byte-array-input-stream
                               :array data
                               :start offset
                               :end (+ offset length))))
    (let ((result (%classload-from-stream (substitute #\/ #\. (lstring class-name)) stream class-loader ldk-loader)))
      (unless result
        (let ((exc (%make-java-instance "java/lang/NoClassDefFoundError")))
          (|<init>(Ljava/lang/String;)| exc class-name)
          (error (%lisp-condition exc))))
      (java-class result))))

(defmethod |defineClass(Ljava/lang/String;[BIILjava/lang/ClassLoader;Ljava/security/ProtectionDomain;)|
    ((unsafe |sun/misc/Unsafe|) class-name data offset length class-loader protection-domain)
  (declare (ignore protection-domain))
  (let* ((ldk-loader (get-ldk-loader-for-java-loader class-loader))
         (stream (make-instance 'byte-array-input-stream :array data :start offset :end (+ offset length)))
         (result (%classload-from-stream (substitute #\/ #\. (lstring class-name)) stream class-loader ldk-loader)))
    (unless result
      (let ((exc (%make-java-instance "java/lang/NoClassDefFoundError")))
        (|<init>(Ljava/lang/String;)| exc class-name)
        (error (%lisp-condition exc))))
    (java-class result)))

(defmethod |defineClass1(Ljava/lang/String;[BIILjava/security/ProtectionDomain;Ljava/lang/String;)|
    ((class-loader |java/lang/ClassLoader|) class-name data offset length protection-domain source)
  (declare (ignore source)
           (ignore protection-domain))
  (let* ((ldk-loader (get-ldk-loader-for-java-loader class-loader))
         (stream (make-instance 'byte-array-input-stream :array data :start offset :end (+ offset length)))
         (result (%classload-from-stream (substitute #\/ #\. (lstring class-name)) stream class-loader ldk-loader)))
    (unless result
      (let ((exc (%make-java-instance "java/lang/NoClassDefFoundError")))
        (|<init>(Ljava/lang/String;)| exc class-name)
        (error (%lisp-condition exc))))
    (java-class result)))

(defmethod |allocateInstance(Ljava/lang/Class;)| ((unsafe |sun/misc/Unsafe|) class)
  (let* ((bin-name (substitute #\/ #\. (lstring (slot-value class '|name|))))
         (pkg (class-package bin-name)))
    (make-instance (intern bin-name pkg))))

(defmethod |getLocalHostName()| ((inet4 |java/net/Inet4AddressImpl|))
  (jstring (uiop:hostname)))

(defmethod |getLocalHostName()| ((inet4 |java/net/Inet4AddressImpl|))
  (jstring (uiop:hostname)))

(defmethod |lookupAllHostAddr(Ljava/lang/String;)| ((inet4 |java/net/Inet4AddressImpl|) hostname)
  (let (;; FIXME (hostent (sb-bsd-sockets:get-host-by-name (lstring hostname)))
        (inet4addr (%make-java-instance "java/net/Inet4Address")))
    (|<init>(Ljava/lang/String;[B)| inet4addr hostname (make-java-array
                                                        :component-class (%get-java-class-by-fq-name "byte")
                                                        :initial-contents (coerce (mapcar #'parse-integer (uiop:split-string "127.0.0.1" :separator '(#\.))) 'vector)))
    (make-java-array
     :component-class (%get-java-class-by-fq-name "byte")
     :initial-contents (coerce (list inet4addr) 'vector))))

#|
(sb-bsd-sockets:host-ent-addresses (sb-bsd-sockets:get-host-by-name "fedora"))
|#

(defun |java/net/Inet4Address.init()| ()
  nil)

(defmethod |getOption(I)| ((this |java/net/SocketOptions|) option-id)
  (declare (ignore this)
           (ignore option-id))
  (slot-value |+static-java/lang/Boolean+| '|TRUE|))

(defun |sun/management/MemoryImpl.getMemoryManagers0()| ()
  (let* ((mm-mxbean-class (|java/lang/Class.forName0(Ljava/lang/String;ZLjava/lang/ClassLoader;Ljava/lang/Class;)|
                           (jstring "sun/management/MemoryManagerImpl") nil nil nil))
         (mm-mxbean (%make-java-instance "sun/management/MemoryManagerImpl")))
    (|<init>(Ljava/lang/String;)| mm-mxbean (jstring "sbcl-heap-manager"))
    (make-java-array :component-class (%get-java-class-by-bin-name "java/lang/management/MemoryManagerMXBean")
                     :initial-contents (list mm-mxbean))))

(defun |sun/management/MemoryImpl.getMemoryPools0()| ()
  (let* ((mp-mxbean-class (|java/lang/Class.forName0(Ljava/lang/String;ZLjava/lang/ClassLoader;Ljava/lang/Class;)|
                           (jstring "sun/management/MemoryPoolImpl") nil nil nil))

         ;; Allocate a single pool for demonstration
         (mp-mxbean (%make-java-instance "sun/management/MemoryPoolImpl")))

    (|<init>(Ljava/lang/String;ZJJ)|
     mp-mxbean (jstring "SBCL Heap")
     t                               ;; isHeap = true
     ;; Negative thresholds mean "not supported" -- SBCL's GC has no
     ;; usage/collection threshold notification mechanism to back them.
     -1                              ;; usageThreshold
     -1)                             ;; gcThreshold

    ;; Return it as a Java array of the interface type
    (make-java-array :component-class (%get-java-class-by-bin-name "java/lang/management/MemoryPoolMXBean")
                     :initial-contents (list mp-mxbean))))

(defun |jdk/internal/platform/CgroupMetrics.isUseContainerSupport()| ()
  0)

(defmethod |getStartupTime()| ((this |sun/management/VMManagementImpl|))
  "VM start time in epoch milliseconds (RuntimeMXBean.getStartTime)."
  (or *vm-start-time-millis*
      (|java/lang/System.currentTimeMillis()|)))

(defmethod |read0()| ((this |java/io/FileInputStream|))
  (let* ((file-descriptor (slot-value this '|fd|))
         (fd (if (and file-descriptor (slot-exists-p file-descriptor '|fd|))
                 (slot-value file-descriptor '|fd|)
                 file-descriptor)))
    (cond
      ((eql fd 0) (force-output *standard-output*)
                  (let ((b (read-byte *standard-input* nil nil)))
                    (or b -1)))
      ((streamp fd) (let ((b (read-byte fd nil nil)))
                      (or b -1)))
      (t (unimplemented "fd ~A in FileInputStream.read0" fd)))))

(defun %class->descriptor-string (class)
  "Return a JVM type descriptor fragment for the given java.lang.Class."
  (let* ((raw-name (and (slot-exists-p class '|name|)
                        (slot-boundp class '|name|)
                        (slot-value class '|name|)))
         (name (and raw-name (lstring raw-name))))
    (cond
      ((null name) (error "Missing class name for descriptor ~A" class))
      ((string= name "void") "V")
      ((string= name "boolean") "Z")
      ((string= name "byte") "B")
      ((string= name "char") "C")
      ((string= name "short") "S")
      ((string= name "int") "I")
      ((string= name "long") "J")
      ((string= name "float") "F")
      ((string= name "double") "D")
      ;; Array names are already descriptor-shaped, except '.' vs '/'
      ;; Normalize incorrect [Lprimitive; names (e.g. [Lbyte; → [B)
      ((char= (char name 0) #\[)
       (let ((rest (subseq name 1)))
         (cond
           ((string= rest "Lbyte;")    "[B")
           ((string= rest "Lshort;")   "[S")
           ((string= rest "Lint;")     "[I")
           ((string= rest "Llong;")    "[J")
           ((string= rest "Lfloat;")   "[F")
           ((string= rest "Ldouble;")  "[D")
           ((string= rest "Lchar;")    "[C")
           ((string= rest "Lboolean;") "[Z")
           (t (substitute #\/ #\. name)))))
      (t
       (format nil "L~A;" (substitute #\/ #\. name))))))

(defun %build-method-descriptor (rtype ptypes-array)
  (let* ((params (map 'list #'%class->descriptor-string
                      (or (and ptypes-array (java-array-data ptypes-array))
                          #())))
         (ret (%class->descriptor-string rtype)))
    (format nil "(~{~A~})~A" params ret)))

(defun %make-simple-method-type (rtype ptypes-array)
  "Construct a minimal MethodType instance backed by R T and PTYPES-ARRAY."
  (classload "java/lang/invoke/MethodType")
  (let* ((mt (%make-java-instance "java/lang/invoke/MethodType"))
         (descriptor (%build-method-descriptor rtype ptypes-array)))
    (setf (slot-value mt '|rtype|) rtype)
    (when (slot-exists-p mt '|ptypes|)
      (setf (slot-value mt '|ptypes|) ptypes-array))
    (when (slot-exists-p mt '|methodDescriptor|)
      (setf (slot-value mt '|methodDescriptor|) (jstring descriptor)))
    mt))

(defun |java/lang/invoke/MethodType.methodType(Ljava/lang/Class;)| (rtype)
  (%make-simple-method-type
   rtype
   (make-java-array :component-class (%get-java-class-by-bin-name "java/lang/Class")
                    :initial-contents '())))

(defun |java/lang/invoke/MethodType.methodType(Ljava/lang/Class;Ljava/lang/Class;)| (rtype p0)
  (%make-simple-method-type
   rtype
   (make-java-array :component-class (%get-java-class-by-bin-name "java/lang/Class")
                    :initial-contents (list p0))))

(defun |java/lang/invoke/MethodType.methodType(Ljava/lang/Class;[Ljava/lang/Class;)| (rtype ptypes)
  (%make-simple-method-type rtype ptypes))

(defun |java/lang/invoke/MethodType.methodType(Ljava/lang/Class;Ljava/lang/Class;[Ljava/lang/Class;)| (rtype p0 ptypes)
  "MethodType.methodType(Class rtype, Class ptype0, Class... ptypes)"
  (let* ((extra (java-array-data ptypes))
         (all-ptypes (make-java-array :component-class (%get-java-class-by-bin-name "java/lang/Class")
                                      :initial-contents (cons p0 (coerce extra 'list)))))
    (%make-simple-method-type rtype all-ptypes)))

(defun |java/lang/invoke/MethodType.parameterCount()| (this)
  "Native implementation of MethodType.parameterCount() with logging to trace arity issues."
  (let* ((ptypes (when (and (slot-exists-p this '|ptypes|)
                            (slot-boundp this '|ptypes|))
                   (slot-value this '|ptypes|)))
         (count (if ptypes
                    (java-array-length ptypes)
                    0)))
    (format t "~&*** MethodType.parameterCount() called ***~%")
    (format t "    MethodType: ~A~%" this)
    (format t "    ptypes array: ~A~%" ptypes)
    (format t "    Returning count: ~A~%" count)
    (format t "    Type of count: ~A~%" (type-of count))
    (force-output)
    ;; If we see 255 being returned, trigger a break
    (when (= count 255)
      (format t "~%!!! WARNING: parameterCount() returning 255 - this is the bug! !!!~%")
      (force-output)
      (break "parameterCount() returned 255"))
    count))

(defun |java/lang/invoke/MethodHandleNatives.registerNatives()| ()
  nil)

(defun |java/lang/invoke/MethodHandleNatives.getConstant(I)| (i)
  (declare (ignore i))
  0)

(defun |java/lang/invoke/MethodHandleNatives.getNamedCon(I[Ljava/lang/Object;)| (which objarray)
  (declare (ignore objarray))
  (assert (zerop which))
  0)

(defun find-method-in-class (class name &key static)
  "Find a method by name in a class. When STATIC is :yes, only match static methods.
   When STATIC is :no, only match non-static methods. When STATIC is nil, match any."
  (find-if (lambda (m)
             (and (string= (slot-value m 'name) name)
                  (case static
                    (:yes (not (zerop (logand #x0008 (access-flags m)))))
                    (:no  (zerop (logand #x0008 (access-flags m))))
                    (t t))))
           (coerce (slot-value class 'methods) 'list)))

(defun |java/lang/invoke/MethodHandleNatives.resolve(Ljava/lang/invoke/MemberName;Ljava/lang/Class;IZ)| (member-name klass speculative-resolve native-access)
  "JDK 17: resolve(MemberName self, Class<?> caller, int speculativeResolve, boolean nativeAccess)"
  (declare (ignore klass speculative-resolve native-access))
  (let* ((member-class (slot-value member-name '|clazz|))
         (ldk-class (get-ldk-class-for-java-class member-class))
         (mn-flags (slot-value member-name '|flags|))
         ;; Reference kind is in bits 24-27. REF_invokeStatic = 6.
         (ref-kind (logand #xf (ash mn-flags -24)))
         ;; Also check ACC_STATIC in modifier bits (0x0008)
         (want-static (or (= ref-kind 6) (not (zerop (logand #x0008 mn-flags)))))
         (method (when ldk-class
                   (find-method-in-class ldk-class (lstring (slot-value member-name '|name|))
                                         :static (if want-static :yes :no)))))
    (when method
      (setf (slot-value member-name '|flags|) (logior mn-flags (access-flags method)))))
  member-name)

(defun |java/lang/invoke/MethodHandleNatives.getMemberVMInfo(Ljava/lang/invoke/MemberName;)| (member-name)
  (let ((o (%make-java-instance "java/lang/Long"))
        (vm-target member-name)
        (flags (slot-value member-name '|flags|)))
    (cond
      ((eq (logand flags #x40000) #x40000)
       ;; Field
       (setf vm-target (slot-value member-name '|type|))
       (setf (slot-value o '|value|) 31337))
      ((and (eq (logand flags #x10000) #x10000)
            (eq (logand flags (ash 6 24)) (ash 6 24)))
       ;; Static Method
       (setf (slot-value o '|value|) -31337))
      (t
       ;; Other Method
       (setf (slot-value o '|value|) 31337)))
    (make-java-array :component-class (%get-java-class-by-bin-name "java/lang/Object")
                     :initial-contents (list o vm-target))))

(defun |java/lang/invoke/MethodHandleNatives.init(Ljava/lang/invoke/MemberName;Ljava/lang/Object;)| (member-name objref)
  (setf (slot-value member-name '|clazz|) (slot-value objref '|clazz|))
  (setf (slot-value member-name '|name|) (slot-value objref '|name|))

  ;; This must be a method
  (assert (typep objref '|java/lang/reflect/Method|))

  #|
  See https://github.com/openjdk/jdk8u/blob/b10963f0e8db961c6122e092372c5dc56e1a755e/hotspot/src/share/vm/prims/methodHandles.cpp
  and...
        static final int
                MN_IS_METHOD           = 0x00010000, // method (not constructor)
                MN_IS_CONSTRUCTOR      = 0x00020000, // constructor
                MN_IS_FIELD            = 0x00040000, // field
                MN_IS_TYPE             = 0x00080000, // nested type
                MN_CALLER_SENSITIVE    = 0x00100000, // @CallerSensitive annotation detected
                MN_REFERENCE_KIND_SHIFT = 24, // refKind
                MN_REFERENCE_KIND_MASK = 0x0F000000 >> MN_REFERENCE_KIND_SHIFT,
                // The SEARCH_* bits are not for MN.flags but for the matchFlags argument of MHN.getMembers:
                MN_SEARCH_SUPER CLASSES = 0x00100000,
                MN_SEARCH_INTERFACES   = 0x00200000;

  Also of note:

            REF_NONE                    = 0,  // null value
            REF_getField                = 1,
            REF_getStatic               = 2,
            REF_putField                = 3,
            REF_putStatic               = 4,
            REF_invokeVirtual           = 5,
            REF_invokeStatic            = 6,
            REF_invokeSpecial           = 7,
            REF_newInvokeSpecial        = 8,
            REF_invokeInterface         = 9,
            REF_LIMIT                  = 10;
  |#

  (cond
    ((not (eq 0 (logand #x8 (slot-value objref '|modifiers|))))
     ;; Static method
     (setf (slot-value member-name '|flags|) (logior #x10000 (ash 6 24) (slot-value objref '|modifiers|))))
    (t
     ;; Any other method
     (setf (slot-value member-name '|flags|) (logior #x10000 (ash 5 24) (slot-value objref '|modifiers|))))))

(defmethod |defineAnonymousClass(Ljava/lang/Class;[B[Ljava/lang/Object;)|
    ((unsafe |sun/misc/Unsafe|) clazz data cp-patches)
  (let* ((stream (make-instance 'byte-array-input-stream :array data :start 0 :end (java-array-length data)))
         (java-loader (slot-value clazz '|classLoader|))
         (ldk-loader (get-ldk-loader-for-java-loader java-loader))
         (result (%classload-from-stream (format nil "~A/~A" (substitute #\/ #\. (lstring (slot-value clazz '|name|))) (gensym "anonymous-class-")) stream java-loader ldk-loader)))
    (unless result
      (let ((exc (%make-java-instance "java/lang/NoClassDefFoundError")))
        (|<init>(Ljava/lang/String;)| exc (slot-value clazz '|name|))
        (error (%lisp-condition exc))))
    (java-class result)))

(defun %invoke-polymorphic-signature (method-handle &rest args)
  "Invoke a MethodHandle's target method with the given arguments.
   The first argument is the MethodHandle, the rest are passed to the target."
  ;; Extract the target MemberName from the MethodHandle
  (let* ((form (when (and (slot-exists-p method-handle '|form|)
                          (slot-boundp method-handle '|form|))
                 (slot-value method-handle '|form|)))
         (vmentry (when (and form
                             (slot-exists-p form '|vmentry|)
                             (slot-boundp form '|vmentry|))
                    (slot-value form '|vmentry|))))
    (unless vmentry
      (error "MethodHandle.invokeExact: no vmentry found in ~A" method-handle))

    ;; Extract method information from the MemberName
    (let* ((clazz (when (and (slot-exists-p vmentry '|clazz|)
                            (slot-boundp vmentry '|clazz|))
                   (slot-value vmentry '|clazz|)))
           (name (when (and (slot-exists-p vmentry '|name|)
                           (slot-boundp vmentry '|name|))
                  (slot-value vmentry '|name|)))
           (type (when (and (slot-exists-p vmentry '|type$|)
                           (slot-boundp vmentry '|type$|))
                  (slot-value vmentry '|type$|)))
           (flags (when (and (slot-exists-p vmentry '|flags|)
                            (slot-boundp vmentry '|flags|))
                   (slot-value vmentry '|flags|))))

      (unless (and clazz name type)
        (error "MethodHandle.invokeExact: incomplete MemberName ~A" vmentry))

      ;; Get the class name and method name as strings
      (let* ((class-name-raw (lstring (slot-value clazz '|name|)))
             ;; Class names from Class.getName() use . separator, but we need /
             (class-name (substitute #\/ #\. class-name-raw))
             (method-name (lstring name))
             ;; type can be either a String (descriptor) or a MethodType
             (method-type (if (typep type '|java/lang/String|)
                              ;; It's already a string descriptor
                              (lstring type)
                              ;; It's a MethodType, build descriptor from rtype/ptypes
                              (let ((rtype (slot-value type '|rtype|))
                                    (ptypes (when (slot-exists-p type '|ptypes|)
                                              (slot-value type '|ptypes|))))
                                (%build-method-descriptor rtype ptypes))))
             ;; Check if it's a static method (REF_invokeStatic = 6, shifted left by 24 bits)
             (ref-kind (ash (logand flags #x0F000000) -24))
             (is-static (= ref-kind 6)))

        ;; Construct the lispized method name: class.method(descriptor)
        ;; Static methods use loader's package (include class name)
        ;; Instance methods use :openldk (generic function dispatch)
        (let* ((full-method-sig (format nil "~A.~A~A" class-name method-name method-type))
               (lispized-name (lispize-method-name full-method-sig))
               (pkg (if is-static
                        (class-package class-name)
                        (find-package :openldk)))
               (lisp-method-name (intern lispized-name pkg)))

          ;; Invoke the method.
          ;; LambdaForm internal methods (invokeStaticInit_*, etc.) expect the
          ;; MethodHandle as their first argument (part of the LambdaForm calling
          ;; convention).  Actual target methods do NOT -- they receive only the
          ;; user-visible arguments.  We distinguish by checking the parameter
          ;; count: if the method takes exactly (length args) parameters, skip the
          ;; MethodHandle; if it takes (1+ (length args)), prepend it.
          (let ((param-count (count-parameters method-type)))
            (if (= param-count (length args))
                (apply lisp-method-name args)
                (apply lisp-method-name method-handle args))))))))

(defun |java/lang/invoke/MethodHandles.lookup()| ()
  "Return a basic MethodHandles.Lookup instance. We intentionally relax access checks for now."
  (classload "java/lang/invoke/MethodHandles$Lookup")
  (let ((lk (%make-java-instance "java/lang/invoke/MethodHandles$Lookup"))
        ;; We don't have caller-sensitive machinery yet; default to Object to
        ;; avoid NIL lookupClass that blows up in LambdaMetafactory.
        (caller-class (%get-java-class-by-bin-name "java/lang/Object")))
    (when (slot-exists-p lk '|lookupClass|)
      (setf (slot-value lk '|lookupClass|) caller-class))
    ;; Treat this lookup as trusted to bypass Java access checks for now.
    (when (slot-exists-p lk '|allowedModes|)
      (setf (slot-value lk '|allowedModes|) -1)) ; TRUSTED in JDK sources
    lk))

(defun |java/lang/invoke/MethodHandles$Lookup.lookupClass()| (this)
  "Return the class this lookup was created for."
  (when (slot-exists-p this '|lookupClass|)
    (slot-value this '|lookupClass|)))

(defun |java/lang/invoke/MethodHandles$Lookup.hasFullPrivilegeAccess()| (this)
  "Conservatively claim full privilege to keep bootstrap happy."
  (declare (ignore this))
  1) ; true

(defun |java/lang/invoke/MethodHandles$Lookup.checkUnprivilegedlookupClass(Ljava/lang/Class;I)| (klass mode)
  "Native no-op to bypass security check for unprivileged lookup classes.
   The JDK throws IllegalArgumentException for bootstrap classes (java.*, sun.*)
   with full access mode (15), but we need to allow this for lambda metafactory."
  (declare (ignore klass mode))
  nil)

(defun %build-member-name-for-static (klass name method-type)
  (classload "java/lang/invoke/MemberName")
  (let* ((mn (%make-java-instance "java/lang/invoke/MemberName"))
         ;; MN_IS_METHOD | REF_invokeStatic << 24 | ACC_STATIC
         (flags (logior #x10000 (ash 6 24) #x0008)))
    (when (slot-exists-p mn '|clazz|)
      (setf (slot-value mn '|clazz|) klass))
    (when (slot-exists-p mn '|name|)
      (setf (slot-value mn '|name|) name))
    (when (slot-exists-p mn '|type$|)
      (setf (slot-value mn '|type$|) method-type))
    (when (slot-exists-p mn '|type|)
      (setf (slot-value mn '|type|) method-type))
    (when (slot-exists-p mn '|flags|)
      (setf (slot-value mn '|flags|) flags))
    mn))

(defun |java/lang/invoke/DirectMethodHandle.isCrackable()| (this)
  "DirectMethodHandles are crackable - they can reveal their internal MemberName."
  (declare (ignore this))
  1) ; Return 1 (true in Java)

(defun |java/lang/invoke/MethodHandle.isCrackable()| (this)
  "Base MethodHandles are not crackable."
  (declare (ignore this))
  0) ; Return 0 (false in Java)

(defun |java/lang/invoke/DirectMethodHandle.internalMemberName()| (this)
  "Return the MemberName from a DirectMethodHandle."
  (format t "~&*** internalMemberName() called on DirectMethodHandle~%")
  (let ((result (when (and (slot-exists-p this '|member|)
                          (slot-boundp this '|member|))
                 (slot-value this '|member|))))
    (format t "~&*** internalMemberName() returning: ~A~%" result)
    result))

(defun |java/lang/invoke/MethodHandle.internalMemberName()| (this)
  "Base MethodHandles don't have an internal MemberName."
  (declare (ignore this))
  nil)

(defun |java/lang/invoke/MethodHandles$Lookup.findStatic(Ljava/lang/Class;Ljava/lang/String;Ljava/lang/invoke/MethodType;)| (lookup klass name method-type)
  "Create a DirectMethodHandle for static method invocation.
   DirectMethodHandle is required by LambdaMetafactory for lambda expressions."
  (declare (ignore lookup))
  (classload "java/lang/invoke/DirectMethodHandle")
  (classload "java/lang/invoke/LambdaForm")
  (let* ((member (%build-member-name-for-static klass name method-type))
         (lf (%make-java-instance "java/lang/invoke/LambdaForm"))
         (mh (%make-java-instance "java/lang/invoke/DirectMethodHandle")))
    ;; Set vmentry on LambdaForm
    (setf (slot-value lf '|vmentry|) member)

    ;; Set type on MethodHandle
    (when (slot-exists-p mh '|type|)
      (setf (slot-value mh '|type|) method-type))

    ;; Set form on MethodHandle
    (when (slot-exists-p mh '|form|)
      (setf (slot-value mh '|form|) lf))

    ;; Set the member field
    (setf (slot-value mh '|member|) member)
    mh))

(defun %build-member-name-for-special (klass name method-type)
  "Build a MemberName for invokespecial (private methods, constructors, super calls)."
  (classload "java/lang/invoke/MemberName")
  (let* ((mn (%make-java-instance "java/lang/invoke/MemberName"))
         ;; MN_IS_METHOD | REF_invokeSpecial << 24 (special = 7)
         (flags (logior #x10000 (ash 7 24))))
    (when (slot-exists-p mn '|clazz|)
      (setf (slot-value mn '|clazz|) klass))
    (when (slot-exists-p mn '|name|)
      (setf (slot-value mn '|name|) name))
    (when (slot-exists-p mn '|type$|)
      (setf (slot-value mn '|type$|) method-type))
    (when (slot-exists-p mn '|type|)
      (setf (slot-value mn '|type|) method-type))
    (when (slot-exists-p mn '|flags|)
      (setf (slot-value mn '|flags|) flags))
    mn))

(defun |java/lang/invoke/MethodHandles$Lookup.findSpecial(Ljava/lang/Class;Ljava/lang/String;Ljava/lang/invoke/MethodType;Ljava/lang/Class;)|
    (lookup refc name method-type special-caller)
  "Create a DirectMethodHandle for invokespecial method invocation.
   Used for private methods, constructors, and super calls in lambda metafactory."
  (declare (ignore lookup special-caller))
  (classload "java/lang/invoke/DirectMethodHandle")
  (classload "java/lang/invoke/LambdaForm")
  (let* ((member (%build-member-name-for-special refc name method-type))
         (lf (%make-java-instance "java/lang/invoke/LambdaForm"))
         (mh (%make-java-instance "java/lang/invoke/DirectMethodHandle")))
    ;; Set vmentry on LambdaForm
    (setf (slot-value lf '|vmentry|) member)

    ;; Set type on MethodHandle
    (when (slot-exists-p mh '|type|)
      (setf (slot-value mh '|type|) method-type))

    ;; Set form on MethodHandle
    (when (slot-exists-p mh '|form|)
      (setf (slot-value mh '|form|) lf))

    ;; Set the member field
    (setf (slot-value mh '|member|) member)
    mh))

(defun %build-member-name-for-constructor (klass method-type)
  "Build a MemberName for constructor invocation."
  (classload "java/lang/invoke/MemberName")
  (let* ((mn (%make-java-instance "java/lang/invoke/MemberName"))
         ;; MN_IS_CONSTRUCTOR | REF_newInvokeSpecial << 24 (newInvokeSpecial = 8)
         (flags (logior #x20000 (ash 8 24))))
    (when (slot-exists-p mn '|clazz|)
      (setf (slot-value mn '|clazz|) klass))
    (when (slot-exists-p mn '|name|)
      (setf (slot-value mn '|name|) (jstring "<init>")))
    (when (slot-exists-p mn '|type$|)
      (setf (slot-value mn '|type$|) method-type))
    (when (slot-exists-p mn '|type|)
      (setf (slot-value mn '|type|) method-type))
    (when (slot-exists-p mn '|flags|)
      (setf (slot-value mn '|flags|) flags))
    mn))

(defun |java/lang/invoke/MethodHandles$Lookup.findConstructor(Ljava/lang/Class;Ljava/lang/invoke/MethodType;)| (lookup refc method-type)
  "Create a DirectMethodHandle for constructor invocation.
   Used by lambda metafactory for method references to constructors."
  (declare (ignore lookup))
  (classload "java/lang/invoke/DirectMethodHandle")
  (classload "java/lang/invoke/LambdaForm")
  (let* ((member (%build-member-name-for-constructor refc method-type))
         (lf (%make-java-instance "java/lang/invoke/LambdaForm"))
         (mh (%make-java-instance "java/lang/invoke/DirectMethodHandle")))
    ;; Set vmentry on LambdaForm
    (setf (slot-value lf '|vmentry|) member)

    ;; Set type on MethodHandle
    (when (slot-exists-p mh '|type|)
      (setf (slot-value mh '|type|) method-type))

    ;; Set form on MethodHandle
    (when (slot-exists-p mh '|form|)
      (setf (slot-value mh '|form|) lf))

    ;; Set the member field
    (setf (slot-value mh '|member|) member)
    mh))

(defun %build-member-name-for-virtual (klass name method-type)
  "Build a MemberName for virtual method invocation."
  (classload "java/lang/invoke/MemberName")
  (let* ((mn (%make-java-instance "java/lang/invoke/MemberName"))
         ;; MN_IS_METHOD | REF_invokeVirtual << 24 (virtual = 5)
         (flags (logior #x10000 (ash 5 24))))
    (when (slot-exists-p mn '|clazz|)
      (setf (slot-value mn '|clazz|) klass))
    (when (slot-exists-p mn '|name|)
      (setf (slot-value mn '|name|) name))
    (when (slot-exists-p mn '|type$|)
      (setf (slot-value mn '|type$|) method-type))
    (when (slot-exists-p mn '|type|)
      (setf (slot-value mn '|type|) method-type))
    (when (slot-exists-p mn '|flags|)
      (setf (slot-value mn '|flags|) flags))
    mn))

(defun |java/lang/invoke/MethodHandles$Lookup.findVirtual(Ljava/lang/Class;Ljava/lang/String;Ljava/lang/invoke/MethodType;)| (lookup refc name method-type)
  "Create a DirectMethodHandle for virtual method invocation.
   Used by lambda metafactory for instance method references."
  (declare (ignore lookup))
  (classload "java/lang/invoke/DirectMethodHandle")
  (classload "java/lang/invoke/LambdaForm")
  (let* ((member (%build-member-name-for-virtual refc name method-type))
         (lf (%make-java-instance "java/lang/invoke/LambdaForm"))
         (mh (%make-java-instance "java/lang/invoke/DirectMethodHandle")))
    ;; Set vmentry on LambdaForm
    (setf (slot-value lf '|vmentry|) member)

    ;; Set type on MethodHandle
    (when (slot-exists-p mh '|type|)
      (setf (slot-value mh '|type|) method-type))

    ;; Set form on MethodHandle
    (when (slot-exists-p mh '|form|)
      (setf (slot-value mh '|form|) lf))

    ;; Set the member field
    (setf (slot-value mh '|member|) member)
    mh))

(defun |java/lang/invoke/CallSite.getTarget()| (this)
  "Accessor required by generated invokedynamic stubs."
  (when (slot-exists-p this '|target|)
    (slot-value this '|target|)))

(defun |java/lang/invoke/MethodHandles$Lookup.revealDirect(Ljava/lang/invoke/MethodHandle;)| (lookup target)
  "Crack a direct method handle to reveal its MemberName.
   Returns a MethodHandleInfo that wraps the MemberName."
  (declare (ignore lookup))
  (format t "~&*** revealDirect() called on: ~A~%" target)
  ;; Check if target has a member slot (i.e., is a DirectMethodHandle)
  (unless (and (slot-exists-p target '|member|)
               (slot-boundp target '|member|))
    (format t "~&*** revealDirect() ERROR: target has no member slot or it's unbound~%")
    (let ((exc (%make-java-instance "java/lang/IllegalArgumentException")))
      (|<init>(Ljava/lang/String;)| exc (jstring "not a direct method handle"))
      (error (%lisp-condition exc))))

  ;; Extract the MemberName
  (let ((member (slot-value target '|member|)))
    (format t "~&*** revealDirect() member = ~A~%" member)
    (unless member
      (format t "~&*** revealDirect() ERROR: member is NIL~%")
      (let ((exc (%make-java-instance "java/lang/IllegalArgumentException")))
        (|<init>(Ljava/lang/String;)| exc (jstring "not a direct method handle"))
        (error (%lisp-condition exc))))

    ;; Get the reference kind from the MemberName flags
    ;; The flags field encodes the reference kind in bits 24-27
    (let* ((flags (when (and (slot-exists-p member '|flags|)
                            (slot-boundp member '|flags|))
                   (slot-value member '|flags|)))
           (ref-kind (if flags
                         (logand #xFF (ash flags -24))
                         5))) ; Default to REF_invokeVirtual if no flags

      ;; Create and return InfoFromMemberName
      (classload "java/lang/invoke/InfoFromMemberName")
      (let ((info (%make-java-instance "java/lang/invoke/InfoFromMemberName")))
        ;; Set the member field
        (when (slot-exists-p info '|member|)
          (setf (slot-value info '|member|) member))
        ;; Set the referenceKind field
        (when (slot-exists-p info '|referenceKind|)
          (setf (slot-value info '|referenceKind|) ref-kind))
        info))))

(defun %descriptor-param-prim-flags (descriptor)
  "Return a list of booleans, one per parameter of DESCRIPTOR, true when that
parameter has a primitive type (Z B S C I J F D)."
  (let ((flags nil)
        (index (1+ (position #\( descriptor)))
        (end (position #\) descriptor)))
    (loop while (< index end)
          do (let ((ch (char descriptor index)))
               (cond
                 ((find ch "ZBSCIJFD") (push t flags) (incf index))
                 ((char= ch #\L)
                  (push nil flags)
                  (setf index (1+ (position #\; descriptor :start index))))
                 ((char= ch #\[)
                  (loop while (char= (char descriptor index) #\[) do (incf index))
                  (if (char= (char descriptor index) #\L)
                      (setf index (1+ (position #\; descriptor :start index)))
                      (incf index))
                  (push nil flags))
                 (t (incf index)))))
    (nreverse flags)))

(defun %maybe-unbox (arg)
  "If ARG is a boxed primitive wrapper object, return its primitive value;
otherwise return ARG unchanged."
  (if (and arg
           (typep arg '|java/lang/Object|)
           (slot-exists-p arg '|value|)
           (slot-boundp arg '|value|)
           (typep arg '(or |java/lang/Integer| |java/lang/Long| |java/lang/Short|
                        |java/lang/Byte| |java/lang/Character| |java/lang/Boolean|
                        |java/lang/Float| |java/lang/Double|)))
      (slot-value arg '|value|)
      arg))

(defun %unbox-args-for-descriptor (descriptor args)
  "Unbox each element of ARGS whose corresponding parameter in DESCRIPTOR is a
primitive type. ARGS must line up with the descriptor's declared parameters
(i.e. any leading receiver has already been removed). Returns a fresh list."
  (if (null descriptor)
      args
      (loop for prim in (%descriptor-param-prim-flags descriptor)
            for arg in args
            collect (if prim (%maybe-unbox arg) arg))))

(defvar *member-name-invoke-cache*
  (make-hash-table :test 'eq :weakness :key :synchronized t)
  "Memoized invocation plans keyed by MemberName instance (eq, weak on key) so
that linkTo*/%invoke-from-member-name resolves the target symbol once per call
site instead of on every invocation.")

(defun %resolve-member-name (member-name)
  "Resolve MEMBER-NAME to a cached invocation plan (kind a b):
   (:constructor class-symbol init-symbol)
   (:static      fn-symbol   descriptor)
   (:instance    fn-symbol   descriptor)
The plan depends only on the (stable) MemberName, so it is memoized."
  (or (gethash member-name *member-name-invoke-cache*)
      (setf (gethash member-name *member-name-invoke-cache*)
            (let* ((clazz (when (and (slot-exists-p member-name '|clazz|)
                                     (slot-boundp member-name '|clazz|))
                            (slot-value member-name '|clazz|)))
                   (name (when (and (slot-exists-p member-name '|name|)
                                    (slot-boundp member-name '|name|))
                           (slot-value member-name '|name|)))
                   (type (when (and (slot-exists-p member-name '|type$|)
                                    (slot-boundp member-name '|type$|))
                           (slot-value member-name '|type$|)))
                   (flags (when (and (slot-exists-p member-name '|flags|)
                                     (slot-boundp member-name '|flags|))
                            (slot-value member-name '|flags|))))
              (unless (and clazz name)
                (error "linkTo*: incomplete MemberName ~A" member-name))
              (let* ((class-name (substitute #\/ #\. (lstring (slot-value clazz '|name|))))
                     (method-name (lstring name))
                     ;; type is a String descriptor or a MethodType
                     (method-type (if (and type (typep type '|java/lang/String|))
                                      (lstring type)
                                      (when type
                                        (let ((rtype (slot-value type '|rtype|))
                                              (ptypes (when (slot-exists-p type '|ptypes|)
                                                        (slot-value type '|ptypes|))))
                                          (%build-method-descriptor rtype ptypes)))))
                     ;; REF_invokeVirtual=5 Static=6 Special=7 newInvokeSpecial=8
                     (ref-kind (ash (logand flags #x0F000000) -24)))
                (cond
                  ((= ref-kind 8)       ; constructor
                   (classload class-name)
                   (let* ((pkg (class-package class-name))
                          (lisp-class-name (intern class-name pkg))
                          ;; <init> is void; normalise any return type to V
                          (desc (cond
                                  ((null method-type) "()V")
                                  ((position #\) method-type)
                                   (format nil "~AV" (subseq method-type 0 (1+ (position #\) method-type)))))
                                  (t (format nil "~AV" method-type))))
                          (init-method-name (format nil "<init>~A" (subseq desc 0 (1+ (position #\) desc)))))
                          (lisp-init-name (intern init-method-name :openldk)))
                     (list :constructor lisp-class-name lisp-init-name)))
                  ((= ref-kind 6)       ; static: name is class.method(desc) in the loader package
                   (let* ((java-loader (slot-value clazz '|classLoader|))
                          (ldk-loader (get-ldk-loader-for-java-loader java-loader))
                          (pkg (class-package class-name ldk-loader))
                          (full-method-sig (format nil "~A.~A~A" class-name method-name method-type)))
                     (list :static (intern (lispize-method-name full-method-sig) pkg) method-type)))
                  (t                    ; virtual/special/interface: GF named by method+descriptor
                   (list :instance
                         (intern (lispize-method-name (format nil "~A~A" method-name method-type)) :openldk)
                         method-type))))))))

(defun %invoke-from-member-name (member-name &rest args)
  "Invoke a method described by a MemberName with the given ARGS. Core of
linkToStatic/linkToVirtual/linkToSpecial; resolution is memoized per MemberName.
Primitive-typed args are unboxed so method references adapted to functional
interfaces (which pass boxed Object args) reach primitive-parameter methods."
  (destructuring-bind (kind a b) (%resolve-member-name member-name)
    (ecase kind
      (:constructor                     ; a=class-symbol, b=init-symbol
       (let ((instance (make-instance a)))
         (apply b instance args)
         instance))
      (:static                          ; a=fn-symbol, b=descriptor
       (apply a (%unbox-args-for-descriptor b args)))
      (:instance                        ; a=fn-symbol, b=descriptor; first arg is the receiver
       (apply a (if args
                    (cons (first args)
                          (%unbox-args-for-descriptor b (rest args)))
                    args))))))

