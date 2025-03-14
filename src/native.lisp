;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: OPENLDK; Base: 10 -*-
;;;
;;; Copyright (C) 2023, 2024, 2025  Anthony Green <green@moxielogic.com>
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

(in-package :openldk)

;; In Java, a method call on a NULL object results in a
;; NullPointerException.  CLOS makes it easy to implement this
;; behaviour by providing our own CLOS no-applicable-method method.

(defmethod no-applicable-method ((gf generic-function) &rest args)
  (if (null (car args))
      (error (%lisp-condition (%make-throwable '|java/lang/NullPointerException|)))
      (error "internal error: no applicable method for invocation of ~A with arguments ~S" gf args)))

(defun |java/lang/Object.registerNatives()| ()
  ())

(defun |java/lang/ClassLoader.registerNatives()| ()
  ())

(defun |java/lang/System.registerNatives()| ()
  ())

(defun |java/lang/Class.registerNatives()| ()
  ())

(defun |java/lang/Class.desiredAssertionStatus0(Ljava/lang/Class;)| (class)
  nil)

(defun |java/lang/Class.getSecurityManager()| ()
  (classload "java/lang/SecurityManager")
  (eval (list 'make-instance (list 'quote '|java/lang/SecurityManager|))))

(defmethod |fillInStackTrace(I)| ((this |java/lang/Throwable|) dummy)
  (declare (ignore dummy))
  (setf (slot-value this '|backtrace|) (sb-debug:list-backtrace)))

(defmethod |getStackTraceDepth()| ((this |java/lang/Throwable|))
  (length (slot-value this '|backtrace|)))

(defun %caller-class-name-from-stack-frame (caller-list)
  (let ((caller-string (format nil "~A" caller-list)))
    (let ((dot-position (position #\. caller-string)))
      (cond
        ((str:starts-with? "(%clinit-" caller-string)
         (subseq caller-string 9 (1- (length caller-string))))
        ((str:starts-with? "((METHOD" caller-string)
         (format nil "~A" (type-of (cadr caller-list))))
        ((str:starts-with? "((LAMBDA " caller-string)
         (substitute #\/ #\. (lstring (slot-value (cadr caller-list) '|name|))))
        ((str:starts-with? "((LABELS CLINIT IN %CLINIT" caller-string)
         (name (cadr caller-list)))
        ((str:starts-with? "(%CLINIT " caller-string)
         (name (cadr caller-list)))
        (dot-position
         (subseq caller-string 1 dot-position))
        ;; FIXME: maybe use an OpenLDK internal class to indicate internal frame
        (t "java/lang/System")))))

(defmethod |getStackTraceElement(I)| ((this |java/lang/Throwable|) index)
  (let ((ste (make-instance '|java/lang/StackTraceElement|))
        (stack-frame (nth index (slot-value this '|backtrace|))))
    (|<init>(Ljava/lang/String;Ljava/lang/String;Ljava/lang/String;I)|
     ste
     (jstring (%caller-class-name-from-stack-frame stack-frame))
     (ijstring "unknown") (jstring (format nil "~A" stack-frame)) -1)
    ste))

(defun %remove-adjacent-repeats (list)
  "Remove all adjacent repeated objects from LIST."
  (let ((result nil)
        (last nil))
    (loop for item in list
          unless (equal item last)
            do (push item result)
          do (setf last item))
    (nreverse result)))

(defun %remove-elements-with-substrings (element-list substring-list)
  "Remove elements from ELEMENT-LIST whose string representation contains
any substring in SUBSTRING-LIST."
  (remove-if
   (lambda (elem)
     (let ((str-representation (format nil "~A" elem)))
       (some (lambda (substr)
               (search substr str-representation))
             substring-list)))
   element-list))

(defun %remove-invoke-frames (frames)
  "Remove any frames associated with java.lang.reflect.Method.invoke()
and its implementation."
  (%remove-elements-with-substrings
   frames
   '("sun/reflect/NativeMethodAccessorImpl.invoke0"
     "METHOD invoke"
     "#<java/lang/reflect/Method "
     "#<sun/reflect/NativeMethodAccessorImpl ")))

(defun |sun/reflect/Reflection.getCallerClass(I)| (index)
  ;; FIXME: we don't need the whole backtrace
  (let* ((backtrace (%remove-invoke-frames (%remove-adjacent-repeats (sb-debug:list-backtrace)))))
    (assert (stringp (%caller-class-name-from-stack-frame (nth index backtrace))))
    (%get-java-class-by-bin-name (%caller-class-name-from-stack-frame (nth index backtrace)))))

(defun |sun/reflect/Reflection.getCallerClass()| ()
  (|sun/reflect/Reflection.getCallerClass(I)| 3))

(defun %type-to-descriptor (type)
  (cond
    ((eq type 'double-float) "D")
    ((eq type 'single-float) "F")
    ((equal type '(signed-byte 8)) "B")
    ((equal type '(signed-byte 16)) "S")
    ((equal type '(signed-byte 32)) "I")
    ((equal type '(signed-byte 64)) "J")
    ((equal type 'standard-char) "C")
    ((equal type 'bit) "Z")
    ((stringp type) (if (eq 1 (length type)) type (format nil "L~A;" type)))
    (t (format nil "Ljava/lang/Object;" type))))

(defun %get-array-lclass-from-name (cname)
  (let* ((ldk-class (%get-ldk-class-by-bin-name cname t)))
    (if ldk-class
        ldk-class
        (let ((lclass (make-instance '<class>
                                     :name cname
                                     :super "java/lang/Object"))
              (java-class (make-instance '|java/lang/Class|)))
          (setf (slot-value java-class '|name|) (ijstring cname))
          (setf (slot-value java-class '|classLoader|) nil)
          (setf (slot-value lclass 'java-class) java-class)
          (setf (gethash cname *ldk-classes-by-bin-name*) lclass)
          (setf (gethash cname *java-classes-by-bin-name*) java-class)
          lclass))))

(defun %get-array-lclass (element-type)
  (let* ((cname (format nil "[~A" (%type-to-descriptor element-type))))
    (%get-array-lclass-from-name cname)))

(defmethod |getClass()| (object)
  ;;; FIXME - throw nullpointerexception
  (unwind-protect
       (progn
         (when *debug-trace*
           (format t "~&~V@A trace: java/lang/Object.getClass(~A)" (incf *call-nesting-level* 1) "*" object))
         (cond
           ((arrayp object) (java-class (%get-array-lclass (array-element-type object))))
           (t (let ((jc (%get-java-class-by-bin-name (format nil "~A" (type-of object)) t)))
                (or jc (|java/lang/Class.forName0(Ljava/lang/String;ZLjava/lang/ClassLoader;Ljava/lang/Class;)| (jstring (format nil "~A" (type-of object))) nil nil nil))))))
    (when *debug-trace*
      (incf *call-nesting-level* -1))))

(defmethod |java/lang/Class.forName0(Ljava/lang/String;ZLjava/lang/ClassLoader;Ljava/lang/Class;)| (name initialize loader caller)
  (unwind-protect
       (progn
         (when *debug-trace*
           (format t "~&~V@A trace: entering java/lang/Class.forName0(Ljava/lang/String;ZLjava/lang/ClassLoader;Ljava/lang/Class;) ~A~%"
                   (incf *call-nesting-level* 1) "*" name))
         (let ((lname (substitute #\/ #\. (lstring name))))
           (or (and (eq (char lname 0) #\[)
                    (or (%get-java-class-by-bin-name lname t)
                        (java-class (%get-array-ldk-class-from-name (subseq lname 1)))))
               (and (%get-ldk-class-by-bin-name lname t)
                    (java-class (%get-ldk-class-by-bin-name lname)))
               (let ((klass (classload lname)))
                 (when klass
                   (%clinit klass)
                   (java-class klass))))))
    (when *debug-trace*
      (incf *call-nesting-level* -1))))

(defmethod |java/lang/System.currentTimeMillis()| ()
	;;; FIXME: this probably isn't right.
  (floor (* (get-internal-real-time) 1000)
				 internal-time-units-per-second))

(defmethod |java/lang/System.arraycopy(Ljava/lang/Object;ILjava/lang/Object;II)| (source_arr sourcePos dest_arr destPos len)
	(dotimes (i len)
		(setf (aref dest_arr (+ destPos i)) (aref source_arr (+ sourcePos i)))))

(defmethod |run()| (arg)
  (declare (ignore arg))
  (error "internal error"))

(defmethod |java/security/AccessController.doPrivileged(Ljava/security/PrivilegedAction;)| (action)
  (|run()| action))

(defmethod |java/lang/Class.getPrimitiveClass(Ljava/lang/String;)| (class-name)
  (let ((name (slot-value class-name '|value|)))
    (%get-java-class-by-fq-name name)))

(defmethod |java/lang/Float.floatToRawIntBits(F)| (float)
  (float-features:single-float-bits float))

(defmethod |java/lang/Double.doubleToRawLongBits(D)| (double)
  (float-features:double-float-bits (coerce double 'double-float)))

(defmethod |java/lang/Double.longBitsToDouble(J)| (long-bits)
  (float-features:bits-double-float long-bits))

(defmethod |java/util/TimeZone.getSystemTimeZoneID(Ljava/lang/String;)| (arg)
  (jstring (local-time:format-timestring nil (local-time:now) :format '(:timezone))))

(defmethod |length()| ((str string))
  (length (lstring str)))

(defmethod |java/util/TimeZone.getSystemGMTOffsetID()| ()
  (jstring (local-time:format-timestring nil (local-time:now) :format '(:gmt-offset))))

(defvar field-offset-table (make-hash-table :test #'equal))

(defmethod |objectFieldOffset(Ljava/lang/reflect/Field;)| ((unsafe |sun/misc/Unsafe|) field)
  (declare (ignore unsafe))
  (let ((offset (sxhash field)))
    (setf (gethash offset field-offset-table) field)
    offset))

(defun |java/lang/Class$Atomic.objectFieldOffset([Ljava/lang/reflect/Field;Ljava/lang/String;)| (field name)
  ;; FIXME
  (error "ofo"))

(defmethod |staticFieldBase(Ljava/lang/reflect/Field;)| ((unsafe |sun/misc/Unsafe|) field)
  (declare (ignore unsafe))
  (declare (ignore field))
  nil)

(defmethod |staticFieldOffset(Ljava/lang/reflect/Field;)| ((unsafe |sun/misc/Unsafe|) field)
  (declare (ignore unsafe))
  (let ((offset (sxhash field)))
    (setf (gethash offset field-offset-table) field)
    offset))

(defun %stringize-array (array)
  "Convert an array of characters and integers (ASCII values) into a string."
  (coerce
   (map 'list
        (lambda (x)
          (if (integerp x)
              (code-char x) ;; Convert integer to character
              x))           ;; Keep character as is
        array)
   'string))

(defmethod print-object ((str |java/lang/String|) out)
  (print-unreadable-object (str out :type t)
    (format out "~S" (%stringize-array (slot-value str '|value|)))))

(defmethod print-object ((class |java/lang/Class|) out)
  (print-unreadable-object (class out :type t)
    (format out "~A" (slot-value class '|name|))))

(defmethod |java/lang/Thread.registerNatives()| ()
  ;; FIXME: What does this do??
  )

;;; The current |java/lang/Thread| object.
(defvar *current-thread* nil)

(defmethod |add(Ljava/lang/Thread;)| (thread-group thread)
  (declare (ignore thread-group))
  (declare (ignore thread))
  (error "internal error"))

(defmethod |java/lang/Thread.currentThread()| ()
  (or *current-thread*
      (let ((thread (make-instance '|java/lang/Thread|))
            (thread-group (make-instance '|java/lang/ThreadGroup|)))
        (|<init>()| thread-group)
        (setf *current-thread* thread)
        (setf (slot-value thread '|priority|) 1)
        (|add(Ljava/lang/Thread;)| thread-group thread)
        (|<init>(Ljava/lang/ThreadGroup;Ljava/lang/Runnable;Ljava/lang/String;J)| thread thread-group nil (jstring "Init-Thread") 0)
        thread)))

(defmethod |setPriority0(I)| ((thread |java/lang/Thread|) priority)
  ;; FIXME
  )

(defmethod |isAlive()| ((thread |java/lang/Thread|))
  ;; FIXME
  0)

(defmethod |isInterrupted(Z)| ((thread |java/lang/Thread|) x)
  ;; FIXME
  0)

(defmethod |start0()| ((thread |java/lang/Thread|))
  ;; FIXME
  )

(defun |sun/misc/Unsafe.registerNatives()| ()
  ;; FIXME
  )

(defmethod |hashCode()| (obj)
  (|java/lang/System.identityHashCode(Ljava/lang/Object;)| obj))

(defclass %array-base-offset ()
  ((array :initarg :array)))

(defmethod |arrayBaseOffset(Ljava/lang/Class;)| ((unsafe |sun/misc/Unsafe|) array)
  0)
;  (make-instance '%array-base-offset :array array))

(defmethod |arrayIndexScale(Ljava/lang/Class;)| ((unsafe |sun/misc/Unsafe|) array)
  1)

(defmethod |addressSize()| ((unsafe |sun/misc/Unsafe|))
  ;; FIXME
  997)

(defmethod |availableProcessors()| ((runtime |java/lang/Runtime|))
  ;; FIXME
  1)

(defmethod |isArray()| ((class |java/lang/Class|))
  ;; FIXME
  (let ((name-string (format nil "~A" (slot-value (slot-value class '|name|) '|value|))))
    (if (string= name-string "java/util/Arrays")
        1
        0)))

(defmethod |getComponentType()| ((class |java/lang/Class|))
  ;; FIXME
  (java-class (gethash "java/lang/Object" *ldk-classes-by-bin-name*)))

(defmethod |isPrimitive()| ((class |java/lang/Class|))
  (let ((name-string (format nil "~A" (slot-value (slot-value class '|name|) '|value|))))
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

(defmethod |isInterface()| ((this |java/lang/Class|))
  (if (and (eq 0 (|isPrimitive()| this))
           (let ((lclass (%get-ldk-class-by-fq-name (slot-value (slot-value this '|name|) '|value|))))
             (interface-p lclass)))
      1
      0))

(defclass/std <constant-pool> (|java/lang/Object|)
  ((ldk-class)))

(defmethod |getConstantPool()| ((this |java/lang/Class|))
  (unless (%get-ldk-class-by-fq-name "sun.reflect.ConstantPool" t)
    (%clinit (classload "sun/reflect/ConstantPool")))
  (let ((ldk-class (%get-ldk-class-by-fq-name (lstring (slot-value this '|name|)))))
    (let ((cp (make-instance '|sun/reflect/ConstantPool|)))
      (setf (slot-value cp '|constantPoolOop|)
            (make-instance '<constant-pool> :ldk-class ldk-class))
      cp)))

(defmethod |getDeclaredConstructors0(Z)| ((this |java/lang/Class|) arg)
  ;; FIXME
  (unwind-protect
       (progn
         (when *debug-trace*
           (format t "~&~V@A trace: entering java/lang/Class.getDeclaredConstructors0(Z)~%" (incf *call-nesting-level* 1) "*"))
         (unless (%get-ldk-class-by-bin-name "java/lang/reflect/Constructor")
           (|java/lang/Class.forName0(Ljava/lang/String;ZLjava/lang/ClassLoader;Ljava/lang/Class;)| (jstring "java/lang/reflect/Constructor") nil nil nil))

         ;; Get the lclass for THIS
         (let ((lclass (%get-ldk-class-by-fq-name (slot-value (slot-value this '|name|) '|value|))))
           (coerce (append (loop for method across (methods lclass)
                                 when (str:starts-with? "<init>" (name method))
                                   collect (let ((c (make-instance '|java/lang/reflect/Constructor|)))
                                             (|<init>(Ljava/lang/Class;[Ljava/lang/Class;[Ljava/lang/Class;IILjava/lang/String;[B[B)| c this (%get-parameter-types (descriptor method)) (make-array 0) (access-flags method) 0 (ijstring (descriptor method)) (make-array 0) (make-array 0))
                                             c)))
                   'vector)))
    (when *debug-trace*
      (incf *call-nesting-level* -1))))

(defmethod |getDeclaredClasses0()| ((this |java/lang/Class|))
  (let ((lclass (%get-ldk-class-by-fq-name (slot-value (slot-value this '|name|) '|value|))))
    (let ((java-classes (mapcar (lambda (name)
                                  (%get-java-class-by-bin-name name))
                                (inner-classes lclass))))
    (coerce java-classes 'vector))))

(defmethod |getDeclaredMethods0(Z)| ((this |java/lang/Class|) arg)
  ;; FIXME
  (unwind-protect
       (progn
         (when *debug-trace*
           (format t "~&~V@A trace: entering java/lang/Class.getDeclaredMethods(Z)~%" (incf *call-nesting-level* 1) "*"))
         (unless (gethash "java/lang/reflect/Method" *ldk-classes-by-bin-name*)
           (|java/lang/Class.forName0(Ljava/lang/String;ZLjava/lang/ClassLoader;Ljava/lang/Class;)| (jstring "java/lang/reflect/Method") nil nil nil))

         ;; Get the lclass for THIS
         (let ((lclass (%get-ldk-class-by-fq-name (slot-value (slot-value this '|name|) '|value|))))
           (coerce (append (loop for method across (methods lclass)
                                 unless (str:starts-with? "<init>" (name method))
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
                                   collect (let ((c (make-instance '|java/lang/reflect/Method|)))
                                             (|<init>(Ljava/lang/Class;Ljava/lang/String;[Ljava/lang/Class;Ljava/lang/Class;[Ljava/lang/Class;IILjava/lang/String;[B[B[B)|
                                              c this (ijstring (name method))
                                              (%get-parameter-types (descriptor method))
                                              (%get-return-type (descriptor method))
                                              (make-array 0) (access-flags method) 0 (ijstring (descriptor method)) (make-array 0) (make-array 0) (make-array 0))
                                             c)))
                   'vector)))
    (when *debug-trace*
      (incf *call-nesting-level* -1))))


(defmethod |getDeclaredFields0(Z)| ((this |java/lang/Class|) arg)
  (unwind-protect
       (progn
         (when *debug-trace*
           (format t "~&~V@A trace: entering java/lang/Class.getDeclaredFields0(Z)~%" (incf *call-nesting-level* 1) "*"))
         (unless (gethash "java/lang/reflect/Field" *ldk-classes-by-bin-name* t)
           (|java/lang/Class.forName0(Ljava/lang/String;ZLjava/lang/ClassLoader;Ljava/lang/Class;)| (jstring "java/lang/reflect/Field") nil nil nil))

         ;; Get the lclass for THIS
         (let ((lclass (%get-ldk-class-by-fq-name (slot-value (slot-value this '|name|) '|value|))))
           (labels ((get-fields (lclass)
                      (when lclass
                        (append (loop for field across (fields lclass)
                                      collect (let ((f (make-instance '|java/lang/reflect/Field|)))
                                                (|<init>(Ljava/lang/Class;Ljava/lang/String;Ljava/lang/Class;IILjava/lang/String;[B)|
                                                 f this (ijstring (name field))
                                                 (let ((cn (slot-value field 'descriptor)))
                                                   (when (eq (char cn 0) #\L)
                                                     (setf cn (subseq cn 1 (1- (length cn)))))
                                                   (or (%get-java-class-by-bin-name cn t)
                                                       (let ((njc (make-instance '|java/lang/Class|)))
                                                         (setf (slot-value njc '|name|) (ijstring cn))
                                                         (setf (gethash (substitute #\. #\/ cn) *java-classes-by-fq-name*) njc)
                                                         (setf (gethash cn *java-classes-by-bin-name*) njc))))
                                                 (access-flags field) nil nil nil)
                                                f))
                                (when (super lclass)
                                  (get-fields (%get-ldk-class-by-bin-name (super lclass) t)))))))
             (coerce (get-fields lclass) 'vector))))
    (when *debug-trace*
      (incf *call-nesting-level* -1))))

(defun |sun/misc/VM.initialize()| ()
  ;; FIXME
  )

(defmethod |compareAndSwapObject(Ljava/lang/Object;JLjava/lang/Object;Ljava/lang/Object;)| ((unsafe |sun/misc/Unsafe|) obj field-id expected-value new-value)
  ;; FIXME
  (cond
    ((vectorp obj)
     (if (equal (aref obj field-id) expected-value)
         (progn
           (setf (aref obj field-id) new-value)
           1)
         0))
    (t
     (let* ((field (gethash field-id field-offset-table))
            (key (intern (slot-value (slot-value field '|name|) '|value|) :openldk)))
       (if (equal (slot-value obj key) expected-value)
           (progn
             (setf (slot-value obj key) new-value)
             1)
           0)))))

(defmethod |compareAndSwapInt(Ljava/lang/Object;JII)| ((unsafe |sun/misc/Unsafe|) obj field-id expected-value new-value)
  ;; FIXME: use atomics package
  (let* ((field (gethash field-id field-offset-table))
         (key (intern (slot-value (slot-value field '|name|) '|value|) :openldk)))
    (if (equal (slot-value obj key) expected-value)
        (progn
          (setf (slot-value obj key) new-value)
          1)
        0)))

(defmethod |compareAndSwapLong(Ljava/lang/Object;JJJ)| ((unsafe |sun/misc/Unsafe|) obj field-id expected-value new-value)
  (|compareAndSwapInt(Ljava/lang/Object;JII)| unsafe obj field-id expected-value new-value))

(defmethod |getObjectVolatile(Ljava/lang/Object;J)| ((unsafe |sun/misc/Unsafe|) obj l)
  ;; FIXME
  (cond
    ((vectorp obj)
     (aref obj l))
    (t (error "internal error: unrecognized object type in getObjectVolatile: ~A" obj))))

(defmethod |putObjectVolatile(Ljava/lang/Object;JLjava/lang/Object;)| ((unsafe |sun/misc/Unsafe|) obj l value)
  ;; FIXME
  (cond
    ((vectorp obj)
     (setf (aref obj l) value))
    (t (error "internal error: unrecognized object type in putObjectVolatile: ~A" obj))))

(defmethod |getLongVolatile(Ljava/lang/Object;J)| ((unsafe |sun/misc/Unsafe|) obj l)
  (cond
    ((vectorp obj)
     (aref obj l))
    ((typep obj '|java/lang/Object|)
     (let* ((field (gethash l field-offset-table))
            (key (intern (slot-value (slot-value field '|name|) '|value|) :openldk)))
       (slot-value obj key)))
    (t (error "internal error: unrecognized object type in getLongVolatile: ~A" obj))))

(defmethod |putObject(Ljava/lang/Object;JLjava/lang/Object;)| ((unsafe |sun/misc/Unsafe|) obj l value)
  ; (format t "putObject: ~A ~A ~A~%" obj (gethash l field-offset-table) value)
  (cond
    ((vectorp obj)
     (setf (aref obj l) value))
    ((typep obj '|java/lang/Object|)
     (let* ((field (gethash l field-offset-table))
            (key (intern (slot-value (slot-value field '|name|) '|value|) :openldk)))
       (setf (slot-value obj key) value)))
    (t (error "internal error: unrecognized object type in putObjectVolatile: ~A" obj))))

(defun |java/security/AccessController.getStackAccessControlContext()| ()
  ;; FIXME -- implement
  nil)

(defun |java/lang/System.initProperties(Ljava/util/Properties;)| (props)
  (dolist (prop `(("java.specification.version" . "1.8")
                  ("java.specification.name" . "Java Platform API Specification")
                  ("java.specification.vendor" . "Oracle Corporation")
                  ("java.version" . "8.0")
                  ("java.vendor" . "OpenLDK")
                  ("java.vendor.url" . "https://github.com/atgreen/openldk")
                  ("java.vendor.url.bug" . "https://github.com/atgreen/openldk/issues")
                  ("java.class.version" . "52.0")
                  ("sun.cds.enableSharedLookupCache" . "1")
                  ("java.class.path" . ,(or (uiop:getenv "LDK_CLASSPATH")
                                            (uiop:getenv "CLASSPATH")
                                            "."))
                  ("sun.boot.class.path" .
                                         ,(format nil "~{~A~^:~}"
                                                  (mapcar #'namestring
                                                          (directory
                                                           (concatenate 'string
                                                                        (uiop:getenv "JAVA_HOME")
                                                                        "/lib/*.jar")))))
                  ("java.home" . ,(uiop:getenv "JAVA_HOME"))
                  ("user.home" . ,(uiop:getenv "HOME"))
                  ("user.dir" . ,(namestring (uiop:getcwd)))
                  ("user.name" . ,(let ((uid (sb-posix:getuid)))
                                    (slot-value (sb-posix:getpwuid uid) 'sb-posix::name)))
                  ("os.name" . ,(cond
                                  ((find :LINUX *features*)
                                   "Linux")
                                  (t (error "internal error"))))
                  ("os.version" . ,(if (find :LINUX *features*)
                                       (with-open-file (stream "/proc/version" :direction :input)
                                         (let ((line (read-line stream)))
                                           (let* ((version-start (+ (search "Linux version " line)
                                                                    (length "Linux version ")))
                                                  (space-pos (position #\Space line :start version-start))
                                                  (version (subseq line version-start space-pos)))
                                             version)))
                                       (error "internal error")))
                  ("os.arch" . ,(cond
                                  ((find :X86-64 *features*)
                                   "amd64")
                                  (t (error "internal error"))))
                  ("sun.jnu.encoding" . "UTF-8")
                  ("sun.cpu.endian" . ,(cond
                                         ((find :LITTLE-ENDIAN *features*)
                                          "little")
                                         ((find :BIG-ENDIAN *features*)
                                          "big")
                                         (t (error "internal error"))))
                  ("file.separator" . "/")
                  ("file.encoding.pkg" . "sun.io")
                  ("java.io.tmpdir" . ,(namestring (uiop:temporary-directory)))
                  ("file.encoding" . "UTF-8")
                  ("path.separator" . ":")
                  ("java.library.path" . ,(concatenate 'string
                                                       (uiop:getenv "JAVA_HOME")
                                                       "/lib/"))
                  ("java.security.debug" . "0")
                  ("line.separator" . ,(format nil "~%"))))
    (|java/lang/System.setProperty(Ljava/lang/String;Ljava/lang/String;)| (ijstring (car prop)) (ijstring (cdr prop))))
  props)

#|
Need to add:

java.awt.printerjob
sun.arch.data.model
sun.awt.graphicsenv
sun.cpu.isalist
sun.desktop
sun.io.unicode.encoding
sun.java2d.fontpath
sun.jnu.encoding
sun.os.patch.level
sun.stderr.encoding
sun.stdout.encoding
user.country
user.language
user.script
user.timezone
user.variant

|#

(defun |java/io/FileDescriptor.initIDs()| ()
  )

(defun |sun/nio/ch/IOUtil.initIDs()| ()
  )

(defmethod |run()| (arg)
  (declare (ignore arg))
  (error "internal error"))

(defun |java/security/AccessController.doPrivileged(Ljava/security/PrivilegedExceptionAction;)| (action)
  (let ((result (|run()| action)))
    result))

(defun |java/security/AccessController.doPrivileged(Ljava/security/PrivilegedExceptionAction;Ljava/security/AccessControlContext;)| (action context)
  ;; FIXME
  (|run()| action))

(defmethod |isAssignableFrom(Ljava/lang/Class;)| ((this |java/lang/Class|) other)
  (let ((this-class (find-class (intern (name (%get-ldk-class-by-fq-name (slot-value (slot-value this '|name|) '|value|))) :openldk)))
        (other-class (find-class (intern (name (%get-ldk-class-by-fq-name (slot-value (slot-value other '|name|) '|value|))) :openldk))))
    (closer-mop:subclassp this-class other-class)))

(defun |java/lang/System.setIn0(Ljava/io/InputStream;)| (in-stream)
  (setf (slot-value |+static-java/lang/System+| '|in|) in-stream))

(defun |java/lang/System.setErr0(Ljava/io/PrintStream;)| (print-stream)
  (setf (slot-value |+static-java/lang/System+| '|err|) print-stream))

(defun |java/lang/System.setOut0(Ljava/io/PrintStream;)| (print-stream)
  (setf (slot-value |+static-java/lang/System+| '|out|) print-stream))

(defmethod |getIntVolatile(Ljava/lang/Object;J)| ((unsafe |sun/misc/Unsafe|) param-object param-long)
  (cond
    ((vectorp param-object)
     (aref param-object param-long))
    (t
     (let* ((field (gethash param-long field-offset-table))
            (key (intern (slot-value (slot-value field '|name|) '|value|) :openldk)))
       (slot-value param-object key)))))

(defmethod |clone()| ((array vector))
  (copy-seq array))

(defun |sun/reflect/Reflection.getClassAccessFlags(Ljava/lang/Class;)| (class)
  (let ((lclass (%get-ldk-class-by-fq-name (slot-value (slot-value class '|name|) '|value|))))
    (access-flags lclass)))

(defmethod |getModifiers()| ((class |java/lang/Class|))
  (let ((lclass (%get-ldk-class-by-fq-name (slot-value (slot-value class '|name|) '|value|))))
    (access-flags lclass)))

(defmethod |getSuperclass()| ((class |java/lang/Class|))
  (let ((lclass (%get-ldk-class-by-fq-name (slot-value (slot-value class '|name|) '|value|))))
    (gethash (super lclass) *java-classes-by-bin-name*)))

(defmethod |getInterfaces0()| ((class |java/lang/Class|))
  ;; FIXME: do something different for interfaces?
  (let ((lclass (%get-ldk-class-by-fq-name (slot-value (slot-value class '|name|) '|value|))))
    (coerce (mapcar (lambda (iname) (java-class (gethash iname *ldk-classes-by-bin-name*))) (coerce (interfaces lclass) 'list))
            'vector)))

(defun |sun/reflect/NativeConstructorAccessorImpl.newInstance0(Ljava/lang/reflect/Constructor;[Ljava/lang/Object;)|
    (constructor params)
  (let ((bin-class-name (substitute #\/ #\. (lstring (slot-value (slot-value constructor '|clazz|) '|name|)))))
    (|java/lang/Class.forName0(Ljava/lang/String;ZLjava/lang/ClassLoader;Ljava/lang/Class;)| (jstring bin-class-name) nil nil nil)
    (let ((obj (make-instance (intern bin-class-name :openldk))))
      (if (string= "()V" (slot-value (slot-value constructor '|signature|) '|value|))
          (|<init>()| obj)
          (error "unimplemented"))
      obj)))

(defmethod |ensureClassInitialized(Ljava/lang/Class;)| ((unsafe |sun/misc/Unsafe|) class)
  (let ((lclass (%get-ldk-class-by-fq-name (slot-value (slot-value class '|name|) '|value|))))
    (assert lclass)
    (%clinit lclass)))

(defvar %unsafe-memory-table (make-hash-table))

(defmethod |allocateMemory(J)| ((unsafe |sun/misc/Unsafe|) size)
  (let* ((mem (sb-alien:make-alien sb-alien:char size))
         (ptr (sb-sys:sap-int (sb-alien:alien-sap mem))))
    (setf (gethash ptr %unsafe-memory-table) mem)
    ptr))

(defmethod |putLong(JJ)| ((unsafe |sun/misc/Unsafe|) address value)
  (setf (sb-sys:sap-ref-64 (sb-sys:int-sap address) 0) value))

(defmethod |getByte(J)| ((unsafe |sun/misc/Unsafe|) address)
  (sb-sys:sap-ref-8 (sb-sys:int-sap address) 0))

(defmethod |freeMemory(J)| ((unsafe |sun/misc/Unsafe|) address)
  (sb-alien:free-alien (gethash address %unsafe-memory-table)))

(defun |java/lang/System.mapLibraryName(Ljava/lang/String;)| (library-name)
  #+LINUX (jstring (format nil "lib~A.so" (lstring library-name)))
  #-LINUX (error "unimplemented"))

(defun |java/lang/ClassLoader.findBuiltinLib(Ljava/lang/String;)| (library-name)
  ;; FIXME
  library-name)

(defmethod |load(Ljava/lang/String;Z)| ((loader t) library-name is-builtin)
  (when *debug-trace*
    (format t "FIXME: ~A loading ~A~%" loader library-name))
  (setf (slot-value loader '|loaded|) 1)
  )

(defmethod |sun/misc/Signal.findSignal(Ljava/lang/String;)| (signal-name)
  (let ((sname (lstring signal-name)))
    (cond
      ((string= sname "HUP") 1)
      ((string= sname "INT") 2)
      ((string= sname "KILL") 9)
      ((string= sname "TERM") 15)
      (t (error "unimplemented")))))

(defun |sun/misc/Signal.handle0(IJ)| (sig native-h)
  ;; FIXME
  1)

(defmethod |notifyAll()| ((objref |java/lang/Object|))
  ;; FIXME
  )

(defun |sun/misc/URLClassPath.getLookupCacheURLs(Ljava/lang/ClassLoader;)| (class-loader)
  (declare (ignore class-loader))
  ;; FIXME
  nil)

(defmethod |open0(Ljava/lang/String;I)| ((fis |java/io/RandomAccessFile|) filename mode)
  (handler-case
      (setf (slot-value fis '|fd|) (open (lstring filename)
                                         :element-type '(unsigned-byte 8)
                                         :direction (ecase mode
                                                      (1 :input)
                                                      (2 :io))))
    ((or sb-ext:file-does-not-exist sb-int:simple-file-error) (e)
      (declare (ignore e))
      (let ((fnf (make-instance '|java/io/FileNotFoundException|)))
        (|<init>(Ljava/lang/String;)| fnf filename)
        (error (%lisp-condition fnf))))))

(defmethod |open0(Ljava/lang/String;)| ((fis |java/io/FileInputStream|) filename)
  (handler-case
      (setf (slot-value fis '|fd|) (open (lstring filename)
                                         :element-type '(unsigned-byte 8)
                                         :direction :input))
    ((or sb-ext:file-does-not-exist sb-int:simple-file-error) (e)
      (declare (ignore e))
      (let ((fnf (make-instance '|java/io/FileNotFoundException|)))
        (|<init>(Ljava/lang/String;)| fnf filename)
        (error (%lisp-condition fnf))))))

(defmethod |skip0(J)| ((fis |java/io/FileInputStream|) n)
  (let ((in-stream (slot-value fis '|fd|))
        (bytes-read 0))
    (format t "SKIP0 ~A ~A~%" fis n)
    (when (eq n :END)
      (setf n 999999999999))
    (loop for i from 0 below n
          for byte = (read-byte in-stream nil nil) ; Read a byte, return NIL on EOF
          while byte
          do (incf bytes-read))  ; Count bytes read
    bytes-read))

(defmethod |readBytes([BII)| ((fis |java/io/FileInputStream|) byte-array offset length)
  (let ((in-stream (slot-value fis '|fd|))
        (bytes-read 0))
    (loop for i from offset below (+ offset length)
          for byte = (read-byte in-stream nil nil) ; Read a byte, return NIL on EOF
          while byte
          do (setf (aref byte-array i) byte)
             (incf bytes-read))  ; Count bytes read
    bytes-read))

(defmethod |available0()| ((fis |java/io/FileInputStream|))
  ;; FIXME - may throw exception
  (let* ((in-stream (slot-value fis '|fd|))
         (remaining (- (file-length in-stream) (file-position in-stream))))
    remaining))

(defmethod |isInstance(Ljava/lang/Object;)| ((this |java/lang/Class|) objref)
  (if (typep objref (intern (substitute #\/ #\. (lstring (slot-value this '|name|))) :openldk)) 1 0))

(defmethod |closeAll(Ljava/io/Closeable;)| ((stream stream) closeable)
  (close stream))

(defun %convert-to-unsigned-8-bit (signed-array)
  "Convert a signed 8-bit array to an unsigned 8-bit array."
  (let* ((dimensions (array-dimensions signed-array))
         (unsigned-array (make-array dimensions :element-type '(unsigned-byte 8))))
    (dotimes (i (array-total-size signed-array))
      (setf (row-major-aref unsigned-array i)
            (logand (row-major-aref signed-array i) #xFF)))
    unsigned-array))

(defmethod |writeBytes([BIIZ)| ((fos |java/io/FileOutputStream|) byte-array offset length append?)
  (declare (ignore append?))
  ;;  (format t "~&WRITE-BYTES: 1:~A 2:~A 3:~A 4:~A 5:~A 6:~A 7:~A~%" fos (slot-value fos '|fd|) (slot-value (slot-value fos '|fd|) '|fd|) byte-array offset length append?)
  (let* ((file-descriptor (slot-value fos '|fd|))
         (fd (slot-value file-descriptor '|fd|)))
    (cond
      ((eq fd 1)
       (write-sequence (%convert-to-unsigned-8-bit byte-array) *standard-output* :start offset :end (+ offset length)))
      ((eq fd 2)
       (write-sequence (%convert-to-unsigned-8-bit byte-array) *error-output* :start offset :end (+ offset length)))
      (t
       (error "unimplemented")))))

(defmethod |getEnclosingMethod0()| ((this |java/lang/Class|))
  ;; FIXME
  nil)

(defmethod |getDeclaringClass0()| ((this |java/lang/Class|))
  ;; FIXME
  nil)

(defmethod |getBooleanAttributes0(Ljava/io/File;)| ((this |java/io/UnixFileSystem|) file)
  (handler-case
      (let ((attr (org.shirakumo.file-attributes:decode-attributes
                   (org.shirakumo.file-attributes:attributes (lstring (slot-value file '|path|))))))
        (+ #x01 ;; :EXISTS
           (if (getf attr :NORMAL) #x02 #x00)
           (if (getf attr :DIRECTORY) #x04 #x00)))
    (sb-int:simple-file-error (e)
      0)))

(defmethod |getLength(Ljava/io/File;)| ((this |java/io/UnixFileSystem|) file)
  (with-open-file (stream (lstring (slot-value file '|path|)) :element-type '(unsigned-byte 8))
    (file-length stream)))

(defun |java/security/AccessController.doPrivileged(Ljava/security/PrivilegedAction;Ljava/security/AccessControlContext;)| (action context)
  (declare (ignore context))
  ;; FIXME
  (let ((result (|run()| action)))
    result))

(defun |java/net/InetAddress.init()| ()
  ;; FIXME
  )

(defun |java/io/ObjectStreamClass.initNative()| ()
  ;; FIXME
  )

(defmethod |java/lang/System.gc()| ()
  (trivial-garbage:gc))

(defun |java/lang/Thread.sleep(J)| (milliseconds)
  (sleep (/ milliseconds 1000.0)))

(defun |java/lang/ProcessEnvironment.environ()| ()
  ;; FIXME: don't force utf-8 encoding
  (let ((env (remove-if (lambda (e) (not (find #\= e))) (sb-ext:posix-environ))))
    (let ((jenvs (make-array (* 2 (length env)))))
      (loop for kv in env
            for i from 0 by 2
            for p = (position #\= kv)
            do (progn
                 (setf (aref jenvs i)
                       (flexi-streams:string-to-octets (subseq kv 0 p) :external-format :utf-8))
                 (setf (aref jenvs (+ i 1))
                       (flexi-streams:string-to-octets (subseq kv (1+ p)) :external-format :utf-8))))
      jenvs)))

(defun |java/lang/System.nanoTime()| ()
  ;; Do some more math if this is not true.
  (assert (eq org.shirakumo.precise-time:precise-time-units-per-second
              1000000000))
  (multiple-value-bind (universal-time nanoseconds)
      (org.shirakumo.precise-time:get-precise-time)
    (+ (* universal-time 1000000000) nanoseconds)))

(defun |java/lang/System.identityHashCode(Ljava/lang/Object;)| (objref)
  ;; Hash down the 64-bit SXHASH to 32-bits.
  (unsigned-to-signed-integer (cl-murmurhash:murmurhash (sxhash objref))))

(defun |java/lang/Thread.yield()| ()
  ;; FIXME
  )

(defmethod |notify()| ((objref |java/lang/Object|))
  ;; FIXME
  )

(defun |java/util/concurrent/atomic/AtomicLong.VMSupportsCS8()| ()
  0)

(defun |sun/reflect/NativeMethodAccessorImpl.invoke0(Ljava/lang/reflect/Method;Ljava/lang/Object;[Ljava/lang/Object;)| (method object args)
  (when *debug-trace*
    (format t "~&~V@A trace: entering sun/reflect/NativeMethodAccessorImpl.invoke0(Ljava/lang/reflect/Method;Ljava/lang/Object;[Ljava/lang/Object;)~A~%"
            (incf *call-nesting-level* 1) "*"
            (list method object args)))
  (unwind-protect
       (progn
         (dotimes (i (length args))
           (when (typep (aref args i) '|java/lang/Integer|)
             (setf (aref args i) (slot-value (aref args i) '|value|))))
         (let ((result (apply (intern
                               (lispize-method-name
                                (concatenate 'string
                                             (substitute #\/ #\. (lstring (slot-value (slot-value method '|clazz|) '|name|)))
                                             "."
                                             (lstring (slot-value method '|name|))
                                             (lstring (slot-value method '|signature|))))
                               :openldk)
                              (if (eq 0 (logand #x8 (slot-value method '|modifiers|)))
                                  (cons object (coerce args 'list)) ; non-static method
                                  (coerce args 'list))))) ; static method
           (when *debug-trace*
             (format t "~&~V@A trace: result = ~A~%"
                     *call-nesting-level* "*" result))
           result))
    (when *debug-trace*
      (incf *call-nesting-level* -1))))

(defun |java/lang/reflect/Array.newArray(Ljava/lang/Class;I)| (class size)
  ;; FIXME
  (make-array size
              :initial-element nil))

(defmethod |findLoadedClass0(Ljava/lang/String;)| ((loader |java/lang/ClassLoader|) name)
  ;; FIXME
  (gethash (slot-value name '|value|) *java-classes-by-fq-name*))

(defmethod |findBootstrapClass(Ljava/lang/String;)| ((loader |java/lang/ClassLoader|) name)
  ;; FIXME
  (handler-case
      (let ((ldk-class (classload (substitute #\/ #\. (coerce (slot-value name '|value|) 'string)))))
        (java-class ldk-class))
    (condition (c)
      nil)))

(defun |java/io/UnixFileSystem.initIDs()| ()
  ;; FIXME
  )

(defun |java/util/LinkedHashMap.hash(Ljava/lang/Object;)| (obj)
  ;; FIXME: the compiler should not generate calls to LinkedHashMap.hash.
  ;; It's provided by the parent class.
  ;; This is a temp workaround.
  (|java/util/HashMap.hash(Ljava/lang/Object;)| obj))

(defmethod |canonicalize0(Ljava/lang/String;)| ((ufs |java/io/UnixFileSystem|) filename)
  (declare (ignore ufs))
  (jstring (namestring (uiop:parse-unix-namestring (coerce (slot-value filename '|value|) 'string)))))

(defmethod |getLastModifiedTime(Ljava/io/File;)| ((ufs |java/io/UnixFileSystem|) file)
  (declare (ignore ufs))
  (* (org.shirakumo.file-attributes:modification-time
      (lstring (|getName()| file)))
     1000))

(defun |sun/misc/Perf.registerNatives()| ()
  ;; FIXME
  )

(defmethod |createLong(Ljava/lang/String;IIJ)| (perf name variability units value)
  (classload "java/nio/DirectByteBuffer")
  (let* ((dbb (make-instance '|java/nio/DirectByteBuffer|))
         (mem (sb-alien:make-alien sb-alien:long 1))
         (ptr (sb-sys:sap-int (sb-alien:alien-sap mem))))
    (setf (sb-alien:deref mem 0) value)
    (|<init>(JI)| dbb ptr 8)
    dbb))

(defmethod |getLong(J)| ((unsafe |sun/misc/Unsafe|) ptr)
  (declare (ignore unsafe))
  ;; Convert the integer pointer back to a system address pointer (SAP)
  (let ((sap (sb-sys:int-sap ptr)))
    ;; Dereference the memory to get the long value
    (sb-alien:with-alien ((mem (* sb-alien:long) sap))
      (sb-alien:deref mem 0))))

(defmethod |getLongVolatile(J)| ((unsafe |sun/misc/Unsafe|) ptr)
  ;; FIXME: how is the volatile version different?
  (declare (ignore unsafe))
  ;; Convert the integer pointer back to a system address pointer (SAP)
  (let ((sap (sb-sys:int-sap ptr)))
    ;; Dereference the memory to get the long value
    (sb-alien:with-alien ((mem (* sb-alien:long) sap))
      (sb-alien:deref mem 0))))

(defmethod |setMemory(Ljava/lang/Object;JJB)| ((unsafe |sun/misc/Unsafe|) obj ptr size byte)
  (assert (null obj))
  (let ((sap (sb-sys:int-sap ptr)))
    ;; Loop through the memory range and set each byte to the specified value
    (dotimes (i size)
      ;; Calculate the offset for the current byte
      (let ((offset-sap (sb-sys:sap+ sap i)))
        ;; Set the byte at the current offset
        (setf (sb-sys:sap-ref-8 offset-sap 0) byte)))))

(defun |java/lang/Shutdown.beforeHalt()| ()
  ;; FIXME
  )

(defun |java/lang/Shutdown.halt0(I)| (status)
  (uiop:quit status t))

(defmethod |getRawAnnotations()| ((class |java/lang/Class|))
  (let ((lclass (%get-ldk-class-by-fq-name (slot-value (slot-value class '|name|) '|value|))))
    (when (and lclass (attributes lclass))
      (gethash "RuntimeVisibleAnnotations" (attributes lclass)))))

(defun |java/awt/image/ColorModel.initIDs()| ()
  ;; FIXME
  )

(defun |java/net/InetAddressImplFactory.isIPv6Supported()| ()
  0)

(defun |java/awt/image/IndexColorModel.initIDs()| ()
  ;; FIXME
  )

(defun |java/awt/image/Raster.initIDs()| ()
  ;; FIXME
  )

(defun |java/awt/image/SampleModel.initIDs()| ()
  ;; FIXME
  )

(defun |java/util/zip/Deflater.initIDs()| ()
  ;; FIXME
  )

(defun |java/util/zip/Inflater.initIDs()| ()
  ;; FIXME
  )

(defun |java/security/SystemConfigurator.getSystemFIPSEnabled()| ( )
  0)

(defun |java/lang/Package.getSystemPackage0(Ljava/lang/String;)| (name)
  (gethash (lstring name) *packages*))

(defun |java/util/zip/Inflater.init(Z)| (v)
  ;; FIXME
  )

(defun |java/lang/Thread.holdsLock(Ljava/lang/Object;)| (objref)
  (let ((monitor (%get-monitor objref))
        (current-thread (bordeaux-threads:current-thread)))
    (if (eq (owner monitor) current-thread)
        1 0)))

(defun |sun/nio/ch/IOUtil.iovMax()| ()
  0)

(defun |sun/nio/ch/FileChannelImpl.initIDs()| ()
  )

(defun |sun/nio/ch/NativeThread.init()| ()
  )

(defun |sun/nio/ch/NativeThread.current()| ()
  -1)

(defun |java/nio/Bits.pageSize()| ()
  4096)

(defmethod |pageSize()| ((unsafe |sun/misc/Unsafe|))
  4096)

(defun |sun/nio/ch/FileDispatcherImpl.init()| ()
  ;; FIXME
  )

(defun |sun/nio/ch/FileDispatcherImpl.read0(Ljava/io/FileDescriptor;JI)| (fd ptr length)
  (let ((in-stream fd)
        (bytes-read 0)
        (sap (sb-sys:int-sap ptr)))
    (loop for i below length
          for byte = (read-byte in-stream nil nil) ; Read a byte, return NIL on EOF
          while byte
          do (let ((offset-sap (sb-sys:sap+ sap i)))
               (setf (sb-sys:sap-ref-8 offset-sap 0) byte)
               (incf bytes-read)))  ; Count bytes read
    bytes-read))

(defun |java/nio/MappedByteBuffer.checkBounds(III)| (off len size)
  ;; FIXME
  )

(defmethod |copyMemory(Ljava/lang/Object;JLjava/lang/Object;JJ)| ((unsafe |sun/misc/Unsafe|) source source-offset dest dest-offset length)
  (assert (null source))
  (let ((sap (sb-alien:alien-sap (gethash source-offset %unsafe-memory-table)))
        (bytes-read 0))
    (loop for i below length
          do (let ((offset-sap (sb-sys:sap+ sap i)))
               (setf (aref dest (+ dest-offset i)) (sb-sys:sap-ref-8 offset-sap 0))
               (incf bytes-read)))  ; Count bytes read
    bytes-read))

(defun |sun/nio/fs/UnixNativeDispatcher.init()| ()
  ;; FIXME
  )

(defun |sun/nio/fs/UnixNativeDispatcher.getcwd()| ()
  (flexi-streams:string-to-octets (namestring (uiop:getcwd)) :external-format :utf-8))

(defmethod |getUTF8At0(Ljava/lang/Object;I)| ((this |sun/reflect/ConstantPool|) cp index)
  (format t "GETUTF8AT0 ~A ~A ~A~%" this (ldk-class cp) index)
  (let* ((cp (constant-pool (ldk-class cp)))
         (s (format nil "~A" (emit (aref cp index) cp))))
    (format t " = ~S~%" s)
    (jstring s)))
