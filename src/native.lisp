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
        ((str:starts-with? "((METHOD " caller-string)
         (type-of (cadr caller-list)))
        (dot-position
         (subseq caller-string 1 dot-position))
        (t "openldk-internal")))))

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

(defmethod |sun/reflect/Reflection.getCallerClass()| ()
  ;; FIXME: we don't need the whole backtrace
  (let* ((caller-list (fourth (%remove-adjacent-repeats (sb-debug:list-backtrace)))))
    (gethash (%caller-class-name-from-stack-frame caller-list) *java-classes*)))

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
  (let* ((ldk-class (gethash cname *classes*)))
    (if ldk-class
        ldk-class
        (let ((lclass (make-instance '<class>
                                     :name cname
                                     :super "java/lang/Object"))
              (java-class (make-instance '|java/lang/Class|)))
          (setf (slot-value java-class '|name|) (ijstring cname))
          (setf (slot-value java-class '|classLoader|) nil)
          (setf (slot-value lclass 'java-class) java-class)
          (setf (gethash cname *classes*) lclass)
          (setf (gethash cname *java-classes*) java-class)
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
           (t (java-class (gethash (format nil "~A" (type-of object)) *classes*)))))
    (when *debug-trace*
      (incf *call-nesting-level* -1))))

(defmethod |java/lang/Class.forName0(Ljava/lang/String;ZLjava/lang/ClassLoader;Ljava/lang/Class;)| (name initialize loader caller)
  (unwind-protect
       (progn
         (when *debug-trace*
           (format t "~&~V@A trace: entering java/lang/Class.forName0(Ljava/lang/String;ZLjava/lang/ClassLoader;Ljava/lang/Class;) ~A~%"
                   (incf *call-nesting-level* 1) "*" name))
         (let ((lname (substitute #\/ #\. (coerce (slot-value name '|value|) 'string))))
           (or (and (eq (char lname 0) #\[)
                    (or (gethash lname *java-classes*)
                        (java-class (%get-array-ldk-class-from-name (subseq lname 1)))))
               (and (gethash lname *classes*)
                    (java-class (gethash lname *classes*)))
               (progn (let ((klass (classload lname)))
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
    (gethash name *java-classes*)))

(defmethod |java/lang/Float.floatToRawIntBits(F)| (float)
  (float-features:single-float-bits float))

(defmethod |java/lang/Double.doubleToRawLongBits(D)| (double)
  (float-features:double-float-bits (coerce double 'double-float)))

(defmethod |java/lang/Double.longBitsToDouble(J)| (long-bits)
  (float-features:bits-double-float long-bits))

(defmethod |java/util/TimeZone.getSystemTimeZoneID(Ljava/lang/String;)| (arg)
  (jstring (local-time:format-timestring nil (local-time:now) :format '(:timezone))))

(defmethod |length()| ((str string))
  (length str))

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
  (java-class (gethash "java/lang/Object" *classes*)))

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
  (if (and (not (|isPrimitive()| this))
           (let ((lclass (gethash (slot-value (slot-value this '|name|) '|value|) *classes*)))
             (interface-p lclass)))
      1
      0))

(defmethod |getDeclaredConstructors0(Z)| ((this |java/lang/Class|) arg)
  ;; FIXME
  (unwind-protect
       (progn
         (when *debug-trace*
           (format t "~&~V@A trace: entering java/lang/Class.getDeclaredConstructors0(Z)~%" (incf *call-nesting-level* 1) "*"))
         (unless (gethash "java/lang/reflect/Constructor" *classes*)
           (|java/lang/Class.forName0(Ljava/lang/String;ZLjava/lang/ClassLoader;Ljava/lang/Class;)| (jstring "java/lang/reflect/Constructor") nil nil nil))

         ;; Get the lclass for THIS
         (let ((lclass (gethash (slot-value (slot-value this '|name|) '|value|) *classes*)))
           (coerce (append (loop for method across (methods lclass)
                                 when (str:starts-with? "<init>" (name method))
                                   collect (let ((c (make-instance '|java/lang/reflect/Constructor|)))
                                             (|<init>(Ljava/lang/Class;[Ljava/lang/Class;[Ljava/lang/Class;IILjava/lang/String;[B[B)| c this (%get-parameter-types (descriptor method)) (make-array 0) (access-flags method) 0 (ijstring (descriptor method)) (make-array 0) (make-array 0))
                                             c)))
                   'vector)))
    (when *debug-trace*
      (incf *call-nesting-level* -1))))


(defmethod |getDeclaredMethods0(Z)| ((this |java/lang/Class|) arg)
  ;; FIXME
  (unwind-protect
       (progn
         (when *debug-trace*
           (format t "~&~V@A trace: entering java/lang/Class.getDeclaredMethods(Z)~%" (incf *call-nesting-level* 1) "*"))
         (unless (gethash "java/lang/reflect/Method" *classes*)
           (|java/lang/Class.forName0(Ljava/lang/String;ZLjava/lang/ClassLoader;Ljava/lang/Class;)| (jstring "java/lang/reflect/Method") nil nil nil))

         ;; Get the lclass for THIS
         (let ((lclass (gethash (slot-value (slot-value this '|name|) '|value|) *classes*)))
           (coerce (append (loop for method across (methods lclass)
                                 unless (str:starts-with? "<init>" (name method))
                                   collect (let ((c (make-instance '|java/lang/reflect/Method|)))
                                             (|<init>(Ljava/lang/Class;Ljava/lang/String;[Ljava/lang/Class;Ljava/lang/Class;[Ljava/lang/Class;IILjava/lang/String;[B[B[B)| c this (ijstring (name method)) (%get-parameter-types (descriptor method)) nil (make-array 0) (access-flags method) 0 (ijstring (descriptor method)) (make-array 0) (make-array 0) (make-array 0))
                                             c)))
                   'vector)))
    (when *debug-trace*
      (incf *call-nesting-level* -1))))


(defmethod |getDeclaredFields0(Z)| ((this |java/lang/Class|) arg)
  (unwind-protect
       (progn
         (when *debug-trace*
           (format t "~&~V@A trace: entering java/lang/Class.getDeclaredFields0(Z)~%" (incf *call-nesting-level* 1) "*"))
         (unless (gethash "java/lang/reflect/Field" *classes*)
           (|java/lang/Class.forName0(Ljava/lang/String;ZLjava/lang/ClassLoader;Ljava/lang/Class;)| (jstring "java/lang/reflect/Field") nil nil nil))

         ;; Get the lclass for THIS
         (let ((lclass (gethash (slot-value (slot-value this '|name|) '|value|) *classes*)))
           (labels ((get-fields (lclass)
                      (when lclass
                        (append (loop for field across (fields lclass)
                                      collect (let ((f (make-instance '|java/lang/reflect/Field|)))
                                                (|<init>(Ljava/lang/Class;Ljava/lang/String;Ljava/lang/Class;IILjava/lang/String;[B)| f this (ijstring (name field))
                                                 (gethash
                                                  (let ((cn (slot-value field 'descriptor)))
                                                    (if (eq (char cn 0) #\L)
                                                        (subseq cn 1 (1- (length cn)))
                                                        cn))
                                                  *java-classes*)
                                                 (access-flags field) nil nil nil)
                                                f))
                                (get-fields (gethash (super lclass) *classes*))))))
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

(defun |java/security/AccessController.getStackAccessControlContext()| ()
  ;; FIXME -- implement
  nil)

(defun |java/lang/System.initProperties(Ljava/util/Properties;)| (props)
  (dolist (prop `(("java.specification.version" . "8.0")
                  ("java.specification.name" . "Java Platform API Specification")
                  ("java.specification.vendor" . "Oracle Corporation")
                  ("java.version" . "8.0")
                  ("java.vendor" . "OpenLDK")
                  ("java.vendor.url" . "https://github.com/atgreen/openldk")
                  ("java.vendor.url.bug" . "https://github.com/atgreen/openldk/issues")
                  ("java.class.version" . "52.0")
                  ;; FIXME
                  ("java.home" . "/home/green/git/openldk")
                  ("user.home" . ,(uiop:getenv "HOME"))
                  ("os.name" . "Linux")
                  ("os.version" . "FIXME")
                  ("os.arch" . "FIXME")
                  ("file.separator" . "/")
                  ("file.encoding" . "UTF-8")
                  ("path.separator" . ":")
                  ;; FIXME
                  ("java.library.path" . "/usr/lib/jvm/java-21-openjdk-21.0.5.0.11-1.fc40.x86_64/lib/")
                  ("line.separator" . ,(format nil "~%"))))
    (|java/lang/System.setProperty(Ljava/lang/String;Ljava/lang/String;)| (ijstring (car prop)) (ijstring (cdr prop))))
  props)

#|
Need to add:

file.encoding.pkg
java.awt.printerjob
java.io.tmpdir
sun.arch.data.model
sun.awt.graphicsenv
sun.cpu.endian
sun.cpu.isalist
sun.desktop
sun.io.unicode.encoding
sun.java2d.fontpath
sun.jnu.encoding
sun.os.patch.level
sun.stderr.encoding
sun.stdout.encoding
user.country
user.dir
user.home
user.language
user.name
user.script
user.timezone
user.variant

|#

(defun |java/io/FileDescriptor.initIDs()| ()
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
  (let ((this-class (find-class (intern (name (gethash (slot-value (slot-value this '|name|) '|value|) *classes*)) :openldk)))
        (other-class (find-class (intern (name (gethash (slot-value (slot-value other '|name|) '|value|) *classes*)) :openldk))))
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
  (let ((lclass (gethash (slot-value (slot-value class '|name|) '|value|) *classes*)))
    (access-flags lclass)))

(defmethod |getModifiers()| ((class |java/lang/Class|))
  (let ((lclass (gethash (slot-value (slot-value class '|name|) '|value|) *classes*)))
    (access-flags lclass)))

(defmethod |getSuperclass()| ((class |java/lang/Class|))
  (let ((lclass (gethash (slot-value (slot-value class '|name|) '|value|) *classes*)))
    (gethash (super lclass) *java-classes*)))

(defmethod |getInterfaces0()| ((class |java/lang/Class|))
  ;; FIXME: do something different for interfaces?
  (let ((lclass (gethash (slot-value (slot-value class '|name|) '|value|) *classes*)))
    (coerce (mapcar (lambda (iname) (java-class (gethash iname *classes*))) (coerce (interfaces lclass) 'list))
            'vector)))

(defun |sun/reflect/NativeConstructorAccessorImpl.newInstance0(Ljava/lang/reflect/Constructor;[Ljava/lang/Object;)|
    (constructor params)
  (let ((class-name (slot-value (slot-value constructor '|clazz|) '|name|)))
    (|java/lang/Class.forName0(Ljava/lang/String;ZLjava/lang/ClassLoader;Ljava/lang/Class;)| class-name nil nil nil)
    (let ((obj (make-instance (intern (slot-value class-name '|value|) :openldk))))
      (if (string= "()V" (slot-value (slot-value constructor '|signature|) '|value|))
          (|<init>()| obj)
          (error "unimplemented"))
      obj)))

(defmethod |ensureClassInitialized(Ljava/lang/Class;)| ((unsafe |sun/misc/Unsafe|) class)
  (let ((lclass (gethash (slot-value (slot-value class '|name|) '|value|) *classes*)))
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
  #+LINUX (jstring (format nil "lib~A.so" (coerce (slot-value library-name '|value|) 'string)))
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
  (let ((sname (coerce (slot-value signal-name '|value|) 'string)))
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

(defmethod |open0(Ljava/lang/String;)| ((fis |java/io/FileInputStream|) filename)
  (handler-case
      (setf (slot-value fis '|fd|) (open (coerce (slot-value filename '|value|) 'string)
                                         :element-type '(unsigned-byte 8)
                                         :direction :input))
    ((or sb-ext:file-does-not-exist sb-int:simple-file-error) (e)
      (declare (ignore e))
      (let ((fnf (make-instance '|java/io/FileNotFoundException|)))
        (|<init>(Ljava/lang/String;)| fnf filename)
        (error (%lisp-condition fnf))))))

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
  (if (typep objref (intern (coerce (slot-value (slot-value this '|name|) '|value|) 'string) :openldk)) 1 0))

(defmethod |closeAll(Ljava/io/Closeable;)| ((stream stream) closeable)
  (close stream))

(defmethod |writeBytes([BIIZ)| ((fos |java/io/FileOutputStream|) byte-array offset length append?)
  (declare (ignore append?))
  (let* ((file-descriptor (slot-value fos '|fd|))
         (fd (slot-value file-descriptor '|fd|)))
    (cond
      ((eq fd 1)
       (write-sequence byte-array *standard-output* :start offset :end (+ offset length)))
      (t
       (assert "unimplemented")))))

(defmethod |getEnclosingMethod0()| ((this |java/lang/Class|))
  ;; FIXME
  nil)

(defmethod |getDeclaringClass0()| ((this |java/lang/Class|))
  ;; FIXME
  nil)

(defmethod |getBooleanAttributes0(Ljava/io/File;)| ((this |java/io/UnixFileSystem|) file)
  (format t "~&FILE-ATTRIBUTES: ~A~%" (lstring (slot-value file '|path|)))
  (handler-case
      (org.shirakumo.file-attributes:attributes (lstring (slot-value file '|path|)))
    (sb-int:simple-file-error (e)
      0)))

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

#|

The following java.lang.Math methods were generated by an LLM.  Here's
the prompt I used:

  I'm writing a java bytecode to common lisp transpiler.  Looking at
  the java.lang.Math class, here's an example of what I need:

  (defun |java/lang/Math.acos(D)| (a)
    (acos a))

  Now make the rest of them. Be sure to add documentation strings.
|#

(defun |java/lang/Math.abs(I)| (a)
  "Returns the absolute value of an integer."
  (abs a))

(defun |java/lang/Math.abs(J)| (a)
  "Returns the absolute value of a long integer."
  (abs a))

(defun |java/lang/Math.abs(F)| (a)
  "Returns the absolute value of a float."
  (abs a))

(defun |java/lang/Math.abs(D)| (a)
  "Returns the absolute value of a double."
  (abs a))

(defun |java/lang/Math.acos(D)| (a)
  "Returns the arc cosine of a value, in radians."
  (acos a))

(defun |java/lang/Math.asin(D)| (a)
  "Returns the arc sine of a value, in radians."
  (asin a))

(defun |java/lang/Math.atan(D)| (a)
  "Returns the arc tangent of a value, in radians."
  (atan a))

(defun |java/lang/Math.atan2(DD)| (a b)
  "Returns the angle theta from the conversion of rectangular coordinates (a, b) to polar coordinates."
  (atan a b))

(defun |java/lang/Math.cbrt(D)| (a)
  "Returns the cube root of a value."
  (expt a (/ 1 3)))

(defun |java/lang/Math.ceil(D)| (a)
  "Returns the smallest integer greater than or equal to the given value."
  (ceiling a))

(defun |java/lang/Math.cos(D)| (a)
  "Returns the cosine of an angle in radians."
  (cos a))

(defun |java/lang/Math.cosh(D)| (a)
  "Returns the hyperbolic cosine of a value."
  (cosh a))

(defun |java/lang/Math.exp(D)| (a)
  "Returns Euler's number e raised to the power of a value."
  (exp a))

(defun |java/lang/Math.expm1(D)| (a)
  "Returns e^a - 1."
  (- (exp a) 1))

(defun |java/lang/Math.floor(D)| (a)
  "Returns the largest integer less than or equal to the given value."
  (floor a))

(defun |java/lang/Math.hypot(DD)| (a b)
  "Returns sqrt(a^2 + b^2) without intermediate overflow or underflow."
  (sqrt (+ (* a a) (* b b))))

(defun |java/lang/Math.IEEEremainder(DD)| (a b)
  "Computes the remainder operation as defined by IEEE 754."
  (rem a b))

(defun |java/lang/Math.log(D)| (a)
  "Returns the natural logarithm (base e) of a value."
  (log a))

(defun |java/lang/Math.log10(D)| (a)
  "Returns the base 10 logarithm of a value."
  (log a 10))

(defun |java/lang/Math.log1p(D)| (a)
  "Returns the natural logarithm of (1 + a)."
  (log (+ a 1)))

(defun |java/lang/Math.max(II)| (a b)
  "Returns the greater of two integers."
  (max a b))

(defun |java/lang/Math.max(JJ)| (a b)
  "Returns the greater of two long integers."
  (max a b))

(defun |java/lang/Math.max(FF)| (a b)
  "Returns the greater of two floats."
  (max a b))

(defun |java/lang/Math.max(DD)| (a b)
  "Returns the greater of two doubles."
  (max a b))

(defun |java/lang/Math.min(II)| (a b)
  "Returns the smaller of two integers."
  (min a b))

(defun |java/lang/Math.min(JJ)| (a b)
  "Returns the smaller of two long integers."
  (min a b))

(defun |java/lang/Math.min(FF)| (a b)
  "Returns the smaller of two floats."
  (min a b))

(defun |java/lang/Math.min(DD)| (a b)
  "Returns the smaller of two doubles."
  (min a b))

(defun |java/lang/Math.pow(DD)| (a b)
  "Returns a raised to the power of b."
  (expt a b))

(defun |java/lang/Math.random()| ()
  "Returns a pseudo-random number between 0.0 and 1.0."
  (random 1.0))

(defun |java/lang/Math.rint(D)| (a)
  "Returns the integer value that is closest to the argument."
  (fround a))

(defun |java/lang/Math.round(F)| (a)
  "Returns the closest integer to the given float."
  (round a))

(defun |java/lang/Math.round(D)| (a)
  "Returns the closest integer to the given double."
  (round a))

(defun |java/lang/Math.signum(F)| (a)
  "Returns the signum function of a float: -1 if negative, 1 if positive, 0 if zero."
  (cond ((> a 0) 1.0)
        ((< a 0) -1.0)
        (t 0.0)))

(defun |java/lang/Math.signum(D)| (a)
  "Returns the signum function of a double: -1 if negative, 1 if positive, 0 if zero."
  (cond ((> a 0) 1.0)
        ((< a 0) -1.0)
        (t 0.0)))

(defun |java/lang/Math.sin(D)| (a)
  "Returns the sine of an angle in radians."
  (sin a))

(defun |java/lang/Math.sinh(D)| (a)
  "Returns the hyperbolic sine of a value."
  (sinh a))

(defun |java/lang/Math.sqrt(D)| (a)
  "Returns the square root of a value."
  (sqrt a))

(defun |java/lang/Math.tan(D)| (a)
  "Returns the tangent of an angle in radians."
  (tan a))

(defun |java/lang/Math.tanh(D)| (a)
  "Returns the hyperbolic tangent of a value."
  (tanh a))

(defun |java/lang/Math.toDegrees(D)| (a)
  "Converts an angle from radians to degrees."
  (* a (/ 180.0 cl-user::pi)))

(defun |java/lang/Math.toRadians(D)| (a)
  "Converts an angle from degrees to radians."
  (* a (/ cl-user::pi 180.0)))

(defun |java/lang/Math.ulp(F)| (a)
  "Returns the unit in the last place (ULP) of a float."
  (float-epsilon a))

(defun |java/lang/Math.ulp(D)| (a)
  "Returns the unit in the last place (ULP) of a double."
  (float-epsilon a))

#|

Prompt: Now do the same for StrictMath.

FIXME: these aren't really strict/ Look at sb-mpfr/

|#

(defun |java/lang/StrictMath.abs(I)| (a)
  "Returns the absolute value of an integer."
  (abs a))

(defun |java/lang/StrictMath.abs(J)| (a)
  "Returns the absolute value of a long integer."
  (abs a))

(defun |java/lang/StrictMath.abs(F)| (a)
  "Returns the absolute value of a float."
  (abs a))

(defun |java/lang/StrictMath.abs(D)| (a)
  "Returns the absolute value of a double."
  (abs a))

(defun |java/lang/StrictMath.acos(D)| (a)
  "Returns the arc cosine of a value, in radians."
  (acos a))

(defun |java/lang/StrictMath.asin(D)| (a)
  "Returns the arc sine of a value, in radians."
  (asin a))

(defun |java/lang/StrictMath.atan(D)| (a)
  "Returns the arc tangent of a value, in radians."
  (atan a))

(defun |java/lang/StrictMath.atan2(DD)| (a b)
  "Returns the angle theta from the conversion of rectangular coordinates (a, b) to polar coordinates."
  (atan a b))

(defun |java/lang/StrictMath.cbrt(D)| (a)
  "Returns the cube root of a value."
  (expt a (/ 1 3)))

(defun |java/lang/StrictMath.ceil(D)| (a)
  "Returns the smallest integer greater than or equal to the given value."
  (ceiling a))

(defun |java/lang/StrictMath.cos(D)| (a)
  "Returns the cosine of an angle in radians."
  (cos a))

(defun |java/lang/StrictMath.cosh(D)| (a)
  "Returns the hyperbolic cosine of a value."
  (cosh a))

(defun |java/lang/StrictMath.exp(D)| (a)
  "Returns Euler's number e raised to the power of a value."
  (exp a))

(defun |java/lang/StrictMath.expm1(D)| (a)
  "Returns e^a - 1."
  (- (exp a) 1))

(defun |java/lang/StrictMath.floor(D)| (a)
  "Returns the largest integer less than or equal to the given value."
  (floor a))

(defun |java/lang/StrictMath.hypot(DD)| (a b)
  "Returns sqrt(a^2 + b^2) without intermediate overflow or underflow."
  (sqrt (+ (* a a) (* b b))))

(defun |java/lang/StrictMath.IEEEremainder(DD)| (a b)
  "Computes the remainder operation as defined by IEEE 754."
  (rem a b))

(defun |java/lang/StrictMath.log(D)| (a)
  "Returns the natural logarithm (base e) of a value."
  (log a))

(defun |java/lang/StrictMath.log10(D)| (a)
  "Returns the base 10 logarithm of a value."
  (log a 10))

(defun |java/lang/StrictMath.log1p(D)| (a)
  "Returns the natural logarithm of (1 + a)."
  (log (+ a 1)))

(defun |java/lang/StrictMath.max(II)| (a b)
  "Returns the greater of two integers."
  (max a b))

(defun |java/lang/StrictMath.max(JJ)| (a b)
  "Returns the greater of two long integers."
  (max a b))

(defun |java/lang/StrictMath.max(FF)| (a b)
  "Returns the greater of two floats."
  (max a b))

(defun |java/lang/StrictMath.max(DD)| (a b)
  "Returns the greater of two doubles."
  (max a b))

(defun |java/lang/StrictMath.min(II)| (a b)
  "Returns the smaller of two integers."
  (min a b))

(defun |java/lang/StrictMath.min(JJ)| (a b)
  "Returns the smaller of two long integers."
  (min a b))

(defun |java/lang/StrictMath.min(FF)| (a b)
  "Returns the smaller of two floats."
  (min a b))

(defun |java/lang/StrictMath.min(DD)| (a b)
  "Returns the smaller of two doubles."
  (min a b))

(defun |java/lang/StrictMath.pow(DD)| (a b)
  "Returns a raised to the power of b."
  (expt a b))

(defun |java/lang/StrictMath.random()| ()
  "Returns a pseudo-random number between 0.0 and 1.0."
  (random 1.0))

(defun |java/lang/StrictMath.rint(D)| (a)
  "Returns the integer value that is closest to the argument."
  (fround a))

(defun |java/lang/StrictMath.round(F)| (a)
  "Returns the closest integer to the given float."
  (round a))

(defun |java/lang/StrictMath.round(D)| (a)
  "Returns the closest integer to the given double."
  (round a))

(defun |java/lang/StrictMath.signum(F)| (a)
  "Returns the signum function of a float: -1 if negative, 1 if positive, 0 if zero."
  (cond ((> a 0) 1.0)
        ((< a 0) -1.0)
        (t 0.0)))

(defun |java/lang/StrictMath.signum(D)| (a)
  "Returns the signum function of a double: -1 if negative, 1 if positive, 0 if zero."
  (cond ((> a 0) 1.0)
        ((< a 0) -1.0)
        (t 0.0)))

(defun |java/lang/StrictMath.sin(D)| (a)
  "Returns the sine of an angle in radians."
  (sin a))

(defun |java/lang/StrictMath.sinh(D)| (a)
  "Returns the hyperbolic sine of a value."
  (sinh a))

(defun |java/lang/StrictMath.sqrt(D)| (a)
  "Returns the square root of a value."
  (sqrt a))

(defun |java/lang/StrictMath.tan(D)| (a)
  "Returns the tangent of an angle in radians."
  (tan a))

(defun |java/lang/StrictMath.tanh(D)| (a)
  "Returns the hyperbolic tangent of a value."
  (tanh a))

(defun |java/lang/StrictMath.toDegrees(D)| (a)
  "Converts an angle from radians to degrees."
  (* a (/ 180.0 cl-user::pi)))

(defun |java/lang/StrictMath.toRadians(D)| (a)
  "Converts an angle from degrees to radians."
  (* a (/ cl-user::pi 180.0)))

(defun |java/lang/StrictMath.ulp(F)| (a)
  "Returns the unit in the last place (ULP) of a float."
  (float-epsilon a))

(defun |java/lang/StrictMath.ulp(D)| (a)
  "Returns the unit in the last place (ULP) of a double."
  (float-epsilon a))

(defun |java/lang/System.nanoTime()| ()
  ;; Do some more math if this is not true.
  (assert (eq org.shirakumo.precise-time:precise-time-units-per-second
              1000000000))
  (multiple-value-bind (universal-time nanoseconds)
      (org.shirakumo.precise-time:get-precise-time)
    (+ (* universal-time 1000000000) nanoseconds)))

(defun |java/lang/System.identityHashCode(Ljava/lang/Object;)| (objref)
  ;; A bit mixing hash to bring 64-bit integers down to 32-bits.
  (let ((x (sxhash objref)))
    (let ((x (logxor x (ash x -33))))
      (setf x (* x #x62A9D9ED799705F5))
      (setf x (logxor x (ash x -28)))
      (ldb (byte 32 0) x))))

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
         (let ((result (apply (intern
                               (lispize-method-name
                                (concatenate 'string
                                             (coerce (slot-value (slot-value (slot-value method '|clazz|) '|name|) '|value|) 'string)
                                             "."
                                             (coerce (slot-value (slot-value method '|name|) '|value|) 'string)
                                             (coerce (slot-value (slot-value method '|signature|) '|value|) 'string)))
                               :openldk)
                              (if (eq 0 (logand #x8 (slot-value method '|modifiers|)))
                                  (cons object (coerce args 'list)) ; non-static method
                                  (coerce args 'list))))) ; static method
           (when *debug-trace*
             (format t "~&~V@A trace: result = ~A~%"
                     *call-nesting-level* "*" result))
           result))
    (incf *call-nesting-level* -1)))

(defun |java/lang/reflect/Array.newArray(Ljava/lang/Class;I)| (class size)
  ;; FIXME
  (make-array size
              :initial-element nil))

(defmethod |findLoadedClass0(Ljava/lang/String;)| ((loader |java/lang/ClassLoader|) name)
  ;; FIXME
  (gethash (slot-value name '|value|) *java-classes*))

(defmethod |findBootstrapClass(Ljava/lang/String;)| ((loader |java/lang/ClassLoader|) name)
  ;; FIXME
  (let ((ldk-class (classload (slot-value name '|value|))))
    (java-class ldk-class)))
