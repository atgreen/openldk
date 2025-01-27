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
  (print "java/lang/Class.getSecurityManager()")
  (classload "java/lang/SecurityManager")
  (eval (list 'make-instance (list 'quote '|java/lang/SecurityManager|))))

(defmethod |println(Ljava/lang/String;)| (stream string)
  (format t "~A~%" (slot-value string '|value|)))

(defmethod |println(I)| (stream number)
  (format t "~A~%" number))

(defmethod |println(Ljava/lang/Object;)| (stream object)
  (format t "~A~%" object))

(defmethod |println(Ljava/lang/Object;)| (stream (object (eql nil)))
  (format t "null~%"))

(defmethod |fillInStackTrace(I)| ((|this| |java/lang/Throwable|) dummy)
  (let ((bt (trivial-backtrace:print-backtrace nil :output nil)))
    (print bt)))

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
  ;; FIXME: this only works for static methods at the moment
  ;; FIXME: we don't need the whole backtrace
  (let* ((caller-string (format nil "~A" (fourth (%remove-adjacent-repeats (sb-debug:list-backtrace))))))
    ;; (cstring (subseq caller-string 1 (position #\. caller-string))))
    (java-class
     (gethash
      (let ((dot-position (position #\. caller-string)))
        (cond
          ((str:starts-with? "(%clinit-" caller-string)
           (subseq caller-string 9 (1- (length caller-string))))
          (dot-position
           (subseq caller-string 1 dot-position))
          (t (error (format nil "ERROR in sun/reflect/Reflection.getCallerClass(): don't recognize ~S" caller-string)))))
      *classes*))))

(defmethod |getClass()| (object)
  ;;; FIXME - throw nullpointerexception
	(when *debug-trace*
		(format t "tracing: java/lang/Object.getClass(~A): ~A~%" object (java-class (gethash (format nil "~A" (type-of object)) *classes*))))
	(java-class (gethash (format nil "~A" (type-of object)) *classes*)))

(defmethod |java/lang/Class.forName0(Ljava/lang/String;ZLjava/lang/ClassLoader;Ljava/lang/Class;)| (name initialize loader caller)
  (unwind-protect
       (progn
         (when *debug-trace*
           (format t "; trace: entering java/lang/Class.forName0(Ljava/lang/String;ZLjava/lang/ClassLoader;Ljava/lang/Class;)~%"))
         (let ((lname (substitute #\/ #\. (slot-value name '|value|))))
           (or (and (gethash lname *classes*)
                    (java-class (gethash lname *classes*)))
               (progn (let ((klass (classload lname)))
                        (%clinit klass)
                        (java-class klass))))))
    (when *debug-trace*
      (format t "; trace: leaving  java/lang/Class.forName0(Ljava/lang/String;ZLjava/lang/ClassLoader;Ljava/lang/Class;)~%"))))

(defmethod |java/lang/System.currentTimeMillis()| ()
	;;; FIXME: this probably isn't right.
  (floor (* (get-internal-real-time) 1000)
				 internal-time-units-per-second))

(defmethod |java/lang/System.arraycopy(Ljava/lang/Object;ILjava/lang/Object;II)| (source_arr sourcePos dest_arr destPos len)
	(dotimes (i len)
		(setf (aref dest_arr (+ destPos i)) (aref source_arr (+ sourcePos i)))))

(defmethod |java/security/AccessController.doPrivileged(Ljava/security/PrivilegedAction;)| (action)
  (|run()| action))

(defmethod |java/lang/Class.getPrimitiveClass(Ljava/lang/String;)| (class-name)
  (let ((name (slot-value class-name '|value|)))
    (cond
      ((string= name "float")
       (let ((jc (java-class (gethash "java/lang/Float" *classes*))))
         jc))
      ((string= name "int")
       (let ((jc (java-class (gethash "java/lang/Integer" *classes*))))
         jc))
      ((string= name "byte")
       (let ((jc (java-class (gethash "java/lang/Byte" *classes*))))
         jc))
      ((string= name "long")
       (let ((jc (java-class (gethash "java/lang/Long" *classes*))))
         jc))
      ((string= name "boolean")
       (let ((jc (java-class (gethash "java/lang/Boolean" *classes*))))
         jc))
      ((string= name "char")
       (let ((jc (java-class (gethash "java/lang/Character" *classes*))))
         jc))
      ((string= name "short")
       (let ((jc (java-class (gethash "java/lang/Short" *classes*))))
         jc))
      ((string= name "double")
       (let ((jc (java-class (gethash "java/lang/Double" *classes*))))
         jc))
      ((string= name "void")
       (let ((jc (java-class (gethash "java/lang/Void" *classes*))))
         jc))
       (t (error (format nil "getPrimitiveClass(~A)" name))))))

(defmethod |java/lang/Float.floatToRawIntBits(F)| (float)
  (float-features:single-float-bits float))

(defmethod |java/lang/Double.doubleToRawLongBits(D)| (double)
  (float-features:double-float-bits double))

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
  (let ((offset (sxhash field)))
    (setf (gethash offset field-offset-table) field)
    offset))

(defun |java/lang/Class$Atomic.objectFieldOffset([Ljava/lang/reflect/Field;Ljava/lang/String;)| (field name)
  ;; FIXME
  0)

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

(defvar hash-code-counter 2025)

(defmethod |hashCode()| (obj)
  (with-slots (%hash-code) obj
    (or %hash-code
        (setf %hash-code (incf hash-code-counter))
        hash-code-counter)))

(defclass %array-base-offset ()
  ((array :initarg :array)))

(defmethod |arrayBaseOffset(Ljava/lang/Class;)| ((unsafe |sun/misc/Unsafe|) array)
  0)
;  (make-instance '%array-base-offset :array array))

(defmethod |arrayIndexScale(Ljava/lang/Class;)| ((unsafe |sun/misc/Unsafe|) array)
  ;; Always use 2, which is 2x what we really need.
  2)

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
    (if (null (find name-string '("java/lang/Boolean"
                                  "java/lang/Character"
                                  "java/lang/Byte"
                                  "java/lang/Short"
                                  "java/lang/Integer"
                                  "java/lang/Long"
                                  "java/lang/Float"
                                  "java/lang/Double"
                                  "java/lang/Void")
                    :test #'equal))
        0
        1)))

(defmethod |getDeclaredFields0(Z)| ((this |java/lang/Class|) arg)
  ;; FIXME
  ;; Load java/lang/reflect/Field if it hasn't been yet.
  (unless (gethash "java/lang/reflect/Field" *classes*)
    (|java/lang/Class.forName0(Ljava/lang/String;ZLjava/lang/ClassLoader;Ljava/lang/Class;)| (jstring "java/lang/reflect/Field") nil nil nil))

  ;; Get the ldk-class for THIS
  (let ((ldk-class (gethash (slot-value (slot-value this '|name|) '|value|) *classes*)))
    (labels ((get-fields (ldk-class)
               (when ldk-class
                 (append (loop for field across (fields ldk-class)
                               collect (let ((f (make-instance '|java/lang/reflect/Field|)))
                                         (|<init>(Ljava/lang/Class;Ljava/lang/String;Ljava/lang/Class;IILjava/lang/String;[B)| f this (ijstring (name field)) nil nil nil nil nil)
                                         f))
                         (get-fields (gethash (super ldk-class) *classes*))))))
      (coerce (get-fields ldk-class) 'vector))))

(defun |sun/misc/VM.initialize()| ()
  ;; FIXME
  )

#|
boolean compareAndSwapObject(Object obj, long fieldId, Object expectedValue, Object newValue) {
    synchronized (globalExchangeLock) {
        if (obj.fieldArray.get(fieldId) == expectedValue) {
            obj.fieldArray.set(fieldId, newValue);
            return true;
        }
    }
    return false;
}
|#

(defmethod |compareAndSwapObject(Ljava/lang/Object;JLjava/lang/Object;Ljava/lang/Object;)| ((unsafe |sun/misc/Unsafe|) obj field-id expected-value new-value)
  ;; FIXME
  ; (format t "~&~A.compareAndSwapObject(~A, ~A, ~A, ~A)~%" unsafe obj field-id expected-value new-value)
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
  ;; FIXME
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
     ; (format t "~A.getObjectVolatile(~A, ~A) = ~A~%" unsafe obj l (aref obj l))
     (aref obj l))
#|
     (format t "~A.getObjectVolatile(~A, ~A) = ~A~%" unsafe obj l (aref obj (ash l -2)))
     (aref obj (ash l -2)))
|#
    (t (error "Unrecognized object type in getObjectVolatile: " obj))))

(defun |java/security/AccessController.getStackAccessControlContext()| ()
  ;; FIXME -- implement
  nil)
