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

(defmethod |wait(J)| ((this |java/lang/Object|) timeout)
  (let* ((monitor (%get-monitor this))
         (mutex (mutex monitor))
         (cv (condition-variable monitor))
         (current-thread (bordeaux-threads:current-thread)))
    (bordeaux-threads:with-lock-held (mutex)
      (unless (eq (owner monitor) current-thread)
        (error (%lisp-condition (%make-throwable '|java/lang/IllegalMonitorStateException|))))
      (let ((saved-recursion (recursion-count monitor)))
        (push current-thread (wait-set monitor))
        (setf (owner monitor) nil (recursion-count monitor) 0)
        (bordeaux-threads:condition-notify cv)
        (loop while (member current-thread (wait-set monitor))
              do (if (zerop timeout)
                     (bordeaux-threads:condition-wait cv mutex)
                     (unless (bordeaux-threads:condition-wait cv mutex :timeout (/ timeout 1000.0))
                       (setf (wait-set monitor) (remove current-thread (wait-set monitor)))
                       (return))))
        (loop while (owner monitor)
              do (bordeaux-threads:condition-wait cv mutex))
        (setf (owner monitor) current-thread (recursion-count monitor) saved-recursion)))))

(defun |java/lang/ClassLoader.registerNatives()| ()
  ())

(defun |java/lang/System.registerNatives()| ()
  ())

(defun |java/lang/Class.registerNatives()| ()
  ())

(defun |java/lang/Class.desiredAssertionStatus0(Ljava/lang/Class;)| (class)
  (declare (ignore class))
  ;; Return 0 (false) to disable assertions
  0)

(defun |java/lang/Class.getSecurityManager()| ()
  (classload "java/lang/SecurityManager")
  (eval (list 'make-instance (list 'quote '|java/lang/SecurityManager|))))

(defmethod |fillInStackTrace(I)| ((this |java/lang/Throwable|) dummy)
  (declare (ignore dummy))
  (setf (slot-value this '|backtrace|) (sb-debug:list-backtrace)))

(defmethod |getStackTraceDepth()| ((this |java/lang/Throwable|))
  (length (slot-value this '|backtrace|)))

(defun %coerce-java-integer (obj)
  "Return a Common Lisp integer extracted from OBJ when possible.
Accepts native CL integers and Java numeric wrapper instances."
  (cond
    ((integerp obj) obj)
    ((typep obj '|java/lang/Number|)
     (when (slot-exists-p obj '|value|)
       (slot-value obj '|value|)))
    (t nil)))

(defun %boolean-object (truthy)
  (slot-value |+static-java/lang/Boolean+|
              (if truthy '|TRUE| '|FALSE|)))

(defun %caller-class-name-from-stack-frame (caller-list)
  (let ((caller-string (format nil "~A" caller-list)))
    (let ((dot-position (position #\. caller-string)))
      (cond
        ((starts-with? "(%clinit-" caller-string)
         (subseq caller-string 9 (1- (length caller-string))))
        ((starts-with? "((METHOD" caller-string)
         (format nil "~A" (type-of (cadr caller-list))))
        ((starts-with? "((LAMBDA " caller-string)
         (substitute #\/ #\. (format nil "~A" (type-of (cadr caller-list)))))
        ((starts-with? "((LABELS CLINIT IN %CLINIT" caller-string)
         (name (cadr caller-list)))
        ((starts-with? "(%CLINIT " caller-string)
         (name (cadr caller-list)))
        (dot-position
         (subseq caller-string 1 dot-position))
        ;; FIXME: maybe use an OpenLDK internal class to indicate internal frame
        (t "java/lang/System")))))

(defmethod |getStackTraceElement(I)| ((this |java/lang/Throwable|) index)
  (let ((ste (%make-java-instance "java/lang/StackTraceElement"))
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
     "%RESOLVE-INVOKEDYNAMIC"
     "METHOD invoke"
     "#<java/lang/reflect/Method "
     "#<sun/reflect/NativeMethodAccessorImpl "
     "INVOKE-SPECIAL")))


(defun %is-internal-frame-p (caller-string)
  "Return T if the frame is an internal OpenLDK frame that should be skipped."
  (let ((lower-string (string-downcase caller-string)))
    (or (search "%clinit-" lower-string)
        (search "(labels clinit in %clinit" lower-string)
        (search "(%clinit " lower-string)
        (search "sun/reflect/reflection.getcallerclass" lower-string))))

(defun |sun/reflect/Reflection.getCallerClass(I)| (index)
  ;; FIXME: we don't need the whole backtrace
  (let* ((backtrace (%remove-invoke-frames (%remove-adjacent-repeats (sb-debug:list-backtrace)))))
    (assert (stringp (%caller-class-name-from-stack-frame (nth index backtrace))))
    (%get-java-class-by-bin-name (%caller-class-name-from-stack-frame (nth index backtrace)))))

(defun |sun/reflect/Reflection.getCallerClass()| ()
  ;; Skip internal frames to find the actual caller
  ;; The caller is the first non-internal Java method frame after:
  ;; - getCallerClass() itself (frame 0)
  ;; - The method that called getCallerClass (e.g., registerAsParallelCapable) (frame 1)
  ;; So we start at frame 2
  (let* ((backtrace (%remove-invoke-frames (%remove-adjacent-repeats (sb-debug:list-backtrace))))
         (skip-count 2)) ; Skip getCallerClass() and its immediate caller
    ;; Find the first non-internal frame after the skip count
    (loop for i from skip-count below (length backtrace)
          for frame = (nth i backtrace)
          for caller-string = (format nil "~A" frame)
          unless (%is-internal-frame-p caller-string)
            do (let ((class-name (%caller-class-name-from-stack-frame frame)))
                 (when (and (stringp class-name)
                            (not (find #\. class-name))
                            (gethash class-name *java-classes-by-bin-name*))
                   (return (%get-java-class-by-bin-name class-name))))
          finally (return (%get-java-class-by-bin-name "java/lang/System")))))

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
    (t
     (format nil "Ljava/lang/Object;"))))

(defun %get-array-ldk-class-from-name (cname)
  (let* ((ldk-class (%get-ldk-class-by-bin-name cname t)))
    (if ldk-class
        ldk-class
        (let* ((fq-name (substitute #\. #\/ cname)) ;; fq-name uses dots
               (lclass (make-instance '<class>
                                      :name cname
                                      :super "java/lang/Object"))
               (java-class (%make-java-instance "java/lang/Class")))
          (setf (slot-value java-class '|name|) (ijstring fq-name))
          (setf (slot-value java-class '|classLoader|) nil)
          (setf (slot-value lclass 'java-class) java-class)
          ;; Store by fq-name (with dots) in *-by-fq-name* tables
          (setf (gethash fq-name *ldk-classes-by-fq-name*) lclass)
          (setf (gethash fq-name *java-classes-by-fq-name*) java-class)
          ;; Store by bin-name (with slashes) in *-by-bin-name* tables
          (setf (gethash cname *ldk-classes-by-bin-name*) lclass)
          (setf (gethash cname *java-classes-by-bin-name*) java-class)
          lclass))))

(defun %get-array-ldk-class (element-type)
  (let* ((cname (format nil "[~A" (%type-to-descriptor element-type))))
    (%get-array-ldk-class-from-name cname)))

(defmethod |getClass()| (object)
  ;;; FIXME - throw nullpointerexception
  (unwind-protect
       (progn
         (when *debug-trace*
           (format t "~&~V@A trace: java/lang/Object.getClass(~A)"
                   (incf *call-nesting-level* 1) "*" object))
         (let ((c (cond
                    ((typep object 'java-array)
                     (|java/lang/Class.forName0(Ljava/lang/String;ZLjava/lang/ClassLoader;Ljava/lang/Class;)|
                      (jstring (format nil "[L~A;"
                                       (lstring (slot-value (java-array-component-class object) '|name|))))
                      nil nil nil))
                    (t
                     (let ((jc (%get-java-class-by-bin-name (format nil "~A" (type-of object)) t)))
                       (or jc
                           (|java/lang/Class.forName0(Ljava/lang/String;ZLjava/lang/ClassLoader;Ljava/lang/Class;)|
                            (jstring (format nil "~A" (type-of object))) nil nil nil)))))))
           c))
    (when *debug-trace*
      (incf *call-nesting-level* -1))))

(defmethod |java/lang/Class.forName0(Ljava/lang/String;ZLjava/lang/ClassLoader;Ljava/lang/Class;)| (name initialize loader caller)
  (unwind-protect
       (progn
         (when *debug-trace*
           (format t "~&~V@A trace: entering java/lang/Class.forName0(Ljava/lang/String;ZLjava/lang/ClassLoader;Ljava/lang/Class;) ~A~%"
                   (incf *call-nesting-level* 1) "*" (list name initialize loader caller)))
         (if (and loader (not (equal loader *boot-class-loader*)))
             (|findClass(Ljava/lang/String;)| loader name)
             (let ((lname (substitute #\/ #\. (lstring name))))
               (or (and (eq (char lname 0) #\[)
                        (or (%get-java-class-by-bin-name lname t)
                            (java-class (%get-array-ldk-class-from-name lname))))
                   (and (%get-ldk-class-by-bin-name lname t)
                        (java-class (%get-ldk-class-by-bin-name lname)))
                   (let ((klass (classload lname)))
                     (when klass
                       (%clinit klass)
                       (java-class klass)))))))
    (when *debug-trace*
      (incf *call-nesting-level* -1))))

(defun |java/lang/System.currentTimeMillis()| ()
  ;; Do some more math if this is not true.
  (assert (eq org.shirakumo.precise-time:precise-time-units-per-second
              1000000000))
  (unwind-protect
       (progn
         (when *debug-trace*
           (format t "~&~V@A trace: entering java/lang/System.currentTimeMillis()~%" (incf *call-nesting-level* 1) "*"))
         (multiple-value-bind (universal-time nanoseconds)
             (org.shirakumo.precise-time:get-precise-time)
           (let ((res (+ (* (local-time:timestamp-to-unix
                             (local-time:universal-to-timestamp universal-time))
                            1000)
                         (truncate nanoseconds 1000000))))
             (when *debug-trace*
               (format t "~&~V@A trace: java/lang/System.currentTimeMillis() = ~A~%" *call-nesting-level* "*" res))
             res)))
    (when *debug-trace*
      (incf *call-nesting-level* -1))))

(defun |java/lang/System.nanoTime()| ()
  ;; Do some more math if this is not true.
  (assert (eq org.shirakumo.precise-time:precise-time-units-per-second
              1000000000))
  (multiple-value-bind (universal-time nanoseconds)
      (org.shirakumo.precise-time:get-precise-time)
    (+ (* (local-time:timestamp-to-unix
           (local-time:universal-to-timestamp universal-time))
          1000000000)
       nanoseconds)))

(defmethod |java/lang/System.arraycopy(Ljava/lang/Object;ILjava/lang/Object;II)| (src-array src-pos dest-array dest-pos length)
  "Copies LENGTH elements from SRC-ARRAY starting at SRC-POS
   to DEST-ARRAY starting at DEST-POS.
   Handles overlapping regions correctly even within the same array."
  (let ((src-array (java-array-data src-array))
        (dest-array (java-array-data dest-array)))

    (declare (type array src-array dest-array)
             (type fixnum src-pos dest-pos length))

    ;; Validate arguments
    (when (< length 0)
      (error "Length cannot be negative: ~A" length))

    (when (< src-pos 0)
      (error "Source position cannot be negative: ~A" src-pos))

    (when (< dest-pos 0)
      (error "Destination position cannot be negative: ~A" dest-pos))

    (when (> (+ src-pos length) (array-total-size src-array))
      (error "Source array index out of bounds: size=~A, access=~A"
             (array-total-size src-array) (+ src-pos length -1)))

    (when (> (+ dest-pos length) (array-total-size dest-array))
      (error "Destination array index out of bounds: size=~A, access=~A"
             (array-total-size dest-array) (+ dest-pos length -1)))

    ;; Handle the case when src-array and dest-array are the same and regions overlap
    (if (and (eq src-array dest-array)
             (> dest-pos src-pos)
             (< dest-pos (+ src-pos length)))
        ;; Copy backwards to avoid overwriting source elements before they're copied
        (loop for i from (1- length) downto 0 do
          (setf (row-major-aref dest-array (+ dest-pos i))
                (row-major-aref src-array (+ src-pos i))))
        ;; Otherwise, copy forwards
        (loop for i from 0 below length do
          (setf (row-major-aref dest-array (+ dest-pos i))
                (row-major-aref src-array (+ src-pos i)))))))

(defmethod |run()| (arg)
  (declare (ignore arg))
  (error "internal error"))

(defmethod |java/security/AccessController.doPrivileged(Ljava/security/PrivilegedAction;)| (action)
  (|run()| action))

(defmethod |java/lang/Class.getPrimitiveClass(Ljava/lang/String;)| (class-name)
  (let ((name (lstring class-name)))
    (%get-java-class-by-fq-name name)))

(defmethod |java/lang/Float.floatToRawIntBits(F)| (float)
  (float-features:single-float-bits float))

(defmethod |java/lang/Double.doubleToRawLongBits(D)| (double)
  (float-features:double-float-bits (coerce double 'double-float)))

(defmethod |java/lang/Double.longBitsToDouble(J)| (long-bits)
  (float-features:bits-double-float (ldb (byte 64 0) long-bits)))

(defmethod |java/lang/Float.intBitsToFloat(I)| (int-bits)
  (float-features:bits-single-float (ldb (byte 32 0) int-bits)))

(defmethod |java/util/TimeZone.getSystemTimeZoneID(Ljava/lang/String;)| (arg)
  (jstring (local-time:format-timestring nil (local-time:now) :format '(:timezone))))

(defmethod |length()| ((str string))
  (length (lstring str)))

(defmethod |java/util/TimeZone.getSystemGMTOffsetID()| ()
  (jstring (local-time:format-timestring nil (local-time:now) :format '(:gmt-offset))))

(defvar *field-offset-table* (make-hash-table :test #'equal))

(defmethod |objectFieldOffset(Ljava/lang/reflect/Field;)| ((unsafe |sun/misc/Unsafe|) field)
  (declare (ignore unsafe))
  (let ((offset (unsigned-to-signed-integer (cl-murmurhash:murmurhash (sxhash field)))))
    (setf (gethash offset *field-offset-table*) field)
    offset))

(defun |java/lang/Class$Atomic.objectFieldOffset([Ljava/lang/reflect/Field;Ljava/lang/String;)| (field name)
  (declare (ignore field)
           (ignore name))
  ;; FIXME
  (error "ofo"))

(defmethod |staticFieldBase(Ljava/lang/reflect/Field;)| ((unsafe |sun/misc/Unsafe|) field)
  (declare (ignore unsafe)
           (ignore field))
  nil)

(defmethod |staticFieldOffset(Ljava/lang/reflect/Field;)| ((unsafe |sun/misc/Unsafe|) field)
  (declare (ignore unsafe))
  (let ((offset (sxhash field)))
    (setf (gethash offset *field-offset-table*) field)
    offset))

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
  ;; FIXME: What does this do??
  nil)

;;; The current |java/lang/Thread| object.
(defvar *current-thread* nil)

(defmethod |add(Ljava/lang/Thread;)| (thread-group thread)
  (declare (ignore thread-group))
  (declare (ignore thread))
  (error "internal error"))

(defmethod |java/lang/Thread.currentThread()| ()
  "Return the Java Thread object for the current Lisp thread."
  ;; Check if current Lisp thread has an associated Java Thread
  (let* ((current-lisp-thread (bordeaux-threads:current-thread))
         (java-thread (gethash current-lisp-thread *lisp-to-java-threads*)))
    (or java-thread
        ;; Fallback to main thread (for compatibility)
        *current-thread*
        ;; Create main thread if it doesn't exist
        (let ((thread (%make-java-instance "java/lang/Thread"))
              (thread-group (%make-java-instance "java/lang/ThreadGroup")))
          (|<init>()| thread-group)
          (setf *current-thread* thread)
          ;; Register main thread in our mappings
          (setf (gethash current-lisp-thread *lisp-to-java-threads*) thread)
          (setf (slot-value thread '|priority|) 1)
          (|add(Ljava/lang/Thread;)| thread-group thread)
          (|<init>(Ljava/lang/ThreadGroup;Ljava/lang/Runnable;Ljava/lang/String;J)|
           thread thread-group nil (jstring "main") 0)
          thread))))

(defmethod |setPriority0(I)| ((thread |java/lang/Thread|) priority)
  ;; FIXME
  (declare (ignore thread priority))
  nil)

(defmethod |isAlive()| ((thread |java/lang/Thread|))
  ;; FIXME
  0)

(defmethod |isInterrupted(Z)| ((thread |java/lang/Thread|) x)
  ;; FIXME
  0)

(defmethod |start0()| ((thread |java/lang/Thread|))
  ;; FIXME
  (declare (ignore thread))
  nil)

(defun |sun/misc/Unsafe.registerNatives()| ()
  ;; FIXME
  nil)

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
  (unless (%get-ldk-class-by-fq-name "sun.reflect.ConstantPool" t)
    (%clinit (classload "sun/reflect/ConstantPool")))
  (let ((ldk-class (get-ldk-class-for-java-class this)))
    (when ldk-class
      (let ((cp (%make-java-instance "sun/reflect/ConstantPool")))
        (setf (slot-value cp '|constantPoolOop|)
              (make-instance '<constant-pool> :ldk-class ldk-class))
        cp))))

(defmethod |getDeclaredConstructors0(Z)| ((this |java/lang/Class|) arg)
  ;; FIXME
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
                                    when (starts-with? "<init>" (name method))
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
                                               (access-flags method) 0 (ijstring (descriptor method))
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

(defmethod |getDeclaredMethods0(Z)| ((this |java/lang/Class|) arg)
  ;; FIXME
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
                                                      unless (starts-with? "<init>" (name method))
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
                                                                   (ijstring (descriptor method))
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


(defmethod |getDeclaredFields0(Z)| ((this |java/lang/Class|) arg)
  (unwind-protect
       (progn
         (when *debug-trace*
           (format t "~&~V@A trace: entering java/lang/Class.getDeclaredFields0(Z)~%" (incf *call-nesting-level* 1) "*"))
         (unless (gethash "java/lang/reflect/Field" *ldk-classes-by-bin-name* t)
           (|java/lang/Class.forName0(Ljava/lang/String;ZLjava/lang/ClassLoader;Ljava/lang/Class;)| (jstring "java/lang/reflect/Field") nil nil nil))

         ;; Get the lclass for THIS (use correct loader)
         (let ((lclass (get-ldk-class-for-java-class this)))
           (labels ((get-fields (lclass)
                      (when lclass
                        (append (loop for field across (fields lclass)
                                      collect (let ((f (%make-java-instance "java/lang/reflect/Field")))
                                                (|<init>(Ljava/lang/Class;Ljava/lang/String;Ljava/lang/Class;IILjava/lang/String;[B)|
                                                 f this (ijstring (name field))
                                                 (let ((cn (slot-value field 'descriptor)))
                                                   (when (eq (char cn 0) #\L)
                                                     (setf cn (subseq cn 1 (1- (length cn)))))
                                                   (or (%get-java-class-by-bin-name cn t)
                                                       (let ((njc (%make-java-instance "java/lang/Class")))
                                                         (setf (slot-value njc '|name|) (ijstring (substitute #\. #\/ cn)))
                                                         (setf (gethash (substitute #\. #\/ cn) *java-classes-by-fq-name*) njc)
                                                         (setf (gethash cn *java-classes-by-bin-name*) njc))))
                                                 (access-flags field) nil nil nil)
                                                f))
                                (when (super lclass)
                                  (get-fields (%get-ldk-class-by-bin-name (super lclass) t)))))))

             (make-java-array
              :component-class (%get-java-class-by-fq-name "java.lang.reflect.Field")
              :initial-contents (coerce (get-fields lclass) 'vector)))))
    (when *debug-trace*
      (incf *call-nesting-level* -1))))

(defun |sun/misc/VM.initialize()| ()
  ;; FIXME
  nil)

(defmethod |compareAndSwapObject(Ljava/lang/Object;JLjava/lang/Object;Ljava/lang/Object;)| ((unsafe |sun/misc/Unsafe|) obj field-id expected-value new-value)
  ;; FIXME
  (cond
    ((typep obj 'java-array)
     (if (equal (jaref obj field-id) expected-value)
         (progn
           (setf (jaref obj field-id) new-value)
           1)
         0))
    (t
    (let* ((field (gethash field-id *field-offset-table*))
            (key (intern (lstring (slot-value field '|name|)) :openldk)))
       (if (equal (slot-value obj key) expected-value)
           (progn
             (setf (slot-value obj key) new-value)
             1)
           0)))))

(defmethod |compareAndSwapInt(Ljava/lang/Object;JII)| ((unsafe |sun/misc/Unsafe|) obj field-id expected-value new-value)
  ;; FIXME: use atomics package
  (let* ((field (gethash field-id *field-offset-table*))
         (key (intern (lstring (slot-value field '|name|)) :openldk)))
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
    ((typep obj 'java-array)
     (jaref obj l))
    ((typep obj '|java/lang/Object|)
    (let* ((field (gethash l *field-offset-table*))
            (key (intern (lstring (slot-value field '|name|)) :openldk)))
       (slot-value obj key)))
    ((null obj)
     ;; FIXME: check that the field is STATIC
    (let* ((field (gethash l *field-offset-table*))
            (key (intern (lstring (slot-value field '|name|)) :openldk)))
       (let* ((clazz (slot-value field '|clazz|))
              (lname (lstring (slot-value clazz '|name|)))
              (bin-name (substitute #\/ #\. lname))
              (pkg (class-package bin-name)))
         (let ((v (slot-value (eval (intern (format nil "+static-~A+" bin-name) pkg)) key)))
           v))))
    (t (error "internal error: unrecognized object type in getObjectVolatile: ~A" obj))))

(defmethod |putObjectVolatile(Ljava/lang/Object;JLjava/lang/Object;)| ((unsafe |sun/misc/Unsafe|) obj l value)
  ;; FIXME
  (cond
    ((typep obj 'java-array)
     (setf (jaref obj l) value))
    (t (error "internal error: unrecognized object type in putObjectVolatile: ~A" obj))))

;; getObject - same as getObjectVolatile for OpenLDK (no volatile semantics needed in Lisp)
(defmethod |getObject(Ljava/lang/Object;J)| ((unsafe |sun/misc/Unsafe|) obj l)
  (cond
    ((typep obj 'java-array)
     (jaref obj l))
    ((typep obj '|java/lang/Object|)
    (let* ((field (gethash l *field-offset-table*))
            (key (intern (lstring (slot-value field '|name|)) :openldk)))
       (slot-value obj key)))
    ((null obj)
     ;; FIXME: check that the field is STATIC
    (let* ((field (gethash l *field-offset-table*))
            (key (intern (lstring (slot-value field '|name|)) :openldk)))
       (let* ((clazz (slot-value field '|clazz|))
              (lname (lstring (slot-value clazz '|name|)))
              (bin-name (substitute #\/ #\. lname))
              (pkg (class-package bin-name)))
         (let ((v (slot-value (eval (intern (format nil "+static-~A+" bin-name) pkg)) key)))
           v))))
    (t (error "internal error: unrecognized object type in getObject: ~A" obj))))

(defmethod |getLongVolatile(Ljava/lang/Object;J)| ((unsafe |sun/misc/Unsafe|) obj l)
  (cond
    ((typep obj 'java-array)
     (jaref obj l))
    ((typep obj '|java/lang/Object|)
    (let* ((field (gethash l *field-offset-table*))
            (key (intern (lstring (slot-value field '|name|)) :openldk)))
       (slot-value obj key)))
    (t (error "internal error: unrecognized object type in getLongVolatile: ~A" obj))))

(defmethod |putObject(Ljava/lang/Object;JLjava/lang/Object;)| ((unsafe |sun/misc/Unsafe|) obj l value)
  ; (format t "putObject: ~A ~A ~A~%" obj (gethash l field-offset-table) value)
  (cond
    ((typep obj 'java-array)
     (setf (jaref obj l) value))
    ((typep obj '|java/lang/Object|)
    (let* ((field (gethash l *field-offset-table*))
            (key (intern (lstring (slot-value field '|name|)) :openldk)))
       (setf (slot-value obj key) value)))
    (t (error "internal error: unrecognized object type in putObjectVolatile: ~A" obj))))

(defmethod |putOrderedObject(Ljava/lang/Object;JLjava/lang/Object;)| ((unsafe |sun/misc/Unsafe|) obj l value)
  ;; FIXME
  (|putObject(Ljava/lang/Object;JLjava/lang/Object;)| unsafe obj l value))

(defun |java/security/AccessController.getStackAccessControlContext()| ()
  ;; FIXME -- implement
  nil)

(defun |java/lang/System.initProperties(Ljava/util/Properties;)| (props)
  (dolist (prop `(("log4j2.disable.jmx" . "true")
                  ("java.specification.version" . "1.8")
                  ("java.specification.name" . "Java Platform API Specification")
                  ("java.specification.vendor" . "Oracle Corporation")
                  ("java.vm.specification.version" . "1.8")
                  ("java.vm.specification.name" . "Java Virtual Machine Specification")
                  ("java.vm.specification.vendor" . "Oracle Corporation")
                  ("java.vm.name" . "OpenLDK")
                  ("java.vm.version" . "1.0")
                  ("java.vm.vendor" . "OpenLDK")
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
  "Initialize file descriptor native IDs (no-op)."
  nil)

(defun |java/io/FileOutputStream.initIDs()| ()
  "Initialize FileOutputStream native IDs (no-op)."
  nil)

(defun |sun/nio/ch/IOUtil.initIDs()| ()
  "Initialize NIO IOUtil native IDs (no-op)."
  nil)

(defmethod |run()| (arg)
  (declare (ignore arg))
  (error "internal error"))

(defun |java/security/AccessController.doPrivileged(Ljava/security/PrivilegedExceptionAction;)| (action)
  (let ((result (|run()| action)))
    result))

(defun |java/security/AccessController.doPrivileged(Ljava/security/PrivilegedExceptionAction;Ljava/security/AccessControlContext;)| (action context)
  (declare (ignore context))
  ;; FIXME
  (let ((result (|run()| action)))
    result))

(defmethod |isAssignableFrom(Ljava/lang/Class;)| ((this |java/lang/Class|) other)
  (if (equal this other)
      1
      (if (or (eq (|isPrimitive()| this) 1) (eq (|isPrimitive()| other) 1))
          0
          (let ((this-name (lstring (slot-value this '|name|)))
                (other-name (lstring (slot-value other '|name|))))
            ;; Handle array types specially - they don't have CLOS classes
            (cond
              ;; Both are arrays
              ((and (char= (char this-name 0) #\[)
                    (char= (char other-name 0) #\[))
               ;; For arrays, check component type assignability
               ;; All arrays are assignable to Object, Cloneable, Serializable
               (if (or (string= this-name other-name)
                       (string= this-name "[Ljava.lang.Object;"))
                   1
                   ;; TODO: More complex array covariance checking
                   0))
              ;; Other is array, this is not - array assignable to Object/Cloneable/Serializable
              ((char= (char other-name 0) #\[)
               (if (member this-name '("java.lang.Object" "java.lang.Cloneable" "java.io.Serializable")
                           :test #'string=)
                   1
                   0))
              ;; Neither is array - use normal class hierarchy
              (t
               (let ((this-ldk-class (get-ldk-class-for-java-class this))
                     (other-ldk-class (get-ldk-class-for-java-class other)))
                 (if (and this-ldk-class
                          other-ldk-class
                          (find-class (intern (name other-ldk-class) (class-package (name other-ldk-class))) nil)
                          (find-class (intern (name this-ldk-class) (class-package (name this-ldk-class))) nil)
                          (closer-mop:subclassp (find-class (intern (name other-ldk-class) (class-package (name other-ldk-class))))
                                                (find-class (intern (name this-ldk-class) (class-package (name this-ldk-class))))))
                     1
                     0))))))))

(defun |java/lang/System.setIn0(Ljava/io/InputStream;)| (in-stream)
  (setf (slot-value |+static-java/lang/System+| '|in|) in-stream))

(defun |java/lang/System.setErr0(Ljava/io/PrintStream;)| (print-stream)
  (setf (slot-value |+static-java/lang/System+| '|err|) print-stream))

(defun |java/lang/System.setOut0(Ljava/io/PrintStream;)| (print-stream)
  (setf (slot-value |+static-java/lang/System+| '|out|) print-stream))

(defmethod |getIntVolatile(Ljava/lang/Object;J)| ((unsafe |sun/misc/Unsafe|) param-object param-long)
  (cond
    ((typep param-object 'java-array)
     (jaref param-object param-long))
    (t
    (let* ((field (gethash param-long *field-offset-table*))
            (key (intern (lstring (slot-value field '|name|)) :openldk)))
       (slot-value param-object key)))))

(defmethod |clone()| ((array java-array))
  (make-java-array :component-class (java-array-component-class array) :initial-contents (copy-seq (java-array-data array))))

(defun |sun/reflect/Reflection.getClassAccessFlags(Ljava/lang/Class;)| (class)
  (let ((lclass (get-ldk-class-for-java-class class)))
    (if lclass (access-flags lclass) 0)))

(defmethod |getModifiers()| ((class |java/lang/Class|))
  (if (eq (|isArray()| class) 1)
      0
      (let ((lclass (get-ldk-class-for-java-class class)))
        (if lclass (access-flags lclass) 0))))

(defmethod |getSuperclass()| ((class |java/lang/Class|))
  (let ((lclass (get-ldk-class-for-java-class class)))
    (when lclass
      (gethash (super lclass) *java-classes-by-bin-name*))))

(defmethod |getInterfaces0()| ((class |java/lang/Class|))
  ;; FIXME: do something different for interfaces?
  (let ((lclass (get-ldk-class-for-java-class class)))
    (make-java-array
     :component-class (%get-java-class-by-fq-name "java.lang.Class")
     :initial-contents
     (if lclass
         (coerce (mapcar (lambda (iname) (java-class (gethash iname *ldk-classes-by-bin-name*))) (coerce (interfaces lclass) 'list))
                 'vector)
         #()))))

(defun |sun/reflect/NativeConstructorAccessorImpl.newInstance0(Ljava/lang/reflect/Constructor;[Ljava/lang/Object;)|
    (constructor params)
  (let* ((java-class (slot-value constructor '|clazz|))
         (bin-class-name (substitute #\/ #\. (lstring (slot-value java-class '|name|)))))
    (|java/lang/Class.forName0(Ljava/lang/String;ZLjava/lang/ClassLoader;Ljava/lang/Class;)| (jstring bin-class-name) nil nil nil)
    ;; Get class package from loader - class symbols live in loader's package
    (let* ((pkg (class-package bin-class-name))
           (class-sym (intern bin-class-name pkg))
           (obj (make-instance class-sym)))
      ;; Ensure clazz metadata is populated
      (when (slot-exists-p obj '|clazz|)
        (let ((klass (%get-java-class-by-bin-name bin-class-name t)))
          (when klass (setf (slot-value obj '|clazz|) klass))))
      (if (string= "()V" (lstring (slot-value constructor '|signature|)))
          (|<init>()| obj)
          (progn
            (apply (intern
                    (lispize-method-name
                     (format nil "<init>~A" (lstring (slot-value constructor '|signature|))))
                    :openldk)
                   ;; params can be NIL for zero-arg constructor paths; guard before accessing array-data.
                   (cons obj (if params (coerce (java-array-data params) 'list) nil)))))
      ; (format t "~&NEWINSTANCE0 ~A = ~A~%" constructor obj)
      obj)))

(defmethod |ensureClassInitialized(Ljava/lang/Class;)| ((unsafe |sun/misc/Unsafe|) class)
  (let ((lclass (get-ldk-class-for-java-class class)))
    (when lclass
      (%clinit lclass))))

(defmethod |shouldBeInitialized(Ljava/lang/Class;)| ((unsafe |sun/misc/Unsafe|) class)
  (let ((lclass (get-ldk-class-for-java-class class)))
    (if (and lclass (initialized-p lclass))
        1
        0)))

(defvar *unsafe-memory-table* (make-hash-table))

(defmethod |allocateMemory(J)| ((unsafe |sun/misc/Unsafe|) size)
  (let* ((mem (sb-alien:make-alien sb-alien:char size))
         (ptr (sb-sys:sap-int (sb-alien:alien-sap mem))))
    (setf (gethash ptr *unsafe-memory-table*) mem)
    ptr))

(defmethod |putLong(JJ)| ((unsafe |sun/misc/Unsafe|) address value)
  (setf (sb-sys:sap-ref-64 (sb-sys:int-sap address) 0) value))

(defmethod |getByte(J)| ((unsafe |sun/misc/Unsafe|) address)
  (sb-sys:sap-ref-8 (sb-sys:int-sap address) 0))

(defmethod |freeMemory(J)| ((unsafe |sun/misc/Unsafe|) address)
  (sb-alien:free-alien (gethash address *unsafe-memory-table*)))

(defun |java/lang/System.mapLibraryName(Ljava/lang/String;)| (library-name)
  #+LINUX (jstring (format nil "lib~A.so" (lstring library-name)))
  #-LINUX (error "unimplemented"))

(defun |java/lang/ClassLoader.findBuiltinLib(Ljava/lang/String;)| (library-name)
  ;; FIXME
  library-name)

(defun |java/lang/ClassLoader.findLoadedClass0(Ljava/lang/String;)| (loader name)
  "Check if a class has already been loaded by this class loader.
   Returns the java.lang.Class if found, nil otherwise."
  (let* ((class-name (lstring name))
         (bin-name (substitute #\/ #\. class-name))
         (ldk-loader (get-ldk-loader-for-java-loader loader)))
    ;; Check only this loader's class map (not parent - that's Java's job)
    (when ldk-loader
      (gethash bin-name (slot-value ldk-loader 'java-classes-by-bin-name)))))

(defun |java/lang/ClassLoader.defineClass1(Ljava/lang/String;[BIILjava/security/ProtectionDomain;Ljava/lang/String;)|
    (loader name bytes offset len pd source)
  "Define a class from byte array data using the specified class loader."
  (declare (ignore pd source))
  (let* ((class-name (if name (lstring name) nil))
         (byte-data (java-array-data bytes))
         ;; Extract the relevant portion of the byte array
         (class-bytes (if (and (zerop offset) (= len (length byte-data)))
                          byte-data
                          (subseq byte-data offset (+ offset len)))))
    (%define-class-from-bytes loader class-name class-bytes)))

(defun %define-class-from-bytes (loader class-name-hint class-bytes)
  "Internal function to define a class from raw bytes.
   LOADER is the java.lang.ClassLoader.
   CLASS-NAME-HINT is optional hint (may be nil, we read actual name from bytes).
   CLASS-BYTES is the raw classfile bytes."
  (let* ((ldk-loader (get-ldk-loader-for-java-loader loader)))
    ;; Create an input stream from the bytes
    (flexi-streams:with-input-from-sequence (stream class-bytes)
      (let* ((class (read-classfile stream))
             (classname (slot-value class 'name))
             (fq-classname (substitute #\. #\/ classname)))
        ;; Verify name matches hint if provided
        (when (and class-name-hint
                   (not (string= classname (substitute #\/ #\. class-name-hint))))
          (error "Class name ~A does not match expected ~A" classname class-name-hint))

        ;; Check if class already defined - return existing java.lang.Class
        ;; This mirrors JVM behavior where defineClass on existing class is an error,
        ;; but we return the existing class instead to handle warm-up scenarios
        ;; Check both java.lang.Class and LDK class to prevent double loading
        (let ((existing-java-class (or (gethash classname *java-classes-by-bin-name*)
                                       (when ldk-loader
                                         (gethash classname (slot-value ldk-loader 'java-classes-by-bin-name))))))
          (when existing-java-class
            (return-from %define-class-from-bytes existing-java-class)))
        ;; Also check for existing LDK class (may have been loaded via classload path)
        (let ((existing-ldk-class (or (gethash classname *ldk-classes-by-bin-name*)
                                      (when ldk-loader
                                        (gethash classname (slot-value ldk-loader 'ldk-classes-by-bin-name))))))
          (when existing-ldk-class
            (return-from %define-class-from-bytes (java-class existing-ldk-class))))

        ;; Set the loader on the class
        (setf (slot-value class 'ldk-loader) ldk-loader)

        ;; Store in loader's class maps
        (setf (gethash classname (slot-value ldk-loader 'ldk-classes-by-bin-name)) class)
        (setf (gethash fq-classname (slot-value ldk-loader 'ldk-classes-by-fq-name)) class)
        ;; Also store in global tables for backward compatibility
        (setf (gethash classname *ldk-classes-by-bin-name*) class)
        (setf (gethash fq-classname *ldk-classes-by-fq-name*) class)

        ;; Create java.lang.Class object
        (let ((klass (%make-java-instance "java/lang/Class"))
              (cname (jstring fq-classname)))
          (with-slots (|name| |classLoader|) klass
            (setf |name| cname)
            (setf |classLoader| loader))
          (setf (java-class class) klass)
          (setf (gethash classname (slot-value ldk-loader 'java-classes-by-bin-name)) klass)
          (setf (gethash fq-classname (slot-value ldk-loader 'java-classes-by-fq-name)) klass)
          ;; Also store in global tables for backward compatibility
          (setf (gethash classname *java-classes-by-bin-name*) klass)
          (setf (gethash fq-classname *java-classes-by-fq-name*) klass)

          ;; Emit and evaluate the class definition
          (let ((code (emit-<class> class ldk-loader)))
            (%eval code))

          ;; Load super and interfaces (using this loader for resolution)
          (let ((super (slot-value class 'super))
                (interfaces (slot-value class 'interfaces)))
            (when super (classload super loader))
            (when interfaces
              (dolist (iface (coerce interfaces 'list))
                (classload iface loader))))

          ;; Emit the class initializer - use loader's package for class symbols
          (let* ((pkg (loader-package ldk-loader))
                 (lisp-class (find-class (intern (substitute #\/ #\. classname) pkg))))
            (closer-mop:finalize-inheritance lisp-class)
            (let ((icc (append (list 'defun (intern (format nil "%clinit-~A" (substitute #\/ #\. classname)) pkg) (list))
                               (loop for k in (reverse (closer-mop:class-precedence-list lisp-class))
                                     ;; Get each class's package from its symbol's package
                                     for clinit-pkg = (symbol-package (class-name k))
                                     for clinit-function = (intern (format nil "~a.<clinit>()" (class-name k)) clinit-pkg)
                                     when (fboundp clinit-function)
                                       collect (let ((ldkclass (%get-ldk-class-by-bin-name (format nil "~A" (class-name k)) t ldk-loader)))
                                                 (when ldkclass
                                                   (list 'unless (list 'initialized-p ldkclass)
                                                         (list 'setf (list 'initialized-p ldkclass) t)
                                                         (list clinit-function))))))))
              (%eval icc)))

          ;; Return the java.lang.Class
          klass)))))

(defmethod |load(Ljava/lang/String;Z)| ((loader t) library-name is-builtin)
  (when *debug-trace*
    (format t "FIXME: ~A loading ~A~%" loader library-name))
  (setf (slot-value loader '|loaded|) 1)
  nil)

(defmethod |sun/misc/Signal.findSignal(Ljava/lang/String;)| (signal-name)
  (let ((sname (lstring signal-name)))
    (cond
      ((string= sname "HUP") 1)
      ((string= sname "INT") 2)
      ((string= sname "KILL") 9)
      ((string= sname "TERM") 15)
      (t (error "unimplemented")))))

(defun |sun/misc/Signal.handle0(IJ)| (sig native-h)
  (declare (ignore sig)
           (ignore native-h))
  ;; FIXME
  1)

(defmethod |notifyAll()| ((objref |java/lang/Object|))
  (let* ((monitor (%get-monitor objref))
         (mutex (mutex monitor))
         (cv (condition-variable monitor))
         (current-thread (bordeaux-threads:current-thread)))
    (bordeaux-threads:with-lock-held (mutex)
      (unless (eq (owner monitor) current-thread)
        (error (%lisp-condition (%make-throwable '|java/lang/IllegalMonitorStateException|))))
      (when (wait-set monitor)
        (setf (wait-set monitor) nil)
        (sb-thread:condition-broadcast cv)))))

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
      (let ((fnf (%make-java-instance "java/io/FileNotFoundException")))
        (|<init>(Ljava/lang/String;)| fnf filename)
        (error (%lisp-condition fnf))))))

(defmethod |length()| ((raf |java/io/RandomAccessFile|))
  (file-length (slot-value raf '|fd|)))

(defmethod |getFilePointer()| ((raf |java/io/RandomAccessFile|))
  (file-position (slot-value raf '|fd|)))

(defmethod |read0()| ((raf |java/io/RandomAccessFile|))
  (let ((byte (read-byte (slot-value raf '|fd|) nil nil)))
    (if byte byte -1)))

(defmethod |seek0(J)| ((raf |java/io/RandomAccessFile|) position)
  (file-position (slot-value raf '|fd|) position))

(defmethod |readBytes([BII)| ((raf |java/io/RandomAccessFile|) byte-array offset length)
  (let ((in-stream (slot-value raf '|fd|))
        (bytes-read 0))
    (loop for i from offset below (+ offset length)
          for byte = (read-byte in-stream nil nil) ; Read a byte, return NIL on EOF
          while byte
          do (setf (jaref byte-array i) byte)
             (incf bytes-read))  ; Count bytes read
    bytes-read))

(defmethod |open0(Ljava/lang/String;)| ((fis |java/io/FileInputStream|) filename)
  (handler-case
      (setf (slot-value fis '|fd|) (open (lstring filename)
                                         :element-type '(unsigned-byte 8)
                                         :direction :input))
    ((or sb-ext:file-does-not-exist sb-int:simple-file-error) (e)
      (declare (ignore e))
      (let ((fnf (%make-java-instance "java/io/FileNotFoundException")))
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
          do (setf (jaref byte-array i) byte)
             (incf bytes-read))  ; Count bytes read
    bytes-read))

(defmethod |available0()| ((fis |java/io/FileInputStream|))
  ;; FIXME - may throw exception
  (let* ((in-stream (slot-value fis '|fd|))
         (remaining (- (file-length in-stream) (file-position in-stream))))
    remaining))

(defmethod |isInstance(Ljava/lang/Object;)| ((this |java/lang/Class|) objref)
  (let* ((class-name (lstring (slot-value this '|name|)))
         (normalized-name (substitute #\/ #\. class-name))
         ;; Get class's loader package for correct type lookup
         (pkg (class-package normalized-name))
         (class-symbol (intern normalized-name pkg)))
    ;; Handle native Lisp integers for Java integral wrapper types
    (cond
      ((and (integerp objref)
            (member normalized-name '("java/lang/Integer" "java/lang/Long"
                                      "java/lang/Short" "java/lang/Byte"
                                      "java/lang/Number")
                    :test #'string=))
       1)
      ;; Standard typep check for CLOS objects
      ((typep objref class-symbol) 1)
      (t 0))))

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
  (let* ((file-descriptor (slot-value fos '|fd|))
         (fd (if (and file-descriptor (slot-exists-p file-descriptor '|fd|))
                 (slot-value file-descriptor '|fd|)
                 file-descriptor)))
    (cond
      ((eq fd 1)
       (write-sequence (%convert-to-unsigned-8-bit (java-array-data byte-array)) *standard-output* :start offset :end (+ offset length)))
      ((eq fd 2)
       (write-sequence (%convert-to-unsigned-8-bit (java-array-data byte-array)) *error-output* :start offset :end (+ offset length)))
      ((streamp fd)
       (write-sequence (%convert-to-unsigned-8-bit (java-array-data byte-array)) fd :start offset :end (+ offset length)))
      (t
       (error "unimplemented file descriptor ~A in FileOutputStream.writeBytes" fd)))))

(defmethod |open0(Ljava/lang/String;Z)| ((fos |java/io/FileOutputStream|) filename append?)
  (handler-case
      (let* ((stream (open (lstring filename)
                           :element-type '(unsigned-byte 8)
                           :direction :output
                           :if-does-not-exist :create
                           :if-exists (if (and append? (not (zerop append?))) :append :supersede)))
             (fd (slot-value fos '|fd|)))
        ;; FileOutputStream stores a FileDescriptor; stash the stream in its fd
        ;; slot when available, otherwise keep it directly.
        (if (and fd (slot-exists-p fd '|fd|))
            (setf (slot-value fd '|fd|) stream)
            (setf (slot-value fos '|fd|) stream)))
    ((or sb-ext:file-does-not-exist sb-int:simple-file-error) (e)
      (declare (ignore e))
      (let ((fnf (%make-java-instance "java/io/FileNotFoundException")))
        (|<init>(Ljava/lang/String;)| fnf filename)
        (error (%lisp-condition fnf))))))

(defmethod |close0()| (this)
  "Native close for file streams."
  (when (slot-exists-p this '|fd|)
    (let* ((fd-holder (slot-value this '|fd|))
           (fd (if (and fd-holder (slot-exists-p fd-holder '|fd|))
                   (slot-value fd-holder '|fd|)
                   fd-holder)))
      (when (streamp fd)
        (close fd))))
  nil)

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
      (declare (ignore e))
      0)))

(defmethod |getLength(Ljava/io/File;)| ((this |java/io/UnixFileSystem|) file)
  (with-open-file (stream (lstring (slot-value file '|path|)) :element-type '(unsigned-byte 8))
    (file-length stream)))

(defmethod |list(Ljava/io/File;)| ((this |java/io/UnixFileSystem|) file)
  (handler-case
      (let* ((path (lstring (slot-value file '|path|)))
             (dir (uiop:directory-files (uiop:ensure-directory-pathname path)))
             (files (mapcar (lambda (p) (jstring (file-namestring p))) dir)))
        (when files
          (make-java-array
           :component-class (%get-java-class-by-bin-name "java/lang/String")
           :initial-contents files)))
    (error (e)
      (declare (ignore e))
      nil)))

(defun |java/security/AccessController.doPrivileged(Ljava/security/PrivilegedAction;Ljava/security/AccessControlContext;)| (action context)
  (declare (ignore context))
  ;; FIXME
  (let ((result (|run()| action)))
    result))

(defun |java/net/InetAddress.init()| ()
  ;; FIXME
  nil)

(defun |java/io/ObjectStreamClass.initNative()| ()
  ;; FIXME
  nil)

(defmethod |java/lang/System.gc()| ()
  (trivial-garbage:gc))

(defun |java/lang/Thread.sleep(J)| (milliseconds)
  "Sleep for the specified milliseconds, checking for interruption."
  ;; Get current thread
  (let* ((current-lisp-thread (bordeaux-threads:current-thread))
         (current-java-thread (gethash current-lisp-thread *lisp-to-java-threads*)))
    ;; Check if interrupted before sleeping
    (when (and current-java-thread
               (gethash current-java-thread *thread-interrupted*))
      ;; Clear interrupt flag and throw InterruptedException
      (setf (gethash current-java-thread *thread-interrupted*) nil)
      (let ((exc (%make-java-instance "java/lang/InterruptedException")))
        (|<init>()| exc)
        (error (%lisp-condition exc))))
    ;; Sleep (this can be interrupted by interrupt0)
    (sleep (/ milliseconds 1000.0))
    ;; Check if interrupted after sleeping
    (when (and current-java-thread
               (gethash current-java-thread *thread-interrupted*))
      ;; Clear interrupt flag and throw InterruptedException
      (setf (gethash current-java-thread *thread-interrupted*) nil)
      (let ((exc (%make-java-instance "java/lang/InterruptedException")))
        (|<init>()| exc)
        (error (%lisp-condition exc))))))

(defun |java/lang/ProcessEnvironment.environ()| ()
  ;; FIXME: don't force utf-8 encoding
  (let ((env (remove-if (lambda (e) (not (find #\= e))) (sb-ext:posix-environ))))
    (let ((jenvs (make-java-array :size (* 2 (length env)))))
      (loop for kv in env
            for i from 0 by 2
            for p = (position #\= kv)
            do (progn
                 (setf (jaref jenvs i)
                       (make-java-array :initial-contents (flexi-streams:string-to-octets (subseq kv 0 p) :external-format :utf-8)))
                 (setf (jaref jenvs (+ i 1))
                       (make-java-array :initial-contents (flexi-streams:string-to-octets (subseq kv (1+ p)) :external-format :utf-8)))))
      jenvs)))

(defun |java/lang/System.identityHashCode(Ljava/lang/Object;)| (objref)
  ;; Hash down the 64-bit SXHASH to 32-bits.
  (unsigned-to-signed-integer (cl-murmurhash:murmurhash (sxhash objref))))

(defun |java/lang/Thread.yield()| ()
  ;; FIXME
  nil)

(defmethod |start0()| ((thread |java/lang/Thread|))
  "Start a new Lisp thread that executes the Thread's run() method."
  (let ((lisp-thread
          (bordeaux-threads:make-thread
           (lambda ()
             ;; Register this Lisp thread with the Java Thread
             (setf (gethash (bordeaux-threads:current-thread) *lisp-to-java-threads*) thread)
             ;; Call the Thread's run() method
             (handler-case
                 (|run()| thread)
               (error (e)
                 (format *error-output* "~&Thread ~A terminated with error: ~A~%" thread e))))
           :name (format nil "Java-Thread-~A" (slot-value thread '|name|)))))
    ;; Store the Lisp thread in our mapping
    (setf (gethash thread *java-threads*) lisp-thread)))

(defmethod |interrupt0()| ((thread |java/lang/Thread|))
  "Interrupt the thread by signaling the underlying Lisp thread."
  ;; In Java 8, the interrupted status is maintained by the VM, not as a field.
  ;; We maintain it in the *thread-interrupted* hash table.
  (setf (gethash thread *thread-interrupted*) t)
  ;; If the thread has an associated Lisp thread, interrupt it.
  (let ((lisp-thread (gethash thread *java-threads*)))
    (when lisp-thread
      ;; Interrupt the Lisp thread to wake it from blocking operations
      (bordeaux-threads:interrupt-thread
       lisp-thread
       (lambda ()
         ;; Check if interrupted flag is set and throw InterruptedException
         (when (gethash thread *thread-interrupted*)
           (let ((exc (%make-java-instance "java/lang/InterruptedException")))
             (|<init>()| exc)
             (error (%lisp-condition exc)))))))))

(defmethod |notify()| ((objref |java/lang/Object|))
  (let* ((monitor (%get-monitor objref))
         (mutex (mutex monitor))
         (cv (condition-variable monitor))
         (current-thread (bordeaux-threads:current-thread)))
    (bordeaux-threads:with-lock-held (mutex)
      (unless (eq (owner monitor) current-thread)
        (error (%lisp-condition (%make-throwable '|java/lang/IllegalMonitorStateException|))))
      (when (wait-set monitor)
        (pop (wait-set monitor))
        (sb-thread:condition-broadcast cv)))))

(defun |java/util/concurrent/atomic/AtomicLong.VMSupportsCS8()| ()
  0)

(defun |sun/reflect/NativeMethodAccessorImpl.invoke0(Ljava/lang/reflect/Method;Ljava/lang/Object;[Ljava/lang/Object;)| (method object args)
  (when *debug-trace*
    (format t "~&~V@A trace: entering sun/reflect/NativeMethodAccessorImpl.invoke0(Ljava/lang/reflect/Method;Ljava/lang/Object;[Ljava/lang/Object;)~A~%"
            (incf *call-nesting-level* 1) "*"
            (list method object args)))
  (unwind-protect
       (progn
         ;; Unbox primitive wrapper types
         (when args
           (dotimes (i (length (java-array-data args)))
             (let ((arg (jaref args i)))
               (cond
                 ((typep arg '|java/lang/Integer|)
                  (setf (jaref args i) (slot-value arg '|value|)))
                 ((typep arg '|java/lang/Long|)
                  (setf (jaref args i) (slot-value arg '|value|)))
                 ((typep arg '|java/lang/Boolean|)
                  (setf (jaref args i) (slot-value arg '|value|)))
                 ((typep arg '|java/lang/Byte|)
                  (setf (jaref args i) (slot-value arg '|value|)))
                 ((typep arg '|java/lang/Short|)
                  (setf (jaref args i) (slot-value arg '|value|)))
                 ((typep arg '|java/lang/Character|)
                  (setf (jaref args i) (slot-value arg '|value|)))
                 ((typep arg '|java/lang/Float|)
                  (setf (jaref args i) (slot-value arg '|value|)))
                 ((typep arg '|java/lang/Double|)
                  (setf (jaref args i) (slot-value arg '|value|)))))))
         (let* ((java-class (slot-value method '|clazz|))
                (class-name (substitute #\/ #\. (lstring (slot-value java-class '|name|))))
                (is-static (not (eq 0 (logand #x8 (slot-value method '|modifiers|)))))
                ;; For static methods, use the class's package; for instance methods use :openldk
                (java-loader (slot-value java-class '|classLoader|))
                (ldk-loader (get-ldk-loader-for-java-loader java-loader))
                (method-name (lispize-method-name
                              (concatenate 'string
                                           class-name
                                           "."
                                           (lstring (slot-value method '|name|))
                                           (lstring (slot-value method '|signature|)))))
                ;; Static methods are in the class's loader package, instance methods in :openldk
                (pkg (if is-static
                         (class-package class-name ldk-loader)
                         (find-package :openldk)))
                (result (apply (intern method-name pkg)
                               (if is-static
                                   (if args (coerce (java-array-data args) 'list) nil)
                                   (cons object (if args (coerce (java-array-data args) 'list) nil))))))
           (when *debug-trace*
             (format t "~&~V@A trace: result = ~A~%"
                     *call-nesting-level* "*" result))
           result))
    (when *debug-trace*
      (incf *call-nesting-level* -1))))

(defun |java/lang/reflect/Array.newArray(Ljava/lang/Class;I)| (class size)
  ;; FIXME
  (make-java-array :size size
                   :component-class class
                   :initial-element nil))

(defmethod |findLoadedClass0(Ljava/lang/String;)| ((loader |java/lang/ClassLoader|) name)
  ;; FIXME
  (gethash (lstring name) *java-classes-by-fq-name*))

(defmethod |findBootstrapClass(Ljava/lang/String;)| ((loader |java/lang/ClassLoader|) name)
  ;; FIXME
  (handler-case
      (let ((ldk-class (classload (substitute #\/ #\. (lstring name)))))
        (java-class ldk-class))
    (condition (c)
      (declare (ignore c))
      nil)))

(defun %make-url-from-string (url-string)
  "Create a java.net.URL object from URL-STRING."
  (let ((url (%make-java-instance "java/net/URL")))
    (|<init>(Ljava/lang/String;)| url (jstring url-string))
    url))

(defmethod |getBootstrapResource(Ljava/lang/String;)| ((loader |java/lang/ClassLoader|) name)
  "Find a resource by NAME on the bootstrap classpath."
  (declare (ignore loader))
  (let ((resource-name (lstring name)))
    (when-let (url-string (get-resource-url-on-classpath resource-name))
      (%make-url-from-string url-string))))

(defmethod |getBootstrapResource(Ljava/lang/String;)| ((loader (eql nil)) name)
  "Find a resource by NAME on the bootstrap classpath (static call)."
  (let ((resource-name (lstring name)))
    (when-let (url-string (get-resource-url-on-classpath resource-name))
      (%make-url-from-string url-string))))

(defun |java/lang/ClassLoader.getBootstrapResource(Ljava/lang/String;)| (name)
  "Static native method to find a resource on the bootstrap classpath."
  (let ((resource-name (lstring name)))
    (when-let (url-string (get-resource-url-on-classpath resource-name))
      (%make-url-from-string url-string))))

(defclass/std <resource-input-stream> (|java/io/InputStream|)
  ((lisp-stream :std nil))
  (:documentation "InputStream wrapping a Lisp flexi-stream for resource reading."))

(defmethod |read()| ((this <resource-input-stream>))
  "Read a single byte from the resource stream."
  (let ((byte (read-byte (slot-value this 'lisp-stream) nil nil)))
    (if byte byte -1)))

(defmethod |read([BII)| ((this <resource-input-stream>) byte-array offset length)
  "Read up to LENGTH bytes into BYTE-ARRAY starting at OFFSET."
  (let ((stream (slot-value this 'lisp-stream))
        (bytes-read 0))
    (loop for i from offset below (+ offset length)
          for byte = (read-byte stream nil nil)
          while byte
          do (progn
               (setf (jaref byte-array i) byte)
               (incf bytes-read)))
    (if (zerop bytes-read) -1 bytes-read)))

(defmethod |close()| ((this <resource-input-stream>))
  "Close the resource stream."
  (when-let (stream (slot-value this 'lisp-stream))
    (close stream)
    (setf (slot-value this 'lisp-stream) nil)))

(defun |java/lang/ClassLoader.getSystemResourceAsStream(Ljava/lang/String;)| (name)
  "Get a system resource as an InputStream."
  (let ((resource-name (lstring name)))
    (when-let (stream (open-resource-on-classpath resource-name))
      (make-instance '<resource-input-stream> :lisp-stream stream))))

(defun %parse-jar-url (url-string)
  "Parse a jar: URL and return (jar-path entry-path) or NIL.
   Format: jar:file:/path/to/file.jar!/path/inside/jar"
  (when (starts-with? "jar:file:" url-string)
    (let ((excl-pos (search "!/" url-string)))
      (when excl-pos
        (let ((jar-path (subseq url-string 9 excl-pos))  ; Skip "jar:file:"
              (entry-path (subseq url-string (+ excl-pos 2))))  ; Skip "!/"
          (values jar-path entry-path))))))

(defun %open-jar-url-stream (url-string)
  "Open an InputStream for a jar: URL."
  (multiple-value-bind (jar-path entry-path) (%parse-jar-url url-string)
    (when (and jar-path entry-path)
      (handler-case
          (let ((zf (zip:open-zipfile jar-path)))
            (when-let (ze (zip:get-zipfile-entry entry-path zf))
              (let ((contents (zip:zipfile-entry-contents ze)))
                (zip:close-zipfile zf)
                (flexi-streams:make-in-memory-input-stream contents))))
        (error ()
          nil)))))

;; Note: |openStream()| for java/net/URL is defined in url.lisp after the class is loaded

(defun |java/io/UnixFileSystem.initIDs()| ()
  ;; FIXME
  nil)

(defun |java/util/LinkedHashMap.hash(Ljava/lang/Object;)| (obj)
  ;; FIXME: the compiler should not generate calls to LinkedHashMap.hash.
  ;; It's provided by the parent class.
  ;; This is a temp workaround.
  (|java/util/HashMap.hash(Ljava/lang/Object;)| obj))

(defmethod |canonicalize0(Ljava/lang/String;)| ((ufs |java/io/UnixFileSystem|) filename)
  (declare (ignore ufs))
  (let ((path-str (lstring filename)))
    (when *debug-trace*
      (format t "~&DEBUG: canonicalize0 called with: ~S~%" path-str))
    (handler-case
        (let ((result (jstring (namestring (truename path-str)))))
          (when *debug-trace*
            (format t "~&DEBUG: canonicalize0 truename succeeded: ~S -> ~S~%"
                    path-str (lstring result)))
          result)
      (error (e)
        (when *debug-trace*
          (format t "~&DEBUG: canonicalize0 truename failed: ~S error: ~A~%"
                  path-str e))
        ;; If file doesn't exist, manually resolve to absolute path
        (let* ((pathname (uiop:parse-unix-namestring path-str))
               (absolute-path (if (uiop:absolute-pathname-p pathname)
                                  pathname
                                  (merge-pathnames pathname (uiop:getcwd))))
               (result (jstring (uiop:native-namestring absolute-path))))
          (when *debug-trace*
            (format t "~&DEBUG: canonicalize0 fallback: ~S -> ~S~%"
                    path-str (lstring result)))
          result)))))

(defmethod |getLastModifiedTime(Ljava/io/File;)| ((ufs |java/io/UnixFileSystem|) file)
  (declare (ignore ufs))
  (* (org.shirakumo.file-attributes:modification-time
      (lstring (slot-value file '|path|)))
     1000))

(defun |sun/misc/Perf.registerNatives()| ()
  ;; FIXME
  nil)

(defmethod |createLong(Ljava/lang/String;IIJ)| (perf name variability units value)
  (classload "java/nio/DirectByteBuffer")
  (let* ((dbb (%make-java-instance "java/nio/DirectByteBuffer"))
         (mem (sb-alien:make-alien sb-alien:long 1))
         (ptr (sb-sys:sap-int (sb-alien:alien-sap mem))))
    (setf (sb-alien:deref mem 0) value)
    (|<init>(JI)| dbb ptr 8)
    dbb))

(defmethod |getInt(Ljava/lang/Object;J)|((unsafe |sun/misc/Unsafe|) objref ptr)
  (declare (ignore unsafe))
  (let* ((field (gethash ptr *field-offset-table*))
         (key (intern (lstring (slot-value field '|name|)) :openldk)))
    (slot-value objref key)))

(defmethod |putLong(Ljava/lang/Object;JJ)|((unsafe |sun/misc/Unsafe|) objref ptr value)
  (declare (ignore unsafe))
  (let* ((field (gethash ptr *field-offset-table*))
         (key (intern (lstring (slot-value field '|name|)) :openldk)))
    (setf (slot-value objref key) value)))

(defmethod |putInt(Ljava/lang/Object;JI)|((unsafe |sun/misc/Unsafe|) objref ptr value)
  (declare (ignore unsafe))
  (let* ((field (gethash ptr *field-offset-table*))
         (key (intern (lstring (slot-value field '|name|)) :openldk)))
    (setf (slot-value objref key) value)))

(defmethod |getLong(Ljava/lang/Object;J)|((unsafe |sun/misc/Unsafe|) objref ptr)
  (declare (ignore unsafe))
  (let* ((field (gethash ptr *field-offset-table*))
         (key (intern (lstring (slot-value field '|name|)) :openldk)))
    (slot-value objref key)))

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
  nil)

(defun |java/lang/Shutdown.halt0(I)| (status)
  (unless *ignore-quit*
    (uiop:quit status t)))

(defmethod |getRawAnnotations()| ((class |java/lang/Class|))
  (let ((lclass (get-ldk-class-for-java-class class)))
    (when (and lclass (attributes lclass))
      (gethash "RuntimeVisibleAnnotations" (attributes lclass)))))

(defun |java/awt/image/ColorModel.initIDs()| ()
  ;; FIXME
  nil)

(defun |java/net/InetAddressImplFactory.isIPv6Supported()| ()
  0)

(defun |java/awt/image/IndexColorModel.initIDs()| ()
  ;; FIXME
  nil)

(defun |java/awt/image/Raster.initIDs()| ()
  ;; FIXME
  nil)

(defun |java/awt/image/SampleModel.initIDs()| ()
  ;; FIXME
  nil)

(defun |java/util/zip/Deflater.initIDs()| ()
  ;; FIXME
  nil)

(defun |java/util/zip/Inflater.initIDs()| ()
  ;; FIXME
  nil)

(defun |java/security/SystemConfigurator.getSystemFIPSEnabled()| ()
  0)

(defun |java/lang/Package.getSystemPackage0(Ljava/lang/String;)| (name)
  (gethash (lstring name) *packages*))

(defun |java/util/zip/Inflater.init(Z)| (v)
  (declare (ignore v))
  ;; FIXME
  nil)

(defun |java/lang/Thread.holdsLock(Ljava/lang/Object;)| (objref)
  (let ((monitor (%get-monitor objref))
        (current-thread (bordeaux-threads:current-thread)))
    (if (eq (owner monitor) current-thread)
        1 0)))

(defun |sun/nio/ch/IOUtil.iovMax()| ()
  0)

(defun |sun/nio/ch/FileChannelImpl.initIDs()| ()
  nil)

(defun |sun/nio/ch/NativeThread.init()| ()
  nil)

(defun |sun/nio/ch/NativeThread.current()| ()
  -1)

(defun |java/nio/Bits.pageSize()| ()
  4096)

(defmethod |pageSize()| ((unsafe |sun/misc/Unsafe|))
  4096)

(defmethod |storeFence()| ((unsafe |sun/misc/Unsafe|))
  "Ensures that stores before the fence will not be reordered with stores after the fence."
  (declare (ignore unsafe))
  (sb-thread:barrier (:memory)))

(defmethod |loadFence()| ((unsafe |sun/misc/Unsafe|))
  "Ensures that loads before the fence will not be reordered with loads after the fence."
  (declare (ignore unsafe))
  (sb-thread:barrier (:memory)))

(defmethod |fullFence()| ((unsafe |sun/misc/Unsafe|))
  "Ensures that loads and stores before the fence will not be reordered with those after the fence."
  (declare (ignore unsafe))
  (sb-thread:barrier (:memory)))

(defun |sun/nio/ch/FileDispatcherImpl.init()| ()
  ;; FIXME
  nil)

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
  (declare (ignore off)
           (ignore len)
           (ignore size))
  ;; FIXME
  nil)

(defmethod |copyMemory(Ljava/lang/Object;JLjava/lang/Object;JJ)| ((unsafe |sun/misc/Unsafe|) source source-offset dest dest-offset length)
  (assert (null source))
  (let ((sap (sb-alien:alien-sap (gethash source-offset *unsafe-memory-table*)))
        (bytes-read 0))
    (loop for i below length
          do (let ((offset-sap (sb-sys:sap+ sap i)))
               (setf (jaref dest (+ dest-offset i)) (sb-sys:sap-ref-8 offset-sap 0))
               (incf bytes-read)))  ; Count bytes read
    bytes-read))

(defun |sun/nio/fs/UnixNativeDispatcher.init()| ()
  ;; FIXME
  nil)

(defun |sun/nio/fs/UnixNativeDispatcher.getcwd()| ()
  (make-java-array
   :component-class (%get-ldk-class-by-fq-name "byte")
   :initial-contents (flexi-streams:string-to-octets (namestring (uiop:getcwd)) :external-format :utf-8)))

(defmethod |getUTF8At0(Ljava/lang/Object;I)| ((this |sun/reflect/ConstantPool|) cp index)
  (let* ((cp (constant-pool (ldk-class cp)))
         (s (format nil "~A" (emit (aref cp index) cp))))
    (jstring s)))

(defmethod |getIntAt0(Ljava/lang/Object;I)| ((this |sun/reflect/ConstantPool|) cp index)
  (let* ((cp (constant-pool (ldk-class cp)))
         (i (slot-value (aref cp index) 'value)))
    i))

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
    (java-class (%classload-from-stream (substitute #\/ #\. (lstring class-name)) stream class-loader ldk-loader))))

(defmethod |defineClass(Ljava/lang/String;[BIILjava/lang/ClassLoader;Ljava/security/ProtectionDomain;)|
    ((unsafe |sun/misc/Unsafe|) class-name data offset length class-loader protection-domain)
  (declare (ignore protection-domain))
  (let* ((ldk-loader (get-ldk-loader-for-java-loader class-loader))
         (stream (make-instance 'byte-array-input-stream :array data :start offset :end (+ offset length))))
    (java-class (%classload-from-stream (substitute #\/ #\. (lstring class-name)) stream class-loader ldk-loader))))

(defmethod |defineClass1(Ljava/lang/String;[BIILjava/security/ProtectionDomain;Ljava/lang/String;)|
    ((class-loader |java/lang/ClassLoader|) class-name data offset length protection-domain source)
  (declare (ignore source)
           (ignore protection-domain))
  (let* ((ldk-loader (get-ldk-loader-for-java-loader class-loader))
         (stream (make-instance 'byte-array-input-stream :array data :start offset :end (+ offset length))))
    (java-class (%classload-from-stream (substitute #\/ #\. (lstring class-name)) stream class-loader ldk-loader))))

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
  ;; FIXME
  nil)

(defmethod |getOption(I)| ((this |java/net/SocketOptions|) option-id)
  (declare (ignore this)
           (ignore option-id))
  ;; FIXME
  (slot-value |+static-java/lang/Boolean+| '|TRUE|))

;; FIXME -- am I picking up the wrong static method?  Found with log4j.
(defun |sun/util/calendar/BaseCalendar.getDayOfWeekDateOnOrBefore(JI)| (l i)
  (|sun/util/calendar/AbstractCalendar.getDayOfWeekDateOnOrBefore(JI)| l i))

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
     100000000                       ;; usageThreshold -- FIXME
     100000000)                      ;; gcThreshold -- FIXME

    ;; Return it as a Java array of the interface type
    (make-java-array :component-class (%get-java-class-by-bin-name "java/lang/management/MemoryPoolMXBean")
                     :initial-contents (list mp-mxbean))))

(defun |jdk/internal/platform/CgroupMetrics.isUseContainerSupport()| ()
  0)

(defmethod |getStartupTime()| ((this |sun/management/VMManagementImpl|))
  ;; FIXME
  555)

(defmethod |read0()| ((this |java/io/FileInputStream|))
  (let ((in-stream (slot-value this '|fd|)))
    (read-byte in-stream nil nil)))

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
      ((char= (char name 0) #\[)
       (substitute #\/ #\. name))
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
  ;; FIXME
  nil)

(defun |java/lang/invoke/MethodHandleNatives.getConstant(I)| (i)
  (declare (ignore i))
  0)

(defun |java/lang/invoke/MethodHandleNatives.getNamedCon(I[Ljava/lang/Object;)| (which objarray)
  ;; FIXME
  (declare (ignore objarray))
  (assert (zerop which))
  0)

(defun find-method-in-class (class name)
  (find-if (lambda (m)
             (string= (slot-value m 'name) name))
           (coerce (slot-value class 'methods) 'list)))

(defun |java/lang/invoke/MethodHandleNatives.resolve(Ljava/lang/invoke/MemberName;Ljava/lang/Class;)| (member-name klass)
  (declare (ignore klass))
  ;; FIXME
  (let* ((member-class (slot-value member-name '|clazz|))
         (ldk-class (get-ldk-class-for-java-class member-class))
         (method (when ldk-class
                   (find-method-in-class ldk-class (lstring (slot-value member-name '|name|))))))
    (when method
      (setf (slot-value member-name '|flags|) (logior (slot-value member-name '|flags|) (slot-value method 'access-flags)))))
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
         (ldk-loader (get-ldk-loader-for-java-loader java-loader)))
    (java-class (%classload-from-stream (format nil "~A/~A" (substitute #\/ #\. (lstring (slot-value clazz '|name|))) (gensym "anonymous-class-")) stream java-loader ldk-loader))))

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

          ;; Invoke the method
          ;; For LambdaForm methods, always pass the MethodHandle as first arg
          (apply lisp-method-name method-handle args))))))

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
  ;; Help diagnose missing or incomplete metadata during lambda bootstrapping.
  (format t "~&[findStatic] klass=~A name=~A method-type=~A~%" klass name method-type)
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

(defun |java/lang/invoke/MethodHandles$Lookup.findSpecial(Ljava/lang/Class;Ljava/lang/String;Ljava/lang/invoke/MethodType;Ljava/lang/Class;)| (lookup refc name method-type special-caller)
  "Create a DirectMethodHandle for invokespecial method invocation.
   Used for private methods, constructors, and super calls in lambda metafactory."
  (declare (ignore lookup special-caller))
  (format t "~&[findSpecial] refc=~A name=~A method-type=~A~%" refc name method-type)
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
  (format t "~&[findConstructor] refc=~A method-type=~A~%" refc method-type)
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
  (format t "~&[findVirtual] refc=~A name=~A method-type=~A~%" refc name method-type)
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

(defun %invoke-from-member-name (member-name &rest args)
  "Invoke a method described by a MemberName with the given arguments.
   This is the core implementation for linkToStatic, linkToVirtual, etc."
  ;; Extract method information from the MemberName
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

    ;; Get the class name and method name as strings
    (let* ((class-name-raw (lstring (slot-value clazz '|name|)))
           ;; Class names from Class.getName() use . separator, but we need /
           (class-name (substitute #\/ #\. class-name-raw))
           (method-name (lstring name))
           ;; type can be either a String (descriptor) or a MethodType
           (method-type (if (and type (typep type '|java/lang/String|))
                            ;; It's already a string descriptor
                            (lstring type)
                            ;; It's a MethodType, build descriptor from rtype/ptypes
                            (when type
                              (let ((rtype (slot-value type '|rtype|))
                                    (ptypes (when (slot-exists-p type '|ptypes|)
                                              (slot-value type '|ptypes|))))
                                (%build-method-descriptor rtype ptypes)))))
           ;; Extract the reference kind from flags
           ;; REF_invokeStatic = 6, REF_invokeSpecial = 7, REF_newInvokeSpecial = 8
           (ref-kind (ash (logand flags #x0F000000) -24))
           (is-constructor (= ref-kind 8)))

      ;; Handle constructors specially - create instance, call init, return instance
      (if is-constructor
            (progn
              ;; Load the class and create a new instance
              (classload class-name)
              (let* ((pkg (class-package class-name))
                     (lisp-class-name (intern class-name pkg))
                     (instance (make-instance lisp-class-name)))
                ;; Constructor descriptors always return void - replace any return type with V
                ;; The MethodType from findConstructor has the class as return type, but
                ;; <init> methods have void return type
                (let* ((desc (cond
                               ((null method-type) "()V")
                               ;; Extract params portion and append V
                               ((position #\) method-type)
                                (format nil "~AV" (subseq method-type 0 (1+ (position #\) method-type)))))
                               (t (format nil "~AV" method-type))))
                       ;; Instance methods like constructors are defined as generic functions
                       ;; with just the method name, not the fully qualified class.method name
                       (init-method-name (format nil "<init>~A" (subseq desc 0 (1+ (position #\) desc)))))
                       (lisp-init-name (intern init-method-name :openldk)))
                  (apply lisp-init-name instance args))
                ;; Return the constructed instance
                instance))
            ;; Normal method invocation
            ;; REF_invokeVirtual = 5, REF_invokeStatic = 6, REF_invokeSpecial = 7
            (let ((is-static (= ref-kind 6))
                  (is-virtual (= ref-kind 5))
                  (is-special (= ref-kind 7)))
              (if is-static
                  ;; Static methods use fully qualified names like class.method(desc)
                  ;; They live in the loader's package - get loader from the Class object
                  (let* ((java-loader (slot-value clazz '|classLoader|))
                         (ldk-loader (get-ldk-loader-for-java-loader java-loader))
                         (pkg (class-package class-name ldk-loader))
                         (full-method-sig (format nil "~A.~A~A" class-name method-name method-type))
                         (lisp-method-name (intern (lispize-method-name full-method-sig) pkg)))
                    (when (search "shiftLeft" method-name)
                      (format t "~&; DEBUG invoke-from-member-name: class=~A method=~A loader=~A pkg=~A lisp-method=~A~%"
                              class-name method-name ldk-loader pkg lisp-method-name)
                      (force-output))
                    (apply lisp-method-name args))
                  ;; Virtual and special methods are generic functions with just the method name
                  ;; The first argument is the receiver (this)
                  (let* ((simple-method-name (format nil "~A~A" method-name method-type))
                         (lisp-method-name (intern (lispize-method-name simple-method-name) :openldk)))
                    (apply lisp-method-name args))))))))

(defun |java/lang/invoke/MethodHandle.linkToStatic(Ljava/lang/invoke/MemberName;)| (&rest args)
  "MethodHandle intrinsic: invoke a static method via MemberName (no-arg variant).
   The last argument is the MemberName, the rest are method arguments."
  (let* ((member-name (car (last args)))
         (method-args (butlast args)))
    (apply #'%invoke-from-member-name member-name method-args)))

(defun |java/lang/invoke/MethodHandle.linkToStatic(Ljava/lang/Object;Ljava/lang/invoke/MemberName;)| (arg member-name)
  "MethodHandle intrinsic: invoke a static method via MemberName (1-arg variant)."
  (%invoke-from-member-name member-name arg))

(defun |java/lang/invoke/MethodHandle.linkToStatic(Ljava/lang/Object;Ljava/lang/Object;Ljava/lang/invoke/MemberName;)| (arg1 arg2 member-name)
  "MethodHandle intrinsic: invoke a static method via MemberName (2-arg variant)."
  (%invoke-from-member-name member-name arg1 arg2))

(defun |java/lang/invoke/MethodHandle.linkToStatic(Ljava/lang/Object;Ljava/lang/Object;Ljava/lang/Object;Ljava/lang/invoke/MemberName;)| (arg1 arg2 arg3 member-name)
  "MethodHandle intrinsic: invoke a static method via MemberName (3-arg variant)."
  (%invoke-from-member-name member-name arg1 arg2 arg3))

(defun |java/lang/invoke/MethodHandle.linkToVirtual(Ljava/lang/invoke/MemberName;)| (&rest args)
  "MethodHandle intrinsic: invoke a virtual method via MemberName (varargs variant).
   The last argument is the MemberName, the rest are method arguments (including receiver)."
  (let* ((member-name (car (last args)))
         (method-args (butlast args)))
    (apply #'%invoke-from-member-name member-name method-args)))

(defun |java/lang/invoke/MethodHandle.linkToVirtual(Ljava/lang/Object;Ljava/lang/invoke/MemberName;)| (receiver member-name)
  "MethodHandle intrinsic: invoke a virtual method via MemberName (receiver-only variant)."
  (%invoke-from-member-name member-name receiver))

(defun |java/lang/invoke/MethodHandle.linkToVirtual(Ljava/lang/Object;Ljava/lang/Object;Ljava/lang/invoke/MemberName;)| (receiver arg1 member-name)
  "MethodHandle intrinsic: invoke a virtual method via MemberName (1-arg variant)."
  (%invoke-from-member-name member-name receiver arg1))

(defun |java/lang/invoke/MethodHandle.linkToVirtual(Ljava/lang/Object;Ljava/lang/Object;Ljava/lang/Object;Ljava/lang/invoke/MemberName;)| (receiver arg1 arg2 member-name)
  "MethodHandle intrinsic: invoke a virtual method via MemberName (2-arg variant)."
  (%invoke-from-member-name member-name receiver arg1 arg2))

(defun |java/lang/invoke/MethodHandle.linkToSpecial(Ljava/lang/invoke/MemberName;)| (&rest args)
  "MethodHandle intrinsic: invoke a special (non-virtual) method via MemberName (varargs variant).
   The last argument is the MemberName, the rest are method arguments (including receiver)."
  (let* ((member-name (car (last args)))
         (method-args (butlast args)))
    (apply #'%invoke-from-member-name member-name method-args)))

(defun |java/lang/invoke/MethodHandle.linkToSpecial(Ljava/lang/Object;Ljava/lang/invoke/MemberName;)| (receiver member-name)
  "MethodHandle intrinsic: invoke a special method via MemberName (receiver-only variant)."
  (%invoke-from-member-name member-name receiver))

(defun |java/lang/invoke/MethodHandle.linkToSpecial(Ljava/lang/Object;Ljava/lang/Object;Ljava/lang/invoke/MemberName;)| (receiver arg1 member-name)
  "MethodHandle intrinsic: invoke a special method via MemberName (1-arg variant)."
  (%invoke-from-member-name member-name receiver arg1))

(defun |java/lang/invoke/MethodHandle.linkToSpecial(Ljava/lang/Object;Ljava/lang/Object;Ljava/lang/Object;Ljava/lang/invoke/MemberName;)| (receiver arg1 arg2 member-name)
  "MethodHandle intrinsic: invoke a special method via MemberName (2-arg variant)."
  (%invoke-from-member-name member-name receiver arg1 arg2))

(defun |java/lang/invoke/MethodHandle.linkToInterface(Ljava/lang/invoke/MemberName;)| (&rest args)
  "MethodHandle intrinsic: invoke an interface method via MemberName (varargs variant).
   The last argument is the MemberName, the rest are method arguments (including receiver)."
  (let* ((member-name (car (last args)))
         (method-args (butlast args)))
    (apply #'%invoke-from-member-name member-name method-args)))

(defun |java/lang/invoke/MethodHandle.linkToInterface(Ljava/lang/Object;Ljava/lang/invoke/MemberName;)| (receiver member-name)
  "MethodHandle intrinsic: invoke an interface method via MemberName (receiver-only variant)."
  (%invoke-from-member-name member-name receiver))

(defun |java/lang/invoke/MethodHandle.linkToInterface(Ljava/lang/Object;Ljava/lang/Object;Ljava/lang/invoke/MemberName;)| (receiver arg1 member-name)
  "MethodHandle intrinsic: invoke an interface method via MemberName (1-arg variant)."
  (%invoke-from-member-name member-name receiver arg1))

(defun |java/lang/invoke/MethodHandle.linkToInterface(Ljava/lang/Object;Ljava/lang/Object;Ljava/lang/Object;Ljava/lang/invoke/MemberName;)| (receiver arg1 arg2 member-name)
  "MethodHandle intrinsic: invoke an interface method via MemberName (2-arg variant)."
  (%invoke-from-member-name member-name receiver arg1 arg2))

;; Native stub for MethodHandleImpl.makeArrays() to avoid MAX_JVM_ARITY issue
;; The original tries to create collectors for all arities up to 255, but
;; arity 255 fails because adding MemberName parameter makes it 256 (over limit).
;; We return a small array that's sufficient for typical use cases.
(defun |java/lang/invoke/MethodHandleImpl.makeArrays()| ()
  "Native stub: return small array of MethodHandle collectors to avoid arity 256 issue.
   Returns array of size 11 (arities 0-10) which is enough for most lambda expressions."
  (let* ((mh-class (%get-java-class-by-bin-name "java/lang/invoke/MethodHandle"))
         ;; Create small array - size 11 for arities 0-10
         (array (make-java-array :component-class mh-class :size 11)))
    ;; Leave all entries as NULL - they will be lazily initialized if needed
    array))

;; Accessor method for makeArrays() called by MethodHandleImpl$Lazy
(defun |java/lang/invoke/MethodHandleImpl.access$000()| ()
  "Accessor for makeArrays() - delegates to native stub."
  (|java/lang/invoke/MethodHandleImpl.makeArrays()|))

;; Stub findCollector to prevent creating 255-arity collectors
(defun |java/lang/invoke/MethodHandleImpl.findCollector(Ljava/lang/String;ILjava/lang/Class;[Ljava/lang/Class;)| (name arity array-type param-types)
  "Native stub: prevent creating collectors with arity >= 254 to avoid parameter count overflow."
  (declare (ignore name array-type param-types))
  (when (>= arity 254)
    (error (%lisp-condition
            (%make-throwable '|java/lang/UnsupportedOperationException|
                             (ijstring (format nil "Collector arity ~D not supported (max 253)" arity))))))
  ;; For smaller arities, let Java code handle it by returning NIL (not implemented)
  nil)

;; Minimal lambda support -----------------------------------------------------
;; Lambda implementations that wrap a MethodHandle target and invoke it with
;; any captured arguments supplied via the metafactory fast-path.

(defun %lambda-invoke (mh captures args)
  "Common invoke logic for lambda implementations."
  (let ((member (when (and mh (slot-exists-p mh '|member|))
                  (slot-value mh '|member|))))
    (if member
        (apply #'%invoke-from-member-name member (append captures args))
        (let ((args-array (make-java-array :component-class (%get-java-class-by-bin-name "java/lang/Object")
                                           :initial-contents (coerce (append captures args) 'vector))))
          (|invokeWithArguments([Ljava/lang/Object;)| mh args-array)))))

;; Supplier implementation (for get() with no args)
(defclass/std |openldk/LambdaSupplier| (|java/lang/Object| |java/util/function/Supplier|)
  ((target)
   (captures)))

(defmethod |get()| ((this |openldk/LambdaSupplier|))
  (%lambda-invoke (slot-value this 'target) (slot-value this 'captures) nil))

;; Predicate implementation (for test(Object))
(defclass/std |openldk/LambdaPredicate| (|java/lang/Object| |java/util/function/Predicate|)
  ((target)
   (captures)))

(defmethod |test(Ljava/lang/Object;)| ((this |openldk/LambdaPredicate|) obj)
  (%lambda-invoke (slot-value this 'target) (slot-value this 'captures) (list obj)))

;; Function implementation (for apply(Object))
(defclass/std |openldk/LambdaFunction| (|java/lang/Object| |java/util/function/Function|)
  ((target)
   (captures)))

(defmethod |apply(Ljava/lang/Object;)| ((this |openldk/LambdaFunction|) obj)
  (%lambda-invoke (slot-value this 'target) (slot-value this 'captures) (list obj)))

;; Consumer implementation (for accept(Object))
(defclass/std |openldk/LambdaConsumer| (|java/lang/Object| |java/util/function/Consumer|)
  ((target)
   (captures)))

(defmethod |accept(Ljava/lang/Object;)| ((this |openldk/LambdaConsumer|) obj)
  (%lambda-invoke (slot-value this 'target) (slot-value this 'captures) (list obj))
  nil)

;; BiConsumer implementation (for accept(Object, Object))
(defclass/std |openldk/LambdaBiConsumer| (|java/lang/Object| |java/util/function/BiConsumer|)
  ((target)
   (captures)))

(defmethod |accept(Ljava/lang/Object;Ljava/lang/Object;)| ((this |openldk/LambdaBiConsumer|) a b)
  (%lambda-invoke (slot-value this 'target) (slot-value this 'captures) (list a b))
  nil)

;; BinaryOperator implementation (for apply(Object, Object))
(defclass/std |openldk/LambdaBinaryOperator| (|java/lang/Object| |java/util/function/BinaryOperator|)
  ((target)
   (captures)))

(defmethod |apply(Ljava/lang/Object;Ljava/lang/Object;)| ((this |openldk/LambdaBinaryOperator|) a b)
  (%lambda-invoke (slot-value this 'target) (slot-value this 'captures) (list a b)))

(defun %lambda-metafactory (impl-handle captures &optional (method-name "get"))
  "Construct a functional interface implementation for Java lambdas.
METHOD-NAME is the interface method name (get, test, apply, accept).
CAPTURES is a list of pre-bound values for captured variables."
  (let* ((method-str (if (stringp method-name) method-name (lstring method-name)))
         (lambda-class (cond
                         ((string= method-str "get") '|openldk/LambdaSupplier|)
                         ((string= method-str "test") '|openldk/LambdaPredicate|)
                         ((and (string= method-str "apply") (= 1 (length captures)))
                          '|openldk/LambdaFunction|)
                         ((and (string= method-str "apply") (>= (length captures) 0))
                          '|openldk/LambdaBinaryOperator|)
                         ((and (string= method-str "accept") (<= (length captures) 1))
                          '|openldk/LambdaConsumer|)
                         ((string= method-str "accept") '|openldk/LambdaBiConsumer|)
                         (t '|openldk/LambdaSupplier|)))
         (instance (make-instance lambda-class)))
    (setf (slot-value instance 'target) impl-handle)
    (setf (slot-value instance 'captures) captures)
    instance))

;; ---------------------------------------------------------------------------
;; Javac helper: allow setEnclosingType on ClassReader$2 / ClassType without
;; tripping the UnsupportedOperationException override in ClassReader$2.  We
;; keep it simple: if the expected slots exist, set them and clear cached
;; params.  Works for both Type$ClassType and the anonymous subclass.
(defmethod |setEnclosingType(Lcom/sun/tools/javac/code/Type;)| (this outer)
  (when (slot-exists-p this '|outer_field|)
    (setf (slot-value this '|outer_field|) outer))
  (when (slot-exists-p this '|allparams_field|)
    (setf (slot-value this '|allparams_field|) nil))
  nil)

(defun %ensure-methodtypeform-handle-cache (form index)
  "Ensure MethodTypeForm.methodHandles is a java array large enough for INDEX."
  (let* ((cache (when (slot-exists-p form '|methodHandles|)
                  (slot-value form '|methodHandles|)))
         (current-len (if cache (java-array-length cache) 0)))
    (when (or (null cache) (>= index current-len))
      (let* ((new-len (max (1+ index) (max 16 current-len)))
             (component (%get-java-class-by-bin-name "java/lang/invoke/MethodHandle"))
             (new-cache (make-java-array :component-class component :size new-len)))
        ;; copy existing entries
        (when cache
          (loop for i below current-len
                do (setf (jaref new-cache i) (jaref cache i))))
        (setf cache new-cache)
        (when (slot-exists-p form '|methodHandles|)
          (setf (slot-value form '|methodHandles|) cache))))
    cache))

(defun |java/lang/invoke/MethodTypeForm.setCachedMethodHandle(ILjava/lang/invoke/MethodHandle;)| (form index mh)
  "Native shim used by LambdaForm generation to cache handles per MethodTypeForm."
  (assert (and form (>= index 0)))
  (let ((cache (%ensure-methodtypeform-handle-cache form index)))
    (setf (jaref cache index) mh)
    mh))

(defun |java/lang/invoke/MethodHandleNatives.objectFieldOffset(Ljava/lang/invoke/MemberName;)| (member-name)
  (declare (ignore unsafe))
  (let ((offset (unsigned-to-signed-integer (cl-murmurhash:murmurhash (sxhash member-name)))))
    (setf (gethash offset *field-offset-table*) member-name)
    offset))

(defun |java/lang/invoke/MethodHandleNatives.getMembers(Ljava/lang/Class;Ljava/lang/String;Ljava/lang/String;ILjava/lang/Class;I[Ljava/lang/invoke/MemberName;)|
    (defc match-name match-sig match-flags caller skip results)
  ;; FIXME
  (assert (null match-name))
  (assert (null match-sig))
  (assert (eq match-flags 65536)) ;; methods only
  (assert (null caller))
  (let ((ldk-class (get-ldk-class-for-java-class defc))
        (class-loader (|getClassLoader()| defc)))
    ;; Caller may pass NIL to query count; only fill when RESULTS provided.
    (when results
      (loop for mn across (java-array-data results)
            for index from 0
            for method = (aref (methods ldk-class) index)
            do (if (eq skip 0)
                   (progn
                     (|init(Ljava/lang/Class;Ljava/lang/String;Ljava/lang/Object;I)|
                      mn defc (jstring (name method))
                      (|java/lang/invoke/MethodType.fromMethodDescriptorString(Ljava/lang/String;Ljava/lang/ClassLoader;)|
                       (jstring (descriptor method)) class-loader)
                      (+ (access-flags method)
                         (if (static-p method) (ash 6 24) (ash 5 24))
                         (if (string= "<init>" (name method)) 131072 65536))))
                   (incf skip -1))))
    (- (length (methods ldk-class)) (or skip 0))))

(defmethod |getProtectionDomain0()| ((clazz |java/lang/Class|))
  ;; FIXME
  nil)
