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
  (classload "java/lang/SecurityManager")
  (eval (list 'make-instance (list 'quote '|java/lang/SecurityManager|))))

#|
(defmethod |println(Ljava/lang/String;)| (stream string)
  (format t "~A~%" (slot-value string '|value|)))

(defmethod |println(I)| (stream number)
  (format t "~A~%" number))

(defmethod |println(Ljava/lang/Object;)| (stream object)
  (format t "~A~%" object))

(defmethod |println(Ljava/lang/Object;)| (stream (object (eql nil)))
  (format t "null~%"))
|#

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
  ;; FIXME: we don't need the whole backtrace
  (let ((klass
          (let* ((caller-list (fourth (%remove-adjacent-repeats (sb-debug:list-backtrace))))
                 (caller-string (format nil "~A" caller-list)))
            ;; (cstring (subseq caller-string 1 (position #\. caller-string))))
            (let ((dot-position (position #\. caller-string)))
              (cond
                ((str:starts-with? "(%clinit-" caller-string)
                 (java-class (gethash (subseq caller-string 9 (1- (length caller-string))) *classes*)))
                ((str:starts-with? "((METHOD " caller-string)
                 (|getClass()| (cadr caller-list)))
                (dot-position
                 (java-class (gethash (subseq caller-string 1 dot-position) *classes*)))
                (t (error (format nil "ERROR in sun/reflect/Reflection.getCallerClass(): don't recognize ~S" caller-string))))))))
    klass))

(defmethod |getClass()| (object)
  ;;; FIXME - throw nullpointerexception
	(when *debug-trace*
		(format t "; trace: java/lang/Object.getClass(~A)" object))
  (cond
    ((arrayp object) (java-class (gethash "java/util/Arrays" *classes*)))
    (t (java-class (gethash (format nil "~A" (type-of object)) *classes*)))))

(defmethod |java/lang/Class.forName0(Ljava/lang/String;ZLjava/lang/ClassLoader;Ljava/lang/Class;)| (name initialize loader caller)
  (unwind-protect
       (progn
         (when *debug-trace*
           (format t "; trace: entering java/lang/Class.forName0(Ljava/lang/String;ZLjava/lang/ClassLoader;Ljava/lang/Class;)~%"))
         (let ((lname (substitute #\/ #\. (coerce (slot-value name '|value|) 'string))))
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

(defmethod |isInterface()| ((this |java/lang/Class|))
  (let ((ldk-class (gethash (slot-value (slot-value this '|name|) '|value|) *classes*)))
    (if (interface-p ldk-class)
        1
        0)))

(defmethod |getDeclaredConstructors0(Z)| ((this |java/lang/Class|) arg)
  ;; FIXME
  (unwind-protect
       (progn
         (when *debug-trace*
           (format t "~&; trace: entering java/lang/Class.getDeclaredConstructors0(Z)~%"))
         (unless (gethash "java/lang/reflect/Constructor" *classes*)
           (|java/lang/Class.forName0(Ljava/lang/String;ZLjava/lang/ClassLoader;Ljava/lang/Class;)| (jstring "java/lang/reflect/Constructor") nil nil nil))

         ;; Get the ldk-class for THIS
         (let ((ldk-class (gethash (slot-value (slot-value this '|name|) '|value|) *classes*)))
           (coerce (append (loop for method across (methods ldk-class)
                                 when (str:starts-with? "<init>" (name method))
                                   collect (let ((c (make-instance '|java/lang/reflect/Constructor|)))
                                             (|<init>(Ljava/lang/Class;[Ljava/lang/Class;[Ljava/lang/Class;IILjava/lang/String;[B[B)| c this (%get-parameter-types (descriptor method)) (make-array 0) (access-flags method) 0 (ijstring (descriptor method)) (make-array 0) (make-array 0))
                                             c)))
                   'vector)))
    (when *debug-trace*
      (format t "~&; trace: leaving  java/lang/Class.getDeclaredConstructors0(Z)~%"))))


(defmethod |getDeclaredFields0(Z)| ((this |java/lang/Class|) arg)
  (unwind-protect
       (progn
         (when *debug-trace*
           (format t "~&; trace: entering java/lang/Class.getDeclaredFields0(Z)~%"))
         (unless (gethash "java/lang/reflect/Field" *classes*)
           (|java/lang/Class.forName0(Ljava/lang/String;ZLjava/lang/ClassLoader;Ljava/lang/Class;)| (jstring "java/lang/reflect/Field") nil nil nil))

         ;; Get the ldk-class for THIS
         (let ((ldk-class (gethash (slot-value (slot-value this '|name|) '|value|) *classes*)))
           (labels ((get-fields (ldk-class)
                      (when ldk-class
                        (append (loop for field across (fields ldk-class)
                                      collect (let ((f (make-instance '|java/lang/reflect/Field|)))
                                                (|<init>(Ljava/lang/Class;Ljava/lang/String;Ljava/lang/Class;IILjava/lang/String;[B)| f this (ijstring (name field)) nil (access-flags field) nil nil nil)
                                                f))
                                (get-fields (gethash (super ldk-class) *classes*))))))
             (coerce (get-fields ldk-class) 'vector))))
    (when *debug-trace*
      (format t "~&; trace: leaving  java/lang/Class.getDeclaredFields0(Z)~%"))))

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
     (aref obj l))
    (t (error "Unrecognized object type in getObjectVolatile: " obj))))

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

(defun |java/security/AccessController.doPrivileged(Ljava/security/PrivilegedExceptionAction;)| (action)
  (let ((result (|run()| action)))
    result))

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
  (let ((ldk-class (gethash (slot-value (slot-value class '|name|) '|value|) *classes*)))
    (access-flags ldk-class)))

(defmethod |getModifiers()| ((class |java/lang/Class|))
  (let ((ldk-class (gethash (slot-value (slot-value class '|name|) '|value|) *classes*)))
    (access-flags ldk-class)))

(defmethod |getSuperclass()| ((class |java/lang/Class|))
  (let ((ldk-class (gethash (slot-value (slot-value class '|name|) '|value|) *classes*)))
    (gethash (super ldk-class) *java-classes*)))

(defun |sun/reflect/NativeConstructorAccessorImpl.newInstance0(Ljava/lang/reflect/Constructor;[Ljava/lang/Object;)|
    (constructor params)
  (let ((class-name (slot-value (slot-value constructor '|clazz|) '|name|)))
    (|java/lang/Class.forName0(Ljava/lang/String;ZLjava/lang/ClassLoader;Ljava/lang/Class;)| class-name nil nil nil)
    (let ((obj (make-instance (intern (slot-value class-name '|value|) :openldk))))
      (if (string= "()V" (slot-value (slot-value constructor '|signature|) '|value|))
          (|<init>()| obj)
          (error "unimplemented"))
      obj)))

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
  ;; FIXME
  nil)

(defmethod |open0(Ljava/lang/String;)| ((fis |java/io/FileInputStream|) filename)
  (handler-case
      (setf (slot-value fis '|fd|) (open (coerce (slot-value filename '|value|) 'string)
                                         :element-type '(unsigned-byte 8)
                                         :direction :input))
    (error (e)
      (print e)
      (error e))))

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
  (error "not implemented"))
