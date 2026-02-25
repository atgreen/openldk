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

;; Methods registered here will not have their native Lisp implementations
;; overwritten by bytecode compilation.  Key is the method key string
;; (e.g. "java/lang/System.console()Ljava/io/Console;").
(defvar *native-overrides* (make-hash-table :test #'equal))

;;; Post-<clinit> hooks: functions called after a class's static initializer runs.
;;; Used for classes whose fields are pre-populated by the JVM before <clinit>.
(defvar *post-clinit-hooks* (make-hash-table :test #'equal))

;; jdk/internal/misc/UnsafeConstants: the JVM pre-populates ADDRESS_SIZE0, PAGE_SIZE, etc.
;; before <clinit> runs (which just sets them all to 0). We set the real values after.
(setf (gethash "jdk/internal/misc/UnsafeConstants" *post-clinit-hooks*)
      (lambda (class pkg)
        (let* ((static-name (format nil "+static-~A+" (slot-value class 'name)))
               (static-sym (find-symbol static-name pkg)))
          (when (and static-sym (boundp static-sym))
            (let ((s (symbol-value static-sym)))
              (setf (slot-value s '|ADDRESS_SIZE0|) 8)
              (setf (slot-value s '|PAGE_SIZE|) (sb-posix:getpagesize))
              (setf (slot-value s '|BIG_ENDIAN|) 0)
              (setf (slot-value s '|UNALIGNED_ACCESS|) 1)
              (setf (slot-value s '|DATA_CACHE_LINE_FLUSH_SIZE|) 0))))))

;; In Java, a method call on a NULL object results in a
;; NullPointerException.  CLOS makes it easy to implement this
;; behaviour by providing our own CLOS no-applicable-method method.

(defmethod no-applicable-method ((gf generic-function) &rest args)
  (if (null (car args))
      (error (%lisp-condition (%make-throwable '|java/lang/NullPointerException|)))
      (error "internal error: no applicable method for invocation of ~A with arguments ~S" gf args)))

(defun |java/lang/Object.registerNatives()| ()
  ())

(defmethod |getExtendedNPEMessage()| (this)
  ;; JDK 14+ enhanced NPE messages.  Return nil for now.
  (declare (ignore this))
  nil)

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

(defmethod |getClassLoader0()| ((this |java/lang/Class|))
  "Return the ClassLoader that loaded this class, or NIL for bootstrap classes."
  (slot-value this '|classLoader|))

(defun |java/lang/Class.desiredAssertionStatus0(Ljava/lang/Class;)| (class)
  (declare (ignore class))
  ;; Return 0 (false) to disable assertions
  0)

(defun |java/lang/Class.getSecurityManager()| ()
  (classload "java/lang/SecurityManager")
  (eval (list 'make-instance (list 'quote '|java/lang/SecurityManager|))))

(defmethod |getNestHost()| ((this |java/lang/Class|))
  "Return the nest host of this class.  Returns self if no NestHost attribute."
  (let* ((name (slot-value this '|name|))
         (bin-name (if (stringp name) name (coerce (java-array-data name) 'string)))
         (bin-name (substitute #\/ #\. bin-name))
         (ldk-class (%get-ldk-class-by-bin-name bin-name t)))
    (if (and ldk-class (nest-host ldk-class))
        (or (%get-java-class-by-bin-name (nest-host ldk-class) t)
            this)
        this)))

(defmethod |getNestMembers()| ((this |java/lang/Class|))
  "Return an array of nest member classes.  Returns singleton array of self if no NestMembers."
  (let* ((name (slot-value this '|name|))
         (bin-name (if (stringp name) name (coerce (java-array-data name) 'string)))
         (bin-name (substitute #\/ #\. bin-name))
         (ldk-class (%get-ldk-class-by-bin-name bin-name t)))
    (if (and ldk-class (nest-members ldk-class))
        (make-java-array
         :component-class (%get-java-class-by-bin-name "java/lang/Class")
         :initial-contents (mapcar (lambda (member-name)
                                     (or (%get-java-class-by-bin-name member-name t)
                                         this))
                                   (nest-members ldk-class)))
        (make-java-array
         :component-class (%get-java-class-by-bin-name "java/lang/Class")
         :initial-contents (list this)))))

(defmethod |isNestmateOf(Ljava/lang/Class;)| ((this |java/lang/Class|) other)
  "Return true (1) if this class and OTHER share the same nest host."
  (if (eq (|getNestHost()| this) (|getNestHost()| other)) 1 0))

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

;; JDK 17: static native initStackTraceElements — fill array from Throwable's backtrace
(defun |java/lang/StackTraceElement.initStackTraceElements([Ljava/lang/StackTraceElement;Ljava/lang/Throwable;)|
    (ste-array throwable)
  (let* ((bt (when (slot-boundp throwable '|backtrace|)
               (slot-value throwable '|backtrace|)))
         (data (java-array-data ste-array))
         (n (min (length data) (length bt))))
    (dotimes (i n)
      (setf (aref data i) (|getStackTraceElement(I)| throwable i)))))

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
     "jdk/internal/reflect/DirectMethodHandleAccessor$NativeAccessor.invoke0"
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

(defun |jdk/internal/reflect/Reflection.getCallerClass()| ()
  (|sun/reflect/Reflection.getCallerClass()|))

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
          ;; JDK 17: getComponentType() reads the componentType field directly
          ;; (no longer native). Set it so array types resolve correctly.
          (when (and (> (length cname) 1)
                     (slot-exists-p java-class '|componentType|))
            (let ((ct (%bin-type-name-to-class (subseq cname 1))))
              (when ct
                (setf (slot-value java-class '|componentType|) ct))))
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
  (when (null object)
    (error (%lisp-condition (%make-throwable '|java/lang/NullPointerException|))))
  (unwind-protect
       (progn
         (when *debug-trace*
           (format t "~&~V@A trace: java/lang/Object.getClass(~A)"
                   (incf *call-nesting-level* 1) "*" object))
         (let ((c (cond
                    ((integerp object)
                     (%get-java-class-by-bin-name "java/lang/Long"))
                    ((typep object 'single-float)
                     (%get-java-class-by-bin-name "java/lang/Float"))
                    ((typep object 'double-float)
                     (%get-java-class-by-bin-name "java/lang/Double"))
                    ((characterp object)
                     (%get-java-class-by-bin-name "java/lang/Character"))
                    ((typep object 'java-array)
                     (let* ((comp-class (java-array-component-class object))
                            (comp-name (lstring (slot-value comp-class '|name|)))
                            (array-name
                              (cond
                                ;; Primitive component types → compact array descriptor
                                ((string= comp-name "byte")    "[B")
                                ((string= comp-name "short")   "[S")
                                ((string= comp-name "int")     "[I")
                                ((string= comp-name "long")    "[J")
                                ((string= comp-name "float")   "[F")
                                ((string= comp-name "double")  "[D")
                                ((string= comp-name "char")    "[C")
                                ((string= comp-name "boolean") "[Z")
                                ;; Array component (name already starts with [)
                                ((char= (char comp-name 0) #\[)
                                 (format nil "[~A" comp-name))
                                ;; Reference type → [Lname;
                                (t (format nil "[L~A;" comp-name)))))
                       (|java/lang/Class.forName0(Ljava/lang/String;ZLjava/lang/ClassLoader;Ljava/lang/Class;)|
                        (jstring array-name) nil nil nil)))
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
         (let* ((lname (substitute #\/ #\. (lstring name)))
                (result
                  ;; Try user class loader first (if provided and not boot loader),
                  ;; then fall back to our classpath search.
                  ;; This mirrors JVM's loadClass delegation: parent first, then findClass.
                  (or (and (and loader (not (equal loader *boot-class-loader*)))
                           (handler-case
                               (|findClass(Ljava/lang/String;)| loader name)
                             (|condition-java/lang/ClassNotFoundException| () nil)
                             (error () nil)))
                      (and (eq (char lname 0) #\[)
                           (or (%get-java-class-by-bin-name lname t)
                               (java-class (%get-array-ldk-class-from-name lname))))
                      (and (%get-ldk-class-by-bin-name lname t)
                           (java-class (%get-ldk-class-by-bin-name lname)))
                      (let ((klass (classload lname)))
                        (when klass
                          (java-class klass))))))
           ;; Per JVM spec: when initialize=true, ensure <clinit> has run.
           ;; Previously only the classload branch called %clinit; the findClass
           ;; and already-loaded branches skipped it, breaking autoloads that
           ;; rely on Class.forName triggering static initialization.
           (when (and result initialize (not (eql initialize 0)))
             (let ((ldk-class (%get-ldk-class-by-bin-name lname t)))
               (when ldk-class
                 (%clinit ldk-class))))
           ;; Only throw ClassNotFoundException at runtime (after app loader is initialized).
           ;; During image build, return nil for missing classes.
           (when (and (not result) *app-ldk-class-loader*)
             (let ((cnfe (%make-java-instance "java/lang/ClassNotFoundException")))
               (setf (slot-value cnfe '|detailMessage|) name)
               (error (make-condition '|condition-java/lang/ClassNotFoundException| :|objref| cnfe))))
           result))
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
  (error (%lisp-condition (%make-throwable '|java/lang/UnsupportedOperationException|))))

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

(defmethod |staticFieldOffset0(Ljava/lang/reflect/Field;)| ((unsafe |jdk/internal/misc/Unsafe|) field)
  (declare (ignore unsafe))
  (let ((offset (sxhash field)))
    (setf (gethash offset *field-offset-table*) field)
    offset))

(defmethod |staticFieldBase0(Ljava/lang/reflect/Field;)| ((unsafe |jdk/internal/misc/Unsafe|) field)
  (declare (ignore unsafe field))
  nil)

(defmethod |objectFieldOffset0(Ljava/lang/reflect/Field;)| ((unsafe |jdk/internal/misc/Unsafe|) field)
  (declare (ignore unsafe))
  (let ((offset (unsigned-to-signed-integer (cl-murmurhash:murmurhash (sxhash field)))))
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
        ;; Fallback to main thread (for compatibility).
        ;; Re-register the mapping so the current lisp thread is properly tracked.
        (when *current-thread*
          (setf (gethash current-lisp-thread *lisp-to-java-threads*) *current-thread*)
          *current-thread*)
        ;; Create main thread if it doesn't exist
        (let ((thread (%make-java-instance "java/lang/Thread"))
              (thread-group (%make-java-instance "java/lang/ThreadGroup")))
          (|<init>()| thread-group)
          (setf *current-thread* thread)
          ;; Register main thread in our mappings
          (setf (gethash current-lisp-thread *lisp-to-java-threads*) thread)
          ;; Set priority — JDK 21 moved this to Thread$FieldHolder, but it may
          ;; still be a direct slot on Thread depending on class version.
          (when (slot-exists-p thread '|priority|)
            (setf (slot-value thread '|priority|) 1))
          ;; Initialize the FieldHolder for JDK 21+ Thread structure
          (when (slot-exists-p thread '|holder|)
            (when (classload "java/lang/Thread$FieldHolder")
              (let ((holder (%make-java-instance "java/lang/Thread$FieldHolder")))
                (when (slot-exists-p holder '|group|)
                  (setf (slot-value holder '|group|) thread-group))
                (when (slot-exists-p holder '|priority|)
                  (setf (slot-value holder '|priority|) 5)) ;; NORM_PRIORITY
                (when (slot-exists-p holder '|daemon|)
                  (setf (slot-value holder '|daemon|) 0))
                (setf (slot-value thread '|holder|) holder))))
          (handler-case
              (|<init>(Ljava/lang/ThreadGroup;Ljava/lang/Runnable;Ljava/lang/String;J)|
               thread thread-group nil (jstring "main") 0)
            (condition ()
              ;; If the old constructor doesn't work, set name directly
              (setf (slot-value thread '|name|) (jstring "main"))))
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

;;; JDK 21 Thread native methods for virtual thread support.
;;; OpenLDK treats all threads as platform (carrier) threads.

(defun |java/lang/Thread.currentCarrierThread()| ()
  "Return the current carrier thread. Same as currentThread for OpenLDK."
  (|java/lang/Thread.currentThread()|))

(defun |java/lang/Thread.sleep0(J)| (milliseconds)
  "JDK 21 private native sleep0 — replaces public native sleep(J)."
  (|java/lang/Thread.sleep(J)| milliseconds))

(defun |java/lang/Thread.yield0()| ()
  "JDK 21 private native yield0 — replaces public native yield()."
  nil)

(defvar *scoped-value-cache* nil
  "Thread-local scoped value cache for JDK 21 ScopedValue support.")

(defun |java/lang/Thread.scopedValueCache()| ()
  "Return the current thread's scoped value cache."
  *scoped-value-cache*)

(defun |java/lang/Thread.setScopedValueCache([Ljava/lang/Object;)| (cache)
  "Set the current thread's scoped value cache."
  (setf *scoped-value-cache* cache))

(defun |java/lang/Thread.findScopedValueBindings()| ()
  "Return scoped value bindings for the current thread."
  nil)

(defun |java/lang/Thread.ensureMaterializedForStackWalk(Ljava/lang/Object;)| (obj)
  "No-op for platform threads."
  (declare (ignore obj))
  nil)

(defvar *next-thread-id* 0
  "Atomic thread ID counter for JDK 21 Thread$ThreadIdentifiers.")

(defun |java/lang/Thread.getNextThreadIdOffset()| ()
  "Return the field offset for the thread ID counter.
   Returns a dummy value — OpenLDK doesn't use Unsafe for thread IDs."
  0)

;; Override Thread$ThreadIdentifiers.next() to use a simple Lisp counter
;; instead of Unsafe atomic operations on a JVM-internal address.
(setf (gethash "java/lang/Thread$ThreadIdentifiers.next()J" *native-overrides*)
      (lambda ()
        (bordeaux-threads:with-lock-held (*identity-hash-counter-lock*)
          (incf *next-thread-id*))))

(defmethod |setCurrentThread(Ljava/lang/Thread;)| ((thread |java/lang/Thread|) new-thread)
  "Set the current thread reference. Used by virtual thread machinery."
  (declare (ignore thread))
  (let ((lisp-thread (bordeaux-threads:current-thread)))
    (setf (gethash lisp-thread *lisp-to-java-threads*) new-thread)))

(defmethod |getStackTrace0()| ((thread |java/lang/Thread|))
  "Return stack trace elements for the thread."
  (declare (ignore thread))
  nil)

;;; JDK 21: Object.wait0(J) — private native wait.
;;; Must contain the actual wait logic (not delegate to wait(J) which calls wait0).
(defmethod |wait0(J)| ((this |java/lang/Object|) timeout)
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

;;; JDK 21 VirtualThread — JVMTI notification stubs.
;;; OpenLDK doesn't support JVMTI, so all are no-ops.

(defun |java/lang/VirtualThread.registerNatives()| ()
  nil)

(setf (gethash "java/lang/VirtualThread.notifyJvmtiStart()V" *native-overrides*)
      (lambda (this) (declare (ignore this)) nil))

(setf (gethash "java/lang/VirtualThread.notifyJvmtiEnd()V" *native-overrides*)
      (lambda (this) (declare (ignore this)) nil))

(setf (gethash "java/lang/VirtualThread.notifyJvmtiMount(Z)V" *native-overrides*)
      (lambda (this first-mount) (declare (ignore this first-mount)) nil))

(setf (gethash "java/lang/VirtualThread.notifyJvmtiUnmount(Z)V" *native-overrides*)
      (lambda (this first-unmount) (declare (ignore this first-unmount)) nil))

(setf (gethash "java/lang/VirtualThread.notifyJvmtiHideFrames(Z)V" *native-overrides*)
      (lambda (this hide) (declare (ignore this hide)) nil))

;;; JDK 21 Continuation — virtual thread continuation support stubs.
;;; OpenLDK doesn't implement continuations; virtual threads run as platform threads.

(defun |jdk/internal/vm/Continuation.registerNatives()| ()
  nil)

(defun |jdk/internal/vm/Continuation.doYield()| ()
  "Stub: yield from continuation. Returns 0 (success)."
  0)

(setf (gethash "jdk/internal/vm/Continuation.enterSpecial(Ljdk/internal/vm/Continuation;ZZ)V" *native-overrides*)
      (lambda (cont is-virtual-thread force-yield)
        (declare (ignore cont is-virtual-thread force-yield))
        nil))

(defun |jdk/internal/vm/Continuation.pin()| ()
  nil)

(defun |jdk/internal/vm/Continuation.unpin()| ()
  nil)

(setf (gethash "jdk/internal/vm/Continuation.isPinned0(Ljdk/internal/vm/ContinuationScope;)I" *native-overrides*)
      (lambda (scope)
        (declare (ignore scope))
        0))

(defun |jdk/internal/vm/ContinuationSupport.isSupported0()| ()
  "Continuations are not supported in OpenLDK. This causes the JDK to create
   BoundVirtualThread (extends BaseVirtualThread) instead of VirtualThread.
   BoundVirtualThread uses start0()/run() like platform threads."
  0)

;;; Virtual thread support: BoundVirtualThread (extends BaseVirtualThread).
;;; When ContinuationSupport.isSupported0() returns 0, the JDK creates
;;; BoundVirtualThread instead of VirtualThread. BoundVirtualThread uses
;;; Thread.start() → start0() like platform threads. The task runs via
;;; BoundVirtualThread.run() → runWith(bindings, task).
;;;
;;; These :around methods on BaseVirtualThread ensure isAlive() and join()
;;; work correctly by checking the actual Lisp thread status. Thread.join()
;;; in bytecode uses `while(isAlive()) { wait(0); }` for non-VirtualThread
;;; instances, so correct isAlive() is critical. We also override join(J)
;;; directly to use bordeaux-threads:join-thread for reliable waiting.

(defmethod |isAlive()| :around ((thread |java/lang/BaseVirtualThread|))
  "Check if the virtual thread's underlying platform thread is still running."
  (let ((lisp-thread (gethash thread *java-threads*)))
    (if (and lisp-thread (bordeaux-threads:thread-alive-p lisp-thread))
        1
        0)))

(defmethod |join(J)| :around ((thread |java/lang/BaseVirtualThread|) millis)
  "Wait for the virtual thread's platform thread to finish.
   Overrides Thread.join(long) to use bordeaux-threads:join-thread directly."
  (let ((lisp-thread (gethash thread *java-threads*)))
    (cond
      ((null lisp-thread) nil)
      ((not (bordeaux-threads:thread-alive-p lisp-thread)) nil)
      ((zerop millis)
       (bordeaux-threads:join-thread lisp-thread)
       nil)
      (t
       (let ((timeout-sec (/ millis 1000.0d0)))
         (handler-case
             (bordeaux-threads:join-thread lisp-thread :timeout timeout-sec)
           (error () nil)))))))

(defun |sun/misc/Unsafe.registerNatives()| ()
  ;; FIXME
  nil)

(defun |jdk/internal/misc/Unsafe.registerNatives()| ()
  nil)

(defun |jdk/internal/misc/ScopedMemoryAccess.registerNatives()| ()
  nil)

;; Runtime native methods
(defmethod |availableProcessors()| ((rt |java/lang/Runtime|))
  (max 1 (sb-alien:alien-funcall
          (sb-alien:extern-alien "sysconf" (function sb-alien:long sb-alien:int))
          sb-posix::_sc-nprocessors-onln)))

(defmethod |freeMemory()| ((rt |java/lang/Runtime|))
  ;; Return SBCL's available dynamic space
  (- (sb-ext:dynamic-space-size) (sb-kernel:dynamic-usage)))

(defmethod |totalMemory()| ((rt |java/lang/Runtime|))
  (sb-kernel:dynamic-usage))

(defmethod |maxMemory()| ((rt |java/lang/Runtime|))
  (sb-ext:dynamic-space-size))

(defmethod |gc()| ((rt |java/lang/Runtime|))
  (sb-ext:gc :full t)
  nil)

;; JDK 9+: Signal native methods — no-op stubs for Terminator.setup()
(defun |jdk/internal/misc/Signal.findSignal0(Ljava/lang/String;)| (name)
  (declare (ignore name))
  ;; Return a dummy signal number
  0)

(defun |jdk/internal/misc/Signal.handle0(IJ)| (sig handler)
  (declare (ignore sig handler))
  ;; Return 0 (success / previous handler was default)
  0)

;; JDK 9+: StringUTF16 native — x86_64 is little-endian
(defun |java/lang/StringUTF16.isBigEndian()| ()
  0)

;; JDK 9+: BootLoader resource loading — delegates to classpath infrastructure.
;; These are bytecoded in JDK but depend on BuiltinClassLoader internals we
;; don't have, so we provide direct implementations.
(setf (gethash "jdk/internal/loader/BootLoader.findResource(Ljava/lang/String;)Ljava/net/URL;" *native-overrides*)
      (lambda (name)
        (let* ((resource-name (lstring name))
               (url-string (get-resource-url-on-classpath resource-name)))
          (when url-string
            (%make-url-from-string url-string)))))

(setf (gethash "jdk/internal/loader/BootLoader.findResourceAsStream(Ljava/lang/String;Ljava/lang/String;)Ljava/io/InputStream;" *native-overrides*)
      (lambda (module-name name)
        (declare (ignore module-name))
        (let* ((resource-name (lstring name))
               (stream (open-resource-on-classpath resource-name)))
          (when stream
            (let ((bytes (flexi-streams:with-output-to-sequence (out)
                           (loop for byte = (read-byte stream nil nil)
                                 while byte do (write-byte byte out)))))
              (close stream)
              (let ((bais (%make-java-instance "java/io/ByteArrayInputStream")))
                (|<init>([B)| bais
                 (make-java-array
                  :component-class (%get-ldk-class-by-fq-name "byte")
                  :initial-contents (coerce bytes 'vector)))
                bais))))))

;; JDK 9+: NativeLibraries — return nil (no built-in libraries)
(defun |jdk/internal/loader/NativeLibraries.findBuiltinLib(Ljava/lang/String;)| (name)
  (declare (ignore name))
  nil)

;;; JDK 9+ jdk/internal/misc/Unsafe native methods
;;; These mirror the sun/misc/Unsafe methods but are on the new class.

(defmethod |arrayBaseOffset0(Ljava/lang/Class;)| ((unsafe |jdk/internal/misc/Unsafe|) array)
  0)

(defmethod |arrayIndexScale0(Ljava/lang/Class;)| ((unsafe |jdk/internal/misc/Unsafe|) array)
  1)

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
                           :clazz clazz))
         (offset (unsigned-to-signed-integer (cl-murmurhash:murmurhash (sxhash f)))))
    (setf (gethash offset *field-offset-table*) f)
    offset))

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
  (cond
    ;; Native memory → Java array (common for ICU data loading)
    ((and (null source) dest)
     (let ((sap (sb-sys:int-sap source-offset)))
       (loop for i below length
             do (setf (jaref dest (+ dest-offset i))
                      (sb-sys:sap-ref-8 sap i)))))
    ;; Java array → Java array
    ((and source dest)
     (loop for i below length
           do (setf (jaref dest (+ dest-offset i))
                    (jaref source (+ source-offset i)))))
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
                      (jaref source (+ source-offset i))))))))

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
  (when (> time 0)
    (sleep (/ time 1000000000.0d0))))

(defmethod |unpark(Ljava/lang/Object;)| ((unsafe |jdk/internal/misc/Unsafe|) thread)
  "Unpark a thread. Stub — Lisp threads don't support park/unpark natively."
  (declare (ignore unsafe thread))
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
        (error "Reflective invoke0: method ~A not found (class=~A, static=~A)"
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
          (error "Reflective newInstance0: constructor ~A not found for class ~A"
                 lispized class-name))
      instance)))

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
         (unless (gethash "java/lang/reflect/Field" *ldk-classes-by-bin-name* t)
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
  ;; FIXME
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
(defun |jdk/internal/misc/CDS.initializeFromArchive(Ljava/lang/Class;)| (class)
  (declare (ignore class)) nil)
(defun |jdk/internal/misc/CDS.defineArchivedModules(Ljava/lang/ClassLoader;Ljava/lang/ClassLoader;)| (a b)
  (declare (ignore a b)) nil)
(defun |jdk/internal/misc/CDS.getRandomSeedForDumping()| () 0)

;;; JDK 9+ Reference handling native stubs
(defun |java/lang/ref/Reference.waitForReferencePendingList()| () nil)
(defun |java/lang/ref/Reference.getAndClearReferencePendingList()| () nil)
(defun |java/lang/ref/Reference.hasReferencePendingList()| () 0)

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
            (key (intern (mangle-field-name (lstring (slot-value field '|name|))) :openldk)))
       (if (equal (slot-value obj key) expected-value)
           (progn
             (setf (slot-value obj key) new-value)
             1)
           0)))))

(defmethod |compareAndSwapInt(Ljava/lang/Object;JII)| ((unsafe |sun/misc/Unsafe|) obj field-id expected-value new-value)
  ;; FIXME: use atomics package
  (let* ((field (gethash field-id *field-offset-table*))
         (key (intern (mangle-field-name (lstring (slot-value field '|name|))) :openldk)))
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
            (key (intern (mangle-field-name (lstring (slot-value field '|name|))) :openldk)))
       (slot-value obj key)))
    ((null obj)
     ;; FIXME: check that the field is STATIC
    (let* ((field (gethash l *field-offset-table*))
            (key (intern (mangle-field-name (lstring (slot-value field '|name|))) :openldk)))
       (let* ((clazz (slot-value field '|clazz|))
              (lname (lstring (slot-value clazz '|name|)))
              (bin-name (substitute #\/ #\. lname))
              (pkg (class-package bin-name)))
         (let ((v (slot-value (eval (intern (format nil "+static-~A+" bin-name) pkg)) key)))
           v))))
    (t (error "internal error: unrecognized object type in getObjectVolatile: ~A" obj))))

(defmethod |putObjectVolatile(Ljava/lang/Object;JLjava/lang/Object;)| ((unsafe |sun/misc/Unsafe|) obj l value)
  (cond
    ((typep obj 'java-array)
     (setf (jaref obj l) value))
    ((typep obj '|java/lang/Object|)
     (let* ((field (gethash l *field-offset-table*))
            (key (intern (mangle-field-name (lstring (slot-value field '|name|))) :openldk)))
       (setf (slot-value obj key) value)))
    ((null obj)
     ;; Static field access
     (let* ((field (gethash l *field-offset-table*))
            (key (intern (mangle-field-name (lstring (slot-value field '|name|))) :openldk))
            (clazz (slot-value field '|clazz|))
            (lname (lstring (slot-value clazz '|name|)))
            (bin-name (substitute #\/ #\. lname))
            (pkg (class-package bin-name)))
       (setf (slot-value (eval (intern (format nil "+static-~A+" bin-name) pkg)) key) value)))
    (t (error "internal error: unrecognized object type in putObjectVolatile: ~A" obj))))

;; getObject - same as getObjectVolatile for OpenLDK (no volatile semantics needed in Lisp)
(defmethod |getObject(Ljava/lang/Object;J)| ((unsafe |sun/misc/Unsafe|) obj l)
  (cond
    ((typep obj 'java-array)
     (jaref obj l))
    ((typep obj '|java/lang/Object|)
    (let* ((field (gethash l *field-offset-table*))
            (key (intern (mangle-field-name (lstring (slot-value field '|name|))) :openldk)))
       (slot-value obj key)))
    ((null obj)
     ;; FIXME: check that the field is STATIC
    (let* ((field (gethash l *field-offset-table*)))
       (if (null field)
           nil
           (let* ((key (intern (mangle-field-name (lstring (slot-value field '|name|))) :openldk))
                  (clazz (slot-value field '|clazz|))
                  (lname (lstring (slot-value clazz '|name|)))
                  (bin-name (substitute #\/ #\. lname))
                  (pkg (class-package bin-name))
                  (static-sym (find-symbol (format nil "+static-~A+" bin-name) pkg)))
             (if (and static-sym (boundp static-sym))
                 (let ((static-obj (symbol-value static-sym)))
                   (if (slot-boundp static-obj key)
                       (slot-value static-obj key)
                       nil))
                 nil)))))
    (t (error "internal error: unrecognized object type in getObject: ~A" obj))))

(defmethod |getLongVolatile(Ljava/lang/Object;J)| ((unsafe |sun/misc/Unsafe|) obj l)
  (cond
    ((typep obj 'java-array)
     (jaref obj l))
    ((typep obj '|java/lang/Object|)
    (let* ((field (gethash l *field-offset-table*))
            (key (intern (mangle-field-name (lstring (slot-value field '|name|))) :openldk)))
       (slot-value obj key)))
    (t (error "internal error: unrecognized object type in getLongVolatile: ~A" obj))))

(defmethod |putObject(Ljava/lang/Object;JLjava/lang/Object;)| ((unsafe |sun/misc/Unsafe|) obj l value)
  (cond
    ((typep obj 'java-array)
     (setf (jaref obj l) value))
    ((typep obj '|java/lang/Object|)
     (let* ((field (gethash l *field-offset-table*))
            (key (intern (mangle-field-name (lstring (slot-value field '|name|))) :openldk)))
       (setf (slot-value obj key) value)))
    ((null obj)
     ;; Static field access
     (let* ((field (gethash l *field-offset-table*))
            (key (intern (mangle-field-name (lstring (slot-value field '|name|))) :openldk))
            (clazz (slot-value field '|clazz|))
            (lname (lstring (slot-value clazz '|name|)))
            (bin-name (substitute #\/ #\. lname))
            (pkg (class-package bin-name)))
       (setf (slot-value (eval (intern (format nil "+static-~A+" bin-name) pkg)) key) value)))
    (t (error "internal error: unrecognized object type in putObject: ~A" obj))))

(defmethod |putOrderedObject(Ljava/lang/Object;JLjava/lang/Object;)| ((unsafe |sun/misc/Unsafe|) obj l value)
  ;; FIXME
  (|putObject(Ljava/lang/Object;JLjava/lang/Object;)| unsafe obj l value))

;;; JDK 9+ renamed native methods -- contain actual implementations to avoid
;;; infinite recursion with compiled bytecode that delegates old→new names.

(defmethod |getReference(Ljava/lang/Object;J)| ((unsafe |sun/misc/Unsafe|) obj l)
  (cond
    ((typep obj 'java-array) (jaref obj l))
    ((typep obj '|java/lang/Object|)
     (let* ((field (gethash l *field-offset-table*))
            (key (intern (mangle-field-name (lstring (slot-value field '|name|))) :openldk)))
       (slot-value obj key)))
    ((null obj)
     (let* ((field (gethash l *field-offset-table*)))
       (when field
         (let* ((key (intern (mangle-field-name (lstring (slot-value field '|name|))) :openldk))
                (clazz (slot-value field '|clazz|))
                (lname (lstring (slot-value clazz '|name|)))
                (bin-name (substitute #\/ #\. lname))
                (pkg (class-package bin-name))
                (static-sym (find-symbol (format nil "+static-~A+" bin-name) pkg)))
           (when (and static-sym (boundp static-sym))
             (let ((static-obj (symbol-value static-sym)))
               (when (slot-boundp static-obj key)
                 (slot-value static-obj key))))))))
    (t nil)))

(defmethod |putReference(Ljava/lang/Object;JLjava/lang/Object;)| ((unsafe |sun/misc/Unsafe|) obj l value)
  (cond
    ((typep obj 'java-array) (setf (jaref obj l) value))
    ((typep obj '|java/lang/Object|)
     (let* ((field (gethash l *field-offset-table*))
            (key (intern (mangle-field-name (lstring (slot-value field '|name|))) :openldk)))
       (setf (slot-value obj key) value)))
    ((null obj)
     (let* ((field (gethash l *field-offset-table*))
            (key (intern (mangle-field-name (lstring (slot-value field '|name|))) :openldk))
            (clazz (slot-value field '|clazz|))
            (lname (lstring (slot-value clazz '|name|)))
            (bin-name (substitute #\/ #\. lname))
            (pkg (class-package bin-name)))
       (setf (slot-value (eval (intern (format nil "+static-~A+" bin-name) pkg)) key) value)))
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
  ;; FIXME -- implement
  nil)

(defun |java/security/AccessController.ensureMaterializedForStackWalk(Ljava/lang/Object;)| (obj)
  "JDK 17 no-op: prevents JIT from optimizing away references during stack walks."
  (declare (ignore obj))
  nil)

;; ---------------------------------------------------------------------------
;; JDK 17: SystemProps$Raw native methods for System.initPhase1()
;; platformProperties() returns a String[39] indexed by the _NDX constants.
;; vmProperties() returns String[] of key-value pairs (like -D properties).

(defun |jdk/internal/util/SystemProps$Raw.platformProperties()| ()
  "Return a String[39] of indexed platform properties for JDK 17."
  (let* ((len 39)
         (arr (make-array len :initial-element nil)))
    ;; Index 0: _display_country_NDX
    (setf (aref arr 0) (jstring "US"))
    ;; Index 1: _display_language_NDX
    (setf (aref arr 1) (jstring "en"))
    ;; Index 2: _display_script_NDX  (empty)
    (setf (aref arr 2) (jstring ""))
    ;; Index 3: _display_variant_NDX (empty)
    (setf (aref arr 3) (jstring ""))
    ;; Index 4: _file_encoding_NDX
    (setf (aref arr 4) (jstring "UTF-8"))
    ;; Index 5: _file_separator_NDX
    (setf (aref arr 5) (jstring "/"))
    ;; Index 6: _format_country_NDX
    (setf (aref arr 6) (jstring "US"))
    ;; Index 7: _format_language_NDX
    (setf (aref arr 7) (jstring "en"))
    ;; Index 8: _format_script_NDX (empty)
    (setf (aref arr 8) (jstring ""))
    ;; Index 9: _format_variant_NDX (empty)
    (setf (aref arr 9) (jstring ""))
    ;; Indices 10-17: proxy settings (nil = not set)
    ;; Index 18: _java_io_tmpdir_NDX
    (setf (aref arr 18) (jstring (namestring (uiop:temporary-directory))))
    ;; Index 19: _line_separator_NDX
    (setf (aref arr 19) (jstring (format nil "~%")))
    ;; Index 20: _os_arch_NDX
    (setf (aref arr 20) (jstring (cond
                                   ((find :X86-64 *features*) "amd64")
                                   ((find :ARM64 *features*) "aarch64")
                                   (t "unknown"))))
    ;; Index 21: _os_name_NDX
    (setf (aref arr 21) (jstring (cond
                                   ((find :LINUX *features*) "Linux")
                                   ((find :DARWIN *features*) "Mac OS X")
                                   (t "Unknown"))))
    ;; Index 22: _os_version_NDX
    (setf (aref arr 22) (jstring (cond
                                   ((find :LINUX *features*)
                                    (handler-case
                                        (with-open-file (stream "/proc/version" :direction :input)
                                          (let* ((line (read-line stream))
                                                 (version-start (+ (search "Linux version " line)
                                                                   (length "Linux version ")))
                                                 (space-pos (position #\Space line :start version-start)))
                                            (subseq line version-start space-pos)))
                                      (condition () "0.0")))
                                   ((find :DARWIN *features*)
                                    (string-trim '(#\Newline) (uiop:run-program "sw_vers --productVersion" :output :string)))
                                   (t "0.0"))))
    ;; Index 23: _path_separator_NDX
    (setf (aref arr 23) (jstring ":"))
    ;; Indices 24-26: SOCKS proxy (nil)
    ;; Index 27: _sun_arch_abi_NDX (empty)
    (setf (aref arr 27) (jstring ""))
    ;; Index 28: _sun_arch_data_model_NDX
    (setf (aref arr 28) (jstring "64"))
    ;; Index 29: _sun_cpu_endian_NDX
    (setf (aref arr 29) (jstring (if (find :LITTLE-ENDIAN *features*) "little" "big")))
    ;; Index 30: _sun_cpu_isalist_NDX (empty)
    (setf (aref arr 30) (jstring ""))
    ;; Index 31: _sun_io_unicode_encoding_NDX
    (setf (aref arr 31) (jstring (if (find :LITTLE-ENDIAN *features*) "UnicodeLittle" "UnicodeBig")))
    ;; Index 32: _sun_jnu_encoding_NDX
    (setf (aref arr 32) (jstring "UTF-8"))
    ;; Index 33: _sun_os_patch_level_NDX (empty)
    (setf (aref arr 33) (jstring ""))
    ;; Index 34: _sun_stderr_encoding_NDX
    (setf (aref arr 34) (jstring "UTF-8"))
    ;; Index 35: _sun_stdout_encoding_NDX
    (setf (aref arr 35) (jstring "UTF-8"))
    ;; Index 36: _user_dir_NDX
    (setf (aref arr 36) (jstring (namestring (uiop:getcwd))))
    ;; Index 37: _user_home_NDX
    (setf (aref arr 37) (jstring (uiop:getenv "HOME")))
    ;; Index 38: _user_name_NDX
    (setf (aref arr 38) (jstring (slot-value (sb-posix:getpwuid (sb-posix:getuid)) 'sb-posix::name)))
    ;; Wrap as a java-array
    (make-java-array :component-class (gethash "java/lang/String" *java-classes-by-bin-name*)
                     :initial-contents arr)))

(defun |jdk/internal/util/SystemProps$Raw.vmProperties()| ()
  "Return String[] of key-value pairs for JDK 21 VM properties."
  (let* ((pairs `(("java.home" ,(uiop:getenv "JAVA_HOME"))
                  ("java.specification.version" "21")
                  ("java.specification.name" "Java Platform API Specification")
                  ("java.specification.vendor" "Oracle Corporation")
                  ("java.vm.specification.version" "21")
                  ("java.vm.specification.name" "Java Virtual Machine Specification")
                  ("java.vm.specification.vendor" "Oracle Corporation")
                  ("java.vm.name" "OpenLDK")
                  ("java.vm.version" "1.0")
                  ("java.vm.vendor" "OpenLDK")
                  ("java.vm.info" "interpreted mode")
                  ("java.version" "21")
                  ("java.version.date" "2024-09-17")
                  ("java.runtime.version" "21+35")
                  ("java.runtime.name" "OpenLDK Runtime Environment")
                  ("java.vendor" "OpenLDK")
                  ("java.vendor.url" "https://github.com/atgreen/openldk")
                  ("java.vendor.url.bug" "https://github.com/atgreen/openldk/issues")
                  ("java.class.version" "65.0")
                  ("java.class.path" ,(or (uiop:getenv "LDK_CLASSPATH")
                                          (uiop:getenv "CLASSPATH")
                                          "."))
                  ("java.library.path" ,(concatenate 'string
                                                     (uiop:getenv "JAVA_HOME")
                                                     "/lib/"))
                  ("java.io.tmpdir" ,(namestring (uiop:temporary-directory)))
                  ("file.encoding" "UTF-8")
                  ("file.encoding.pkg" "sun.io")
                  ("native.encoding" "UTF-8")
                  ("stdout.encoding" "UTF-8")
                  ("stderr.encoding" "UTF-8")
                  ("sun.cds.enableSharedLookupCache" "1")
                  ("java.security.debug" "0")
                  ("log4j2.disable.jmx" "true")))
         ;; Flatten to alternating key-value string array
         (flat (loop for (k v) in pairs
                     when v
                     collect (jstring k)
                     and collect (jstring v)))
         (arr (make-array (length flat) :initial-contents flat)))
    (make-java-array :component-class (gethash "java/lang/String" *java-classes-by-bin-name*)
                     :initial-contents arr)))

(defun |java/lang/System.initProperties(Ljava/util/Properties;)| (props)
  (dolist (prop `(("log4j2.disable.jmx" . "true")
                  ("java.specification.version" . "21")
                  ("java.specification.name" . "Java Platform API Specification")
                  ("java.specification.vendor" . "Oracle Corporation")
                  ("java.vm.specification.version" . "21")
                  ("java.vm.specification.name" . "Java Virtual Machine Specification")
                  ("java.vm.specification.vendor" . "Oracle Corporation")
                  ("java.vm.name" . "OpenLDK")
                  ("java.vm.version" . "1.0")
                  ("java.vm.vendor" . "OpenLDK")
                  ("java.version" . "21")
                  ("java.vendor" . "OpenLDK")
                  ("java.vendor.url" . "https://github.com/atgreen/openldk")
                  ("java.vendor.url.bug" . "https://github.com/atgreen/openldk/issues")
                  ("java.class.version" . "65.0")
                  ("sun.cds.enableSharedLookupCache" . "1")
                  ("java.class.path" . ,(or (uiop:getenv "LDK_CLASSPATH")
                                            (uiop:getenv "CLASSPATH")
                                            "."))
                  ("java.home" . ,(uiop:getenv "JAVA_HOME"))
                  ("user.home" . ,(uiop:getenv "HOME"))
                  ("user.dir" . ,(namestring (uiop:getcwd)))
                  ("user.name" . ,(let ((uid (sb-posix:getuid)))
                                    (slot-value (sb-posix:getpwuid uid) 'sb-posix::name)))
                  ("os.name" . ,(cond
                                  ((find :LINUX *features*)
                                   "Linux")
                                  ((find :DARWIN *features*)
                                   "Mac OS X")
                                  (t (error "internal error"))))
                  ("os.version" . ,(cond
                                    ((find :LINUX *features*)
                                     (with-open-file (stream "/proc/version" :direction :input)
                                                     (let ((line (read-line stream)))
                                                       (let* ((version-start (+ (search "Linux version " line)
                                                                                (length "Linux version ")))
                                                              (space-pos (position #\Space line :start version-start))
                                                              (version (subseq line version-start space-pos)))
                                                         version))))
                                    ((find :DARWIN *features*)
                                     (string-trim '(#\Newline) (uiop:run-program "sw_vers --productVersion" :output :string)))
                                    (error "internal error")))
                  ("os.arch" . ,(cond
                                  ((find :X86-64 *features*)
                                   "amd64")
                                  ((find :ARM64 *features*)
                                   "aarch64")
                                  (t (error "internal error"))))
                  ("sun.jnu.encoding" . "UTF-8")
                  ("sun.cpu.endian" . ,(cond
                                         ((find :LITTLE-ENDIAN *features*)
                                          "little")
                                         ((find :BIG-ENDIAN *features*)
                                          "big")
                                         (t (error "internal error"))))
                  ("file.separator" . ,(cond
                                         ((find :UNIX *features*) "/")
                                         (t (error "internal error"))))
                  ("file.encoding.pkg" . "sun.io")
                  ("java.io.tmpdir" . ,(namestring (uiop:temporary-directory)))
                  ("file.encoding" . "UTF-8")
                  ("path.separator" . ":")
                  ("java.library.path" . ,(concatenate 'string
                                                       (uiop:getenv "JAVA_HOME")
                                                       "/lib/"))
                  ("java.security.debug" . "0")
                  ("line.separator" . ,(format nil "~%"))
                  ("native.encoding" . "UTF-8")
                  ("stdout.encoding" . "UTF-8")
                  ("stderr.encoding" . "UTF-8")
                  ("sun.stdout.encoding" . "UTF-8")
                  ("sun.stderr.encoding" . "UTF-8")
                  ("java.version.date" . "2024-09-17")
                  ("java.runtime.version" . "21+35")))
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

(defun |java/io/FileDescriptor.getHandle(I)| (fd)
  "Get OS file handle for fd.  On Unix, return -1 (handles are Windows-only)."
  (declare (ignore fd))
  -1)

(defun |java/io/FileDescriptor.getAppend(I)| (fd)
  "Check if fd is in append mode.  Return false for stdin/stdout/stderr."
  (declare (ignore fd))
  0)

(defmethod |valid()| ((this |java/io/FileDescriptor|))
  "Return true (1) if the file descriptor is valid (has an open stream or known fd)."
  (let ((fd (when (slot-exists-p this '|fd|) (slot-value this '|fd|))))
    (cond
      ((and (streamp fd) (open-stream-p fd)) 1)
      ((and (integerp fd) (>= fd 0)) 1)
      (t 0))))

(defun |java/io/FileInputStream.initIDs()| ()
  "Initialize FileInputStream native IDs (no-op)."
  nil)

(defun |java/io/FileOutputStream.initIDs()| ()
  "Initialize FileOutputStream native IDs (no-op)."
  nil)

(defun |java/io/RandomAccessFile.initIDs()| ()
  "Initialize RandomAccessFile native IDs (no-op)."
  nil)

(defun |java/io/Console.istty()| ()
  "Return whether stdin is a tty."
  (not (zerop (sb-alien:alien-funcall
               (sb-alien:extern-alien "isatty" (function sb-alien:int sb-alien:int))
               0))))

(defun |java/lang/System.console()| ()
  "Return null - console object not yet supported in OpenLDK."
  nil)
(setf (gethash "java/lang/System.console()Ljava/io/Console;" *native-overrides*)
      #'|java/lang/System.console()|)

;; Override CheckConsole.haveConsole() to always return true.
;; In Kawa's processArgs bytecode, haveConsole()==false skips Shell.run()
;; entirely and only tries startGuiConsole (which fails without a display).
;; Returning true ensures Shell.run() is always called, which correctly
;; handles both TTY input (with prompts) and piped input (no prompts).
(defun |gnu/kawa/io/CheckConsole.haveConsole()| ()
  1)
(setf (gethash "gnu/kawa/io/CheckConsole.haveConsole()Z" *native-overrides*)
      #'|gnu/kawa/io/CheckConsole.haveConsole()|)

;; Override TtyInPort.make() to directly create a plain TtyInPort.
;; The bytecoded version has a codegen bug: the exception handler for the
;; JLine reflection path doesn't fall through to the fallback TtyInPort
;; creation code (end-of-handler? prevents it). This native override
;; bypasses the buggy compiled code entirely.
(defun |gnu/kawa/io/TtyInPort.make(Ljava/io/InputStream;Lgnu/kawa/io/Path;Lgnu/kawa/io/OutPort;)| (in-stream path out-port)
  (let ((tty (make-instance (intern "gnu/kawa/io/TtyInPort" (find-package "OPENLDK.APP")))))
    (|<init>(Ljava/io/InputStream;Lgnu/kawa/io/Path;Lgnu/kawa/io/OutPort;)| tty in-stream path out-port)
    tty))
(setf (gethash "gnu/kawa/io/TtyInPort.make(Ljava/io/InputStream;Lgnu/kawa/io/Path;Lgnu/kawa/io/OutPort;)Lgnu/kawa/io/TtyInPort;" *native-overrides*)
      #'|gnu/kawa/io/TtyInPort.make(Ljava/io/InputStream;Lgnu/kawa/io/Path;Lgnu/kawa/io/OutPort;)|)

(defun |java/io/Console.encoding()| ()
  "Return the console encoding, or null for default."
  nil)

(defun |java/io/Console.echo(Z)| (on)
  "Set console echo mode. Returns previous value."
  (declare (ignore on))
  t)

(defun |sun/nio/ch/IOUtil.initIDs()| ()
  "Initialize NIO IOUtil native IDs (no-op)."
  nil)

(defmethod |run()| (arg)
  (declare (ignore arg))
  (error (%lisp-condition (%make-throwable '|java/lang/UnsupportedOperationException|))))

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
              ;; java.lang.Object is assignable from everything
              ((string= this-name "java.lang.Object") 1)
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
    ((null param-object)
     ;; Static field access: look up the static singleton
     (let* ((field (gethash param-long *field-offset-table*))
            (key (intern (mangle-field-name (lstring (slot-value field '|name|))) :openldk))
            (clazz (slot-value field '|clazz|))
            (lname (lstring (slot-value clazz '|name|)))
            (bin-name (substitute #\/ #\. lname))
            (pkg (class-package bin-name)))
       (slot-value (eval (intern (format nil "+static-~A+" bin-name) pkg)) key)))
    (t
     (let* ((field (gethash param-long *field-offset-table*))
            (key (intern (mangle-field-name (lstring (slot-value field '|name|))) :openldk)))
       (slot-value param-object key)))))

(defmethod |putIntVolatile(Ljava/lang/Object;JI)| ((unsafe |sun/misc/Unsafe|) obj offset value)
  "Same as putInt for OpenLDK (no volatile semantics needed in Lisp)."
  (declare (ignore unsafe))
  (cond
    ((typep obj 'java-array)
     (setf (jaref obj offset) value))
    ((null obj)
     (let* ((field (gethash offset *field-offset-table*))
            (key (intern (mangle-field-name (lstring (slot-value field '|name|))) :openldk))
            (clazz (slot-value field '|clazz|))
            (lname (lstring (slot-value clazz '|name|)))
            (bin-name (substitute #\/ #\. lname))
            (pkg (class-package bin-name)))
       (setf (slot-value (eval (intern (format nil "+static-~A+" bin-name) pkg)) key) value)))
    (t
     (let* ((field (gethash offset *field-offset-table*))
            (key (intern (mangle-field-name (lstring (slot-value field '|name|))) :openldk)))
       (setf (slot-value obj key) value)))))

(defmethod |getCharVolatile(Ljava/lang/Object;J)| ((unsafe |sun/misc/Unsafe|) param-object param-long)
  (cond
    ((typep param-object 'java-array)
     (jaref param-object param-long))
    ((null param-object)
     ;; Static field access: look up the static singleton
     (let* ((field (gethash param-long *field-offset-table*))
            (key (intern (mangle-field-name (lstring (slot-value field '|name|))) :openldk))
            (clazz (slot-value field '|clazz|))
            (lname (lstring (slot-value clazz '|name|)))
            (bin-name (substitute #\/ #\. lname))
            (pkg (class-package bin-name)))
       (slot-value (eval (intern (format nil "+static-~A+" bin-name) pkg)) key)))
    (t
     (let* ((field (gethash param-long *field-offset-table*))
            (key (intern (mangle-field-name (lstring (slot-value field '|name|))) :openldk)))
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
    (when (and lclass (super lclass))
      (or (gethash (super lclass) *java-classes-by-bin-name*)
          (let ((super-lclass (classload (super lclass))))
            (when super-lclass
              (java-class super-lclass)))))))

;; Kawa's ClassType.getSuperclass() has a guard that checks
;; "java.lang.Object".equals(getName()) to avoid calling
;; reflectClass.getSuperclass() on Object (which returns null).
;; However, ClassType objects may store names with slashes
;; ("java/lang/Object"), so the guard fails and Type.make(null)
;; is called, triggering a NullPointerException on null.isArray().
;; This :around method catches that NPE and returns nil, which is
;; the correct result: Object has no superclass.
(defmethod |getSuperclass()| :around (self)
  (handler-case (call-next-method)
    (|condition-java/lang/NullPointerException| () nil)))

(defmethod |getInterfaces0()| ((class |java/lang/Class|))
  ;; FIXME: do something different for interfaces?
  (let ((lclass (get-ldk-class-for-java-class class)))
    (make-java-array
     :component-class (%get-java-class-by-fq-name "java.lang.Class")
     :initial-contents
     (if lclass
         (coerce (remove nil (mapcar (lambda (iname)
                                        (let ((lc (gethash iname *ldk-classes-by-bin-name*)))
                                          (when lc (java-class lc))))
                                      (coerce (interfaces lclass) 'list)))
                 'vector)
         #()))))

(defun java-class-to-type-descriptor (jclass)
  "Convert a java.lang.Class object to its JVM type descriptor string.
   E.g., int -> \"I\", java.lang.String -> \"Ljava/lang/String;\", int[] -> \"[I\"."
  (let ((name (lstring (slot-value jclass '|name|))))
    (cond
      ((string= name "int") "I")
      ((string= name "long") "J")
      ((string= name "boolean") "Z")
      ((string= name "byte") "B")
      ((string= name "char") "C")
      ((string= name "short") "S")
      ((string= name "float") "F")
      ((string= name "double") "D")
      ((string= name "void") "V")
      ;; Array types: name is like "[I" or "[Ljava.lang.String;" - already in descriptor form
      ;; but with dots instead of slashes for the element type
      ((and (> (length name) 0) (char= (char name 0) #\[))
       (substitute #\/ #\. name))
      ;; Object types
      (t (format nil "L~A;" (substitute #\/ #\. name))))))

(defun build-method-descriptor (parameter-types &optional return-type)
  "Build a JVM method descriptor string from parameterTypes (java-array of Class)
   and optional returnType (Class). If return-type is nil, uses V (void)."
  (format nil "(~{~A~})~A"
          (if parameter-types
              (loop for pclass across (java-array-data parameter-types)
                    collect (java-class-to-type-descriptor pclass))
              nil)
          (if return-type
              (java-class-to-type-descriptor return-type)
              "V")))

(defun |sun/reflect/NativeConstructorAccessorImpl.newInstance0(Ljava/lang/reflect/Constructor;[Ljava/lang/Object;)|
    (constructor params)
  (let* ((java-class (slot-value constructor '|clazz|))
         (bin-class-name (substitute #\/ #\. (lstring (slot-value java-class '|name|)))))
    (|java/lang/Class.forName0(Ljava/lang/String;ZLjava/lang/ClassLoader;Ljava/lang/Class;)| (jstring bin-class-name) nil nil nil)
    ;; Get class package from loader - class symbols live in loader's package
    (let* ((pkg (class-package bin-class-name))
           (class-sym (intern bin-class-name pkg))
           (obj (make-instance class-sym))
           (descriptor (build-method-descriptor (slot-value constructor '|parameterTypes|))))
      ;; Ensure clazz metadata is populated
      (when (slot-exists-p obj '|clazz|)
        (let ((klass (%get-java-class-by-bin-name bin-class-name t)))
          (when klass (setf (slot-value obj '|clazz|) klass))))
      (if (string= "()V" descriptor)
          (|<init>()| obj)
          (progn
            (apply (intern
                    (lispize-method-name
                     (format nil "<init>~A" descriptor))
                    :openldk)
                   ;; params can be NIL for zero-arg constructor paths; guard before accessing array-data.
                   (cons obj (if params (coerce (java-array-data params) 'list) nil)))))
      ; (format t "~&NEWINSTANCE0 ~A = ~A~%" constructor obj)
      obj)))

(defmethod |ensureClassInitialized(Ljava/lang/Class;)| ((unsafe |sun/misc/Unsafe|) class)
  (let ((lclass (get-ldk-class-for-java-class class)))
    (when lclass
      (%clinit lclass))))

;; JDK 17: Unsafe.ensureClassInitialized0 — native variant
(defmethod |ensureClassInitialized0(Ljava/lang/Class;)| ((unsafe |jdk/internal/misc/Unsafe|) class)
  (let ((lclass (get-ldk-class-for-java-class class)))
    (when lclass
      (%clinit lclass))))

(defmethod |shouldBeInitialized(Ljava/lang/Class;)| ((unsafe |sun/misc/Unsafe|) class)
  (let ((lclass (get-ldk-class-for-java-class class)))
    (if (and lclass (initialized-p lclass))
        nil
        t)))

(defmethod |shouldBeInitialized0(Ljava/lang/Class;)| ((unsafe |jdk/internal/misc/Unsafe|) class)
  (let ((lclass (get-ldk-class-for-java-class class)))
    (if (and lclass (initialized-p lclass))
        nil
        t)))

(defvar *unsafe-memory-table* (make-hash-table))


(defun |java/lang/System.mapLibraryName(Ljava/lang/String;)| (library-name)
  (or #+LINUX (jstring (format nil "lib~A.so" (lstring library-name)))
      #+DARWIN (jstring (format nil "lib~A.dylib" (lstring library-name)))
      (error "unimplemented")))

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

(defun |java/lang/ClassLoader.defineClass1(Ljava/lang/ClassLoader;Ljava/lang/String;[BIILjava/security/ProtectionDomain;Ljava/lang/String;)|
    (loader name bytes offset len pd source)
  "Define a class from byte array data using the specified class loader.
   JDK 17 static native — delegates to %classload-from-stream for full
   class setup (module, inner classes, nest host, throwable conditions, etc.)."
  (declare (ignore pd source))
  (let* ((ldk-loader (get-ldk-loader-for-java-loader loader))
         (class-name (substitute #\/ #\. (lstring name)))
         (stream (make-instance 'byte-array-input-stream
                                :array bytes :start offset :end (+ offset len)))
         (result (%classload-from-stream class-name stream loader ldk-loader)))
    (unless result
      (let ((exc (%make-java-instance "java/lang/NoClassDefFoundError")))
        (|<init>(Ljava/lang/String;)| exc name)
        (error (%lisp-condition exc))))
    (java-class result)))

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
      (let ((stream (open (lstring filename)
                          :element-type '(unsigned-byte 8)
                          :direction :input))
            (fd (slot-value fis '|fd|)))
        ;; Store the Lisp stream inside the FileDescriptor's fd slot
        ;; (like FileOutputStream does), so fd.valid() works in JDK 17.
        (if (and fd (slot-exists-p fd '|fd|))
            (setf (slot-value fd '|fd|) stream)
            (setf (slot-value fis '|fd|) stream)))
    ((or sb-ext:file-does-not-exist sb-int:simple-file-error) (e)
      (declare (ignore e))
      (let ((fnf (%make-java-instance "java/io/FileNotFoundException")))
        (|<init>(Ljava/lang/String;)| fnf filename)
        (error (%lisp-condition fnf))))))

(defmethod |skip0(J)| ((fis |java/io/FileInputStream|) n)
  (let* ((file-descriptor (slot-value fis '|fd|))
         (in-stream (if (and file-descriptor (slot-exists-p file-descriptor '|fd|))
                        (slot-value file-descriptor '|fd|)
                        file-descriptor))
         (bytes-read 0))
    (when (eq n :END)
      (setf n 999999999999))
    (loop for i from 0 below n
          for byte = (read-byte in-stream nil nil)
          while byte
          do (incf bytes-read))
    bytes-read))

(defmethod |readBytes([BII)| ((fis |java/io/FileInputStream|) byte-array offset length)
  (let* ((file-descriptor (slot-value fis '|fd|))
         (fd (if (and file-descriptor (slot-exists-p file-descriptor '|fd|))
                 (slot-value file-descriptor '|fd|)
                 file-descriptor))
         (in-stream (cond ((eql fd 0)
                           ;; Flush stdout before blocking on stdin, like C stdio.
                           (force-output *standard-output*)
                           *standard-input*)
                          ((streamp fd) fd)
                          (t (error "unimplemented fd ~A in FileInputStream.readBytes" fd))))
         (bytes-read 0))
    ;; First byte: block waiting for input.
    ;; Subsequent bytes: only read if immediately available (listen).
    ;; This matches OS read() behavior on terminals, which returns
    ;; available data rather than trying to fill the entire buffer.
    (when (plusp length)
      (let ((byte (read-byte in-stream nil nil)))
        (when byte
          (setf (jaref byte-array offset) byte)
          (incf bytes-read)
          (loop for i from (1+ offset) below (+ offset length)
                while (listen in-stream)
                for b = (read-byte in-stream nil nil)
                while b
                do (setf (jaref byte-array i) b)
                   (incf bytes-read)))))
    (if (and (zerop bytes-read) (plusp length)) -1 bytes-read)))

(defmethod |available0()| ((fis |java/io/FileInputStream|))
  (let* ((file-descriptor (slot-value fis '|fd|))
         (fd (if (and file-descriptor (slot-exists-p file-descriptor '|fd|))
                 (slot-value file-descriptor '|fd|)
                 file-descriptor)))
    (cond
      ((eql fd 0) 0)
      ((streamp fd) (- (file-length fd) (file-position fd)))
      (t 0))))

(defmethod |isInstance(Ljava/lang/Object;)| ((this |java/lang/Class|) objref)
  (let* ((class-name (lstring (slot-value this '|name|)))
         (normalized-name (substitute #\/ #\. class-name))
         ;; Get class's loader package for correct type lookup
         (pkg (class-package normalized-name))
         (class-symbol (intern normalized-name pkg)))
    ;; Handle native Lisp types (integers, floats, doubles, characters)
    (cond
      ((%native-type-castable-p objref normalized-name)
       1)
      ;; Java arrays (java-array structs) are instances of Object, Cloneable, Serializable,
      ;; and matching array types
      ((java-array-p objref)
       (cond
         ((member class-name '("java.lang.Object" "java.lang.Cloneable" "java.io.Serializable")
                  :test #'string=) 1)
         ;; Check if the target is an array class matching the component type
         ((and (plusp (length class-name)) (char= (char class-name 0) #\[)) 1)
         (t 0)))
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

;; Flush the underlying Lisp stream for stdout/stderr.
;; In standard Java, FileOutputStream.write() calls the OS write() directly
;; (no Lisp-level buffering). In OpenLDK, writes go through SBCL's stream
;; buffering, so we need explicit force-output when Java code calls flush().
(defmethod |flush()| ((fos |java/io/FileOutputStream|))
  (let* ((file-descriptor (slot-value fos '|fd|))
         (fd (if (and file-descriptor (slot-exists-p file-descriptor '|fd|))
                 (slot-value file-descriptor '|fd|)
                 file-descriptor)))
    (cond
      ((eq fd 1) (force-output *standard-output*))
      ((eq fd 2) (force-output *error-output*))
      ((streamp fd) (force-output fd)))))

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
    (let ((jenvs (make-java-array :component-class "[B" :size (* 2 (length env)))))
      (loop for kv in env
            for i from 0 by 2
            for p = (position #\= kv)
            do (progn
                 (setf (jaref jenvs i)
                       (make-java-array :component-class "B" :initial-contents (flexi-streams:string-to-octets (subseq kv 0 p) :external-format :utf-8)))
                 (setf (jaref jenvs (+ i 1))
                       (make-java-array :component-class "B" :initial-contents (flexi-streams:string-to-octets (subseq kv (1+ p)) :external-format :utf-8)))))
      jenvs)))

(defun |java/lang/System.identityHashCode(Ljava/lang/Object;)| (objref)
  ;; Return a unique, stable identity hash per object instance.
  ;; SBCL's sxhash returns the same value for all instances of the same
  ;; CLOS class, which breaks identity-based hash tables like Kawa's
  ;; AbstractWeakHashTable. Instead, assign a unique counter-based hash
  ;; on first access (mimicking JVM object header identity hash).
  (if (null objref)
      0
      (bordeaux-threads:with-lock-held (*identity-hash-counter-lock*)
        (or (gethash objref *identity-hash-table*)
            (let ((hash (unsigned-to-signed-integer
                         (logand (incf *identity-hash-counter*) #xFFFFFFFF))))
              (setf (gethash objref *identity-hash-table*) hash)
              hash)))))

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
  ;; The interrupted status is maintained in the *thread-interrupted* hash table.
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
         ;; Unbox primitive wrapper types only when the parameter type is primitive
         (when args
           (let ((param-types (slot-value method '|parameterTypes|)))
             (dotimes (i (length (java-array-data args)))
               (let ((arg (jaref args i))
                     (param-type (when param-types (jaref param-types i))))
                 ;; Only unbox if parameter type is primitive (isPrimitive returns 1)
                 (when (and param-type (eq 1 (|isPrimitive()| param-type)))
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
                      (setf (jaref args i) (slot-value arg '|value|)))))))))
         (let* ((java-class (slot-value method '|clazz|))
                (class-name (substitute #\/ #\. (lstring (slot-value java-class '|name|))))
                (is-static (not (eq 0 (logand #x8 (slot-value method '|modifiers|)))))
                ;; For static methods, use the class's package; for instance methods use :openldk
                (java-loader (slot-value java-class '|classLoader|))
                (ldk-loader (get-ldk-loader-for-java-loader java-loader))
                (descriptor (build-method-descriptor
                             (slot-value method '|parameterTypes|)
                             (slot-value method '|returnType|)))
                (method-name (lispize-method-name
                              (if is-static
                                  (concatenate 'string
                                               class-name
                                               "."
                                               (lstring (slot-value method '|name|))
                                               descriptor)
                                  (concatenate 'string
                                               (lstring (slot-value method '|name|))
                                               descriptor))))
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

;; JDK 17: findBootstrapClass is a private static native method
(defun |java/lang/ClassLoader.findBootstrapClass(Ljava/lang/String;)| (name)
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

(defmethod |checkAccess(Ljava/io/File;I)| ((ufs |java/io/UnixFileSystem|) file access-mode)
  ;; TODO: check actual POSIX permissions (R_OK/W_OK/X_OK) instead of just file existence
  (declare (ignore ufs access-mode))
  (let ((path (lstring (slot-value file '|path|))))
    (handler-case
        (if (probe-file path) 1 0)
      (error () 0))))

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
    (setf (gethash ptr *unsafe-memory-table*) (cons mem 8))
    (setf (sb-alien:deref mem 0) value)
    (|<init>(JI)| dbb ptr 8)
    dbb))

(defmethod |getBoolean(Ljava/lang/Object;J)|((unsafe |sun/misc/Unsafe|) objref ptr)
  (declare (ignore unsafe))
  (let* ((field (gethash ptr *field-offset-table*))
         (key (intern (mangle-field-name (lstring (slot-value field '|name|))) :openldk)))
    (if objref
        (let ((v (slot-value objref key)))
          (if v (if (eql v 0) 0 1) 0))
        ;; Static field access: look up the static singleton
        (let* ((clazz (slot-value field '|clazz|))
               (lname (lstring (slot-value clazz '|name|)))
               (bin-name (substitute #\/ #\. lname))
               (pkg (class-package bin-name))
               (v (slot-value (eval (intern (format nil "+static-~A+" bin-name) pkg)) key)))
          (if v (if (eql v 0) 0 1) 0)))))

(defmethod |getInt(Ljava/lang/Object;J)|((unsafe |sun/misc/Unsafe|) objref ptr)
  (declare (ignore unsafe))
  (let* ((field (gethash ptr *field-offset-table*))
         (key (intern (mangle-field-name (lstring (slot-value field '|name|))) :openldk)))
    (if objref
        (slot-value objref key)
        ;; Static field access: look up the static singleton
        (let* ((clazz (slot-value field '|clazz|))
               (lname (lstring (slot-value clazz '|name|)))
               (bin-name (substitute #\/ #\. lname))
               (pkg (class-package bin-name)))
          (slot-value (eval (intern (format nil "+static-~A+" bin-name) pkg)) key)))))

(defmethod |putLong(Ljava/lang/Object;JJ)|((unsafe |sun/misc/Unsafe|) objref ptr value)
  (declare (ignore unsafe))
  (let* ((field (gethash ptr *field-offset-table*))
         (key (intern (mangle-field-name (lstring (slot-value field '|name|))) :openldk)))
    (if objref
        (setf (slot-value objref key) value)
        ;; Static field access: look up the static singleton
        (let* ((clazz (slot-value field '|clazz|))
               (lname (lstring (slot-value clazz '|name|)))
               (bin-name (substitute #\/ #\. lname))
               (pkg (class-package bin-name)))
          (setf (slot-value (eval (intern (format nil "+static-~A+" bin-name) pkg)) key) value)))))

(defmethod |putInt(Ljava/lang/Object;JI)|((unsafe |sun/misc/Unsafe|) objref ptr value)
  (declare (ignore unsafe))
  (let* ((field (gethash ptr *field-offset-table*))
         (key (intern (mangle-field-name (lstring (slot-value field '|name|))) :openldk)))
    (if objref
        (setf (slot-value objref key) value)
        ;; Static field access: look up the static singleton
        (let* ((clazz (slot-value field '|clazz|))
               (lname (lstring (slot-value clazz '|name|)))
               (bin-name (substitute #\/ #\. lname))
               (pkg (class-package bin-name)))
          (setf (slot-value (eval (intern (format nil "+static-~A+" bin-name) pkg)) key) value)))))

(defmethod |getLong(Ljava/lang/Object;J)|((unsafe |sun/misc/Unsafe|) objref ptr)
  (declare (ignore unsafe))
  (let* ((field (gethash ptr *field-offset-table*))
         (key (intern (mangle-field-name (lstring (slot-value field '|name|))) :openldk)))
    (if objref
        (slot-value objref key)
        ;; Static field access: look up the static singleton
        (let* ((clazz (slot-value field '|clazz|))
               (lname (lstring (slot-value clazz '|name|)))
               (bin-name (substitute #\/ #\. lname))
               (pkg (class-package bin-name)))
          (slot-value (eval (intern (format nil "+static-~A+" bin-name) pkg)) key)))))

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

(defun |sun/nio/ch/NativeThread.signal(J)| (thread-id)
  "Signal a native thread blocked in an I/O operation. No-op in single-threaded OpenLDK."
  (declare (ignore thread-id))
  nil)

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

(defun |sun/nio/ch/FileDispatcherImpl.size0(Ljava/io/FileDescriptor;)| (fd)
  "Return the size of the file associated with FD."
  (let ((real-fd (if (and (slot-exists-p fd '|fd|) (slot-boundp fd '|fd|))
                     (slot-value fd '|fd|)
                     fd)))
    (handler-case
        (let ((stat (sb-posix:fstat real-fd)))
          (sb-posix:stat-size stat))
      (sb-posix:syscall-error ()
        (error (%lisp-condition (%make-throwable '|java/io/IOException|)))))))

(defun |sun/nio/ch/FileDispatcherImpl.close0(Ljava/io/FileDescriptor;)| (fd)
  "Close the file descriptor."
  (let ((real-fd (if (and (slot-exists-p fd '|fd|) (slot-boundp fd '|fd|))
                     (slot-value fd '|fd|)
                     fd)))
    (handler-case
        (sb-posix:close real-fd)
      (sb-posix:syscall-error ()
        nil))))

(defun |sun/nio/ch/FileDispatcherImpl.preClose0(Ljava/io/FileDescriptor;)| (fd)
  "Pre-close — no-op in our implementation."
  (declare (ignore fd))
  nil)

(defun |sun/nio/ch/FileDispatcherImpl.closeIntFD(I)| (fd)
  "Close a raw int file descriptor."
  (handler-case (sb-posix:close fd)
    (sb-posix:syscall-error () nil)))

(defun |sun/nio/ch/FileDispatcherImpl.canTransferToFromOverlappedMap0()| ()
  0)

(defun |sun/nio/ch/FileChannelImpl.maxDirectTransferSize0()| ()
  ;; Linux default: 2GB
  (ash 1 31))

(defun |sun/nio/ch/FileDispatcherImpl.seek0(Ljava/io/FileDescriptor;J)| (fd offset)
  "lseek(2) — return current position or seek to OFFSET."
  (let ((real-fd (if (and (slot-exists-p fd '|fd|) (slot-boundp fd '|fd|))
                     (slot-value fd '|fd|)
                     fd)))
    (handler-case
        (if (= offset -1)
            ;; -1 means query current position (SEEK_CUR with offset 0)
            (sb-posix:lseek real-fd 0 sb-posix:seek-cur)
            (sb-posix:lseek real-fd offset sb-posix:seek-set))
      (sb-posix:syscall-error ()
        (error (%lisp-condition (%make-throwable '|java/io/IOException|)))))))

(defun |sun/nio/ch/FileDispatcherImpl.force0(Ljava/io/FileDescriptor;Z)| (fd metadata)
  "fsync/fdatasync the file descriptor."
  (declare (ignore metadata))
  (let ((real-fd (if (and (slot-exists-p fd '|fd|) (slot-boundp fd '|fd|))
                     (slot-value fd '|fd|)
                     fd)))
    (handler-case
        (sb-posix:fsync real-fd)
      (sb-posix:syscall-error ()
        (error (%lisp-condition (%make-throwable '|java/io/IOException|)))))))

(defun |sun/nio/ch/FileDispatcherImpl.write0(Ljava/io/FileDescriptor;JI)| (fd ptr length)
  "Write LENGTH bytes from native buffer at PTR to file descriptor FD."
  (let ((real-fd (if (and (slot-exists-p fd '|fd|) (slot-boundp fd '|fd|))
                     (slot-value fd '|fd|)
                     fd))
        (sap (sb-sys:int-sap ptr)))
    (sb-unix:unix-write real-fd sap 0 length)))

(defun |sun/nio/ch/FileDispatcherImpl.truncate0(Ljava/io/FileDescriptor;J)| (fd size)
  "Truncate the file to SIZE bytes."
  (let ((real-fd (if (and (slot-exists-p fd '|fd|) (slot-boundp fd '|fd|))
                     (slot-value fd '|fd|)
                     fd)))
    (handler-case
        (progn (sb-posix:ftruncate real-fd size) 0)
      (sb-posix:syscall-error ()
        (error (%lisp-condition (%make-throwable '|java/io/IOException|)))))))

(defun |sun/nio/ch/FileDispatcherImpl.read0(Ljava/io/FileDescriptor;JI)| (fd ptr length)
  "read(2) — read up to LENGTH bytes from FD into native buffer at PTR."
  (let ((real-fd (if (and (slot-exists-p fd '|fd|) (slot-boundp fd '|fd|))
                     (slot-value fd '|fd|)
                     fd))
        (sap (sb-sys:int-sap ptr)))
    (sb-unix:unix-read real-fd sap length)))

;;; ChannelInputStream.read([BII) native override — bypass heavy NIO path
;;; The Java NIO read path (FileChannelImpl → IOUtil → DirectByteBuffer
;;; allocation → Bits.tryReserveMemory CAS loops) triggers dozens of
;;; first-time JIT compilations.  Short-circuit with a direct read().
(setf (gethash "sun/nio/ch/ChannelInputStream.read([BII)I" *native-overrides*)
      (lambda (this b off len)
        (if (zerop len)
            0
            (let* ((ch (slot-value this '|ch|))
                   (fd-obj (when (typep ch '|sun/nio/ch/FileChannelImpl|)
                             (slot-value ch '|fd|)))
                   (fd (when fd-obj (slot-value fd-obj '|fd|))))
              (unless fd
                (error "ChannelInputStream.read: unsupported channel type ~A"
                       (type-of ch)))
              (let* ((mem (sb-alien:make-alien sb-alien:char len))
                     (sap (sb-alien:alien-sap mem))
                     (n (sb-unix:unix-read fd sap len)))
                (cond
                  ((and n (> n 0))
                   (let ((data (java-array-data b)))
                     (loop for i below n
                           do (setf (aref data (+ off i))
                                    (sb-sys:sap-ref-8 sap i))))
                   (sb-alien:free-alien mem)
                   n)
                  (t
                   (sb-alien:free-alien mem)
                   -1)))))))

;;; Channels$1.write([BII) native override — bypass heavy NIO write path
;;; The Java NIO write path (FileChannelImpl → IOUtil → DirectByteBuffer
;;; allocation → copyMemory) triggers complex memory management.
;;; Short-circuit with a direct write().
(setf (gethash "java/nio/channels/Channels$1.write([BII)V" *native-overrides*)
      (lambda (this b off len)
        (when (> len 0)
          (let* ((ch (slot-value this '|val$ch|))
                 (fd-obj (when (typep ch '|sun/nio/ch/FileChannelImpl|)
                           (slot-value ch '|fd|)))
                 (fd (when fd-obj (slot-value fd-obj '|fd|))))
            (unless fd
              (error "Channels$1.write: unsupported channel type ~A"
                     (type-of ch)))
            (let* ((data (java-array-data b))
                   (mem (sb-alien:make-alien sb-alien:char len))
                   (sap (sb-alien:alien-sap mem)))
              (loop for i below len
                    do (setf (sb-sys:sap-ref-8 sap i)
                             (let ((v (aref data (+ off i))))
                               (if (< v 0) (+ v 256) v))))
              (sb-unix:unix-write fd sap 0 len)
              (sb-alien:free-alien mem))))))

(defun |java/nio/MappedByteBuffer.checkBounds(III)| (off len size)
  (declare (ignore off)
           (ignore len)
           (ignore size))
  ;; FIXME
  nil)


(defun |sun/nio/fs/UnixNativeDispatcher.init()| ()
  "Return capabilities bitmask: openat(2) + futimes(4) + futimens(8)."
  ;; Bit 1 (2)  = SUPPORTS_OPENAT
  ;; Bit 2 (4)  = SUPPORTS_FUTIMES
  ;; Bit 3 (8)  = SUPPORTS_FUTIMENS
  ;; Bit 4 (16) = SUPPORTS_LUTIMES
  ;; Bit 5 (32) = SUPPORTS_XATTR
  ;; Bit 16 (65536) = SUPPORTS_BIRTHTIME
  (logior 2 4 8))

(defun %read-c-string-from-sap (address)
  "Read a null-terminated C string from native memory at ADDRESS."
  (let ((sap (sb-sys:int-sap address)))
    (loop for i from 0
          for byte = (sb-sys:sap-ref-8 sap i)
          until (zerop byte)
          collect (code-char byte) into chars
          finally (return (coerce chars 'string)))))

(defun |sun/nio/fs/UnixNativeDispatcher.exists0(J)| (address)
  "Check if file at native C-string ADDRESS exists. Returns non-zero if so."
  (let ((path (%read-c-string-from-sap address)))
    (if (probe-file path) 1 0)))

(defun %populate-unix-file-attributes (path attrs follow-links)
  "Populate a UnixFileAttributes object from PATH. FOLLOW-LINKS controls symlink behavior."
  (handler-case
      (let ((stat (if follow-links
                      (sb-posix:stat path)
                      (sb-posix:lstat path))))
        (when (slot-exists-p attrs '|st_mode|)
          (setf (slot-value attrs '|st_mode|) (sb-posix:stat-mode stat)))
        (when (slot-exists-p attrs '|st_ino|)
          (setf (slot-value attrs '|st_ino|) (sb-posix:stat-ino stat)))
        (when (slot-exists-p attrs '|st_dev|)
          (setf (slot-value attrs '|st_dev|) (sb-posix:stat-dev stat)))
        (when (slot-exists-p attrs '|st_rdev|)
          (setf (slot-value attrs '|st_rdev|) (sb-posix:stat-rdev stat)))
        (when (slot-exists-p attrs '|st_nlink|)
          (setf (slot-value attrs '|st_nlink|) (sb-posix:stat-nlink stat)))
        (when (slot-exists-p attrs '|st_uid|)
          (setf (slot-value attrs '|st_uid|) (sb-posix:stat-uid stat)))
        (when (slot-exists-p attrs '|st_gid|)
          (setf (slot-value attrs '|st_gid|) (sb-posix:stat-gid stat)))
        (when (slot-exists-p attrs '|st_size|)
          (setf (slot-value attrs '|st_size|) (sb-posix:stat-size stat)))
        (when (slot-exists-p attrs '|st_atime_sec|)
          (setf (slot-value attrs '|st_atime_sec|) (sb-posix:stat-atime stat)))
        (when (slot-exists-p attrs '|st_mtime_sec|)
          (setf (slot-value attrs '|st_mtime_sec|) (sb-posix:stat-mtime stat)))
        (when (slot-exists-p attrs '|st_ctime_sec|)
          (setf (slot-value attrs '|st_ctime_sec|) (sb-posix:stat-ctime stat)))
        0) ; success
    (sb-posix:syscall-error (e)
      (sb-posix:syscall-errno e))))

(defun |sun/nio/fs/UnixNativeDispatcher.stat0(JLsun/nio/fs/UnixFileAttributes;)| (address attrs)
  "stat(2) — populate UnixFileAttributes. Throws UnixException on failure (JDK 17: void return)."
  (let ((path (%read-c-string-from-sap address)))
    (let ((result (%populate-unix-file-attributes path attrs t)))
      (unless (zerop result)
        (let ((ux (%make-java-instance "sun/nio/fs/UnixException")))
          (when (slot-exists-p ux '|errno|)
            (setf (slot-value ux '|errno|) result))
          (error (%lisp-condition ux)))))))

(defun |sun/nio/fs/UnixNativeDispatcher.stat1(J)| (address)
  "stat(2) — returns st_mode on success, 0 on failure."
  (let ((path (%read-c-string-from-sap address)))
    (handler-case
        (sb-posix:stat-mode (sb-posix:stat path))
      (sb-posix:syscall-error (e)
        (declare (ignore e))
        0))))

(defun |sun/nio/fs/UnixNativeDispatcher.lstat0(JLsun/nio/fs/UnixFileAttributes;)| (address attrs)
  "lstat(2) — like stat0 but does not follow symlinks."
  (let ((path (%read-c-string-from-sap address)))
    (let ((result (%populate-unix-file-attributes path attrs nil)))
      (unless (zerop result)
        (let ((ux (%make-java-instance "sun/nio/fs/UnixException")))
          (when (slot-exists-p ux '|errno|)
            (setf (slot-value ux '|errno|) result))
          (error (%lisp-condition ux)))))))

(defun |sun/nio/fs/UnixNativeDispatcher.access0(JI)| (address mode)
  "access(2) — check file access. Throws UnixException on failure (JDK 17: void return)."
  (let ((path (%read-c-string-from-sap address)))
    (handler-case
        (sb-posix:access path mode)
      (sb-posix:syscall-error (e)
        (let ((ux (%make-java-instance "sun/nio/fs/UnixException")))
          (when (slot-exists-p ux '|errno|)
            (setf (slot-value ux '|errno|) (sb-posix:syscall-errno e)))
          (error (%lisp-condition ux)))))))

(defun |sun/nio/fs/UnixNativeDispatcher.open0(JII)| (address flags mode)
  "open(2) — open file. Returns file descriptor."
  (let ((path (%read-c-string-from-sap address)))
    (handler-case
        (sb-posix:open path flags mode)
      (sb-posix:syscall-error (e)
        (let ((ux (%make-java-instance "sun/nio/fs/UnixException")))
          (when (slot-exists-p ux '|errno|)
            (setf (slot-value ux '|errno|) (sb-posix:syscall-errno e)))
          (error (%lisp-condition ux)))))))

(defun |sun/nio/fs/UnixNativeDispatcher.close0(I)| (fd)
  "close(2) — close file descriptor."
  (handler-case
      (sb-posix:close fd)
    (sb-posix:syscall-error (e)
      (let ((ux (%make-java-instance "sun/nio/fs/UnixException")))
        (when (slot-exists-p ux '|errno|)
          (setf (slot-value ux '|errno|) (sb-posix:syscall-errno e)))
        (error (%lisp-condition ux))))))

(defun |sun/nio/fs/UnixNativeDispatcher.read0(IJI)| (fd address len)
  "read(2) — read from file descriptor into native buffer."
  (let ((sap (sb-sys:int-sap address)))
    (handler-case
        (let ((bytes-read 0))
          (loop for i below len
                for byte = (sb-posix:read fd (sb-sys:sap+ sap i) 1)
                while (plusp byte)
                do (incf bytes-read))
          bytes-read)
      (sb-posix:syscall-error (e)
        (let ((ux (%make-java-instance "sun/nio/fs/UnixException")))
          (when (slot-exists-p ux '|errno|)
            (setf (slot-value ux '|errno|) (sb-posix:syscall-errno e)))
          (error (%lisp-condition ux)))))))

(defun |sun/nio/fs/UnixNativeDispatcher.fstat0(ILsun/nio/fs/UnixFileAttributes;)| (fd attrs)
  "fstat(2) — stat by file descriptor."
  (handler-case
      (let ((stat (sb-posix:fstat fd)))
        (when (slot-exists-p attrs '|st_mode|)
          (setf (slot-value attrs '|st_mode|) (sb-posix:stat-mode stat)))
        (when (slot-exists-p attrs '|st_ino|)
          (setf (slot-value attrs '|st_ino|) (sb-posix:stat-ino stat)))
        (when (slot-exists-p attrs '|st_dev|)
          (setf (slot-value attrs '|st_dev|) (sb-posix:stat-dev stat)))
        (when (slot-exists-p attrs '|st_rdev|)
          (setf (slot-value attrs '|st_rdev|) (sb-posix:stat-rdev stat)))
        (when (slot-exists-p attrs '|st_nlink|)
          (setf (slot-value attrs '|st_nlink|) (sb-posix:stat-nlink stat)))
        (when (slot-exists-p attrs '|st_uid|)
          (setf (slot-value attrs '|st_uid|) (sb-posix:stat-uid stat)))
        (when (slot-exists-p attrs '|st_gid|)
          (setf (slot-value attrs '|st_gid|) (sb-posix:stat-gid stat)))
        (when (slot-exists-p attrs '|st_size|)
          (setf (slot-value attrs '|st_size|) (sb-posix:stat-size stat)))
        (when (slot-exists-p attrs '|st_atime_sec|)
          (setf (slot-value attrs '|st_atime_sec|) (sb-posix:stat-atime stat)))
        (when (slot-exists-p attrs '|st_mtime_sec|)
          (setf (slot-value attrs '|st_mtime_sec|) (sb-posix:stat-mtime stat)))
        (when (slot-exists-p attrs '|st_ctime_sec|)
          (setf (slot-value attrs '|st_ctime_sec|) (sb-posix:stat-ctime stat))))
    (sb-posix:syscall-error (e)
      (let ((ux (%make-java-instance "sun/nio/fs/UnixException")))
        (when (slot-exists-p ux '|errno|)
          (setf (slot-value ux '|errno|) (sb-posix:syscall-errno e)))
        (error (%lisp-condition ux))))))

(defun |sun/nio/fs/UnixNativeDispatcher.realpath0(J)| (address)
  "realpath(3) — resolve canonical path. Returns byte array."
  (let* ((path (%read-c-string-from-sap address))
         (real (namestring (truename (pathname path))))
         (bytes (flexi-streams:string-to-octets real :external-format :utf-8)))
    (make-java-array
     :component-class (%get-ldk-class-by-fq-name "byte")
     :initial-contents bytes)))

(defun |sun/nio/fs/UnixNativeDispatcher.getcwd()| ()
  (make-java-array
   :component-class (%get-ldk-class-by-fq-name "byte")
   :initial-contents (flexi-streams:string-to-octets (namestring (uiop:getcwd)) :external-format :utf-8)))

(defun |sun/nio/fs/UnixNativeDispatcher.dup(I)| (fd)
  "dup(2) — duplicate file descriptor."
  (handler-case
      (sb-posix:dup fd)
    (sb-posix:syscall-error (e)
      (let ((ux (%make-java-instance "sun/nio/fs/UnixException")))
        (when (slot-exists-p ux '|errno|)
          (setf (slot-value ux '|errno|) (sb-posix:syscall-errno e)))
        (error (%lisp-condition ux))))))

(defun |sun/nio/fs/UnixNativeDispatcher.openat0(IJII)| (dfd address flags mode)
  "openat(2) — open file relative to directory fd."
  (let* ((path (%read-c-string-from-sap address))
         (result (sb-alien:alien-funcall
                  (sb-alien:extern-alien "openat"
                                         (function sb-alien:int
                                                   sb-alien:int
                                                   sb-alien:c-string
                                                   sb-alien:int
                                                   sb-alien:int))
                  dfd path flags mode)))
    (when (< result 0)
      (let ((ux (%make-java-instance "sun/nio/fs/UnixException")))
        (when (slot-exists-p ux '|errno|)
          (setf (slot-value ux '|errno|) (sb-alien:get-errno)))
        (error (%lisp-condition ux))))
    result))

(defvar *dir-pointer-table* (make-hash-table)
  "Map from integer DIR* address to SBCL alien for closedir/readdir.")

(defun |sun/nio/fs/UnixNativeDispatcher.fdopendir(I)| (fd)
  "fdopendir(3) — open directory stream from fd. Returns DIR* as long."
  (let ((dirp (sb-alien:alien-funcall
               (sb-alien:extern-alien "fdopendir"
                                      (function (* t) sb-alien:int))
               fd)))
    (when (sb-alien:null-alien dirp)
      (let ((ux (%make-java-instance "sun/nio/fs/UnixException")))
        (when (slot-exists-p ux '|errno|)
          (setf (slot-value ux '|errno|) (sb-alien:get-errno)))
        (error (%lisp-condition ux))))
    (let ((addr (sb-sys:sap-int (sb-alien:alien-sap dirp))))
      (setf (gethash addr *dir-pointer-table*) dirp)
      addr)))

(defun |sun/nio/fs/UnixNativeDispatcher.closedir(J)| (dirp-addr)
  "closedir(3) — close directory stream."
  (let ((dirp (gethash dirp-addr *dir-pointer-table*)))
    (when dirp
      (remhash dirp-addr *dir-pointer-table*)
      (sb-posix:closedir dirp))))

(defun |sun/nio/fs/UnixNativeDispatcher.readdir(J)| (dirp-addr)
  "readdir(3) — read directory entry. Returns filename as byte[] or nil."
  (let ((dirp (gethash dirp-addr *dir-pointer-table*)))
    (unless dirp (return-from |sun/nio/fs/UnixNativeDispatcher.readdir(J)| nil))
    ;; Use the raw readdir(3) FFI to avoid SBCL's sb-posix:readdir trying to
    ;; naturalize d_name from a null dirent pointer at end-of-directory,
    ;; which causes a CORRUPTION WARNING (memory fault at 0x13 = d_name offset).
    (let ((entry (sb-alien:alien-funcall
                  (sb-alien:extern-alien "readdir"
                                         (function (* t) (* t)))
                  dirp)))
      (when (sb-alien:null-alien entry)
        (return-from |sun/nio/fs/UnixNativeDispatcher.readdir(J)| nil))
      ;; d_name is at offset 19 in struct dirent on Linux x86-64
      (let* ((name (%read-c-string-from-sap (+ (sb-sys:sap-int (sb-alien:alien-sap entry)) 19)))
             (bytes (flexi-streams:string-to-octets name :external-format :utf-8)))
        (make-java-array
         :component-class (%get-ldk-class-by-fq-name "byte")
         :initial-contents bytes)))))

(defun |sun/nio/fs/UnixNativeDispatcher.write(IJI)| (fd address len)
  "write(2) — write to file descriptor from native buffer."
  (let ((sap (sb-sys:int-sap address)))
    (handler-case
        (sb-posix:write fd sap len)
      (sb-posix:syscall-error (e)
        (let ((ux (%make-java-instance "sun/nio/fs/UnixException")))
          (when (slot-exists-p ux '|errno|)
            (setf (slot-value ux '|errno|) (sb-posix:syscall-errno e)))
          (error (%lisp-condition ux)))))))

(defun |sun/nio/fs/UnixNativeDispatcher.strerror(I)| (errno)
  "strerror(3) — return error description as byte[]."
  (let* ((msg (sb-int:strerror errno))
         (bytes (flexi-streams:string-to-octets msg :external-format :utf-8)))
    (make-java-array
     :component-class (%get-ldk-class-by-fq-name "byte")
     :initial-contents bytes)))

(defun |sun/nio/fs/UnixNativeDispatcher.getpwuid(I)| (uid)
  "getpwuid(3) — return user name as byte[]."
  (handler-case
      (let* ((pw (sb-posix:getpwuid uid))
             (name (sb-posix:passwd-name pw))
             (bytes (flexi-streams:string-to-octets name :external-format :utf-8)))
        (make-java-array
         :component-class (%get-ldk-class-by-fq-name "byte")
         :initial-contents bytes))
    (sb-posix:syscall-error (e)
      (let ((ux (%make-java-instance "sun/nio/fs/UnixException")))
        (when (slot-exists-p ux '|errno|)
          (setf (slot-value ux '|errno|) (sb-posix:syscall-errno e)))
        (error (%lisp-condition ux))))))

(defun |sun/nio/fs/UnixNativeDispatcher.getgrgid(I)| (gid)
  "getgrgid(3) — return group name as byte[]."
  (handler-case
      (let* ((gr (sb-posix:getgrgid gid))
             (name (sb-posix:group-name gr))
             (bytes (flexi-streams:string-to-octets name :external-format :utf-8)))
        (make-java-array
         :component-class (%get-ldk-class-by-fq-name "byte")
         :initial-contents bytes))
    (sb-posix:syscall-error (e)
      (let ((ux (%make-java-instance "sun/nio/fs/UnixException")))
        (when (slot-exists-p ux '|errno|)
          (setf (slot-value ux '|errno|) (sb-posix:syscall-errno e)))
        (error (%lisp-condition ux))))))

(defun |sun/nio/fs/UnixNativeDispatcher.fchmod(II)| (fd mode)
  "fchmod(2) — change file mode."
  (handler-case
      (sb-posix:fchmod fd mode)
    (sb-posix:syscall-error (e)
      (let ((ux (%make-java-instance "sun/nio/fs/UnixException")))
        (when (slot-exists-p ux '|errno|)
          (setf (slot-value ux '|errno|) (sb-posix:syscall-errno e)))
        (error (%lisp-condition ux))))))

(defun |sun/nio/fs/UnixNativeDispatcher.fchown(III)| (fd uid gid)
  "fchown(2) — change file owner."
  (handler-case
      (sb-posix:fchown fd uid gid)
    (sb-posix:syscall-error (e)
      (let ((ux (%make-java-instance "sun/nio/fs/UnixException")))
        (when (slot-exists-p ux '|errno|)
          (setf (slot-value ux '|errno|) (sb-posix:syscall-errno e)))
        (error (%lisp-condition ux))))))

(defun |sun/nio/fs/UnixNativeDispatcher.unlink0(J)| (address)
  "unlink(2) — delete a file."
  (let ((path (%read-c-string-from-sap address)))
    (handler-case
        (sb-posix:unlink path)
      (sb-posix:syscall-error (e)
        (let ((ux (%make-java-instance "sun/nio/fs/UnixException")))
          (when (slot-exists-p ux '|errno|)
            (setf (slot-value ux '|errno|) (sb-posix:syscall-errno e)))
          (error (%lisp-condition ux)))))))

(defmethod |getUTF8At0(Ljava/lang/Object;I)| ((this |sun/reflect/ConstantPool|) cp index)
  (let* ((cp (constant-pool (ldk-class cp)))
         (s (format nil "~A" (emit (aref cp index) cp))))
    (jstring s)))

(defmethod |getIntAt0(Ljava/lang/Object;I)| ((this |sun/reflect/ConstantPool|) cp index)
  (let* ((cp (constant-pool (ldk-class cp)))
         (i (slot-value (aref cp index) 'value)))
    i))

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
      (t (error "unimplemented fd ~A in FileInputStream.read0" fd)))))

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

(defun |java/lang/invoke/MethodHandle.linkToStatic(Ljava/lang/Object;Ljava/lang/Object;Ljava/lang/Object;Ljava/lang/Object;Ljava/lang/invoke/MemberName;)| (arg1 arg2 arg3 arg4 member-name)
  "MethodHandle intrinsic: invoke a static method via MemberName (4-arg variant)."
  (%invoke-from-member-name member-name arg1 arg2 arg3 arg4))

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

;;; -----------------------------------------------------------------------
;;; DirectMethodHandle$Holder trampolines
;;;
;;; In JDK 17, the JVM dynamically generates bytecoded methods in
;;; DirectMethodHandle$Holder (invokeStatic, invokeStaticInit, etc.)
;;; at startup.  These are the compiled forms of LambdaForms used by
;;; the method handle dispatch machinery.  Since OpenLDK doesn't run
;;; HotSpot's GenerateJLIClassesHelper, we define them here.
;;;
;;; Calling convention: arg0 = DirectMethodHandle, arg1..N = method args.
;;; We extract the MemberName from the DMH and dispatch via
;;; %invoke-from-member-name.
;;; -----------------------------------------------------------------------

(defun %holder-invoke-method (mh &rest args)
  "Generic trampoline for DirectMethodHandle$Holder methods.
   Extracts the target MemberName from the DirectMethodHandle and dispatches."
  (let ((member (slot-value mh '|member|)))
    (apply #'%invoke-from-member-name member args)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun %make-holder-descriptor (n)
    "Build a method descriptor with N java.lang.Object parameters."
    (with-output-to-string (s)
      (write-char #\( s)
      (dotimes (i n)
        (write-string "Ljava/lang/Object;" s))
      (write-char #\) s))))

;; Generate holder trampolines for arities 1-10, covering all common
;; LambdaForm entry points.
(loop for method-name in '("invokeStatic" "invokeStaticInit"
                            "invokeSpecial" "invokeVirtual"
                            "invokeInterface" "newInvokeSpecial")
      do (loop for n from 1 to 10
               for descriptor = (%make-holder-descriptor n)
               for full-name = (format nil
                                       "java/lang/invoke/DirectMethodHandle$Holder.~A~A"
                                       method-name descriptor)
               do (setf (fdefinition (intern full-name :openldk))
                        #'%holder-invoke-method)))

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

(defun %get-return-type-char (member-name)
  "Extract the return type descriptor character from a MemberName's type.
Returns NIL if the return type is an object/array (no boxing needed),
or one of #\\Z #\\B #\\S #\\I #\\J #\\F #\\D #\\C for primitives."
  (let ((type (when (and (slot-exists-p member-name '|type$|)
                         (slot-boundp member-name '|type$|))
                (slot-value member-name '|type$|))))
    (cond
      ((null type) nil)
      ;; String descriptor like "(Ljava/lang/Object;)Z" — extract char after ')'
      ((typep type '|java/lang/String|)
       (let* ((desc (lstring type))
              (ret-start (position #\) desc)))
         (when ret-start
           (let ((ret-char (char desc (1+ ret-start))))
             (when (find ret-char "ZBSIJFDC")
               ret-char)))))
      ;; MethodType object — extract return type from rtype slot
      ((and (slot-exists-p type '|rtype|)
            (slot-boundp type '|rtype|))
       (let* ((rtype (slot-value type '|rtype|))
              (desc (when rtype (%class->descriptor-string rtype))))
         (when (and desc (= (length desc) 1) (find (char desc 0) "ZBSIJFDC"))
           (char desc 0))))
      (t nil))))

(defun %box-primitive-return (value ret-char)
  "Box a primitive VALUE according to the return type descriptor character."
  (case ret-char
    (#\Z (let ((b (%make-java-instance "java/lang/Boolean")))
           (setf (slot-value b '|value|) (if (and (integerp value) (zerop value)) 0 1))
           b))
    (#\B (let ((b (%make-java-instance "java/lang/Byte")))
           (setf (slot-value b '|value|) value) b))
    (#\S (let ((b (%make-java-instance "java/lang/Short")))
           (setf (slot-value b '|value|) value) b))
    (#\I (let ((b (%make-java-instance "java/lang/Integer")))
           (setf (slot-value b '|value|) value) b))
    (#\J (let ((b (%make-java-instance "java/lang/Long")))
           (setf (slot-value b '|value|) value) b))
    (#\F (let ((b (%make-java-instance "java/lang/Float")))
           (setf (slot-value b '|value|) value) b))
    (#\D (let ((b (%make-java-instance "java/lang/Double")))
           (setf (slot-value b '|value|) value) b))
    (#\C (let ((b (%make-java-instance "java/lang/Character")))
           (setf (slot-value b '|value|) value) b))
    (t value)))

(defun %lambda-invoke (mh captures args &key box-return)
  "Common invoke logic for lambda implementations.
When BOX-RETURN is true, boxes primitive return values to their wrapper types
so that SAM methods returning Object get proper boxed values."
  (let ((member (when (and mh (slot-exists-p mh '|member|))
                  (slot-value mh '|member|))))
    (if member
        (let ((result (apply #'%invoke-from-member-name member (append captures args))))
          (if box-return
              (let ((ret-char (%get-return-type-char member)))
                (if ret-char
                    (%box-primitive-return result ret-char)
                    result))
              result))
        (let ((args-array (make-java-array :component-class (%get-java-class-by-bin-name "java/lang/Object")
                                           :initial-contents (coerce (append captures args) 'vector))))
          (|invokeWithArguments([Ljava/lang/Object;)| mh args-array)))))

;; Supplier implementation (for get() with no args)
(defclass/std |openldk/LambdaSupplier| (|java/lang/Object| |java/util/function/Supplier|)
  ((target)
   (captures)))

(defmethod |get()| ((this |openldk/LambdaSupplier|))
  (%lambda-invoke (slot-value this 'target) (slot-value this 'captures) nil :box-return t))

;; Predicate implementation (for test(Object))
(defclass/std |openldk/LambdaPredicate| (|java/lang/Object| |java/util/function/Predicate|)
  ((target)
   (captures)))

(defmethod |test(Ljava/lang/Object;)| ((this |openldk/LambdaPredicate|) obj)
  (%lambda-invoke (slot-value this 'target) (slot-value this 'captures) (list obj)))

;; IntPredicate/LongPredicate/DoublePredicate bridge methods
(defmethod |test(I)| ((this |openldk/LambdaPredicate|) int-val)
  (%lambda-invoke (slot-value this 'target) (slot-value this 'captures) (list int-val)))
(defmethod |test(J)| ((this |openldk/LambdaPredicate|) long-val)
  (%lambda-invoke (slot-value this 'target) (slot-value this 'captures) (list long-val)))
(defmethod |test(D)| ((this |openldk/LambdaPredicate|) double-val)
  (%lambda-invoke (slot-value this 'target) (slot-value this 'captures) (list double-val)))

;; BiPredicate implementation (for test(Object, Object))
(defclass/std |openldk/LambdaBiPredicate| (|java/lang/Object| |java/util/function/BiPredicate|)
  ((target)
   (captures)))

(defmethod |test(Ljava/lang/Object;Ljava/lang/Object;)| ((this |openldk/LambdaBiPredicate|) a b)
  (%lambda-invoke (slot-value this 'target) (slot-value this 'captures) (list a b)))

;; Function implementation (for apply(Object))
(defclass/std |openldk/LambdaFunction| (|java/lang/Object| |java/util/function/Function|)
  ((target)
   (captures)))

(defmethod |apply(Ljava/lang/Object;)| ((this |openldk/LambdaFunction|) obj)
  (%lambda-invoke (slot-value this 'target) (slot-value this 'captures) (list obj) :box-return t))

;; IntFunction/LongFunction/DoubleFunction bridge methods (primitive-specialized apply)
(defmethod |apply(I)| ((this |openldk/LambdaFunction|) int-val)
  (%lambda-invoke (slot-value this 'target) (slot-value this 'captures) (list int-val) :box-return t))
(defmethod |apply(J)| ((this |openldk/LambdaFunction|) long-val)
  (%lambda-invoke (slot-value this 'target) (slot-value this 'captures) (list long-val) :box-return t))
(defmethod |apply(D)| ((this |openldk/LambdaFunction|) double-val)
  (%lambda-invoke (slot-value this 'target) (slot-value this 'captures) (list double-val) :box-return t))

;; Consumer implementation (for accept(Object))
(defclass/std |openldk/LambdaConsumer| (|java/lang/Object| |java/util/function/Consumer|)
  ((target)
   (captures)))

(defmethod |accept(Ljava/lang/Object;)| ((this |openldk/LambdaConsumer|) obj)
  (%lambda-invoke (slot-value this 'target) (slot-value this 'captures) (list obj))
  nil)

;; IntConsumer/LongConsumer/DoubleConsumer bridge methods
(defmethod |accept(I)| ((this |openldk/LambdaConsumer|) int-val)
  (%lambda-invoke (slot-value this 'target) (slot-value this 'captures) (list int-val))
  nil)
(defmethod |accept(J)| ((this |openldk/LambdaConsumer|) long-val)
  (%lambda-invoke (slot-value this 'target) (slot-value this 'captures) (list long-val))
  nil)
(defmethod |accept(D)| ((this |openldk/LambdaConsumer|) double-val)
  (%lambda-invoke (slot-value this 'target) (slot-value this 'captures) (list double-val))
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
  (%lambda-invoke (slot-value this 'target) (slot-value this 'captures) (list a b) :box-return t))

(defvar *dynamic-lambda-classes* (make-hash-table :test #'equal)
  "Cache of dynamically created lambda classes keyed by SAM method lispized name.")

(defun %ensure-dynamic-lambda-class (method-str sam-method-type &optional interface-type-name)
  "Get or create a dynamic lambda class for the given SAM method name and type.
INTERFACE-TYPE-NAME is the binary name of the functional interface (e.g.
\"com/sun/tools/javac/util/JavacMessages$ResourceBundleHelper\") so that
instances pass CHECKCAST for that interface.
Returns the class symbol."
  (let* ((param-count (if (and sam-method-type
                               (slot-exists-p sam-method-type '|ptypes|)
                               (slot-boundp sam-method-type '|ptypes|)
                               (slot-value sam-method-type '|ptypes|))
                          (java-array-length (slot-value sam-method-type '|ptypes|))
                          0))
         ;; Build the descriptor from SAM method type parameter types
         (descriptor (if sam-method-type
                        (let ((desc (lstring (|toMethodDescriptorString()| sam-method-type))))
                          desc)
                        "()Ljava/lang/Object;"))
         (lispized-name (lispize-method-name (format nil "~A~A" method-str descriptor)))
         ;; Use interface name as cache key when available for proper CHECKCAST
         (class-name-str (if interface-type-name
                             (format nil "openldk/DynamicLambda_~A" (substitute #\_ #\/ interface-type-name))
                             (format nil "openldk/DynamicLambda_~A_~A" method-str param-count)))
         (cached (gethash class-name-str *dynamic-lambda-classes*)))
    (if cached
        ;; Ensure the method is defined for this specific lispized name
        (let ((method-sym (intern lispized-name :openldk)))
          (unless (and (fboundp method-sym)
                       (typep (fdefinition method-sym) 'generic-function))
            (%define-lambda-method method-sym cached param-count))
          cached)
        ;; Create new dynamic lambda class, extending the interface if known
        (let* ((class-sym (intern class-name-str :openldk))
               (method-sym (intern lispized-name :openldk))
               (interface-sym (when interface-type-name
                                (handler-case
                                    (progn
                                      (classload interface-type-name)
                                      (let ((sym (find-symbol interface-type-name :openldk)))
                                        (when (and sym (find-class sym nil))
                                          sym)))
                                  (condition () nil))))
               (superclasses (if interface-sym
                                 (list '|java/lang/Object| interface-sym)
                                 (list '|java/lang/Object|))))
          ;; Define the class with target and captures slots
          (eval `(defclass/std ,class-sym ,superclasses
                   ((target) (captures))))
          ;; Define the dispatch method
          (%define-lambda-method method-sym class-sym param-count)
          (setf (gethash class-name-str *dynamic-lambda-classes*) class-sym)
          class-sym))))

(defun %define-lambda-method (method-sym class-sym param-count)
  "Define a lambda dispatch method METHOD-SYM on CLASS-SYM with PARAM-COUNT parameters."
  (let ((params (loop for i below param-count
                      collect (intern (format nil "P~A" i) :openldk))))
    ;; Only create/change GF if not already a GF (avoid change-class violation)
    (unless (and (fboundp method-sym)
                 (typep (fdefinition method-sym) 'generic-function))
      (ensure-generic-function method-sym
                               :generic-function-class 'java-generic-function
                               :lambda-list (cons '|this| params)))
    (eval `(defmethod ,method-sym ((|this| ,class-sym) ,@params)
             (%lambda-invoke (slot-value |this| 'target)
                             (slot-value |this| 'captures)
                             (list ,@params))))))

(defun %lambda-metafactory (impl-handle captures &optional (method-name "get") sam-method-type interface-type-name)
  "Construct a functional interface implementation for Java lambdas.
METHOD-NAME is the interface method name (get, test, apply, accept, etc.).
CAPTURES is a list of pre-bound values for captured variables.
SAM-METHOD-TYPE is the MethodType of the functional interface method,
used to determine the correct arity (e.g. Consumer vs BiConsumer).
INTERFACE-TYPE-NAME is the binary name of the target functional interface."
  (let* ((method-str (if (stringp method-name) method-name (lstring method-name)))
         (sam-param-count (if (and sam-method-type
                                   (slot-exists-p sam-method-type '|ptypes|)
                                   (slot-boundp sam-method-type '|ptypes|)
                                   (slot-value sam-method-type '|ptypes|))
                              (java-array-length (slot-value sam-method-type '|ptypes|))
                              nil))
         (lambda-class (cond
                         ((string= method-str "get") '|openldk/LambdaSupplier|)
                         ((and (string= method-str "test") sam-param-count (<= sam-param-count 1))
                          '|openldk/LambdaPredicate|)
                         ((string= method-str "test")
                          '|openldk/LambdaBiPredicate|)
                         ((and (string= method-str "apply") sam-param-count (<= sam-param-count 1))
                          '|openldk/LambdaFunction|)
                         ((string= method-str "apply")
                          '|openldk/LambdaBinaryOperator|)
                         ((and (string= method-str "accept") sam-param-count (<= sam-param-count 1))
                          '|openldk/LambdaConsumer|)
                         ((string= method-str "accept") '|openldk/LambdaBiConsumer|)
                         ;; Dynamic: create class + method for unknown SAM names
                         (t (%ensure-dynamic-lambda-class method-str sam-method-type interface-type-name))))
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
