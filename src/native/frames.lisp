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

;;; Structural backtrace-frame predicates and caller-class discovery.

(in-package :openldk)

;;; Backtrace frames are examined constantly (every Throwable construction,
;;; every Reflection.getCallerClass) -- javac alone makes thousands of such
;;; calls per compilation.  All predicates below therefore work structurally
;;; on the frame HEAD (a symbol or a small name list) and never print frame
;;; ARGUMENTS: (format nil "~A" frame) pretty-prints arbitrarily large Java
;;; object graphs and once dominated the entire javac runtime profile.

(defun %frame-head (frame)
  (if (consp frame) (car frame) frame))

(defun %frame-head-kind (head)
  "Classify a backtrace frame head: :METHOD, :LAMBDA, :LABELS-CLINIT,
:SYMBOL, or NIL."
  (cond
    ((symbolp head) :symbol)
    ((and (consp head) (symbolp (car head)))
     (let ((n (symbol-name (car head))))
       (cond ((string= n "METHOD") :method)
             ((string= n "FAST-METHOD") :method)
             ((string= n "LAMBDA") :lambda)
             ((string= n "LABELS")
              (let ((l (cadr head)))
                (if (and (symbolp l) (string-equal (symbol-name l) "CLINIT"))
                    :labels-clinit
                    :lambda)))
             (t nil))))
    (t nil)))

(defun %invoke-frame-p (frame)
  "True for frames belonging to reflective-invoke plumbing that Java stack
traces should not show."
  (let* ((head (%frame-head frame))
         (kind (%frame-head-kind head)))
    (case kind
      (:symbol
       (let ((n (symbol-name head)))
         (or (search "invoke0" n)
             (search "%RESOLVE-INVOKEDYNAMIC" n)
             (string= n "INVOKE-SPECIAL"))))
      (:method
       (let ((m (cadr head)))
         (and (symbolp m)
              (let ((n (symbol-name m)))
                (and (>= (length n) 6) (string= n "invoke" :end1 6))))))
      (t
       ;; Frames whose arguments carry reflective machinery objects.
       (and (consp frame)
            (some (lambda (a)
                    (or (typep a '|java/lang/reflect/Method|)
                        (and (find-class '|sun/reflect/NativeMethodAccessorImpl| nil)
                             (typep a '|sun/reflect/NativeMethodAccessorImpl|))))
                  (rest frame)))))))

(defun %internal-frame-p (frame)
  "True for internal OpenLDK frames that should be skipped when looking
for the Java caller."
  (let* ((head (%frame-head frame))
         (kind (%frame-head-kind head)))
    (case kind
      (:symbol
       (let ((n (symbol-name head)))
         (or (and (>= (length n) 8) (string-equal n "%clinit-" :end1 8))
             (search "Reflection.getCallerClass" n))))
      (:labels-clinit t)
      (t nil))))

(defun %filtered-backtrace ()
  (remove-if #'%invoke-frame-p
             (%remove-adjacent-repeats (sb-debug:list-backtrace))))

(defun |sun/reflect/Reflection.getCallerClass(I)| (index)
  (let ((backtrace (%filtered-backtrace)))
    (%get-java-class-by-bin-name
     (%caller-class-name-from-stack-frame (nth index backtrace)))))

(defun |sun/reflect/Reflection.getCallerClass()| ()
  ;; Skip internal frames to find the actual caller
  ;; The caller is the first non-internal Java method frame after:
  ;; - getCallerClass() itself (frame 0)
  ;; - The method that called getCallerClass (e.g., registerAsParallelCapable) (frame 1)
  (let ((backtrace (nthcdr 2 (%filtered-backtrace))))
    (loop for frame in backtrace
          unless (%internal-frame-p frame)
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
                     (let* ((comp-class (%array-component-class object))
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

;; Class.forName(Module, String) is Java code delegating into the built-in
;; class-loader/module machinery (BootLoader.loadClass etc.), which OpenLDK
;; fakes -- it silently returns null there, breaking ResourceBundle's
;; bundle-class lookup (javac's messages, among others).  Resolve through
;; OpenLDK's own class loading instead.  Per spec: returns null when not
;; found and does NOT initialize the class.
(setf (gethash "java/lang/Class.forName(Ljava/lang/Module;Ljava/lang/String;)Ljava/lang/Class;"
               *native-overrides*)
      (lambda (module name)
        (declare (ignore module))
        (handler-case
            (let ((lclass (classload (substitute #\/ #\. (lstring name)))))
              (when lclass
                (java-class lclass)))
          (condition () nil))))

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
  (float-features:single-float-bits (coerce float 'single-float)))

(defmethod |java/lang/Double.doubleToRawLongBits(D)| (double)
  (float-features:double-float-bits (coerce double 'double-float)))

(defmethod |java/lang/Double.longBitsToDouble(J)| (long-bits)
  (float-features:bits-double-float (ldb (byte 64 0) long-bits)))

(defmethod |java/lang/Float.intBitsToFloat(I)| (int-bits)
  (float-features:bits-single-float (ldb (byte 32 0) int-bits)))

