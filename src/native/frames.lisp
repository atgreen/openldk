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
                    (or (and (find-class '|java/lang/reflect/Method| nil)
                             (typep a '|java/lang/reflect/Method|))
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
             (search "Reflection.getCallerClass" n)
             ;; The evaluator running an interpreted method's body, between
             ;; the method's own frame and its callees'.
             (eq (symbol-package head) (find-package "SB-EVAL"))
             (eq head 'sb-int:simple-eval-in-lexenv))))
      (:labels-clinit t)
      (t nil))))

(defun %filtered-backtrace ()
  (remove-if #'%invoke-frame-p
             (%remove-adjacent-repeats (sb-debug:list-backtrace))))

(defun %frame-java-class (frame class-name)
  "The java.lang.Class named CLASS-NAME that FRAME runs a method of, or NIL.
A class a user-defined loader defined is in that loader's tables, not the
global ones; the loader is the one whose Lisp package names the frame's
function, or, for a CLOS method, its receiver's class."
  (let* ((head (%frame-head frame))
         (symbol (case (%frame-head-kind head)
                   (:symbol head)
                   (:method (let ((receiver (cadr frame)))
                              (and (typep receiver 'standard-object)
                                   (class-name (class-of receiver)))))
                   (t nil)))
         (loader (and symbol (symbolp symbol) (%ldk-loader-for-symbol symbol))))
    (or (and loader (%get-java-class-by-bin-name class-name t loader))
        (and (gethash class-name *java-classes-by-bin-name*)
             (%get-java-class-by-bin-name class-name)))))

(defun %frame-caller-java-class (frame)
  "The java.lang.Class whose method FRAME runs, or NIL for frames that
are not a Java method's."
  (unless (%internal-frame-p frame)
    (let ((class-name (%caller-class-name-from-stack-frame frame)))
      (when (and (stringp class-name)
                 (not (find #\. class-name)))
        (%frame-java-class frame class-name)))))

(defun %java-caller-class (function-name)
  "The class of the Java method that called FUNCTION-NAME, the Lisp
function implementing a caller-sensitive method, which must be running."
  (let* ((backtrace (%filtered-backtrace))
         (callee (position-if (lambda (frame) (eq (%frame-head frame) function-name)) backtrace)))
    (and callee (some #'%frame-caller-java-class (nthcdr (1+ callee) backtrace)))))

(defun |sun/reflect/Reflection.getCallerClass(I)| (index)
  (let ((backtrace (%filtered-backtrace)))
    (%get-java-class-by-bin-name
     (%caller-class-name-from-stack-frame (nth index backtrace)))))

(defun |sun/reflect/Reflection.getCallerClass()| ()
  "The class of the method that called the caller-sensitive method that
called getCallerClass: past the getCallerClass frames, skip one Java
frame (the caller-sensitive method, AtomicReferenceFieldUpdater.newUpdater
say) and return the next one's class."
  (flet ((java-class-of (frame) (%frame-caller-java-class frame)))
    (let* ((backtrace (%filtered-backtrace))
           (last-get-caller (position-if (lambda (frame)
                                           (and (eq (%frame-head-kind (%frame-head frame)) :symbol)
                                                (search "Reflection.getCallerClass"
                                                        (symbol-name (%frame-head frame)))))
                                         backtrace :from-end t))
           (frames (nthcdr (if last-get-caller (1+ last-get-caller) 0) backtrace))
           (caller-sensitive (position-if #'java-class-of frames)))
      (or (and caller-sensitive
               (some #'java-class-of (nthcdr (1+ caller-sensitive) frames)))
          (%get-java-class-by-bin-name "java/lang/System")))))

(defun |jdk/internal/reflect/Reflection.areNestMates(Ljava/lang/Class;Ljava/lang/Class;)Z| (a b)
  "True when classes A and B belong to the same nest."
  (if (eq (%nest-host a) (%nest-host b)) 1 0))

(defun |jdk/internal/reflect/Reflection.getCallerClass()Ljava/lang/Class;| ()
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
  "Resolve an array without asking ClassLoader.loadClass for an array name.
The component's defining loader owns the array, including multidimensional
arrays. Resolving an array never initializes its component class."
  (let* ((cname (substitute #\/ #\. cname))
         (component (if (char= (char cname 1) #\[)
                        (java-class (%get-array-ldk-class-from-name (subseq cname 1)))
                        (%bin-type-name-to-class (subseq cname 1))))
         (java-loader (and component (slot-value component '|classLoader|)))
         (loader (get-ldk-loader-for-java-loader java-loader))
         (ldk-table (if loader (slot-value loader 'ldk-classes-by-bin-name)
                        *ldk-classes-by-bin-name*)))
    (bordeaux-threads:with-recursive-lock-held ((%get-class-lock cname))
      (or (gethash cname ldk-table)
          (let* ((fq-name (substitute #\. #\/ cname))
                 (class (make-instance '<class> :name cname :super "java/lang/Object"
                                       :methods #() :fields #() :initialized-p t :verified-p t))
                 (java-class (%make-java-instance "java/lang/Class")))
            (setf (ldk-loader class) loader
                  (interfaces class) (vector "java/lang/Cloneable" "java/io/Serializable")
                  (slot-value java-class '|name|) (ijstring fq-name)
                  (slot-value java-class '|classLoader|) java-loader
                  (slot-value java-class '|module|) (slot-value component '|module|)
                  (slot-value java-class '|componentType|) component
                  (slot-value java-class '|modifiers|)
                  (logior (logand (or (slot-value component '|modifiers|) 1) #x7) #x10 #x400)
                  (java-class class) java-class
                  (gethash cname ldk-table) class)
            (when loader
              (setf (gethash fq-name (slot-value loader 'ldk-classes-by-fq-name)) class
                    (gethash cname (slot-value loader 'java-classes-by-bin-name)) java-class
                    (gethash fq-name (slot-value loader 'java-classes-by-fq-name)) java-class))
            ;; Keep the compatibility tables for Lisp callers with no loader.
            (setf (gethash cname *ldk-classes-by-bin-name*) class
                  (gethash fq-name *ldk-classes-by-fq-name*) class
                  (gethash cname *java-classes-by-bin-name*) java-class
                  (gethash fq-name *java-classes-by-fq-name*) java-class)
            class)))))

(defun %get-array-ldk-class (element-type)
  (let* ((cname (format nil "[~A" (%type-to-descriptor element-type))))
    (%get-array-ldk-class-from-name cname)))

(defun %lisp-only-java-class (class-symbol)
  "A java.lang.Class for CLASS-SYMBOL, a class OpenLDK defines in Lisp with
no class file (openldk/LambdaSupplier, a dynamic lambda class): a public
final synthetic subclass of Object implementing the Java interfaces among
its superclasses, made and registered on first use; or NIL for any other."
  (let ((cname (and (symbolp class-symbol) (symbol-name class-symbol))))
    (when (and cname (eq (symbol-package class-symbol) (find-package :openldk))
               (starts-with? "openldk/" cname)
               (find-class class-symbol nil))
      (or (gethash cname *java-classes-by-bin-name*)
          (let* ((fq-name (substitute #\. #\/ cname))
                 (jclass (%make-java-instance "java/lang/Class"))
                 (lclass (make-instance '<class>))
                 (interfaces
                   (loop for super in (closer-mop:class-direct-superclasses (find-class class-symbol))
                         for super-name = (symbol-name (class-name super))
                         for super-lclass = (gethash super-name *ldk-classes-by-bin-name*)
                         when (and super-lclass (logtest #x200 (or (access-flags super-lclass) 0)))
                           collect super-name)))
            (setf (slot-value jclass '|name|) (ijstring fq-name))
            (when (slot-exists-p jclass '|modifiers|)
              (setf (slot-value jclass '|modifiers|) #x1011))
            (when (and *unnamed-module* (slot-exists-p jclass '|module|))
              (setf (slot-value jclass '|module|) *unnamed-module*))
            (setf (name lclass) cname
                  (super lclass) "java/lang/Object"
                  (interfaces lclass) (coerce interfaces 'vector)
                  (access-flags lclass) #x1011
                  (fields lclass) #()
                  (methods lclass) #()
                  (constant-pool lclass) #()
                  (attributes lclass) (make-hash-table :test #'equal)
                  (initialized-p lclass) t
                  (java-class lclass) jclass)
            (setf (gethash fq-name *ldk-classes-by-fq-name*) lclass
                  (gethash cname *ldk-classes-by-bin-name*) lclass
                  (gethash fq-name *java-classes-by-fq-name*) jclass
                  (gethash cname *java-classes-by-bin-name*) jclass)
            jclass)))))

(defmethod |getClass()Ljava/lang/Class;| (object)
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
                       (|java/lang/Class.forName0(Ljava/lang/String;ZLjava/lang/ClassLoader;Ljava/lang/Class;)Ljava/lang/Class;|
                        (jstring array-name) nil (slot-value comp-class '|classLoader|) nil)))
                    (t
                     ;; The class's symbol is in its defining loader's package;
                     ;; look the name up in that loader, as two loaders can
                     ;; each have a class of this name.
                     (let* ((class-symbol (type-of object))
                            (jc (%get-java-class-by-bin-name (format nil "~A" class-symbol) t
                                                             (and (symbolp class-symbol)
                                                                  (%ldk-loader-for-symbol class-symbol)))))
                       (or jc
                           (%lisp-only-java-class class-symbol)
                           (|java/lang/Class.forName0(Ljava/lang/String;ZLjava/lang/ClassLoader;Ljava/lang/Class;)Ljava/lang/Class;|
                            (jstring (format nil "~A" (type-of object))) nil nil nil)))))))
           c))
    (when *debug-trace*
      (incf *call-nesting-level* -1))))

;; Runtime modules use OpenLDK's classpath backend. Other modules resolve
;; through their Java loader, preserving custom-loader resources. In either
;; case the result must belong to the requested module and is not initialized.
(setf (gethash "java/lang/Class.forName(Ljava/lang/Module;Ljava/lang/String;)Ljava/lang/Class;"
               *native-overrides*)
      (lambda (module name)
        (handler-case
            (let* ((module-name (slot-value module '|name|))
                   (runtime-module (and module-name
                                        (eq module (gethash (lstring module-name) *boot-modules*))))
                   (class (if runtime-module
                              (let ((metadata (%classload-system (substitute #\/ #\. (lstring name)))))
                                (and metadata (java-class metadata)))
                              (|java/lang/Class.forName0(Ljava/lang/String;ZLjava/lang/ClassLoader;Ljava/lang/Class;)Ljava/lang/Class;|
                               name nil (slot-value module '|loader|) nil))))
              (when (and class (eq (slot-value class '|module|) module)) class))
          (|condition-java/lang/ClassNotFoundException| () nil))))

(defmethod |java/lang/Class.forName0(Ljava/lang/String;ZLjava/lang/ClassLoader;Ljava/lang/Class;)Ljava/lang/Class;| (name initialize loader caller)
  (unwind-protect
       (progn
         (when *debug-trace*
           (format t "~&~V@A trace: entering java/lang/Class.forName0(Ljava/lang/String;ZLjava/lang/ClassLoader;Ljava/lang/Class;) ~A~%"
                   (incf *call-nesting-level* 1) "*" (list name initialize loader caller)))
         (let* ((lname (substitute #\/ #\. (lstring name)))
                (user-loader (and loader (not (eq loader *boot-class-loader*))))
                (result
                  ;; Ask a user class loader first (if provided and not the boot
                  ;; loader) through loadClass, as the JVM does: it checks the
                  ;; loader's own classes, delegates to its parent, and only then
                  ;; calls findClass.  Then fall back to our classpath search.
                  ;; OpenLDK's own callers (descriptor resolution) pass
                  ;; binary names, but Java loaders take dotted ones.
                  (or (and (eq (char lname 0) #\[)
                           (let ((*descriptor-loader* loader))
                             (java-class (%get-array-ldk-class-from-name lname))))
                      (and user-loader
                           (handler-case
                               (|loadClass(Ljava/lang/String;)Ljava/lang/Class;|
                                loader (if (find #\/ (lstring name))
                                           (jstring (substitute #\. #\/ (lstring name)))
                                           name))
                             (|condition-java/lang/ClassNotFoundException| () nil)))
                      (and (not user-loader)
                           (or (and (%get-ldk-class-by-bin-name lname t)
                                    (java-class (%get-ldk-class-by-bin-name lname)))
                               (let ((klass (classload lname)))
                                 (when klass (java-class klass))))))))
           (when result
             (let ((class (get-ldk-class-for-java-class result)))
               ;; Lisp descriptor lookups also pass NIL and may use the
               ;; compatibility tables. Such a result was not initiated by
               ;; the bootstrap loader: preserve its actual defining loader.
               (%record-initiating-class class (if loader (get-ldk-loader-for-java-loader loader)
                                                  (ldk-loader class)))))
           ;; Per JVM spec: when initialize=true, ensure <clinit> has run.
           ;; Previously only the classload branch called %clinit; the findClass
           ;; and already-loaded branches skipped it, breaking autoloads that
           ;; rely on Class.forName triggering static initialization.
           (when (and result initialize (not (eql initialize 0)))
             ;; Initialize the class the loader returned, not whichever class
             ;; of that name the global table holds.
             (let ((ldk-class (or (and (typep result '|java/lang/Class|)
                                       (get-ldk-class-for-java-class result))
                                  (%get-ldk-class-by-bin-name lname t))))
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

(defun |java/lang/System.currentTimeMillis()J| ()
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

(defun |java/lang/System.nanoTime()J| ()
  ;; Do some more math if this is not true.
  (assert (eq org.shirakumo.precise-time:precise-time-units-per-second
              1000000000))
  (multiple-value-bind (universal-time nanoseconds)
      (org.shirakumo.precise-time:get-precise-time)
    (+ (* (local-time:timestamp-to-unix
           (local-time:universal-to-timestamp universal-time))
          1000000000)
       nanoseconds)))

(defun |jdk/internal/misc/VM.getNanoTimeAdjustment(J)J| (offset-in-seconds)
  "Nanoseconds from OFFSET-IN-SECONDS (seconds since the epoch) to now, or -1
when the offset is 2^32 seconds or more away, as HotSpot answers.  This backs
Clock.currentInstant, and so Instant.now()."
  (assert (eq org.shirakumo.precise-time:precise-time-units-per-second
              1000000000))
  (multiple-value-bind (universal-time nanoseconds)
      (org.shirakumo.precise-time:get-precise-time)
    (let ((diff (- (local-time:timestamp-to-unix
                    (local-time:universal-to-timestamp universal-time))
                   offset-in-seconds)))
      (if (< (- (expt 2 32)) diff (expt 2 32))
          (+ (* diff 1000000000) nanoseconds)
          -1))))

(defun %throw-arraycopy-index-error (format-control &rest format-arguments)
  "System.arraycopy bounds violations throw ArrayIndexOutOfBoundsException."
  (let ((exc (%make-java-instance "java/lang/ArrayIndexOutOfBoundsException")))
    (|<init>(Ljava/lang/String;)V| exc
     (jstring (apply #'format nil format-control format-arguments)))
    (error (%lisp-condition exc))))

(defmethod |java/lang/System.arraycopy(Ljava/lang/Object;ILjava/lang/Object;II)V| (src-array src-pos dest-array dest-pos length)
  "Copies LENGTH elements from SRC-ARRAY starting at SRC-POS
   to DEST-ARRAY starting at DEST-POS.
   Handles overlapping regions correctly even within the same array."
  (let ((src-array (java-array-data src-array))
        (dest-array (java-array-data dest-array)))

    (declare (type array src-array dest-array)
             (type fixnum src-pos dest-pos length))

    ;; Validate arguments
    (when (< length 0)
      (%throw-arraycopy-index-error "length ~A" length))

    (when (< src-pos 0)
      (%throw-arraycopy-index-error "source position ~A" src-pos))

    (when (< dest-pos 0)
      (%throw-arraycopy-index-error "destination position ~A" dest-pos))

    (when (> (+ src-pos length) (array-total-size src-array))
      (%throw-arraycopy-index-error "source size=~A, access=~A"
                                    (array-total-size src-array) (+ src-pos length -1)))

    (when (> (+ dest-pos length) (array-total-size dest-array))
      (%throw-arraycopy-index-error "destination size=~A, access=~A"
                                    (array-total-size dest-array) (+ dest-pos length -1)))

    ;; REPLACE copies overlapping regions of one array correctly, and is a
    ;; block move when both arrays have the same specialized element type.
    (replace dest-array src-array
             :start1 dest-pos :end1 (+ dest-pos length)
             :start2 src-pos)
    nil))

(defmethod |java/security/AccessController.doPrivileged(Ljava/security/PrivilegedAction;)Ljava/lang/Object;| (action)
  (|run()Ljava/lang/Object;| action))

(defmethod |java/lang/Class.getPrimitiveClass(Ljava/lang/String;)Ljava/lang/Class;| (class-name)
  (let ((name (lstring class-name)))
    (%get-java-class-by-fq-name name)))

(defmethod |java/lang/Float.floatToRawIntBits(F)I| (float)
  ;; single-float-bits yields the unsigned 32-bit pattern; Java returns a
  ;; signed int, so a negative float (sign bit set) must come back negative --
  ;; otherwise Float.toString reads the sign as positive and drops the '-'.
  (%int-to-signed (float-features:single-float-bits (coerce float 'single-float))))

(defmethod |java/lang/Double.doubleToRawLongBits(D)J| (double)
  ;; Likewise: Java returns a signed 64-bit long.
  (%long-to-signed (float-features:double-float-bits (coerce double 'double-float))))

(defmethod |java/lang/Double.longBitsToDouble(J)D| (long-bits)
  (float-features:bits-double-float (ldb (byte 64 0) long-bits)))

(defmethod |java/lang/Float.intBitsToFloat(I)F| (int-bits)
  (float-features:bits-single-float (ldb (byte 32 0) int-bits)))


;;; StackWalker.  The JDK fills StackWalker's frame buffers from VM frames
;;; through StackStreamFactory natives; OpenLDK instead builds the frames
;;; from the Lisp backtrace, as fillInStackTrace does.

(defclass/std |openldk/StackFrame| (|java/lang/Object| |java/lang/StackWalker$StackFrame|)
  ((class-name)
   (method-name)
   (descriptor)
   (frame)))

(defun %stack-walker-frame-p (frame)
  (equal (%caller-class-name-from-stack-frame frame) "java/lang/StackWalker"))

(defun %frames-after-stack-walker ()
  "The Java frames of the code that called into StackWalker, innermost
first.  The overrides below are tail-called, so StackWalker's own frame is
usually gone already."
  (let* ((backtrace (remove-if-not #'%java-frame-p (%filtered-backtrace)))
         (frames (or (member-if #'%stack-walker-frame-p backtrace) backtrace)))
    (loop while (and frames (%stack-walker-frame-p (car frames)))
          do (pop frames))
    frames))

(defun %make-stack-frame (frame)
  ;; The interface is openldk/StackFrame's superclass, and loading
  ;; StackWalker does not load it.
  (classload "java/lang/StackWalker$StackFrame")
  (let* ((head (%frame-head frame))
         (name (symbol-name (if (eq (%frame-head-kind head) :method) (cadr head) head)))
         (paren (position #\( name)))
    (make-instance '|openldk/StackFrame|
                   :class-name (%caller-class-name-from-stack-frame frame)
                   :method-name (%method-name-from-stack-frame frame)
                   :descriptor (when paren (subseq name paren))
                   :frame frame)))

(defmethod |getClassName()Ljava/lang/String;| ((this |openldk/StackFrame|))
  (jstring (substitute #\. #\/ (class-name this))))

(defmethod |getMethodName()Ljava/lang/String;| ((this |openldk/StackFrame|))
  (jstring (method-name this)))

(defmethod |getDescriptor()Ljava/lang/String;| ((this |openldk/StackFrame|))
  (jstring (or (descriptor this) "()V")))

(defmethod |getDeclaringClass()Ljava/lang/Class;| ((this |openldk/StackFrame|))
  (or (%frame-java-class (frame this) (class-name this))
      (%get-java-class-by-bin-name (class-name this) t)
      (%get-java-class-by-bin-name "java/lang/Object")))

(defmethod |getByteCodeIndex()I| ((this |openldk/StackFrame|))
  -1)

(defmethod |getLineNumber()I| ((this |openldk/StackFrame|))
  -1)

(defmethod |getFileName()Ljava/lang/String;| ((this |openldk/StackFrame|))
  nil)

(defmethod |isNativeMethod()Z| ((this |openldk/StackFrame|))
  0)

(defmethod |toStackTraceElement()Ljava/lang/StackTraceElement;| ((this |openldk/StackFrame|))
  (%fill-stack-trace-element (%make-java-instance "java/lang/StackTraceElement")
                             (frame this)))

(defmethod |toString()Ljava/lang/String;| ((this |openldk/StackFrame|))
  (|toString()Ljava/lang/String;| (|toStackTraceElement()Ljava/lang/StackTraceElement;| this)))

(setf (gethash "java/lang/StackWalker.walk(Ljava/util/function/Function;)Ljava/lang/Object;" *native-overrides*)
      (lambda (this function)
        (declare (ignore this))
        (let ((frames (mapcar #'%make-stack-frame (%frames-after-stack-walker))))
          (|apply(Ljava/lang/Object;)Ljava/lang/Object;|
           function
           (|java/util/Arrays.stream([Ljava/lang/Object;)Ljava/util/stream/Stream;|
            (make-java-array :component-class (%get-java-class-by-bin-name "java/lang/StackWalker$StackFrame")
                             :initial-contents frames))))))

(setf (gethash "java/lang/StackWalker.forEach(Ljava/util/function/Consumer;)V" *native-overrides*)
      (lambda (this consumer)
        (declare (ignore this))
        (dolist (frame (%frames-after-stack-walker))
          (|accept(Ljava/lang/Object;)V| consumer (%make-stack-frame frame)))))

;; The class of the method that called the method calling getCallerClass.
(setf (gethash "java/lang/StackWalker.getCallerClass()Ljava/lang/Class;" *native-overrides*)
      (lambda (this)
        (declare (ignore this))
        (let ((caller (second (%frames-after-stack-walker))))
          (if caller
              (|getDeclaringClass()Ljava/lang/Class;| (%make-stack-frame caller))
              (%throw '|java/lang/IllegalCallerException| "no caller frame")))))
