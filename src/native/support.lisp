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

;;; Native-method support: override registration and post-<clinit> hooks.

(in-package :openldk)

;; *native-overrides* is defined in global-state.lisp so earlier-loaded
;; files (e.g. jrt.lisp) can register overrides too.

;; OpenLDK supplies JDK native entry points as Lisp methods.  Loading the
;; corresponding HotSpot JNI libraries (awt, javajpeg, zip, and friends) into
;; SBCL would neither register nor satisfy those methods, so treat the Java
;; library-load notification as already fulfilled.
(setf (gethash "java/lang/System.loadLibrary(Ljava/lang/String;)V"
               *native-overrides*)
      (lambda (library-name)
        (declare (ignore library-name))
        nil))

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

(defun %java-method-gf-p (gf)
  "True if GF represents a Java method, so a missing applicable method should map
to Java semantics rather than an ordinary Lisp error. Java method GFs either use
the java-generic-function metaclass or are named with a JVM method descriptor
\(the name contains parentheses), e.g. |toString()| or |wait(J)|. Note some Java
method GFs are plain standard-generic-functions (created by a hand-written
defmethod before the JIT would upgrade them), which is why the name check matters."
  (or (typep gf 'java-generic-function)
      (let ((name (ignore-errors (closer-mop:generic-function-name gf))))
        (and (symbolp name)
             (let ((s (symbol-name name)))
               (and (find #\( s) (find #\) s)))))))

;; In Java, a method call on a NULL object results in a NullPointerException.
;; CLOS lets us implement this via no-applicable-method -- but ONLY for Java
;; method GFs. A genuine no-applicable-method on an ordinary Lisp generic
;; function is an OpenLDK (or dependency) bug and must surface as a normal Lisp
;; error with its real backtrace, not be masked as a Java NPE / "internal error".
(defmethod no-applicable-method ((gf generic-function) &rest args)
  (cond
    ((not (%java-method-gf-p gf))
     (error "no applicable method for ~S with arguments ~S" gf args))
    ((null (car args))
     (error (%lisp-condition (%make-throwable '|java/lang/NullPointerException|))))
    (t
     (internal-error "no applicable method for invocation of ~A with arguments ~S" gf args))))

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
         (current-thread (current-thread-identity)))
    ;; SBCL's condition-wait and condition-notify auto-dispatch to fiber-aware
    ;; paths when running in a fiber, so no separate fiber code path is needed.
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

(defun %caller-class-name-from-stack-frame (frame)
  "Java class name for a backtrace FRAME, examining only the frame head
and, for CLOS method frames, the receiver's type -- never printing frame
arguments (that pretty-prints arbitrarily large Java object graphs)."
  (let* ((head (%frame-head frame))
         (kind (%frame-head-kind head)))
    (case kind
      (:method (format nil "~A" (type-of (cadr frame))))
      (:lambda (substitute #\/ #\. (format nil "~A" (type-of (cadr frame)))))
      (:labels-clinit
       (if (typep (cadr frame) '<class>)
           (name (cadr frame))
           "java/lang/System"))
      (:symbol
       (let ((n (symbol-name head)))
         (cond
           ((and (> (length n) 8) (string-equal n "%clinit-" :end1 8))
            (subseq n 8))
           ((and (string-equal n "%clinit")
                 (typep (cadr frame) '<class>))
            (name (cadr frame)))
           ;; Instance-method impl defuns (see %wrap-method-body):
           ;; |%jimpl:java/util/Date.toString()| -> "java/util/Date"
           ((and (> (length n) 7) (string= n "%jimpl:" :end1 7))
            (let ((qualified (subseq n 7)))
              (subseq qualified 0 (position #\. qualified))))
           ((find #\. n)
            (subseq n 0 (position #\. n)))
           ;; FIXME: maybe use an OpenLDK internal class to indicate internal frame
           (t "java/lang/System"))))
      (t "java/lang/System"))))

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

;; JDK 21 passes Throwable.backtrace and depth separately instead of passing
;; the Throwable itself.
(defun |java/lang/StackTraceElement.initStackTraceElements([Ljava/lang/StackTraceElement;Ljava/lang/Object;I)|
    (ste-array backtrace depth)
  "Populate STE-ARRAY from a JDK 21 Throwable backtrace object."
  (let* ((data (java-array-data ste-array))
         (frames (if (listp backtrace) backtrace nil))
         (count (min (length data) depth (length frames))))
    (dotimes (index count)
      (let ((ste (%make-java-instance "java/lang/StackTraceElement"))
            (frame (nth index frames)))
        (|<init>(Ljava/lang/String;Ljava/lang/String;Ljava/lang/String;I)|
         ste
         (jstring (%caller-class-name-from-stack-frame frame))
         (ijstring "unknown") (jstring (format nil "~A" frame)) -1)
        (setf (aref data index) ste)))))

(defun %remove-adjacent-repeats (list)
  "Remove all adjacent repeated objects from LIST."
  (let ((result nil)
        (last nil))
    (loop for item in list
          unless (equal item last)
            do (push item result)
          do (setf last item))
    (nreverse result)))

