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

(annot:enable-annot-syntax)

(in-package :openldk)

(defvar *classpath* nil)
(defvar *classes* (make-hash-table :test #'equal))
(defvar *java-classes* (make-hash-table :test #'equal))
(defvar *context* nil)
(defvar *condition-table* (make-hash-table))

(defvar *dump-dir* nil)
(defvar *debug-codegen* nil)
(defvar *debug-trace* nil)
(defvar *debug-stack* nil)
(defvar *debug-unmuffle* nil)

(defun %eval (code)
  (when *debug-codegen*
    (pprint code)
    (format t "~%"))
  (if *debug-unmuffle*
      (eval code)
      (handler-bind
          (#+ansi-cl
           (style-warning (lambda (c)
                            (declare (ignore c))
                            (invoke-restart 'muffle-warning))))
        (eval code))))

(defun lispize-method-name (name)
  (subseq name 0 (1+ (position #\) name))))

(defun %compile-method (class-name method-index)
  (let* ((class (gethash class-name *classes*))
         (method (aref (slot-value class 'methods) (1- method-index)))
         (exception-table (slot-value (gethash "Code" (slot-value method 'attributes)) 'exceptions))
         (code (slot-value (gethash "Code" (slot-value method 'attributes)) 'code))
         (max-locals (slot-value (gethash "Code" (slot-value method 'attributes)) 'max-locals))
         (length (length code))
         (*context* (make-instance '<context>
                                   :class class
                                   :classes *classes*
                                   :exception-table exception-table
                                   :bytecode code
                                   :pc 0
                                   :is-clinit-p (string= "<clinit>" (slot-value method 'name)))))
    (when *debug-codegen*
      (format t "; compiling ~A.~A~%" class-name (lispize-method-name (format nil "~A~A" (name method) (descriptor method))))
      (force-output))
    (if (static-p method)
        (setf (fn-name *context*) (format nil "~A.~A" (slot-value class 'name) (lispize-method-name (format nil "~A~A" (slot-value method 'name) (slot-value method 'descriptor)))))
        (setf (fn-name *context*) (format nil "~A" (lispize-method-name (format nil "~A~A" (slot-value method 'name) (slot-value method 'descriptor))))))
    ;; (dump "compile-method" (list class-name method-index))
    (let* ((ssa-code-0
             (setf (ssa-code *context*)
                   (apply #'append
                          (loop
                            while (< (pc *context*) length)
                            for result = (funcall
                                          (aref +opcodes+ (aref code (pc *context*)))
                                          *context* code)
                            unless (null result)
                              collect result))))
           (blocks (build-basic-blocks ssa-code-0))
           (lisp-code
             (list (list 'block nil
                         (cons 'tagbody (loop for bloc in blocks append (codegen bloc))))))
           (traced-lisp-code (if *debug-trace* (list (list 'unwind-protect (car lisp-code) (list 'format 't "; trace: leaving  ~A.~A~%" class-name (fn-name *context*)))) lisp-code))
           (definition-code
             (let ((parameter-count (count-parameters (slot-value method 'descriptor))))
               (append (if (static-p method)
                           (list 'defun
                                 (intern (fn-name *context*):openldk)
                                 (loop for i from 1 upto parameter-count
                                       collect (intern (format nil "arg~A" (1- i)) :openldk)))
                           (list 'defmethod
                                 (intern (fn-name *context*) :openldk)
                                 (cons (list (intern "this" :openldk) (intern (slot-value class 'name) :openldk))
                                       (loop for i from 1 upto parameter-count
                                             collect (intern (format nil "arg~A" i) :openldk)))))
                       (when *debug-trace*
                         (list (list 'format 't "; trace: entering ~A.~A~%" class-name (fn-name *context*))))
                       (if (slot-value *context* 'uses-stack-p)
                           (list (append (list 'let (if (static-p method)
                                                        (append (list (list 'stack nil))
                                                                (loop for i from 1 upto parameter-count
                                                                      collect (list (intern (format nil "local-~A" (1- i)) :openldk) (intern (format nil "arg~A" (1- i)) :openldk)))
                                                                (loop for i from (1+ parameter-count) upto max-locals
                                                                      collect (list (intern (format nil "local-~A" (1- i)) :openldk))))
                                                        (append (list (list 'stack nil))
                                                                (cons (list (intern "local-0" :openldk) (intern "this" :openldk))
                                                                      (loop for i from 1 upto parameter-count
                                                                            collect (list (intern (format nil "local-~A" i) :openldk) (intern (format nil "arg~A" i) :openldk))))
                                                                (loop for i from (+ 2 parameter-count) upto max-locals
                                                                      collect (list (intern (format nil "local-~A" (1- i)) :openldk))))))
                                         traced-lisp-code))
                           traced-lisp-code)))))
      (%eval definition-code))))

(defun %clinit (class)
  (let ((class (gethash (name class) *classes*)))
    (assert
     (or class (error "Can't find ~A" class)))
    (labels ((clinit (class)
               (let ((super-class (gethash (slot-value class 'super) *classes*)))
                 (when (and super-class
                            (not (initialized-p super-class)))
                   (clinit super-class)))
               (let ((<clinit>-method (find-if
                                       (lambda (method) (and (string= (slot-value method 'name) "<clinit>")
                                                             (string= (slot-value method 'descriptor) "()V")))
                                       (slot-value class 'methods))))
                 (when <clinit>-method
                   (setf (initialized-p class) t)
                   (%eval (list (intern (format nil "~A.<clinit>()" (slot-value class 'name)) :openldk)))))))
      (clinit class))))

(defun open-java-classfile-on-classpath (class)
  (let* ((class (substitute (uiop:directory-separator-for-host) #\. class)))
    (loop for cpe in *classpath*
          for classfile-stream = (open-java-classfile cpe class)
          when classfile-stream
            return classfile-stream)))

(defun initform-from-descriptor (descriptor)
  (cond
    ((string= descriptor "I")
     0)
    ((string= descriptor "J")
     0)
    ((string= descriptor "F")
     0.0)
    ((string= descriptor "D")
     0.0d0)
    ((string= descriptor "S")
     0)
    ((string= descriptor "B")
     0)
    ((string= descriptor "C")
     #\0)
    ((string= descriptor "Z")
     0)
    (t nil)))

(defun emit-<class> (class)
  (let ((defclass-code (with-slots (name super interfaces fields) class
                         (list
                          'progn
                          (list
                           'defclass (intern name :openldk)
                           (if (or super interfaces) (append (if super (list (intern super :openldk)) nil) (mapcar (lambda (i) (intern i :openldk)) (coerce interfaces  'list))) (list))
                           (append
                            (map 'list
                                 (lambda (f)
                                   (list (intern (slot-value f 'name) :openldk)
                                         :initform (initform-from-descriptor (slot-value f 'descriptor))
                                         :allocation
                                         (if (eq 0 (logand 8 (slot-value f 'access-flags))) :instance :class)))
                                 fields)
                            (when (string= name "java/lang/Object")
                              (list (list '%hash-code :initform nil)))))
                          (list
                           'defparameter (intern (format nil "+static-~A+" (intern name)) :openldk)
                           (list
                            'make-instance (list 'quote (intern name :openldk)))))))
        (methods-code
          (let ((method-index 0))
            (with-slots (name super methods) class
              (remove nil (map 'list
                               (lambda (m)
                                 (if (or (bridge-p m) (native-p m))
                                     (progn
                                       (incf method-index)
                                       nil)
                                     (if (static-p m)
                                         (list 'defun (intern (format nil "~A.~A" (slot-value class 'name) (lispize-method-name (format nil "~A~A" (slot-value m 'name) (slot-value m 'descriptor)))) :openldk)
                                               (loop for i from 1 upto (count-parameters (slot-value m 'descriptor))
                                                     collect (intern (format nil "arg~A" i) :openldk))
                                               (list '%compile-method (slot-value class 'name) (incf method-index))
                                               (cons (intern (format nil "~A.~A" (slot-value class 'name) (lispize-method-name (format nil "~A~A" (slot-value m 'name) (slot-value m 'descriptor)))) :openldk)
                                                     (loop for i from 1 upto (count-parameters (slot-value m 'descriptor))
                                                           collect (intern (format nil "arg~A" i) :openldk))))
                                         (list 'defmethod (intern (lispize-method-name (format nil "~A~A" (slot-value m 'name) (slot-value m 'descriptor))) :openldk)
                                               (cons (list (intern "this" :openldk) (intern (slot-value (slot-value m 'class) 'name) :openldk))
                                                     (loop for i from 1 upto (count-parameters (slot-value m 'descriptor))
                                                           collect (intern (format nil "arg~A" i) :openldk)))
                                               (list '%compile-method (slot-value class 'name) (incf method-index))
                                               (cons (intern (lispize-method-name (format nil "~A~A" (slot-value m 'name) (slot-value m 'descriptor))) :openldk)
                                                     (cons (intern "this" :openldk)
                                                           (loop for i from 1 upto (count-parameters (slot-value m 'descriptor))
                                                                 collect (intern (format nil "arg~A" i) :openldk))))))))
                               methods))))))
    (append defclass-code methods-code)))

(defun classload (classname)
  (let ((class (gethash classname *classes*))
        (internal-classname (intern (substitute #\/ #\. classname) :openldk)))
    (if class
        class
        (let ((classfile-stream (open-java-classfile-on-classpath classname)))
          (if classfile-stream
              (unwind-protect
                   (let* ((class
                            (let ((c (read-classfile classfile-stream)))
                              (setf (gethash (substitute #\/ #\. classname) *classes*) c)
                              c))
                          (super (let ((super (slot-value class 'super)))
                                   (when super (classload super))))
                          (interfaces (let ((interfaces (slot-value class 'interfaces)))
                                        (when interfaces
                                          (mapcar (lambda (i) (classload i)) (coerce interfaces 'list))))))
                     (let ((klass (make-instance '|java/lang/Class|))
                           (cname (make-instance '|java/lang/String|))
                           (cloader nil)) ;; (make-instance '|java/lang/ClassLoader|)))
                       (with-slots (|name| |classLoader|) klass
                         (setf (slot-value cname '|value|) (substitute #\/ #\. classname))
                         (setf |name| cname)
                         (setf |classLoader| cloader))
                       (setf (java-class class) klass)
                       (setf (gethash (substitute #\/ #\. classname) *java-classes*) klass))
                     (let ((code (emit-<class> class)))
                       (%eval code))

                     ;; Emit the class initializer
                     (let ((lisp-class (find-class (intern (substitute #\/ #\. classname) :openldk))))
                       (closer-mop:finalize-inheritance lisp-class)
                       (let ((icc (append (list 'defun (intern (format nil "%clinit-~A" (substitute #\/ #\. classname)) :openldk) (list))
                                          ;; (list (list 'format 't ">>> clinit ~A~%" lisp-class))
                                          (loop for k in (reverse (closer-mop:class-precedence-list lisp-class))
                                                for clinit-function = (intern (format nil "~a.<clinit>()" (class-name k)) :openldk)
                                                when (fboundp clinit-function)
                                                  collect (let ((ldkclass (gethash (format nil "~A" (class-name k)) *classes*)))
                                                            (list 'unless (list 'initialized-p ldkclass)
                                                                  (list 'setf (list 'initialized-p ldkclass) t)
                                                                  (list clinit-function)))))))
                         (%eval icc)))

                     (when (and (not (string= classname "java/lang/Throwable"))
                                (subtypep (find-class internal-classname) (find-class '|java/lang/Throwable|)))
                       (let ((condition-symbol (intern (format nil "condition-~A" classname) :openldk)))
                         (setf (gethash (find-class (intern classname :openldk)) *condition-table*) condition-symbol)
                         (let ((ccode (list 'define-condition condition-symbol
                                            (list (intern (format nil "condition-~A" (slot-value super 'name)) :openldk)) (list))))
                           (%eval ccode))
                         (let ((ccode (list 'defmethod 'lisp-condition (list (list 'throwable (intern (format nil "~A" classname) :openldk)))
                                            (list 'make-condition (list 'quote (intern (format nil "condition-~A" classname) :openldk))))))
                           (%eval ccode))))
                     class)
                (close classfile-stream))
              (format t "ERROR: Can't find ~A on classpath~%" classname))))))

@cli:command
(defun main (mainclass &optional (args (list)) &key dump-dir classpath)
  (declare
   (cli:parser (list identity) args)
   (cli:parser identity classpath)
   (cli:parser identity dump-dir))
  "openldk - copyright (C) 2023-2024 Anthony Green <green@moxielogic.com>
   Distributed under the terms of the GPLv3 + Classpath Exception

   MAINCLASS: The class with the static main method to execute.

   ARGS: Java program command line arguments

   CLASSPATH: The classpath from which classes are loaded.

   DUMP-DIR: The directory into which internal debug info is dumped."

  ;; If classpath isn't set on the command line, then get it
  ;; from the LDK_CLASSPATH environment variable.
  (unless classpath
    (setf classpath (uiop:getenv "LDK_CLASSPATH")))

  (let ((LDK_DEBUG (uiop:getenv "LDK_DEBUG")))
    (when LDK_DEBUG
      (progn
        (when (find #\c LDK_DEBUG)
          (setf *debug-codegen* t))
        (when (find #\s LDK_DEBUG)
          (setf *debug-stack* t))
        (when (find #\t LDK_DEBUG)
          (setf *debug-trace* t))
        (when (find #\u LDK_DEBUG)
          (setf *debug-unmuffle* t)))))

  (setf *dump-dir* dump-dir)

  (setf *classpath*
        (loop for cpe in (split-sequence:split-sequence (uiop:inter-directory-separator) classpath)
              collect (if (str:ends-with? ".jar" cpe)
                          (make-instance 'jar-classpath-entry :jarfile cpe)
                          (make-instance 'dir-classpath-entry :dir cpe))))

  ;; We need to hand load these before Class.forName0 will work.
  (%clinit (classload "java/lang/Object"))
  (%clinit (classload "java/lang/String"))
  (%clinit (classload "java/lang/Class"))

  ;; Preload some important classes.
  (dolist (c '("java/lang/Boolean"
               "java/lang/Character"
               "java/lang/Byte"
               "java/lang/Short"
               "java/lang/Integer"
               "java/lang/Long"
               "java/lang/Float"
               "java/lang/Double"
               "java/lang/Void"
               "java/lang/ClassLoader"
               "java/security/PrivilegedAction"
               "java/lang/System"
               "java/lang/ThreadGroup"
               "java/lang/Thread"
               "java/lang/ref/SoftReference"
               "java/util/Properties"))
    (|java/lang/Class.forName0(Ljava/lang/String;ZLjava/lang/ClassLoader;Ljava/lang/Class;)| (jstring c) nil nil nil))

  (let ((props (make-instance '|java/util/Properties|)))
    (|<init>()| props)
    (setf (slot-value |+static-java/lang/System+| '|props|) props))

  (let* ((class (classload mainclass))
         (argv (make-array (length args))))
    (assert (or class (error "Can't load ~A" mainclass)))
    (dotimes (i (length args))
      (let ((arg (make-instance '|java/lang/String|)))
        (setf (slot-value arg '|value|) (nth i args))
        (setf (aref argv i) arg)))
    (%clinit class)
    (%eval (list (intern (format nil "~A.main([Ljava/lang/String;)" (slot-value class 'name)) :openldk) argv))))

(defun main-wrapper ()
  "Main entry point into OpenLDK.  Process command line errors here."
  (format t "DYNAMIC SPACE (GB) = ~A~%" (ceiling (sb-ext:dynamic-space-size) (* 1024 1024 1024)))
  (handler-case
      (main-command)
    (cli:wrong-number-of-args (e)
      (format t "~A~%" e))))
