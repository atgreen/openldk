;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: OPENLDK; Base: 10 -*-
;;;
;;; Copyright (C) 2026  Anthony Green <green@moxielogic.com>
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

;;;
;;;
(in-package :openldk)

(defvar *loader-lifecycle-lock* (bt:make-recursive-lock "Java loader lifecycle"))
(defvar *class-unloading-enabled* nil)
(defvar *unloaded-java-class-count* 0)

(defstruct loader-resources id package (classes nil) (functions nil))
(defstruct owned-java-method class function)

(defun %owner-cache-table (registry owner &optional (test 'equal) weakness)
  "A persistent cache whose owner, not the registry, controls its lifetime."
  (or (gethash owner registry)
      (bt:with-recursive-lock-held (*loader-lifecycle-lock*)
        (or (gethash owner registry)
            (setf (gethash owner registry) (make-hash-table :test test :weakness weakness :synchronized t))))))

(defun %retain-class-runtime-root (class object)
  "Keep runtime state with its defining class, rather than a process registry."
  (bt:with-recursive-lock-held (*loader-lifecycle-lock*)
    (pushnew object (slot-value class 'runtime-roots) :test #'eq))
  object)

(defun %retain-field-offset-owner (field)
  "A synthetic offset must stay valid while its declaring class remains live."
  (when-let ((class (get-ldk-class-for-java-class (slot-value field '|clazz|))))
    (%retain-class-runtime-root class field))
  field)

(defun %weak-class-reference (class)
  "A lazy entry point must not become an independent class-loader root."
  (if (%user-ldk-loader-p (ldk-loader class)) (sb-ext:make-weak-pointer class) class))

(defun %resolve-class-reference (reference)
  "Dereference a loader-owned class at a method entry point."
  (or (if (typep reference 'sb-ext:weak-pointer) (sb-ext:weak-pointer-value reference) reference)
      (%throw '|java/lang/NoClassDefFoundError| "Defining class loader has been unloaded")))

(defun %remember-loader-function (class symbol)
  "Record a process-owned function name for eventual loader cleanup."
  (when-let ((resources (slot-value (ldk-loader class) 'unload-resources)))
    (bt:with-recursive-lock-held (*loader-lifecycle-lock*)
      (pushnew symbol (loader-resources-functions resources)))))

(defmacro %call-owned-java-method (reference &rest arguments)
  "Pin a loader-owned method artifact for the whole invocation."
  `(let ((artifact (sb-ext:weak-pointer-value ,reference)))
     (unless artifact
       (%throw '|java/lang/NoClassDefFoundError| "Defining class loader has been unloaded"))
     (sb-sys:with-pinned-objects (artifact)
       (funcall (owned-java-method-function artifact) ,@arguments))))

(defmacro %define-owned-method-bridge-factory ()
  "Generate fixed-arity weak bridges without per-call list allocation."
  `(defun %owned-method-bridge (reference arity)
     "Make a weak entry point without allocating argument lists for common arities."
     (case arity
       ,@(loop for arity from 0 to 8
               for args = (loop repeat arity collect (gensym "ARG"))
               collect `(,arity (lambda ,args (%call-owned-java-method reference ,@args))))
       (otherwise
        (lambda (&rest args)
          (let ((artifact (sb-ext:weak-pointer-value reference)))
            (unless artifact
              (%throw '|java/lang/NoClassDefFoundError| "Defining class loader has been unloaded"))
            (sb-sys:with-pinned-objects (artifact)
              (apply (owned-java-method-function artifact) args))))))))

(%define-owned-method-bridge-factory)

(defun %install-owned-java-function (class symbol function &optional arity)
  "Expose a weak entry point; the defining class owns the actual function.
The pinned artifact keeps the owner live throughout an executing method."
  (let* ((artifact (%retain-class-runtime-root class (make-owned-java-method :class class :function function)))
         (reference (sb-ext:make-weak-pointer artifact)))
    (%remember-loader-function class symbol)
    (setf (fdefinition symbol) (%owned-method-bridge reference arity))))

(defun %evaluate-java-definition (class form)
  "Keep user-loader method bodies out of process-owned function definitions."
  (if (not (%user-ldk-loader-p (ldk-loader class)))
      (%eval form)
      (case (first form)
        ((progn) (dolist (definition (rest form)) (%evaluate-java-definition class definition)))
        (defun
         (destructuring-bind (name arguments &rest body) (rest form)
           (let ((declarations nil))
             (when (stringp (first body)) (pop body))
             (loop while (and (consp (first body)) (eq (caar body) 'declare))
                   do (push (pop body) declarations))
             (let* ((lambda-form `(sb-int:named-lambda ,name ,arguments ,@(nreverse declarations) (block ,name ,@body)))
                    (function (%eval lambda-form)))
               ;; Interpreted methods still need a named compiled frame for
               ;; caller discovery. Only this tiny forwarding body compiles.
               (unless (compiled-function-p function)
                 (let ((variables (remove-if (lambda (arg) (member arg '(&rest &optional))) arguments)))
                   (setf function (compile nil `(sb-int:named-lambda ,name ,arguments (funcall ,function ,@variables))))))
               (%install-owned-java-function class name function (length arguments))))))
        (otherwise (%eval form)))))

(defun %class-static-instance (class)
  "Static storage of CLASS, with loader-owned state for unloadable classes."
  (if (%user-ldk-loader-p (ldk-loader class))
      (slot-value class 'static-instance)
      (symbol-value (intern (format nil "+static-~A+" (name class)) (ldk-class-package class)))))

(defun %class-static-instance-by-symbol (symbol)
  "Resolve an unloadable class's static storage from its Lisp class symbol."
  (let* ((loader (%ldk-loader-for-symbol symbol))
         (class (and loader (gethash (symbol-name symbol) (slot-value loader 'ldk-classes-by-bin-name)))))
    (unless class (%throw '|java/lang/NoClassDefFoundError| (symbol-name symbol)))
    (%class-static-instance class)))

(defun %register-owned-static-instance (class instance)
  "Retain static Java state through its defining class and loader."
  (when (symbolp class)
    (let ((loader (%ldk-loader-for-symbol class)))
      (setf class (gethash (symbol-name class) (slot-value loader 'ldk-classes-by-bin-name)))))
  (setf (slot-value class 'static-instance) instance
        (slot-value instance *java-loader-owner-slot*) (ldk-loader class))
  (when-let ((resources (slot-value (ldk-loader class) 'unload-resources)))
    (bt:with-recursive-lock-held (*loader-lifecycle-lock*)
      (pushnew (intern (name class) (ldk-class-package class)) (loader-resources-classes resources))))
  instance)

(defun %java-static-value (symbol)
  "Read a static storage symbol, including unloadable symbol-macro storage."
  (if (boundp symbol) (symbol-value symbol)
      (let* ((name (symbol-name symbol))
             (class-name (subseq name 8 (1- (length name)))))
        (%class-static-instance-by-symbol (intern class-name (symbol-package symbol))))))

(defmethod initialize-instance :after ((object |java/lang/Object|) &key)
  (let ((loader (%ldk-loader-for-symbol (class-name (class-of object)))))
    (when (%user-ldk-loader-p loader)
      (setf (slot-value object *java-loader-owner-slot*) loader))))

(defun %cleanup-loader-resources (resources)
  "Release process-owned names after no Java root retains their loader."
  (when (fboundp '%jni-unload-loader-libraries)
    (%jni-unload-loader-libraries (loader-resources-id resources)))
  (%forget-loader-constraints (loader-resources-id resources))
  (when (boundp '*methods-being-compiled*)
    (let ((table (symbol-value '*methods-being-compiled*)))
      (maphash (lambda (key value)
                 (declare (ignore value))
                 (when (and (consp key) (eql (first key) (loader-resources-id resources)))
                   (remhash key table))) table)))
  (dolist (variable '(*dynamic-lambda-classes* *mh-proxy-classes*))
    (when (boundp variable)
      (let ((table (symbol-value variable)))
        (maphash (lambda (key value)
                   (when (member value (loader-resources-classes resources)) (remhash key table))) table))))
  ;; Metadata is already unreachable, so no thread can add to this record.
  ;; Do not hold the lifecycle lock while invalidating generic-function caches.
  (mapc #'fmakunbound (loader-resources-functions resources))
  (dolist (symbol (loader-resources-classes resources))
    (when-let ((class (find-class symbol nil)))
      (remhash class *condition-table*)
      (dolist (method (copy-list (sb-mop:specializer-direct-methods class)))
        (when-let ((generic (sb-mop:method-generic-function method)))
          (remove-method generic method)))
      ;; Unbinding a name alone leaves its class rooted in superclass lists.
      (sb-kernel:with-world-lock ()
        (dolist (parent (sb-mop:class-direct-superclasses class))
          (sb-mop:remove-direct-subclass parent class))
        (setf (find-class symbol) nil))))
  (bt:with-recursive-lock-held (*loader-lifecycle-lock*)
    (incf *unloaded-java-class-count* (length (loader-resources-classes resources)))
    (let ((package (loader-resources-package resources)))
      (when (package-name package)
        (dolist (user (package-used-by-list package))
          (when (package-name user) (unuse-package package user)))
        (delete-package package)))))

(defun %register-loader-resources (loader)
  "Register cleanup without retaining the loader from its finalizer."
  (when (and (%user-ldk-loader-p loader) (not (slot-value loader 'unload-resources)))
    (let ((resources (make-loader-resources :id (id loader) :package (loader-package loader))))
      (setf (slot-value loader 'unload-resources) resources)
      (trivial-garbage:finalize loader (lambda () (%cleanup-loader-resources resources)))))
  loader)

(defun %enable-class-unloading ()
  "Detach compatibility aliases from the permanent bootstrap class maps."
  (unless *class-unloading-enabled*
    (dolist (variable '(*ldk-classes-by-bin-name* *ldk-classes-by-fq-name*
                        *java-classes-by-bin-name* *java-classes-by-fq-name*))
      (let ((old (symbol-value variable))
            (weak (make-hash-table :test #'equal :weakness :value :synchronized t)))
        (maphash (lambda (key value) (setf (gethash key weak) value)) old)
        (setf (symbol-value variable) weak)))
    (setf *class-unloading-enabled* t))
  (maphash (lambda (package loader) (declare (ignore package)) (%register-loader-resources loader))
           *ldk-loaders-by-package*)
  nil)
