;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: OPENLDK; Base: 10 -*-
;;;
;;; Copyright (C) 2025  Anthony Green <green@moxielogic.com>
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

;;; Custom generic function metaclass for Java single-dispatch methods.
;;; Uses a hash-table cache keyed on receiver class instead of PCL's
;;; discrimination nets, avoiding expensive dfun rebuilds when thousands
;;; of Java classes are loaded.

(defclass java-generic-function (standard-generic-function)
  ((dispatch-cache :initform (make-hash-table :test 'eq)
                   :accessor java-gf-dispatch-cache)
   (invoke-special-cache :initform (make-hash-table :test 'eq)
                         :accessor java-gf-invoke-special-cache)
   (cache-lock :initform (bordeaux-threads:make-lock "java-gf-cache")
               :reader java-gf-cache-lock))
  (:metaclass sb-mop:funcallable-standard-class))

(defun %compute-java-effective-method (gf class)
  "Compute an effective method function for GF dispatching on CLASS.
Returns (values FUNCTION APPLICABLE-P); when APPLICABLE-P is NIL the
function signals no-applicable-method and must not be cached (the class
may still be a bare stub that a later classload fleshes out)."
  (let* ((lambda-list (closer-mop:generic-function-lambda-list gf))
         (nargs (length lambda-list))
         (class-list (cons class
                          (make-list (max 0 (1- nargs))
                                     :initial-element (find-class 't)))))
    (multiple-value-bind (methods definitive-p)
        (closer-mop:compute-applicable-methods-using-classes gf class-list)
      (declare (ignore definitive-p))
      (if (null methods)
          ;; No applicable methods — signal no-applicable-method
          ;; (preserves null → NullPointerException via native.lisp)
          (values (lambda (args)
                    (apply #'no-applicable-method gf args))
                  nil)
          ;; Partition into :around and primary methods
          (let* ((around (remove-if-not
                          (lambda (m) (equal (method-qualifiers m) '(:around)))
                          methods))
                 (primary (remove-if-not
                           (lambda (m) (null (method-qualifiers m)))
                           methods))
                 (chain (append around primary)))
            (if (null chain)
                (values (lambda (args)
                          (apply #'no-applicable-method gf args))
                        nil)
                (let ((first-mf (closer-mop:method-function (first chain)))
                      (rest-chain (rest chain)))
                  (values (lambda (args)
                            (funcall first-mf args rest-chain))
                          t))))))))

(defun %invalidate-java-gf-cache (gf specializer)
  "Invalidate GF's dispatch cache entries affected by a method change on
SPECIALIZER: only receivers that are subtypes of it can see a different
applicable-method set.  Non-class specializers flush everything."
  (let ((cache (java-gf-dispatch-cache gf))
        (special-cache (java-gf-invoke-special-cache gf)))
    (bordeaux-threads:with-lock-held ((java-gf-cache-lock gf))
      (if (typep specializer 'class)
          (let (stale)
            (maphash (lambda (k v)
                       (declare (ignore v))
                       (when (subtypep k specializer)
                         (push k stale)))
                     cache)
            (dolist (k stale) (remhash k cache)))
          (clrhash cache))
      (clrhash special-cache))))

(defmethod sb-mop:add-method :after ((gf java-generic-function) method)
  (%invalidate-java-gf-cache gf (first (sb-mop:method-specializers method))))

(defmethod sb-mop:remove-method :after ((gf java-generic-function) method)
  (%invalidate-java-gf-cache gf (first (sb-mop:method-specializers method))))

(defmethod closer-mop:compute-discriminating-function ((gf java-generic-function))
  "Return a discriminating function that dispatches via a hash-table cache
keyed on the receiver's class.  SBCL's update-dfun calls this whenever the
method set changes; invalidation is handled surgically by the
add-method/remove-method :after hooks above, so the caches persist —
flushing them wholesale here made every lazy class load recompute hot
dispatch (equals/hashCode/toString) from scratch."
  (let ((cache (java-gf-dispatch-cache gf))
        (lock (java-gf-cache-lock gf)))
    (lambda (&rest args)
      (let* ((receiver (first args))
             (class (class-of receiver))
             (emfun (gethash class cache)))
        (if emfun
            (funcall emfun args)
            (multiple-value-bind (new-emfun applicable-p)
                (%compute-java-effective-method gf class)
              (when applicable-p
                (bordeaux-threads:with-lock-held (lock)
                  (setf (gethash class cache) new-emfun)))
              (funcall new-emfun args)))))))


;;; Method names carry their return types, as the JVM's do, but some
;;; hand-written natives, and Lisp code calling Java methods, use names
;;; without them.  Each full-named generic function is registered under its
;;; short name, and a call that finds no applicable method in one forwards
;;; to the other (see NO-APPLICABLE-METHOD in native/support.lisp).

(defvar *full-gfs-by-short-name* (make-hash-table :test #'equal :synchronized t)
  "Short method name (no return type) -> full-named generic function symbols.")

(defun %register-full-gf (symbol)
  (let* ((name (symbol-name symbol))
         (close (position #\) name)))
    (when (and close (< (1+ close) (length name)))
      (pushnew symbol (gethash (subseq name 0 (1+ close)) *full-gfs-by-short-name*)))))

(defun %ensure-java-gf-1 (symbol nargs)
  (unless (fboundp symbol)
    (ensure-generic-function symbol
                             :generic-function-class 'java-generic-function
                             :lambda-list (cons (intern "this" :openldk)
                                                (loop for i from 1 below nargs
                                                      collect (intern (format nil "arg~A" i) :openldk))))))

(defun %ensure-java-gf (symbol nargs)
  "Make SYMBOL a Java method generic function of NARGS arguments (receiver
included) unless it already names a function, and register it.  The
generic function of its short name exists too, for Lisp code calling it."
  (%ensure-java-gf-1 symbol nargs)
  (let* ((name (symbol-name symbol))
         (close (position #\) name)))
    (when (and close (< (1+ close) (length name)))
      (%ensure-java-gf-1 (intern (subseq name 0 (1+ close)) (symbol-package symbol)) nargs)))
  (%register-full-gf symbol)
  symbol)

(defun %adopt-native-short-methods ()
  "For each hand-written native method named without its return type and
specialized on a loaded Java class, give the generic function of its full
name (the class's method of that name and arguments) a method forwarding
to it, unless it has one for that class.  Forwarding on a missing method
can't serve these: a method the full-named generic function has for a
superclass would shadow the native, as the bytecode of
sun.misc.Unsafe.getLongVolatile, calling itself on the
jdk.internal.misc.Unsafe instance OpenLDK's natives handle, did.  Run once
the runtime's classes are loaded."
  (do-symbols (short (find-package :openldk))
    (let* ((name (symbol-name short))
           (close (position #\) name)))
      (when (and close (= (1+ close) (length name))
                 (eq (symbol-package short) (find-package :openldk))
                 (fboundp short)
                 (typep (fdefinition short) 'generic-function))
        (dolist (method (closer-mop:generic-function-methods (fdefinition short)))
          (let* ((specializers (closer-mop:method-specializers method))
                 (class (first specializers))
                 (class-name (and (typep class 'class) (class-name class)))
                 (bin-name (and (symbolp class-name) (symbol-name class-name))))
            (when (and (null (method-qualifiers method))
                       bin-name (find #\/ bin-name)
                       (gethash bin-name *ldk-classes-by-bin-name*))
              (let ((full-name (%resolve-method-name bin-name name)))
                (unless (string= full-name name)
                  (let* ((full (intern full-name :openldk))
                         (nargs (length specializers)))
                    (%ensure-java-gf full nargs)
                    (let ((full-gf (fdefinition full)))
                      (when (and (typep full-gf 'generic-function)
                                 (= nargs (length (closer-mop:generic-function-lambda-list full-gf)))
                                 (notany (lambda (m)
                                           (and (null (method-qualifiers m))
                                                (eq class (first (closer-mop:method-specializers m)))))
                                         (closer-mop:generic-function-methods full-gf)))
                        (let ((params (loop for i from 1 below nargs
                                            collect (intern (format nil "ARG~A" i) :openldk))))
                          (ignore-errors
                           (eval `(defmethod ,full ((this ,class-name) ,@params)
                                    (,short this ,@params)))))))))))))))))
