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
Returns a function of one argument (the argument list)."
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
          (lambda (args)
            (apply #'no-applicable-method gf args))
          ;; Partition into :around and primary methods
          (let* ((around (remove-if-not
                          (lambda (m) (equal (method-qualifiers m) '(:around)))
                          methods))
                 (primary (remove-if-not
                           (lambda (m) (null (method-qualifiers m)))
                           methods))
                 (chain (append around primary)))
            (if (null chain)
                (lambda (args)
                  (apply #'no-applicable-method gf args))
                (let ((first-mf (closer-mop:method-function (first chain)))
                      (rest-chain (rest chain)))
                  (lambda (args)
                    (funcall first-mf args rest-chain)))))))))

(defmethod closer-mop:compute-discriminating-function ((gf java-generic-function))
  "Return a discriminating function that dispatches via a hash-table cache
keyed on the receiver's class."
  (let ((cache (java-gf-dispatch-cache gf))
        (lock (java-gf-cache-lock gf)))
    (lambda (&rest args)
      (let* ((receiver (first args))
             (class (class-of receiver))
             (emfun (gethash class cache)))
        (if emfun
            (funcall emfun args)
            (let ((new-emfun (%compute-java-effective-method gf class)))
              (bordeaux-threads:with-lock-held (lock)
                (setf (gethash class cache) new-emfun))
              (funcall new-emfun args)))))))

;;; Override update-dfun to skip PCL's expensive discrimination-net
;;; rebuilding for java-generic-function GFs.  This is the single
;;; biggest win — prevents PCL from rebuilding dfuns when methods are
;;; added/removed or new classes are defined.
(let ((original-update-dfun (fdefinition 'sb-pcl::update-dfun)))
  (sb-ext:without-package-locks
    (setf (fdefinition 'sb-pcl::update-dfun)
          (lambda (gf &rest args)
            (if (and (typep gf 'java-generic-function)
                     (slot-boundp gf 'dispatch-cache))
                ;; Clear both caches and reinstall our fast DF
                (progn
                  (bordeaux-threads:with-lock-held ((java-gf-cache-lock gf))
                    (clrhash (java-gf-dispatch-cache gf))
                    (clrhash (java-gf-invoke-special-cache gf)))
                  (sb-mop:set-funcallable-instance-function
                   gf (closer-mop:compute-discriminating-function gf)))
                (apply original-update-dfun gf args))))))
