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

(defvar *compiled-lambda-forms*
  (make-hash-table :test 'eq :weakness :key :synchronized t)
  "LambdaForm -> compiled graph. Values contain no Java object constants.")

(defmacro %member-invoke-fixed (member &rest arguments)
  "Invoke a MemberName without constructing or unboxing an argument list."
  (let ((target (gensym "MEMBER"))
        (args (loop repeat (length arguments) collect (gensym "ARG")))
        (kind (gensym "KIND")) (a (gensym "A")) (b (gensym "B"))
        (ret (gensym "RETURN")) (extra (gensym "EXTRA")))
    (labels ((unboxed (flags values)
               (loop for value in values for i from 0
                     collect `(if (nth ,i ,flags) (%maybe-unbox ,value) ,value))))
      `(let ((,target ,member) ,@(mapcar #'list args arguments))
         (case (%member-ref-kind ,target)
           (1 (slot-value ,(first args) (%member-field-slot ,target)))
           (2 (slot-value (%member-static-storage ,target) (%member-field-slot ,target)))
           (3 (setf (slot-value ,(first args) (%member-field-slot ,target)) ,(second args)) nil)
           (4 (setf (slot-value (%member-static-storage ,target) (%member-field-slot ,target)) ,(first args)) nil)
           (t
            (destructuring-bind (,kind ,a ,b &optional ,ret ,extra) (%resolve-member-name ,target)
              (declare (ignore ,ret))
              (ecase ,kind
                (:constructor
                 (%clinit ,a)
                 (let ((instance (make-instance (intern (name ,a) (ldk-class-package ,a)))))
                   (funcall ,b instance ,@(unboxed extra args))
                   instance))
                (:static
                 (when ,extra (%clinit ,extra))
                 (funcall ,a ,@(unboxed b args)))
                (:instance
                 (funcall ,a ,@(when args (cons (first args) (unboxed b (rest args))))))))))))))

(defmacro %mh-invoke-fixed (handle &rest arguments)
  "Fixed-arity graph invocation; a compiled graph receives ordinary arguments."
  (let ((mh (gensym "HANDLE"))
        (args (loop repeat (length arguments) collect (gensym "ARG"))))
    `(let ((,mh ,handle) ,@(mapcar #'list args arguments))
       (cond ((null ,mh)
              (error (%lisp-condition (%make-throwable '|java/lang/NullPointerException|))))
             ((%java-class-p ,mh "java/lang/invoke/DirectMethodHandle")
              (%member-invoke-fixed (slot-value ,mh '|member|) ,@args))
             (t (let ((form (slot-value ,mh '|form|)))
                  (funcall (%lf-compiled-function form) form ,mh ,@args)))))))

(defmacro %named-function-invoke-fixed (function &rest arguments)
  "Read NamedFunction's current target and invoke it without an argument list."
  (let ((fn (gensym "FUNCTION")) (member (gensym "MEMBER"))
        (args (loop repeat (length arguments) collect (gensym "ARG"))))
    `(let* ((,fn ,function) ,@(mapcar #'list args arguments)
            (,member (slot-value ,fn '|member|)))
       (if ,member
           (case (%member-intrinsic ,member)
             (:invoke (%mh-invoke-fixed ,(first args) ,@(rest args)))
             (:link-to (%member-invoke-fixed ,(car (last args)) ,@(butlast args)))
             (t (%member-invoke-fixed ,member ,@args)))
           (%mh-invoke-fixed
            (or (slot-value ,fn '|resolvedHandle|)
                (|resolvedHandle()Ljava/lang/invoke/MethodHandle;| ,fn))
            ,@args)))))

(defun %lf-fixed-invoker (arity)
  "One shared fixed-arity dispatcher per argument count, with no Java roots."
  (let ((symbol (intern (format nil "%LF-NAMED-INVOKE-~D" arity) :openldk)))
    (unless (fboundp symbol)
      (let ((args (loop repeat arity collect (gensym "ARG"))))
        (compile symbol `(lambda (function ,@args)
                           (%named-function-invoke-fixed function ,@args)))))
    symbol))

(defun %compile-lambda-form (form &optional (name-class (find-class '|java/lang/invoke/LambdaForm$Name|)))
  "Compile immutable Name wiring without capturing Java object constants.
Read constants and NamedFunction targets through the live form argument, so
cached functions cannot retain their weak keys or an application loader."
  (let* ((names (slot-value form '|names|))
         (arity (slot-value form '|arity|))
         (count (java-array-length names))
         (variables (loop repeat count collect (gensym "VALUE")))
         (live-form (gensym "FORM"))
         (live-names (gensym "NAMES"))
         (result (slot-value form '|result|)))
    (compile nil
             `(lambda (,live-form ,@(subseq variables 0 arity))
                (declare (optimize (speed 3) (safety 1))
                         (ignorable ,@(subseq variables 0 arity)))
                (let* ((,live-names (slot-value ,live-form '|names|))
                       ,@(loop for i from arity below count
                               for name = (jaref names i)
                               for live-name = (gensym "NAME")
                               append
                               (list
                                `(,live-name (jaref ,live-names ,i))
                                (list (nth i variables)
                                      `(,(%lf-fixed-invoker (java-array-length (slot-value name '|arguments|)))
                                        (slot-value ,live-name '|function$|)
                                        ,@(loop for argument across (java-array-data (slot-value name '|arguments|))
                                                for j from 0
                                                collect (if (typep argument name-class)
                                                            (nth (slot-value argument '|index|) variables)
                                                            `(jaref (slot-value ,live-name '|arguments|) ,j))))))))
                  (declare (ignorable ,live-names ,@(subseq variables arity)))
                  ,(if (minusp result) nil (nth result variables)))))))

(defun %lf-compiled-function (form)
  "Cache graphs by weak form identity, never by a global method signature."
  (or (gethash form *compiled-lambda-forms*)
      (setf (gethash form *compiled-lambda-forms*) (%compile-lambda-form form))))
