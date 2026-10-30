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

;;; JSR-292 method handles.
;;;
;;; The JDK runs a method handle by jumping to its LambdaForm's vmentry,
;;; bytecode it generates at runtime (InvokerBytecodeGenerator, through the
;;; ClassFile API) for every new form: bound handles, adapters, combinators,
;;; call-site invokers.  OpenLDK generates none.  It runs a method handle
;;; itself (%MH-INVOKE):
;;;
;;;  - a direct method handle calls its MemberName: a method, a
;;;    constructor, or a field access;
;;;  - any other handle (bound, delegating, a combinator's) interprets its
;;;    LambdaForm through a cached compiled graph: each Name
;;;    applies its NamedFunction to earlier Names' values, and a function
;;;    that is itself a method handle intrinsic (invokeBasic, linkTo*)
;;;    recurses here. The interpreter remains available for comparisons.
;;;
;;; LambdaForm.compileToBytecode does nothing, and prepare() gives a form
;;; without a vmentry the marker *LF-INTERPRET-ENTRY*.

(in-package :openldk)

(defvar *enable-compiled-lambda-forms* t)

(defvar *lf-interpret-entry* nil
  "The vmentry of LambdaForms OpenLDK interprets: a MemberName naming nothing.")

(defun %lf-interpret-entry ()
  (or *lf-interpret-entry*
      (setf *lf-interpret-entry* (%make-java-instance "java/lang/invoke/MemberName"))))

(defun %java-class-p (object class-name)
  "True when OBJECT is an instance of the Java class CLASS-NAME, if that is loaded."
  (let ((class (find-class (intern class-name :openldk) nil)))
    (and class (typep object class))))

(defun %member-ref-kind (member)
  (ash (logand (slot-value member '|flags|) #x0F000000) -24))

(defvar *member-field-slots* (make-hash-table :test 'eq :weakness :key :synchronized t)
  "MemberName -> the CLOS slot holding the field it names.")

(defun %field-object-slot (field)
  "Resolve a reflective Field or field MemberName using its declaring class."
  (%resolve-field-slot (get-ldk-class-for-java-class (slot-value field '|clazz|))
                       (lstring (slot-value field '|name|))))

(defun %member-field-slot (member)
  "The CLOS slot holding the field MEMBER names.  Computed once per
MemberName: field access through a method handle asks on every call."
  (or (gethash member *member-field-slots*)
      (setf (gethash member *member-field-slots*)
            (%field-object-slot member))))

(defun %member-static-storage (member)
  "The object holding the static fields of MEMBER's class, initializing it."
  (let* ((clazz (slot-value member '|clazz|))
         (class-name (substitute #\/ #\. (lstring (slot-value clazz '|name|))))
         (ldk-class (get-ldk-class-for-java-class clazz)))
    (%clinit ldk-class)
    (%class-static-instance ldk-class)))

(defun %member-invoke (member args)
  "Apply what MEMBER (a MemberName) names to ARGS: call a method or
constructor, or get or put a field (REF_getField 1, getStatic 2, putField 3,
putStatic 4)."
  (case (%member-ref-kind member)
    (1 (slot-value (first args) (%member-field-slot member)))
    (2 (slot-value (%member-static-storage member) (%member-field-slot member)))
    (3 (setf (slot-value (first args) (%member-field-slot member)) (second args))
       nil)
    (4 (setf (slot-value (%member-static-storage member) (%member-field-slot member)) (first args))
       nil)
    (t (apply #'%invoke-from-member-name member args))))

(defvar *member-intrinsics* (make-hash-table :test 'eq :weakness :key :synchronized t)
  "MemberName -> :INVOKE, :LINK-TO or :NONE: which method handle intrinsic
it names, if any.")

(defun %member-intrinsic (member)
  (or (gethash member *member-intrinsics*)
      (setf (gethash member *member-intrinsics*)
            (let ((clazz (slot-value member '|clazz|)))
              (if (and clazz (string= (lstring (slot-value clazz '|name|)) "java.lang.invoke.MethodHandle"))
                  (let ((name (lstring (slot-value member '|name|))))
                    (cond
                      ((member name '("invokeBasic" "invokeExact" "invoke") :test #'string=) :invoke)
                      ((starts-with? "linkTo" name) :link-to)
                      (t :none)))
                  :none)))))

(defun %mh-intrinsic-invoke (member args)
  "When MEMBER is a method handle intrinsic, apply it to ARGS and return
(values result T); otherwise NIL."
  (case (%member-intrinsic member)
    (:invoke (values (%mh-invoke (first args) (rest args)) t))
    (:link-to (values (%member-invoke (car (last args)) (butlast args)) t))))

(defun %named-function-invoke (function args)
  "Apply the LambdaForm NamedFunction FUNCTION to ARGS."
  (let ((member (slot-value function '|member|)))
    (if member
        (multiple-value-bind (result intrinsic) (%mh-intrinsic-invoke member args)
          (if intrinsic
              result
              (%member-invoke member args)))
        (%mh-invoke (or (slot-value function '|resolvedHandle|)
                        (|resolvedHandle()Ljava/lang/invoke/MethodHandle;| function))
                    args))))

(defun %lf-interpret (form args)
  "Interpret the LambdaForm FORM on ARGS (the method handle, then its
arguments), as LambdaForm.interpretWithArguments does."
  (let* ((names (slot-value form '|names|))
         (count (java-array-length names))
         (values (make-array count :initial-element nil))
         (name-class (find-class '|java/lang/invoke/LambdaForm$Name|)))
    (loop for arg in args
          for i from 0
          do (setf (aref values i) arg))
    (loop for i from (slot-value form '|arity|) below count
          for name = (jaref names i)
          do (setf (aref values i)
                   (%named-function-invoke
                    (slot-value name '|function$|)
                    (map 'list (lambda (a)
                                 (if (typep a name-class)
                                     (aref values (slot-value a '|index|))
                                     a))
                         (java-array-data (slot-value name '|arguments|))))))
    (let ((result (slot-value form '|result|)))
      (if (< result 0) nil (aref values result)))))

(defun %mh-invoke (mh args)
  "Invoke the method handle MH on ARGS, in the representation of its type
(primitives as Lisp values, as invokeExact passes them)."
  (cond
    ((null mh)
     (error (%lisp-condition (%make-throwable '|java/lang/NullPointerException|))))
    ((%java-class-p mh "java/lang/invoke/DirectMethodHandle")
     (%member-invoke (slot-value mh '|member|) args))
    (t
     (if *enable-compiled-lambda-forms*
         (let ((form (slot-value mh '|form|)))
           (apply (%lf-compiled-function form) form mh args))
         (%lf-interpret (slot-value mh '|form|) (cons mh args))))))

;;; MethodHandle.invoke: asType to the call site's type, then invokeExact.

(defvar *call-site-method-types* (make-hash-table :test #'eq :weakness :key :synchronized t)
  "Calling Class -> descriptor -> MethodType, retaining resolved type identities.")
(defvar *host-call-site-method-types* (make-hash-table :test #'equal :weakness :value :synchronized t)
  "Host calls without a Java constant pool must not retain application classes.")

(defun %descriptor-method-type (descriptor &optional caller)
  "Resolve a signature through its caller's loader and retain it with that caller."
  (let ((table (if caller (%owner-cache-table *call-site-method-types* caller) *host-call-site-method-types*)))
    (or (gethash descriptor table)
        (setf (gethash descriptor table)
              (|java/lang/invoke/MethodType.fromMethodDescriptorString(Ljava/lang/String;Ljava/lang/ClassLoader;)Ljava/lang/invoke/MethodType;|
               (jstring descriptor) (if caller (%class-java-loader caller) *boot-class-loader*))))))

(defun %invoke-generic-at (mh descriptor caller &rest args)
  "MethodHandle.invoke at a call site whose defining class is CALLER."
  (when (null mh)
    (error (%lisp-condition (%make-throwable '|java/lang/NullPointerException|))))
  (let ((type (%descriptor-method-type descriptor caller)))
    (%mh-invoke (if (eq type (slot-value mh '|type$|))
                    mh
                    (|asType(Ljava/lang/invoke/MethodType;)| mh type))
                args)))

(defun %invoke-generic (mh descriptor &rest args)
  "Adapt a method handle for a host call without a Java constant-pool owner."
  (apply #'%invoke-generic-at mh descriptor nil args))

;;; Call sites.

(defun |java/lang/invoke/MethodHandleNatives.setCallSiteTargetNormal(Ljava/lang/invoke/CallSite;Ljava/lang/invoke/MethodHandle;)V| (site target)
  (setf (slot-value site '|target|) target)
  nil)

(defun |java/lang/invoke/MethodHandleNatives.setCallSiteTargetVolatile(Ljava/lang/invoke/CallSite;Ljava/lang/invoke/MethodHandle;)V| (site target)
  (setf (slot-value site '|target|) target)
  nil)

;;; invokeWithArguments: boxed arguments in, a boxed result out.

(defun %mh-invoke-with-arguments (mh arguments)
  "MethodHandle.invokeWithArguments: ARGUMENTS (a list) boxed, the result
boxed.  As the JDK does, adapt MH to the generic type taking that many
Objects (asType boxes, unboxes and collects varargs)."
  (when (null mh)
    (error (%lisp-condition (%make-throwable '|java/lang/NullPointerException|))))
  (%mh-invoke (|asType(Ljava/lang/invoke/MethodType;)|
               mh (|java/lang/invoke/MethodType.genericMethodType(I)Ljava/lang/invoke/MethodType;| (length arguments)))
              arguments))

(setf (gethash "java/lang/invoke/MethodHandle.invokeWithArguments([Ljava/lang/Object;)Ljava/lang/Object;"
               *native-overrides*)
      (lambda (mh arguments)
        (%mh-invoke-with-arguments mh (and arguments (coerce (java-array-data arguments) 'list)))))

(setf (gethash "java/lang/invoke/MethodHandle.invokeWithArguments(Ljava/util/List;)Ljava/lang/Object;"
               *native-overrides*)
      (lambda (mh arguments)
        (%mh-invoke-with-arguments mh (coerce (java-array-data (|toArray()| arguments)) 'list))))

;;; No bytecode for LambdaForms.

(setf (gethash "java/lang/invoke/LambdaForm.compileToBytecode()V" *native-overrides*)
      (lambda (form) (declare (ignore form)) nil))

(setf (gethash "java/lang/invoke/LambdaForm.prepare()V" *native-overrides*)
      (lambda (form)
        (unless (slot-value form '|vmentry|)
          (setf (slot-value form '|vmentry|) (%lf-interpret-entry)))
        nil))

;;; JDK 25's sun.misc.Unsafe memory-access methods first call
;;; beforeMemoryAccess, which, unless --sun-misc-unsafe-memory-access=allow,
;;; walks the stack (StackWalker) to warn about its caller.  OpenLDK allows
;;; it: the IntelliJ platform (in the Kotlin compiler) calls them through
;;; method handles.
(setf (gethash "sun/misc/Unsafe.beforeMemoryAccess()V" *native-overrides*)
      (lambda () nil))

;;; MethodHandleProxies.asInterfaceInstance: JDK 25 generates a hidden
;;; proxy class with the ClassFile API.  Instead, a Lisp class implementing
;;; the interface, whose single abstract method invokes the handle, adapted
;;; to that method's type, exactly.

(defvar *mh-proxy-classes* (make-hash-table :test #'equal :synchronized t)
  "Interface binary name -> its proxy class symbol.")

(defun %single-abstract-method (lclass)
  "The <method> of the interface LCLASS (or a superinterface) that is
abstract, as (values method declaring-class)."
  (or (loop for m across (slot-value lclass 'methods)
            when (and (logtest #x400 (access-flags m)) (not (static-p m)))
              return (values m lclass))
      (loop for iname across (interfaces lclass)
            for super = (%resolve-class iname (ldk-loader lclass))
            do (multiple-value-bind (m c) (and super (%single-abstract-method super))
                 (when m (return (values m c)))))))

(defun %mh-proxy-class (interface-name method &optional loader)
  (let ((key (list (if loader (id loader) 0) interface-name)))
    (or (gethash key *mh-proxy-classes*)
        (setf (gethash key *mh-proxy-classes*)
            (let* ((class-sym (intern (format nil "openldk/MHProxy_~A" (substitute #\_ #\/ interface-name))
                                      (if (%user-ldk-loader-p loader) (loader-package loader) :openldk)))
                   (descriptor (slot-value method 'descriptor))
                   (method-sym (intern (lispize-method-name (format nil "~A~A" (slot-value method 'name) descriptor))
                                       :openldk))
                   (params (loop for i from 1 upto (count-parameters descriptor)
                                 collect (intern (format nil "P~A" i) :openldk))))
              (eval `(defclass/std ,class-sym (|java/lang/Object| ,(%interface-class-symbol interface-name loader))
                       ((target))))
              (unless (and (fboundp method-sym) (typep (fdefinition method-sym) 'generic-function))
                (ensure-generic-function method-sym :generic-function-class 'java-generic-function
                                                    :lambda-list (cons '|this| params)))
              (eval `(defmethod ,method-sym ((|this| ,class-sym) ,@params)
                       (%mh-invoke (slot-value |this| 'target) (list ,@params))))
              (when (%user-ldk-loader-p loader)
                (bt:with-recursive-lock-held (*loader-lifecycle-lock*)
                  (pushnew class-sym (loader-resources-classes (slot-value loader 'unload-resources)))))
              class-sym)))))

(setf (gethash "java/lang/invoke/MethodHandleProxies.asInterfaceInstance(Ljava/lang/Class;Ljava/lang/invoke/MethodHandle;)Ljava/lang/Object;"
               *native-overrides*)
      (lambda (interface mh)
        (let* ((interface-name (substitute #\/ #\. (lstring (slot-value interface '|name|))))
               (class (get-ldk-class-for-java-class interface))
               (method (%single-abstract-method class)))
          (unless method
            (error (%lisp-condition
                    (%make-throwable '|java/lang/IllegalArgumentException|
                                     (format nil "not a single-method interface: ~A" interface-name)))))
          (let ((proxy (make-instance (%mh-proxy-class interface-name method (ldk-loader class)))))
            (setf (slot-value proxy 'target)
                  (|asType(Ljava/lang/invoke/MethodType;)|
                   mh (%descriptor-method-type (slot-value method 'descriptor) interface)))
            proxy))))
