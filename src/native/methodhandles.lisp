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

;;; linkTo* intrinsics and DirectMethodHandle$Holder trampolines.

(in-package :openldk)

;;; The linkTo* MethodHandle intrinsics all delegate to
;;; %invoke-from-member-name: the last argument is the MemberName, the
;;; rest are method arguments (including the receiver for the non-static
;;; kinds).  Each kind gets a varargs variant named
;;; |java/lang/invoke/MethodHandle.linkToKIND(Ljava/lang/invoke/MemberName;)|
;;; plus fixed-arity variants
;;; |...linkToKIND(Ljava/lang/Object;...Ljava/lang/invoke/MemberName;)|.

(defmacro %define-linkto-wrappers (kind max-arity)
  "Define the linkTo<KIND> intrinsic family: a varargs variant plus
fixed variants taking 1..MAX-ARITY leading Object arguments."
  (let ((base (format nil "java/lang/invoke/MethodHandle.linkTo~A" kind)))
    `(progn
       (defun ,(intern (format nil "~A(Ljava/lang/invoke/MemberName;)" base) :openldk)
           (&rest args)
         ,(format nil "MethodHandle intrinsic: linkTo~A, varargs variant." kind)
         (apply #'%invoke-from-member-name (car (last args)) (butlast args)))
       ,@(loop for n from 1 upto max-arity
               collect
               (let ((name (intern (format nil "~A(~{~A~}Ljava/lang/invoke/MemberName;)"
                                           base
                                           (loop repeat n collect "Ljava/lang/Object;"))
                                   :openldk))
                     (params (loop for i from 1 upto n
                                   collect (intern (format nil "arg~A" i) :openldk))))
                 `(defun ,name (,@params member-name)
                    ,(format nil "MethodHandle intrinsic: linkTo~A, ~A-argument variant." kind n)
                    (%invoke-from-member-name member-name ,@params)))))))

(%define-linkto-wrappers "Static" 4)
(%define-linkto-wrappers "Virtual" 3)
(%define-linkto-wrappers "Special" 3)
(%define-linkto-wrappers "Interface" 3)

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

(defun %prebuilt-lambda-class (method-str sam-param-count interface-type-name)
  "Return the prebuilt openldk/Lambda* class for METHOD-STR when it
implements the requested INTERFACE-TYPE-NAME (or when the interface is
unknown, preserving legacy behavior), else NIL."
  (flet ((covers (&rest ifaces)
           (or (null interface-type-name)
               (member interface-type-name ifaces :test #'string=))))
    (cond
      ((and (string= method-str "get")
            (covers "java/util/function/Supplier"))
       '|openldk/LambdaSupplier|)
      ((and (string= method-str "test") sam-param-count (<= sam-param-count 1)
            (covers "java/util/function/Predicate"))
       '|openldk/LambdaPredicate|)
      ((and (string= method-str "test")
            (covers "java/util/function/BiPredicate"))
       '|openldk/LambdaBiPredicate|)
      ((and (string= method-str "apply") sam-param-count (<= sam-param-count 1)
            (covers "java/util/function/Function"))
       '|openldk/LambdaFunction|)
      ((and (string= method-str "apply")
            ;; BinaryOperator extends BiFunction, so the prebuilt class
            ;; passes checkcast for both.
            (covers "java/util/function/BinaryOperator" "java/util/function/BiFunction"))
       '|openldk/LambdaBinaryOperator|)
      ((and (string= method-str "accept") sam-param-count (<= sam-param-count 1)
            (covers "java/util/function/Consumer"))
       '|openldk/LambdaConsumer|)
      ((and (string= method-str "accept")
            (covers "java/util/function/BiConsumer"))
       '|openldk/LambdaBiConsumer|))))

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
         (sam-has-primitive-param
           (and sam-method-type
                (ignore-errors
                 (some #'identity
                       (%descriptor-param-prim-flags
                        (lstring (|toMethodDescriptorString()| sam-method-type)))))))
         (lambda-class (or
                        ;; A SAM with primitive parameters (e.g. a custom
                        ;; interface `int apply(int,int)`) cannot use the generic
                        ;; Object-based Lambda* helpers, whose method is
                        ;; apply(Object,Object).  Likewise a prebuilt Lambda*
                        ;; class only passes CHECKCAST for the exact
                        ;; java.util.function interface it implements -- a
                        ;; custom SAM like java.util.stream.Sink (accept, but
                        ;; NOT a Consumer as far as checkcast is concerned)
                        ;; must get a generated class implementing it.
                        (unless (and interface-type-name sam-has-primitive-param)
                          (%prebuilt-lambda-class method-str sam-param-count interface-type-name))
                        (%ensure-dynamic-lambda-class method-str sam-method-type interface-type-name)))
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
  nil)

