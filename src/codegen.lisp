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

(defclass/std <expression> ()
  ((insn
    code
    expression-type)))

(defmethod print-object ((expr <expression>) out)
  (print-unreadable-object (expr out :type t)
    (format out "{~A : ~A}" (insn expr) (code expr))))

(defun trace-insn (insn code)
  (if *debug-x*
      (list 'progn
            (list 'format t (format nil "~&; x[~A]~%" (address insn)))
            code)
      code))

(defmethod codegen ((insn ir-literal) context)
  (declare (ignore context))
  (make-instance '<expression>
                 :insn insn
                 :code (slot-value insn 'value)
                 :expression-type (slot-value insn 'type)))

(defmethod codegen ((insn ir-array-literal) context)
  (let* ((values (slot-value insn 'value))
         (size (length values)))
    (cond
      ;; Large constant array - precompute the Java strings NOW and embed reference
      ((and (vectorp values) (> size 50))
       (let* ((java-strings (coerce (loop for val across values
                                          collect (cond
                                                    ((null val) nil)
                                                    ((stringp val) (ijstring val))
                                                    (t val)))
                                   'vector)))
         (make-instance '<expression>
                        :insn insn
                        ;; Reference the pre-computed vector directly
                        :code `(make-java-array :component-class ,(component-class insn)
                                                :initial-contents (copy-seq ',java-strings))
                        :expression-type (slot-value insn 'type))))
      ;; Small constant array or vectorp - inline it
      ((vectorp values)
       (let ((wrapped-values (loop for val across values
                                   collect (cond
                                             ((null val) nil)
                                             ((stringp val) `(ijstring ,val))
                                             (t val)))))
         (make-instance '<expression>
                        :insn insn
                        :code `(make-java-array :component-class ,(component-class insn)
                                                :initial-contents (vector ,@wrapped-values))
                        :expression-type (slot-value insn 'type))))
      ;; IR nodes - must codegen each one
      (t
       (let ((codegenned-values (mapcar (lambda (ir-val)
                                          (code (codegen ir-val context)))
                                        values)))
         (make-instance '<expression>
                        :insn insn
                        :code `(make-java-array :component-class ,(component-class insn)
                                                :initial-contents (vector ,@codegenned-values))
                        :expression-type (slot-value insn 'type)))))))

(defmethod codegen ((insn ir-int-literal) context)
  (declare (ignore context))
  (make-instance '<expression>
                 :insn insn
                 :code (unsigned-to-signed-integer (slot-value insn 'value))
                 :expression-type (slot-value insn 'type)))

(defmethod codegen ((insn ir-double-literal) context)
  (declare (ignore context))
  (make-instance '<expression>
                 :insn insn
                 :code (coerce (slot-value insn 'value) 'double-float)
                 :expression-type (slot-value insn 'type)))

(defmethod codegen ((insn ir-long-literal) context)
  (declare (ignore context))
  (make-instance '<expression>
                 :insn insn
                 :code (unsigned-to-signed-long (slot-value insn 'value))
                 :expression-type (slot-value insn 'type)))

(defmethod codegen ((insn ir-string-literal) context)
  (declare (ignore context))
  (make-instance '<expression>
                 :insn insn
                 :code (ijstring (slot-value insn 'value))
                 :expression-type :REFERENCE))

(defun %make-throwable (throwable-class)
  (let ((throwable (make-instance throwable-class)))
    (|<init>()| throwable)
    throwable))

(defmethod codegen ((insn ir-aastore) context)
  ;;; FIXME: throw nullpointerexception if needed
  (with-slots (arrayref index value) insn
    (make-instance '<expression>
                   :insn insn
                   :code `(let ((value ,(code (codegen value context)))
                                (index ,(code (codegen index context)))
                                (arrayref ,(code (codegen arrayref context))))
                            (setf (jaref arrayref index) value)))))

(defmethod codegen ((insn ir-iastore) context)
  (with-slots (arrayref index value) insn
    (make-instance '<expression>
                   :insn insn
                   :code `(let ((value ,(code (codegen value context)))
                                (index ,(code (codegen index context)))
                                (arrayref ,(code (codegen arrayref context))))
                            (setf (jaref arrayref index) value)))))

(defmethod codegen ((insn ir-lastore) context)
  (with-slots (arrayref index value) insn
    (make-instance '<expression>
                   :insn insn
                   :code `(let ((value ,(code (codegen value context)))
                                (index ,(code (codegen index context)))
                                (arrayref ,(code (codegen arrayref context))))
                            (setf (jaref arrayref index) value)))))

(defmethod codegen ((insn ir-fastore) context)
  (with-slots (arrayref index value) insn
    (make-instance '<expression>
                   :insn insn
                   :code `(let ((value ,(code (codegen value context)))
                                (index ,(code (codegen index context)))
                                (arrayref ,(code (codegen arrayref context))))
                            (setf (jaref arrayref index) value)))))

(defmethod codegen ((insn ir-sastore) context)
  (with-slots (arrayref index value) insn
    (make-instance '<expression>
                   :insn insn
                   :code `(let ((value ,(code (codegen value context)))
                                (index ,(code (codegen index context)))
                                (arrayref ,(code (codegen arrayref context))))
                            (setf (jaref arrayref index) value)))))

(defmethod codegen ((insn ir-bastore) context)
  (with-slots (arrayref index value) insn
    (make-instance '<expression>
                   :insn insn
                   :code `(let ((value ,(code (codegen value context)))
                                (index ,(code (codegen index context)))
                                (arrayref ,(code (codegen arrayref context))))
                            (setf (jaref arrayref index) value)))))

(defmethod codegen ((insn ir-dastore) context)
  (with-slots (arrayref index value) insn
    (make-instance '<expression>
                   :insn insn
                   :code `(let ((value ,(code (codegen value context)))
                                (index ,(code (codegen index context)))
                                (arrayref ,(code (codegen arrayref context))))
                            (setf (jaref arrayref index) value)))))

(defmethod codegen ((insn ir-idiv) context)
  ;; FIXME - handle all weird conditions
  (make-instance '<expression>
                 :insn insn
                 :code `(handler-case
                            (let ((value2 ,(code (codegen (value2 insn) context)))
                                  (value1 ,(code (codegen (value1 insn) context))))
                              (unsigned-to-signed-integer (logand (floor (/ value1 value2)) #xFFFFFFFF)))
                          (division-by-zero ()
                            (error (%lisp-condition (%make-throwable '|java/lang/ArithmeticException|)))))
                 :expression-type :INTEGER))

(defmethod codegen ((insn ir-ldiv) context)
  ;; FIXME - handle all weird conditions
  (make-instance '<expression>
                 :insn insn
                 :code `(handler-case
                            (let ((value2 ,(code (codegen (value2 insn) context)))
                                  (value1 ,(code (codegen (value1 insn) context))))
                              (unsigned-to-signed-long (logand (floor (/ value1 value2)) #xFFFFFFFFFFFFFFFF)))
                          (division-by-zero ()
                            (error (%lisp-condition (%make-throwable '|java/lang/ArithmeticException|)))))
                 :expression-type :LONG))

(defun %codegen-binop (insn operator jtype context)
  (make-instance '<expression>
                 :insn insn
                 :code (list 'let (list (list 'value2 (code (codegen (value2 insn) context)))
                                        (list 'value1 (code (codegen (value1 insn) context))))
                             (list operator 'value1 'value2))
                 :expression-type jtype))

(defun %codegen-integer-binop (insn operator context)
  (make-instance '<expression>
                 :insn insn
                 :code (list 'let* (list (list 'value2 (code (codegen (value2 insn) context)))
                                         (list 'value1 (code (codegen (value1 insn) context)))
                                         (list 'result (list 'logand (list operator 'value1 'value2) #xFFFFFFFF))
                                         (list 'sresult (list 'if (list '> 'result 2147483647)
                                                              (list '- 'result 4294967296)
                                                              'result)))
                             ;; (list 'format t "~&~A: ~A ~A ~A = ~A" (list 'quote operator) 'value1 (list 'quote operator) 'value2 'sresult)
                             'sresult)
                 :expression-type :INTEGER))

(defun %codegen-long-binop (insn operator context)
  (make-instance '<expression>
                 :insn insn
                 :code (list 'let* (list (list 'value2 (code (codegen (value2 insn) context)))
                                         (list 'value1 (code (codegen (value1 insn) context)))
                                         (list 'result (list 'logand (list operator 'value1 'value2) #xFFFFFFFFFFFFFFFF))
                                         (list 'sresult (list 'if (list '> 'result 9223372036854775807)
                                                              (list '- 'result 18446744073709551616)
                                                              'result)))
                             ;; (list 'format t "~&~A: ~A ~A ~A = ~A" (list 'quote operator) 'value1 (list 'quote operator) 'value2 'sresult)
                             'sresult)
                 :expression-type :LONG))

(defmacro %define-binop-codegen-methods (&rest opcodes)
  `(progn
     ,@(mapcar (lambda (opcode)
                 (let ((ir-class (car opcode))
                       (operator (cadr opcode))
                       (jtype (caddr opcode)))
                   (cond
                     ((eq jtype :INTEGER)
                      `(defmethod codegen ((insn ,ir-class) context)
                         (%codegen-integer-binop insn ,operator context)))
                     ((eq jtype :LONG)
                      `(defmethod codegen ((insn ,ir-class) context)
                         (%codegen-long-binop insn ,operator context)))
                     (t
                      `(defmethod codegen ((insn ,ir-class) context)
                         (%codegen-binop insn ,operator ,jtype context))))))
               opcodes)))

(%define-binop-codegen-methods
  (ir-dmul '* :DOUBLE nil)
  (ir-dadd '+ :DOUBLE nil)
  (ir-dsub '- :DOUBLE nil)
  (ir-fadd '+ :FLOAT nil)
  (ir-fdiv '/ :FLOAT nil)
  (ir-fmul '* :FLOAT nil)
  (ir-fsub '- :FLOAT nil)
  (ir-iadd '+ :INTEGER #xFFFFFFFF)
  (ir-imul '* :INTEGER #xFFFFFFFF)
  (ir-isub '- :INTEGER #xFFFFFFFF)
  (ir-ladd '+ :LONG #xFFFFFFFFFFFFFFFF)
  (ir-lmul '* :LONG #xFFFFFFFFFFFFFFFF)
  (ir-lsub '- :LONG #xFFFFFFFFFFFFFFFF))

(defmethod codegen ((insn ir-ixor) context)
  (make-instance '<expression>
                 :insn insn
                 :code (list 'logxor (code (codegen (value1 insn) context)) (code (codegen (value2 insn) context)))
                 :expression-type :INTEGER))

(defmethod codegen ((insn ir-lxor) context)
  (make-instance '<expression>
                 :insn insn
                 :code (list 'logxor (code (codegen (value1 insn) context)) (code (codegen (value2 insn) context)))
                 :expression-type :LONG))

(defmethod codegen ((insn ir-ior) context)
  (make-instance '<expression>
                 :insn insn
                 :code (list 'logior (code (codegen (value1 insn) context)) (code (codegen (value2 insn) context)))
                 :expression-type :INTEGER))

(defmethod codegen ((insn ir-lor) context)
  (make-instance '<expression>
                 :insn insn
                 :code `(let ((op1 (logand ,(code (codegen (value1 insn) context)) #xFFFFFFFFFFFFFFFF))
                              (op2 (logand ,(code (codegen (value2 insn) context)) #xFFFFFFFFFFFFFFFF)))
                          ;; (format t "~&~A | ~A = ~A~%" op1 op2 (logior op1 op2))
                          (logior op1 op2))
                 :expression-type :LONG))

(defmethod codegen ((insn ir-iand) context)
  (make-instance '<expression>
                 :insn insn
                 :code (list 'logand (code (codegen (value1 insn) context)) (code (codegen (value2 insn) context)))
                 :expression-type :INTEGER))

(defmethod codegen ((insn ir-land) context)
  (make-instance '<expression>
                 :insn insn
                 :code `(let ((op1 ,(code (codegen (value1 insn) context)))
                              (op2 ,(code (codegen (value2 insn) context))))
                          ;; (format t "~%land: ~A & ~A = ~A~%" op1 op2 (logand op1 op2))
                          (logand op1 op2))
                 :expression-type :LONG))

(defmethod codegen ((insn ir-array-length) context)
  (make-instance '<expression>
                 :insn insn
                 :code `(let ((arrayref ,(code (codegen (slot-value insn 'arrayref) context))))
                          (if arrayref
                              (if (stringp arrayref) (length arrayref) (java-array-length arrayref))
                              (error (%lisp-condition (%make-throwable '|java/lang/NullPointerException|)))))
                 :expression-type :INTEGER))

(defmethod codegen ((insn ir-assign) context)
  (with-slots (lvalue rvalue) insn
   (make-instance '<expression>
                   :insn insn
                   :code (list 'setf (code (codegen lvalue context)) (code (codegen rvalue context))))))

(defmethod codegen ((insn ir-call-dynamic) context)
  (with-slots (class) insn
    (make-instance '<expression>
                   :insn insn
                   :code `(let ((lookup (%make-java-instance "java/lang/invoke/MethodHandles$Lookup")))
                            (|<init>(Ljava/lang/Class;)| lookup ,class)
                            (print lookup)
                            (error "unimplemented")))))

(defun %find-declaring-class (class method-name &optional loader)
  "Find the class that declares METHOD-NAME, searching class hierarchy.
   LOADER is the <ldk-class-loader> to use for class lookups."
  (let* ((ldk-class (%get-ldk-class-by-bin-name class t loader))
         (method (when ldk-class
                   (find method-name (methods ldk-class)
                         :test (lambda (method-name method)
                                 (string= method-name
                                          (lispize-method-name
                                           (format nil "~A~A" (name method) (descriptor method)))))))))
    (if method
        class
        (when ldk-class
          (find method-name (remove nil (cons (super ldk-class) (coerce (interfaces ldk-class) 'list)))
                :test (lambda (method-name class)
                        (%find-declaring-class class method-name loader)))))))

(defmethod codegen ((insn ir-call-static-method) context)
  (with-slots (class method-name args return-type) insn
    (make-instance '<expression>
                   :insn insn
                   :code (let* ((loader (slot-value context 'ldk-loader))
                                ;; Ensure class is loaded before package lookup
                                (_ (classload class))
                                (declaring-class (or (%find-declaring-class class method-name loader) class))
                                (pkg (class-package declaring-class loader))
                                (full-name (format nil "~A.~A" declaring-class method-name))
                                ;; Use static-method-symbol to check :openldk first (for native methods)
                                (method-sym (static-method-symbol full-name pkg))
                                (_ (when (search "shiftLeft" full-name)
                                     (format t "~&; DEBUG codegen static-call: class=~A method=~A loader=~A pkg=~A sym=~A sym-pkg=~A~%"
                                             declaring-class method-name loader pkg method-sym (symbol-package method-sym))
                                     (force-output)))
                                (nargs (length args))
                                (call (cond
                                        ((eq nargs 0)
                                         (list method-sym))
                                        ((eq nargs 1)
                                         (list method-sym (code (codegen (car args) context))))
                                        (t
                                         (list 'apply
                                               (list 'function method-sym)
                                               (list 'reverse (cons 'list (mapcar (lambda (a) (code (codegen a context))) args))))))))
                           call)
                   :expression-type return-type)))

(defmethod codegen ((insn ir-caload) context)
  (with-slots (index arrayref) insn
    (make-instance '<expression>
                   :insn insn
                   :code `(let ((index ,(code (codegen index context)))
                                (arrayref ,(code (codegen arrayref context))))
                            (let ((c (jaref arrayref index)))
                              (typecase c
                                (character (char-code c))
                                (integer c))))
                   :expression-type :CHAR)))

(defmethod codegen ((insn ir-iaload) context)
  (with-slots (index arrayref) insn
    (make-instance '<expression>
                   :insn insn
                   :code `(let ((index ,(code (codegen index context)))
                                (arrayref ,(code (codegen arrayref context))))
                            (jaref arrayref index))
                   :expression-type :INTEGER)))

(defmethod codegen ((insn ir-saload) context)
  (with-slots (index arrayref) insn
    (make-instance '<expression>
                   :insn insn
                   :code `(let ((index ,(code (codegen index context)))
                                (arrayref ,(code (codegen arrayref context))))
                            (jaref arrayref index))
                   :expression-type :SHORT)))

(defmethod codegen ((insn ir-laload) context)
  (with-slots (index arrayref) insn
    (make-instance '<expression>
                   :insn insn
                   :code `(let ((index ,(code (codegen index context)))
                                (arrayref ,(code (codegen arrayref context))))
                            (jaref arrayref index))
                   :expression-type :LONG)))

(defmethod codegen ((insn ir-baload) context)
  (with-slots (index arrayref) insn
    (make-instance '<expression>
                   :insn insn
                   :code `(let ((index ,(code (codegen index context)))
                                (arrayref ,(code (codegen arrayref context))))
                            (jaref arrayref index))
                   :expression-type :BYTE)))

(defmethod codegen ((insn ir-daload) context)
  (with-slots (index arrayref) insn
    (make-instance '<expression>
                   :insn insn
                   :code `(let ((index ,(code (codegen index context)))
                                (arrayref ,(code (codegen arrayref context))))
                            (jaref arrayref index))
                   :expression-type :DOUBLE)))

(defmethod codegen ((insn ir-faload) context)
  (with-slots (index arrayref) insn
    (make-instance '<expression>
                   :insn insn
                   :code `(let ((index ,(code (codegen index context)))
                                (arrayref ,(code (codegen arrayref context))))
                            (jaref arrayref index))
                   :expression-type :FLOAT)))

(defmethod codegen ((insn ir-aaload) context)
  (with-slots (index arrayref) insn
    (make-instance '<expression>
                   :insn insn
                   :code `(let ((index ,(code (codegen index context)))
                                (arrayref ,(code (codegen arrayref context))))
                            (jaref arrayref index))
                   :expression-type :REFERENCE)))

(defmethod codegen ((insn ir-castore) context)
  ;;; FIXME: throw nullpointerexception and invalid array index exception if needed
  (with-slots (arrayref index value) insn
    (make-instance '<expression>
                   :insn insn
                   :code `(let ((value ,(code (codegen value context)))
                                (index ,(code (codegen index context)))
                                (arrayref ,(code (codegen arrayref context))))
;;                            (format t "~&castore[~A] = ~A in ~A~%" index (code-char value) arrayref)
                            (setf (jaref arrayref index) (code-char value))))))

(defmethod codegen ((insn ir-checkcast) context)
  (with-slots (classname) insn
    ;; Ensure class is loaded before package lookup (unless it's an array type)
    (unless (eq (char classname 0) #\[)
      (classload classname))
    (make-instance '<expression>
                   :insn insn
                   :code (progn
                           (if (eq (char classname 0) #\[)
                             `(let ((objref ,(code (codegen (objref insn) context))))
                                (when objref
                                  (unless (and (typep objref 'java-array)
                                               (|isAssignableFrom(Ljava/lang/Class;)|
                                                (java-array-component-class objref)
                                                (%bin-type-name-to-class ,(subseq classname 1))))
                                    (error (%lisp-condition (%make-throwable '|java/lang/ClassCastException|))))))
                             `(let ((objref ,(code (codegen (objref insn) context))))
                                (when objref
                                  (unless (typep objref (quote ,(intern (slot-value insn 'classname) (class-package classname))))
                                    (error (%lisp-condition (%make-throwable '|java/lang/ClassCastException|))))))))
                   :expression-type nil)))

(defmethod codegen ((insn ir-class) context)
  (let* ((classname (slot-value (slot-value insn 'class) 'name))
         (loader (slot-value context 'ldk-loader)))
    (let ((expr (make-instance '<expression>
                               :insn insn
                               :code (java-class (%get-ldk-class-by-bin-name classname t loader))
                               :expression-type :REFERENCE)))
      expr)))

(defmethod codegen ((insn ir-branch-target) context)
  (declare (ignore context))
  (let ((expr (make-instance '<expression>
                             :insn insn
                             :code (intern (format nil "branch-target-~A" (slot-value insn 'index)))
                             :expression-type nil)))
    expr))

(defmethod codegen ((insn ir-irem) context)
  (make-instance '<expression>
                 :insn insn
                 :code (list 'handler-case
                             (list 'let (list (list 'value2 (code (codegen (value2 insn) context)))
                                              (list 'value1 (code (codegen (value1 insn) context))))
                                   (list 'rem 'value1 'value2))
                             (list 'division-by-zero ()
                                   (list 'error (list '%lisp-condition (list '%make-throwable (list 'quote '|java/lang/ArithmeticException|))))))
                 :expression-type :INTEGER))

(defmethod codegen ((insn ir-lrem) context)
  (make-instance '<expression>
                 :insn insn
                 :code (list 'handler-case
                             (list 'let (list (list 'value2 (code (codegen (value2 insn) context)))
                                              (list 'value1 (code (codegen (value1 insn) context))))
                                   (list 'rem 'value1 'value2))
                             (list 'division-by-zero ()
                                   (list 'error (list '%lisp-condition (list '%make-throwable (list 'quote '|java/lang/ArithmeticException|))))))
                 :expression-type :LONG))

(defmethod codegen ((insn ir-fdiv) context)
  ;; FIXME - handle all weird conditions
  (make-instance '<expression>
                 :insn insn
                 :code `(let ((value2 ,(code (codegen (value2 insn) context)))
                              (value1 ,(code (codegen (value1 insn) context))))
                          (if (eq value2 0.0)
                              (cond
                                ((< value1 0.0) float-features:single-float-negative-infinity)
                                ((> value1 0.0) float-features:single-float-positive-infinity)
                                (t float-features:single-float-nan))
                              (/ value1 value2)))
                 :expression-type :FLOAT))

(defmethod codegen ((insn ir-ddiv) context)
  ;; FIXME - handle all weird conditions
  (make-instance '<expression>
                 :insn insn
                 :code `(let ((value2 ,(code (codegen (value2 insn) context)))
                              (value1 ,(code (codegen (value1 insn) context))))
                          (if (eq value2 0.0d0)
                              (cond
                                ((< value1 0.0d0) float-features:double-float-negative-infinity)
                                ((> value1 0.0d0) float-features:double-float-positive-infinity)
                                (t float-features:double-float-nan))
                              (/ value1 value2)))
                 :expression-type :DOUBLE))

(defmethod codegen ((insn ir-frem) context)
  (make-instance '<expression>
                 :insn insn
                 :code `(let ((value2 ,(code (codegen (value2 insn) context)))
                              (value1 ,(code (codegen (value1 insn) context))))
                          (cond
                            ;; If either value1 or value2 is NaN, the result is NaN
                            ((or (float-features:float-nan-p value1)
                                 (float-features:float-nan-p value2))
                             float-features:single-float-nan)

                            ;; If the dividend is an infinity or the divisor is a zero or both, the result is NaN
                            ((or (float-features:float-infinity-p value1)
                                 (eq 0.0 value2)
                                 (and (float-features:float-infinity-p value1)
                                      (eq 0.0 value2)))
                             float-features:single-float-nan)

                            ;; If the dividend is finite and the divisor is an infinity, the result equals the dividend
                            ((and (not (float-features:float-infinity-p value1))
                                  (float-features:float-infinity-p value2))
                             value1)

                            ;; If the dividend is a zero and the divisor is finite, the result equals the dividend
                            ((and (eq 0.0 value1)
                                  (not (float-features:float-infinity-p value2)))
                             value1)

                            ;; In the remaining cases, compute the remainder
                            (t (let ((q (truncate value1 value2)))
                                 (- value1 (* value2 q))))))
                 :expression-type :FLOAT))

(defmethod codegen ((insn ir-drem) context)
  (make-instance '<expression>
                 :insn insn
                 :code `(let ((value2 ,(code (codegen (value2 insn) context)))
                              (value1 ,(code (codegen (value1 insn) context))))
                          (cond
                            ;; If either value1 or value2 is NaN, the result is NaN
                            ((or (float-features:float-nan-p value1)
                                 (float-features:float-nan-p value2))
                             float-features:double-float-nan)

                            ;; If the dividend is an infinity or the divisor is a zero or both, the result is NaN
                            ((or (float-features:float-infinity-p value1)
                                 (eq 0.0d0 value2)
                                 (and (float-features:float-infinity-p value1)
                                      (eq 0.0d0 value2)))
                             float-features:double-float-nan)

                            ;; If the dividend is finite and the divisor is an infinity, the result equals the dividend
                            ((and (not (float-features:float-infinity-p value1))
                                  (float-features:float-infinity-p value2))
                             value1)

                            ;; If the dividend is a zero and the divisor is finite, the result equals the dividend
                            ((and (eq 0.0d0 value1)
                                  (not (float-features:float-infinity-p value2)))
                             value1)

                            ;; In the remaining cases, compute the remainder
                            (t (let ((q (truncate value1 value2)))
                                 (- value1 (* value2 q))))))
                 :expression-type :DOUBLE))

(defmethod codegen ((insn ir-fcmpg) context)
  (make-instance '<expression>
                 :insn insn
                 :code (list 'let (list (list 'value2 (code (codegen (value2 insn) context)))
                                        (list 'value1 (code (codegen (value1 insn) context))))
                             (list 'if (list 'or (list 'float-features:float-nan-p 'value1) (list 'float-features:float-nan-p 'value2))
                                   1
                                   (list 'if (list '> 'value1 'value2)
                                         1
                                         (list 'if (list '< 'value1 'value2)
                                               -1
                                               0))))
                 :expression-type :INTEGER))

(defmethod codegen ((insn ir-dcmpg) context)
  (make-instance '<expression>
                 :insn insn
                 :code (list 'let (list (list 'value2 (code (codegen (value2 insn) context)))
                                        (list 'value1 (code (codegen (value1 insn) context))))
                             (list 'if (list 'or (list 'float-features:float-nan-p 'value1) (list 'float-features:float-nan-p 'value2))
                                   1
                                   (list 'if (list '> 'value1 'value2)
                                         1
                                         (list 'if (list '< 'value1 'value2)
                                               -1
                                               0))))
                 :expression-type :INTEGER))

(defmethod codegen ((insn ir-fcmpl) context)
  (make-instance '<expression>
                 :insn insn
                 :code (list 'let (list (list 'value2 (code (codegen (value2 insn) context)))
                                        (list 'value1 (code (codegen (value1 insn) context))))
                             (list 'if (list 'or (list 'float-features:float-nan-p 'value1) (list 'float-features:float-nan-p 'value2))
                                   -1
                                   (list 'if (list '> 'value1 'value2)
                                         1
                                         (list 'if (list '< 'value1 'value2)
                                               -1
                                               0))))
                 :expression-type :INTEGER))

(defmethod codegen ((insn ir-dcmpl) context)
  (make-instance '<expression>
                 :insn insn
                 :code `(let ((value2 ,(code (codegen (value2 insn) context)))
                              (value1 ,(code (codegen (value1 insn) context))))
                          (if (or (float-features:float-nan-p value1) (float-features:float-nan-p value2))
                              -1
                              (if (> value1 value2)
                                  1
                                  (if (< value1 value2)
                                      -1
                                      0))))
                 :expression-type :INTEGER))

(defmethod codegen ((insn ir-i2f) context)
  (make-instance '<expression>
                 :insn insn
                 :code (list 'float (code (codegen (value insn) context)))
                 :expression-type :FLOAT))

(defmethod codegen ((insn ir-i2d) context)
  (make-instance '<expression>
                 :insn insn
                 :code `(float ,(code (codegen (value insn) context)))
                 :expression-type :DOUBLE))

(defmethod codegen ((insn ir-l2d) context)
  (make-instance '<expression>
                 :insn insn
                 :code `(float ,(code (codegen (value insn) context)))
                 :expression-type :DOUBLE))

(defmethod codegen ((insn ir-f2d) context)
  (make-instance '<expression>
                 :insn insn
                 :code (code (codegen (value insn) context))
                 :expression-type :DOUBLE))

(defmethod codegen ((insn ir-d2f) context)
  (make-instance '<expression>
                 :insn insn
                 :code (code (codegen (value insn) context))
                 :expression-type :FLOAT))

(defmethod codegen ((insn ir-d2i) context)
  (make-instance '<expression>
                 :insn insn
                 :code `(let ((value ,(code (codegen (value insn) context))))
                          (unsigned-to-signed-integer
                           (logand
                            (cond
                              ((float-features:float-nan-p value) 0)
                              ((float-features:float-infinity-p value)
                               (if (plusp value) #x7FFFFFFF #x80000000))
                              (t (floor value)))
                            #xFFFFFFFF)))
                 :expression-type :INTEGER))

(defmethod codegen ((insn ir-d2l) context)
  (make-instance '<expression>
                 :insn insn
                 :code `(let ((value ,(code (codegen (value insn) context))))
                          (unsigned-to-signed-long
                           (logand
                            (cond
                              ((float-features:float-nan-p value) 0)
                              ((float-features:float-infinity-p value)
                               (if (plusp value) #x7FFFFFFFFFFFFFFF #x8000000000000000))
                              (t (floor value)))
                            #xFFFFFFFFFFFFFFFF)))
                 :expression-type :LONG))

(defmethod codegen ((insn ir-l2f) context)
  (make-instance '<expression>
                 :insn insn
                 :code (list 'float (code (codegen (value insn) context)))
                 :expression-type :FLOAT))

(defmethod codegen ((insn ir-lneg) context)
  (make-instance '<expression>
                 :insn insn
                 :code `(progn
                          (let ((value ,(code (codegen (value insn) context))))
                            ;; (format t "~&lneg ~A = ~A~%" value (unsigned-to-signed-long (- value)))
                            (unsigned-to-signed-long (- value))))
                 :expression-type :LONG))

(defmethod codegen ((insn ir-ineg) context)
  (make-instance '<expression>
                 :insn insn
                 :code `(unsigned-to-signed-integer (- ,(code (codegen (value insn) context))))
                 :expression-type :INTEGER))

(defmethod codegen ((insn ir-dneg) context)
  (make-instance '<expression>
                 :insn insn
                 :code `(- ,(code (codegen (value insn) context)))
                 :expression-type :DOUBLE))

(defmethod codegen ((insn ir-fneg) context)
  (make-instance '<expression>
                 :insn insn
                 :code `(- ,(code (codegen (value insn) context)))
                 :expression-type :FLOAT))

(defmethod codegen ((insn ir-i2l) context)
  (make-instance '<expression>
                 :insn insn
                 :code (code (codegen (value insn) context))
                 :expression-type :LONG))

(defmethod codegen ((insn ir-i2b) context)
  ;; FIXME - char width, also maybe use :INTEGER?
  (make-instance '<expression>
                 :insn insn
                 :code (list 'unsigned-to-signed-byte (list 'logand (code (codegen (value insn) context)) #xFF))
                 :expression-type :BYTE))

(defmethod codegen ((insn ir-i2s) context)
  ;; FIXME - maybe use :INTEGER?
  (make-instance '<expression>
                 :insn insn
                 :code (list 'unsigned-to-signed-short (list 'logand (code (codegen (value insn) context)) #xFFFF))
                 :expression-type :SHORT))

(defmethod codegen ((insn ir-l2i) context)
  ;; FIXME - review this
  (make-instance '<expression>
                 :insn insn
                 :code (list 'unsigned-to-signed-integer (list 'logand (code (codegen (value insn) context)) #xFFFFFFFF))
                 :expression-type :INTEGER))

(defmethod codegen ((insn ir-i2c) context)
  ;; FIXME - char width, also maybe use :INTEGER?
  (make-instance '<expression>
                 :insn insn
                 :code (list 'logand (code (codegen (value insn) context)) #xFFFF)
                 :expression-type :CHAR))

(defmethod codegen ((insn ir-f2i) context)
  (make-instance '<expression>
                 :insn insn
                 :code `(let ((value ,(code (codegen (value insn) context))))
                          (unsigned-to-signed-integer
                           (logand
                            (cond
                              ((float-features:float-nan-p value) 0)
                              ((float-features:float-infinity-p value)
                               (if (plusp value) #x7FFFFFFF #x80000000))
                              (t (floor value)))
                            #xFFFFFFFF)))
                 :expression-type :INTEGER))

(defmethod codegen ((insn ir-f2l) context)
  (make-instance '<expression>
                 :insn insn
                 :code `(let ((value ,(code (codegen (value insn) context))))
                          (unsigned-to-signed-long
                           (logand
                            (cond
                              ((float-features:float-nan-p value) 0)
                              ((float-features:float-infinity-p value)
                               (if (plusp value) #x7FFFFFFFFFFFFFFF #x8000000000000000))
                              (t (floor value)))
                            #xFFFFFFFFFFFFFFFF)))
                 :expression-type :LONG))

(defmethod codegen ((insn ir-iinc) context)
  ;; FIXME: don't increment above width of type
  ;; Local variables use :openldk package - they don't need per-loader isolation
  (with-slots (index const) insn
     (let ((expr (make-instance '<expression>
                                :insn insn
                                :code (list 'incf (intern (format nil "local-~A" index) :openldk) const))))
       expr)))

(defmethod codegen ((insn ir-if-acmpeq) context)
  (with-slots (offset value1 value2) insn
    (make-instance '<expression>
                   :insn insn
                   :code (list 'when (list 'eq (code (codegen value1 context)) (code (codegen value2 context)))
                               (list 'go (intern (format nil "branch-target-~A" offset)))))))

(defun %codegen-ir-if-xcmpne (insn context)
  (with-slots (offset value1 value2) insn
    (make-instance '<expression>
                   :insn insn
                   :code (list 'when (list 'not (list 'eq (code (codegen value1 context)) (code (codegen value2 context))))
                               (list 'go (intern (format nil "branch-target-~A" offset)))))))

(defmethod codegen ((insn ir-if-icmpne) context)
  (%codegen-ir-if-xcmpne insn context))

(defmethod codegen ((insn ir-if-acmpne) context)
  (%codegen-ir-if-xcmpne insn context))

(defmacro %define-if-icmp<cond>-codegen-methods (&rest opcodes)
  `(progn
     ,@(mapcar (lambda (opcode)
                 (let ((ir-class (car opcode))
                       (comparison (cadr opcode)))
                   `(defmethod codegen ((insn ,ir-class) context)
                      (with-slots (offset value1 value2) insn
                        (make-instance '<expression>
                                       :insn insn
                                       :code (list 'progn
                                                   (list 'when (list ',comparison (code (codegen value1 context)) (code (codegen value2 context)))
                                                         (list 'go (intern (format nil "branch-target-~A" offset))))))))))
               opcodes)))

(%define-if-icmp<cond>-codegen-methods
  (ir-if-icmpeq eq)
  (ir-if-icmpge >=)
  (ir-if-icmpgt >)
  (ir-if-icmple <=)
  (ir-if-icmplt <))

(defmacro %define-if<cond>-codegen-methods (&rest opcodes)
  `(progn
     ,@(mapcar (lambda (opcode)
                 (let ((ir-class (car opcode))
                       (comparison (cadr opcode)))
                   `(defmethod codegen ((insn ,ir-class) context)
                      (with-slots (offset value) insn
                        (make-instance '<expression>
                                       :insn insn
                                       :code (list 'when (list ',comparison (code (codegen value context)) 0)
                                                   (list 'go (intern (format nil "branch-target-~A" offset)))))))))
               opcodes)))

(%define-if<cond>-codegen-methods
 (ir-ifeq eq)
 (ir-ifge >=)
 (ir-ifgt >)
 (ir-ifle <=)
 (ir-iflt <))

(defmethod codegen ((insn ir-ifnull) context)
  (with-slots (offset value) insn
    (make-instance '<expression>
                   :insn insn
                   :code (list 'when (list 'null (code (codegen value context)))
                               (list 'go (intern (format nil "branch-target-~A" offset)))))))

(defmethod codegen ((insn ir-ifne) context)
  (with-slots (offset value) insn
    (make-instance '<expression>
                   :insn insn
                   :code (list 'when (list 'not (list 'eq (code (codegen value context)) 0))
                               (list 'go (intern (format nil "branch-target-~A" offset)))))))

(defmethod codegen ((insn ir-condition-exception) context)
  (declare (ignore context))
  ;; condition-cache and objref are runtime symbols in :openldk
  (make-instance '<expression>
                 :insn insn
                 :code (list 'slot-value '|condition-cache| (list 'quote 'openldk::|objref|))))

(defmethod codegen ((insn ir-ifnonnull) context)
  (with-slots (offset value) insn
    (make-instance '<expression>
                   :insn insn
                   :code (list 'when (list 'not (list 'null (code (codegen value context))))
                               (list 'go (intern (format nil "branch-target-~A" offset)))))))

(defun %instanceof-array (objref typename)
  ;; FIXME - this isn't following any of the array instanceof rules
  (if (typep objref 'java-array)
      1
      0))

;; Helper function for instanceof check
(defun %instanceof-check (obj target-class-name target-class)
  "Check instanceof. Returns 1 or 0."
  (declare (ignore target-class-name))
  (if (typep obj target-class) 1 0))

;; Helper for integer instanceof check
(defun %instanceof-integer-check (obj target-class-name target-class)
  "Check instanceof for integer types - handles native Lisp integers."
  (declare (ignore target-class-name))
  (cond
    ;; Plain Lisp integer - treat as matching all integral types
    ((integerp obj) 1)
    ;; CLOS instances - use typep
    (t (if (typep obj target-class) 1 0))))

(defmethod codegen ((insn ir-instanceof) context)
  (with-slots (class objref) insn
    (make-instance '<expression>
                   :insn insn
                   :code (let* ((cname (name (slot-value (slot-value insn 'class) 'class)))
                                ;; Ensure class is loaded before package lookup (unless array type)
                                (_ (unless (eq (char cname 0) #\[) (classload cname)))
                                (pkg (class-package cname))
                                (obj (code (codegen objref context))))
                           (cond
                             ;; Array checks
                             ((eq (char cname 0) #\[)
                              `(%instanceof-array ,obj ,cname))
                             ;; Treat native Lisp integers as instances of Java integral wrappers.
                             ((member cname '("java/lang/Integer" "java/lang/Long"
                                              "java/lang/Short" "java/lang/Byte")
                                      :test #'string=)
                              `(%instanceof-integer-check ,obj ,cname (quote ,(intern cname pkg))))
                             (t
                              `(%instanceof-check ,obj ,cname (quote ,(intern cname pkg))))))
                   :expression-type :INTEGER)))

;; Utility functions for signed shifts
(defun %int-to-signed (n)
  "Convert a 32-bit unsigned integer to signed representation"
  (if (logbitp 31 n)
      (- n #x100000000)
      n))

(defun %long-to-signed (n)
  "Convert a 64-bit unsigned integer to signed representation"
  (if (logbitp 63 n)
      (- n #x10000000000000000)
      n))

;; Fixed codegen methods
(defmethod codegen ((insn ir-ishl) context)
  (make-instance '<expression>
                 :insn insn
                 :code `(let* ((int-value (logand ,(code (codegen (value1 insn) context)) #xFFFFFFFF))
                               (shift-amount (logand ,(code (codegen (value2 insn) context)) #x1F))
                               (result (logand (ash int-value shift-amount) #xFFFFFFFF)))
                          (%int-to-signed result))
                 :expression-type :INTEGER))

(defmethod codegen ((insn ir-ishr) context)
  (make-instance '<expression>
                 :insn insn
                 :code `(let* ((int-value (logand ,(code (codegen (value1 insn) context)) #xFFFFFFFF))
                               (shift-amount (logand ,(code (codegen (value2 insn) context)) #x1F)))
                          ;; Special case for -1
                          (if (= int-value #xFFFFFFFF)
                              -1
                              (let ((sign-bit (logbitp 31 int-value)))
                                (%int-to-signed
                                 (logand
                                  (if sign-bit
                                      (logior (ash int-value (- shift-amount))
                                              (ash (lognot 0) (- 32 shift-amount)))
                                      (ash int-value (- shift-amount)))
                                  #xFFFFFFFF)))))
                 :expression-type :INTEGER))

(defmethod codegen ((insn ir-iushr) context)
  (make-instance '<expression>
                 :insn insn
                 :code `(let ((shift-amount (logand ,(code (codegen (value2 insn) context)) #x1F)))
                          (%int-to-signed (logand (ash (logand ,(code (codegen (value1 insn) context)) #xFFFFFFFF) (- shift-amount)) #xFFFFFFFF)))
                 :expression-type :INTEGER))

(defmethod codegen ((insn ir-lshl) context)
  (make-instance '<expression>
                 :insn insn
                 :code `(let* ((long-value (logand ,(code (codegen (value1 insn) context)) #xFFFFFFFFFFFFFFFF))
                               (shift-amount (logand ,(code (codegen (value2 insn) context)) #x3F))
                               (result (logand (ash long-value shift-amount) #xFFFFFFFFFFFFFFFF)))
                          (%long-to-signed result))
                 :expression-type :LONG))

(defmethod codegen ((insn ir-lshr) context)
  (make-instance '<expression>
                 :insn insn
                 :code `(let* ((long-value (logand ,(code (codegen (value1 insn) context)) #xFFFFFFFFFFFFFFFF))
                               (shift-amount (logand ,(code (codegen (value2 insn) context)) #x3F)))
                          ;; Special case for -1
                          (if (= long-value #xFFFFFFFFFFFFFFFF)
                              -1
                              (let ((sign-bit (logbitp 63 long-value)))
                                (%long-to-signed
                                 (logand
                                  (if sign-bit
                                      (logior (ash long-value (- shift-amount))
                                              (ash (lognot 0) (- 64 shift-amount)))
                                      (ash long-value (- shift-amount)))
                                  #xFFFFFFFFFFFFFFFF)))))
                 :expression-type :LONG))

(defmethod codegen ((insn ir-lushr) context)
  (make-instance '<expression>
                 :insn insn
                 :code `(let ((shift-amount (logand ,(code (codegen (value2 insn) context)) #x3F)))
                          (%long-to-signed (logand (ash (logand ,(code (codegen (value1 insn) context)) #xFFFFFFFFFFFFFFFF)
                                                       (- shift-amount))
                                                  #xFFFFFFFFFFFFFFFF)))
                 :expression-type :LONG))

(defmethod codegen ((insn ir-lcmp) context)
  (make-instance '<expression>
                 :insn insn
                 :code (list 'let (list (list 'value2 (code (codegen (value2 insn) context)))
                                        (list 'value1 (code (codegen (value1 insn) context))))
                             (list 'cond
                                   (list (list '= 'value1 'value2)
                                         0)
                                   (list (list '> 'value1 'value2)
                                         1)
                                   (list 't
                                         -1)))
                 :expression-type :INTEGER))

(defmethod codegen ((insn ir-goto) context)
  (declare (ignore context))
  (with-slots (offset) insn
    (let ((expr (make-instance '<expression>
                               :insn insn
                               :code (list 'go (intern (format nil "branch-target-~A" offset))))))
      expr)))

(defmethod codegen ((insn ir-tableswitch) context)
  (declare (ignore context))
  (with-slots (default-offset low high jump-offsets) insn
    (let ((cases (loop for index from low to high
                       for offset in jump-offsets
                       collect (list index (list 'go (intern (format nil "branch-target-~A" offset))))))
          (default-target (list 'go (intern (format nil "branch-target-~A" default-offset)))))
      (make-instance '<expression>
                     :insn insn
                     :code (append (list 'case (code (codegen (index insn) context)))
                                   cases
                                   (list `(otherwise ,default-target)))))))

(defmethod codegen ((insn ir-lookupswitch) context)
  (declare (ignore context))
  (with-slots (default-offset match-offset-pairs) insn
    (let ((cases (loop for (match . offset) in match-offset-pairs
                       collect (list match (list 'go (intern (format nil "branch-target-~A" offset))))))
          (default-target (list 'go (intern (format nil "branch-target-~A" default-offset)))))
      (make-instance '<expression>
                     :insn insn
                     :code (append (list 'case (code (codegen (index insn) context)))
                                   cases
                                   (list `(otherwise ,default-target)))))))

(defmethod codegen ((insn ir-call-virtual-method) context)
  (with-slots (method-name args) insn
    ;; Virtual method dispatch uses :openldk package for generic function names
    ;; Instance methods are defined as CLOS generic functions in :openldk for cross-loader dispatch
    (make-instance '<expression>
                   :insn insn
                   :code (let* ((nargs (length args))
                                (call (cond
                                        ((eq nargs 0)
                                         (error "internal error"))
                                        ((eq nargs 1)
                                         (list (intern (format nil "~A" method-name) :openldk) (code (codegen (car args) context))))
                                        (t
                                         `(funcall (function ,(intern (format nil "~A" method-name) :openldk))
                                                   ,@(mapcar (lambda (a) (code (codegen a context))) args))))))
                           call))))

(defparameter *invokedynamic-cache* (make-hash-table :test #'equal))

(defun %resolve-invokedynamic (method-name bootstrap-method-name address fname &rest args)
  (format t "~&%resolve-invokedynamic: method=~A bootstrap=~A~%" method-name bootstrap-method-name)
  (force-output)
  (let* ((key (list bootstrap-method-name address))
         (cached (gethash key *invokedynamic-cache*)))
    (if cached
        (progn
          (format t "  Using cached CallSite~%")
          cached)
        (progn
          (format t "  Calling bootstrap method...~%")
          (force-output)
          (let ((resolved (apply bootstrap-method-name
                                 (append (list (|java/lang/invoke/MethodHandles.lookup()|) fname)
                                         args))))
            (format t "  Bootstrap returned: ~A~%" resolved)
            (setf (gethash key *invokedynamic-cache*) resolved)
            resolved)))))

(defmethod codegen ((insn ir-call-dynamic-method) context)
  (with-slots (method-name args dynamic-args bootstrap-method-name address) insn
    (let ((constant-pool (constant-pool (<context>-class context)))
          (pkg (context-package context)))
      (make-instance '<expression>
                     :insn insn
                     :code
                     `(let* ((fname ,(jstring method-name))
                             (bootstrap-name (symbol-name ',(intern bootstrap-method-name pkg))))
                        (if (starts-with? "java/lang/invoke/LambdaMetafactory.metafactory" bootstrap-name)
                            ;; Fast path for Java 8 lambdas: build the functional object directly
                            (openldk::%lambda-metafactory
                             ,(code (codegen (third args) context))
                             (list ,@(mapcar (lambda (a) (code (codegen a context))) dynamic-args))
                             ,method-name)
                            ;; Fallback: generic invokedynamic handling
                            (let ((callsite (%resolve-invokedynamic ',(intern method-name pkg)
                                                                    ',(intern bootstrap-method-name pkg)
                                                                    ,address
                                                                    fname
                                                                    ,@(mapcar (lambda (a) (code (codegen a context))) args))))
                              (format t "~&Getting target from CallSite...~%")
                              (force-output)
                              (let ((target (|getTarget()| callsite)))
                                (format t "~&Got target: ~A~%" target)
                                (format t "~&Creating args array for invokeWithArguments...~%")
                                (force-output)
                                (let ((args-array (make-java-array :component-class (%get-java-class-by-bin-name "java/lang/Object")
                                                                   :initial-contents (list ,@(mapcar (lambda (a) (code (codegen a context))) dynamic-args)))))
                                  (format t "~&Args array: ~A (length=~A)~%" args-array (java-array-length args-array))
                                  (format t "~&Calling invokeWithArguments...~%")
                                  (force-output)
                                  (|invokeWithArguments([Ljava/lang/Object;)| target args-array))))))))))

(defmethod codegen ((insn ir-clinit) context)
  (with-slots (class) insn
    (make-instance '<expression>
                   :insn insn
                   :code (let* ((class (ir-class-class class))
                                (pkg (class-package (slot-value class 'name))))
                           (list 'unless (list 'initialized-p class)
                                 (list (intern (format nil "%clinit-~A" (slot-value class 'name)) pkg)))))))

(defmethod codegen ((insn ir-local-variable) context)
  (with-slots (index) insn
    ;; FIXME: track type of local vars
    ;; Local variables use :openldk package - they don't need per-loader isolation
    (let ((expr (make-instance '<expression>
                               :insn insn
                               :code (intern (format nil "local-~A" index) :openldk))))
      expr)))

(defmethod codegen ((insn ir-long-local-variable) context)
  (with-slots (index) insn
    ;; Local variables use :openldk package - they don't need per-loader isolation
    (let ((expr (make-instance '<expression>
                               :insn insn
                               :code (intern (format nil "local-~A" index) :openldk)
                               :expression-type :LONG)))
      expr)))

(defmethod codegen ((insn ir-monitorenter) context)
  (make-instance '<expression>
                 :insn insn
                 :code `(monitor-enter ,(code (codegen (slot-value insn 'objref) context)))))

(defmethod codegen ((insn ir-monitorexit) context)
  (make-instance '<expression>
                 :insn insn
                 :code `(monitor-exit ,(code (codegen (slot-value insn 'objref) context)))))

(defmethod codegen ((insn ir-new) context)
  (declare (ignore context))
  (with-slots (class) insn
    (with-slots (class) class
      ;; Ensure class is loaded before package lookup
      (let* ((classname (slot-value class 'name))
             (_ (classload classname))
             (pkg (class-package classname)))
        (make-instance '<expression>
                       :insn insn
                       :code `(let* ((obj (make-instance ',(intern classname pkg)))
                                     (klass ,(java-class class)))
                                ;; Ensure clazz slot is populated for instanceof/reflection
                                (when (and klass (slot-exists-p obj '|clazz|))
                                  (setf (slot-value obj '|clazz|) klass))
                                obj)
                       :expression-type :REFERENCE)))))

(defmethod codegen ((insn ir-new-array) context)
  (let ((init-element
          (case (atype insn)
            ;; Determine the initial element based on the array type
            (4 0)        ; Integer
            (5 #\Null)   ; Character
            (6 0.0)      ; Single-precision float
            (7 0.0d0)    ; Double-precision float
            ((8 9 10 11) 0) ; Other integer types (assuming default to 0)
            (t nil))))   ; Default to nil for unknown types
    (make-instance '<expression>
                   :insn insn
                   :code `(progn
                            ;; Create the array with the determined initial element
                            (make-java-array :size ,(code (codegen (size insn) context))
                                             :component-class ,(component-class insn)
                                             :initial-element ,init-element))
                   :expression-type :ARRAY)))

(defun %make-multi-array (dimensions)
  (if (null dimensions)
      nil
      (let ((size (car dimensions)))
        ;; Check for negative array size
        (when (< size 0)
          (let ((exc (%make-java-instance "java/lang/NegativeArraySizeException")))
            (|<init>()| exc)
            (error (%lisp-condition exc))))
        (make-java-array :size size
                         :component-class :multi-array-placeholder
                         :initial-contents
                         (loop repeat size
                               collect (%make-multi-array (cdr dimensions)))))))

(defmethod codegen ((insn ir-multi-new-array) context)
  (let ((init-element
          (case (atype insn)
            ;; Determine the initial element based on the array type
            (4 0)        ; Integer
            (5 #\Null)      ; Character
            (6 0.0)      ; Single-precision float
            (7 0.0d0)    ; Double-precision float
            ((8 9 10 11) 0) ; Other integer types (assuming default to 0)
            (t nil))))   ; Default to nil for unknown types
    (make-instance '<expression>
                   :insn insn
                   :code `(progn
                            ;; Create the multi-dimensional array with the determined initial element
                            ;; Reverse sizes because bytecode pops dimensions in reverse order
                            (%make-multi-array (list ,@(mapcar (lambda (c) (code (codegen c context))) (reverse (sizes insn))))))
                   :expression-type :ARRAY)))

(defmethod codegen ((insn ir-nop) context)
  (declare (ignore context))
  nil)

(defmethod codegen ((insn ir-stop-marker) context)
  (declare (ignore context))
  (make-instance '<expression>
                 :insn insn
                 :code (list 'return-from 'try-body nil)))

(defmethod codegen ((insn ir-call-special-method) context)
  (with-slots (class method-name args) insn
    (let* ((class-name (slot-value class 'name))
           ;; Ensure class is loaded before package lookup
           (_ (classload class-name))
           (pkg (class-package class-name))
           ;; Method symbol goes in :openldk - constructors are generic functions
           ;; shared across loaders for CLOS dispatch, like instance methods
           (method-symbol (intern (format nil "~A" method-name) :openldk))
           ;; Owner class symbol goes in the class's package
           (owner-symbol (intern class-name pkg))
           (arg-code (mapcar (lambda (a) (code (codegen a context))) args)))
      (make-instance '<expression>
                     :insn insn
                     :code `(openldk::invoke-special ',method-symbol ',owner-symbol
                                                     (list ,@arg-code))))))

(defmethod codegen ((insn ir-member) context)
  (with-slots (objref member-name) insn
    ;; Field names use context package since they're accessed on objects
    (let ((pkg (context-package context)))
      (make-instance '<expression>
                     :insn insn
                     :code `(slot-value
                             (let ((objref ,(code (codegen objref context))))
                               (when (null objref)
                                 (error (format nil "Null Pointer Exception ~A" ,(slot-value insn 'address))))
                               objref)
                             ;; Field names stay in :openldk for CLOS slot inheritance
                             (quote ,(intern (mangle-field-name member-name) :openldk)))))))

(defmethod codegen ((insn ir-static-member) context)
  (declare (ignore context))
  (with-slots (class member-name) insn
    (let ((class-name (slot-value (slot-value class 'class) 'name)))
      ;; Ensure class is loaded before looking up its package
      ;; This is necessary because the class might be in a different loader's package
      (classload class-name)
      (let ((pkg (class-package class-name)))
        (make-instance '<expression>
                       :insn insn
                       :code `(slot-value
                               ,(intern (format nil "+static-~A+" class-name) pkg)
                               ;; Field names stay in :openldk for CLOS slot inheritance
                               (quote ,(intern (mangle-field-name member-name) :openldk))))))))

(define-condition java-lang-throwable (error)
  ((throwable :initarg :throwable :reader throwable)))

(defun make-java-condition (e)
  (make-condition (gethash (class-of e) *condition-table*) :objref e))

(defmethod codegen ((insn ir-throw) context)
  (make-instance '<expression>
                 :insn insn
                 :code `(let ((c (%lisp-condition ,(code (codegen (slot-value insn 'objref) context)))))
                          (setf |condition-cache| c)
                          (error c))))

(defmethod codegen ((insn ir-return) context)
  (declare (ignore context))
  (make-instance '<expression>
                 :insn insn
                 :code `(return)))

(defmethod codegen ((insn ir-return-value) context)
  ;; Static methods have format "class/name.method()" with "." before "("
  ;; Instance methods have format "method(Ljava/lang/Object;)" - "/" only in descriptors
  ;; Static methods use loader's package, instance methods use :openldk (generic functions)
  (let* ((fn-name (slot-value insn 'fn-name))
         (fn-pkg (if (find #\. fn-name)
                     (context-package context)
                     (find-package :openldk))))
    (make-instance '<expression>
                   :insn insn
                   :code `(let ((result ,(code (codegen (slot-value insn 'value) context))))
                            (cond
                              (*debug-trace-args*
                               (format t "~&~V@A <~A> trace: ~A result = ~A~%"
                                       *call-nesting-level* "*" *call-nesting-level*
                                       ,(fn-name *context*) result))
                              (*debug-trace*
                               (format t "~&~V@A <~A> trace: ~A~%"
                                       *call-nesting-level* "*" *call-nesting-level* ,(fn-name *context*))))
                            (return-from ,(intern fn-name fn-pkg) result)))))

(defvar *current-block* nil
  "Dynamic variable holding the current <basic-block> during codegen.
Used to consult block-local substitutions in addition to global ones.")

(defmethod codegen ((insn <stack-variable>) context)
  ;; First check global substitutions, then block-local
  ;; This order prevents issues with local variables that may not exist in final code
  (let ((v (gethash insn (single-assignment-table context))))
    ;; Only check block-local if not in global table
    (unless v
      (when (and *current-block*
                 (slot-boundp *current-block* 'local-substitutions))
        (let ((local-subs (slot-value *current-block* 'local-substitutions)))
          (when local-subs
            (setf v (gethash insn local-subs))))))
    (if v
        (codegen v context)
        ;; Stack variables use :openldk package - they don't need per-loader isolation
        (make-instance '<expression>
                       :insn insn
                       :code (intern (format nil "s{~{~A~^,~}}" (sort (copy-list (slot-value insn 'var-numbers)) #'<)) :openldk)))))

(defmethod codegen-block ((basic-block <basic-block>) dominator-block)
  "Generate Lisp code for a basic block, handling exception scopes and control flow."

  (let ((new-scope nil))
    ;;  (when (or handler-start (fset:contains? (dominators basic-block) dominator-block))
    (when (fset:contains? (dominators basic-block) dominator-block)
      (unless (find basic-block (car (emitted-block-scopes *context*)))
        (when (try-catch basic-block)
          (push basic-block (first (emitted-block-scopes *context*)))
          (push (list) (emitted-block-scopes *context*)))
        (let* ((stop-emitting-blocks? nil)
               (lisp-code
                 ;; Bind *current-block* so codegen can access local substitutions
                 (let ((*current-block* basic-block))
                   (cons (intern (format nil "branch-target-~A" (address (car (slot-value basic-block 'code)))))
                         (loop for insn in (slot-value basic-block 'code)
                               do (when (and *debug-codegen* (slot-value insn 'dead-p))
                                    (format t "; Skipping dead instruction: ~A~%" (type-of insn)))
                               when (not (slot-value insn 'dead-p))  ; Skip dead instructions
                                 append (let ((expr (codegen insn *context*)))
                                          (when (typep insn 'ir-stop-marker)
                                            (setf stop-emitting-blocks? t))
                                          (if expr
                                              (list (trace-insn insn (code expr)))
                                              nil)))))))
          (push basic-block (first (emitted-block-scopes *context*)))
          (pop (slot-value *context* 'blocks))

          (unless (end-of-handler? basic-block)

            ;; Emit code for successors if not stopping
            (unless stop-emitting-blocks?
              (when (fall-through-address basic-block)
#|
                (when (try-catch basic-block)
                  (push (list) (emitted-block-scopes *context*))
                (setf new-scope t))
|#
                (setf lisp-code
                      (nconc lisp-code
                             (if (or (find (fall-through-address basic-block) (car (emitted-block-scopes *context*)))
                                     (gethash (address (fall-through-address basic-block)) (try-end-table *context*)))
                                 (list (list 'go (intern (format nil "branch-target-~A" (address (car (code (fall-through-address basic-block))))))))
                                 (codegen-block
                                  (fall-through-address basic-block)
                                  (if (try-catch basic-block) basic-block dominator-block))))))
#|
              (when (and (not new-scope) (try-catch basic-block))
                (push (list) (emitted-block-scopes *context*))
                (setf new-scope t))
|#
              (let ((successor-list (sort (fset:convert 'list (successors basic-block))
                                          (lambda (a b) (< (address a) (address b))))))
                (dolist (successor successor-list)
                  (unless (gethash (address successor) (try-end-table *context*))
                    (setf lisp-code (nconc lisp-code (codegen-block successor (if (try-catch basic-block) basic-block dominator-block)))))))))

          ;; Handle exception handlers (try-catch)
          (let ((try-catch-handlers (try-catch basic-block))
                (ctx-pkg (context-package *context*)))
            (when try-catch-handlers
              (pop (emitted-block-scopes *context*))
              ;; Wrap the block's code in HANDLER-CASE
              ;; Pull any branch target out of the HANDLER-CASE first.
              (setf lisp-code
                    (if (and lisp-code
                             (starts-with? "branch-target-" (format nil "~A" (car lisp-code))))
                        (let ((bt (car lisp-code))
                              (lisp-code (cdr lisp-code)))
                          `(,bt
                            (HANDLER-CASE
                                (BLOCK TRY-BODY
                                  (TAGBODY ,@lisp-code))
                              ;; Condition symbols always in :openldk for cross-loader catching
                              ,@(loop for (exception-type . handler-block) in try-catch-handlers
                                      when (> (length exception-type) 0)
                                        do (classload exception-type)
                                      collect `(,(intern (format nil "condition-~A" (or exception-type
                                                                                        "java/lang/Throwable")) :openldk)
                                                (|condition|)
                                                (setf |condition-cache| |condition|)
                                                (go ,(intern (format nil "branch-target-~A" (address handler-block)))))))))
                        `((HANDLER-CASE
                              (BLOCK TRY-BODY
                                (TAGBODY ,@lisp-code))
                            ;; Condition symbols always in :openldk for cross-loader catching
                            ,@(loop for (exception-type . handler-block) in try-catch-handlers
                                    do (classload exception-type)
                                    collect `(,(intern (format nil "condition-~A" (or exception-type
                                                                                      "java/lang/Throwable")) :openldk)
                                              (|condition|)
                                              (setf |condition-cache| |condition|)
                                              (go ,(intern (format nil "branch-target-~A" (address handler-block))))))))))))
          lisp-code)))))
