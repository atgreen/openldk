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

;;; Lower the IR produced by bc-to-ir.lisp to Lisp forms.  Each IR node
;;; class has a CODEGEN method returning an <EXPRESSION> whose CODE slot
;;; is the Lisp form to evaluate; codegen-block stitches these together
;;; per basic block, wrapping try/catch scopes as HANDLER-CASE.

(defgeneric codegen (insn context)
  (:documentation "Return an <EXPRESSION> containing the Lisp form that
implements IR node INSN, generating operand code recursively."))

(declaim (notinline %java-slot-value (setf %java-slot-value)))

(defun %java-slot-value (object slot-name)
  "Read a dynamically defined Java field without baking in an SBCL global slot accessor."
  (slot-value object slot-name))

(defun (setf %java-slot-value) (value object slot-name)
  "Write a dynamically defined Java field without baking in an SBCL global slot accessor."
  (setf (slot-value object slot-name) value))

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
  (with-slots (arrayref index value) insn
    (make-instance '<expression>
                   :insn insn
                   :code `(let ((value ,(code (codegen value context)))
                                (index ,(code (codegen index context)))
                                (arrayref ,(code (codegen arrayref context))))
                            (%aastore arrayref index value)))))

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
  ;; Java integer division truncates toward zero (not floor toward -infinity)
  (make-instance '<expression>
                 :insn insn
                 :code `(handler-case
                            (let ((value2 ,(code (codegen (value2 insn) context)))
                                  (value1 ,(code (codegen (value1 insn) context))))
                              (unsigned-to-signed-integer (logand (truncate value1 value2) #xFFFFFFFF)))
                          (division-by-zero ()
                            (error (%lisp-condition (%make-throwable '|java/lang/ArithmeticException|)))))
                 :expression-type :INTEGER))

(defmethod codegen ((insn ir-ldiv) context)
  ;; Java long division truncates toward zero (not floor toward -infinity)
  (make-instance '<expression>
                 :insn insn
                 :code `(handler-case
                            (let ((value2 ,(code (codegen (value2 insn) context)))
                                  (value1 ,(code (codegen (value1 insn) context))))
                              (unsigned-to-signed-long (logand (truncate value1 value2) #xFFFFFFFFFFFFFFFF)))
                          (division-by-zero ()
                            (error (%lisp-condition (%make-throwable '|java/lang/ArithmeticException|)))))
                 :expression-type :LONG))

(defun %codegen-binop (insn operator jtype context)
  "Codegen a plain two-operand op: (OPERATOR value1 value2), typed JTYPE."
  (make-instance '<expression>
                 :insn insn
                 :code (list 'let (list (list 'value2 (code (codegen (value2 insn) context)))
                                        (list 'value1 (code (codegen (value1 insn) context))))
                             (list operator 'value1 'value2))
                 :expression-type jtype))

(defun %codegen-integer-binop (insn operator context)
  "Codegen an int op with Java 32-bit wrap-around overflow semantics."
  (make-instance '<expression>
                 :insn insn
                 :code (list 'let* (list (list 'value2 (code (codegen (value2 insn) context)))
                                         (list 'value1 (code (codegen (value1 insn) context)))
                                         (list 'result (list 'logand (list operator 'value1 'value2) #xFFFFFFFF))
                                         (list 'sresult (list 'if (list '> 'result 2147483647)
                                                              (list '- 'result 4294967296)
                                                              'result)))
                             'sresult)
                 :expression-type :INTEGER))

(defun %codegen-long-binop (insn operator context)
  "Codegen a long op with Java 64-bit wrap-around overflow semantics."
  (make-instance '<expression>
                 :insn insn
                 :code (list 'let* (list (list 'value2 (code (codegen (value2 insn) context)))
                                         (list 'value1 (code (codegen (value1 insn) context)))
                                         (list 'result (list 'logand (list operator 'value1 'value2) #xFFFFFFFFFFFFFFFF))
                                         (list 'sresult (list 'if (list '> 'result 9223372036854775807)
                                                              (list '- 'result 18446744073709551616)
                                                              'result)))
                             'sresult)
                 :expression-type :LONG))

(defmacro %define-binop-codegen-methods (&rest opcodes)
  "Define CODEGEN methods for two-operand arithmetic and logical opcodes.
Each entry is (IR-CLASS OPERATOR JTYPE KIND) where KIND is :WRAPPING for
ops that must wrap around on 32/64-bit overflow, or :PLAIN for ops whose
result always stays in range (bitwise ops, floats, doubles)."
  `(progn
     ,@(mapcar (lambda (opcode)
                 (destructuring-bind (ir-class operator jtype kind) opcode
                   (if (eq kind :WRAPPING)
                       (ecase jtype
                         (:INTEGER
                          `(defmethod codegen ((insn ,ir-class) context)
                             (%codegen-integer-binop insn ,operator context)))
                         (:LONG
                          `(defmethod codegen ((insn ,ir-class) context)
                             (%codegen-long-binop insn ,operator context))))
                       `(defmethod codegen ((insn ,ir-class) context)
                          (%codegen-binop insn ,operator ,jtype context)))))
               opcodes)))

(%define-binop-codegen-methods
  (ir-dmul '* :DOUBLE :PLAIN)
  (ir-dadd '+ :DOUBLE :PLAIN)
  (ir-dsub '- :DOUBLE :PLAIN)
  (ir-fadd '+ :FLOAT :PLAIN)
  (ir-fdiv '/ :FLOAT :PLAIN)
  (ir-fmul '* :FLOAT :PLAIN)
  (ir-fsub '- :FLOAT :PLAIN)
  (ir-iadd '+ :INTEGER :WRAPPING)
  (ir-imul '* :INTEGER :WRAPPING)
  (ir-isub '- :INTEGER :WRAPPING)
  (ir-ladd '+ :LONG :WRAPPING)
  (ir-lmul '* :LONG :WRAPPING)
  (ir-lsub '- :LONG :WRAPPING)
  (ir-iand 'logand :INTEGER :PLAIN)
  (ir-ior 'logior :INTEGER :PLAIN)
  (ir-ixor 'logxor :INTEGER :PLAIN)
  (ir-land 'logand :LONG :PLAIN)
  (ir-lxor 'logxor :LONG :PLAIN))

(defmethod codegen ((insn ir-lor) context)
  ;; Unlike LAND/LXOR, LOR masks its operands to their unsigned 64-bit
  ;; representation before combining; some producers hand it non-canonical
  ;; (unsigned) long values.
  (make-instance '<expression>
                 :insn insn
                 :code `(let ((op1 (logand ,(code (codegen (value1 insn) context)) #xFFFFFFFFFFFFFFFF))
                              (op2 (logand ,(code (codegen (value2 insn) context)) #xFFFFFFFFFFFFFFFF)))
                          (logior op1 op2))
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
                            (unimplemented "IR-CALL-DYNAMIC codegen")))))

(defun %class-method-name-table (ldk-class)
  "Hash set of LDK-CLASS's own methods' lispized name+descriptor
strings, built on first use.  A class's method vector never changes
after load, so the table is cached on the <class> object."
  (or (method-name-table ldk-class)
      (setf (method-name-table ldk-class)
            (let ((table (make-hash-table :test #'equal)))
              (map nil (lambda (method)
                         (setf (gethash (lispize-method-name
                                         (format nil "~A~A" (name method) (descriptor method)))
                                        table)
                                t))
                   (or (methods ldk-class) #()))
              table))))

(defun %find-field-declaring-class (class field-name &optional loader)
  "Bin-name of the class/interface that actually declares FIELD-NAME,
searching CLASS then its superclass and superinterfaces.  A static field
access initializes only the declaring class (JLS 12.4.1), not the class
named in the reference."
  (let ((ldk-class (%get-ldk-class-by-bin-name class t loader)))
    (when ldk-class
      (if (some (lambda (f) (and f (string= (name f) field-name)))
                (coerce (fields ldk-class) 'list))
          class
          (loop for parent in (remove nil (cons (super ldk-class)
                                                (coerce (interfaces ldk-class) 'list)))
                for result = (%find-field-declaring-class parent field-name loader)
                when result return result)))))

(defun %field-declaring-ir-class (ref-ir-class fieldname)
  "ir-class for the class that DECLARES FIELDNAME, searching up from the
referenced class REF-IR-CLASS.  Falls back to REF-IR-CLASS when it declares
the field itself or the declarer can't be resolved.  Used so a static field
access initializes the declaring class, not the referenced one."
  (let* ((ref-name (name (ir-class-class ref-ir-class)))
         (decl-name (%find-field-declaring-class ref-name fieldname)))
    (if (and decl-name (not (string= decl-name ref-name)))
        (let ((lc (classload decl-name)))
          (if lc (make-instance 'ir-class :class lc) ref-ir-class))
        ref-ir-class)))

(defun %declares-default-method-p (ldk-class)
  "True when LDK-CLASS is an interface declaring a default method (a
non-abstract, non-static instance method).  Per JLS 12.4.2, initializing a
class/interface initializes only those superinterfaces that declare a
default method -- not every superinterface."
  (and (interface-p ldk-class)
       (some (lambda (m)
               (and m (not (abstract-p m)) (not (static-p m))
                    (let ((n (name m)))
                      (not (or (string= n "<clinit>") (string= n "<init>"))))))
             (coerce (methods ldk-class) 'list))))

(defun %find-declaring-class (class method-name &optional loader)
  "Find the class that declares METHOD-NAME, searching class hierarchy.
   LOADER is the <ldk-class-loader> to use for class lookups."
  (let ((ldk-class (%get-ldk-class-by-bin-name class t loader)))
    (when ldk-class
      (if (gethash method-name (%class-method-name-table ldk-class))
          class
          (loop for parent in (remove nil (cons (super ldk-class) (coerce (interfaces ldk-class) 'list)))
                for result = (%find-declaring-class parent method-name loader)
                when result return result)))))

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
                                (nargs (length args))
                                (call (cond
                                        ((eq nargs 0)
                                         (list method-sym))
                                        ((eq nargs 1)
                                         (list method-sym (code (codegen (car args) context))))
                                        (t
                                         (cons method-sym
                                               (reverse (mapcar (lambda (a) (code (codegen a context))) args)))))))
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
                                  (unless (%array-assignable-to-p objref ,classname)
                                    (error (%lisp-condition (%make-throwable '|java/lang/ClassCastException|))))))
                             `(let ((objref ,(code (codegen (objref insn) context))))
                                (when objref
                                  (unless (or (not (find-class (quote ,(intern (slot-value insn 'classname) (class-package classname))) nil))
                                              (typep objref (quote ,(intern (slot-value insn 'classname) (class-package classname))))
                                              (%native-type-castable-p objref ,classname))
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
                                 (zerop value2)
                                 (and (float-features:float-infinity-p value1)
                                      (zerop value2)))
                             float-features:single-float-nan)

                            ;; If the dividend is finite and the divisor is an infinity, the result equals the dividend
                            ((and (not (float-features:float-infinity-p value1))
                                  (float-features:float-infinity-p value2))
                             value1)

                            ;; If the dividend is a zero and the divisor is finite, the result equals the dividend
                            ((and (zerop value1)
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
                                 (zerop value2)
                                 (and (float-features:float-infinity-p value1)
                                      (zerop value2)))
                             float-features:double-float-nan)

                            ;; If the dividend is finite and the divisor is an infinity, the result equals the dividend
                            ((and (not (float-features:float-infinity-p value1))
                                  (float-features:float-infinity-p value2))
                             value1)

                            ;; If the dividend is a zero and the divisor is finite, the result equals the dividend
                            ((and (zerop value1)
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
                 :code `(coerce ,(code (codegen (value insn) context))
                                'single-float)
                 :expression-type :FLOAT))

(defmethod codegen ((insn ir-i2d) context)
  (make-instance '<expression>
                 :insn insn
                 :code `(coerce ,(code (codegen (value insn) context))
                                'double-float)
                 :expression-type :DOUBLE))

(defmethod codegen ((insn ir-l2d) context)
  (make-instance '<expression>
                 :insn insn
                 :code `(coerce ,(code (codegen (value insn) context))
                                'double-float)
                 :expression-type :DOUBLE))

(defmethod codegen ((insn ir-f2d) context)
  (make-instance '<expression>
                 :insn insn
                 :code `(coerce ,(code (codegen (value insn) context))
                                'double-float)
                 :expression-type :DOUBLE))

(defmethod codegen ((insn ir-d2f) context)
  (make-instance '<expression>
                 :insn insn
                 :code `(coerce ,(code (codegen (value insn) context))
                                'single-float)
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
                 :code `(coerce ,(code (codegen (value insn) context))
                                'single-float)
                 :expression-type :FLOAT))

(defmethod codegen ((insn ir-lneg) context)
  (make-instance '<expression>
                 :insn insn
                 :code `(let ((value ,(code (codegen (value insn) context))))
                          (unsigned-to-signed-long (- value)))
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
  ;; Local variables use :openldk package - they don't need per-loader isolation
  (with-slots (index const) insn
    (let* ((local-var (intern (format nil "local-~A" index) :openldk))
           (expr (make-instance '<expression>
                                :insn insn
                                :code (list 'setf local-var
                                            (list 'unsigned-to-signed-integer
                                                  (list 'logand (list '+ local-var const) #xFFFFFFFF))))))
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

(defun %box-if-native (obj)
  "Auto-box native Lisp values to Java wrapper objects for virtual method dispatch.
   Native integers become java/lang/Long, floats become java/lang/Float, etc."
  (cond
    ((integerp obj)
     (let ((boxed (%make-java-instance "java/lang/Long")))
       (setf (slot-value boxed '|value|) obj)
       boxed))
    ((typep obj 'single-float)
     (let ((boxed (%make-java-instance "java/lang/Float")))
       (setf (slot-value boxed '|value|) obj)
       boxed))
    ((typep obj 'double-float)
     (let ((boxed (%make-java-instance "java/lang/Double")))
       (setf (slot-value boxed '|value|) obj)
       boxed))
    ((characterp obj)
     (let ((boxed (%make-java-instance "java/lang/Character")))
       (setf (slot-value boxed '|value|) obj)
       boxed))
    (t obj)))

(defun %array-assignable-to-p (objref target-descriptor)
  "True when array OBJREF's runtime type is assignable to the array type
named by TARGET-DESCRIPTOR (a JVM array descriptor, e.g. \"[I\", \"[[I\",
or \"[Lp/C;\"), per JLS array covariance/identity: the object's component
type must be assignable to the target's component type.  Primitive
component types must match exactly (short[] is not an int[])."
  (and (typep objref 'java-array)
       (let ((obj-comp (%array-component-class objref))
             (target-comp (%bin-type-name-to-class (subseq target-descriptor 1))))
         (and obj-comp target-comp
              ;; target-comp.isAssignableFrom(obj-comp): obj's component is a
              ;; subtype of (or equal to) the target's component.
              (eql 1 (|isAssignableFrom(Ljava/lang/Class;)| target-comp obj-comp))))))

(defun %instanceof-array (objref typename)
  "instanceof against an array target type TYPENAME."
  (if (%array-assignable-to-p objref typename) 1 0))

;; Check if a native Lisp value is compatible with a Java class type.
(defun %native-type-castable-p (obj classname)
  "Return T if native Lisp value OBJ is compatible with Java CLASSNAME."
  (cond
    ((integerp obj)
     (member classname '("java/lang/Object" "java/lang/Number"
                          "java/lang/Long" "java/lang/Integer"
                          "java/lang/Short" "java/lang/Byte"
                          "java/lang/Comparable" "java/io/Serializable")
             :test #'string=))
    ((typep obj 'single-float)
     (member classname '("java/lang/Object" "java/lang/Number"
                          "java/lang/Float"
                          "java/lang/Comparable" "java/io/Serializable")
             :test #'string=))
    ((typep obj 'double-float)
     (member classname '("java/lang/Object" "java/lang/Number"
                          "java/lang/Double"
                          "java/lang/Comparable" "java/io/Serializable")
             :test #'string=))
    ((characterp obj)
     (member classname '("java/lang/Object" "java/lang/Character"
                          "java/lang/Comparable" "java/io/Serializable")
             :test #'string=))
    (t nil)))

;; Helper function for instanceof check
(defun %instanceof-check (obj target-class-name target-class)
  "Check instanceof. Returns 1 or 0.
   Returns 0 when the target class is not loaded (e.g. AWT classes not on classpath)."
  (cond
    ((%native-type-castable-p obj target-class-name) 1)
    ;; Every array is an instance of Object, Cloneable, and Serializable.
    ((and (typep obj 'java-array)
          (member target-class-name
                  '("java/lang/Object" "java/lang/Cloneable" "java/io/Serializable")
                  :test #'string=))
     1)
    ((not (find-class target-class nil)) 0)
    ((typep obj target-class) 1)
    (t 0)))

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
                                (receiver-code (code (codegen (car args) context)))
                                (receiver-sym (gensym "recv"))
                                (null-checked-receiver
                                  `(let ((,receiver-sym (%box-if-native ,receiver-code)))
                                     (when (null ,receiver-sym)
                                       (error (%lisp-condition
                                               (%make-throwable '|java/lang/NullPointerException|))))
                                     ,receiver-sym))
                                (call (cond
                                        ((eq nargs 0)
                                         (internal-error "virtual call ~A has no receiver argument" method-name))
                                        ((eq nargs 1)
                                         (list (intern (format nil "~A" method-name) :openldk) null-checked-receiver))
                                        (t
                                         `(funcall (function ,(intern (format nil "~A" method-name) :openldk))
                                                   ,null-checked-receiver
                                                   ,@(mapcar (lambda (a) (code (codegen a context))) (cdr args)))))))
                           call))))

(defparameter *invokedynamic-cache* (make-hash-table :test #'equal))

(defun %resolve-invokedynamic (method-name bootstrap-method-name address fname &rest args)
  "Resolve an invokedynamic call site by invoking its bootstrap method.
BOOTSTRAP-METHOD-NAME is the full \"class/name.method(descriptor)\" string of the
bootstrap method (e.g. java/lang/runtime/SwitchBootstraps.typeSwitch(...)). The
bootstrap class is always a boot/library class, so we load it and resolve the
static function in its own defining package rather than the caller's."
  (declare (ignore method-name))
  (let* ((key (list bootstrap-method-name address))
         (cached (gethash key *invokedynamic-cache*)))
    (or cached
        (let* ((paren (position #\( bootstrap-method-name))
               (dot (position #\. bootstrap-method-name :end paren :from-end t))
               (bootstrap-class (subseq bootstrap-method-name 0 dot)))
          (classload bootstrap-class)
          (let* ((pkg (class-package bootstrap-class))
                 (fn-sym (static-method-symbol (lispize-method-name bootstrap-method-name) pkg))
                 (param-types (parse-parameter-types bootstrap-method-name))
                 (nparams (length param-types))
                 (full-args (append (list (|java/lang/invoke/MethodHandles.lookup()|) fname) args))
                 ;; Bootstrap methods are frequently varargs (last parameter an
                 ;; Object[]), e.g. SwitchBootstraps.typeSwitch and
                 ;; ObjectMethods.bootstrap. Collect the trailing arguments into a
                 ;; single Object[] so the fixed-arity Lisp function receives them
                 ;; the way the JVM would after varargs packing.
                 (final-args
                   (if (and (plusp nparams)
                            (let ((last (car (last param-types))))
                              (and (stringp last) (>= (length last) 2)
                                   (string= "[]" (subseq last (- (length last) 2)))))
                            (>= (length full-args) nparams))
                       (let ((fixed (1- nparams)))
                         (append (subseq full-args 0 fixed)
                                 (list (make-java-array
                                        :component-class (%get-java-class-by-bin-name "java/lang/Object")
                                        :initial-contents (coerce (nthcdr fixed full-args) 'vector)))))
                       full-args))
                 (resolved (apply fn-sym final-args)))
            (setf (gethash key *invokedynamic-cache*) resolved)
            resolved)))))

(defun %extract-string-concat-recipe (args)
  "Extract the recipe string from StringConcatFactory bootstrap args.
The recipe is the second element (first bootstrap arg after the MethodType)."
  (let ((recipe-node (second args)))
    (when (typep recipe-node 'ir-string-literal)
      (slot-value recipe-node 'value))))

(defun %descriptor-param-type-chars (descriptor)
  "Return the top-level parameter type chars of a JVM method DESCRIPTOR, in order:
one of Z B S C I J F D for primitives, #\\L for object refs, #\\[ for arrays."
  (when descriptor
    (let ((chars nil)
          (i (1+ (position #\( descriptor)))
          (end (position #\) descriptor)))
      (loop while (< i end)
            do (let ((ch (char descriptor i)))
                 (cond
                   ((find ch "ZBSCIJFD") (push ch chars) (incf i))
                   ((char= ch #\L) (push #\L chars) (setf i (1+ (position #\; descriptor :start i))))
                   ((char= ch #\[) (push #\[ chars)
                    (loop while (char= (char descriptor i) #\[) do (incf i))
                    (if (char= (char descriptor i) #\L)
                        (setf i (1+ (position #\; descriptor :start i)))
                        (incf i)))
                   (t (incf i)))))
      (nreverse chars))))

(defun %string-concat-arg-form (code type-char)
  "Wrap a dynamic string-concat argument CODE so it renders per its Java static
TYPE-CHAR: boolean -> true/false, char -> the character; otherwise via
%to-java-string (which handles numbers, strings, null, and Object.toString)."
  (case type-char
    (#\Z `(if (or (null ,code) (eql ,code 0)) "false" "true"))
    (#\C `(let ((c ,code)) (string (if (characterp c) c (code-char c)))))
    (t `(%to-java-string ,code))))

(defun %generate-string-concat-code (recipe dynamic-arg-codes &optional type-chars)
  "Generate Lisp code for StringConcatFactory.makeConcatWithConstants.
RECIPE is the template string with \\x01 placeholders for dynamic args.
DYNAMIC-ARG-CODES is a list of codegen'd expressions for the dynamic arguments.
TYPE-CHARS, if given, are the call-site parameter type chars (aligned with
DYNAMIC-ARG-CODES) used to render boolean/char args the Java way."
  (let ((parts nil)
        (arg-idx 0)
        (start 0))
    (loop for i below (length recipe)
          for ch = (char recipe i)
          do (cond
               ((char= ch (code-char 1))
                (when (< start i)
                  (push (subseq recipe start i) parts))
                (when (< arg-idx (length dynamic-arg-codes))
                  ;; Pre-format each dynamic arg per its call-site static type so
                  ;; boolean prints true/false and char prints the character.
                  (push (%string-concat-arg-form (nth arg-idx dynamic-arg-codes)
                                                 (nth arg-idx type-chars))
                        parts))
                (incf arg-idx)
                (setf start (1+ i)))
               ((char= ch (code-char 2))
                (when (< start i)
                  (push (subseq recipe start i) parts))
                (setf start (1+ i)))))
    (when (< start (length recipe))
      (push (subseq recipe start (length recipe)) parts))
    (setf parts (nreverse parts))
    ;; PARTS holds string literals and pre-wrapped code forms (each yielding a
    ;; string at runtime), so no further per-part conversion is needed.
    `(jstring (format nil "~{~A~}" (list ,@parts)))))

(defun %to-java-string (val)
  "Convert a value to its Java string representation for StringConcatFactory."
  (cond
    ((null val) "null")
    ((typep val '|java/lang/String|) (lstring val))
    ((integerp val) (format nil "~D" val))
    ((floatp val) (format nil "~F" val))
    ((characterp val) (string val))
    ((typep val '|java/lang/Object|)
     (let ((str (|toString()| val)))
       (if str (lstring str) "null")))
    (t (format nil "~A" val))))

(defun %type-switch-match (target label)
  "True if TARGET matches a single SwitchBootstraps.typeSwitch LABEL, which is a
Class (instanceof), a boxed integral constant, or a String constant."
  (cond
    ((typep label '|java/lang/Class|)
     (= 1 (|isInstance(Ljava/lang/Object;)| label target)))
    ((integerp label)
     (and (typep target '(or |java/lang/Integer| |java/lang/Short|
                          |java/lang/Byte| |java/lang/Character|))
          (slot-boundp target '|value|)
          (eql (slot-value target '|value|) label)))
    ((typep label '|java/lang/String|)
     (and (typep target '|java/lang/String|)
          (string= (lstring target) (lstring label))))
    (t (and target (typep target '|java/lang/Object|)
            (= 1 (|equals(Ljava/lang/Object;)| target label))))))

(defun %type-switch (target restart labels)
  "Native implementation of java.lang.runtime.SwitchBootstraps.typeSwitch's target
method handle: returns the index of the first LABEL (at or after RESTART) that
matches TARGET, LABELS's length if none match, or -1 when TARGET is null."
  (if (null target)
      -1
      (let* ((v (coerce labels 'simple-vector))
             (n (length v)))
        (loop for i from (max restart 0) below n
              when (%type-switch-match target (svref v i))
                do (return-from %type-switch i))
        n)))

(defun %enum-switch-match (target label)
  "True if enum constant TARGET matches an enumSwitch LABEL (an enum constant name
as a String, or an Enum$EnumDesc)."
  (let ((name (cond
                ((typep label '|java/lang/String|) (lstring label))
                ((and (typep label '|java/lang/Object|)
                      (slot-exists-p label '|constantName|)
                      (slot-boundp label '|constantName|))
                 (lstring (slot-value label '|constantName|)))
                (t nil))))
    (and name
         (let ((tn (|name()| target)))
           (and tn (string= (lstring tn) name))))))

(defun %enum-switch (target restart labels)
  "Native implementation of SwitchBootstraps.enumSwitch's target method handle."
  (if (null target)
      -1
      (let* ((v (coerce labels 'simple-vector))
             (n (length v)))
        (loop for i from (max restart 0) below n
              when (%enum-switch-match target (svref v i))
                do (return-from %enum-switch i))
        n)))

(defun %record-component-names (names-jstring)
  "Split a record's ';'-separated component-name string into a list of names."
  (remove "" (uiop:split-string (lstring names-jstring) :separator ";") :test #'string=))

(defun %record-component-value (record name)
  (slot-value record (intern (mangle-field-name name) :openldk)))

(defun %record-component-descriptor (class name)
  "Return the field descriptor string for record component NAME in CLASS, or NIL."
  (let ((lc (get-ldk-class-for-java-class class)))
    (when lc
      (loop for f in (coerce (fields lc) 'list)
            when (string= (name f) name)
              return (descriptor f)))))

(defun %format-record-component (value descriptor)
  "Render a record component VALUE for toString, honouring its DESCRIPTOR so that
boolean prints true/false and char prints the character rather than an int."
  (case (and descriptor (plusp (length descriptor)) (char descriptor 0))
    (#\Z (if (or (null value) (eql value 0)) "false" "true"))
    (#\C (string (if (characterp value) value (code-char value))))
    (t (%to-java-string value))))

(defun %class-simple-name (class)
  "Return the simple (unqualified) name of a Class object."
  (let* ((n (lstring (slot-value class '|name|)))
         (dot (position #\. n :from-end t))
         (n (if dot (subseq n (1+ dot)) n))
         (dollar (position #\$ n :from-end t))
         (n (if dollar (subseq n (1+ dollar)) n)))
    ;; Local/anonymous classes have binary names like Outer$1Name; getSimpleName
    ;; strips the compiler-added leading digits (Outer$1Name -> Name, Outer$1 -> "").
    (string-left-trim "0123456789" n)))

(defun %record-to-string (record class names-jstring)
  "Native implementation of a record's ObjectMethods-generated toString()."
  (jstring
   (format nil "~A[~{~A~^, ~}]"
           (%class-simple-name class)
           (loop for name in (%record-component-names names-jstring)
                 collect (format nil "~A=~A" name
                                 (%format-record-component
                                  (%record-component-value record name)
                                  (%record-component-descriptor class name)))))))

(defun %record-field-hash (v)
  (cond ((null v) 0)
        ((typep v '|java/lang/Object|) (or (|hashCode()| v) 0))
        ((integerp v) v)
        ((characterp v) (char-code v))
        (t (sxhash v))))

(defun %record-hash-code (record class names-jstring)
  "Native implementation of a record's ObjectMethods-generated hashCode()."
  (declare (ignore class))
  (let ((h 0))
    (dolist (name (%record-component-names names-jstring))
      (setf h (logand (+ (* 31 h) (%record-field-hash (%record-component-value record name)))
                      #xFFFFFFFF)))
    (if (>= h #x80000000) (- h #x100000000) h)))

(defun %record-field-equal (a b)
  (cond ((and (null a) (null b)) t)
        ((or (null a) (null b)) nil)
        ((typep a '|java/lang/Object|) (= 1 (|equals(Ljava/lang/Object;)| a b)))
        (t (eql a b))))

(defun %record-equals (record other class names-jstring)
  "Native implementation of a record's ObjectMethods-generated equals()."
  (if (and other (= 1 (|isInstance(Ljava/lang/Object;)| class other)))
      (if (every (lambda (name)
                   (%record-field-equal (%record-component-value record name)
                                        (%record-component-value other name)))
                 (%record-component-names names-jstring))
          1 0)
      0))

(defmethod codegen ((insn ir-call-dynamic-method) context)
  (with-slots (method-name args dynamic-args bootstrap-method-name address interface-type-name call-site-descriptor) insn
    (let ((pkg (context-package context)))
      (cond
        ;; Fast path for ObjectMethods.bootstrap (record toString/hashCode/equals).
        ;; The real JDK generates a hidden class; instead we compute directly from
        ;; the record's components. args[1] is the record Class, args[2] is the
        ;; ';'-separated component-name string; dynamic-args are (this) or (this, other).
        ((search "ObjectMethods.bootstrap" bootstrap-method-name)
         (let ((mname (if (stringp method-name) method-name (lstring method-name)))
               (class-code (code (codegen (second args) context)))
               (names-code (code (codegen (third args) context)))
               (this-code (code (codegen (first dynamic-args) context))))
           (cond
             ((string= mname "toString")
              (make-instance '<expression> :insn insn
                             :code `(%record-to-string ,this-code ,class-code ,names-code)
                             :expression-type :REFERENCE))
             ((string= mname "hashCode")
              (make-instance '<expression> :insn insn
                             :code `(%record-hash-code ,this-code ,class-code ,names-code)
                             :expression-type :INTEGER))
             ((string= mname "equals")
              (make-instance '<expression> :insn insn
                             :code `(%record-equals ,this-code
                                                    ,(code (codegen (second dynamic-args) context))
                                                    ,class-code ,names-code)
                             :expression-type :INTEGER))
             (t (unimplemented "ObjectMethods bootstrap method ~A" mname)))))
        ;; Fast path for SwitchBootstraps.typeSwitch / enumSwitch (pattern-matching
        ;; switch). The real JDK generates a hidden class via the ClassFile API;
        ;; instead we compute the case index directly. args[0] is the call-site
        ;; MethodType and (rest args) are the case labels; dynamic-args are
        ;; (selector, restartIndex).
        ((search "SwitchBootstraps.typeSwitch" bootstrap-method-name)
         (make-instance '<expression>
                        :insn insn
                        :code `(%type-switch ,(code (codegen (first dynamic-args) context))
                                             ,(code (codegen (second dynamic-args) context))
                                             (list ,@(mapcar (lambda (a) (code (codegen a context))) (rest args))))
                        :expression-type :INTEGER))
        ((search "SwitchBootstraps.enumSwitch" bootstrap-method-name)
         (make-instance '<expression>
                        :insn insn
                        :code `(%enum-switch ,(code (codegen (first dynamic-args) context))
                                             ,(code (codegen (second dynamic-args) context))
                                             (list ,@(mapcar (lambda (a) (code (codegen a context))) (rest args))))
                        :expression-type :INTEGER))
        ;; Fast path for StringConcatFactory (JDK 9+ string concatenation)
        ((search "StringConcatFactory.makeConcatWithConstants" bootstrap-method-name)
         (let ((recipe (%extract-string-concat-recipe args))
               (type-chars (%descriptor-param-type-chars call-site-descriptor))
               (dyn-codes (mapcar (lambda (a) (code (codegen a context))) dynamic-args)))
           (if recipe
               (make-instance '<expression>
                              :insn insn
                              :code (%generate-string-concat-code recipe dyn-codes type-chars)
                              :expression-type :REFERENCE)
               ;; No recipe: just concatenate the dynamic args in order (still
               ;; honouring boolean/char static types).
               (make-instance '<expression>
                              :insn insn
                              :code `(jstring (format nil "~{~A~}"
                                              (list ,@(loop for c in dyn-codes
                                                            for tc in (append type-chars
                                                                              (make-list (max 0 (- (length dyn-codes)
                                                                                                   (length type-chars)))))
                                                            collect (%string-concat-arg-form c tc)))))
                              :expression-type :REFERENCE))))
        ;; Fast path for LambdaMetafactory
        ((search "LambdaMetafactory.metafactory" bootstrap-method-name)
         (make-instance '<expression>
                        :insn insn
                        :code `(openldk::%lambda-metafactory
                                ,(code (codegen (third args) context))
                                (list ,@(mapcar (lambda (a) (code (codegen a context))) dynamic-args))
                                ,method-name
                                ,(code (codegen (second args) context))
                                ,interface-type-name)))
        ;; Fallback: generic invokedynamic handling
        (t
         (make-instance '<expression>
                        :insn insn
                        :code
                        `(let ((callsite (%resolve-invokedynamic ,method-name
                                                                 ,bootstrap-method-name
                                                                 ,address
                                                                 ,(jstring method-name)
                                                                 ,@(remove nil (mapcar (lambda (a)
                                                                                        (when a (code (codegen a context))))
                                                                                      args)))))
                           (let ((target (|getTarget()| callsite)))
                             (let ((args-array (make-java-array :component-class (%get-java-class-by-bin-name "java/lang/Object")
                                                                :initial-contents (list ,@(mapcar (lambda (a) (code (codegen a context))) dynamic-args)))))
                               (|invokeWithArguments([Ljava/lang/Object;)| target args-array))))))))))

(defmethod codegen ((insn ir-clinit) context)
  (with-slots (class) insn
    (make-instance '<expression>
                   :insn insn
                   :code (let* ((class (ir-class-class class)))
                           (if class
                               (let ((pkg (class-package (slot-value class 'name))))
                                 (list 'unless (list 'initialized-p class)
                                       (list (intern (format nil "%clinit-~A" (slot-value class 'name)) pkg))))
                               nil)))))

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
      (when (null class)
        (return-from codegen
          (make-instance '<expression>
                         :insn insn
                         :code `(error (%lisp-condition
                                        (%make-throwable '|java/lang/NoClassDefFoundError|)))
                         :expression-type :REFERENCE)))
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

(defun %multi-array-leaf-default (component-name)
  "Default element for a leaf array whose component descriptor is
COMPONENT-NAME: the primitive zero/false/NUL, or nil for references."
  (case (char component-name 0)
    (#\D 0.0d0)
    (#\F 0.0)
    ((#\I #\J #\S #\B #\Z) 0)
    (#\C #\Null)
    (t nil)))

(defun %make-multi-array (type-name dimensions)
  "Build a multidimensional array for the JVM array descriptor TYPE-NAME
(slash-separated, e.g. \"[[Ljava/lang/Long;\") allocating the leading
DIMENSIONS.  Each level's component class is TYPE-NAME with one leading
'[' removed, so the true element type is preserved -- without it aastore
covariance checks (ArrayStoreException) can't see the real component
type."
  (if (null dimensions)
      nil
      (let* ((size (car dimensions))
             (component-name (subseq type-name 1))
             (rest-dims (cdr dimensions)))
        ;; Check for negative array size
        (when (< size 0)
          (let ((exc (%make-java-instance "java/lang/NegativeArraySizeException")))
            (|<init>()| exc)
            (error (%lisp-condition exc))))
        (let ((component-class (%bin-type-name-to-class component-name)))
          (if rest-dims
              (make-java-array :size size
                               :component-class component-class
                               :initial-contents
                               (loop repeat size
                                     collect (%make-multi-array component-name rest-dims)))
              (make-java-array :size size
                               :component-class component-class
                               :initial-element (%multi-array-leaf-default component-name)))))))

(defmethod codegen ((insn ir-multi-new-array) context)
  ;; The MULTIANEWARRAY operand is the full array type (e.g. "[[Ljava/lang/Long;");
  ;; pass it so %make-multi-array can set the real component class at each level.
  (let ((type-name (substitute #\/ #\.
                               (lstring (slot-value (java-class (ir-class-class (slot-value insn 'class)))
                                                    '|name|)))))
    (make-instance '<expression>
                   :insn insn
                   :code `(%make-multi-array
                           ,type-name
                           ;; Reverse sizes because bytecode pops dimensions in reverse order
                           (list ,@(mapcar (lambda (c) (code (codegen c context))) (reverse (sizes insn)))))
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
    (when (null class)
      (return-from codegen
        (make-instance '<expression>
                       :insn insn
                       :code `(error (%lisp-condition
                                      (%make-throwable '|java/lang/NoClassDefFoundError|)))
                       :expression-type :REFERENCE)))
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

(defun %resolve-field-slot (ref-class-name member-name)
  "Resolve a Fieldref (REF-CLASS-NAME, MEMBER-NAME) to its CLOS slot
symbol, honoring JVM field resolution: walk up from the referenced class
to the first class declaring the field; a shadowing declaration has its
own class-qualified slot recorded in *FIELD-SHADOW-SLOTS*."
  (let ((plain (intern (mangle-field-name member-name) :openldk))
        ;; The Fieldref's class arrives as whatever EMIT produced for the
        ;; class constant: an IR-CLASS wrapping <CLASS> metadata (or, in
        ;; principle, a bare string) -- normalize to the binary name.
        (ref-class-name (typecase ref-class-name
                          (string ref-class-name)
                          (ir-class (let ((c (ir-class-class ref-class-name)))
                                      (and c (name c))))
                          (<class> (name ref-class-name))
                          (t nil))))
    (if (null ref-class-name)
        plain
        (loop with cname = ref-class-name
              for depth from 0 below 64
              while cname
              do (let ((shadow (gethash (format nil "~A.~A" cname member-name)
                                        *field-shadow-slots*)))
                   (when shadow (return shadow))
                   (let ((cls (gethash cname *ldk-classes-by-bin-name*)))
                     (unless cls (return plain))
                     (when (find member-name (fields cls)
                                 :key (lambda (fl) (slot-value fl 'name))
                                 :test #'string=)
                       (return plain))
                     (setf cname (slot-value cls 'super))))
              finally (return plain)))))

(defmethod codegen ((insn ir-member) context)
  (with-slots (objref member-name ref-class) insn
    (make-instance '<expression>
                   :insn insn
                   :code `(%java-slot-value
                           (let ((objref ,(code (codegen objref context))))
                             (when (null objref)
                               (error (%lisp-condition
                                       (%make-throwable '|java/lang/NullPointerException|))))
                             objref)
                           ;; Field names stay in :openldk for CLOS slot inheritance
                           (quote ,(%resolve-field-slot ref-class member-name))))))

(defmethod codegen ((insn ir-static-member) context)
  (declare (ignore context))
  (with-slots (class member-name) insn
    (let ((class-name (slot-value (slot-value class 'class) 'name)))
      ;; Ensure class is loaded before looking up its package
      ;; This is necessary because the class might be in a different loader's package
      (classload class-name)
      (let* ((pkg (class-package class-name))
             (static-sym-name (format nil "+static-~A+" class-name))
             (sym (intern static-sym-name pkg)))
        (make-instance '<expression>
                       :insn insn
                       :code `(%java-slot-value
                               ,sym
                               ;; Field names stay in :openldk for CLOS slot inheritance
                               (quote ,(intern (mangle-field-name member-name) :openldk))))))))

(defmethod codegen ((insn ir-throw) context)
  (make-instance '<expression>
                 :insn insn
                 :code `(let* ((obj ,(code (codegen (slot-value insn 'objref) context)))
                               (c (%lisp-condition obj)))
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
                     (find-package :openldk)))
         (ret-sym (intern fn-name fn-pkg))
         (value-code (code (codegen (slot-value insn 'value) context))))
    (make-instance '<expression>
                   :insn insn
                   ;; Only emit the per-return trace when a debug-trace flag is set
                   ;; at (JIT) compile time; otherwise a bare return, so production
                   ;; code doesn't pay two special-variable reads on every return.
                   :code (if (or *debug-trace-args* *debug-trace*)
                             `(let ((result ,value-code))
                                (cond
                                  (*debug-trace-args*
                                   (format t "~&~V@A <~A> trace: ~A result = ~A~%"
                                           *call-nesting-level* "*" *call-nesting-level*
                                           ,(fn-name *context*) result))
                                  (*debug-trace*
                                   (format t "~&~V@A <~A> trace: ~A~%"
                                           *call-nesting-level* "*" *call-nesting-level* ,(fn-name *context*))))
                                (return-from ,ret-sym result))
                             `(return-from ,ret-sym ,value-code)))))

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
                (let* ((ft-block (fall-through-address basic-block))
                       (ft-tag (intern (format nil "branch-target-~A" (address (car (code ft-block)))))))
                  (setf lisp-code
                        (nconc lisp-code
                               (if (or (find ft-block (car (emitted-block-scopes *context*)))
                                       (gethash (address ft-block) (try-end-table *context*)))
                                   (list (list 'go ft-tag))
                                   ;; If codegen-block returns NIL (e.g. dominator check fails),
                                   ;; generate a GO to the fall-through block's tag in an enclosing TAGBODY.
                                   (or (codegen-block ft-block (if (try-catch basic-block) basic-block dominator-block))
                                       (list (list 'go ft-tag))))))))
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
              ;; Compute a GO to the try-end block to prevent fall-through
              ;; after the HANDLER-CASE (JVM semantics: normal completion
              ;; of a try block continues at the end-pc).
              (let ((fall-through-go
                      (when-let ((end-block (first (exception-end-blocks basic-block))))
                        (list (list 'go (intern (format nil "branch-target-~A"
                                                        (address (car (code end-block))))))))))
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
                                                  (when *debug-exceptions*
                                                    (format *error-output*
                                                            "~&Caught Java exception in ~A.~A~%"
                                                            ,(slot-value
                                                              (slot-value *context* 'class)
                                                              'name)
                                                            ,(fn-name *context*))
                                                    (%print-java-stack-trace
                                                     (slot-value |condition| '|objref|)
                                                     :stream *error-output*))
                                                  (setf |condition-cache| |condition|)
                                                  (go ,(intern (format nil "branch-target-~A" (address handler-block)))))))
                              ,@fall-through-go))
                          `((HANDLER-CASE
                                (BLOCK TRY-BODY
                                  (TAGBODY ,@lisp-code))
                              ;; Condition symbols always in :openldk for cross-loader catching
                              ,@(loop for (exception-type . handler-block) in try-catch-handlers
                                      do (classload exception-type)
                                      collect `(,(intern (format nil "condition-~A" (or exception-type
                                                                                        "java/lang/Throwable")) :openldk)
                                                (|condition|)
                                                (when *debug-exceptions*
                                                  (format *error-output*
                                                          "~&Caught Java exception in ~A.~A~%"
                                                          ,(slot-value
                                                            (slot-value *context* 'class)
                                                            'name)
                                                          ,(fn-name *context*))
                                                  (%print-java-stack-trace
                                                   (slot-value |condition| '|objref|)
                                                   :stream *error-output*))
                                                (setf |condition-cache| |condition|)
                                                (go ,(intern (format nil "branch-target-~A" (address handler-block)))))))
                            ,@fall-through-go))))))
          lisp-code)))))
