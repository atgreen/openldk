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

(in-package :openldk)

(defclass/std <expression> ()
  ((insn
    code
    expression-type)))

(defmethod print-object ((expr <expression>) out)
  (print-unreadable-object (expr out :type t)
    (format out "{~A : ~A}" (slot-value expr 'insn) (slot-value expr 'code))))

(defun gen-push-item (item)
  (assert (not (eq (type-of item) '<expression>)))
  (if *debug-stack*
      (list 'let (list (list 'item item))
            (list 'format t "; --- push ~A to ~A~%" 'item 'stack)
            (list 'push 'item 'stack))
      (list 'push item 'stack)))

(defun gen-pop-item ()
  (if *debug-stack*
      (list 'progn
            (list 'format t "; -- pop from ~A~%" 'stack)
            (list 'pop 'stack))
      (list 'pop 'stack)))

(defun trace-insn (insn code)
  (if *debug-trace*
      (list 'progn
            (list 'format t (format nil "~&; [~A]~%" (address insn)))
            code)
      code))

(defun gen-peek-item ()
  (list 'car 'stack))

(defmethod codegen ((insn ir-literal) context)
  (declare (ignore context))
  (make-instance '<expression>
                 :insn insn
                 :code (slot-value insn 'value)
                 :expression-type (slot-value insn 'type)))

(defmethod codegen ((insn ir-string-literal) context)
  (declare (ignore context))
  (make-instance '<expression>
                 :insn insn
                 :code (let ((s (make-instance '|java/lang/String|)))
                         (setf (slot-value s '|value|) (slot-value insn 'value))
                         s)
                 :expression-type :REFERENCE))

(defmethod codegen ((insn ir-aaload) context)
  (let ((expr (make-instance '<expression>
                             :insn insn
                             :code (list 'let (list (list 'index (gen-pop-item))
                                                    (list 'arrayref (gen-pop-item)))
                                         (gen-push-item (list 'aref 'arrayref 'index)))
                             :expression-type :REFERENCE)))
    (pop (stack context)) (pop (stack context)) (push expr (stack context))
    expr))

(defmethod codegen ((insn ir-aastore) context)
  (let ((expr (make-instance '<expression>
                             :insn insn
                             :code (list 'let (list (list 'value (gen-pop-item))
                                                    (list 'index (gen-pop-item))
                                                    (list 'arrayref (gen-pop-item)))
                                         (list 'setf (list 'aref 'arrayref 'index) 'value)))))
    (pop (stack context)) (pop (stack context)) (pop (stack context))
    expr))

(defmethod codegen ((insn ir-iadd) context)
  (let ((expr (make-instance '<expression>
                             :insn insn
                             :code (gen-push-item (list 'logand (list '+ (gen-pop-item) (gen-pop-item)) #xFFFFFFFF))
                             :expression-type :INTEGER)))
    (pop (stack context)) (pop (stack context)) (push expr (stack context))
    expr))

(defmethod codegen ((insn ir-ladd) context)
  (let ((expr (make-instance '<expression>
                             :insn insn
                             :code (gen-push-item (list 'logand (list '+ (gen-pop-item) (gen-pop-item)) #xFFFFFFFFFFFFFFFF))
                             :expression-type :LONG)))
    (pop (stack context)) (pop (stack context)) (push expr (stack context))
    expr))

(defmethod codegen ((insn ir-fadd) context)
  ;; FIXME -- handle NaN cases
  (let ((expr (make-instance '<expression>
                             :insn insn
                             :code (gen-push-item (list '+ (gen-pop-item) (gen-pop-item)))
                             :expression-type :FLOAT)))
    (pop (stack context)) (pop (stack context)) (push expr (stack context))
    expr))

(defmethod codegen ((insn ir-imul) context)
  (let ((expr (make-instance '<expression>
                             :insn insn
                             :code (gen-push-item (list 'logand (list '* (gen-pop-item) (gen-pop-item)) #xFFFFFFFF))
                             :expression-type :INTEGER)))
    (pop (stack context)) (pop (stack context)) (push expr (stack context))
    expr))

(defmethod codegen ((insn ir-iand) context)
  (let ((expr (make-instance '<expression>
                             :insn insn
                             :code (gen-push-item (list 'logand (gen-pop-item) (gen-pop-item)))
                             :expression-type :INTEGER)))
    (pop (stack context)) (pop (stack context)) (push expr (stack context))
    expr))

(defmethod codegen ((insn ir-land) context)
  (let ((expr (make-instance '<expression>
                             :insn insn
                             :code (gen-push-item (list 'logand (gen-pop-item) (gen-pop-item)))
                             :expression-type :LONG)))
    (pop (stack context)) (pop (stack context)) (push expr (stack context))
    expr))

(defmethod codegen ((insn ir-ior) context)
  (let ((expr (make-instance '<expression>
                             :insn insn
                             :code (gen-push-item (list 'logior (gen-pop-item) (gen-pop-item)))
                             :expression-type :INTEGER)))
    (pop (stack context)) (pop (stack context)) (push expr (stack context))
    expr))

(defmethod codegen ((insn ir-ixor) context)
  (let ((expr (make-instance '<expression>
                             :insn insn
                             :code (gen-push-item (list 'logxor (gen-pop-item) (gen-pop-item)))
                             :expression-type :INTEGER)))
    (pop (stack context)) (pop (stack context)) (push expr (stack context))
    expr))

(defmethod codegen ((insn ir-lor) context)
  (let ((expr (make-instance '<expression>
                             :insn insn
                             :code (gen-push-item (list 'logior (gen-pop-item) (gen-pop-item)))
                             :expression-type :LONG)))
    (pop (stack context)) (pop (stack context)) (push expr (stack context))
    expr))

(defmethod codegen ((insn ir-lxor) context)
  (let ((expr (make-instance '<expression>
                             :insn insn
                             :code (gen-push-item (list 'logxor (gen-pop-item) (gen-pop-item)))
                             :expression-type :LONG)))
    (pop (stack context)) (pop (stack context)) (push expr (stack context))
    expr))

(defmethod codegen ((insn ir-array-length) context)
  (let ((expr (make-instance '<expression>
                             :insn insn
                             :code (gen-push-item (list 'length (gen-pop-item)))
                             :expression-type :INTEGER)))
    (pop (stack context)) (push expr (stack context))
    expr))

(defmethod codegen ((insn ir-assign) context)
  (with-slots (source target) insn
    (let ((expr (make-instance '<expression>
                               :insn insn
                               :code (list 'let (list (list 'value (code (codegen source context))))
                                           (list 'setf (code (codegen target context)) 'value))
                               :expression-type :INTEGER)))
    expr)))

(defmethod codegen ((insn ir-call-static-method) context)
  (with-slots (class method-name args return-type) insn
    (let ((expr (make-instance '<expression>
                               :insn insn
                               :code (let* ((nargs (length args))
                                            (call (cond
                                                    ((eq nargs 0)
                                                     (list (intern (format nil "~A.~A" class method-name) :openldk)))
                                                    ((eq nargs 1)
                                                     (list (intern (format nil "~A.~A" class method-name) :openldk) (code (codegen (car args) context))))
                                                    (t
                                                     (list 'apply
                                                           (list 'function (intern (format nil "~A.~A"
                                                                                           class
                                                                                           method-name)
                                                                                   :openldk))
                                                           (list 'reverse (cons 'list (mapcar (lambda (a) (code (codegen a context))) args))))))))
                                       (if (void-return-p insn)
                                           call
                                           (gen-push-item call)))
                               :expression-type return-type)))
      (unless (void-return-p insn)
        (push expr (stack context)))
      expr)))

(defmethod codegen ((insn ir-caload) context)
  ;;; FIXME: throw nullpointerexception and invalid array index exception if needed
  (with-slots (source target) insn
    (let ((expr (make-instance '<expression>
                               :insn insn
                               :code (list 'let (list (list 'index (gen-pop-item))
                                                      (list 'arrayref (gen-pop-item)))
                                           (gen-push-item (list 'char-code (list 'aref 'arrayref 'index))))
                               :expression-type :CHAR)))
      (pop (stack context)) (pop (stack context)) (push expr (stack context))
      expr)))

(defmethod codegen ((insn ir-iaload) context)
  ;;; FIXME: throw nullpointerexception and invalid array index exception if needed
  (with-slots (source target) insn
    (let ((expr (make-instance '<expression>
                               :insn insn
                               :code (list 'let (list (list 'index (gen-pop-item))
                                                      (list 'arrayref (gen-pop-item)))
                                           (gen-push-item (list 'aref 'arrayref 'index)))
                               :expression-type :INTEGER)))
      (pop (stack context)) (pop (stack context)) (push expr (stack context))
      expr)))

(defmethod codegen ((insn ir-castore) context)
  ;;; FIXME: throw nullpointerexception and invalid array index exception if needed
  (let ((expr (make-instance '<expression>
                             :insn insn
                             :code (list 'let (list (list 'value (gen-pop-item))
                                                    (list 'index (gen-pop-item))
                                                    (list 'arrayref (gen-pop-item)))
                                         (list 'setf (list 'aref 'arrayref 'index) (list 'code-char 'value)))
                             :expression-type nil)))
    (pop (stack context)) (pop (stack context)) (pop (stack context))
    expr))


(defmethod codegen ((insn ir-checkcast) context)
  (declare (ignore context))
  ;; FIXME: the array test can be done at compiletime
  (with-slots (class) insn
    (let ((expr (make-instance '<expression>
                               :insn insn
                               :code (list 'progn
                                           (list 'when (gen-peek-item)
                                                 (list 'unless (list 'or
                                                                     (list 'typep (gen-peek-item)
                                                                           (list 'quote (intern (name (slot-value (slot-value insn 'class) 'class)) :openldk)))
                                                                     (list 'and
                                                                           (list 'arrayp (gen-peek-item))
                                                                           (list 'eq (list 'quote '|java/util/Arrays|) (list 'quote (intern (name (slot-value (slot-value insn 'class) 'class)) :openldk)))))
                                                       (gen-push-item (list 'make-instance (list 'quote '|java/lang/ClassCastException|)))
                                                       (list 'error (list 'lisp-condition (gen-peek-item))))))
                               :expression-type nil)))
      expr)))

(defmethod codegen ((insn ir-class) context)
  (declare (ignore context))
  (let ((classname (slot-value (slot-value insn 'class) 'name)))
    (let ((expr (make-instance '<expression>
                               :insn insn
                               :code (java-class (gethash classname *classes*))
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
  (let ((expr (make-instance '<expression>
                             :insn insn
                             :code (list 'handler-case
                                         (list 'let (list (list 'value2 (gen-pop-item))
                                                          (list 'value1 (gen-pop-item)))
                                               (gen-push-item (list 'rem 'value1 'value2)))
                                         (list 'division-by-zero (list 'e)
                                               (gen-push-item (list 'make-instance (list 'quote '|java/lang/ArithmeticException|)))
                                               (list 'error (list 'lisp-condition (gen-peek-item)))))
                             :expression-type :INTEGER)))
    (pop (stack context)) (pop (stack context)) (push expr (stack context))
    expr))

(defmethod codegen ((insn ir-fdiv) context)
  ;; FIXME - handle all weird conditions
  (let ((expr (make-instance '<expression>
                             :insn insn
                              :code (list 'handler-case
                                         (list 'let (list (list 'value2 (gen-pop-item))
                                                          (list 'value1 (gen-pop-item)))
                                              (gen-push-item (list '/ 'value1 'value2)))
                                         (list 'division-by-zero (list 'e)
                                               (gen-push-item (list 'make-instance (list 'quote '|java/lang/ArithmeticException|)))
                                               (list 'error (list 'lisp-condition (gen-peek-item)))))
                             :expression-type :FLOAT)))
    (pop (stack context)) (pop (stack context)) (push expr (stack context))
    expr))

(defmethod codegen ((insn ir-idiv) context)
  ;; FIXME - handle all weird conditions
  (let ((expr (make-instance '<expression>
                             :insn insn
                             :code (list 'handler-case
                                         (list 'let (list (list 'value2 (gen-pop-item))
                                                          (list 'value1 (gen-pop-item)))
                                               (gen-push-item (list 'floor (list '/ 'value1 'value2))))
                                         (list 'division-by-zero (list 'e)
                                               (gen-push-item (list 'make-instance (list 'quote '|java/lang/ArithmeticException|)))
                                               (list 'error (list 'lisp-condition (gen-peek-item)))))
                             :expression-type :INTEGER)))
    (pop (stack context)) (pop (stack context)) (push expr (stack context))
    expr))

(defmethod codegen ((insn ir-ldiv) context)
  ;; FIXME - handle all weird conditions
  (let ((expr (make-instance '<expression>
                             :insn insn
                             :code (list 'handler-case
                                         (list 'let (list (list 'value2 (gen-pop-item))
                                                          (list 'value1 (gen-pop-item)))
                                               (gen-push-item (list 'floor (list '/ 'value1 'value2))))
                                         (list 'division-by-zero (list 'e)
                                               (gen-push-item (list 'make-instance (list 'quote '|java/lang/ArithmeticException|)))
                                               (list 'error (list 'lisp-condition (gen-peek-item)))))
                             :expression-type :LONG)))
    (pop (stack context)) (pop (stack context)) (push expr (stack context))
    expr))

(defmethod codegen ((insn ir-dup) context)
  (let ((expr (make-instance '<expression>
                             :insn insn
                             :code (gen-push-item (gen-peek-item))
                             :expression-type (expression-type (car (stack context))))))
    (push expr (stack context))
    expr))

(defmethod codegen ((insn ir-dup-x1) context)
  (let ((tos-type (expression-type (car (stack context)))))
    (when (or (eq tos-type :LONG) (eq tos-type :DOUBLE))
      (error "FIXME: handle long/double on stack")))
  (let ((expr (make-instance '<expression>
                             :insn insn
                             :code (list 'let (list (list 'value1 (gen-pop-item))
                                                    (list 'value2 (gen-pop-item)))
                                         (gen-push-item 'value1)
                                         (gen-push-item 'value2)
                                         (gen-push-item 'value1)
                                         :expression-type (expression-type (car (stack context)))))))
    (let ((expr1 (pop (stack context)))
          (expr2 (pop (stack context))))
      (push expr1 (stack context))
      (push expr2 (stack context))
      (push expr1 (stack context)))
    expr))

(defmethod codegen ((insn ir-dup2) context)
  (let ((tos-type (expression-type (car (stack context)))))
    (when (or (eq tos-type :LONG) (eq tos-type :DOUBLE))
      (error "FIXME: handle long/double on stack")))
  (let ((expr (make-instance '<expression>
                             :insn insn
                             :code (list 'let (list (list 'value1 (gen-pop-item))
                                                    (list 'value2 (gen-pop-item)))
                                         (gen-push-item 'value2)
                                         (gen-push-item 'value1)
                                         (gen-push-item 'value2)
                                         (gen-push-item 'value1)))))
    (let ((expr1 (pop (stack context)))
          (expr2 (pop (stack context))))
      (push expr2 (stack context))
      (push expr1 (stack context))
      (push expr2 (stack context))
      (push expr1 (stack context)))
    expr))

(defmethod codegen ((insn ir-fcmpg) context)
  (let ((expr (make-instance '<expression>
                             :insn insn
                             :code (list 'let (list (list 'value2 (gen-pop-item))
                                                    (list 'value1 (gen-pop-item)))
                                         (list 'if (list 'or (list 'float-features:float-nan-p 'value1) (list 'float-features:float-nan-p 'value2))
                                               (gen-push-item 1)
                                               (list 'if (list '> 'value1 'value2)
                                                     (gen-push-item 1)
                                                     (list 'if (list '< 'value1 'value2)
                                                           (gen-push-item -1)
                                                           (gen-push-item 0)))))
                             :expression-type :INTEGER)))
    (pop (stack context)) (pop (stack context)) (push expr (stack context))
    expr))

(defmethod codegen ((insn ir-fcmpl) context)
  (let ((expr (make-instance '<expression>
                             :insn insn
                             :code (list 'let (list (list 'value2 (gen-pop-item))
                                                    (list 'value1 (gen-pop-item)))
                                         (list 'if (list 'or (list 'float-features:float-nan-p 'value1) (list 'float-features:float-nan-p 'value2))
                                               (gen-push-item -1)
                                               (list 'if (list '> 'value1 'value2)
                                                     (gen-push-item 1)
                                                     (list 'if (list '< 'value1 'value2)
                                                           (gen-push-item -1)
                                                           (gen-push-item 0)))))
                             :expression-type :INTEGER)))
    (pop (stack context)) (pop (stack context)) (push expr (stack context))
    expr))

(defmethod codegen ((insn ir-iastore) context)
  (let ((expr (make-instance '<expression>
                             :insn insn
                             :code (list 'let (list (list 'value (gen-pop-item))
                                                    (list 'index (gen-pop-item))
                                                    (list 'arrayref (gen-pop-item)))
                                         (list 'setf (list 'aref 'arrayref 'index) 'value)))))
    (pop (stack context))
    (pop (stack context))
    (pop (stack context))
    expr))

(defmethod codegen ((insn ir-ineg) context)
  ;; FIXME: handle integer overflow
  (let ((expr (make-instance '<expression>
                             :insn insn
                             :code (gen-push-item (list '- (gen-pop-item)))
                             :expression-type :INTEGER)))
    (pop (stack context)) (push expr (stack context))
    expr))

(defmethod codegen ((insn ir-i2c) context)
  (let ((expr (make-instance '<expression>
                             :insn insn
                             :code (gen-push-item (list 'code-char (gen-pop-item)))
                             :expression-type :CHAR)))
    (pop (stack context)) (push expr (stack context))
    expr))

(defmethod codegen ((insn ir-l2f) context)
  (let ((expr (make-instance '<expression>
                             :insn insn
                             :code (gen-push-item (list 'float (gen-pop-item)))
                             :expression-type :FLOAT)))
    (pop (stack context)) (push expr (stack context))
    expr))

(defmethod codegen ((insn ir-f2i) context)
  (let ((expr (make-instance '<expression>
                             :insn insn
                             :code (gen-push-item (list 'floor (gen-pop-item)))
                             :expression-type :INTEGER)))
    (pop (stack context)) (push expr (stack context))
    expr))

(defmethod codegen ((insn ir-d2l) context)
  (let ((expr (make-instance '<expression>
                             :insn insn
                             :code (gen-push-item (list 'floor (gen-pop-item)))
                             :expression-type :LONG)))
    (pop (stack context)) (push expr (stack context))
    expr))

(defmethod codegen ((insn ir-i2f) context)
  (let ((expr (make-instance '<expression>
                             :insn insn
                             :code (gen-push-item (list 'float (gen-pop-item)))
                             :expression-type :FLOAT)))
    (pop (stack context)) (push expr (stack context))
    expr))

(defmethod codegen ((insn ir-iinc) context)
   (with-slots (index const) insn
     (let ((expr (make-instance '<expression>
                                :insn insn
                                :code (list 'incf (intern (format nil "local-~A" index) :openldk) const))))
       expr)))

(defmethod codegen ((insn ir-if-acmpeq) context)
   (with-slots (index offset const) insn
     (let ((expr (make-instance '<expression>
                                :insn insn
                                :code (list 'let (list (list 'o1 (list 'sxhash (gen-pop-item)))
                                                       (list 'o2 (list 'sxhash (gen-pop-item))))
                                            (list 'when (list 'eq 'o1 'o2)
                                                  (list 'go (intern (format nil "branch-target-~A" offset))))))))
       (pop (stack context)) (pop (stack context))
       expr)))

(defmethod codegen ((insn ir-if-acmpne) context)
   (with-slots (offset) insn
     (let ((expr (make-instance '<expression>
                                :insn insn
                                :code (list 'let (list (list 'o1 (gen-pop-item))
                                                       (list 'o2 (gen-pop-item)))
                                            (list 'when (list 'not (list 'eq (list 'sxhash 'o1) (list 'sxhash 'o2)))
                                                  (list 'go (intern (format nil "branch-target-~A" offset))))))))
       (pop (stack context)) (pop (stack context))
       expr)))

(defmethod codegen ((insn ir-if-icmpeq) context)
  (with-slots (offset) insn
    (let ((expr (make-instance '<expression>
                               :insn insn
                               :code (list 'when (list 'equal (gen-pop-item) (gen-pop-item))
                                           (list 'go (intern (format nil "branch-target-~A" offset)))))))
      (pop (stack context)) (pop (stack context))
      expr)))

(defmethod codegen ((insn ir-if-icmple) context)
  (with-slots (offset) insn
    (let ((expr (make-instance '<expression>
                               :insn insn
                               :code (list 'when (list '>= (gen-pop-item) (gen-pop-item))
                                           (list 'go (intern (format nil "branch-target-~A" offset)))))))
      (pop (stack context)) (pop (stack context))
      expr)))

(defmethod codegen ((insn ir-if-icmpge) context)
  (with-slots (offset) insn
    (let ((expr (make-instance '<expression>
                               :insn insn
                               :code (list 'when (list '<= (gen-pop-item) (gen-pop-item))
                                           (list 'go (intern (format nil "branch-target-~A" offset)))))))
      (pop (stack context)) (pop (stack context))
      expr)))

(defmethod codegen ((insn ir-if-icmplt) context)
  (with-slots (offset) insn
    (let ((expr (make-instance '<expression>
                               :insn insn
                               :code (list 'when (list '> (gen-pop-item) (gen-pop-item))
                                           (list 'go (intern (format nil "branch-target-~A" offset)))))))
      (pop (stack context)) (pop (stack context))
      expr)))

(defmethod codegen ((insn ir-if-icmpgt) context)
  (with-slots (offset) insn
    (let ((expr (make-instance '<expression>
                               :insn insn
                               :code (list 'when (list '< (gen-pop-item) (gen-pop-item))
                                           (list 'go (intern (format nil "branch-target-~A" offset)))))))
      (pop (stack context)) (pop (stack context))
      expr)))

(defmethod codegen ((insn ir-if-icmpne) context)
  (with-slots (offset) insn
    (let ((expr (make-instance '<expression>
                               :insn insn
                               :code (list 'when (list 'not (list 'eq (gen-pop-item) (gen-pop-item)))
                                           (list 'go (intern (format nil "branch-target-~A" offset)))))))
      (pop (stack context)) (pop (stack context))
      expr)))

(defmethod codegen ((insn ir-ifeq) context)
  (with-slots (offset) insn
    (let ((expr (make-instance '<expression>
                               :insn insn
                               :code (list 'when (list 'eq (gen-pop-item) 0)
                                           (list 'go (intern (format nil "branch-target-~A" offset)))))))
      (pop (stack context)) (pop (stack context))
      expr)))

(defmethod codegen ((insn ir-ifge) context)
  (with-slots (offset) insn
    (let ((expr (make-instance '<expression>
                               :insn insn
                               :code (list 'when (list '>= (gen-pop-item) 0)
                                           (list 'go (intern (format nil "branch-target-~A" offset)))))))
      (pop (stack context))
      expr)))

(defmethod codegen ((insn ir-ifle) context)
  (with-slots (offset) insn
    (let ((expr (make-instance '<expression>
                               :insn insn
                               :code (list 'when (list '<= (gen-pop-item) 0)
                                           (list 'go (intern (format nil "branch-target-~A" offset)))))))
      (pop (stack context))
      expr)))

(defmethod codegen ((insn ir-iflt) context)
  (with-slots (offset) insn
    (let ((expr (make-instance '<expression>
                               :insn insn
                               :code (list 'when (list '< (gen-pop-item) 0)
                                           (list 'go (intern (format nil "branch-target-~A" offset)))))))
      (pop (stack context))
      expr)))

(defmethod codegen ((insn ir-ifgt) context)
  (with-slots (offset) insn
    (let ((expr (make-instance '<expression>
                               :insn insn
                               :code (list 'when (list '> (gen-pop-item) 0)
                                           (list 'go (intern (format nil "branch-target-~A" offset)))))))
      (pop (stack context))
      expr)))

(defmethod codegen ((insn ir-ifne) context)
  (with-slots (offset) insn
    (let ((expr (make-instance '<expression>
                               :insn insn
                               :code (list 'when (list 'not (list 'eq (gen-pop-item) '0))
                                           (list 'go (intern (format nil "branch-target-~A" offset)))))))
      (pop (stack context))
      expr)))

(defmethod codegen ((insn ir-ifnonnull) context)
  (with-slots (offset) insn
    (let ((expr (make-instance '<expression>
                               :insn insn
                               :code (list 'when (list 'not (list 'null (gen-pop-item)))
                                           (list 'go (intern (format nil "branch-target-~A" offset)))))))
      (pop (stack context))
      expr)))

(defmethod codegen ((insn ir-ifnull) context)
  (with-slots (offset) insn
    (let ((expr (make-instance '<expression>
                               :insn insn
                               :code (list 'when (list 'null (gen-pop-item))
                                           (list 'go (intern (format nil "branch-target-~A" offset)))))))
      (pop (stack context))
      expr)))

(defmethod codegen ((insn ir-instanceof) context)
  (with-slots (class) insn
    (let ((expr (make-instance '<expression>
                               :insn insn
                               :code (gen-push-item
                                      (list 'if (list 'typep (gen-pop-item)
                                                      (list 'quote (intern (name (slot-value (slot-value insn 'class) 'class)) :openldk))) 1 0))
                               :expression-type :INTEGER)))
      (pop (stack context)) (push expr (stack context))
      expr)))

(defun logical-shift-right-32 (integer shift)
  (logand
   (ash (logand integer #xffffffff) (- shift))
   #xffffffff))

(defun shl (x width bits)
  "Compute bitwise left shift of x by 'bits' bits, represented on 'width' bits"
  (logand (ash x bits)
          (1- (ash 1 width))))

(defun shr (x width bits)
  "Compute bitwise right shift of x by 'bits' bits, represented on 'width' bits"
  (logand (ash x (- bits))
          (1- (ash 1 width))))

(defmethod codegen ((insn ir-ishl) context)
  ;; FIXME: this is wrong.
  (let ((expr (make-instance '<expression>
                             :insn insn
                             :code (list 'let (list (list 'value2 (gen-pop-item))
                                                    (list 'value1 (gen-pop-item)))
                                         (gen-push-item (list 'ash 'value1 'value2)))
                             :expression-type :INTEGER)))
    (pop (stack context)) (pop (stack context)) (push expr (stack context))
    expr))

(defmethod codegen ((insn ir-lshl) context)
  ;; FIXME: this is wrong.
  (let ((expr (make-instance '<expression>
                             :insn insn
                             :code (list 'let (list (list 'value2 (gen-pop-item))
                                                    (list 'value1 (gen-pop-item)))
                                         (gen-push-item (list 'shl 'value1 'value2 32)))
                             :expression-type :INTEGER)))
    (pop (stack context)) (pop (stack context)) (push expr (stack context))
    expr))

(defmethod codegen ((insn ir-iushr) context)
  ;; FIXME: this is wrong.
  (let ((expr (make-instance '<expression>
                             :insn insn
                             :code (list 'let (list (list 'value2 (gen-pop-item))
                                                    (list 'value1 (gen-pop-item)))
                                         (gen-push-item (list 'shr 'value1 'value2 32)))
                             :expression-type :INTEGER)))
    (pop (stack context)) (pop (stack context)) (push expr (stack context))
    expr))

(defmethod codegen ((insn ir-lshr) context)
  ;; FIXME: this is wrong.
  (let ((expr (make-instance '<expression>
                             :insn insn
                             :code (list 'let (list (list 'value2 (gen-pop-item))
                                                    (list 'value1 (gen-pop-item)))
                                         (gen-push-item (list 'shr 'value1 'value2 64)))
                             :expression-type :LONG)))
    (pop (stack context)) (pop (stack context)) (push expr (stack context))
    expr))

(defmethod codegen ((insn ir-ishr) context)
  ;; FIXME: this is wrong.
  (let ((expr (make-instance '<expression>
                             :insn insn
                             :code (list 'let (list (list 'value2 (gen-pop-item))
                                                    (list 'value1 (gen-pop-item)))
                                         (gen-push-item (list 'ash 'value1 (list '- 0 'value2))))
                             :expression-type :INTEGER)))
    (pop (stack context)) (pop (stack context)) (push expr (stack context))
    expr))

(defmethod codegen ((insn ir-lcmp) context)
  (let ((expr (make-instance '<expression>
                             :insn insn
                             :code (list 'let (list (list 'value2 (gen-pop-item))
                                                    (list 'value1 (gen-pop-item)))
                                         (list 'cond
                                               (list (list 'eq 'value1 'value2)
                                                     (gen-push-item 0))
                                               (list (list '> 'value1 'value2)
                                                     (gen-push-item 1))
                                               (list 't
                                                     (gen-push-item -1))))
                             :expression-type :INTEGER)))
    (pop (stack context)) (pop (stack context)) (push expr (stack context))
    expr))

(defmethod codegen ((insn ir-lushr) context)
  (let ((expr (make-instance '<expression>
                             :insn insn
                             :code (list 'let (list (list 'value2 (gen-pop-item))
                                                    (list 'value1 (gen-pop-item)))
                                         (gen-push-item (list 'ash 'value1 (list \- 'value2))))
                             :expression-type :LONG)))
    (pop (stack context)) (pop (stack context)) (push expr (stack context))
    expr))

(defmethod codegen ((insn ir-goto) context)
  (declare (ignore context))
  (with-slots (offset) insn
    (let ((expr (make-instance '<expression>
                               :insn insn
                               :code (list 'go (intern (format nil "branch-target-~A" offset))))))
      expr)))

(defmethod codegen ((insn ir-call-virtual-method) context)
  (with-slots (method-name args) insn
    (let ((expr (make-instance '<expression>
                               :insn insn
                               :code (let* ((nargs (length args))
                                            (call (cond
                                                    ((eq nargs 0)
                                                     (error "internal error"))
                                                    ((eq nargs 1)
                                                     ;; FIXME: handle long/double
                                                     (list (intern (format nil "~A" method-name) :openldk) (code (codegen (car args) context))))
                                                    (t
                                                     (list 'apply
                                                           (list 'function (intern (format nil "~A"
                                                                                           method-name) :openldk))
                                                           (list 'reverse (cons 'list (mapcar (lambda (a) (code (codegen a context))) args))))))))
                                       (if (void-return-p insn)
                                           call
                                           (gen-push-item call))))))
      (unless (void-return-p insn)
        (push expr (stack context)))
      expr)))

(defmethod codegen ((insn ir-clinit) context)
  (with-slots (class) insn
    (let ((expr (make-instance '<expression>
                               :insn insn
                               :code (let ((class (ir-class-class class)))
                                       (list 'unless (list 'initialized-p class)
                                             (list (intern (format nil "%clinit-~A" (slot-value class 'name)) :openldk)))))))
      expr)))

(defmethod codegen ((insn ir-local-variable) context)
  (with-slots (index) insn
    ;; FIXME: track type of local vars
    (let ((expr (make-instance '<expression>
                               :insn insn
                               :code (intern (format nil "local-~A" index) :openldk))))
      expr)))

(defmethod codegen ((insn ir-long-local-variable) context)
  (with-slots (index) insn
    (let ((expr (make-instance '<expression>
                               :insn insn
                               :code (intern (format nil "local-~A" index) :openldk)
                               :expression-type :LONG)))
      expr)))

(defmethod codegen ((insn ir-monitorenter) context)
  (let ((expr (make-instance '<expression>
                             :insn insn
                             :code (list 'monitor-enter (gen-pop-item)))))
    (pop (stack context))
    expr))

(defmethod codegen ((insn ir-monitorexit) context)
  (let ((expr (make-instance '<expression>
                             :insn insn
                             :code (list 'monitor-exit (gen-pop-item)))))
    (pop (stack context))
    expr))

(defmethod codegen ((insn ir-mul) context)
  ;; FIXME: track type
  (let ((expr (make-instance '<expression>
                             :insn insn
                             :code (gen-push-item (list '* (gen-pop-item) (gen-pop-item))))))
    (pop (stack context)) (pop (stack context)) (push expr (stack context))
    expr))

(defmethod codegen ((insn ir-new) context)
  (with-slots (class) insn
    (with-slots (class) class
      (let ((expr (make-instance '<expression>
                                 :insn insn
                                 :code (list 'make-instance (list 'quote (intern (slot-value class 'name) :openldk)))
                                 :expression-type :REFERENCE)))
        ;; We don't push this. bc-2-ir pushes this.
        expr))))

(defmethod codegen ((insn ir-new-array) context)
  (let ((expr (make-instance '<expression>
                             :insn insn
                             :code (list 'make-array (gen-pop-item) :initial-element nil)
                             :expression-type :ARRAY)))
    ;; We don't push this. bc-2-ir pushes this.
    expr))

(defmethod codegen ((insn ir-nop) context)
  (declare (ignore context))
  (let ((expr (make-instance '<expression>
                             :insn insn
                             :code (gensym "NOP-"))))
    expr))

(defmethod codegen ((insn ir-pop) context)
  (let ((expr (make-instance '<expression>
                             :insn insn
                             :code (gen-pop-item))))
    (pop (stack context))
    expr))

(defmethod codegen ((insn ir-push) context)
  (let* ((value (codegen (value insn) context))
         (expr (make-instance '<expression>
                              :insn insn
                              :code (gen-push-item (code value))
                              :expression-type (expression-type value))))
    (push expr (stack context))
    expr))

(defmethod codegen ((insn ir-isub) context)
  (let ((expr (make-instance '<expression>
                             :insn insn
                             :code (list 'let (list (list 'value2 (gen-pop-item))
                                                    (list 'value1 (gen-pop-item)))
                                         (gen-push-item (list 'logand (list '- 'value1 'value2) #xFFFFFFFF)))
                             :expression-type :INTEGER)))
    (pop (stack context)) (pop (stack context)) (push expr (stack context))
    expr))

(defmethod codegen ((insn ir-lsub) context)
  (let ((expr (make-instance '<expression>
                             :insn insn
                             :code (list 'let (list (list 'value2 (gen-pop-item))
                                                    (list 'value1 (gen-pop-item)))
                                         (gen-push-item (list 'logand (list '- 'value1 'value2) #xFFFFFFFFFFFFFFFF)))
                             :expression-type :LONG)))
    (pop (stack context)) (pop (stack context)) (push expr (stack context))
    expr))

(defmethod codegen ((insn ir-call-special-method) context)
  (with-slots (class method-name args) insn
     (let ((expr (make-instance '<expression>
                                :insn insn
                                :code (let ((call (list 'destructuring-bind (cons 'method 'next)
                                                        (list 'closer-mop:compute-applicable-methods-using-classes
                                                              (list 'function (intern (format nil "~A" method-name) :openldk))
                                                              ;; FIXME: This should be based on the args list
                                                              (cons 'list
                                                                    (cons (find-class (intern (slot-value class 'name) :openldk))
                                                                          (loop for a in args collect t))))
                                                        (list 'let (list (list 'fn (list 'closer-mop:method-function 'method)))
                                                              (list 'apply 'fn
                                                                    (list 'list (cons 'reverse (list (cons 'list (mapcar (lambda (a) (code (codegen a context))) args)))) 'next))))))
                                        (if (void-return-p insn)
                                            call
                                            (gen-push-item call))))))
       (unless (void-return-p insn)
         (push expr (stack context)))
       expr)))

(defmethod codegen ((insn ir-member) context)
  (with-slots (member-name) insn
    (let ((expr (make-instance '<expression>
                               :insn insn
                               :code (list 'slot-value
                                           (list 'let (list (list 'objref (gen-pop-item)))
                                                 (list 'when (list 'null 'objref) (list 'error
                                                                                        (format nil "Null Pointer Exception ~A" (slot-value insn 'address))))
                                                 'objref)
                                           (list 'quote (intern member-name :openldk))))))
      (pop (stack context))
      expr)))

(defmethod codegen ((insn ir-static-member) context)
  (declare (ignore context))
  (with-slots (class member-name) insn
    (let ((expr (make-instance '<expression>
                               :insn insn
                               :code (list 'slot-value
                                           (intern (format nil "+static-~A+" (slot-value (slot-value class 'class) 'name)) :openldk)
                                           (list 'quote (intern member-name :openldk))))))
      expr)))

(defmethod codegen ((insn ir-store) context)
  (with-slots (target) insn
    (let ((expr (make-instance '<expression>
                               :insn insn
                               :code (list 'setf (code (codegen target context)) (gen-pop-item)))))
      (pop (stack context))
      expr)))

(defmethod codegen ((insn ir-lstore) context)
  (let ((expr (make-instance '<expression>
                             :insn insn
                             :code (with-slots (target) insn
                                     (list 'setf (code (codegen target context)) (gen-pop-item))))))
    (pop (stack context))
    expr))

(define-condition java-lang-throwable (error)
  ((throwable :initarg :throwable :reader throwable)))

(defun make-java-condition (e)
  (make-condition (gethash (class-of e) *condition-table*) :objref e))

(defmethod codegen ((insn ir-throw) context)
  (declare (ignore context))
  (let ((expr (make-instance '<expression>
                             :insn insn
                             :code (list 'let (list (list 'c (list 'lisp-condition (gen-peek-item))))
                                         (list 'error 'c)))))
    expr))

(defmethod codegen ((insn ir-return) context)
  (declare (ignore context))
  (let ((expr (make-instance '<expression>
                             :insn insn
                             :code (list 'return))))
    expr))

(defmethod codegen ((insn ir-return-value) context)
  (let ((expr (make-instance '<expression>
                             :insn insn
                             :code (list 'return-from
                                         (intern (slot-value insn 'fn-name) :openldk)
                                         (gen-pop-item)))))
    (pop (stack context))
    expr))

(defmethod codegen ((insn ir-variable) context)
  (declare (ignore context))
  (let ((expr (make-instance '<expression>
                             :insn insn
                             :code (slot-value insn 'name))))
    expr))

(defmethod codegen-block ((basic-block <basic-block>) &optional (stop-block nil))
  (unless (equal basic-block stop-block)
    (if (not (slot-value basic-block 'code-emitted-p))
        (progn
          (push basic-block (slot-value *context* 'blocks))
          (let ((lisp-code
                  (cons (intern (format nil "branch-target-~A" (address (car (slot-value basic-block 'code)))))
                        (loop for insn in (slot-value basic-block 'code)
                              for expr = (codegen insn *context*)
                              collect (trace-insn insn (code expr))))))
            (setf (slot-value basic-block 'code-emitted-p) t)
            (pop (slot-value *context* 'blocks))
            ;; sort by address
            (let ((successor-list (sort (fset:convert 'list (successors basic-block)) (lambda (a b) (< (address a) (address b))))))
              (if (eq 1 (length successor-list))
                  (if (slot-value (car successor-list) 'code-emitted-p)
                      (when (and (<= (address (car successor-list)) (+ (address (car (last (code basic-block)))) 4)))
                        (setf lisp-code (append lisp-code (list (list 'go (intern (format nil "branch-target-~A" (address (car successor-list)))))))))))
              (dolist (successor successor-list)
                (when successor
                  (setf lisp-code (append lisp-code (codegen-block successor (or stop-block (try-exit-block basic-block))))))))

            ;; Emit handlers for finally handlers. FIXME: in build-basic-blocks, sort try-catch list by end of range
            (when (find-if (lambda (p) (null (car p))) (try-catch basic-block))
              ;; This is a TRY-CATCH block.  Wrap this in HANDLER-CASE.
              (loop for tc in (reverse (try-catch basic-block))
                    unless (car tc)
                      do (setf lisp-code (append (list (list 'handler-case
                                                             (cons 'tagbody lisp-code)
                                                             (list 'condition (list (intern "condition" :openldk))
                                                                   (cons 'tagbody
                                                                         (codegen-block (cdr tc) (try-exit-block basic-block))))))
                                                 (when (try-exit-block basic-block)
                                                   (codegen-block (try-exit-block basic-block)))))))

            ;; Emit handler if there's a non-finally try-catch associated with this block.
            (when (find-if (lambda (p) (car p)) (try-catch basic-block))
              ;; This is a TRY-CATCH block.  Wrap this in HANDLER-CASE.
              (setf lisp-code (append (list (append (list 'handler-case)
                                                    (list (append (list 'tagbody) lisp-code))
                                                    (loop for tc in (try-catch basic-block)
                                                          when (car tc)
                                                            collect (append (list (intern (format nil "condition-~A" (car tc)) :openldk)
                                                                                  (list (intern "condition" :openldk)))
                                                                            (list (cons 'tagbody
                                                                                        (codegen-block (cdr tc) (try-exit-block basic-block))))))))
                                      (when (try-exit-block basic-block)
                                        (codegen-block (try-exit-block basic-block))))))

            lisp-code))
        nil)))
