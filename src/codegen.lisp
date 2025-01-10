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

(defmethod codegen ((insn ssa-literal) &optional (stop-block nil))
  (slot-value insn 'value))

(defmethod codegen ((insn ssa-string-literal) &optional (stop-block nil))
  (let ((s (make-instance '|java/lang/String|)))
    (setf (slot-value s '|value|) (slot-value insn 'value))
    s))

(defmethod codegen ((insn ssa-aaload) &optional (stop-block nil))
  (flag-stack-usage *context*)
  (list 'let (list (list 'index (list 'pop-item))
                   (list 'arrayref (list 'pop-item)))
        (list 'push-item (list 'aref 'arrayref 'index))))

(defmethod codegen ((insn ssa-aastore) &optional (stop-block nil))
  (flag-stack-usage *context*)
  (list 'let (list (list 'value (list 'pop-item))
                   (list 'index (list 'pop-item))
                   (list 'arrayref (list 'pop-item)))
        (list 'setf (list 'aref 'arrayref 'index) 'value)))

(defmethod codegen ((insn ssa-add) &optional (stop-block nil))
  (flag-stack-usage *context*)
  (list 'push-item (list '+ (list 'pop-item) (list 'pop-item))))

(defmethod codegen ((insn ssa-array-length) &optional (stop-block nil))
  (list 'push-item (list 'length (list 'pop-item))))

(defmethod codegen ((insn ssa-assign) &optional (stop-block nil))
  (with-slots (source target) insn
    (list 'let (list (list 'value (codegen source)))
          (list 'setf (codegen target) 'value))))

(defmethod codegen ((insn ssa-call-static-method) &optional (stop-block nil))
  (with-slots (class method-name args) insn
    (let* ((nargs (length args))
           (call (cond
                   ((eq nargs 0)
                    (list (intern (format nil "~A.~A" class method-name) :openldk)))
                   ((eq nargs 1)
                    (list (intern (format nil "~A.~A" class method-name) :openldk) (codegen (car args))))
                   (t
                    (list 'apply
                          (list 'function (intern (format nil "~A.~A"
                                                          class
                                                          method-name)
                                                  :openldk))
                          (list 'reverse (cons 'list (mapcar (lambda (a) (codegen a)) args))))))))
      (if (void-return-p insn)
          call
          (list 'push-item call)))))

(defmethod codegen ((insn ssa-caload) &optional (stop-block nil))
  ;;; FIXME: throw nullpointerexception and invalid array index exception if needed
  (list 'let (list (list 'index (list 'pop-item))
                   (list 'arrayref (list 'pop-item)))
        (list 'push-item (list 'aref 'arrayref 'index))))

(defmethod codegen ((insn ssa-checkcast) &optional (stop-block nil))
  (with-slots (class) insn
    (list 'unless (list 'typep (list 'peek-item)
                        (list 'quote (intern (name (slot-value (slot-value insn 'class) 'class)) :openldk)))
          (list 'push-item (list 'make-instance (list 'quote '|java/lang/ClassCastException|)))
          (list 'error (list 'lisp-condition (list 'peek-item))))))

(defmethod codegen ((insn ssa-class) &optional (stop-block nil))
  (let* ((classname (slot-value (slot-value insn 'class) 'name))
         (java-class (gethash classname *java-classes*)))
    (if java-class
        java-class
        (let ((java-class (make-instance '|java/lang/Class|)))
          (setf (slot-value java-class '|name|)
                (let ((s (make-instance '|java/lang/String|)))
                  (setf (slot-value s '|value|) classname)))
          (setf (slot-value java-class '|classLoader|)
                (make-instance '|java/lang/ClassLoader|))
          (setf (gethash classname *java-classes*) java-class)
          java-class))))

(defmethod codegen ((insn ssa-branch-target) &optional (stop-block nil))
  (intern (format nil "branch-target-~A" (slot-value insn 'index))))

(defmethod codegen ((insn ssa-div) &optional (stop-block nil))
  (flag-stack-usage *context*)
  (list 'handler-case
        (list 'let (list (list 'op2 (list 'pop-item))
                         (list 'op1 (list 'pop-item)))
              (list 'push-item (list '/ 'op1 'op2)))
        (list 'division-by-zero (list 'e)
              (list 'push-item (list 'make-instance (list 'quote '|java/lang/ArithmeticException|)))
              (list 'error (list 'lisp-condition (list 'peek-item))))))

(defmethod codegen ((insn ssa-dup) &optional (stop-block nil))
  (flag-stack-usage *context*)
  (list 'push-item (list 'peek-item)))

(defmethod codegen ((insn ssa-iastore) &optional (stop-block nil))
  (flag-stack-usage *context*)
  (list 'let (list (list 'value (list 'pop-item))
                   (list 'index (list 'pop-item))
                   (list 'arrayref (list 'pop-item)))
        (list 'setf (list 'aref 'arrayref 'index) 'value)))

(defmethod codegen ((insn ssa-iinc) &optional (stop-block nil))
  (with-slots (index const) insn
    (list 'incf (intern (format nil "local-~A" index) :openldk) const)))

(defmethod codegen ((insn ssa-if-acmpeq) &optional (stop-block nil))
  (flag-stack-usage *context*)
  (with-slots (offset) insn
    (list 'if (list 'eq (list 'pop-item) (list 'pop-item))
          (list 'go (intern (format nil "branch-target-~A" offset))))))

(defmethod codegen ((insn ssa-if-acmpne) &optional (stop-block nil))
  (flag-stack-usage *context*)
  (with-slots (offset) insn
    (list 'if (list 'not (list 'eq (list 'pop-item) (list 'pop-item)))
          (list 'go (intern (format nil "branch-target-~A" offset))))))

(defmethod codegen ((insn ssa-if-icmpeq) &optional (stop-block nil))
  (flag-stack-usage *context*)
  (with-slots (offset) insn
    (list 'if (list 'equal (list 'pop-item) (list 'pop-item))
          (list 'go (intern (format nil "branch-target-~A" offset))))))

(defmethod codegen ((insn ssa-if-icmple) &optional (stop-block nil))
  (flag-stack-usage *context*)
  (with-slots (offset) insn
    (list 'if (list '>= (list 'pop-item) (list 'pop-item))
          (list 'go (intern (format nil "branch-target-~A" offset))))))

(defmethod codegen ((insn ssa-if-icmpge) &optional (stop-block nil))
  (flag-stack-usage *context*)
  (with-slots (offset) insn
    (list 'if (list '<= (list 'pop-item) (list 'pop-item))
          (list 'go (intern (format nil "branch-target-~A" offset))))))

(defmethod codegen ((insn ssa-if-icmpne) &optional (stop-block nil))
  (flag-stack-usage *context*)
  (with-slots (offset) insn
    (list 'if (list 'not (list 'eq (list 'pop-item) (list 'pop-item)))
          (list 'go (intern (format nil "branch-target-~A" offset))))))

(defmethod codegen ((insn ssa-ifeq) &optional (stop-block nil))
  (flag-stack-usage *context*)
  (with-slots (offset) insn
    (list 'if (list 'eq (list 'pop-item) 0)
          (list 'go (intern (format nil "branch-target-~A" offset))))))

(defmethod codegen ((insn ssa-ifge) &optional (stop-block nil))
  (flag-stack-usage *context*)
  (with-slots (offset) insn
    (list 'progn
          (list 'if (list '>= (list 'pop-item) 0)
                (list 'go (intern (format nil "branch-target-~A" offset)))))))

(defmethod codegen ((insn ssa-ifle) &optional (stop-block nil))
  (flag-stack-usage *context*)
  (with-slots (offset) insn
    (list 'progn
          (list 'if (list '<= (list 'pop-item) 0)
                (list 'go (intern (format nil "branch-target-~A" offset)))))))

(defmethod codegen ((insn ssa-iflt) &optional (stop-block nil))
  (flag-stack-usage *context*)
  (with-slots (offset) insn
    (list 'progn
          (list 'if (list '< (list 'pop-item) 0)
                (list 'go (intern (format nil "branch-target-~A" offset)))))))

(defmethod codegen ((insn ssa-ifne) &optional (stop-block nil))
  (flag-stack-usage *context*)
  (with-slots (offset) insn
    (list 'if (list 'not (list 'eq (list 'pop-item) '0))
          (list 'go (intern (format nil "branch-target-~A" offset))))))

(defmethod codegen ((insn ssa-ifnonnull) &optional (stop-block nil))
  (flag-stack-usage *context*)
  (with-slots (offset) insn
    (list 'if (list 'not (list 'null (list 'pop-item)))
          (list 'go (intern (format nil "branch-target-~A" offset))))))

(defmethod codegen ((insn ssa-ifnull) &optional (stop-block nil))
  (flag-stack-usage *context*)
  (with-slots (offset) insn
    (list 'if (list 'null (list 'pop-item))
          (list 'go (intern (format nil "branch-target-~A" offset))))))

(defmethod codegen ((insn ssa-instanceof) &optional (stop-block nil))
  (with-slots (class) insn
    (list 'push-item
          (list 'if (list 'typep (list 'pop-item)
                          (list 'quote (intern (name (slot-value (slot-value insn 'class) 'class)) :openldk))) 1 0))))

(defmethod codegen ((insn ssa-ishl) &optional (stop-block nil))
  (flag-stack-usage *context*)
  (list 'let (list (list 'op2 (list 'pop-item))
                   (list 'op1 (list 'pop-item)))
        (list 'progn
              (list 'format 't "ISHL ~A ~A~%" 'op1 'op2)
              (list 'push-item (list 'ash 'op1 'op2)))))

(defmethod codegen ((insn ssa-lcmp) &optional (stop-block nil))
  (flag-stack-usage *context*)
  (list 'let (list (list 'op2 (list 'pop-item))
                   (list 'op1 (list 'pop-item)))
        (list 'cond
              (list (list 'eq 'op1 'op2)
                    (list 'push-item 0))
              (list (list '> 'op1 'op2)
                    (list 'push-item 1))
              (list 't
                    (list 'push-item -1)))))

(defmethod codegen ((insn ssa-lushr) &optional (stop-block nil))
  (flag-stack-usage *context*)
  (list 'let (list (list 'op2 (list 'pop-item))
                   (list 'op1 (list 'pop-item)))
        (list 'progn
              (list 'format 't "LUSHR ~A ~A~%" 'op1 'op2)
              (list 'push-item (list 'ash 'op1 (list \- 'op2))))))

(defmethod codegen ((insn ssa-goto) &optional (stop-block nil))
  (with-slots (offset) insn
    (list 'go (intern (format nil "branch-target-~A" offset)))))

(defmethod codegen ((insn ssa-call-virtual-method) &optional (stop-block nil))
  (with-slots (method-name args) insn
    (let* ((nargs (length args))
           (call (cond
                   ((eq nargs 0)
                    (error "internal error"))
                   ((eq nargs 1)
                    (list (intern (format nil "~A" method-name) :openldk) (codegen (car args))))
                   (t
                    (list 'apply
                          (list 'function (intern (format nil "~A"
                                                          method-name) :openldk))
                          (list 'reverse (cons 'list (mapcar (lambda (a) (codegen a)) args))))))))
      (if (void-return-p insn)
          call
          (list 'push-item call)))))

(defmethod codegen ((insn ssa-clinit) &optional (stop-block nil))
  (with-slots (class) insn
    (list (intern (format nil "~A.<clinit>()" (slot-value (slot-value class 'class) 'name)) :openldk))))

(defmethod codegen ((insn ssa-local-variable) &optional (stop-block nil))
  (with-slots (index) insn
    (intern (format nil "local-~A" index) :openldk)))

(defmethod codegen ((insn ssa-monitorenter) &optional (stop-block nil))
  (flag-stack-usage *context*)
  (list 'monitor-enter (list 'pop-item)))

(defmethod codegen ((insn ssa-monitorexit) &optional (stop-block nil))
  (flag-stack-usage *context*)
  (list 'monitor-exit (list 'pop-item)))

(defmethod codegen ((insn ssa-mul) &optional (stop-block nil))
  (flag-stack-usage *context*)
  (list 'push-item (list '* (list 'pop-item) (list 'pop-item))))

(defmethod codegen ((insn ssa-new) &optional (stop-block nil))
  (with-slots (class) insn
    (with-slots (class) class
      (list 'make-instance (list 'quote (intern (slot-value class 'name) :openldk))))))

(defmethod codegen ((insn ssa-new-array) &optional (stop-block nil))
  (flag-stack-usage *context*)
  (list 'make-array (list 'pop-item)))

(defmethod codegen ((insn ssa-nop) &optional (stop-block nil))
  (gensym "NOP-"))

(defmethod codegen ((insn ssa-pop) &optional (stop-block nil))
  (flag-stack-usage *context*)
  (list 'pop-item))

(defmethod codegen ((insn ssa-push) &optional (stop-block nil))
  (flag-stack-usage *context*)
  (with-slots (value) insn
    (list 'push-item (codegen value))))

(defmethod codegen ((insn ssa-sub) &optional (stop-block nil))
  (flag-stack-usage *context*)
  (list 'let (list (list 'op2 (list 'pop-item))
                   (list 'op1 (list 'pop-item)))
        (list 'push-item (list '- 'op1 'op2))))

(defmethod codegen ((insn ssa-call-special-method) &optional (stop-block nil))
  (with-slots (class method-name args) insn
    (let ((call (list 'destructuring-bind (cons 'method 'next)
                      (list 'closer-mop:compute-applicable-methods-using-classes
                            (list 'function (intern (format nil "~A" method-name) :openldk))
                            ;; FIXME: This should be based on the args list
                            (cons 'list
                                  (cons (find-class (intern (slot-value class 'name) :openldk))
                                        (loop for a in args
                                              collect t))))
                      (list 'let (list (list 'fn (list 'closer-mop:method-function 'method)))
                            (list 'apply 'fn
                                  (list 'list (list 'reverse (cons 'list (mapcar (lambda (a) (codegen a)) args))) 'next))))))
      (if (void-return-p insn)
          call
          (list 'push-item call)))))

(defmethod codegen ((insn ssa-member) &optional (stop-block nil))
  (flag-stack-usage *context*)
  (with-slots (member-name) insn
    (list 'slot-value
          (list 'let (list (list 'objref (list 'pop-item)))
                (list 'when (list 'null 'objref) (list 'error
                                                       (format nil "Null Pointer Exception ~A" (slot-value insn 'address))))
                'objref)
          (list 'quote (intern member-name :openldk)))))

(defmethod codegen ((insn ssa-static-member) &optional (stop-block nil))
  (with-slots (class member-name) insn
    (list 'slot-value
          (intern (format nil "+static-~A+" (slot-value (slot-value class 'class) 'name)) :openldk)
          (list 'quote (intern member-name :openldk)))))

(defmethod codegen ((insn ssa-store) &optional (stop-block nil))
  (flag-stack-usage *context*)
  (with-slots (target) insn
    (list 'setf (codegen target) (list 'pop-item))))

(define-condition java-lang-throwable (error)
  ((throwable :initarg :throwable :reader throwable)))

(defun make-java-condition (e)
  (make-condition (gethash (class-of e) *condition-table*) :objref e))

(defmethod codegen ((insn ssa-throw) &optional (stop-block nil))
  (flag-stack-usage *context*)
  (list 'let (list (list 'c (list 'lisp-condition (list 'peek-item))))
        (list 'error 'c)))

(defmethod codegen ((insn ssa-return) &optional (stop-block nil))
  (list 'return))

(defmethod codegen ((insn ssa-return-value) &optional (stop-block nil))
  (flag-stack-usage *context*)
  (list 'return-from
        (intern (slot-value insn 'fn-name) :openldk)
        (list 'pop-item)))

(defmethod codegen ((insn ssa-variable) &optional (stop-block nil))
  (slot-value insn 'name))

(defmethod codegen ((basic-block <basic-block>) &optional (stop-block nil))
  (unless (equal basic-block stop-block)
    (if (not (slot-value basic-block 'code-emitted-p))
        (progn
          (push basic-block (slot-value *context* 'blocks))
          (let ((lisp-code
                  (cons (intern (format nil "branch-target-~A" (address (car (slot-value basic-block 'code)))))
                        (loop for insn in (slot-value basic-block 'code)
                              collect (codegen insn)))))
            (setf (slot-value basic-block 'code-emitted-p) t)
            (pop (slot-value *context* 'blocks))
            ;; sort by address
            (let ((successor-list (sort (fset:convert 'list (successors basic-block)) (lambda (a b) (< (address a) (address b))))))
              (dolist (successor successor-list)
                (when successor
                  (setf lisp-code (append lisp-code (codegen successor (or stop-block (try-exit-block basic-block))))))))

            ;; Emit handlers for finally handlers. FIXME: in build-basic-blocks, sort try-catch list by end of range
            (when (find-if (lambda (p) (null (car p))) (try-catch basic-block))
              ;; This is a TRY-CATCH block.  Wrap this in HANDLER-CASE.
              (loop for tc in (reverse (try-catch basic-block))
                    unless (car tc)
                      do (setf lisp-code (append (list (list 'handler-case
                                                             (cons 'tagbody lisp-code)
                                                             (list 'condition (list (intern "condition" :openldk))
                                                                   (cons 'tagbody
                                                                         (codegen (cdr tc) (try-exit-block basic-block))))))
                                                 (when (try-exit-block basic-block)
                                                   (codegen (try-exit-block basic-block)))))))

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
                                                                                        (codegen (cdr tc) (try-exit-block basic-block))))))))
                                      (when (try-exit-block basic-block)
                                        (codegen (try-exit-block basic-block))))))

            lisp-code))
        nil)))
