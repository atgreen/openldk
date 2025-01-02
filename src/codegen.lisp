;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: OPENLDK; Base: 10 -*-
;;;
;;; Copyright (C) 2023, 2024  Anthony Green <green@moxielogic.com>
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

(defmethod codegen ((insn ssa-literal))
  (slot-value insn 'value))

(defmethod codegen ((insn ssa-string-literal))
  (let ((s (make-instance '|java/lang/String|)))
    (setf (slot-value s '|value|) (slot-value insn 'value))
    s))

(defmethod codegen ((insn ssa-aaload))
  (flag-stack-usage *context*)
  (list 'let (list (list 'index (list 'pop-item 'stack))
                   (list 'arrayref (list 'pop-item 'stack)))
        (list 'push-item 'stack (list 'aref 'arrayref 'index))))

(defmethod codegen ((insn ssa-aastore))
  (flag-stack-usage *context*)
  (list 'let (list (list 'arrayref (list 'pop-item 'stack))
                   (list 'index (list 'pop-item 'stack))
                   (list 'value (list 'pop-item 'stack)))
        (list 'setf (list 'aref 'arrayref 'index) 'value)))

(defmethod codegen ((insn ssa-add))
  (flag-stack-usage *context*)
  (list 'push-item 'stack (list '+ (list 'pop-item 'stack) (list 'pop-item 'stack))))

(defmethod codegen ((insn ssa-array-length))
  (list 'push-item 'stack (list 'length (list 'pop-item 'stack))))

(defmethod codegen ((insn ssa-assign))
  (with-slots (source target) insn
    (list 'let (list (list 'value (codegen source)))
          (list 'setf (codegen target) 'value))))

(defmethod codegen ((insn ssa-call-static-method))
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
      (if (void-return-p method-name)
          call
          (list 'push-item 'stack call)))))

(defmethod codegen ((insn ssa-checkcast))
  (intern (format nil "#:checkcast-FIXME-~A" (slot-value insn 'index)) :openldk))

(defmethod codegen ((insn ssa-class))
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

(defmethod codegen ((insn ssa-branch-target))
  (intern (format nil "branch-target-~A" (slot-value insn 'index))))

(defmethod codegen ((insn ssa-div))
  (flag-stack-usage *context*)
  (list 'handler-case
	(list 'let (list (list 'op2 (list 'pop-item 'stack))
			 (list 'op1 (list 'pop-item 'stack)))
	      (list 'push-item 'stack (list '/ 'op1 'op2)))
        (list 'division-by-zero (list 'e) (list 'error (list 'make-condition (list 'quote '|condition-java/lang/ArithmeticException|))))))

(defmethod codegen ((insn ssa-dup))
  (flag-stack-usage *context*)
  (list 'push-item 'stack (list 'car 'stack)))

(defmethod codegen ((insn ssa-iinc))
  (with-slots (index const) insn
    (list 'incf (intern (format nil "local-~A" index) :openldk) const)))

(defmethod codegen ((insn ssa-if-acmpeq))
  (flag-stack-usage *context*)
  (with-slots (offset) insn
    (list 'if (list 'eq (list 'pop-item 'stack) (list 'pop-item 'stack))
          (list 'go (intern (format nil "branch-target-~A" offset))))))

(defmethod codegen ((insn ssa-if-acmpne))
  (flag-stack-usage *context*)
  (with-slots (offset) insn
    (list 'if (list 'not (list 'eq (list 'pop-item 'stack) (list 'pop-item 'stack)))
          (list 'go (intern (format nil "branch-target-~A" offset))))))

(defmethod codegen ((insn ssa-if-icmpeq))
  (flag-stack-usage *context*)
  (with-slots (offset) insn
    (list 'if (list 'equal (list 'pop-item 'stack) (list 'pop-item 'stack))
          (list 'go (intern (format nil "branch-target-~A" offset))))))

(defmethod codegen ((insn ssa-if-icmple))
  (flag-stack-usage *context*)
  (with-slots (offset) insn
    (list 'if (list '>= (list 'pop-item 'stack) (list 'pop-item 'stack))
          (list 'go (intern (format nil "branch-target-~A" offset))))))

(defmethod codegen ((insn ssa-if-icmpge))
  (flag-stack-usage *context*)
  (with-slots (offset) insn
    (list 'if (list '<= (list 'pop-item 'stack) (list 'pop-item 'stack))
          (list 'go (intern (format nil "branch-target-~A" offset))))))

(defmethod codegen ((insn ssa-if-icmpne))
  (flag-stack-usage *context*)
  (with-slots (offset) insn
    (list 'if (list 'not (list 'eq (list 'pop-item 'stack) (list 'pop-item 'stack)))
          (list 'go (intern (format nil "branch-target-~A" offset))))))

(defmethod codegen ((insn ssa-ifeq))
  (flag-stack-usage *context*)
  (with-slots (offset) insn
    (list 'if (list 'eq (list 'pop-item 'stack) 0)
          (list 'go (intern (format nil "branch-target-~A" offset))))))

(defmethod codegen ((insn ssa-ifge))
  (flag-stack-usage *context*)
  (with-slots (offset) insn
    (list 'progn
	  (list 'if (list '>= (list 'pop-item 'stack) 0)
          (list 'go (intern (format nil "branch-target-~A" offset)))))))

(defmethod codegen ((insn ssa-ifle))
  (flag-stack-usage *context*)
  (with-slots (offset) insn
    (list 'progn
	  (list 'if (list '<= (list 'pop-item 'stack) 0)
          (list 'go (intern (format nil "branch-target-~A" offset)))))))

(defmethod codegen ((insn ssa-ifne))
  (flag-stack-usage *context*)
  (with-slots (offset) insn
    (list 'if (list 'not (list 'eq (list 'pop-item 'stack) '0))
          (list 'go (intern (format nil "branch-target-~A" offset))))))

(defmethod codegen ((insn ssa-ifnonnull))
  (flag-stack-usage *context*)
  (with-slots (offset) insn
    (list 'if (list 'not (list 'null (list 'pop-item 'stack)))
          (list 'go (intern (format nil "branch-target-~A" offset))))))

(defmethod codegen ((insn ssa-ifnull))
  (flag-stack-usage *context*)
  (with-slots (offset) insn
    (list 'if (list 'null (list 'pop-item 'stack))
          (list 'go (intern (format nil "branch-target-~A" offset))))))

(defmethod codegen ((insn ssa-instanceof))
  (with-slots (class) insn
    (list 'push-item 'stack (list 'typep (list 'pop-item 'stack) class))))

(defmethod codegen ((insn ssa-ishl))
  (flag-stack-usage *context*)
  (list 'let (list (list 'op2 (list 'pop-item 'stack))
                   (list 'op1 (list 'pop-item 'stack)))
	(list 'progn
	      (list 'format 't "ISHL ~A ~A~%" 'op1 'op2)
	      (list 'push-item 'stack (list 'ash 'op1 'op2)))))

(defmethod codegen ((insn ssa-lushr))
  (flag-stack-usage *context*)
  (list 'let (list (list 'op2 (list 'pop-item 'stack))
                   (list 'op1 (list 'pop-item 'stack)))
        (list 'progn
              (list 'format 't "LUSHR ~A ~A~%" 'op1 'op2)
              (list 'push-item 'stack (list 'ash 'op1 (list \- 'op2))))))

(defmethod codegen ((insn ssa-goto))
  (with-slots (offset) insn
    (list 'go (intern (format nil "branch-target-~A" offset)))))

(defmethod codegen ((insn ssa-call-virtual-method))
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
      (if (void-return-p method-name)
	  call
	  (list 'push-item 'stack call)))))

(defmethod codegen ((insn ssa-clinit))
  (with-slots (class) insn
    (list (intern (format nil "~A.<clinit>()V" (slot-value (slot-value class 'class) 'name)) :openldk))))

(defmethod codegen ((insn ssa-local-variable))
  (with-slots (index) insn
    (intern (format nil "local-~A" index) :openldk)))

(defmethod codegen ((insn ssa-mul))
  (flag-stack-usage *context*)
  (list 'push-item 'stack (list '* (list 'pop-item 'stack) (list 'pop-item 'stack))))

(defmethod codegen ((insn ssa-new))
  (with-slots (class) insn
    (with-slots (class) class
      (list 'make-instance (list 'quote (intern (slot-value class 'name) :openldk))))))

(defmethod codegen ((insn ssa-new-array))
  (flag-stack-usage *context*)
  (list 'make-array (list 'pop-item 'stack)))

(defmethod codegen ((insn ssa-nop))
  (gensym "NOP-"))

(defmethod codegen ((insn ssa-pop))
  (flag-stack-usage *context*)
  (list 'pop-item 'stack))

(defmethod codegen ((insn ssa-push))
  (flag-stack-usage *context*)
  (with-slots (value) insn
    (list 'push-item 'stack (codegen value))))

(defmethod codegen ((insn ssa-sub))
  (flag-stack-usage *context*)
  (list 'let (list (list 'op2 (list 'pop-item 'stack))
		   (list 'op1 (list 'pop-item 'stack)))
	(list 'push-item 'stack (list '- 'op1 'op2))))

(defmethod codegen ((insn ssa-call-special-method))
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
      (if (void-return-p method-name)
	  call
	  (list 'push-item 'stack call)))))

(defmethod codegen ((insn ssa-member))
  (flag-stack-usage *context*)
  (with-slots (member-name) insn
    (list 'slot-value
          (list 'let (list (list 'objref (list 'pop-item 'stack)))
                (list 'when (list 'null 'objref) (list 'error
                                                       (format nil "Null Pointer Exception ~A" (slot-value insn 'address))))
                'objref)
          (list 'quote (intern member-name :openldk)))))

(defmethod codegen ((insn ssa-static-member))
  (with-slots (class member-name) insn
    (list 'slot-value
          (intern (format nil "+static-~A+" (slot-value (slot-value class 'class) 'name)) :openldk)
          (list 'quote (intern member-name :openldk)))))

(defmethod codegen ((insn ssa-store))
  (flag-stack-usage *context*)
  (with-slots (target) insn
    (list 'setf (codegen target) (list 'pop-item 'stack))))

(define-condition java-lang-throwable (error)
  ((throwable :initarg :throwable :reader throwable)))

(defun make-java-condition (e)
  (make-condition (gethash (class-of e) *condition-table*) :objref e))

(defmethod codegen ((insn ssa-throw))
  (flag-stack-usage *context*)
  (list 'let* (list (list 'e (list 'pop-item 'stack))
                    (list 'c (list 'make-java-condition 'e)))
        (list 'error 'c)))

(defmethod codegen ((insn ssa-return))
  (list 'return))

(defmethod codegen ((insn ssa-return-value))
  (flag-stack-usage *context*)
  (list 'return-from
        (intern (slot-value insn 'fn-name) :openldk)
        (list 'pop-item 'stack)))

(defmethod codegen ((insn ssa-variable))
  (slot-value insn 'name))

(defmethod codegen ((basic-block <basic-block>))
  (if (not (slot-value basic-block 'code-emitted-p))
      (progn
        (push basic-block (slot-value *context* 'blocks))
        (let ((lisp-code
                (cons (intern (format nil "branch-target-~A" (address (car (slot-value basic-block 'code)))()))
                      (loop for insn in (slot-value basic-block 'code)
                            collect (codegen insn)))))
          (setf (slot-value basic-block 'code-emitted-p) t)
          (pop (slot-value *context* 'blocks))
          (fset:do-set (successor (successors basic-block))
            (when successor
              (setf lisp-code (append lisp-code (codegen successor)))))
					(if (try-catch basic-block)
							(setf lisp-code (list (append (list 'handler-case)
																						(list (append (list 'tagbody) lisp-code))
																						(loop for tc in (try-catch basic-block)
																									collect (append (list (intern (format nil "condition-~A" (car tc)) :openldk)
																																				(list (intern "condition" :openldk)))
																																	(list (cons 'tagbody
																																							(codegen (cdr tc))))))))))
          lisp-code))
      nil))
