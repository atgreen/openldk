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

(defmethod codegen ((insn ssa-castore))
  (flag-stack-usage *context*)
  (list 'let (list (list 'arrayref (list 'pop-item 'stack))
                   (list 'index (list 'pop-item 'stack))
                   (list 'value (list 'pop-item 'stack)))
        (list 'setf (list 'aref 'arrayref 'index) 'value)))

(defmethod codegen ((insn ssa-iastore))
  (flag-stack-usage *context*)
  (list 'let (list (list 'arrayref (list 'pop-item 'stack))
                   (list 'index (list 'pop-item 'stack))
                   (list 'value (list 'pop-item 'stack)))
        (list 'setf (list 'aref 'arrayref 'index) 'value)))

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

(defmethod codegen ((insn ssa-dup-x1))
  (flag-stack-usage *context*)
	(list 'let (list (list 'p1 (list 'pop-item 'stack))
                   (list 'p2 (list 'pop-item 'stack)))
        (list 'push-item 'stack 'p1)
        (list 'push-item 'stack 'p2)
        (list 'push-item 'stack 'p1)))

(defmethod codegen ((insn ssa-iaload))
  (flag-stack-usage *context*)
  (list 'let (list (list 'index (list 'pop-item 'stack))
                   (list 'arrayref (list 'pop-item 'stack)))
        (list 'push-item 'stack (list 'aref 'arrayref 'index))))

(defmethod codegen ((insn ssa-ineg))
  (flag-stack-usage *context*)
  (list 'push-item 'stack (list '- (list 'car 'stack))))

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

(defmethod codegen ((insn ssa-if-icmpgt))
  (flag-stack-usage *context*)
  (with-slots (offset) insn
    (list 'if (list '< (list 'pop-item 'stack) (list 'pop-item 'stack))
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
	      (list 'push-item 'stack (list 'ash 'op1 'op2))))

(defmethod codegen ((insn ssa-ishr))
  (flag-stack-usage *context*)
  (list 'let (list (list 'op2 (list 'pop-item 'stack))
                   (list 'op1 (list 'pop-item 'stack)))
	      (list 'push-item 'stack (list 'ash 'op1 (list '- 'op2)))))

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
                (cons (intern (format nil "branch-target-~A" (.address (car (slot-value basic-block 'code)))()))
                      ;; (intern (format nil ":branch-target-~A" (.address (car (slot-value basic-block 'code)))) :openldk)
                      (loop for insn in (slot-value basic-block 'code)
                            collect (codegen insn)))))
          (setf (slot-value basic-block 'code-emitted-p) t)
          (pop (slot-value *context* 'blocks))
          (dolist (successor (.successor-blocks basic-block))
            (when successor
              (setf lisp-code (append lisp-code (codegen successor)))))
          lisp-code))
      nil))

(defmethod codegen ((try-block <try-block>))
  (push try-block (slot-value *context* 'blocks))
  (let ((lisp-code (codegen (.try-body try-block))))
    (pop (slot-value *context* 'blocks))
    (list (list 'handler-case
                (cons 'tagbody
                      lisp-code)
                (append (list
                         (intern (format nil "condition-~A" (car (car (slot-value try-block 'catch-blocks)))) :openldk)
                         (list (intern "condition" :openldk)))
                        (list (cons 'tagbody
                                    (codegen (cdr (car (slot-value try-block 'catch-blocks)))))))))))
