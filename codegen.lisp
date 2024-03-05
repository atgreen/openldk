(in-package :openldk)

(defmethod codegen ((insn ir-literal))
  (slot-value insn 'value))

(defmethod codegen ((insn ir-string-literal))
  (let ((s (make-instance '|java/lang/String|)))
    (setf (slot-value s '|value|) (slot-value insn 'value))
    s))

(defmethod codegen ((insn ir-aaload))
  (flag-stack-usage *context*)
  (list 'let (list (list 'index (list 'pop-item 'stack))
                   (list 'arrayref (list 'pop-item 'stack)))
        (list 'push-item 'stack (list 'aref 'arrayref 'index))))

(defmethod codegen ((insn ir-aastore))
  (flag-stack-usage *context*)
  (list 'let (list (list 'arrayref (list 'pop-item 'stack))
                   (list 'index (list 'pop-item 'stack))
                   (list 'value (list 'pop-item 'stack)))
        (list 'setf (list 'aref 'arrayref 'index) 'value)))

(defmethod codegen ((insn ir-add))
  (flag-stack-usage *context*)
  (list 'push-item 'stack (list '+ (list 'pop-item 'stack) (list 'pop-item 'stack))))

(defmethod codegen ((insn ir-array-length))
  (list 'push-item 'stack (list 'length (list 'pop-item 'stack))))

(defmethod codegen ((insn ir-assign))
  (with-slots (source target) insn
    (list 'let (list (list 'value (codegen source)))
          (list 'setf (codegen target) 'value))))

(defmethod codegen ((insn ir-call-static-method))
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

(defmethod codegen ((insn ir-castore))
  (flag-stack-usage *context*)
  (list 'let (list (list 'arrayref (list 'pop-item 'stack))
                   (list 'index (list 'pop-item 'stack))
                   (list 'value (list 'pop-item 'stack)))
        (list 'setf (list 'aref 'arrayref 'index) 'value)))

(defmethod codegen ((insn ir-iastore))
  (flag-stack-usage *context*)
  (list 'let (list (list 'arrayref (list 'pop-item 'stack))
                   (list 'index (list 'pop-item 'stack))
                   (list 'value (list 'pop-item 'stack)))
        (list 'setf (list 'aref 'arrayref 'index) 'value)))

(defmethod codegen ((insn ir-checkcast))
  (intern (format nil "#:checkcast-FIXME-~A" (slot-value insn 'index)) :openldk))

(defmethod codegen ((insn ir-class))
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

(defmethod codegen ((insn ir-branch-target))
  (intern (format nil "branch-target-~A" (slot-value insn 'index))))

(defmethod codegen ((insn ir-div))
  (flag-stack-usage *context*)
  (list 'handler-case
	(list 'let (list (list 'op2 (list 'pop-item 'stack))
			 (list 'op1 (list 'pop-item 'stack)))
	      (list 'push-item 'stack (list '/ 'op1 'op2)))
        (list 'division-by-zero (list 'e) (list 'error (list 'make-condition (list 'quote '|condition-java/lang/ArithmeticException|))))))

(defmethod codegen ((insn ir-dup))
  (flag-stack-usage *context*)
  (list 'push-item 'stack (list 'car 'stack)))

(defmethod codegen ((insn ir-dup-x1))
  (flag-stack-usage *context*)
	(list 'let (list (list 'p1 (list 'pop-item 'stack))
                   (list 'p2 (list 'pop-item 'stack)))
        (list 'push-item 'stack 'p1)
        (list 'push-item 'stack 'p2)
        (list 'push-item 'stack 'p1)))

(defmethod codegen ((insn ir-iaload))
  (flag-stack-usage *context*)
  (list 'let (list (list 'index (list 'pop-item 'stack))
                   (list 'arrayref (list 'pop-item 'stack)))
        (list 'push-item 'stack (list 'aref 'arrayref 'index))))

(defmethod codegen ((insn ir-ineg))
  (flag-stack-usage *context*)
  (list 'push-item 'stack (list '- (list 'car 'stack))))

(defmethod codegen ((insn ir-iinc))
  (with-slots (index const) insn
    (list 'incf (intern (format nil "local-~A" index) :openldk) const)))

(defmethod codegen ((insn ir-if-acmpeq))
  (flag-stack-usage *context*)
  (with-slots (offset) insn
    (list 'if (list 'eq (list 'pop-item 'stack) (list 'pop-item 'stack))
          (list 'go (intern (format nil "branch-target-~A" offset))))))

(defmethod codegen ((insn ir-if-acmpne))
  (flag-stack-usage *context*)
  (with-slots (offset) insn
    (list 'if (list 'not (list 'eq (list 'pop-item 'stack) (list 'pop-item 'stack)))
          (list 'go (intern (format nil "branch-target-~A" offset))))))

(defmethod codegen ((insn ir-if-icmpeq))
  (flag-stack-usage *context*)
  (with-slots (offset) insn
    (list 'if (list 'equal (list 'pop-item 'stack) (list 'pop-item 'stack))
          (list 'go (intern (format nil "branch-target-~A" offset))))))

(defmethod codegen ((insn ir-if-icmple))
  (flag-stack-usage *context*)
  (with-slots (offset) insn
    (list 'if (list '>= (list 'pop-item 'stack) (list 'pop-item 'stack))
          (list 'go (intern (format nil "branch-target-~A" offset))))))

(defmethod codegen ((insn ir-if-icmpge))
  (flag-stack-usage *context*)
  (with-slots (offset) insn
    (list 'if (list '<= (list 'pop-item 'stack) (list 'pop-item 'stack))
          (list 'go (intern (format nil "branch-target-~A" offset))))))

(defmethod codegen ((insn ir-if-icmpgt))
  (flag-stack-usage *context*)
  (with-slots (offset) insn
    (list 'if (list '< (list 'pop-item 'stack) (list 'pop-item 'stack))
          (list 'go (intern (format nil "branch-target-~A" offset))))))

(defmethod codegen ((insn ir-if-icmpne))
  (flag-stack-usage *context*)
  (with-slots (offset) insn
    (list 'if (list 'not (list 'eq (list 'pop-item 'stack) (list 'pop-item 'stack)))
          (list 'go (intern (format nil "branch-target-~A" offset))))))

(defmethod codegen ((insn ir-ifeq))
  (flag-stack-usage *context*)
  (with-slots (offset) insn
    (list 'if (list 'eq (list 'pop-item 'stack) 0)
          (list 'go (intern (format nil "branch-target-~A" offset))))))

(defmethod codegen ((insn ir-ifge))
  (flag-stack-usage *context*)
  (with-slots (offset) insn
    (list 'progn
	  (list 'if (list '>= (list 'pop-item 'stack) 0)
          (list 'go (intern (format nil "branch-target-~A" offset)))))))

(defmethod codegen ((insn ir-ifle))
  (flag-stack-usage *context*)
  (with-slots (offset) insn
    (list 'progn
	  (list 'if (list '<= (list 'pop-item 'stack) 0)
          (list 'go (intern (format nil "branch-target-~A" offset)))))))

(defmethod codegen ((insn ir-ifne))
  (flag-stack-usage *context*)
  (with-slots (offset) insn
    (list 'if (list 'not (list 'eq (list 'pop-item 'stack) '0))
          (list 'go (intern (format nil "branch-target-~A" offset))))))

(defmethod codegen ((insn ir-ifnonnull))
  (flag-stack-usage *context*)
  (with-slots (offset) insn
    (list 'if (list 'not (list 'null (list 'pop-item 'stack)))
          (list 'go (intern (format nil "branch-target-~A" offset))))))

(defmethod codegen ((insn ir-ifnull))
  (flag-stack-usage *context*)
  (with-slots (offset) insn
    (list 'if (list 'null (list 'pop-item 'stack))
          (list 'go (intern (format nil "branch-target-~A" offset))))))

(defmethod codegen ((insn ir-instanceof))
  (with-slots (class) insn
    (list 'push-item 'stack (list 'typep (list 'pop-item 'stack) class))))

(defmethod codegen ((insn ir-ishl))
  (flag-stack-usage *context*)
  (list 'let (list (list 'op2 (list 'pop-item 'stack))
                   (list 'op1 (list 'pop-item 'stack)))
	      (list 'push-item 'stack (list 'ash 'op1 'op2))))

(defmethod codegen ((insn ir-ishr))
  (flag-stack-usage *context*)
  (list 'let (list (list 'op2 (list 'pop-item 'stack))
                   (list 'op1 (list 'pop-item 'stack)))
	      (list 'push-item 'stack (list 'ash 'op1 (list '- 'op2)))))

(defmethod codegen ((insn ir-lushr))
  (flag-stack-usage *context*)
  (list 'let (list (list 'op2 (list 'pop-item 'stack))
                   (list 'op1 (list 'pop-item 'stack)))
        (list 'progn
              (list 'format 't "LUSHR ~A ~A~%" 'op1 'op2)
              (list 'push-item 'stack (list 'ash 'op1 (list \- 'op2))))))

(defmethod codegen ((insn ir-goto))
  (with-slots (offset) insn
    (list 'go (intern (format nil "branch-target-~A" offset)))))

(defmethod codegen ((insn ir-call-virtual-method))
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

(defmethod codegen ((insn ir-clinit))
  (with-slots (class) insn
    (list (intern (format nil "~A.<clinit>()V" (slot-value (slot-value class 'class) 'name)) :openldk))))

(defmethod codegen ((insn ir-local-variable))
  (with-slots (index) insn
    (intern (format nil "local-~A" index) :openldk)))

(defmethod codegen ((insn ir-mul))
  (flag-stack-usage *context*)
  (list 'push-item 'stack (list '* (list 'pop-item 'stack) (list 'pop-item 'stack))))

(defmethod codegen ((insn ir-new))
  (with-slots (class) insn
    (with-slots (class) class
      (list 'make-instance (list 'quote (intern (slot-value class 'name) :openldk))))))

(defmethod codegen ((insn ir-new-array))
  (flag-stack-usage *context*)
  (list 'make-array (list 'pop-item 'stack)))

(defmethod codegen ((insn ir-nop))
  (gensym "NOP-"))

(defmethod codegen ((insn ir-pop))
  (flag-stack-usage *context*)
  (list 'pop-item 'stack))

(defmethod codegen ((insn ir-push))
  (flag-stack-usage *context*)
  (with-slots (value) insn
    (list 'push-item 'stack (codegen value))))

(defmethod codegen ((insn ir-sub))
  (flag-stack-usage *context*)
  (list 'let (list (list 'op2 (list 'pop-item 'stack))
		   (list 'op1 (list 'pop-item 'stack)))
	(list 'push-item 'stack (list '- 'op1 'op2))))

(defmethod codegen ((insn ir-call-special-method))
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

(defmethod codegen ((insn ir-member))
  (flag-stack-usage *context*)
  (with-slots (member-name) insn
    (list 'slot-value
          (list 'let (list (list 'objref (list 'pop-item 'stack)))
                (list 'when (list 'null 'objref) (list 'error
                                                       (format nil "Null Pointer Exception ~A" (slot-value insn 'address))))
                'objref)
          (list 'quote (intern member-name :openldk)))))

(defmethod codegen ((insn ir-static-member))
  (with-slots (class member-name) insn
    (list 'slot-value
          (intern (format nil "+static-~A+" (slot-value (slot-value class 'class) 'name)) :openldk)
          (list 'quote (intern member-name :openldk)))))

(defmethod codegen ((insn ir-store))
  (flag-stack-usage *context*)
  (with-slots (target) insn
    (list 'setf (codegen target) (list 'pop-item 'stack))))

(define-condition java-lang-throwable (error)
  ((throwable :initarg :throwable :reader throwable)))

(defun make-java-condition (e)
  (make-condition (gethash (class-of e) *condition-table*) :objref e))

(defmethod codegen ((insn ir-throw))
  (flag-stack-usage *context*)
  (list 'let* (list (list 'e (list 'pop-item 'stack))
                    (list 'c (list 'make-java-condition 'e)))
        (list 'error 'c)))

(defmethod codegen ((insn ir-return))
  (list 'return))

(defmethod codegen ((insn ir-return-value))
  (flag-stack-usage *context*)
  (list 'return-from
        (intern (slot-value insn 'fn-name) :openldk)
        (list 'pop-item 'stack)))

(defmethod codegen ((insn ir-variable))
  (slot-value insn 'name))

(defmethod codegen ((bloc <block>))
  (if (not (slot-value bloc 'code-emitted-p))
      (progn
        (push bloc (slot-value *context* 'blocks))
        (let ((lisp-code
                (cons (intern (format nil "branch-target-~A" (.address (car (slot-value bloc 'code)))()))
                      ;; (intern (format nil ":branch-target-~A" (.address (car (slot-value bloc 'code)))) :openldk)
                      (loop for insn in (slot-value bloc 'code)
                            collect (codegen insn)))))
          (setf (slot-value bloc 'code-emitted-p) t)
          (pop (slot-value *context* 'blocks))
          (dolist (successor (.successor-blocks bloc))
            (when successor
              (setf lisp-code (append lisp-code (codegen successor)))))
          (if (null (slot-value bloc 'catch-blocks))
              lisp-code
              (list (list 'handler-case
                          (cons 'tagbody lisp-code)
                          (append (list
                                   (intern (format nil "condition-~A" (car (car (slot-value bloc 'catch-blocks)))) :openldk)
                                   (list (intern "condition" :openldk)))
                                  (list (cons 'tagbody
                                              (codegen (cdr (car (slot-value bloc 'catch-blocks))))))))))))))
