(in-package :openldk)

(defmethod codegen ((insn ssa-literal))
  (slot-value insn 'value))

(defmethod codegen ((insn ssa-string-literal))
  (let ((s (make-instance '|java/lang/String|)))
    (setf (slot-value s '|value|) (slot-value insn 'value))
    s))

(defmethod codegen ((insn ssa-add))
  (list 'cl-containers:push-item 'stack (list '+ (list 'cl-containers:pop-item 'stack) (list 'cl-containers:pop-item 'stack))))

(defmethod codegen ((insn ssa-assign))
  (with-slots (source target) insn
    (list 'let (list (list 'value (codegen source)))
          (list 'setf (codegen target) 'value))))

(defmethod codegen ((insn ssa-call-static-method))
  (with-slots (class method-name args) insn
    (list 'apply
          (list 'function (intern (format nil "~A.~A"
                                          class
                                          method-name)
                                  :openldk))
          (list 'reverse (cons 'list (mapcar (lambda (a) (codegen a)) args))))))

(defvar *java-classes* (make-hash-table :test #'equal))
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
  (intern (format nil "#:branch-target-~A" (slot-value insn 'index)) :openldk))

(defmethod codegen ((insn ssa-div))
  (list 'cl-containers:push-item 'stack (list '/ (list 'cl-containers:pop-item 'stack) (list 'cl-containers:pop-item 'stack))))

(defmethod codegen ((insn ssa-dup))
  (list 'cl-containers:push-item 'stack (list 'cl-containers:first-element 'stack)))

(defmethod codegen ((insn ssa-if-icmple))
  (with-slots (offset) insn
    (list 'if (list '>= (list 'cl-containers:pop-item 'stack) (list 'cl-containers:pop-item 'stack))
          (list 'go (intern (format nil "#:branch-target-~A" offset) :openldk)))))

(defmethod codegen ((insn ssa-ifeq))
  (with-slots (offset) insn
    (list 'if (list 'eq (list 'cl-containers:pop-item 'stack) 0)
          (list 'go (intern (format nil "#:branch-target-~A" offset) :openldk)))))

(defmethod codegen ((insn ssa-ifge))
  (with-slots (offset) insn
    (list 'if (list '>= (list 'cl-containers:pop-item 'stack) 0)
          (list 'go (intern (format nil "#:branch-target-~A" offset) :openldk)))))

(defmethod codegen ((insn ssa-ifle))
  (with-slots (offset) insn
    (list 'if (list '<= (list 'cl-containers:pop-item 'stack) 0)
          (list 'go (intern (format nil "#:branch-target-~A" offset) :openldk)))))

(defmethod codegen ((insn ssa-ifne))
  (with-slots (offset) insn
    (list 'if (list 'not (list 'eq (list 'cl-containers:pop-item 'stack) '0))
          (list 'go (intern (format nil "#:branch-target-~A" offset) :openldk)))))

(defmethod codegen ((insn ssa-ifnonnull))
  (with-slots (offset) insn
    (list 'if (list 'not (list 'null (list 'cl-containers:pop-item 'stack)))
          (list 'go (intern (format nil "#:branch-target-~A" offset) :openldk)))))

(defmethod codegen ((insn ssa-ifnull))
  (with-slots (offset) insn
    (list 'if (list 'null (list 'cl-containers:pop-item 'stack))
          (list 'go (intern (format nil "#:branch-target-~A" offset) :openldk)))))

(defmethod codegen ((insn ssa-instanceof))
  (with-slots (class) insn
    (format t "MMMMMMMMMMMMMM ~A~%" class)))

(defmethod codegen ((insn ssa-goto))
  (with-slots (offset) insn
    (list 'go (intern (format nil "#:branch-target-~A" offset) :openldk))))

(defmethod codegen ((insn ssa-call-virtual-method))
  (with-slots (method-name args) insn
    (list 'apply
          (list 'function (intern (format nil "~A"
                                          method-name) :openldk))
          (list 'reverse (cons 'list (mapcar (lambda (a) (codegen a)) args))))))

(defmethod codegen ((insn ssa-clinit))
  (with-slots (class) insn
    (list (intern (format nil "~A.<clinit>()V" (slot-value (slot-value class 'class) 'name)) :openldk))))

(defmethod codegen ((insn ssa-local-variable))
  (with-slots (index) insn
    (intern (format nil "local-~A" index) :openldk)))

(defmethod codegen ((insn ssa-mul))
  (list 'cl-containers:push-item 'stack (list '* (list 'cl-containers:pop-item 'stack) (list 'cl-containers:pop-item 'stack))))

(defmethod codegen ((insn ssa-new))
  (with-slots (class) insn
    (with-slots (class) class
      (list 'make-instance (list 'quote (intern (slot-value class 'name) :openldk))))))

(defmethod codegen ((insn ssa-new-array))
  (list 'make-array (list 'cl-containers:pop-item 'stack)))

(defmethod codegen ((insn ssa-nop))
  (gensym "NOP-"))

(defmethod codegen ((insn ssa-pop))
  (list 'cl-containers:pop-item 'stack))

(defmethod codegen ((insn ssa-push))
  (with-slots (value) insn
    (list 'cl-containers:push-item 'stack (codegen value))))

(defmethod codegen ((insn ssa-sub))
  (list 'cl-containers:push-item 'stack (list '- (list 'cl-containers:pop-item 'stack) (list 'cl-containers:pop-item 'stack))))

(defmethod codegen ((insn ssa-call-special-method))
  (with-slots (class method-name args) insn
    (list 'destructuring-bind (cons 'method 'next)
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

#|
(defmethod codegen ((insn ssa-call-special-method))
  ; (classload "java/lang/String" ".:jre8")
  (with-slots (class-name method-name args) insn
    (destructuring-bind (method . next)
        (closer-mop:compute-applicable-methods-using-classes
         (eval (list 'function (intern (format nil "~A" method-name) :openldk)))
         (list (find-class (intern class-name :openldk)) nil))
      (print "===================================================")
      (print method)
      (print (closer-mop:method-function method))
      (print "===================================================")
      (print next)
      (print "===================================================")
      (let ((fn (closer-mop:method-function method)))
        (list 'apply fn
              (list 'list (list 'reverse (cons 'list (mapcar (lambda (a) (codegen a)) args))) (cons 'list next)))))))
|#

(defmethod codegen ((insn ssa-member))
  (with-slots (member-name) insn
    (list 'slot-value
          (list 'let (list (list 'objref (list 'cl-containers:pop-item 'stack)))
                (list 'when (list 'null 'objref) (list 'error
                                                       (format nil "Null Pointer Exception ~A" (slot-value insn 'pc-index))))
                'objref)
          (list 'quote (intern member-name :openldk)))))

(defmethod codegen ((insn ssa-static-member))
  (with-slots (class member-name) insn
    (list 'slot-value
          (intern (format nil "+static-~A+" (slot-value (slot-value class 'class) 'name)) :openldk)
          (list 'quote (intern member-name :openldk)))))

(defmethod codegen ((insn ssa-store))
  (with-slots (target) insn
    (list 'setf (codegen target) (list 'cl-containers:pop-item 'stack))))

(define-condition java-lang-throwable (error)
  ((throwable :initarg :throwable :reader throwable)))

(defun make-java-condition (e)
  (make-condition (gethash (class-of e) *condition-table*) :objref e))

(defmethod codegen ((insn ssa-throw))
  (list 'let* (list (list 'e (list 'cl-containers:pop-item 'stack))
                    (list 'c (list 'make-java-condition 'e)))
        (list 'error 'c)))

(defmethod codegen ((insn ssa-return))
  (list 'return))

(defmethod codegen ((insn ssa-return-value))
  (list 'return-from
        (intern (slot-value insn 'fn-name) :openldk)
        (list 'cl-containers:pop-item 'stack)))

(defmethod codegen ((insn ssa-variable))
  (slot-value insn 'name))
