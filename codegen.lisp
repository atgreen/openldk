(in-package :openldk)

(defmethod codegen ((insn ssa-literal))
  (slot-value insn 'value))

(defmethod codegen ((insn ssa-add))
  (list 'cl-containers:push-item 'stack (list '+ (list 'cl-containers:pop-item 'stack) (list 'cl-containers:pop-item 'stack))))

(defmethod codegen ((insn ssa-assign))
  (with-slots (source target) insn
    (list 'setf (codegen target) (codegen source))))

(defmethod codegen ((insn ssa-call-static-method))
  (with-slots (class-name method-name args) insn
    (list 'apply
          (list 'function (intern (format nil "~A.~A"
                                          class-name
                                          method-name)
                                  :openldk))
          (list 'reverse (cons 'list (mapcar (lambda (a) (codegen a)) args))))))

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
  (with-slots (class-name) insn
    (list (intern "<clinit>()V" :openldk) (intern (format nil "+static-~A+" class-name) :openldk))))

(defmethod codegen ((insn ssa-local-variable))
  (with-slots (index) insn
    (intern (format nil "local-~A" index) :openldk)))

(defmethod codegen ((insn ssa-mul))
  (list 'cl-containers:push-item 'stack (list '* (list 'cl-containers:pop-item 'stack) (list 'cl-containers:pop-item 'stack))))

(defmethod codegen ((insn ssa-new))
  (with-slots (class-name) insn
    (list 'make-instance (list 'quote (intern class-name :openldk)))))

(defmethod codegen ((insn ssa-pop))
  (list 'cl-containers:pop-item 'stack))

(defmethod codegen ((insn ssa-push))
  (with-slots (value) insn
    (list 'cl-containers:push-item 'stack (codegen value))))

(defmethod codegen ((insn ssa-sub))
  (list 'cl-containers:push-item 'stack (list '- (list 'cl-containers:pop-item 'stack) (list 'cl-containers:pop-item 'stack))))

(defmethod codegen ((insn ssa-call-special-method))
  (classload "java/lang/String" ".:jre8")
  (with-slots (class-name method-name args) insn
    (destructuring-bind (method . next)
        (closer-mop:compute-applicable-methods-using-classes
         (eval (list 'function (intern (format nil "~A" method-name) :openldk)))
         (list (find-class (intern class-name :openldk)) (find-class (intern "java/lang/String" :openldk))))
      (let ((fn (closer-mop:method-function method)))
        (list 'apply fn
              (list 'reverse (cons 'list (mapcar (lambda (a) (codegen a)) args))))))))

(defmethod codegen ((insn ssa-static-member))
  (with-slots (class-name member-name) insn
    (list 'slot-value
          (intern (format nil "+static-~A+" class-name) :openldk)
          (list 'quote (intern member-name :openldk)))))

(defmethod codegen ((insn ssa-store))
  (with-slots (target) insn
    (list 'setf (codegen target) (list 'cl-containers:pop-item 'stack))))

(defmethod codegen ((insn ssa-return))
  (list 'return))

(defmethod codegen ((insn ssa-variable))
  (slot-value insn 'name))
