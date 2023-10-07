(in-package :openldk)

(defmethod codegen ((insn ssa-literal))
  (slot-value insn 'value))

(defmethod codegen ((insn ssa-assign))
  (with-slots (source target) insn
    (list 'setf (codegen target) (codegen source))))

(defmethod codegen ((insn ssa-call-static-method))
  (with-slots (class-name method-name args) insn
    (list 'apply
          (list 'function (intern (format nil "~A.~A"
                                          class-name
                                          method-name)))
          (list 'reverse (mapcar (lambda (a) (codegen a)) args)))))

(defmethod codegen ((insn ssa-branch-target))
  (intern (format nil "#:branch-target-~A" (slot-value insn 'index))))

(defmethod codegen ((insn ssa-if-icmple))
  (with-slots (offset) insn
    (list 'if (list '>= (list 'cl-containers:pop-item 'stack) (list 'cl-containers:pop-item 'stack))
          (list 'go (intern (format nil "#:branch-target-~A" offset))))))

(defmethod codegen ((insn ssa-goto))
  (with-slots (offset) insn
    (list 'go (intern (format nil "#:branch-target-~A" offset)))))

(defmethod codegen ((insn ssa-call-virtual-method))
  (with-slots (method-name args) insn
    (list 'apply
          (list 'function (intern (format nil "~A"
                                          method-name)))
          (list 'reverse (cons 'list (mapcar (lambda (a) (codegen a)) args))))))

(defmethod codegen ((insn ssa-clinit))
  (with-slots (class-name) insn
    (list (intern "<clinit>()V") (intern (format nil "+static-~A+" class-name)))))

(defmethod codegen ((insn ssa-local-variable))
  (list (intern (format nil "local-~A" (slot-value insn :index)))))

(defmethod codegen ((insn ssa-pop))
  (list 'cl-containers:pop-item 'stack))

(defmethod codegen ((insn ssa-push))
  (with-slots (value) insn
    (list 'cl-containers:push-item 'stack (codegen value))))

(defmethod codegen ((insn ssa-static-member))
  (with-slots (class-name member-name) insn
    (list 'slot-value
          (intern (format nil "+static-~A+" class-name))
          (list 'quote (intern member-name)))))

(defmethod codegen ((insn ssa-return))
  (list 'return))

(defmethod codegen ((insn ssa-variable))
  (slot-value insn 'name))
