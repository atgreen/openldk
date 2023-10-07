(in-package :openldk)

(let ((vreg 10000))
  (defmethod next-vreg ((context <context>))
    (let ((vreg (intern (format nil "vreg~A" (incf vreg)))))
      (push vreg (slot-value context 'locals))
      (make-instance 'ssa-variable :name vreg))))

(defun pop-args (num-args stack)
  (loop repeat num-args
        for item = (cl-containers:pop-item stack)
        until (null item)
        collect item into items
        finally (return (reverse items))))

(defun :ACONST_NULL (context code)
  (with-slots (pc stack) context
    (let ((pc-start pc))
      (incf pc)
      (let ((vreg (next-vreg context)))
        (cl-containers:push-item stack vreg)
        (list (make-instance 'ssa-assign
                             :index pc-start
                             :target vreg
                             :source (make-instance 'ssa-null-literal
                                                    :index pc-start)))))))

(defun :GETSTATIC (context code)
  (with-slots (pc class stack is-clinit-p) context
    (let ((pc-start pc))
      (with-slots (constant-pool) class
        (let* ((index (+ (* (aref code (incf pc)) 256)
                         (aref code (incf pc))))
               (vreg (next-vreg context)))
          (multiple-value-bind (fieldname classname)
              (emit (aref constant-pool index) constant-pool)
            (incf pc)
            (cl-containers:push-item stack vreg)
            (let ((code (list (make-instance 'ssa-assign
                                             :index pc-start
                                             :target vreg
                                             :source (make-instance 'ssa-static-member
                                                                    :index pc-start
                                                                    :class-name classname
                                                                    :member-name fieldname)))))
              (if is-clinit-p
                  code
                  (cons (make-instance 'ssa-clinit
                                       :index pc-start
                                       :class-name classname)
                        code)))))))))

(defun :INVOKEVIRTUAL (context code)
  (with-slots (pc class stack) context
    (let ((pc-start pc))
      (with-slots (constant-pool) class
        (let* ((index (+ (* (aref code (incf pc)) 256)
                         (aref code (incf pc))))
               (method-reference (aref constant-pool index)))
          (incf pc)
          (let* ((descriptor
                  (slot-value (aref constant-pool
                                    (slot-value
                                     (aref constant-pool
                                           (slot-value method-reference
                                                       'method-descriptor-index))
                                     'type-descriptor-index))
                              'value))
                 (parameter-count (1+ (count-parameters descriptor))))
            (list (make-instance 'ssa-call-virtual-method
                                 :index pc-start
                                 :method-name (emit method-reference constant-pool)
                                 :args (pop-args parameter-count stack)))))))))

(defun :INVOKESTATIC (context code)
  (with-slots (pc class stack) context
    (let ((pc-start pc))
      (with-slots (constant-pool) class
        (let* ((index (+ (* (aref code (incf pc)) 256)
                         (aref code (incf pc))))
               (method-reference (aref constant-pool index)))
          (incf pc)
          (let* ((descriptor
                  (slot-value (aref constant-pool
                                    (slot-value
                                     (aref constant-pool
                                           (slot-value method-reference
                                                       'method-descriptor-index))
                                     'type-descriptor-index))
                              'value))
                 (parameter-count (count-parameters descriptor)))
            (list (make-instance 'ssa-call-static-method
                                 :index pc-start
                                 :class-name (slot-value class 'name)
                                 :method-name (emit method-reference constant-pool)
                                 :args (pop-args parameter-count stack)))))))))

(defun :LDC (context code)
  (with-slots (pc stack class) context
    (let ((pc-start pc))
      (with-slots (constant-pool) class
        (let ((index (aref code (incf pc))))
          (incf pc)
          (cl-containers:push-item stack
                                   (emit (aref constant-pool index) constant-pool))
          nil)))))

(defun :PUTSTATIC (context code)
  (with-slots (pc class stack is-clinit-p) context
    (let ((pc-start pc))
      (with-slots (constant-pool) class
        (let* ((index (+ (* (aref code (incf pc)) 256)
                         (aref code (incf pc))))
               (field-reference (aref constant-pool index)))
          (multiple-value-bind (fieldname classname)
              (emit (aref constant-pool index) constant-pool)
            (incf pc)
            (let ((code (list (make-instance 'ssa-assign
                                             :index pc-start
                                             :source (cl-containers:pop-item stack)
                                             :target (make-instance 'ssa-static-member
                                                                    :index pc-start
                                                                    :class-name classname
                                                                    :member-name fieldname)))))
              (if is-clinit-p
                  code
                  (cons (make-instance 'ssa-clinit
                                       :index pc-start
                                       :class-name classname)
                        code)))))))))

(defun :RETURN (context code)
  (with-slots (pc) context
    (let ((start-pc pc))
      (incf pc)
      (list (make-instance 'ssa-return
                           :index start-pc)))))
