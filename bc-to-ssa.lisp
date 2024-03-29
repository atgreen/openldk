(in-package :openldk)

#|
(let ((vreg 10000))
  (defmethod next-vreg ((context <context>))
    (let ((vreg (intern (format nil "vreg~A" (incf vreg)))))
      (push vreg (slot-value context 'locals))
      (make-instance 'ssa-variable :name vreg))))
|#

(defun pop-args (num-args)
  (loop repeat num-args
        collect (make-instance 'ssa-pop)))

(defun :AALOAD (context code)
  (declare (ignore code))
  (with-slots (pc) context
    (let ((pc-start pc))
      (incf pc)
      (list (make-instance 'ssa-aaload
                           :address pc-start)))))

(defun :AASTORE (context code)
  (declare (ignore code))
  (with-slots (pc) context
    (let ((pc-start pc))
      (incf pc)
      (list (make-instance 'ssa-aastore
                           :address pc-start)))))

(defun :ACONST_NULL (context code)
  (declare (ignore code))
  (with-slots (pc) context
    (let ((pc-start pc))
      (incf pc)
      (list (make-instance 'ssa-push
                           :address pc-start
                           :value (make-instance 'ssa-null-literal
                                                 :address pc-start))))))

(defun :ALOAD (context code)
  (declare (ignore code))
  (with-slots (pc) context
    (let ((pc-start pc)
          (index (aref code (incf pc))))
      (incf pc)
      (list (make-instance 'ssa-push
                           :address pc-start
                           :value (make-instance 'ssa-local-variable
                                                 :address pc-start
                                                 :index index))))))

(defun :ALOAD_0 (context code)
  (declare (ignore code))
  (with-slots (pc) context
    (let ((pc-start pc))
      (incf pc)
      (list (make-instance 'ssa-push
                           :address pc-start
                           :value (make-instance 'ssa-local-variable
                                                 :address pc-start
                                                 :index 0))))))

(defun :ALOAD_1 (context code)
  (declare (ignore code))
  (with-slots (pc) context
    (let ((pc-start pc))
      (incf pc)
      (list (make-instance 'ssa-push
                           :address pc-start
                           :value (make-instance 'ssa-local-variable
                                                 :address pc-start
                                                 :index 1))))))

(defun :ALOAD_2 (context code)
  (declare (ignore code))
  (with-slots (pc) context
    (let ((pc-start pc))
      (incf pc)
      (list (make-instance 'ssa-push
                           :address pc-start
                           :value (make-instance 'ssa-local-variable
                                                 :address pc-start
                                                 :index 2))))))

(defun :ALOAD_3 (context code)
  (declare (ignore code))
  (with-slots (pc) context
    (let ((pc-start pc))
      (incf pc)
      (list (make-instance 'ssa-push
                           :address pc-start
                           :value (make-instance 'ssa-local-variable
                                                 :address pc-start
                                                 :index 3))))))

(defun :ARRAYLENGTH (context code)
  (with-slots (pc) context
    (let ((pc-start pc))
      (incf pc)
      (list (make-instance 'ssa-array-length
			   :address pc-start)))))


(defun :ASTORE (context code)
  (with-slots (pc) context
    (let ((pc-start pc)
          (index (aref code (incf pc))))
      (incf pc)
      (list (make-instance 'ssa-store
                           :address pc-start
                           :target (make-instance 'ssa-local-variable
                                                  :address pc-start
                                                  :index index))))))

(defun :ASTORE_0 (context code)
  (declare (ignore code))
  (with-slots (pc) context
    (let ((pc-start pc))
      (incf pc)
      (list (make-instance 'ssa-store
                           :address pc-start
                           :target (make-instance 'ssa-local-variable
                                                  :address pc-start
                                                  :index 0))))))

(defun :ASTORE_1 (context code)
  (declare (ignore code))
  (with-slots (pc) context
    (let ((pc-start pc))
      (incf pc)
      (list (make-instance 'ssa-store
                           :address pc-start
                           :target (make-instance 'ssa-local-variable
                                                  :address pc-start
                                                  :index 1))))))

(defun :ASTORE_2 (context code)
  (declare (ignore code))
  (with-slots (pc) context
    (let ((pc-start pc))
      (incf pc)
      (list (make-instance 'ssa-store
                           :address pc-start
                           :target (make-instance 'ssa-local-variable
                                                  :address pc-start
                                                  :index 2))))))

(defun :CASTORE (context code)
  (declare (ignore code))
  (with-slots (pc) context
    (let ((pc-start pc))
      (incf pc)
      (list (make-instance 'ssa-castore
                           :address pc-start)))))

(defun :IASTORE (context code)
  (declare (ignore code))
  (with-slots (pc) context
    (let ((pc-start pc))
      (incf pc)
      (list (make-instance 'ssa-iastore
                           :address pc-start)))))

(defun :IINC (context code)
  (with-slots (pc) context
    (let ((pc-start pc))
      (with-slots (constant-pool) class
        (let* ((index (aref code (incf pc)))
               (const (aref code (incf pc))))
          (incf pc)
          (list (make-instance 'ssa-iinc
                               :address pc-start
                               :index index
                               :const const)))))))

(defun :ISTORE (context code)
  (with-slots (pc) context
    (let ((pc-start pc)
          (index (aref code (incf pc))))
      (incf pc)
      (list (make-instance 'ssa-store
                           :address pc-start
                           :target (make-instance 'ssa-local-variable
                                                  :address pc-start
                                                  :index index))))))

(defun :ISTORE_1 (context code)
  (declare (ignore code))
  (with-slots (pc) context
    (let ((pc-start pc))
      (incf pc)
      (list (make-instance 'ssa-store
                           :address pc-start
                           :target (make-instance 'ssa-local-variable
                                                  :address pc-start
                                                  :index 1))))))

(defun :ISTORE_2 (context code)
  (declare (ignore code))
  (with-slots (pc) context
    (let ((pc-start pc))
      (incf pc)
      (list (make-instance 'ssa-store
                           :address pc-start
                           :target (make-instance 'ssa-local-variable
                                                  :address pc-start
                                                  :index 2))))))

(defun :ISTORE_3 (context code)
  (declare (ignore code))
  (with-slots (pc) context
    (let ((pc-start pc))
      (incf pc)
      (list (make-instance 'ssa-store
                           :address pc-start
                           :target (make-instance 'ssa-local-variable
                                                  :address pc-start
                                                  :index 3))))))

(defun :ASTORE_3 (context code)
  (declare (ignore code))
  (with-slots (pc) context
    (let ((pc-start pc))
      (incf pc)
      (list (make-instance 'ssa-store
                           :address pc-start
                           :target (make-instance 'ssa-local-variable
                                                  :address pc-start
                                                  :index 3))))))

(defun :ATHROW (context code)
  (declare (ignore code))
  (with-slots (pc) context
    (let ((pc-start pc))
      (incf pc)
      (list (make-instance 'ssa-throw :address pc-start)))))

(defun :CHECKCAST (context code)
  (declare (ignore code))
  (with-slots (pc) context
    (let ((pc-start pc)
          (index (+ (* (aref code (incf pc)) 256)
                    (aref code (incf pc)))))
      (incf pc)
      (list (make-instance 'ssa-checkcast
                           :address pc-start
                           :index index)))))

(defun :DADD (context code)
  (declare (ignore code))
  (with-slots (pc) context
    (let ((pc-start pc))
      (incf pc)
      (list (make-instance 'ssa-add
                           :address pc-start)))))

(defun :IADD (context code)
  (declare (ignore code))
  (with-slots (pc) context
    (let ((pc-start pc))
      (incf pc)
      (list (make-instance 'ssa-add
                           :address pc-start)))))

(defun :ISUB (context code)
  (declare (ignore code))
  (with-slots (pc) context
    (let ((pc-start pc))
      (incf pc)
      (list (make-instance 'ssa-sub
                           :address pc-start)))))

(defun :BIPUSH (context code)
  (with-slots (pc) context
    (let ((pc-start pc))
      (let* ((byte (aref code (incf pc))))
        (incf pc)
        (list (make-instance 'ssa-push
                             :address pc-start
                             :value (make-instance 'ssa-int-literal :address pc-start :value byte)))))))

(defun :DCONST_0 (context code)
  (declare (ignore code))
  (with-slots (pc) context
    (let ((pc-start pc))
      (incf pc)
      (list (make-instance 'ssa-push
                           :address pc-start
                           :value (make-instance 'ssa-double-literal
                                                 :address pc-start
                                                 :value 0.0))))))

(defun :DDIV (context code)
  (declare (ignore code))
  (with-slots (pc) context
    (let ((pc-start pc))
      (incf pc)
      (list (make-instance 'ssa-div
                           :address pc-start)))))

(defun :DLOAD (context code)
  (with-slots (pc) context
    (let ((pc-start pc)
          (index (aref code (incf pc))))
      (incf pc)
      (list (make-instance 'ssa-push
                           :address pc-start
                           :value (make-instance 'ssa-local-variable
                                                 :address pc-start
                                                 :index index))))))

(defun :DLOAD_2 (context code)
  (declare (ignore code))
  (with-slots (pc) context
    (let ((pc-start pc))
      (incf pc)
      (list (make-instance 'ssa-push
                           :address pc-start
                           :value (make-instance 'ssa-local-variable
                                                 :address pc-start
                                                 :index 2))))))

(defun :DMUL (context code)
  (declare (ignore code))
  (with-slots (pc) context
    (let ((pc-start pc))
      (incf pc)
      (list (make-instance 'ssa-mul
                           :address pc-start)))))

(defun :DSTORE (context code)
  (with-slots (pc) context
    (let ((pc-start pc)
          (index (aref code (incf pc))))
      (incf pc)
      (list (make-instance 'ssa-store
                           :address pc-start
                           :target (make-instance 'ssa-local-variable
                                                  :address pc-start
                                                  :index index))))))

(defun :DSTORE_2 (context code)
  (declare (ignore code))
  (with-slots (pc) context
    (let ((pc-start pc))
      (incf pc)
      (list (make-instance 'ssa-store
                           :address pc-start
                           :target (make-instance 'ssa-local-variable
                                                  :address pc-start
                                                  :index 2))))))

(defun :DSUB (context code)
  (declare (ignore code))
  (with-slots (pc) context
    (let ((pc-start pc))
      (incf pc)
      (list (make-instance 'ssa-sub
                           :address pc-start)))))

(defun :DUP (context code)
  (declare (ignore code))
  (with-slots (pc) context
    (let ((pc-start pc))
      (incf pc)
      (list (make-instance 'ssa-dup
                           :address pc-start)))))

(defun :DUP_X1 (context code)
  (declare (ignore code))
  (with-slots (pc) context
    (let ((pc-start pc))
      (incf pc)
      (list (make-instance 'ssa-dup-x1
                           :address pc-start)))))

(defun :GETSTATIC (context code)
  (with-slots (pc class is-clinit-p) context
    (let ((pc-start pc))
      (with-slots (constant-pool) class
        (let* ((index (+ (* (aref code (incf pc)) 256)
                         (aref code (incf pc)))))
          (multiple-value-bind (fieldname class)
              (emit (aref constant-pool index) constant-pool)
            (incf pc)
            (let ((code (list (make-instance 'ssa-push
                                             :address pc-start
                                             :value (make-instance 'ssa-static-member
                                                                   :address pc-start
                                                                   :class class
                                                                   :member-name fieldname)))))
              (if is-clinit-p
                  code
                  (cons (make-instance 'ssa-clinit
                                       :address pc-start
                                       :class class)
                        code)))))))))

(defun unsigned-to-signed (unsigned-value)
  (if (>= unsigned-value 32768)
      (- unsigned-value 65536)
      unsigned-value))

(defun :GOTO (context code)
  (with-slots (pc class) context
    (let ((pc-start pc))
      (with-slots (constant-pool) class
        (let* ((offset (unsigned-to-signed (+ (* (aref code (incf pc)) 256)
                                              (aref code (incf pc))))))
          (incf pc)
          (list (make-instance 'ssa-goto
                               :address pc-start
                               :offset (+ pc-start offset))))))))

(defun :IALOAD (context code)
  (declare (ignore code))
  (with-slots (pc) context
    (let ((pc-start pc))
      (incf pc)
      (list (make-instance 'ssa-iaload
                           :address pc-start)))))

(defun :ICONST_0 (context code)
  (declare (ignore code))
  (with-slots (pc) context
    (let ((pc-start pc))
      (incf pc)
      (list (make-instance 'ssa-push
                           :address pc-start
                           :value (make-instance 'ssa-int-literal
                                                 :address pc-start
                                                 :value 0))))))

(defun :ICONST_1 (context code)
  (declare (ignore code))
  (with-slots (pc) context
    (let ((pc-start pc))
      (incf pc)
      (list (make-instance 'ssa-push
                           :address pc-start
                           :value (make-instance 'ssa-int-literal
                                                 :address pc-start
                                                 :value 1))))))

(defun :ICONST_2 (context code)
  (declare (ignore code))
  (with-slots (pc) context
    (let ((pc-start pc))
      (incf pc)
      (list (make-instance 'ssa-push
                           :address pc-start
                           :value (make-instance 'ssa-int-literal
                                                 :address pc-start
                                                 :value 2))))))

(defun :ICONST_3 (context code)
  (declare (ignore code))
  (with-slots (pc) context
    (let ((pc-start pc))
      (incf pc)
      (list (make-instance 'ssa-push
                           :address pc-start
                           :value (make-instance 'ssa-int-literal
                                                 :address pc-start
                                                 :value 3))))))

(defun :ICONST_4 (context code)
  (declare (ignore code))
  (with-slots (pc) context
    (let ((pc-start pc))
      (incf pc)
      (list (make-instance 'ssa-push
                           :address pc-start
                           :value (make-instance 'ssa-int-literal
                                                 :address pc-start
                                                 :value 4))))))

(defun :ICONST_5 (context code)
  (declare (ignore code))
  (with-slots (pc) context
    (let ((pc-start pc))
      (incf pc)
      (list (make-instance 'ssa-push
                           :address pc-start
                           :value (make-instance 'ssa-int-literal
                                                 :address pc-start
                                                 :value 5))))))

(defun :IF_ACMPEQ (context code)
  (with-slots (pc class) context
    (let ((pc-start pc))
      (with-slots (constant-pool) class
        (let* ((offset (unsigned-to-signed (+ (* (aref code (incf pc)) 256)
                                              (aref code (incf pc))))))
          (incf pc)
          (list (make-instance 'ssa-if-acmpeq
                               :address pc-start
                               :offset (+ pc-start offset))))))))

(defun :IF_ACMPNE (context code)
  (with-slots (pc class) context
    (let ((pc-start pc))
      (with-slots (constant-pool) class
        (let* ((offset (unsigned-to-signed (+ (* (aref code (incf pc)) 256)
                                              (aref code (incf pc))))))
          (incf pc)
          (list (make-instance 'ssa-if-acmpne
                               :address pc-start
                               :offset (+ pc-start offset))))))))

(defun :IF_ICMPEQ (context code)
  (with-slots (pc class) context
    (let ((pc-start pc))
      (with-slots (constant-pool) class
        (let* ((offset (unsigned-to-signed (+ (* (aref code (incf pc)) 256)
                                              (aref code (incf pc))))))
          (incf pc)
          (list (make-instance 'ssa-if-icmpeq
                               :address pc-start
                               :offset (+ pc-start offset))))))))

(defun :IF_ICMPGE (context code)
  (with-slots (pc class) context
    (let ((pc-start pc))
      (with-slots (constant-pool) class
        (let* ((offset (unsigned-to-signed (+ (* (aref code (incf pc)) 256)
                                              (aref code (incf pc))))))
          (incf pc)
          (list (make-instance 'ssa-if-icmpge
                               :address pc-start
                               :offset (+ pc-start offset))))))))

(defun :IF_ICMPGT (context code)
  (with-slots (pc class) context
    (let ((pc-start pc))
      (with-slots (constant-pool) class
        (let* ((offset (unsigned-to-signed (+ (* (aref code (incf pc)) 256)
                                              (aref code (incf pc))))))
          (incf pc)
          (list (make-instance 'ssa-if-icmpgt
                               :address pc-start
                               :offset (+ pc-start offset))))))))

(defun :IF_ICMPLE (context code)
  (with-slots (pc class) context
    (let ((pc-start pc))
      (with-slots (constant-pool) class
        (let* ((offset (unsigned-to-signed (+ (* (aref code (incf pc)) 256)
                                              (aref code (incf pc))))))
          (incf pc)
          (list (make-instance 'ssa-if-icmple
                               :address pc-start
                               :offset (+ pc-start offset))))))))

(defun :IF_ICMPNE (context code)
  (with-slots (pc class) context
    (let ((pc-start pc))
      (with-slots (constant-pool) class
        (let* ((offset (unsigned-to-signed (+ (* (aref code (incf pc)) 256)
                                              (aref code (incf pc))))))
          (incf pc)
          (list (make-instance 'ssa-if-icmpne
                               :address pc-start
                               :offset (+ pc-start offset))))))))

(defun :IDIV (context code)
  (declare (ignore code))
  (with-slots (pc) context
    (let ((pc-start pc))
      (incf pc)
      (list (make-instance 'ssa-div
                           :address pc-start)))))

(defun :IFEQ (context code)
  (with-slots (pc class) context
    (let ((pc-start pc))
      (with-slots (constant-pool) class
        (let* ((offset (unsigned-to-signed (+ (* (aref code (incf pc)) 256)
                                              (aref code (incf pc))))))
          (incf pc)
          (list (make-instance 'ssa-ifeq
                               :address pc-start
                               :offset (+ pc-start offset))))))))

(defun :IFGE (context code)
  (with-slots (pc class) context
    (let ((pc-start pc))
      (with-slots (constant-pool) class
        (let* ((offset (unsigned-to-signed (+ (* (aref code (incf pc)) 256)
                                              (aref code (incf pc))))))
          (incf pc)
          (list (make-instance 'ssa-ifge
                               :address pc-start
                               :offset (+ pc-start offset))))))))

(defun :IFLE (context code)
  (with-slots (pc class) context
    (let ((pc-start pc))
      (with-slots (constant-pool) class
        (let* ((offset (unsigned-to-signed (+ (* (aref code (incf pc)) 256)
                                              (aref code (incf pc))))))
          (incf pc)
          (list (make-instance 'ssa-ifle
                               :address pc-start
                               :offset (+ pc-start offset))))))))

(defun :IFNE (context code)
  (with-slots (pc class) context
    (let ((pc-start pc))
      (with-slots (constant-pool) class
        (let* ((offset (unsigned-to-signed (+ (* (aref code (incf pc)) 256)
                                              (aref code (incf pc))))))
          (incf pc)
          (list (make-instance 'ssa-ifne
                               :address pc-start
                               :offset (+ pc-start offset))))))))

(defun :IFNONNULL (context code)
  (with-slots (pc class) context
    (let ((pc-start pc))
      (with-slots (constant-pool) class
        (let* ((offset (unsigned-to-signed (+ (* (aref code (incf pc)) 256)
                                              (aref code (incf pc))))))
          (incf pc)
          (list (make-instance 'ssa-ifnonnull
                               :address pc-start
                               :offset (+ pc-start offset))))))))

(defun :IFNULL (context code)
  (with-slots (pc class) context
    (let ((pc-start pc))
      (with-slots (constant-pool) class
        (let* ((offset (unsigned-to-signed (+ (* (aref code (incf pc)) 256)
                                              (aref code (incf pc))))))
          (incf pc)
          (list (make-instance 'ssa-ifnull
                               :address pc-start
                               :offset (+ pc-start offset))))))))

(defun :ILOAD (context code)
  (with-slots (pc) context
    (let ((pc-start pc)
          (index (aref code (incf pc))))
      (incf pc)
      (list (make-instance 'ssa-push
                           :address pc-start
                           :value (make-instance 'ssa-local-variable
                                                 :address pc-start
                                                 :index index))))))

(defun :ILOAD_0 (context code)
  (declare (ignore code))
  (with-slots (pc) context
    (let ((pc-start pc))
      (incf pc)
      (list (make-instance 'ssa-push
                           :address pc-start
                           :value (make-instance 'ssa-local-variable
                                                 :address pc-start
                                                 :index 0))))))

(defun :ILOAD_1 (context code)
  (declare (ignore code))
  (with-slots (pc) context
    (let ((pc-start pc))
      (incf pc)
      (list (make-instance 'ssa-push
                           :address pc-start
                           :value (make-instance 'ssa-local-variable
                                                 :address pc-start
                                                 :index 1))))))

(defun :ILOAD_2 (context code)
  (declare (ignore code))
  (with-slots (pc) context
    (let ((pc-start pc))
      (incf pc)
      (list (make-instance 'ssa-push
                           :address pc-start
                           :value (make-instance 'ssa-local-variable
                                                 :address pc-start
                                                 :index 2))))))

(defun :ILOAD_3 (context code)
  (declare (ignore code))
  (with-slots (pc) context
    (let ((pc-start pc))
      (incf pc)
      (list (make-instance 'ssa-push
                           :address pc-start
                           :value (make-instance 'ssa-local-variable
                                                 :address pc-start
                                                 :index 3))))))

(defun :INEG (context code)
  (declare (ignore code))
  (with-slots (pc) context
    (let ((pc-start pc))
      (incf pc)
      (list (make-instance 'ssa-ineg
                           :address pc-start)))))

(defun :INSTANCEOF (context code)
  (with-slots (pc class) context
    (let ((pc-start pc))
      (with-slots (constant-pool) class
        (let* ((index (+ (* (aref code (incf pc)) 256)
                         (aref code (incf pc))))
               (class (aref constant-pool index)))
          (incf pc)
          (list (make-instance 'ssa-instanceof
                               :address pc-start
                               :class (emit class constant-pool))))))))

(defun :INVOKEDYNAMIC (context code)
  (with-slots (pc class) context
    (let ((pc-start pc))
      (with-slots (constant-pool) class
        (let* ((index (+ (* (aref code (incf pc)) 256)
                         (aref code (incf pc))))
               (invoke-dynamic-info (aref constant-pool index)))
          (incf pc)
          (incf pc)
          (incf pc)
          (let* ((name-and-type (aref constant-pool
                                      (slot-value invoke-dynamic-info
                                                  'name-and-type-index)))
                 (descriptor
                  (slot-value (aref constant-pool
                                    (slot-value name-and-type 'type-descriptor-index))
                              'value))
                 (parameter-count (1+ (count-parameters descriptor))))
            (format t "==== ~A ====~%" (emit name-and-type constant-pool))
            (list (make-instance 'ssa-call-virtual-method
                                 :address pc-start
                                 :method-name (emit name-and-type constant-pool)
                                 :args (pop-args parameter-count)))))))))

(defun :INVOKEINTERFACE (context code)
  (with-slots (pc class) context
    (let ((pc-start pc))
      (with-slots (constant-pool) class
        (let* ((index (+ (* (aref code (incf pc)) 256)
                         (aref code (incf pc))))
               (method-reference (aref constant-pool index)))
          (incf pc)
          (incf pc)
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
                                 :address pc-start
                                 :method-name (emit method-reference constant-pool)
                                 :args (pop-args parameter-count)))))))))

(defun :INVOKEVIRTUAL (context code)
  (with-slots (pc class) context
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
                                 :address pc-start
                                 :method-name (emit method-reference constant-pool)
                                 :args (pop-args parameter-count)))))))))

(defun :INVOKESPECIAL (context code)
  (with-slots (pc class) context
    (let ((pc-start pc))
      (with-slots (constant-pool) class
        (let* ((index (+ (* (aref code (incf pc)) 256)
                         (aref code (incf pc))))
               (method-reference (aref constant-pool index)))
          (incf pc)
          (let* ((class
                   (slot-value (emit (aref constant-pool
                                           (slot-value method-reference
                                                       'class-index))
                                     constant-pool)
                               'class))
                 (descriptor
                  (slot-value (aref constant-pool
                                    (slot-value
                                     (aref constant-pool
                                           (slot-value method-reference
                                                       'method-descriptor-index))
                                     'type-descriptor-index))
                              'value))
                 (parameter-count (1+ (count-parameters descriptor))))
            (list (make-instance 'ssa-call-special-method
                                 :address pc-start
                                 :class class
                                 :method-name (emit method-reference constant-pool)
                                 :args (pop-args parameter-count)))))))))

(defun :INVOKESTATIC (context code)
  (with-slots (pc class) context
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
                 (callee-class
                   (slot-value (aref constant-pool
                                     (slot-value
                                      (aref constant-pool
                                            (slot-value method-reference
                                                        'class-index))
                                      'index))
                               'value))
                 (parameter-count (count-parameters descriptor)))
            (classload callee-class)
            (list (make-instance 'ssa-call-static-method
                                 :address pc-start
                                 :class callee-class
                                 :method-name (emit method-reference constant-pool)
                                 :args (pop-args parameter-count)))))))))


(defun :IRETURN (context code)
  (declare (ignore code))
  (with-slots (pc) context
    (let ((pc-start pc))
      (incf pc)
      (list (make-instance 'ssa-return-value
                           :fn-name (slot-value context 'fn-name)
                           :address pc-start)))))

(defun :ISHL (context code)
  (declare (ignore code))
  (with-slots (pc) context
    (let ((pc-start pc))
      (incf pc)
      (list (make-instance 'ssa-ishl
                           :address pc-start)))))

(defun :ISHR (context code)
  (declare (ignore code))
  (with-slots (pc) context
    (let ((pc-start pc))
      (incf pc)
      (list (make-instance 'ssa-ishr
                           :address pc-start)))))

(defun :ARETURN (context code)
  (:IRETURN context code))

(defun :LDC (context code)
  (with-slots (pc class) context
    (let ((pc-start pc))
      (with-slots (constant-pool) class
        (let ((index (aref code (incf pc))))
          (incf pc)
          (list (make-instance 'ssa-push :address pc-start
                                         :value (emit (aref constant-pool index) constant-pool))))))))

(defun :LUSHR (context code)
  (declare (ignore code))
  (with-slots (pc) context
    (let ((pc-start pc))
      (incf pc)
      (list (make-instance 'ssa-lushr
                           :address pc-start)))))

(defun :MONITORENTER (context code)
  (declare (ignore code))
  (with-slots (pc) context
    (let ((pc-start pc))
      (incf pc)
      (list (make-instance 'ssa-nop :address pc-start)))))

(defun :MONITOREXIT (context code)
  (declare (ignore code))
  (with-slots (pc) context
    (let ((pc-start pc))
      (incf pc)
      (list (make-instance 'ssa-nop :address pc-start)))))

(defun :NEW (context code)
  (with-slots (pc class) context
    (let ((pc-start pc))
      (with-slots (constant-pool) class
      (let* ((index (+ (* (aref code (incf pc)) 256)
                       (aref code (incf pc))))
             (class (emit (aref constant-pool index) constant-pool)))
        (incf pc)
        (list (make-instance 'ssa-push
                             :address pc-start
                             :value (make-instance 'ssa-new :address pc-start :class class))))))))

(defun :NOP (context code)
  (with-slots (pc class) context
    (let ((pc-start pc))
      (incf pc)
      (list (make-instance 'ssa-nop :address pc-start)))))

(defun :ANEWARRAY (context code)
  (with-slots (pc class) context
    (let ((pc-start pc))
      (with-slots (constant-pool) class
      (let* ((index (+ (* (aref code (incf pc)) 256)
                       (aref code (incf pc))))
             (class (emit (aref constant-pool index) constant-pool)))
        (incf pc)
        (list (make-instance 'ssa-push
                             :address pc-start
                             :value (make-instance 'ssa-new-array :address pc-start :class class))))))))

(defun :NEWARRAY (context code)
  (with-slots (pc class) context
    (let ((pc-start pc))
      (let ((atype (aref code (incf pc))))
        (incf pc)
	;; FIXME: just throw away the type?
        (list (make-instance 'ssa-push
                             :address pc-start
                             :value (make-instance 'ssa-new-array :address pc-start :class nil)))))))

(defun :POP (context code)
  (declare (ignore code))
  (with-slots (pc) context
    (let ((pc-start pc))
      (incf pc)
      (list (make-instance 'ssa-pop :address pc-start)))))

(defun :GETFIELD (context code)
  (with-slots (pc class) context
    (let ((pc-start pc))
      (with-slots (constant-pool) class
        (let* ((index (+ (* (aref code (incf pc)) 256)
                         (aref code (incf pc)))))
          (multiple-value-bind (fieldname)
              (emit (aref constant-pool index) constant-pool)
            (incf pc)
            (list (make-instance 'ssa-push
                                 :address pc-start
                                 :value (make-instance 'ssa-member
                                                       :address pc-start
                                                       :member-name fieldname)))))))))

(defun :PUTFIELD (context code)
  (with-slots (pc class) context
    (let ((pc-start pc))
      (with-slots (constant-pool) class
        (let* ((index (+ (* (aref code (incf pc)) 256)
                         (aref code (incf pc)))))
          (multiple-value-bind (fieldname)
              (emit (aref constant-pool index) constant-pool)
            (incf pc)
            (list (make-instance 'ssa-assign
                                 :address pc-start
                                 :source (make-instance 'ssa-pop :address pc-start)
                                 :target (make-instance 'ssa-member
                                                        :address pc-start
                                                        :member-name fieldname)))))))))

(defun :PUTSTATIC (context code)
  (with-slots (pc class is-clinit-p) context
    (let ((pc-start pc))
      (with-slots (constant-pool) class
        (let* ((index (+ (* (aref code (incf pc)) 256)
                         (aref code (incf pc)))))
          (multiple-value-bind (fieldname class)
              (emit (aref constant-pool index) constant-pool)
            (incf pc)
            (let ((code (list (make-instance 'ssa-assign
                                             :address pc-start
                                             :source (make-instance 'ssa-pop :address pc-start)
                                             :target (make-instance 'ssa-static-member
                                                                    :address pc-start
                                                                    :class class
                                                                    :member-name fieldname)))))
              (if is-clinit-p
                  code
                  (cons (make-instance 'ssa-clinit
                                       :address pc-start
                                       :class class)
                        code)))))))))

(defun :RETURN (context code)
  (declare (ignore code))
  (with-slots (pc) context
    (let ((pc-start pc))
      (incf pc)
      (list (make-instance 'ssa-return
                           :address pc-start)))))

(defun :SIPUSH (context code)
  (with-slots (pc) context
    (let ((pc-start pc))
      (let* ((short (unsigned-to-signed (+ (* (aref code (incf pc)) 256)
                                           (* (aref code (incf pc)))))))
        (incf pc)
        (list (make-instance 'ssa-push
                             :address pc-start
                             :value (make-instance 'ssa-int-literal :address pc-start :value short)))))))
