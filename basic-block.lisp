(in-package :openldk)

(defvar *block-counter* 0)

(defun generate-id ()
  (incf *block-counter*))

(defclass <block> ()
  ((id :reader .id :initform (generate-id))
   (code-emitted-p :initform nil)
   (successor-addresses :reader .successor-addresses :initform (list))
   (successor-blocks :reader .successor-blocks :initform (list))
   (catch-blocks :reader .catch-blocks :initform (list))
   (fixed-up-p :initform nil)
   (code :reader .code :initform (list))))

(defmethod .id (x) 0)

(defmethod dump-dot (b done-table stream)
  (format stream "~A [label=\"wth? ~A\"];~%" b b))

(defmethod dump-dot ((bloc <block>) done-table stream)
  (unless (gethash (.id bloc) done-table)
    (setf (gethash (.id bloc) done-table) t)
    (format stream "~A [label=" (.id bloc))
    (format stream "<<TABLE BORDER=\"0\" CELLBORDER=\"0\" CELLSPACING=\"0\">")
    (dolist (i (.code bloc))
      (format stream "<TR><TD ALIGN=\"LEFT\">\"~A\"</TD></TR>~%" (dot-dump-string i)))
    (format stream "</TABLE>>];~%")
    (dolist (successor (.successor-blocks bloc))
      (when successor
        (dump-dot successor done-table stream)
        (format stream "~A -> ~A~%" (.id bloc) (.id successor))))
    (dolist (catch-block (.catch-blocks bloc))
      (dump-dot (cdr catch-block) done-table stream)
      (format stream "~A -> ~A [label=\"catch\"];~%" (.id bloc) (.id (cdr catch-block))))))

(defun get-short-branch-targets (pc code)
  (let ((start_pc pc)
        (offset (unsigned-to-signed (+ (* (aref code (incf pc)) 256)
                                       (aref code (incf pc))))))
    (if (gethash (aref +opcodes+ (aref code start_pc)) +bytecode-conditional-branch-table+)
        (list (+ start_pc offset) (1+ pc))
        (list (+ start_pc offset)))))

(defvar *instruction-exceptions* (make-hash-table))

(defmacro define-instruction-exceptions (opcode exceptions)
  `(setf (gethash ,opcode *instruction-exceptions*) ,exceptions))

(define-instruction-exceptions :IDIV
    '("java/lang/ArithmeticException" "java/lang/Exception" "java/lang/Throwable"))

(defun opcode-throws-p (opcode throwable)
  (let ((throwables (gethash opcode *instruction-exceptions*)))
    (find throwable throwables :test #'string=)))

(defun find-leader-instructions ()
  "Returns three HASH-TABLE objects.
1. Key: address, Value: t if this is a leader instruction.
2. Key: address, Value: an exception table entry if this is the start of a range.
3. Key: address, Value: a list of possible next PCs (successors)."
  (let* ((code (slot-value *context* 'bytecode))
         (exception-table (slot-value *context* 'exception-table))
         (pc 0)
         (length (length code))
         (branch-target-table (make-hash-table))
         (try-block-table (make-hash-table))
         ;; key  : instruction address
         ;; value: a list of successor addresses, the first of which
         ;;        is the "fall-through" case, should one exist.
         (successor-address-table (make-hash-table)))

    ;; First, let's go through the instructions looking
    ;; for branch targets.
    (dolist (bt (remove-duplicates
                 (apply
                  #'append
                  (loop
                    while (< pc length)
                    for result = (let* ((opcode (aref +opcodes+ (aref code pc)))
                                        (targets (if (gethash opcode +bytecode-short-branch-table+)
                                                     (get-short-branch-targets pc code))))
                                   (when targets
                                     (setf (gethash pc successor-address-table) targets))
                                   (incf pc (gethash opcode +bytecode-lengths-table+))
                                   targets)
                    unless (null result)
                      collect result))))
      (setf (gethash bt branch-target-table) t))

    ;; Now let's go through the exception table, looking for exception
    ;; (catch) targets.  Along the way, let's look for in-method
    ;; "branches" caused by exceptions.
    (when exception-table
      (loop for i from 0 upto (1- (length exception-table))
            do (let ((ete (aref exception-table i)))
                 (with-slots (start-pc end-pc handler-pc catch-type) ete
                   (push ete (gethash start-pc try-block-table))
                   ;; (format t "find ETE~%")
                   ;; (maphash (lambda (k v) (format t "~A: ~A~%" k v)) try-block-table)
                   (loop
                     with pc = start-pc
                     until (eq pc end-pc)
                     do (let* ((opcode (aref +opcodes+ (aref code pc)))
                               (insn-length (gethash opcode +bytecode-lengths-table+)))
                          (if (opcode-throws-p opcode catch-type)
                              (progn
                                (setf (gethash pc successor-address-table)
                                      (list (+ pc insn-length) handler-pc))
                                (setf (gethash (incf pc insn-length) branch-target-table) t)
                                (setf (gethash pc branch-target-table) t))
                              (incf pc insn-length))))
                   (setf (gethash handler-pc branch-target-table) t)))))

    (values branch-target-table try-block-table successor-address-table)))

(defun build-blocks (ssa-code)
  "Build <BLOCK> objects from SSA-CODE. Return the entry block."
  (dump "build-blocks" ssa-code)
  (assert (eq 0 (.address (car ssa-code))))

  (multiple-value-bind (branch-targets try-block-table successor-address-table)
      (find-leader-instructions)
    ;; We now have three hashtables, keyed on instruction address.
    ;; These identify branch targets, the start of try blocks, and
    ;; successor addresses in case of branches, gotos, etc.
    ;;
    (let ((block-by-entry-address (make-hash-table)))

      ;; Create all of the basic blocks by running through the code.
      (let ((bloc nil))
        (dolist (insn ssa-code)
          (when (or (null bloc) (gethash (.address insn) branch-targets))
            (format t "new @ ~A~%" (.address insn))
            ;; We are starting a new block
            (when bloc
              ;; Instructions were pushed in reverse order, so let's fix that.
              (setf (slot-value bloc 'code) (reverse (slot-value bloc 'code)))
              ;; Stash the block in block-by-entry-address.
              (format t "stashing block @ ~A~%" (.address insn))
              (setf (gethash (.address insn) block-by-entry-address) bloc))
            ;; Create a new block
            (setf bloc (make-instance '<block>)))
          (with-slots (code) bloc
            (push insn code)))
        (when bloc
          ;; Instructions were pushed in reverse order, so let's fix that.
          (setf (slot-value bloc 'code) (reverse (slot-value bloc 'code)))
          (format t "stashing last block @ ~A~%" (.address (car (.code bloc))))
          (setf (gethash (.address (car (.code bloc))) block-by-entry-address) bloc))

        ;; Now that we've created all of the blocks, let's set
        ;; up the successors.
        (maphash (lambda (address bloc)
                   (setf (slot-value bloc 'successor-blocks)
                         (loop for s in (gethash (.address (car (last (.code bloc)))) successor-address-table)
                               collect (gethash s block-by-entry-address))))
                 block-by-entry-address)

        (dump-dot (gethash 0 block-by-entry-address) (make-hash-table) t)

        (maphash (lambda (k v) (format t "B: ~A~%" k))
                 block-by-entry-address)

        (gethash 0 block-by-entry-address)))))
