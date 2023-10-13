(in-package :openldk)

(defclass <basic-block> ()
  ((code :initform (list))
   (successors)))

(defun get-short-branch-targets (pc code)
  (let ((start_pc pc)
        (offset (+ (* (aref code (incf pc)) 256)
                   (aref code (incf pc)))))
      (list (1+ pc) (+ start_pc offset))))

(defvar *instruction-exceptions* (make-hash-table))

(defmacro define-instruction-exceptions (opcode exceptions)
  `(setf (gethash ,opcode *instruction-exceptions*) ,exceptions))

(define-instruction-exceptions :IDIV
 '("java/lang/ArithmeticException" "java/lang/Exception" "java/lang/Throwable"))

(defun opcode-throws-p (opcode throwable)
  (let ((throwables (gethash opcode *instruction-exceptions*)))
    (find throwable throwables :test #'string=)))

(defun find-target-instructions ()
  "Return a HASH-TABLE with all of the offsets in CODE that are branch
or exception targets."
  (let* ((code (slot-value *context* 'bytecode))
         (exception-table (slot-value *context* 'exception-table))
         (pc 0)
         (length (length code))
         (branch-target-table (make-hash-table)))

    ;; First, let's go through the instructions looking
    ;; for branch targets.
    (dolist (bt (remove-duplicates
                 (apply #'append (loop
                                   while (< pc length)
                                   for result = (let* ((opcode (aref +opcodes+ (aref code pc)))
                                                       (targets (if (gethash opcode +bytecode-short-branch-table+)
                                                                    (get-short-branch-targets pc code))))
                                                  (incf pc (gethash opcode +bytecode-lengths-table+))
                                                  targets)
                                   unless (null result)
                                     collect result))))
      (setf (gethash bt branch-target-table) t))

    ;; Now let's go through the exception table, looking for exception
    ;; (catch) targets.
    ;; Along the way, let's look for divide-by-zero branches
    (when exception-table
      (loop for i from 0 upto (1- (length exception-table))
            do (let ((ete (aref exception-table i)))
                 (with-slots (start-pc end-pc handler-pc catch-type) ete
                   (loop
                     with pc = start-pc
                     until (eq pc end-pc)
                     do (let* ((opcode (aref +opcodes+ (aref code pc)))
                               (insn-length (gethash opcode +bytecode-lengths-table+)))
                          (format t "~A: ~A~%" opcode insn-length)
                          (if (opcode-throws-p opcode catch-type)
                              (progn
                                (format t "THROWING!!!!!!!!!!!!!!!!!!~%")
                                (setf (gethash (incf pc insn-length) branch-target-table) t)
                                (setf (gethash pc branch-target-table) t))
                              (incf pc insn-length))))
                   (setf (gethash handler-pc branch-target-table) t)
                   (format t "CATCH-TYPE: ~A~%" (slot-value ete 'catch-type))))))

    branch-target-table))

(defun build-basic-blocks (ssa-code)
  (format t "build-basic-blocks: ~A~%" ssa-code)
  (dump "build-basic-blocks" ssa-code)

  (let ((branch-targets (find-target-instructions)))

    (labels ((%add-to-block (basic-block insn)
               (with-slots (code) basic-block
                 (push insn code)))
             (%build-basic-blocks (ssa-code)
               (format t "bbb: ~A~%" ssa-code)
               (let ((basic-bloc (make-instance '<basic-block>))
                     (entry-insn t))
                 (multiple-value-prog1
                     (loop
                       for insn = (car ssa-code)
                       while insn
                       for pc-index = (slot-value insn 'pc-index)
                       do (progn
                            (if (or entry-insn
                                    (not (gethash pc-index branch-targets)))
                                (progn
                                  (%add-to-block basic-bloc insn)
                                  (setf entry-insn nil)
                                  (setf (gethash pc-index branch-targets) nil)
                                  (pop ssa-code))
                                (progn
                                  (setf (gethash pc-index branch-targets) nil)
                                  (return (progn
                                            (with-slots (code) basic-bloc
                                              (setf code (reverse code)))
                                            (values basic-bloc ssa-code))))))
                       finally (progn
                                 (with-slots (code) basic-bloc
                                   (setf code (nreverse code)))
                                 (return (if (slot-value basic-bloc 'code) (values basic-bloc nil) (values nil nil)))))
                   (values basic-bloc nil)))))
      (loop
        with current-ssa = ssa-code
        for (basic-block rest-ssa) = (multiple-value-list (%build-basic-blocks current-ssa))
        while basic-block
        do (setf current-ssa rest-ssa)
        collect basic-block))))


#|z
(setf x (restore "dumps/Hello.build-basic-blocks"))

(dolist (bb (build-basic-blocks x))
  (print (slot-value bb 'code)))

(maphash (lambda (k v)
           (format t "~A: ~A~%" k v))
         (find-target-instructions))
|#
