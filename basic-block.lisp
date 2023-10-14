(in-package :openldk)

(defclass <basic-block> ()
  ((code :initform (list))
   (successors)))

(defun get-short-branch-targets (pc code)
  (let ((start_pc pc)
        (offset (+ (* (aref code (incf pc)) 256)
                   (aref code (incf pc)))))
    (if (gethash (aref code start_pc) +bytecode-conditional-branch-table+)
        (list (1+ pc) (+ start_pc offset))
        (list (+ start_pc offset)))))

(defvar *instruction-exceptions* (make-hash-table))

(defmacro define-instruction-exceptions (opcode exceptions)
  `(setf (gethash ,opcode *instruction-exceptions*) ,exceptions))

(define-instruction-exceptions :IDIV
 '("java/lang/ArithmeticException" "java/lang/Exception" "java/lang/Throwable"))

(defun opcode-throws-p (opcode throwable)
  (let ((throwables (gethash opcode *instruction-exceptions*)))
    (find throwable throwables :test #'string=)))

(defun find-target-instructions ()
  "Return two HASH-TABLE objects.  The first maps a bytecode index key
with lists of successor bytecode indices. The second includes entries
for all of the program indices in the current code that are branch or
exception targets."
  (let* ((code (slot-value *context* 'bytecode))
         (exception-table (slot-value *context* 'exception-table))
         (pc 0)
         (length (length code))
         (branch-target-table (make-hash-table))
         (successor-table (make-hash-table)))

    ;; First, let's go through the instructions looking
    ;; for branch targets.
    (dolist (bt (remove-duplicates
                 (apply #'append (loop
                                   while (< pc length)
                                   for result = (let* ((opcode (aref +opcodes+ (aref code pc)))
                                                       (targets (if (gethash opcode +bytecode-short-branch-table+)
                                                                    (get-short-branch-targets pc code))))
                                                  (when targets
                                                    (setf (gethash pc successor-table) targets))
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
                   (loop
                     with pc = start-pc
                     until (eq pc end-pc)
                     do (let* ((opcode (aref +opcodes+ (aref code pc)))
                               (insn-length (gethash opcode +bytecode-lengths-table+)))
                          (if (opcode-throws-p opcode catch-type)
                              (progn
                                (setf (gethash pc successor-table)
                                      (list (+ pc insn-length) handler-pc))
                                (setf (gethash (incf pc insn-length) branch-target-table) t)
                                (setf (gethash pc branch-target-table) t))
                              (incf pc insn-length))))
                   (setf (gethash handler-pc branch-target-table) t)))))

    (values successor-table branch-target-table)))

(defun build-basic-blocks (ssa-code)
  (dump "build-basic-blocks" ssa-code)
  (multiple-value-bind (successor-table branch-targets)
      (find-target-instructions)
    (labels ((%add-to-block (basic-block insn)
               (with-slots (code) basic-block
                 (push insn code)))
             (%build-basic-blocks (ssa-code)
               (let ((basic-bloc (make-instance '<basic-block>))
                     (entry-insn t))
                 (multiple-value-prog1
                     (loop
                       for insn = (car ssa-code)
                       while insn
                       for pc-index = (slot-value insn 'pc-index)
                       do (if (or entry-insn
                                  (not (gethash pc-index branch-targets)))
                              (progn
                                (%add-to-block basic-bloc insn)
                                (setf entry-insn nil)
                                (setf (gethash pc-index branch-targets) nil)
                                (pop ssa-code))
                              (progn
                                ;; This is the end of the basic block
                                (setf (gethash pc-index branch-targets) nil)
                                (setf (slot-value basic-bloc 'successors) (gethash pc-index successor-table))
                                (return (progn
                                          (with-slots (code) basic-bloc
                                            (setf code (reverse code)))
                                          (values basic-bloc ssa-code)))))
                       finally (with-slots (code) basic-bloc
                                 (setf code (nreverse code))
                                 (return (if (slot-value basic-bloc 'code) (values basic-bloc nil) (values nil nil)))))
                   (values basic-bloc nil)))))
      (loop
        with current-ssa = ssa-code
        for (basic-block rest-ssa) = (multiple-value-list (%build-basic-blocks current-ssa))
        while basic-block
        do (setf current-ssa rest-ssa)
        collect basic-block))))

#|

(setf x (restore "dumps/java/util/Vector<init>(II)V.build-basic-blocks"))

(dolist (bb (build-basic-blocks x))
  (print (slot-value bb 'code)))

(maphash (lambda (k v)
           (format t "~A: ~A~%" k v))
         (find-target-instructions))
|#
