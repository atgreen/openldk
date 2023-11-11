(in-package :openldk)

(defclass <block> ()
  ((successors :initform (list))
   (fixed-up-p :initform nil)))

(defclass <basic-block> (<block>)
  ((code :initform (list))))

(defclass <try-block> (<block>)
  ((try-body :initform (list))
   (catch-blocks :initform (list))))

(defun get-short-branch-targets (pc code)
  (let ((start_pc pc)
        (offset (+ (* (aref code (incf pc)) 256)
                   (aref code (incf pc)))))
    (if (gethash (aref +opcodes+ (aref code start_pc)) +bytecode-conditional-branch-table+)
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
         (try-block-table (make-hash-table))
         (successor-table (make-hash-table)))

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
                                (setf (gethash pc successor-table)
                                      (list (+ pc insn-length) handler-pc))
                                (setf (gethash (incf pc insn-length) branch-target-table) t)
                                (setf (gethash pc branch-target-table) t))
                              (incf pc insn-length))))
                   (setf (gethash handler-pc branch-target-table) t)))))

    (values branch-target-table try-block-table successor-table)))

(defun fixup-branch-insns (basic-block)
  "Find all branch instructions within BASIC-BLOCKS and update them
with target BASIC-BLOCK links."
  (with-slots (code successors fixed-up-p) basic-block
    (unless fixed-up-p
      (when successors
        (progn
          (set-successors (car (last code)) successors)
          (setf fixed-up-p t)
          (dolist (b successors)
            (when b
              (fixup-branch-insns b))))))))

(defun find-ssa-node (address)
  (format t "Z: ~A~%" (slot-value *context* 'ssa-code))
  (let ((ssa-code (slot-value *context* 'ssa-code)))
    (loop
      do (if (eq address (slot-value (car ssa-code) 'address))
             (progn
               (format t "W: ~A~%" address)
               (return ssa-code))
             (setf ssa-code (cdr ssa-code))))))

(defun extract-outer-ete (ete-list)
  (let* ((largest-address (loop for ete in ete-list
                                maximize (slot-value ete 'end-pc)))
         (outer-ete-list (loop for ete in ete-list
                               when (eq (slot-value ete 'end-pc) largest-address)
                                 collect ete))
         (remaining-ete-list (loop for ete in ete-list
                                   when (not (eq (slot-value ete 'end-pc) largest-address))
                                     collect ete)))
    (values outer-ete-list remaining-ete-list)))

(defun build-basic-blocks (ssa-code)
  "Build <BASIC-BLOCK> objects from SSA-CODE, but stop at END-ADDRESS if
non-null.  Return the entry block."
  (dump "build-basic-blocks" ssa-code)
  (multiple-value-bind (branch-targets try-block-table successor-table)
      (find-target-instructions)
    (setf ssa-code (insert-branch-targets ssa-code branch-targets))
    ;;
    ;; We now have three hashtables, keyed on instruction address.
    ;; These identify branch targets, the start of try blocks, and
    ;; successor addresses in case of branches, gotos, etc.
    ;;
    (let ((block-by-entry-address (make-hash-table)))
      (labels (;; Add instructions to a block.  They are in reverse
               ;; order and will need to be resorted once we are done.
               (%add-to-block (basic-block insn)
                 (with-slots (code) basic-block
                   (push insn code)))

               ;; Create a block, starting with SSA-CODE.
               (%build-basic-block (ssa-code &optional (end-address nil))
                 (format t "NEW BLOCK: ~A ~A~%" (slot-value (car ssa-code) 'address) ssa-code)
                 (when ssa-code
                   (with-accessors ((ssa-code-address address)) (car ssa-code)
                     (let* ((block-ete-list (gethash ssa-code-address try-block-table)))
                       (format t "===== block-ete-list: ~A ~A ~A~%" ssa-code-address try-block-table block-ete-list)
                       (format t "eoe: ~A~%" (extract-outer-ete block-ete-list))
                       (multiple-value-bind (outer-ete-list remaining-ete-list)
                           (extract-outer-ete block-ete-list)
                         ;; Remove the outer ETE list from TRY-BLOCK-TABLE
                         (setf (gethash ssa-code-address try-block-table) remaining-ete-list)

                         (if (or (null end-address) (< ssa-code-address end-address))

                             (if outer-ete-list

                                 ;; We are in a TRY block
                                 (let ((bloc (make-instance '<try-block>)))
                                   (setf (slot-value bloc 'try-body)
                                         (%build-basic-block-list ssa-code
								  (loop for ete in outer-ete-list maximize (slot-value ete 'end-pc))))
                                   (setf (slot-value bloc 'catch-blocks)
                                                     (loop for ete in outer-ete-list
                                                           collect (%build-basic-block
                                                                    (find-ssa-node (slot-value ete 'handler-pc)))))
                                   (format t "UU: ~A~%" bloc)
                                   (return-from %build-basic-block (values bloc nil)))

                               ;; We are in a BASIC block
                               (let ((bloc (make-instance '<basic-block>))
                                     (entry-insn t))

                                 ;; Keep track of blocks by their entry pc.
                                 (setf (gethash ssa-code-address block-by-entry-address) bloc)

                                 ;; Loop through all of the instructions,
                                 ;; adding them to the current block, until we reach the
                                 ;; start of a new block or END-ADDRESS.
                                 (loop
                                   for insn = (car ssa-code)
                                   while insn
                                   for address = (slot-value insn 'address)
                                   while (or (null end-address) (< address end-address))

                                   ;; If this instruction has successors,
                                   ;; add them to the block.
                                   do (progn
                                        (format t "P ~A~%" (gethash address successor-table))
                                        (setf (slot-value bloc 'successors)
                                              (append (gethash address successor-table) (slot-value bloc 'successors))))

                                   do (progn
                                        ;; ETE will be non-null if we
                                        ;; are at the start of an
                                        ;; exception range.  This
                                        ;; could be try on entry to a
                                        ;; block, or we have reached
                                        ;; the end of block.
                                        (format t "checking ~A ~A ~A ~A~%" address (gethash address successor-table) entry-insn (gethash address branch-targets))
                                        (when (or entry-insn
                                                  (not (gethash address branch-targets)))
                                          (format t "   adding ~A to ~A~%" insn bloc)
                                          (%add-to-block bloc insn)
                                          (setf entry-insn nil)
                                          (setf (gethash address branch-targets) nil)
                                          (pop ssa-code)
                                          (let ((successors (gethash address successor-table)))
                                            (dolist (successor successors)
                                              (let ((successor-block (%build-basic-block (find-ssa-node successor))))
                                                (format t "V ~A~%" successor-block)
                                                (push successor-block (slot-value bloc 'successors)))))

                                              ;; Generate catch handler blocks, if necessary
                                          #|
                                              (format t "ete-list length = ~A~%" (length ete-list))
                                          (dolist (ete ete-list)
                                          (format t "E: ~A~%" ete)
                                          (push (%build-basic-block (find-ssa-node (slot-value ete 'handler-pc)))
                                          (slot-value basic-bloc 'catch-blocks)))
                                          |#

                                          (with-slots (code) bloc
                                            (setf code (reverse code)))
                                          (format t "RETURNING ~A ~A~%" bloc ssa-code)
                                          (return-from %build-basic-block (values bloc ssa-code))))

                                   finally (with-slots (code) bloc
                                             (format t "B: ~A ~A~%" address end-address)
                                             (setf code (nreverse code))

                                             ;; Generate catch handler blocks, if necessary
                                             #|
                                             (dolist (ete ete-list)
                                             (format t "C: ~A~%" ete))
                                             |#

                                             (return-from %build-basic-block (if (slot-value bloc 'code) (values bloc 1) (values 2 1)))))
                                 (format t "ZZZZZZZZZZZZZZ~%")
                                 (return-from %build-basic-block (values bloc nil))
				 ))))))) (print "DDDDDDDDDDDDDDDDD") )

	       (%build-basic-block-list (ssa-code &optional (end-address 9999999))
		 (loop with current-ssa = ssa-code
		       while (and current-ssa (< (slot-value (car current-ssa) 'address) end-address))
		       for (bloc rest-ssa) = (multiple-value-list (%build-basic-block current-ssa))
		       collect bloc into results
		       do (format t "BBBL CONTINUE: ~A ~A~%" bloc rest-ssa)
		       do (setf current-ssa rest-ssa)
		       finally (return results))))
	
	(format t "DD: ~A~%" ssa-code)
        (let ((blocks (%build-basic-block-list ssa-code)))loop
          (format t "QQQ ~A~%" blocks)
          (dolist (b blocks)
            ;; (print (slot-value b 'code))
            (let ((sb (list)))
              (format t "R ~A~%" b)
              (dolist (pc (slot-value b 'successors))
                (push (gethash pc block-by-entry-address) sb))
              (setf (slot-value b 'successors) sb)))
          ;; Update all of the branch ssa nodes with links to the target blocks.
          (fixup-branch-insns (car blocks))
          blocks)))))
