(in-package :openldk)

(defvar *block-counter* 0)

(defun generate-id ()
  (incf *block-counter*))

(defclass <block> ()
  ((id :reader .id :initform (generate-id))
   (code-emitted-p :initform nil)
   (successor-addresses :reader .successor-addresses :initform (list))
   (successor-blocks :reader .successor-blocks :initform (list))
   (fixed-up-p :initform nil)))

(defclass <basic-block> (<block>)
  ((code :reader .code :initform (list))))

(defclass <try-block> (<block>)
  ((try-body :reader .try-body :initform nil)
   (catch-blocks :reader .catch-blocks :initform (list))))

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
        (format stream "~A -> ~A~%" (.id bloc) (.id successor))))))

(defmethod dump-dot ((bloc <try-block>) done-table stream)
  (dump-dot (.try-body bloc) done-table stream)
  (dolist (catch-block (.catch-blocks bloc))
    (dump-dot (cdr catch-block) done-table stream)
    (format stream "~A -> ~A [label=\"catch\"];~%" (.id bloc) (.id (cdr catch-block))))
  (dolist (successor (.successor-blocks bloc))
    (dump-dot successor done-table stream)
    (format stream "~A -> ~A;~%" (.id bloc) (.id successor))))

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
                                     (format t "FIXME-SAT: ~A ~A~%" pc targets)
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

(defun find-ssa-node (address)
  (let ((ssa-code (slot-value *context* 'ssa-code)))
    (loop
      do (if (eq address (slot-value (car ssa-code) 'address))
             (progn
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
  (multiple-value-bind (branch-targets try-block-table successor-address-table)
      (find-target-instructions)
    ;; (setf ssa-code (insert-branch-targets ssa-code branch-targets))
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
                 (format t "FIXME3 ========================================================~%")
                 (when ssa-code
                   (with-accessors ((ssa-code-address address)) (car ssa-code)
                     (let* ((block-ete-list (gethash ssa-code-address try-block-table)))
                       (multiple-value-bind (outer-ete-list remaining-ete-list)
                           (extract-outer-ete block-ete-list)
                         ;; Remove the outer ETE list from TRY-BLOCK-TABLE
                         (setf (gethash ssa-code-address try-block-table) remaining-ete-list)

                         (if (or (null end-address) (< ssa-code-address end-address))

                             (if outer-ete-list

                                 ;; We are in a TRY block
                                 (let ((bloc (make-instance '<try-block>)))
                                   (setf (slot-value bloc 'try-body)
                                         (%build-basic-block ssa-code
                                                             (loop for ete in outer-ete-list maximize (slot-value ete 'end-pc))))
                                   (setf (slot-value bloc 'catch-blocks)
                                         (loop for ete in outer-ete-list
                                               collect (cons
                                                        (slot-value ete 'catch-type)
                                                        (%build-basic-block
                                                         (find-ssa-node (slot-value ete 'handler-pc))))))
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
                                     for address = (and insn (.address insn))
                                     while insn
                                     until (and end-address (>= address end-address))
                                     until (and (not entry-insn) (gethash address branch-targets))

                                     do (format t "FIXME2: ~A: ~A ~A ~A~%" address entry-insn (gethash address branch-targets) insn)

                                     ;; If this instruction has successors,
                                     ;; add them to the block.
                                     do (progn
                                          (format t "FIXME4: ~A~%" (gethash address successor-address-table))
                                          (setf (slot-value bloc 'successor-addresses)
                                                (append (gethash address successor-address-table) (slot-value bloc 'successor-addresses))))

                                     do (progn
                                          ;; ETE will be non-null if we
                                          ;; are at the start of an
                                          ;; exception range.  This
                                          ;; could be try on entry to a
                                          ;; block, or we have reached
                                          ;; the end of block.
                                          (if (or entry-insn
                                                  (not (gethash address branch-targets)))
                                              (progn
                                                (format t "FIXME1: ~A ~A ~A ~A~%" address entry-insn (gethash address branch-targets) insn)
                                                (%add-to-block bloc insn)
                                                (setf entry-insn nil)
                                                ;; FIXME - this is causing duplicate code gen in Hello
                                                (setf (gethash address branch-targets) nil)
                                                (pop ssa-code)

                                                (if (typep insn (find-class 'ssa-return))
                                                    (with-slots (code) bloc
                                                      (setf code (reverse code))
                                                      (return-from %build-basic-block (values bloc nil))))

                                                (let ((successors (gethash address successor-address-table)))
                                                  (format t "FIXME888: ~A ~A~%" address successors)
                                                  (dolist (successor successors)
                                                    (let ((successor-block (or (gethash successor block-by-entry-address)
                                                                               (%build-basic-block (find-ssa-node successor)))))
                                                      (format t "    SB: ~A~%" successor-block)
                                                      (push successor-block (slot-value bloc 'successor-blocks))))
                                                  (if successors
                                                      (with-slots (code) bloc
                                                        (setf code (reverse code))
                                                        (return-from %build-basic-block (values bloc nil))))))

                                              ;; Generate catch handler blocks, if necessary

                                              #|
                                              (format t "ete-list length = ~A~%" (length ete-list))
                                          (dolist (ete ete-list)
                                          (format t "E: ~A~%" ete)
                                          (push (%build-basic-block (find-ssa-node (slot-value ete 'handler-pc)))
                                          (slot-value basic-bloc 'catch-blocks)))
                                              |#
                                              (with-slots (code) bloc
                                                (setf code (reverse code))
                                                (return-from %build-basic-block (values bloc ssa-code)))))

                                     finally (with-slots (code) bloc
                                               (setf code (nreverse code))
                                               (when address
                                                 (let ((successor-block (%build-basic-block ssa-code)))
                                                   (push successor-block (slot-value bloc 'successor-blocks))))

                                             ;; Generate catch handler blocks, if necessary
                                             #|
                                               (dolist (ete ete-list)
                                               (format t "C: ~A~%" ete))
                                               |#

                                               (return-from %build-basic-block (values bloc nil))))
                                   (return-from %build-basic-block (values bloc nil))
                                   ))))))))

               (%build-basic-block-list (ssa-code &optional (end-address 9999999))
                 (loop with current-ssa = ssa-code
                       while (and current-ssa (< (slot-value (car current-ssa) 'address) end-address))
                       for (bloc rest-ssa) = (multiple-value-list (%build-basic-block current-ssa))
                       collect bloc into results
                       do (setf current-ssa rest-ssa)
                       finally (return results))))

        (let ((blocks (%build-basic-block-list ssa-code)))
#|
          (dolist (b blocks)
            ;; (print (slot-value b 'code))
            (let ((sb (list)))
              (dolist (pc (.successor-addresses b))
                (push (gethash pc block-by-entry-address) sb))
          (setf (slot-value b 'successor-addresses) sb)))
          |#
          ;; Update all of the branch ssa nodes with links to the target blocks.
          (dump-method-dot blocks)
          blocks)))))
