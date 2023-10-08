(in-package :openldk)

(defparameter +bytecode-1-byte+
  '(:ACONST_NULL
    :ALOAD_0
    :ALOAD_1
    :ASTORE_1
    :ATHROW
    :DADD :DCONST_0 :DDIV :DLOAD_2 :DMUL :DSTORE_2 :DSUB
    :DUP
    :RETURN))

(defparameter +bytecode-2-byte+
  '(:ASTORE :DLOAD :DSTORE :LDC))

(defparameter +bytecode-3-byte+ '(:GETSTATIC :GOTO :IF_ICMPLE
  :INVOKEVIRTUAL :INVOKESPECIAL :INVOKESTATIC :NEW :PUTSTATIC :SIPUSH))

(defparameter +bytecode-short-branch-table+
  (let ((sbtable (make-hash-table)))
    (dolist (o '(:GOTO :IF_ICMPLE))
      (setf (gethash o sbtable) t))
    sbtable))

(defparameter +bytecode-lengths-table+
  (let ((bltable (make-hash-table)))
    (dolist (o +bytecode-1-byte+)
      (setf (gethash o bltable) 1))
    (dolist (o +bytecode-2-byte+)
      (setf (gethash o bltable) 2))
    (dolist (o +bytecode-3-byte+)
      (setf (gethash o bltable) 3))
    bltable))

(defun get-short-branch-targets (pc code)
  (let ((start_pc pc)
        (offset (+ (* (aref code (incf pc)) 256)
                   (aref code (incf pc)))))
      (list pc (+ start_pc offset))))

(defun find-branch-targets (code)
  "Return a HASH-TABLE with all of the offsets in CODE that are branch
targets."
  (let ((pc 0)
        (length (length code))
        (branch-target-table (make-hash-table)))
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
    branch-target-table))
