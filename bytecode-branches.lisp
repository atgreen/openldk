(in-package :openldk)

(defparameter +bytecode-1-byte+
  '(:ACONST_NULL
    :ALOAD_0 :ALOAD_1 :ALOAD_2 :ALOAD_3
    :ARRAYLENGTH
    :ARETURN
    :ASTORE_0 :ASTORE_1 :ASTORE_2 :ASTORE_3
    :ATHROW
    :DADD :DCONST_0 :DDIV :DLOAD_2 :DMUL :DSTORE_2 :DSUB
    :DUP
    :IADD
    :ICONST_0 :ICONST_1 :ICONST_2
    :IDIV
    :ILOAD_0 :ILOAD_1 :ILOAD_2 :ILOAD_3
    :ISTORE_2
    :IRETURN
    :ISHL
    :ISUB
    :MONITORENTER :MONITOREXIT
    :NOP
    :POP
    :RETURN))

(defparameter +bytecode-2-byte+
  '(:ASTORE :BIPUSH :DLOAD :DSTORE :ILOAD :LDC :NEWARRAY))

(defparameter +bytecode-3-byte+
  '(:GETFIELD :GETSTATIC :GOTO :IF_ICMPLE :IFEQ :IFGE :IFLE :IFNE :IFNONNULL :IFNULL
    :INSTANCEOF :INVOKEVIRTUAL :INVOKESPECIAL :INVOKESTATIC :NEW :ANEWARRAY
    :PUTFIELD :PUTSTATIC :SIPUSH))

(defparameter +bytecode-short-branch-table+
  (let ((sbtable (make-hash-table)))
    (dolist (o '(:GOTO :IF_ICMPLE :IFGE :IFLE :IFNE :IFNONNULL :IFNULL))
      (setf (gethash o sbtable) t))
    sbtable))

(defparameter +bytecode-conditional-branch-table+
  (let ((cbtable (make-hash-table)))
    (dolist (o '(:IF_ICMPLE :IFGE :IFLE :IFNE :IFNONNULL :IFNULL))
      (setf (gethash o cbtable) t))
    cbtable))

(defparameter +bytecode-lengths-table+
  (let ((bltable (make-hash-table)))
    (dolist (o +bytecode-1-byte+)
      (setf (gethash o bltable) 1))
    (dolist (o +bytecode-2-byte+)
      (setf (gethash o bltable) 2))
    (dolist (o +bytecode-3-byte+)
      (setf (gethash o bltable) 3))
    bltable))
