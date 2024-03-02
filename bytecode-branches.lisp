(in-package :openldk)

(defparameter +bytecode-1-byte+
  '(:AALOAD :AASTORE
    :ACONST_NULL
    :ALOAD_0 :ALOAD_1 :ALOAD_2 :ALOAD_3
    :ARRAYLENGTH
    :ARETURN
    :ASTORE_0 :ASTORE_1 :ASTORE_2 :ASTORE_3
    :ATHROW
    :CASTORE
    :DADD :DCONST_0 :DDIV :DLOAD_2 :DMUL :DSTORE_2 :DSUB
    :DUP
    :IADD :IALOAD
    :IASTORE
    :ICONST_0 :ICONST_1 :ICONST_2 :ICONST_3 :ICONST_4 :ICONST_5
    :IDIV
    :ILOAD_0 :ILOAD_1 :ILOAD_2 :ILOAD_3
    :INEG
    :ISTORE_1 :ISTORE_2 :ISTORE_3
    :IRETURN
    :ISHL
    :ISUB
    :LUSHR
    :MONITORENTER :MONITOREXIT
    :NOP
    :POP
    :RETURN))

(defparameter +bytecode-2-byte+
  '(:ALOAD :ASTORE :BIPUSH :DLOAD :DSTORE :ILOAD :ISTORE :LDC :NEWARRAY))

(defparameter +bytecode-3-byte+
  '(:CHECKCAST
    :GETFIELD :GETSTATIC :GOTO :IINC
    :IF_ACMPEQ :IF_ACMPNE :IF_ICMPEQ :IF_ICMPGE :IF_ICMPGT :IF_ICMPLE :IF_ICMPNE :IFEQ :IFGE :IFLE
    :IFNE :IFNONNULL :IFNULL
    :INSTANCEOF :INVOKEVIRTUAL :INVOKESPECIAL :INVOKESTATIC :NEW :ANEWARRAY
    :PUTFIELD :PUTSTATIC :SIPUSH))

(defparameter +bytecode-5-byte+
  '(:INVOKEINTERFACE))

(defparameter +bytecode-short-branch-table+
  (let ((sbtable (make-hash-table)))
    (dolist (o '(:GOTO :IF_ICMPLE :IF_ICMPEQ :IF_ICMPGE :IF_ICMPGT :IF_ICMPNE :IFEQ :IFGE :IFLE :IFNE :IFNONNULL :IFNULL))
      (setf (gethash o sbtable) t))
    sbtable))

(defparameter +bytecode-conditional-branch-table+
  (let ((cbtable (make-hash-table)))
    (dolist (o '(:IF_ICMPLE :IF_ICMPEQ :IF_ICMPGE :IF_ICMPGT :IF_ICMPNE :IFEQ :IFGE :IFLE :IFNE :IFNONNULL :IFNULL))
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
    (dolist (o +bytecode-5-byte+)
      (setf (gethash o bltable) 5))
    bltable))
