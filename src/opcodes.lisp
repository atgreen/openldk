;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: OPENLDK; Base: 10 -*-
;;;
;;; Copyright (C) 2023, 2024  Anthony Green <green@moxielogic.com>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later WITH Classpath-exception-2.0
;;;
;;; This file is part of OpenLDK.

;;; OpenLDK is free software; you can redistribute it and/or modify it
;;; under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 3, or (at your
;;; option) any later version.

;;; OpenLDK is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; General Public License for more details.

;;; You should have received a copy of the GNU General Public License
;;; along with OpenLDK; see the file COPYING.  If not, please see
;;; <http://www.gnu.org/licenses/>.

;;; Linking this library statically or dynamically with other modules is
;;; making a combined work based on this library.  Thus, the terms and
;;; conditions of the GNU General Public License cover the whole
;;; combination.

;;; As a special exception, the copyright holders of this library give
;;; you permission to link this library with independent modules to
;;; produce an executable, regardless of the license terms of these
;;; independent modules, and to copy and distribute the resulting
;;; executable under terms of your choice, provided that you also
;;; meet, for each linked independent module, the terms and conditions
;;; of the license of that module.  An independent module is a module
;;; which is not derived from or based on this library.  If you modify
;;; this library, you may extend this exception to your version of the
;;; library, but you are not obligated to do so.  If you do not wish
;;; to do so, delete this exception statement from your version.

(in-package :openldk)

(defparameter +opcodes+
  #(:NOP
    :ACONST_NULL
    :ICONST_M1
    :ICONST_0
    :ICONST_1
    :ICONST_2
    :ICONST_3
    :ICONST_4
    :ICONST_5
    :LCONST_0
    :LCONST_1
    :FCONST_0
    :FCONST_1
    :FCONST_2
    :DCONST_0
    :DCONST_1
    :BIPUSH
    :SIPUSH
    :LDC
    :LDC_W
    :LDC2_W
    :ILOAD
    :LLOAD
    :FLOAD
    :DLOAD
    :ALOAD
    :ILOAD_0
    :ILOAD_1
    :ILOAD_2
    :ILOAD_3
    :LLOAD_0
    :LLOAD_1
    :LLOAD_2
    :LLOAD_3
    :FLOAD_0
    :FLOAD_1
    :FLOAD_2
    :FLOAD_3
    :DLOAD_0
    :DLOAD_1
    :DLOAD_2
    :DLOAD_3
    :ALOAD_0
    :ALOAD_1
    :ALOAD_2
    :ALOAD_3
    :IALOAD
    :LALOAD
    :FALOAD
    :DALOAD
    :AALOAD
    :BALOAD
    :CALOAD
    :SALOAD
    :ISTORE
    :LSTORE
    :FSTORE
    :DSTORE
    :ASTORE
    :ISTORE_0
    :ISTORE_1
    :ISTORE_2
    :ISTORE_3
    :LSTORE_0
    :LSTORE_1
    :LSTORE_2
    :LSTORE_3
    :FSTORE_0
    :FSTORE_1
    :FSTORE_2
    :FSTORE_3
    :DSTORE_0
    :DSTORE_1
    :DSTORE_2
    :DSTORE_3
    :ASTORE_0
    :ASTORE_1
    :ASTORE_2
    :ASTORE_3
    :IASTORE
    :LASTORE
    :FASTORE
    :DASTORE
    :AASTORE
    :BASTORE
    :CASTORE
    :SASTORE
    :POP
    :POP2
    :DUP
    :DUP_X1
    :DUP_X2
    :DUP2
    :DUP2_X1
    :DUP2_X2
    :SWAP
    :IADD
    :LADD
    :FADD
    :DADD
    :ISUB
    :LSUB
    :FSUB
    :DSUB
    :IMUL
    :LMUL
    :FMUL
    :DMUL
    :IDIV
    :LDIV
    :FDIV
    :DDIV
    :IREM
    :LREM
    :FREM
    :DREM
    :INEG
    :LNEG
    :FNEG
    :DNEG
    :ISHL
    :LSHL
    :ISHR
    :LSHR
    :IUSHR
    :LUSHR
    :IAND
    :LAND
    :IOR
    :LOR
    :IXOR
    :LXOR
    :IINC
    :I2L
    :I2F
    :I2D
    :L2I
    :L2F
    :L2D
    :F2I
    :F2L
    :F2D
    :D2I
    :D2L
    :D2F
    :I2B
    :I2C
    :I2S
    :LCMP
    :FCMPL
    :FCMPG
    :DCMPL
    :DCMPG
    :IFEQ
    :IFNE
    :IFLT
    :IFGE
    :IFGT
    :IFLE
    :IF_ICMPEQ
    :IF_ICMPNE
    :IF_ICMPLT
    :IF_ICMPGE
    :IF_ICMPGT
    :IF_ICMPLE
    :IF_ACMPEQ
    :IF_ACMPNE
    :GOTO
    :JSR
    :RET
    :TABLESWITCH
    :LOOKUPSWITCH
    :IRETURN
    :LRETURN
    :FRETURN
    :DRETURN
    :ARETURN
    :RETURN
    :GETSTATIC
    :PUTSTATIC
    :GETFIELD
    :PUTFIELD
    :INVOKEVIRTUAL
    :INVOKESPECIAL
    :INVOKESTATIC
    :INVOKEINTERFACE
    :INVOKEDYNAMIC
    :NEW
    :NEWARRAY
    :ANEWARRAY
    :ARRAYLENGTH
    :ATHROW
    :CHECKCAST
    :INSTANCEOF
    :MONITORENTER
    :MONITOREXIT
    :WIDE
    :MULTIANEWARRAY
    :IFNULL
    :IFNONNULL
    :GOTO_W
    :JSR_W))
