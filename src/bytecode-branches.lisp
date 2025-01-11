;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: OPENLDK; Base: 10 -*-
;;;
;;; Copyright (C) 2023, 2024, 2025  Anthony Green <green@moxielogic.com>
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

(defparameter +bytecode-1-byte+
  '(:AALOAD :AASTORE
    :ACONST_NULL
    :ALOAD_0 :ALOAD_1 :ALOAD_2 :ALOAD_3
    :ARRAYLENGTH
    :ARETURN
    :ASTORE_0 :ASTORE_1 :ASTORE_2 :ASTORE_3
    :ATHROW
    :CALOAD :CASTORE
    :DADD :DCONST_0 :DDIV :DLOAD_2 :DMUL :DSTORE_2 :DSUB
    :DRETURN
    :DUP
    :F2I
    :FCMPG :FCMPL
    :FCONST_0
    :FLOAD_0 :FLOAD_1 :FLOAD_2
    :FMUL
    :FRETURN
		:I2F :I2L
    :IADD :IAND :IASTORE :IOR
    :ICONST_0 :ICONST_1 :ICONST_2 :ICONST_3 :ICONST_4 :ICONST_5
    :IDIV :IMUL
    :ILOAD_0 :ILOAD_1 :ILOAD_2 :ILOAD_3
    :IREM
    :IRETURN
    :ISHL :ISHR
    :ISTORE_1 :ISTORE_2 :ISTORE_3
    :ISUB
    :LADD :LAND
    :LOR
		:LCMP
    :LCONST_0 :LCONST_1
    :LLOAD_0 :LLOAD_1 :LLOAD_2 :LLOAD_3
    :LSHL :LSHR
		:LSUB
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
    :IF_ACMPEQ :IF_ACMPNE :IF_ICMPEQ :IF_ICMPGE :IF_ICMPLE :IF_ICMPLT :IF_ICMPNE :IFEQ :IFGE :IFLE
    :IFLT :IFGT :IFNE :IFNONNULL :IFNULL
    :INSTANCEOF :INVOKEVIRTUAL :INVOKESPECIAL :INVOKESTATIC
		:LDC2_W
		:NEW :ANEWARRAY
    :PUTFIELD :PUTSTATIC :SIPUSH))

(defparameter +bytecode-5-byte+
  '(:INVOKEINTERFACE))

(defparameter +bytecode-short-branch-table+
  (let ((sbtable (make-hash-table)))
    (dolist (o '(:GOTO :IF_ACMPEQ :IF_ACMPNE :IF_ICMPLE :IF_ICMPLT :IF_ICMPEQ :IF_ICMPGE :IF_ICMPNE :IFEQ :IFGE :IFLE :IFLT :IFGT :IFNE :IFNONNULL :IFNULL))
      (setf (gethash o sbtable) t))
    sbtable))

(defparameter +bytecode-conditional-branch-table+
  (let ((cbtable (make-hash-table)))
    (dolist (o '(:IF_ACMPEQ :IF_ACMPNE :IF_ICMPLE :IF_ICMPLT :IF_ICMPEQ :IF_ICMPGE :IF_ICMPNE :IFEQ :IFGE :IFLE :IFLT :IFGT :IFNE :IFNONNULL :IFNULL))
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
