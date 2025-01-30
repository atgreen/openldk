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

(defclass/std ir-node ()
  ((address :std -1)))

(defmethod print-object ((node ir-node) out)
  (print-unreadable-object (node out :type t)
    (format out "~A" (slot-value node 'address))))

(defmethod initialize-instance :after ((ir ir-node) &key)
  (assert (or (typep ir '<stack-bottom-marker>)
              (not (eq (slot-value ir 'address ) +stack-bottom-address+)))))

(defmethod dot-dump-string ((node ir-node))
  (format nil "~3A: ~A" (address node) (class-name (class-of node))))

(defmethod uses-stack-p ((node ir-node))
  nil)

(defclass/std ir-xastore (ir-node)
  ((arrayref)
   (index)
   (value)))

(defclass/std ir-aastore (ir-xastore)
  ())

(defclass/std ir-aload (ir-node)
  ((index)))

(defclass/std ir-nop (ir-node)
  ())

(defclass/std ir-stop-marker (ir-nop)
  ())

(defclass/std ir-literal (ir-node)
  ((value)
   (type :with)))

(defmethod emit ((v ir-literal) cp)
  (slot-value v 'value))

(defclass/std ir-local-variable (ir-node)
  ((index)
   (jtype)))

(defclass/std ir-long-local-variable (ir-node)
  ((index)))

(defclass/std ir-null-literal (ir-literal)
  ())

(defclass/std ir-string-literal (ir-literal)
  ())

(defclass/std ir-double-literal (ir-literal)
  ())

(defclass/std ir-iastore (ir-xastore)
  ())

(defclass/std ir-bastore (ir-xastore)
  ())

(defclass/std ir-fcmpg (ir-binop)
  ())

(defclass/std ir-fcmpl (ir-binop)
  ())

(defclass/std ir-float-literal (ir-literal)
  ())

(defclass/std ir-double-literal (ir-literal)
  ())

(defclass/std ir-int-literal (ir-literal)
  ())

(defclass/std ir-long-literal (ir-literal)
  ())

(defclass/std ir-array-index (ir-node)
  ((index)
   (arrayref)))

(defclass/std ir-caload (ir-array-index)
  ())

(defclass/std ir-aaload (ir-array-index)
  ())

(defclass/std ir-iaload (ir-array-index)
  ())

(defclass/std ir-baload (ir-array-index)
  ())

(defclass/std ir-castore (ir-xastore)
  ())

(defclass/std ir-class (ir-node)
  ((class :with)))
(define-print-object/std ir-class)

(defclass/std ir-array-length (ir-node)
  ((arrayref)))

(defclass/std ir-assign (ir-node)
  ((lvalue rvalue)))

(defmethod print-object ((node ir-assign) out)
  (print-unreadable-object (node out :type t)
    (format out "~A: ~A = ~A" (slot-value node 'address) (slot-value node 'lvalue) (slot-value node 'rvalue))))

(defclass/std ir-binop (ir-node)
  ((value1)
   (value2)))

(defclass/std ir-iadd (ir-binop)
  ())

(defclass/std ir-ladd (ir-binop)
  ())

(defclass/std ir-iand (ir-binop)
  ())

(defclass/std ir-ixor (ir-binop)
  ())

(defclass/std ir-lxor (ir-binop)
  ())

(defclass/std ir-ineg (ir-unop)
  ())

(defclass/std ir-land (ir-binop)
  ())

(defclass/std ir-ior (ir-binop)
  ())

(defclass/std ir-lor (ir-binop)
  ())

(defmethod uses-stack-p (ir-add)
  t)

(defclass/std ir-branch (ir-node)
  ((offset successors)))

(defclass/std ir-imul (ir-binop)
  ())

(defclass/std ir-idiv (ir-binop)
  ())

(defclass/std ir-lmul (ir-binop)
  ())

(defclass/std ir-ldiv (ir-binop)
  ())

(defclass/std ir-irem (ir-binop)
  ())

(defclass/std ir-dup2 (ir-node)
  ())

(defclass/std ir-goto (ir-branch)
  ())

(defclass/std ir-unop (ir-node)
  ((value)))

(defclass/std ir-l2f (ir-unop)
  ())

(defclass/std ir-f2i (ir-unop)
  ())

(defclass/std ir-f2d (ir-unop)
  ())

(defclass/std ir-l2i (ir-unop)
  ())

(defclass/std ir-d2l (ir-unop)
  ())

(defclass/std ir-i2b (ir-unop)
  ())

(defclass/std ir-i2c (ir-unop)
  ())

(defclass/std ir-i2s (ir-unop)
  ())

(defclass/std ir-i2f (ir-unop)
  ())

(defclass/std ir-i2l (ir-unop)
  ())

(defclass/std ir-iinc (ir-node)
  ((index const)))

(defclass/std ir-if-xcmp<cond> (ir-branch)
  ((value1)
   (value2)))

(defclass/std ir-if-acmpeq (ir-if-xcmp<cond>)
  ())

(defclass/std ir-if-acmpne (ir-if-xcmp<cond>)
  ())

(defclass/std ir-if-icmpge (ir-if-xcmp<cond>)
  ())

(defclass/std ir-if-icmpeq (ir-if-xcmp<cond>)
  ())

(defclass/std ir-if-icmple (ir-if-xcmp<cond>)
  ())

(defclass/std ir-if-icmplt (ir-if-xcmp<cond>)
  ())

(defclass/std ir-if-icmpgt (ir-if-xcmp<cond>)
  ())

(defclass/std ir-if-icmpne (ir-if-xcmp<cond>)
  ())

(defclass/std ir-if<cond> (ir-branch)
  ((value)))

(defclass/std ir-ifeq (ir-if<cond>)
  ())

(defclass/std ir-ifge (ir-if<cond>)
  ())

(defclass/std ir-ifle (ir-if<cond>)
  ())

(defclass/std ir-iflt (ir-if<cond>)
  ())

(defclass/std ir-ifgt (ir-if<cond>)
  ())

(defclass/std ir-ifne (ir-if<cond>)
  ())

(defclass/std ir-ifnonnull (ir-if<cond>)
  ())

(defclass/std ir-ifnull (ir-if<cond>)
  ())

(defclass/std ir-instanceof (ir-node)
  ((class :with)
   (objref)))

(defclass/std ir-ishl (ir-binop)
  ())

(defclass/std ir-ishr (ir-binop)
  ())

(defclass/std ir-iushr (ir-binop)
  ())

(defclass/std ir-lshl (ir-binop)
  ())

(defclass/std ir-lshr (ir-binop)
  ())

(defclass/std ir-call (ir-node)
  ((return-type)))

(defclass/std ir-branch-target (ir-node)
  ((index)))

(defclass/std ir-call-special-method (ir-call)
  ((class :with)
   (method-name)
   (args)))

(defclass/std ir-call-virtual-method (ir-call)
  ((method-name)
   (args)))

(defclass/std ir-call-static-method (ir-call-virtual-method)
  ((class :with)))
(define-print-object/std ir-call-static-method)

(defclass/std ir-checkcast (ir-node)
  ((class :with)
   (objref)))

(defclass/std ir-clinit (ir-call)
  ((class :with)))

(defclass/std ir-member (ir-node)
  ((objref)
   (member-name)))

(defmethod initialize-instance :after ((insn ir-member) &key)
  (with-slots (objref member-name) insn
    (assert objref)
    (assert member-name)))

(defclass/std ir-monitorenter (ir-node)
  ((objref)))

(defclass/std ir-monitorexit (ir-node)
  ((objref)))

(defclass/std ir-static-member (ir-node)
  ((class :with)
   (member-name)))

(defclass/std ir-lstore (ir-node)
  ((target)))

(defclass/std ir-if (ir-node)
  ((condition :with)
	 (branch-if-true)
	 (branch-if-false)))

(defclass/std ir-new (ir-node)
  ((class :with)))

(defclass/std ir-new-array (ir-new)
  ((size)))

(defclass/std ir-lcmp (ir-binop)
  ())

(defclass/std ir-ldc2_w (ir-node)
  ())

(defclass/std ir-lushr (ir-node)
  ())

(defclass/std ir-fadd (ir-binop)
  ())

(defclass/std ir-fsub (ir-binop)
  ())

(defclass/std ir-dadd (ir-binop)
  ())

(defclass/std ir-dsub (ir-binop)
  ())

(defclass/std ir-fdiv (ir-binop)
  ())

(defclass/std ir-dmul (ir-binop)
  ())

(defclass/std ir-fmul (ir-binop)
  ())

(defclass/std ir-pop (ir-node)
  ())

(defclass/std ir-push (ir-node)
  ((value)))

;; FIXME: delete this
(defmethod initialize-instance ((ir ir-push) &key)
  (error "IR-PUSH"))

(defmethod uses-stack-p ((node ir-node))
  t)

(defclass/std ir-return (ir-node)
  ())

(defclass/std ir-return-value (ir-return)
  ((fn-name)
   (value)))

(defclass/std ir-isub (ir-binop)
  ())

(defclass/std ir-lsub (ir-binop)
  ())

(defclass/std ir-throw (ir-branch)
  ((objref)))

(defclass/std ir-variable (ir-node)
  ((name)))

(defclass/std ir-condition-exception (ir-node)
  ())

(defclass/std ir-tableswitch (ir-node)
  ((index)
   (default-offset)
   (low)
   (high)
   (jump-offsets)))

(defclass/std ir-lookupswitch (ir-node)
  ((index)
   (default-offset)
   (match-offset-pairs)))
