;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: OPENLDK; Base: 10 -*-
;;;
;;; Copyright (C) 2024, 2025  Anthony Green <green@moxielogic.com>
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

(defclass/std ir-node ()
  ((address :std -1
            :doc "Bytecode program counter this node was transpiled from.")
   (dead-p :std nil
           :doc "T if this instruction has been eliminated by DCE."))
  (:documentation "Base class for all OpenLDK IR nodes.  Java bytecode is
transpiled into a tree of these (bc-to-ir.lisp), optimized, and then
compiled to Lisp forms by the CODEGEN methods in codegen.lisp.  Most
subclasses correspond one-to-one with a JVM opcode and are named after
it (e.g. IR-IADD for iadd)."))

(defmethod side-effect-p ((node ir-node))
  "Whether evaluating NODE can have side effects.  Nodes default to T;
pure expression nodes override this so DCE can eliminate them."
  (declare (ignore node))
  t)

(defmethod print-object ((node ir-node) out)
  (print-unreadable-object (node out :type t)
    (format out "~A" (slot-value node 'address))))

(defmethod initialize-instance :after ((ir ir-node) &key)
  (assert (or (typep ir '<stack-bottom-marker>)
              (not (eq (slot-value ir 'address) +stack-bottom-address+)))))

(defmethod dot-dump-string ((node ir-node))
  (format nil "~3A: ~A" (address node) (class-name (class-of node))))

(defclass/std ir-xastore (ir-node)
  ((arrayref)
   (index)
   (value))
  (:documentation "Base class for the *astore array-store opcodes."))

(defmethod print-object ((node ir-xastore) out)
  (print-unreadable-object (node out :type t)
    (format out "~A[~A] = ~A" (slot-value node 'arrayref) (slot-value node 'index) (slot-value node 'value))))

(defclass/std ir-aastore (ir-xastore)
  ())

(defclass/std ir-aload (ir-node)
  ((index)))

(defclass/std ir-nop (ir-node)
  ())

(defclass/std ir-stop-marker (ir-node)
  ())

(defclass/std ir-literal (ir-node)
  ((value)
   (type :with))
  (:documentation "A constant value pushed by ldc/const/push opcodes."))

(defmethod side-effect-p ((node ir-literal))
  (declare (ignore node))
  nil)

(defmethod emit ((v ir-literal) cp)
  (slot-value v 'value))

(defclass/std ir-local-variable (ir-node)
  ((index)
   (jtype)))

(defclass/std ir-long-local-variable (ir-node)
  ((index)))

(defclass/std ir-null-literal (ir-literal)
  ())

(defclass/std ir-object-literal (ir-literal)
  ())

(defclass/std ir-method-handle (ir-literal)
  ((reference-index)))

(defclass/std ir-string-literal (ir-literal)
  ())

(defclass/std ir-double-literal (ir-literal)
  ())

(defclass/std ir-array-literal (ir-literal)
  ((component-class)))

(defclass/std ir-iastore (ir-xastore)
  ())

(defclass/std ir-lastore (ir-xastore)
  ())

(defclass/std ir-fastore (ir-xastore)
  ())

(defclass/std ir-sastore (ir-xastore)
  ())

(defclass/std ir-bastore (ir-xastore)
  ())

(defclass/std ir-dastore (ir-xastore)
  ())

(defclass/std ir-fcmpg (ir-binop)
  ())

(defclass/std ir-fcmpl (ir-binop)
  ())

(defclass/std ir-dcmpg (ir-binop)
  ())

(defclass/std ir-dcmpl (ir-binop)
  ())

(defclass/std ir-float-literal (ir-literal)
  ())

(defclass/std ir-int-literal (ir-literal)
  ())

(defclass/std ir-long-literal (ir-literal)
  ())

(defclass/std ir-array-index (ir-node)
  ((index)
   (arrayref))
  (:documentation "Base class for the *aload array-load opcodes."))

(defclass/std ir-caload (ir-array-index)
  ())

(defclass/std ir-aaload (ir-array-index)
  ())

(defclass/std ir-iaload (ir-array-index)
  ())

(defclass/std ir-saload (ir-array-index)
  ())

(defclass/std ir-laload (ir-array-index)
  ())

(defclass/std ir-baload (ir-array-index)
  ())

(defclass/std ir-daload (ir-array-index)
  ())

(defclass/std ir-faload (ir-array-index)
  ())

(defclass/std ir-castore (ir-xastore)
  ())

(defclass/std ir-class (ir-node)
  ((class :with)))
(define-print-object/std ir-class)

(defmethod side-effect-p ((node ir-class))
  (declare (ignore node))
  nil)

(defclass/std ir-array-length (ir-node)
  ((arrayref)))

(defclass/std ir-assign (ir-node)
  ((lvalue rvalue)))

(defmethod print-object ((node ir-assign) out)
  (print-unreadable-object (node out :type t)
    (format out "~A: ~A = ~A" (slot-value node 'address) (slot-value node 'lvalue) (slot-value node 'rvalue))))

(defclass/std ir-binop (ir-node)
  ((value1)
   (value2))
  (:documentation "Base class for two-operand opcodes (arithmetic, logical,
shift, and compare).  VALUE1/VALUE2 follow JVM operand-stack order."))

(defmethod side-effect-p ((node ir-binop))
  (declare (ignore node))
  nil)

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

(defclass/std ir-land (ir-binop)
  ())

(defclass/std ir-ior (ir-binop)
  ())

(defclass/std ir-lor (ir-binop)
  ())

(defclass/std ir-branch (ir-node)
  ((offset successors))
  (:documentation "Base class for control-transfer opcodes."))

(defclass/std ir-imul (ir-binop)
  ())

(defclass/std ir-idiv (ir-binop)
  ())

(defclass/std ir-lmul (ir-binop)
  ())

(defclass/std ir-ldiv (ir-binop)
  ())

(defclass/std ir-drem (ir-binop)
  ())

(defclass/std ir-frem (ir-binop)
  ())

(defclass/std ir-irem (ir-binop)
  ())

(defclass/std ir-lrem (ir-binop)
  ())

(defclass/std ir-dup2 (ir-node)
  ())

(defclass/std ir-goto (ir-branch)
  ())

(defclass/std ir-unop (ir-node)
  ((value))
  (:documentation "Base class for one-operand opcodes (negation and the
primitive x2y conversions)."))

(defmethod side-effect-p ((node ir-unop))
  (declare (ignore node))
  nil)

(defclass/std ir-l2f (ir-unop)
  ())

(defclass/std ir-l2d (ir-unop)
  ())

(defclass/std ir-f2i (ir-unop)
  ())

(defclass/std ir-f2l (ir-unop)
  ())

(defclass/std ir-f2d (ir-unop)
  ())

(defclass/std ir-l2i (ir-unop)
  ())

(defclass/std ir-d2i (ir-unop)
  ())

(defclass/std ir-d2l (ir-unop)
  ())

(defclass/std ir-d2f (ir-unop)
  ())

(defclass/std ir-i2b (ir-unop)
  ())

(defclass/std ir-i2c (ir-unop)
  ())

(defclass/std ir-i2s (ir-unop)
  ())

(defclass/std ir-i2d (ir-unop)
  ())

(defclass/std ir-i2f (ir-unop)
  ())

(defclass/std ir-i2l (ir-unop)
  ())

(defclass/std ir-lneg (ir-unop)
  ())

(defclass/std ir-ineg (ir-unop)
  ())

(defclass/std ir-dneg (ir-unop)
  ())

(defclass/std ir-fneg (ir-unop)
  ())

(defclass/std ir-iinc (ir-node)
  ((index const)))

(defclass/std ir-if-xcmp<cond> (ir-branch)
  ((value1)
   (value2))
  (:documentation "Base class for the two-operand if_icmp*/if_acmp* branches."))

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
  ((value))
  (:documentation "Base class for the one-operand if* branches (compare
against zero or null)."))

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

(defclass/std ir-call-dynamic-method (ir-call)
  ((method-name)
   (method-type)
   (args)
   (dynamic-args)
   (bootstrap-method-name)
   (interface-type-name)
   (call-site-descriptor :std nil)))

(defclass/std ir-call-static-method (ir-call-virtual-method)
  ((class :with)))
(define-print-object/std ir-call-static-method)

(defclass/std ir-call-dynamic (ir-node)
  ((class :with)))

(defclass/std ir-checkcast (ir-node)
  ((classname)
   (objref)))

(defclass/std ir-clinit (ir-call)
  ((class :with)))

(defclass/std ir-member (ir-node)
  ((objref)
   (member-name)
   (ref-class :std nil
              :doc "Binary name of the class the Fieldref names -- needed to
resolve shadowed fields to their declaring class's slot.")))

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

(defclass/std ir-if (ir-node)
  ((condition :with)
   (branch-if-true)
   (branch-if-false)))

(defclass/std ir-new (ir-node)
  ((class :with)))

(defclass/std ir-new-array (ir-new)
  ((component-class)
   (size)
   (atype)))

(defclass/std ir-multi-new-array (ir-new)
  ((component-class)
   (dimensions)
   (sizes)
   (atype)))

(defclass/std ir-lcmp (ir-binop)
  ())

(defclass/std ir-ldc2_w (ir-node)
  ())

(defclass/std ir-lushr (ir-binop)
  ())

(defclass/std ir-fadd (ir-binop)
  ())

(defclass/std ir-fsub (ir-binop)
  ())

(defclass/std ir-dsub (ir-binop)
  ())

(defclass/std ir-dadd (ir-binop)
  ())

(defclass/std ir-ddiv (ir-binop)
  ())

(defclass/std ir-fdiv (ir-binop)
  ())

(defclass/std ir-dmul (ir-binop)
  ())

(defclass/std ir-fmul (ir-binop)
  ())

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
