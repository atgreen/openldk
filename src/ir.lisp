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

(defmethod dot-dump-string ((node ir-node))
  (format nil "~3A: ~A" (address node) (class-name (class-of node))))

(defmethod uses-stack-p ((node ir-node))
  nil)

(defclass/std ir-aaload (ir-node)
  ())

(defclass/std ir-aastore (ir-node)
  ())

(defclass/std ir-aload (ir-node)
  ((index)))

(defclass/std ir-nop (ir-node)
  ())

(defclass/std ir-literal (ir-node)
  ((value)))

(defmethod emit ((v ir-literal) cp)
  (slot-value v 'value))

(defclass/std ir-local-variable (ir-node)
  ((index)))

(defclass/std ir-null-literal (ir-literal)
  ())

(defclass/std ir-string-literal (ir-literal)
  ())

(defclass/std ir-double-literal (ir-literal)
  ())

(defclass/std ir-iastore (ir-node)
  ())

(defclass/std ir-fcmpg (ir-node)
  ())

(defclass/std ir-fcmpl (ir-node)
  ())

(defclass/std ir-float-literal (ir-literal)
  ())

(defclass/std ir-double-literal (ir-literal)
  ())

(defclass/std ir-int-literal (ir-literal)
  ())

(defclass/std ir-long-literal (ir-literal)
  ())

(defclass/std ir-caload (ir-node)
  ())

(defclass/std ir-iaload (ir-node)
  ())

(defclass/std ir-castore (ir-node)
  ())

(defclass/std ir-class (ir-node)
  ((class :with)))
(define-print-object/std ir-class)

(defclass/std ir-array-length (ir-node)
  ())

(defclass/std ir-assign (ir-node)
  ((target source)))

(defclass/std ir-add (ir-node)
  ())

(defclass/std ir-iand (ir-node)
  ())

(defclass/std ir-ixor (ir-node)
  ())

(defclass/std ir-ineg (ir-node)
  ())

(defclass/std ir-land (ir-node)
  ())

(defclass/std ir-ior (ir-node)
  ())

(defclass/std ir-lor (ir-node)
  ())

(defmethod uses-stack-p (ir-add)
  t)

(defclass/std ir-branch (ir-node)
  ((offset successors)))

(defclass/std ir-imul (ir-node)
  ())

(defclass/std ir-idiv (ir-node)
  ())

(defclass/std ir-ldiv (ir-node)
  ())

(defclass/std ir-irem (ir-node)
  ())

(defclass/std ir-dup (ir-node)
  ())

(defclass/std ir-dup2 (ir-node)
  ())

(defclass/std ir-dup-x1 (ir-node)
  ())

(defclass/std ir-goto (ir-branch)
  ())

(defclass/std ir-l2f (ir-node)
  ())

(defclass/std ir-f2i (ir-node)
  ())

(defclass/std ir-d2l (ir-node)
  ())

(defclass/std ir-i2c (ir-node)
  ())

(defclass/std ir-i2f (ir-node)
  ())

(defclass/std ir-iinc (ir-node)
  ((index const)))

(defclass/std ir-if-acmpeq (ir-branch)
  ())

(defclass/std ir-if-acmpne (ir-branch)
  ())

(defclass/std ir-if-icmpge (ir-branch)
  ())

(defclass/std ir-if-icmpeq (ir-branch)
  ())

(defclass/std ir-if-icmple (ir-branch)
  ())

(defclass/std ir-if-icmplt (ir-branch)
  ())

(defclass/std ir-if-icmpgt (ir-branch)
  ())

(defclass/std ir-if-icmpne (ir-branch)
  ())

(defclass/std ir-ifeq (ir-branch)
  ())

(defclass/std ir-ifge (ir-branch)
  ())

(defclass/std ir-ifle (ir-branch)
  ())

(defclass/std ir-iflt (ir-branch)
  ())

(defclass/std ir-ifgt (ir-branch)
  ())

(defclass/std ir-ifne (ir-branch)
  ())

(defclass/std ir-ifnonnull (ir-branch)
  ())

(defclass/std ir-ifnull (ir-branch)
  ())

(defclass/std ir-instanceof (ir-node)
  ((class :with)))

(defclass/std ir-ishl (ir-node)
  ())

(defclass/std ir-ishr (ir-node)
  ())

(defclass/std ir-iushr (ir-node)
  ())

(defclass/std ir-lshl (ir-node)
  ())

(defclass/std ir-lshr (ir-node)
  ())

(defclass/std ir-call (ir-node)
  ((void-return-p)))

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
  ((class :with)))

(defclass/std ir-clinit (ir-call)
  ((class :with)))

(defclass/std ir-member (ir-node)
  ((member-name)))

(defclass/std ir-monitorenter (ir-node)
  (()))

(defclass/std ir-monitorexit (ir-node)
  (()))

(defclass/std ir-static-member (ir-node)
  ((class :with)
   (member-name)))

(defclass/std ir-store (ir-node)
  ((target)))

(defclass/std ir-lstore (ir-node)
  ((target)))

(defclass/std ir-if (ir-node)
  ((condition :with)
	 (branch-if-true)
	 (branch-if-false)))

(defclass/std ir-new (ir-node)
  ((class :with)))

(defclass/std ir-new-array (ir-new)
  ())

(defclass/std ir-lcmp (ir-node)
  ())

(defclass/std ir-ldc2_w (ir-node)
  ())

(defclass/std ir-lushr (ir-node)
  ())

(defclass/std ir-fadd (ir-node)
  ())

(defclass/std ir-fdiv (ir-node)
  ())

(defclass/std ir-fmul (ir-node)
  ())

(defclass/std ir-mul (ir-node)
  ())

(defclass/std ir-pop (ir-node)
  ())

(defclass/std ir-push (ir-node)
  ((value)))

(defmethod uses-stack-p ((node ir-node))
  t)

(defclass/std ir-return (ir-node)
  ())

(defclass/std ir-return-value (ir-return)
  ((fn-name)))

(defclass/std ir-sub (ir-node)
  ())

(defclass/std ir-throw (ir-branch)
  ())

(defclass/std ir-variable (ir-node)
  ((name)))
