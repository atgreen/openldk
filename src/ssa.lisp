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

(defclass/std ssa-node ()
  ((address :std -1)))

(defmethod print-object ((node ssa-node) out)
  (print-unreadable-object (node out :type t)
    (format out "~A" (slot-value node 'address))))

(defmethod dot-dump-string ((node ssa-node))
  (format nil "~3A: ~A" (address node) (class-name (class-of node))))

(defmethod uses-stack-p ((node ssa-node))
  nil)

(defclass/std ssa-aaload (ssa-node)
  ())

(defclass/std ssa-aastore (ssa-node)
  ())

(defclass/std ssa-aload (ssa-node)
  ((index)))

(defclass/std ssa-nop (ssa-node)
  ())

(defclass/std ssa-literal (ssa-node)
  ((value)))

(defmethod emit ((v ssa-literal) cp)
  (slot-value v 'value))

(defclass/std ssa-local-variable (ssa-node)
  ((index)))

(defclass/std ssa-null-literal (ssa-literal)
  ())

(defclass/std ssa-string-literal (ssa-literal)
  ())

(defclass/std ssa-double-literal (ssa-literal)
  ())

(defclass/std ssa-iastore (ssa-node)
  ())

(defclass/std ssa-int-literal (ssa-literal)
  ())

(defclass/std ssa-long-literal (ssa-literal)
  ())

(defclass/std ssa-class (ssa-node)
  ((class :with)))

(defclass/std ssa-array-length (ssa-node)
  ())

(defclass/std ssa-assign (ssa-node)
  ((target source)))

(defclass/std ssa-add (ssa-node)
  ())

(defmethod uses-stack-p (ssa-add)
  t)

(defclass/std ssa-branch (ssa-node)
  ((offset successors)))

(defclass/std ssa-div (ssa-node)
  ())

(defclass/std ssa-dup (ssa-node)
  ())

(defclass/std ssa-goto (ssa-branch)
  ())

(defclass/std ssa-iinc (ssa-node)
  ((index const)))

(defclass/std ssa-if-acmpeq (ssa-branch)
  ())

(defclass/std ssa-if-acmpne (ssa-branch)
  ())

(defclass/std ssa-if-icmpge (ssa-branch)
  ())

(defclass/std ssa-if-icmpeq (ssa-branch)
  ())

(defclass/std ssa-if-icmple (ssa-branch)
  ())

(defclass/std ssa-if-icmpne (ssa-branch)
  ())

(defclass/std ssa-ifeq (ssa-branch)
  ())

(defclass/std ssa-ifge (ssa-branch)
  ())

(defclass/std ssa-ifle (ssa-branch)
  ())

(defclass/std ssa-iflt (ssa-branch)
  ())

(defclass/std ssa-ifne (ssa-branch)
  ())

(defclass/std ssa-ifnonnull (ssa-branch)
  ())

(defclass/std ssa-ifnull (ssa-branch)
  ())

(defclass/std ssa-instanceof (ssa-node)
  ((class :with)))

(defclass/std ssa-ishl (ssa-node)
  ())

(defclass/std ssa-call (ssa-node)
  ())

(defclass/std ssa-branch-target (ssa-node)
  ((index)))

(defclass/std ssa-call-special-method (ssa-call)
  ((class :with)
   (method-name)
   (args)))

(defclass/std ssa-call-virtual-method (ssa-call)
  ((method-name)
   (args)))

(defclass/std ssa-call-static-method (ssa-call-virtual-method)
  ((class :with)))
(define-print-object/std ssa-call-static-method)

(defclass/std ssa-checkcast (ssa-node)
  ((index)))

(defclass/std ssa-clinit (ssa-call)
  ((class :with)))

(defclass/std ssa-member (ssa-node)
  ((member-name)))

(defclass/std ssa-monitorenter (ssa-node)
  (()))

(defclass/std ssa-monitorexit (ssa-node)
  (()))

(defclass/std ssa-static-member (ssa-node)
  ((class :with)
   (member-name)))

(defclass/std ssa-store (ssa-node)
  ((target)))

(defclass/std ssa-if (ssa-node)
  ((condition :with)
	 (branch-if-true)
	 (branch-if-false)))

(defclass/std ssa-new (ssa-node)
  ((class :with)))

(defclass/std ssa-new-array (ssa-new)
  ())

(defclass/std ssa-lcmp (ssa-node)
  ())

(defclass/std ssa-ldc2_w (ssa-node)
  ())

(defclass/std ssa-lushr (ssa-node)
  ())

(defclass/std ssa-mul (ssa-node)
  ())

(defclass/std ssa-pop (ssa-node)
  ())

(defclass/std ssa-push (ssa-node)
  ((value)))

(defmethod uses-stack-p ((node ssa-node))
  t)

(defclass/std ssa-return (ssa-node)
  ())

(defclass/std ssa-return-value (ssa-return)
  ((fn-name)))

(defclass/std ssa-sub (ssa-node)
  ())

(defclass/std ssa-throw (ssa-branch)
  ())

(defclass/std ssa-variable (ssa-node)
  ((name)))
