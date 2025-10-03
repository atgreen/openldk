;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: OPENLDK; Base: 10 -*-
;;;
;;; Copyright (C) 2023, 2024, 2025  Anthony Green <green@moxielogic.com>
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

(defclass/std <context> ()
  ((class :with)
   (is-clinit-p)
   (uses-stack-p :std t)
   (classes)
   (bytecode)
   (insn-size
    :doc "An array of numbers indicating the size of corresponding
 instruction in BYTECODE")
   (next-insn-list
    :doc "An array of lists of possible next instruction offsets for
 the corresponding instruction in BYTECODE.")
   (exception-table)
   (block-address-table
    :doc "A hashtable mapping addresses to blocks")
   (fn-name)
   (blocks)
   (ir-code)
   (svcount :std 0)
   (next-is-wide-p
    :doc "T if the next opcode is the wide variant.")
   (emitted-block-scopes :std (list (list)))
   (stack-state-table
    :doc "A hashtable mapping addresses to stack states")
   (try-end-table
    :std (make-hash-table)
    :doc "A tabled keyed on addresses, where if the value is T, then this is the end of a TRY region.")
   (stack-variables)
   (single-assignment-table
    :std (make-hash-table :test #'equal))
   (pc :std 0)
   (needs-array-bounds-check :std nil)
   (stack :std (list (make-instance '<stack-bottom-marker>)))))

(defconstant +stack-bottom-address+ -99)

(defmethod flag-stack-usage ((context <context>))
  (setf (slot-value context 'uses-stack-p) t))
