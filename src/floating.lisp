;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: OPENLDK; Base: 10 -*-
;;;
;;; Copyright (C) 2026  Anthony Green <green@moxielogic.com>
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

;;; CL arithmetic permits compiler identities that do not preserve Java's
;;; IEEE 754 values. For example, SBCL derives (- X X) as zero even for NaN.
;;; These operations have opaque result types and native instruction lowering.
;;; On other SBCL backends the out-of-line functions keep operands independent
;;; during optimization, and use the Java thread's existing masked FP traps.
(defmacro %define-java-floating-op (name type operator instruction)
  `(progn
     (sb-c:defknown ,name (,type ,type) ,type (sb-c:movable sb-c:flushable))
     (defun ,name (left right)
       (declare (type ,type left right) (optimize (speed 3) (safety 3)))
       (,operator left right))
     #+x86-64
     (sb-c:define-vop (,name)
       (:translate ,name)
       (:policy :fast-safe)
       (:args (left :scs (,(if (eq type 'single-float) 'sb-vm::single-reg 'sb-vm::double-reg)) :target result)
              (right :scs (,(if (eq type 'single-float) 'sb-vm::single-reg 'sb-vm::double-reg))))
       (:arg-types ,type ,type)
       (:results (result :scs (,(if (eq type 'single-float) 'sb-vm::single-reg 'sb-vm::double-reg))))
       (:result-types ,type)
       (:temporary (:sc ,(if (eq type 'single-float) 'sb-vm::single-reg 'sb-vm::double-reg)) temp)
       (:generator 2
         (cond ((sb-c:location= result left)
                (sb-assem:inst ,instruction result right))
               ((sb-c:location= result right)
                (sb-vm::move temp left)
                (sb-assem:inst ,instruction temp right)
                (sb-vm::move result temp))
               (t
                (sb-vm::move result left)
                (sb-assem:inst ,instruction result right)))))))

(%define-java-floating-op %java-fadd single-float +
  #+x86-64 sb-x86-64-asm::addss #-x86-64 nil)
(%define-java-floating-op %java-fsub single-float -
  #+x86-64 sb-x86-64-asm::subss #-x86-64 nil)
(%define-java-floating-op %java-fmul single-float *
  #+x86-64 sb-x86-64-asm::mulss #-x86-64 nil)
(%define-java-floating-op %java-fdiv single-float /
  #+x86-64 sb-x86-64-asm::divss #-x86-64 nil)
(%define-java-floating-op %java-dadd double-float +
  #+x86-64 sb-x86-64-asm::addsd #-x86-64 nil)
(%define-java-floating-op %java-dsub double-float -
  #+x86-64 sb-x86-64-asm::subsd #-x86-64 nil)
(%define-java-floating-op %java-dmul double-float *
  #+x86-64 sb-x86-64-asm::mulsd #-x86-64 nil)
(%define-java-floating-op %java-ddiv double-float /
  #+x86-64 sb-x86-64-asm::divsd #-x86-64 nil)
