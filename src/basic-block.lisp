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

(defvar *block-counter* 0)

(defun generate-id ()
  (incf *block-counter*))

(defclass/std <basic-block> ()
  ((id :std (generate-id))
	 (code)
	 (address)
	 (predecessors
		:std (fset:empty-set)
		:doc "Set of incoming blocks")
	 (successors
		:std (fset:empty-set)
		:doc "Set of outgoing blocks")
	 (stop)
	 (code-emitted-p)
	 (try-catch)
	 (exception-end-blocks)
	 (exception-table-entries)
	 (catch-handlers)))

(defmethod dump-dot (b done-table stream)
  (format stream "~A [label=\"wth? ~A\"];~%" b b))

(defmethod dump-dot ((bloc <basic-block>) done-table stream)
  (unless (gethash (id bloc) done-table)
    (setf (gethash (id bloc) done-table) t)
    (format stream "~A [label=" (id bloc))
    (format stream "<<TABLE BORDER=\"0\" CELLBORDER=\"0\" CELLSPACING=\"0\">")
    (dolist (i (code bloc))
      (format stream "<TR><TD ALIGN=\"LEFT\">\"~A\"</TD></TR>~%" (dot-dump-string i)))
    (format stream "</TABLE>>];~%")
    (fset:do-set (successor (successors bloc))
      (when successor
        (dump-dot successor done-table stream)
        (format stream "~A -> ~A~%" (id bloc) (id successor))))
		(loop for tc in (try-catch bloc)
					do (format stream "~A -> ~A [label=~S]~%" (id bloc) (id (cdr tc)) (format nil "~A" (car tc))))))

(defvar *instruction-exceptions* (make-hash-table))

(defmacro define-instruction-exceptions (opcode exceptions)
  `(setf (gethash ,opcode *instruction-exceptions*) ,exceptions))

(define-instruction-exceptions :IDIV
    '("java/lang/ArithmeticException" "java/lang/Exception" "java/lang/Throwable"))

(defun opcode-throws-p (opcode throwable)
  (let ((throwables (gethash opcode *instruction-exceptions*)))
    (find throwable throwables :test #'string=)))

(defun get-short-branch-targets (pc code)
	"The opcode at PC in CODE is a branch instruction.  Return a list of
addresses for possible next instructions.  The length of the list will
be 1 in the case of unconditional branches (GOTO), and 2 otherwise."
  (let ((start_pc pc)
        (offset (unsigned-to-signed (+ (* (aref code (incf pc)) 256)
                                       (aref code (incf pc))))))
    (if (gethash (aref +opcodes+ (aref code start_pc)) +bytecode-conditional-branch-table+)
        (list (+ start_pc offset) (1+ pc))
        (list (+ start_pc offset)))))

(defun find-block-starts ()
	"Return a hashtable keyed on instruction address where the value is
 T if the instruction at the address is the start of a block.  It is
 the start of a block if it is:
    - the start of a method
    - the start of an exception range
    - the start of an exception handler
    - a jump or branch target"
	(let ((block-starts (make-hash-table)))

		;; The start of the method...
		(setf (gethash 0 block-starts) t)

		;; Handle all of the exception table entries
		(let ((exception-table (exception-table *context*)))
			(when exception-table
				(loop for i from 0 below (length exception-table)
							for ete = (aref exception-table i)
							do (setf (gethash (start-pc ete) block-starts) t)
							do (setf (gethash (handler-pc ete) block-starts) t))))

		;; Handle all jump and branch targets
		(let ((code (bytecode *context*))
					(pc 0))
			(loop
				while (< pc (length code))
				do (let* ((opcode (aref +opcodes+ (aref code pc)))
									(targets (if (gethash opcode +bytecode-short-branch-table+)
															 (get-short-branch-targets pc code))))
						 (incf pc (gethash opcode +bytecode-lengths-table+))
						 (dolist (target targets)
							 (setf (gethash target block-starts) t)))))

		block-starts))

(defun build-basic-blocks (ssa-code)
  "Build <BASIC-BLOCK> objects from SSA-CODE. Return the entry block."
  (dump "build-basic-blocks" ssa-code)
	(let ((block-starts (find-block-starts)))
		(let* ((block-by-address (make-hash-table))
					 (blocks (loop while ssa-code
												 for insn = (car ssa-code)
												 unless (gethash (address insn) block-starts)
													 do (setf ssa-code (cdr ssa-code))
												 when (gethash (address insn) block-starts)
													 collect (let ((block (make-instance '<basic-block>)))
																		 (loop while ssa-code
																					 for insn = (car ssa-code)
																					 for address = (address insn)
																					 do (push insn (code block))
																					 do (setf (gethash address block-by-address) block)
																					 do (setf ssa-code (cdr ssa-code))
																					 until (and (car ssa-code) (gethash (address (car ssa-code)) block-starts)))
																		 block))))
			(dolist (block blocks)
				;; Make connections between basic blocks.
				(let* ((opcode (aref +opcodes+ (aref (bytecode *context*) (floor (address (car (code block)))))))
							 (targets (if (gethash opcode +bytecode-short-branch-table+)
														(get-short-branch-targets (address (car (code block))) (bytecode *context*))
														(list (+ (address (car (code block))) (gethash opcode +bytecode-lengths-table+))))))
					(dolist (target targets)
						(let ((target-block (gethash target block-by-address)))
							(when target-block
								(fset:adjoinf (successors block) target-block)
								(fset:adjoinf (predecessors target-block) block)))))

				;; Reverse all of the code back into normal order.
				(setf (code block) (nreverse (code block))))

			;; Let's create try blocks
			(let ((exception-table (exception-table *context*)))
				(when exception-table
					(loop for i from 0 below (length exception-table)
								for ete = (aref exception-table i)
								for start-block = (gethash (start-pc ete) block-by-address)
								for end-block = (gethash (end-pc ete) block-by-address)
								for handler = (gethash (handler-pc ete) block-by-address)
								do (push end-block (exception-end-blocks start-block))
								do (push (cons (catch-type ete) handler) (try-catch start-block)))))

			(dump-method-dot blocks)
			blocks)))
