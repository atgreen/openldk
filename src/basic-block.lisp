;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: OPENLDK; Base: 10 -*-
;;;
;;; Copyright (C) 2023, 2024  Anthony Green <green@moxielogic.com>
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
	 (incoming)
	 (exits)
	 (stop)
	 (code-emitted-p)
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
    (dolist (successor (exits bloc))
      (when successor
        (dump-dot successor done-table stream)
        (format stream "~A -> ~A~%" (id bloc) (id successor))))))

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

(defun find-target-instructions ()
	"Return three HASH-TABLE objects.  The first maps a bytecode index key
with lists of successor bytecode indices. The second includes entries
for all of the program indices in the current code that are branch or
exception targets.  The third maps a bytecode index key to a successor
address."
  (let* ((code (bytecode *context*))
         (exception-table (exception-table *context*))
         (pc 0)
         (length (length code))
         (branch-target-table (make-hash-table))
         (try-block-table (make-hash-table))
         ;; key  : instruction address
         ;; value: a list of successor addresses, the first of which
         ;;        is the "fall-through" case, should one exist.
         (successor-address-table (make-hash-table)))

    ;; First, let's go through the instructions looking for branch
    ;; targets.  Record them in BRANCH-TARGET-TABLE.
    (dolist (bt (remove-duplicates
                 (apply
                  #'append
                  (loop
                    while (< pc length)
                    for result = (let* ((opcode (aref +opcodes+ (aref code pc)))
                                        (targets (if (gethash opcode +bytecode-short-branch-table+)
                                                     (get-short-branch-targets pc code))))
                                   (when targets
																		 ;; Record targets in SUCCESSOR-ADDRESS-TABLE.
                                     (setf (gethash pc successor-address-table) targets))
                                   (incf pc (gethash opcode +bytecode-lengths-table+))
                                   targets)
                    unless (null result)
                      collect result))))
      (setf (gethash bt branch-target-table) t))

    ;; Now let's go through the exception table, looking for exception
    ;; (catch) targets.  Along the way, let's look for in-method
    ;; "branches" caused by exceptions.
    (when exception-table
      (loop for i from 0 upto (1- (length exception-table))
            do (let ((ete (aref exception-table i)))
                 (with-slots (start-pc end-pc handler-pc catch-type) ete
                   (push ete (gethash start-pc try-block-table))
                   ;; (format t "find ETE~%")
                   ;; (maphash (lambda (k v) (format t "~A: ~A~%" k v)) try-block-table)
                   (loop
                     with pc = start-pc
                     until (eq pc end-pc)
                     do (let* ((opcode (aref +opcodes+ (aref code pc)))
                               (insn-length (gethash opcode +bytecode-lengths-table+)))
                          (if (opcode-throws-p opcode catch-type)
                              (progn
                                (setf (gethash pc successor-address-table)
                                      (list (+ pc insn-length) handler-pc))
                                (setf (gethash (incf pc insn-length) branch-target-table) t)
                                (setf (gethash pc branch-target-table) t))
                              (incf pc insn-length))))
                   (setf (gethash handler-pc branch-target-table) t)))))

    (values branch-target-table try-block-table successor-address-table)))

(defun build-basic-blocks (ssa-code)
  "Build <BASIC-BLOCK> objects from SSA-CODE. Return the entry block."
  (dump "build-basic-blocks" ssa-code)
  (multiple-value-bind (branch-targets try-block-table successor-address-table)
      (find-target-instructions)
		(let* ((block-by-address (make-hash-table))
					 (blocks (loop for block = (make-instance '<basic-block>)
												 while ssa-code
												 do (setf (gethash (address (car ssa-code)) block-by-address) block)
												 do (push (car ssa-code) (code block))
												 do (setf ssa-code (cdr ssa-code))
												 collect (progn
																	 (loop for insn = (car ssa-code)
																				 while insn
																				 for address = (address insn)
																				 until (gethash address branch-targets)
																				 do (push insn (code block))
																				 do (setf ssa-code (cdr ssa-code)))
																	 block))))
			;; Reverse all of the code back into normal order
			(dolist (block blocks)
				(dolist (target (gethash (address (car (code block))) successor-address-table))
					(push (gethash target block-by-address) (exits block)))
				(setf (code block) (nreverse (code block))))
			(dump-method-dot blocks)
			blocks)))
