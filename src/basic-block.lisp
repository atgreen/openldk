;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: OPENLDK; Base: 10 -*-
;;; SPDX-License-Identifier: GPL-3.0-or-later WITH Classpath-exception-2.0
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
  "Generate a unique ID for a basic block by incrementing the global block counter."
  (incf *block-counter*))

(defclass/std <basic-block> ()
  ((id :std (generate-id))
   (code)
   (address)
   (predecessors
    :std (fset:empty-set)
    :doc "Set of incoming blocks.")
   (successors
    :std (fset:empty-set)
    :doc "Set of outgoing blocks.")
   (dominators
    :doc "A set of basic blocks that dominate this block.

A block `A` is said to dominate a block `B` if every path from the
entry block of the control flow graph (CFG) to `B` must pass through `A`.
This slot stores the set of all such blocks `A` that dominate the current block.

The dominance set is represented as an `fset:set` of <basic-block> objects.")
   (fall-through-address
    :doc "The address that follows naturally in the absence of a goto/branch/return/throw.")
   (stop)
   (code-emitted-p
    :doc "True if this block's code has already been emitted.")
   (try-catch
    :doc "A list of conses of handler type and handler block.")
   (local-substitutions
    :std nil
    :doc "Hash table of block-local variable substitutions (stack-var -> value).
Combined with global substitutions during codegen for this block only.")
   (finally
    :doc "A list of finally blocks for each try-finally starting here.")
   (end-of-handler?
    :doc "T is this is the last block of a try handler, NIL otherwise.")
   (marks)
   (try-exit-block
    :doc "The block at which try/catch handlers exit if this is a try block.")
   (exception-end-blocks)
   (exception-table-entries)
   (catch-handlers)))

(defmethod print-object ((bb <basic-block>) out)
  (print-unreadable-object (bb out :type t)
    (format out "id=~A [~A:~A]" (id bb) (address (first (code bb))) (address (first (last (code bb)))))))

(defmethod dump-dot (b done-table stream)
  (format stream "~A [label=\"wth? ~A\"];~%" b b))

(defmethod dump-dot ((bloc <basic-block>) done-table stream)
  (unless (gethash (id bloc) done-table)
    (setf (gethash (id bloc) done-table) t)
    (format stream "~A [label=" (id bloc))
    (format stream "<<TABLE BORDER=\"0\" CELLBORDER=\"0\" CELLSPACING=\"0\">")
    (format stream "<TR><TD ALIGN=\"LEFT\">\"~A DOMINATORS: ~{~A ~}\"</TD></TR>~%"
            (id bloc)
            (sort (mapcar #'id (fset:convert 'list (dominators bloc))) #'>))
    (dolist (i (code bloc))
      (format stream "<TR><TD ALIGN=\"LEFT\">\"~A\"</TD></TR>~%" (dot-dump-string i)))
    (format stream "</TABLE>>];~%")
    (fset:do-set (successor (successors bloc))
      (when successor
        (dump-dot successor done-table stream)
        (format stream "~A -> ~A~%" (id bloc) (id successor))))
    (loop for tc in (try-catch bloc)
          do (format stream "~A -> ~A [label=~S]~%" (id bloc) (id (rest tc)) (format nil "~A" (first tc))))))

(defvar *instruction-exceptions* (make-hash-table))

(defmacro define-instruction-exceptions (opcode exceptions)
  "Define which exceptions a particular opcode can throw."
  `(setf (gethash ,opcode *instruction-exceptions*) ,exceptions))

(define-instruction-exceptions :IDIV
    '("java/lang/ArithmeticException" "java/lang/Exception" "java/lang/Throwable"))

(defun opcode-throws-p (opcode throwable)
  "Check if OPCODE can throw the specified THROWABLE exception."
  (let ((throwables (gethash opcode *instruction-exceptions*)))
    (find throwable throwables :test #'string=)))

(defun merge-exception-entries (entries)
  "Merge exception table entries in an array where end-pc of one is start-pc-1 of another,
   and handler-pc and catch-type are the same. Return a new array with merged entries."
  (let* ((sorted-entries (sort (coerce entries 'list)
                               #'<
                               :key (lambda (entry) (start-pc entry))))
         (merged (loop
                  with result = nil
                  for entry in sorted-entries
                  unless (eq (start-pc entry) (handler-pc entry)) ;; Ignore self-serving ranges
                    do (if (and result
                                (= (1- (start-pc entry)) (slot-value (first result) 'end-pc))
                                (= (handler-pc entry) (handler-pc (first result)))
                                (equal (catch-type entry) (catch-type (first result))))
                           ;; Merge the current entry into the last merged one
                           (setf (end-pc (first result)) (end-pc entry))
                           ;; Otherwise, add the current entry as a new merged entry
                           (push entry result))
                  finally (return (reverse result)))))
    ;; Convert the result back to an array
    (make-array (length merged) :initial-contents merged)))

(defun find-block-starts ()
  "Return a hashtable keyed on instruction address where the value is
 T if the instruction at the address is the start of a block.  It is
 the start of a block if it is:
    - the start of a method
    - the start of an exception range
    - the instruction following an exception range
    - the start of an exception handler
    - a jump or branch target"
  (let ((block-starts (make-hash-table)))

    ;; The start of the method...
    (setf (gethash 0 block-starts) t)

    ;; Merge exception ranges where possible
    (setf (exception-table *context*) (merge-exception-entries (exception-table *context*)))

    ;; Handle all of the exception table entries
    (when-let ((exception-table (exception-table *context*)))
      (loop for i from 0 below (length exception-table)
            for ete = (aref exception-table i)
            do (setf (gethash (start-pc ete) block-starts) t)
            do (setf (gethash (end-pc ete) block-starts) t)
            do (setf (gethash (end-pc ete) (try-end-table *context*)) t)
            do (setf (gethash (handler-pc ete) block-starts) t)))

    ;; Handle all jump and branch targets
    (let ((code (bytecode *context*))
          (pc 0))
      (loop
        while (< pc (length code))
        do (let* ((opcode (aref *opcodes* (aref code pc)))
                  (targets (let ((next-insn-list (aref (next-insn-list *context*) pc)))
                             (if (or (eql opcode :GOTO) (> (length next-insn-list) 1))
                                 next-insn-list
                                 nil))))
             (incf pc (aref (insn-size *context*) pc))
             (dolist (target targets)
               (setf (gethash target block-starts) t)))))

    block-starts))

(defun propagate-try-blocks (blocks)
  "Propagate try blocks to associated catch handler blocks."
  (dolist (block blocks)
    (let ((try-catch (slot-value block 'try-catch)))
      (dolist (handler-pair try-catch)
        (let ((handler-block (rest handler-pair)))
          ;; Add the current block to the handler block's predecessors
          (setf (slot-value handler-block 'predecessors)
                (fset:union (slot-value handler-block 'predecessors)
                            (fset:set block))))))))


(defun compute-dominance (blocks entry-block)
  "Compute the dominance sets for each block in the CFG, considering try-catch handlers.
   BLOCKS is a list of all <basic-block> objects.
   ENTRY-BLOCK is the entry block of the CFG."

  ;; Update the PREDECESSORS set with catch handlers.
  (propagate-try-blocks blocks)

  ;; Initialize dominance sets.
  (let ((all-blocks-set (fset:convert 'fset:set blocks)))
    (dolist (block blocks)
      (setf (dominators block)
            (if (eq block entry-block)
                (fset:set entry-block)
                all-blocks-set))))

  ;; Iterate to refine the dominance sets.
  (let ((changed t))
    (loop while changed
          do (setf changed nil)
             (dolist (block blocks)
               (unless (eq block entry-block)
                 ;; Compute predecessors including try-catch handlers
                 (let ((predecessors (slot-value block 'predecessors)))
                   ;; Compute new dominance set
                   (unless (fset:empty? predecessors)
                     (let* ((new-dominance-set
                              (reduce #'fset:intersection
                                      (mapcar (lambda (pred)
                                                (slot-value pred 'dominators))
                                              (fset:convert 'list predecessors))))
                            (updated-dominance-set (fset:union new-dominance-set
                                                               (fset:set block)))) ;; Add the block itself
                       ;; Check if the dominance set has changed
                       (unless (fset:equal? updated-dominance-set (dominators block))
                         (setf (dominators block) updated-dominance-set)
                         (setf changed t))))))))))


(defun build-basic-blocks (ir-code)
  "Build <BASIC-BLOCK> objects from IR-CODE. Return the entry block."
  ;; (dump "build-basic-blocks" ir-code)
  (let ((block-starts (find-block-starts)))
    (let* ((block-by-address (make-hash-table))
           (blocks (loop while ir-code
                         for insn = (first ir-code)
                         unless (gethash (address insn) block-starts)
                           do (setf ir-code (rest ir-code))
                         when (gethash (address insn) block-starts)
                           collect (let ((block (make-instance '<basic-block> :address (address insn))))
                                     (loop while ir-code
                                           for insn = (first ir-code)
                                           for address = (address insn)
                                           do (push insn (code block))
                                           do (setf (gethash address block-by-address) block)
                                           do (setf ir-code (rest ir-code))
                                           until (if (and (first ir-code) (gethash (address (first ir-code)) block-starts))
                                                     (progn
                                                       (when (find (address (first ir-code)) (aref (next-insn-list *context*) (floor address)))
                                                         (setf (fall-through-address block) (address (first ir-code))))
                                                       t)
                                                     nil))
                                     block))))
      (dolist (block blocks)
        ;; Make connections between basic blocks.
        (let* ((opcode (aref *opcodes* (aref (bytecode *context*) (floor (address (first (code block)))))))
               (targets (aref (next-insn-list *context*) (floor (address (first (code block)))))))
          (setf (fall-through-address block) (gethash (fall-through-address block) block-by-address))
          (dolist (target targets)
            (when-let ((target-block (gethash (floor target) block-by-address)))
              (fset:adjoinf (successors block) target-block)
              (fset:adjoinf (predecessors target-block) block))))

        ;; Reverse all of the code back into normal order.
        (setf (code block) (nreverse (code block))))

      ;; Let's create try blocks
      (when-let ((exception-table (exception-table *context*)))
        (loop for i from 0 below (length exception-table)
              for ete = (aref exception-table i)
              for start-block = (gethash (start-pc ete) block-by-address)
              for end-block = (gethash (end-pc ete) block-by-address)
              for handler = (gethash (handler-pc ete) block-by-address)
              do (push end-block (exception-end-blocks start-block))
              do (push (cons (catch-type ete) handler) (try-catch start-block))))

      ;; Compute dominance sets for all blocks.
      (compute-dominance blocks (gethash 0 block-by-address))

      ;; Let's eliminate the exit goto for try and handler blocks using dominator sets.
      (labels ((find-merge-point (block dominator seen-table)
                 "Find a successor to BLOCK not dominated by DOMINATOR."
                 (if (not (gethash block seen-table))
                     (progn
                       (setf (gethash block seen-table) t)
                       (if (fset:member? dominator (dominators block))
                           (loop for successor in (fset:convert 'list (successors block))
                                 for merge-point = (find-merge-point successor dominator seen-table)
                                 when merge-point
                                   do (setf (end-of-handler? block) t)
                                   and return merge-point)
                           block))
                     nil))
               (remove-goto (block target-address seen-table)
                 "Remove a trailing GOTO to TARGET-ADDRESS at the end of BLOCK and successors until we reach TARGET-ADDRESS."
                 (unless (gethash block seen-table)
                   (setf (gethash block seen-table) t)
                   (let ((last-insn (first (last (code block)))))
                     (when (and (typep last-insn 'ir-goto)
                                (eql target-address (slot-value last-insn 'offset)))
                       ;; Replace the goto with a nop
                       (setf (code block)
                             (append1 (butlast (code block))
                                               (make-instance 'ir-stop-marker :address (address last-insn))))))
                   (fset:do-set (b (successors block))
                     (remove-goto b target-address seen-table))
                   (dolist (b (mapcar #'rest (try-catch block)))
                     (remove-goto b target-address seen-table)))))
        (dolist (block blocks)
          (dolist (handler-type-block (try-catch block))
            (when-let ((merge-point (find-merge-point (rest handler-type-block) (rest handler-type-block) (make-hash-table :test #'equal))))
              (setf (try-exit-block block) merge-point)))))
                ; (remove-goto block (address merge-point) (make-hash-table))))))


      (setf (block-address-table *context*) block-by-address)

      (dump-method-dot blocks)
      blocks)))
