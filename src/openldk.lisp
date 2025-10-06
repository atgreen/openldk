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

(annot:enable-annot-syntax)

(in-package :openldk)

;; This is a hack to make sure getCallerClass works.
;; It would be good if we didn't have to do this.
(defvar *force-this-to-be-used* nil)

(defvar *methods-being-compiled* (make-hash-table :test #'equal :synchronized t)
  "Hash table tracking methods currently being compiled. Value is either T (compiling) or :DONE (compiled).")

(defvar *method-compilation-lock* (bt:make-lock "method-compilation-lock")
  "Lock to ensure atomic check-and-set for method compilation tracking.")

(defvar *method-compilation-cv* (bt:make-condition-variable :name "method-compilation-cv")
  "Condition variable to signal when a method compilation completes.")

(defun %eval (code)
  "Evaluate generated CODE, optionally printing and muffling warnings."
  (when *debug-codegen*
    (pprint code)
    (format t "~%"))
  (if *debug-unmuffle*
      (eval code) ; lint:suppress eval-usage
      (handler-bind
          (#+ansi-cl
           (style-warning (lambda (c)
                            (declare (ignore c))
                            (invoke-restart 'muffle-warning))))
        (eval code)))) ; lint:suppress eval-usage

(defun lispize-method-name (name)
  "Return a Lisp symbol name derived from Java method NAME."
  (take (1+ (position #\) name)) name))

(defun make-exception-handler-table (context)
  "Build a hashtable of handler PCs keyed by handler start for CONTEXT."
  (let ((exception-table (exception-table context))
        (exception-handler-table (make-hash-table)))
    (when exception-table
      (loop for i from 0 below (length exception-table)
            for ete = (aref exception-table i)
            do (setf (gethash (handler-pc ete) exception-handler-table) t)))
    exception-handler-table))

(defun fix-stack-variables (stack-vars)
  "Merge stack variable groups that share var-numbers across STACK-VARS."

  (let ((groups (make-hash-table :test 'equal)))
    ;; Step 1: Group stack-variables by their var-numbers
    (dolist (stack-var stack-vars)
      (let ((var-numbers (slot-value stack-var 'var-numbers)))
        (dolist (num var-numbers)
          (push stack-var (gethash num groups)))))

    ;; Step 2: Merge groups that share common numbers
    (let ((merged-groups ()))
      (maphash (lambda (num stack-vars)
                 (declare (ignore num))
                 (let ((merged-group ()))
                   (dolist (stack-var stack-vars)
                     (unless (member stack-var merged-group :test #'eq)
                       (push stack-var merged-group)))
                   (push merged-group merged-groups)))
               groups)

      ;; Step 3: Update var-numbers for each stack-variable
      (dolist (group merged-groups)
        (let ((all-var-numbers ()))
          (dolist (stack-var group)
            (setf all-var-numbers (union all-var-numbers (slot-value stack-var 'var-numbers))))
          (dolist (stack-var group)
            (setf (slot-value stack-var 'var-numbers) all-var-numbers))))))

  stack-vars)

(defun eliminate-dead-stack-assignments (blocks)
  "Remove assignments to stack variables that are never read.
   Only removes assignments where rvalue is a literal or another stack var (no side effects).
   Returns T if any assignments were removed."
  (let ((changed nil)
        (read-vars (make-hash-table :test #'eq)))

    ;; First pass: collect ALL stack variables that are read (as rvalues)
    (labels ((collect-reads (ir)
               (cond
                 ((typep ir '<stack-variable>)
                  (setf (gethash ir read-vars) t))
                 ((typep ir 'ir-assign)
                  ;; Only scan rvalue for reads, skip lvalue
                  (when-let ((rval (slot-value ir 'rvalue)))
                    (collect-reads rval)))
                 ((typep ir 'ir-node)
                  (dolist (slot-def (closer-mop:class-slots (class-of ir)))
                    (let ((slot-name (closer-mop:slot-definition-name slot-def)))
                      (when (and (slot-boundp ir slot-name)
                                 (not (eq slot-name 'address)))
                        (let ((val (slot-value ir slot-name)))
                          (cond
                            ((typep val 'ir-node) (collect-reads val))
                            ((listp val)
                             (dolist (item val)
                               (when (typep item 'ir-node)
                                 (collect-reads item)))))))))))))
      (dolist (block blocks)
        (dolist (insn (slot-value block 'code))
          (collect-reads insn))))

    ;; Second pass: remove dead assignments
    (dolist (block blocks)
      (setf (slot-value block 'code)
            (remove-if (lambda (insn)
                        (when (and (typep insn 'ir-assign)
                                   (typep (slot-value insn 'lvalue) '<stack-variable>))
                          (let* ((stack-var (slot-value insn 'lvalue))
                                 (rvalue (slot-value insn 'rvalue))
                                 ;; Only safe to remove if rvalue has no side effects
                                 (safe-rvalue? (or (typep rvalue 'ir-literal)
                                                  (typep rvalue '<stack-variable>)
                                                  (typep rvalue 'ir-local-variable)
                                                  (typep rvalue 'ir-long-local-variable))))
                            (when (and safe-rvalue?
                                      (not (gethash stack-var read-vars)))
                              (setf changed t)
                              t))))  ; remove this instruction
                      (slot-value block 'code))))
    changed))

(defun build-def-use-chains (ir-code)
  "Build def-use and use-def chains for dataflow analysis.
   Returns (values def-table use-list-table use-def-table)
   - def-table: variable -> defining instruction
   - use-list-table: variable -> list of instructions that use it
   - use-def-table: instruction -> variables it uses"
  (let ((def-table (make-hash-table :test 'eq))           ; var -> defining insn
        (use-list-table (make-hash-table :test 'eq))      ; var -> list of using insns
        (use-def-table (make-hash-table :test 'eq)))      ; insn -> list of vars used

    (labels ((collect-uses (ir insn)
               "Collect all variables used in IR, associate with INSN"
               (cond
                 ((typep ir '<stack-variable>)
                  ;; Record that this instruction uses this variable
                  (push insn (gethash ir use-list-table nil))
                  (pushnew ir (gethash insn use-def-table nil) :test 'eq))
                 ((typep ir 'ir-node)
                  ;; Walk all slots
                  (dolist (slot (closer-mop:class-slots (class-of ir)))
                    (let* ((slot-name (closer-mop:slot-definition-name slot)))
                      (when (slot-boundp ir slot-name)
                        (let ((slot-value (slot-value ir slot-name)))
                          (cond
                            ((typep slot-value 'ir-node)
                             (collect-uses slot-value insn))
                            ((listp slot-value)
                             (dolist (item slot-value)
                               (when (typep item 'ir-node)
                                 (collect-uses item insn)))))))))))))

      ;; Build the chains
      (dolist (insn ir-code)
        (cond
          ;; Assignments define a variable
          ((typep insn 'ir-assign)
           (let ((lvalue (slot-value insn 'lvalue))
                 (rvalue (slot-value insn 'rvalue)))
             (when (typep lvalue '<stack-variable>)
               (setf (gethash lvalue def-table) insn))
             ;; Collect uses in the rvalue
             (collect-uses rvalue insn)))
          ;; Other instructions may use variables
          ((typep insn 'ir-node)
           (collect-uses insn insn)))))

    (values def-table use-list-table use-def-table)))

(defun count-variable-uses (ir-code)
  "Count how many times each variable is used (read from) in IR-CODE."
  (multiple-value-bind (def-table use-list-table use-def-table)
      (build-def-use-chains ir-code)
    (declare (ignore def-table use-def-table))
    (let ((use-counts (make-hash-table :test 'eq)))
      (maphash (lambda (var use-list)
                 (setf (gethash var use-counts) (length use-list)))
               use-list-table)
      use-counts)))

(defun substitute-in-ir (ir subst-table)
  "Recursively substitute variables in IR using SUBST-TABLE."
  (cond
    ;; If this is a variable with a substitution, return the substitution
    ((and (typep ir '<stack-variable>)
          (gethash ir subst-table))
     (gethash ir subst-table))
    ;; If this is an IR node, recursively substitute in all slots
    ((typep ir 'ir-node)
     (dolist (slot (closer-mop:class-slots (class-of ir)))
       (let* ((slot-name (closer-mop:slot-definition-name slot)))
         (when (and (slot-boundp ir slot-name)
                    ;; Don't substitute in the lvalue of an assignment
                    (not (and (typep ir 'ir-assign) (eq slot-name 'lvalue))))
           (let ((slot-value (slot-value ir slot-name)))
             (cond
               ((typep slot-value 'ir-node)
                (setf (slot-value ir slot-name)
                      (substitute-in-ir slot-value subst-table)))
               ((listp slot-value)
                (setf (slot-value ir slot-name)
                      (mapcar (lambda (item)
                                (if (typep item 'ir-node)
                                    (substitute-in-ir item subst-table)
                                    item))
                              slot-value))))))))
     ir)
    ;; Otherwise return as-is
    (t ir)))

;;; ============================================================================
;;; Phase 3: Reaching Definitions Analysis (Inter-block propagation)
;;; ============================================================================

(defun compute-local-definitions (block)
  "Return a hash table mapping local-index -> list of IR-ASSIGN instructions that define
   stack variables which are then assigned to that local.

   Pattern: We track assignments of form 'local-X = s{Y}' and record the defining
   assignment 's{Y} = value' (if it exists in this block)."
  (let ((defs (make-hash-table :test #'eql))
        ;; First pass: collect stack-var -> definition mapping in this block
        (stack-defs (make-hash-table :test #'eq)))
    (dolist (insn (slot-value block 'code))
      (when (typep insn 'ir-assign)
        (let ((lvalue (slot-value insn 'lvalue)))
          (when (typep lvalue '<stack-variable>)
            (setf (gethash lvalue stack-defs) insn)))))

    ;; Second pass: for each 'local-X = s{Y}', record s{Y}'s definition
    (dolist (insn (slot-value block 'code))
      (when (typep insn 'ir-assign)
        (let ((lvalue (slot-value insn 'lvalue))
              (rvalue (slot-value insn 'rvalue)))
          (when (and (or (typep lvalue 'ir-local-variable)
                         (typep lvalue 'ir-long-local-variable))
                     (typep rvalue '<stack-variable>))
            (let ((idx (slot-value lvalue 'index))
                  (stack-def (gethash rvalue stack-defs)))
              (when stack-def
                (push stack-def (gethash idx defs))))))))
    defs))

(defun compute-gen-kill-sets (block all-local-defs)
  "Compute GEN and KILL sets for reaching definitions analysis.
   GEN: definitions created in this block
   KILL: definitions to the same local index created elsewhere
   Returns (values gen-set kill-set) as fset:sets of instructions."
  (let ((gen-set (fset:empty-set))
        (kill-set (fset:empty-set))
        (block-defs (compute-local-definitions block)))

    ;; For each local index defined in this block
    (maphash (lambda (idx insns)
               ;; Add this block's definitions to GEN
               (dolist (insn insns)
                 (setf gen-set (fset:with gen-set insn)))

               ;; Add all OTHER blocks' definitions to same index to KILL
               (let ((all-defs-for-idx (gethash idx all-local-defs)))
                 (dolist (def all-defs-for-idx)
                   (unless (member def insns :test #'eq)
                     (setf kill-set (fset:with kill-set def))))))
             block-defs)

    (values gen-set kill-set)))

(defun reaching-definitions-fixpoint (blocks)
  "Compute reaching definitions for all blocks using iterative dataflow analysis.
   Returns a hash table mapping block -> IN set (fset:set of IR-ASSIGN instructions)."

  ;; First, collect all local definitions across all blocks
  (let ((all-local-defs (make-hash-table :test #'eql)))
    (dolist (block blocks)
      (let ((block-defs (compute-local-definitions block)))
        (maphash (lambda (idx insns)
                   (setf (gethash idx all-local-defs)
                         (append insns (gethash idx all-local-defs))))
                 block-defs)))

    ;; Compute GEN/KILL for each block
    (let ((gen-kill (make-hash-table :test #'eq))
          (in-sets (make-hash-table :test #'eq))
          (out-sets (make-hash-table :test #'eq)))

      (dolist (block blocks)
        (multiple-value-bind (gen kill)
            (compute-gen-kill-sets block all-local-defs)
          (setf (gethash block gen-kill) (cons gen kill)))
        (setf (gethash block in-sets) (fset:empty-set))
        (setf (gethash block out-sets) (fset:empty-set)))

      ;; Iterate to fixpoint
      (loop
        (let ((changed nil))
          (dolist (block blocks)
            (let* ((gen (car (gethash block gen-kill)))
                   (kill (cdr (gethash block gen-kill)))
                   ;; IN[B] = union of OUT[P] for all predecessors P
                   (new-in (fset:reduce #'fset:union
                                       (fset:image (lambda (pred)
                                                    (gethash pred out-sets (fset:empty-set)))
                                                  (slot-value block 'predecessors))
                                       :initial-value (fset:empty-set)))
                   ;; OUT[B] = GEN[B] ∪ (IN[B] - KILL[B])
                   (new-out (fset:union gen (fset:set-difference new-in kill))))

              (unless (fset:equal? new-in (gethash block in-sets))
                (setf changed t)
                (setf (gethash block in-sets) new-in))

              (unless (fset:equal? new-out (gethash block out-sets))
                (setf changed t)
                (setf (gethash block out-sets) new-out))))

          (unless changed
            (return))))

      ;; Return IN sets
      in-sets)))

(defun has-intervening-assignment-p (local-var def-insn use-insn block-code)
  "Check if LOCAL-VAR is assigned between DEF-INSN and USE-INSN in BLOCK-CODE.
   Uses instruction identity (eq) for precise tracking within a basic block.
   Detects both explicit assignments (ir-assign) and iinc (ir-iinc).
   Returns T if there is an intervening assignment, NIL if safe to propagate."
  (let ((idx (slot-value local-var 'index))
        (between nil))
    (dolist (insn block-code)
      (cond
        ;; Found the definition - start checking
        ((eq insn def-insn)
         (setf between t))
        ;; Found the use - no intervening assignment
        ((eq insn use-insn)
         (return-from has-intervening-assignment-p nil))
        ;; Between def and use - check for writes to same local
        ((and between
              (or
               ;; Explicit assignment to local variable
               (and (typep insn 'ir-assign)
                    (or (typep (slot-value insn 'lvalue) 'ir-local-variable)
                        (typep (slot-value insn 'lvalue) 'ir-long-local-variable))
                    (= (slot-value (slot-value insn 'lvalue) 'index) idx))
               ;; IINC increments a local in place - also a write!
               (and (typep insn 'ir-iinc)
                    (= (slot-value insn 'index) idx))))
         (return-from has-intervening-assignment-p t))))
    ;; Didn't find use - be conservative
    t))

(defun apply-reaching-definitions (block reaching-in global-table)
  "Apply inter-block local propagation using reaching definitions.

   Strategy: For each instruction 'local-X = s{Y}' in BLOCK, if exactly ONE
   stack-variable definition 's{Y} = value' reaches (tracked via reaching-in),
   and that value is safe (literal or SSA stack-var), add s{Y} -> value
   to the global table to enable cross-block propagation.

   Pattern:
     Block A: s{3} = 42         (this is the reaching definition)
              local-5 = s{3}
     Block B: local-6 = s{3}    (s{3} = 42 reaches here)
              x = s{3}           (will be substituted by Phase 2)

   Result: We add s{3} -> 42 to global table for cross-block uses."
  (let ((in-set (gethash block reaching-in (fset:empty-set))))
    ;; Build a map from stack-var -> list of reaching definitions
    (let ((stack-var-reaching (make-hash-table :test #'eq)))
      (fset:do-set (def-insn in-set)
        ;; def-insn is a stack-var assignment 's{Y} = value'
        (let* ((stack-var (slot-value def-insn 'lvalue)))
          (when (typep stack-var '<stack-variable>)
            (push def-insn (gethash stack-var stack-var-reaching)))))

      ;; For each instruction in this block that uses a stack-var with reaching def
      (dolist (insn (slot-value block 'code))
        (when (typep insn 'ir-assign)
          (let ((lvalue (slot-value insn 'lvalue))
                (rvalue (slot-value insn 'rvalue)))
            ;; Pattern: 'local-X = s{Y}' or any use of s{Y}
            (when (typep rvalue '<stack-variable>)
              (let ((reaching-defs (gethash rvalue stack-var-reaching)))
                ;; If exactly one definition of s{Y} reaches here
                (when (and reaching-defs
                           (= (length reaching-defs) 1))
                  (let* ((unique-def (car reaching-defs))
                         (def-rvalue (slot-value unique-def 'rvalue)))
                    ;; Dereference the rvalue through global table if it's a stack-var
                    (let ((ultimate-value (if (typep def-rvalue '<stack-variable>)
                                             (gethash def-rvalue global-table def-rvalue)
                                             def-rvalue)))
                      ;; Add cross-block mapping: s{Y} -> ultimate-value
                      (when (and ultimate-value
                                 (or (typep ultimate-value 'ir-literal)
                                     (typep ultimate-value '<stack-variable>))
                                 (not (side-effect-p ultimate-value)))
                        (setf (gethash rvalue global-table) ultimate-value)))))))))))))

(defun can-propagate-p (var rvalue def-insn use-list-table ir-code &key allow-locals)
  "Determine if we can safely propagate VAR's definition (RVALUE) to all use sites.
   Uses def-use chains for precise analysis.

   Propagation is safe when:
   1. RValue is a pure value (literal or SSA variable) - always safe
   2. RValue is side-effect-free and used only once - safe to inline
   3. RValue is a local variable AND allow-locals=T AND no intervening assignments

   When allow-locals is NIL (Phase 1):
   - Only propagates literals and stack variables

   When allow-locals is T (Phase 2, intra-block):
   - Also propagates local variables if no intervening assignment exists"
  (let ((use-list (gethash var use-list-table)))
    (and
     ;; Must be a stack variable (SSA)
     (typep var '<stack-variable>)
     ;; Must have single static assignment (SSA property)
     (= (length (slot-value var 'var-numbers)) 1)
     ;; Check if we can propagate based on rvalue type
     (or
      ;; Case 1: Pure values - always safe to propagate
      ;; Literals (constants) can be duplicated without changing semantics
      (typep rvalue 'ir-literal)
      ;; Stack variables are SSA - no aliasing, safe to substitute
      (typep rvalue '<stack-variable>)

      ;; Case 2: Local variables - only if allow-locals=T
      ;; Will be checked for intervening assignments in propagate-copies
      (and allow-locals
           (or (typep rvalue 'ir-local-variable)
               (typep rvalue 'ir-long-local-variable)))

      ;; Case 3: Side-effect-free expression used once
      ;; Safe to inline since we're not duplicating computation
      (and (= (length use-list) 1)
           (not (side-effect-p rvalue)))))))

(defun propagate-copies (ir-code global-table &key allow-locals local-table)
  "Aggressively propagate copies using def-use chains.

   When allow-locals is NIL (default, Phase 1):
   - Only propagates literals and stack variables into global-table

   When allow-locals is T (Phase 2, per-block):
   - Propagates literals/SSA into global-table (safe cross-block)
   - Propagates local variables into local-table ONLY (intra-block only)
   - local-table must be provided when allow-locals is T"
  ;; Build dataflow information
  (multiple-value-bind (def-table use-list-table use-def-table)
      (build-def-use-chains ir-code)
    (declare (ignore use-def-table))

    (when *debug-propagation*
      (format t "~&; PROPAGATE: Processing ~A instructions (allow-locals=~A)~%"
              (length ir-code) allow-locals))

    ;; First pass: identify which assignments can be propagated
    (maphash (lambda (var def-insn)
               (when (typep def-insn 'ir-assign)
                 (let ((rvalue (slot-value def-insn 'rvalue)))
                   (when (can-propagate-p var rvalue def-insn use-list-table ir-code
                                         :allow-locals allow-locals)
                     ;; For local variables, check each use site for intervening assignments
                     (let ((safe-to-propagate t)
                           (is-local (or (typep rvalue 'ir-local-variable)
                                        (typep rvalue 'ir-long-local-variable))))
                       (when (and allow-locals is-local)
                         ;; Check every use site
                         (dolist (use-insn (gethash var use-list-table))
                           (when (has-intervening-assignment-p rvalue def-insn use-insn ir-code)
                             (when *debug-propagation*
                               (format t "~&; PROPAGATE: SKIP ~A = ~A (intervening assignment)~%"
                                       var rvalue))
                             (setf safe-to-propagate nil)
                             (return))))
                       ;; Only propagate if safe
                       (when safe-to-propagate
                         (when *debug-propagation*
                           (format t "~&; PROPAGATE: ~A = ~A (type: ~A, uses: ~A, scope: ~A)~%"
                                   var rvalue (type-of rvalue)
                                   (length (gethash var use-list-table))
                                   (if is-local "local" "global")))
                         ;; Put locals in local-table, everything else in global-table
                         (if is-local
                             (when local-table
                               (setf (gethash var local-table) rvalue))
                             (setf (gethash var global-table) rvalue))))))))
             def-table)

    ;; Merge both tables for substitution (local overrides global for this block)
    (let ((combined-table (make-hash-table :test #'eq)))
      ;; First add global mappings
      (maphash (lambda (k v) (setf (gethash k combined-table) v)) global-table)
      ;; Then add local mappings (overrides global if same key)
      (when local-table
        (maphash (lambda (k v) (setf (gethash k combined-table) v)) local-table))

      ;; Second pass: substitute and remove assignments
      (mapcar (lambda (insn)
                ;; Substitute in all instructions using combined table
                (let ((new-insn (substitute-in-ir insn combined-table)))
                  ;; Only remove assignments that are in the GLOBAL table (cross-block safe)
                  ;; Keep assignments in local-table only (other blocks may need them)
                  (if (and (typep new-insn 'ir-assign)
                           (gethash (slot-value new-insn 'lvalue) global-table))
                      (progn
                        (when *debug-propagation*
                          (format t "~&; PROPAGATE: Removing assignment ~A (global scope)~%" insn))
                        (make-instance 'ir-nop :address (address new-insn)))
                      new-insn)))
              ir-code))))

(defun %get-constant-int (ir context)
  "If IR is or becomes an IR-INT-LITERAL in CONTEXT, return its integer value."
  (let ((ir (or (gethash ir (single-assignment-table context))
                ir)))
    (cond
     ((typep ir 'ir-int-literal)
      (value ir))
     (t
      nil))))

(defun initialize-arrays (ir-code context)
  (let ((code-array (coerce ir-code 'vector))
        (changed nil))
    (loop for i below (length ir-code)
          for insn = (aref code-array i)
          when (and (typep insn 'ir-assign)
                    (let ((rvalue (slot-value insn 'rvalue)))
                      (and (typep rvalue 'ir-new-array)
                           (%get-constant-int (size rvalue) context))))
            do (let* ((rvalue (slot-value insn 'rvalue))
                      (component-class (component-class rvalue))
                      (init-element
                        (case (atype rvalue)
                          ;; Determine the initial element based on the array type
                          (4 0)        ; Byte
                          (5 #\Null)   ; Character
                          (6 0.0)      ; Single-precision float
                          (7 0.0d0)    ; Double-precision float
                          ((8 9 10 11) 0) ; Byte/Short/Int/Long (default to 0)
                          (t nil))))   ; Default to nil for unknown types
                 (let* ((pc (1+ i))
                        (ir-values (loop for array-index from 0 below (%get-constant-int (size rvalue) context)
                                         collect (progn
                                                   (loop until (not (typep (aref code-array pc) 'ir-nop))
                                                         do (incf pc))
                                                   (let ((insn (aref code-array pc)))
                                                     (incf pc)
                                                     (if (typep insn 'ir-xastore)
                                                         (value insn)
                                                         (return nil))))))
                        (array (cond
                                 ((zerop (%get-constant-int (size rvalue) context))
                                  #())
                                 (ir-values
                                  ;; Keep IR nodes (will codegen later)
                                  (loop for nop-pc from (1+ i) below pc
                                        do (setf (aref code-array nop-pc) (make-instance 'ir-nop :address (address (aref code-array nop-pc)))))
                                  (setf changed t)
                                  ir-values)
                                 (t
                                  ;; Pattern didn't match - use default initialization
                                  (make-array (%get-constant-int (size rvalue) context)
                                              :initial-element init-element)))))
                   (setf (slot-value insn 'rvalue)
                         (make-instance 'ir-array-literal
                                        :address (address insn)
                                        :component-class component-class
                                        :value array)))
                 (assert (typep (aref code-array (1- i)) 'ir-nop))))
    (values (coerce code-array 'list) changed)))

(defun %write-aot-method (class-name method-name definition-code)
  "Write AOT compiled Lisp code to a file in the top-level AOT directory."
  (when *aot-dir*
    (let* ((path-parts (split-sequence:split-sequence #\/ class-name))
           (dir-path (format nil "~A/~{~A~^/~}"
                           *aot-dir*
                           (butlast path-parts)))
           (filename (format nil "~A/~A.lisp"
                           dir-path
                           (car (last path-parts))))
           (method-str (with-output-to-string (s)
                        (let ((*print-case* :downcase))
                          (pprint definition-code s)))))
      (ensure-directories-exist filename)
      ;; Append to file if it exists (multiple methods per class)
      (with-open-file (out filename
                          :direction :output
                          :if-exists :append
                          :if-does-not-exist :create)
        (format out "~%~A~%" method-str)))))

(defun %write-aot-class (class-name class-definition-code)
  "Store AOT class definitions in memory for later topological sorting and writing."
  (when *aot-dir*
    ;; Store the class definition along with its parent class name for sorting
    (let* ((class (gethash class-name *ldk-classes-by-bin-name*))
           (parent-name (when class (slot-value class 'super))))
      (setf (gethash class-name *aot-class-definitions*)
            (list :code class-definition-code :parent parent-name)))))

(defun %topological-sort-classes (class-defs-hash)
  "Topologically sort classes so parents come before children."
  (let ((sorted nil)
        (visited (make-hash-table :test #'equal))
        (visiting (make-hash-table :test #'equal)))
    (labels ((visit (class-name)
               (cond
                 ((gethash class-name visited)
                  ;; Already processed
                  nil)
                 ((gethash class-name visiting)
                  ;; Circular dependency - skip
                  (format t ";   Warning: Circular dependency detected for ~A~%" class-name)
                  nil)
                 (t
                  (setf (gethash class-name visiting) t)
                  (let ((class-info (gethash class-name class-defs-hash)))
                    (when class-info
                      (let ((parent (getf class-info :parent)))
                        ;; Visit parent first if it exists and is in our set
                        (when (and parent (gethash parent class-defs-hash))
                          (visit parent)))
                      ;; Now add this class
                      (push (cons class-name class-info) sorted)
                      (setf (gethash class-name visited) t)))
                  (remhash class-name visiting)))))
      ;; Visit all classes
      (maphash (lambda (class-name class-info)
                 (declare (ignore class-info))
                 (visit class-name))
               class-defs-hash)
      (reverse sorted))))

(defun %write-all-aot-classes (aot-dir)
  "Write all collected class definitions to a single classes.lisp file in topological order."
  (when (and *aot-class-definitions* (> (hash-table-count *aot-class-definitions*) 0))
    (let* ((sorted-classes (%topological-sort-classes *aot-class-definitions*))
           (classes-file (format nil "~A/classes.lisp" aot-dir)))
      (with-open-file (out classes-file
                          :direction :output
                          :if-exists :supersede
                          :if-does-not-exist :create)
        (format out ";;;; AOT-compiled class definitions~%")
        (format out ";;;; Classes are topologically sorted (parents before children)~%~%")
        (dolist (class-entry sorted-classes)
          (let ((class-name (car class-entry))
                (class-code (getf (cdr class-entry) :code)))
            (format out "~%; Class: ~A~%" class-name)
            (let ((*print-case* :downcase))
              (pprint class-code out))
            (format out "~%~%"))))
      (format t "; Wrote ~A class definitions to ~A~%"
              (length sorted-classes) classes-file))))

(defun %generate-aot-asdf-file (aot-dir system-name)
  "Generate an ASDF system definition file that loads classes.lisp then all method files."
  (let* ((aot-dir-path (uiop:ensure-directory-pathname aot-dir))
         (method-files nil))
    ;; Collect all .lisp files in aot-dir (excluding classes.lisp)
    (dolist (file (directory (merge-pathnames "**/*.lisp" aot-dir-path)))
      (let ((filename (file-namestring file)))
        (unless (string= filename "classes.lisp")
          (let* ((file-truename (truename file))
                 (dir-truename (truename aot-dir-path))
                 (relative-path (uiop:enough-pathname file-truename dir-truename))
                 ;; Remove .lisp extension and convert to forward slashes
                 (file-path (substitute #\/ (uiop:directory-separator-for-host)
                                       (subseq (uiop:native-namestring relative-path) 0
                                               (- (length (uiop:native-namestring relative-path)) 5)))))
            (push file-path method-files)))))
    ;; Generate the ASDF file
    (let ((asdf-file (format nil "~A/~A.asd" aot-dir system-name)))
      (with-open-file (out asdf-file
                          :direction :output
                          :if-exists :supersede
                          :if-does-not-exist :create)
        (format out ";;;; ASDF system definition for AOT-compiled Java classes~%~%")
        (format out "(defsystem ~S~%" system-name)
        (format out "  :description \"AOT-compiled Java bytecode to Common Lisp\"~%")
        (format out "  :serial t~%")
        (format out "  :components~%")
        (format out "  (")
        ;; First, load classes.lisp with all class definitions
        (format out "~%   ;; Class definitions (topologically sorted)~%")
        (format out "   (:file \"classes\")~%")
        ;; Then, load all method definitions
        (when method-files
          (format out "~%   ;; Method definitions (loaded after classes)~%")
          (dolist (method-file (sort method-files #'string<))
            (format out "   (:file ~S)~%" method-file)))
        (format out "))~%"))
      (format t "~%; Generated ASDF file: ~A~%" asdf-file))))

(defun %compile-method (class-name method-index)
  (let* ((class (%get-ldk-class-by-bin-name class-name))
         (method (aref (slot-value class 'methods) (1- method-index)))
         (method-key (format nil "~A.~A~A" class-name (slot-value method 'name) (slot-value method 'descriptor))))
    ;; Use a lock to atomically check if method is being compiled and claim it if not
    (bt:with-lock-held (*method-compilation-lock*)
      (loop
        (let ((status (gethash method-key *methods-being-compiled*)))
          (cond
            ((eq status :done)
             ;; Already compiled by another thread
             (return-from %compile-method nil))
            ((eq status t)
             ;; In AOT mode, don't wait - just skip methods being compiled to avoid recursion
             (if *aot-dir*
                 (return-from %compile-method nil)
                 ;; In normal mode, wait for the other thread
                 (bt:condition-wait *method-compilation-cv* *method-compilation-lock*)))
            (t
             ;; Not being compiled - claim it and proceed
             (setf (gethash method-key *methods-being-compiled*) t)
             (return))))))
    (unwind-protect
        (when (gethash "Code" (slot-value method 'attributes)) ; otherwise it is abstract
      (let* ((parameter-hints (gen-parameter-hints (descriptor method)))
             (exception-table (slot-value (gethash "Code" (slot-value method 'attributes)) 'exceptions))
             (code (slot-value (gethash "Code" (slot-value method 'attributes)) 'code))
             (max-locals (slot-value (gethash "Code" (slot-value method 'attributes)) 'max-locals))
             (length (length code))
             (*context* (make-instance '<context>
                                       :class class
                                       :classes *ldk-classes-by-bin-name*
                                       :exception-table exception-table
                                       :bytecode code
                                       :insn-size (make-array (length code) :element-type 'fixnum :initial-element -1)
                                       :next-insn-list (make-array (length code) :initial-element nil)
                                       :stack-state-table (make-hash-table)
                                       :pc 0
                                       :is-clinit-p (string= "<clinit>" (slot-value method 'name)))))
        (setf (svcount *context*) 0)
        (when *debug-codegen*
          (format t "; compiling ~A.~A~%" class-name (lispize-method-name (format nil "~A~A" (name method) (descriptor method))))
          (force-output))
        (if (static-p method)
            (setf (fn-name *context*)
                  (format nil "~A.~A"
                          (slot-value class 'name)
                          (lispize-method-name
                           (format nil "~A~A"
                                   (slot-value method 'name)
                                   (slot-value method 'descriptor)))))
            (setf (fn-name *context*)
                  (format nil "~A"
                          (lispize-method-name
                           (format nil "~A~A"
                                   (slot-value method 'name)
                                   (slot-value method 'descriptor))))))
        (let* ((exception-handler-table (make-exception-handler-table *context*))
               (ir-code-0
                 (setf (ir-code *context*)
                       (let ((code (apply #'append
                                          (loop
                                            while (and (< (pc *context*) length))
                                            for no-record-stack-state? = (find (aref *opcodes* (aref code (pc *context*))) '(:GOTO :ATHROW :RETURN :IRETURN :LRETURN :FRETURN :DRETURN :ARETURN))
                                            for result = (progn
                                                           (let ((stk (gethash (pc *context*) (stack-state-table *context*))))
                                                             (when stk
                                                               (setf (stack *context*) (car stk))))
                                                           (when *debug-bytecode*
                                                             (format t "~&; c[~A] ~A ~@<~A~:@>" (pc *context*) (aref *opcodes* (aref code (pc *context*))) (stack *context*)))
                                                           (let* ((pc-start (pc *context*)))
                                                             (if (gethash pc-start exception-handler-table)
                                                                 (let ((var (make-stack-variable *context* pc-start :REFERENCE)))
                                                                   (push var (stack *context*))
                                                                   (cons (make-instance 'ir-assign
                                                                                        :address pc-start
                                                                                        :lvalue var
                                                                                        :rvalue (make-instance 'ir-condition-exception))
                                                                         (mapcar (lambda (insn)
                                                                                   (with-slots (address) insn
                                                                                     (setf address (+ address 0.1)))
                                                                                   insn)
                                                                                 (funcall
                                                                                  (aref *opcodes* (aref code (pc *context*)))
                                                                                  *context* code))))
                                                                 (funcall
                                                                  (aref *opcodes* (aref code (pc *context*)))
                                                                  *context* code))))
                                            unless no-record-stack-state?
                                              do (%record-stack-state (pc *context*) *context*)
                                            unless (null result)
                                              collect result))))
                         ;; Do stack analysis to merge stack variables
                         ;; When multiple control flow paths reach the same PC, we need to
                         ;; unify the stack variables. merge-stacks has side effects - it
                         ;; mutates the var-numbers slot of stack variables to include the
                         ;; union of all paths. The return value is discarded; the important
                         ;; work is the mutation of shared stack-variable objects.
                         (handler-bind
                             ((error (lambda (e)
                                       (format *error-output* "~&Error in method ~A.~A~A: ~A~%"
                                               class-name (slot-value method 'name) (slot-value method 'descriptor) e))))
                           (maphash (lambda (k v)
                                      (when (> (length v) 1)
                                        (reduce #'merge-stacks v)))
                                    (stack-state-table *context*)))
                         (fix-stack-variables (stack-variables *context*))
                         (loop
                           (multiple-value-bind (new-code changed?)
                               (initialize-arrays code *context*)
                             (unless changed?
                               (return))
                             (setf code new-code)))
                         code)))
               ;; (sdfdfd (print ir-code-0))
               ;; Build basic blocks first for CFG-safe propagation
               (blocks-before-prop (build-basic-blocks ir-code-0))
               ;; Phase 3: Compute reaching definitions for inter-block propagation
               (reaching-in (when (and *enable-copy-propagation*
                                      *enable-reaching-definitions*)
                              (reaching-definitions-fixpoint blocks-before-prop)))
               ;; Per-block propagation with separate scopes for locals vs globals
               (blocks (if *enable-copy-propagation*
                           (let ((global-table (single-assignment-table *context*)))
                             ;; Phase 3: Apply reaching definitions before per-block propagation
                             (when *enable-reaching-definitions*
                               (dolist (block blocks-before-prop)
                                 (apply-reaching-definitions block reaching-in global-table)))
                             ;; Phase 2: Per-block local propagation
                             (mapcar (lambda (block)
                                       (let ((block-code (slot-value block 'code))
                                             ;; Per-block table for local variables only
                                             (block-local-table (when *enable-local-propagation*
                                                                  (make-hash-table :test #'eq))))
                                         (setf (slot-value block 'code)
                                               (propagate-copies block-code global-table
                                                               :allow-locals *enable-local-propagation*
                                                               :local-table block-local-table))
                                         block))
                                     blocks-before-prop))
                           blocks-before-prop))
               ;; Dead code elimination: remove assignments to stack vars that are never read
               ;; TODO: Debug why DCE causes NPE
               (blocks-after-dce blocks) #| (let ((blks blocks))
                                  (when *enable-copy-propagation*
                                    (loop while (eliminate-dead-stack-assignments blks)))
                                  blks) |#
               (lisp-code
                 (list (list 'block nil
                             (append (list 'tagbody)
                                     (mapcan (lambda (x) (if (listp x) x (list x)))
                                             (loop for block in blocks-after-dce
                                                   for code = (codegen-block block block)
                                                   when code
                                                     collect (progn
                                                               code)))))))
               (traced-lisp-code (if *debug-trace* `((unwind-protect
                                                          ,(car lisp-code)
                                                       (incf *call-nesting-level* -1)))
                                     lisp-code))
               (array-checked-lisp-code (if (needs-array-bounds-check *context*)
                                            `((handler-bind
                                                  ((sb-int:invalid-array-index-error
                                                     (lambda (e)
                                                       (error (openldk::%lisp-condition
                                                               (openldk::%make-throwable
                                                                'openldk::|java/lang/ArrayIndexOutOfBoundsException|)))))
                                                   (type-error
                                                     (lambda (e)
                                                       (format t "TYPE-ERROR: ~A~%" e)
                                                       (sb-debug:print-backtrace)
                                                       (error (openldk::%lisp-condition
                                                               (openldk::%make-throwable
                                                                'openldk::|java/lang/NullPointerException|))))))
                                                ,(car traced-lisp-code)))
                                            traced-lisp-code))
               (definition-code
                 (let ((parameter-count (count-parameters (slot-value method 'descriptor))))
                   (let ((args (if (static-p method)
                                   (loop for i from 1 upto parameter-count
                                         collect (intern (format nil "arg~A" (1- i)) :openldk))
                                   (loop for i from 1 upto parameter-count
                                         collect (intern (format nil "arg~A" i) :openldk)))))
                     `(progn
                        ,(append (if (static-p method)
                                    (list 'defun (intern (fn-name *context*) :openldk) args)
                                    (list 'defmethod
                                          (intern (fn-name *context*) :openldk)
                                          (cons (list (intern "this" :openldk) (intern (slot-value class 'name) :openldk))
                                                args)))
                                (when *debug-trace*
                                  (list (list 'format 't "~&~V@A <~A> trace: entering ~A.~A(~{~A~^ ~}) ~A~%"
                                              (list 'incf '*call-nesting-level* 1) "*" '*call-nesting-level*
                                              class-name (fn-name *context*) (if *debug-trace-args*
                                                                                 (cons 'list args)
                                                                                 ())
                                              (if (not (static-p method)) (intern "this" :openldk) ""))))
                                (when (not (static-p method))
                                  (list (list 'setf '*force-this-to-be-used* (intern "this" :openldk))))
;;                                        (list 'describe (intern "this" :openldk))))
                                (let ((i 0)
                                      (pc -1))
                                  (list (format nil "bridge=~A" (bridge-p method))
                                        (append (list 'let (if (static-p method)
                                        (append
                                         (list (list '|condition-cache|))
                                         (remove-duplicates
                                          (loop for var in (stack-variables *context*)
                                                unless (gethash var (single-assignment-table *context*))
                                                  collect (list (intern (format nil "s{~{~A~^,~}}"
                                                                                (sort (copy-list (var-numbers var)) #'<))
                                                                       :openldk)))
                                          :test #'equal)
                                         (loop for ph in parameter-hints
                                               collect (list (intern (format nil "local-~A" i) :openldk)
                                                             (intern (format nil "arg~A" (incf pc)) :openldk))
                                               do (if (eq ph t) (incf i) (incf i 2)))
                                         (loop for pc from (- parameter-count 2) upto max-locals
                                               collect (list (intern (format nil "local-~A" (1- (incf i))) :openldk))))
                                        (append
                                         (list (list '|condition-cache|))
                                         (remove-duplicates
                                          (loop for var in (stack-variables *context*)
                                                unless (gethash var (single-assignment-table *context*))
                                                  collect (list (intern (format nil "s{~{~A~^,~}}"
                                                                                (sort (copy-list (var-numbers var)) #'<))
                                                                       :openldk)))
                                          :test #'equal)
                                         (append
                                          (list (list (intern "local-0" :openldk) (intern "this" :openldk)))
                                          (loop for ph in parameter-hints
                                                collect (list (intern (format nil "local-~A" (1+ i)) :openldk)
                                                              (intern (format nil "arg~A" (1+ (incf pc))) :openldk))
                                                do (if (eq ph t) (incf i) (incf i 2)))
                                          (loop for x from parameter-count upto (1+ max-locals)
                                                collect (list (intern (format nil "local-~A" (incf i)) :openldk)))))))
                                                array-checked-lisp-code)))))))))
          (if *aot-dir*
              (%write-aot-method class-name
                               (lispize-method-name (format nil "~A~A" (name method) (descriptor method)))
                               definition-code)
              (%eval definition-code)))))
      ;; Cleanup: mark compilation as done and notify waiting threads
      (bt:with-lock-held (*method-compilation-lock*)
        (setf (gethash method-key *methods-being-compiled*) :done)
        (bt:condition-notify *method-compilation-cv*)))))

(defun %clinit (class)
  (let ((class (gethash (name class) *ldk-classes-by-bin-name*)))
    (assert
     (or class (error "Can't find ~A" class)))
    (labels ((clinit (class)
               (let ((super-class (gethash (slot-value class 'super) *ldk-classes-by-bin-name*)))
                 (when (and super-class
                            (not (initialized-p super-class)))
                   (clinit super-class)))
               (let ((<clinit>-method (find-if
                                       (lambda (method) (and (string= (slot-value method 'name) "<clinit>")
                                                             (string= (slot-value method 'descriptor) "()V")))
                                       (slot-value class 'methods))))
                 (when <clinit>-method
                   (setf (initialized-p class) t)
                   (handler-case
                       (%eval (list (intern (format nil "~A.<clinit>()" (slot-value class 'name)) :openldk)))
                     (error (e)
                       ;; Wrap exception in ExceptionInInitializerError if classes are loaded
                       ;; Use ignore-errors to handle case where parent class isn't loaded yet
                       (let ((eiie (ignore-errors
                                     (when (and (find-class '|java/lang/ExceptionInInitializerError| nil)
                                                (find-class '|java/lang/Error| nil))
                                       (let ((instance (make-instance '|java/lang/ExceptionInInitializerError|)))
                                         (|<init>()| instance)
                                         instance)))))
                         (if eiie
                             (error (%lisp-condition eiie))
                             ;; During early bootstrap, just re-signal the original error
                             (error e)))))))))
      (clinit class))))

(defun open-java-classfile-on-classpath (class)
  (let* ((class (substitute (uiop:directory-separator-for-host) #\. class)))
    (loop for cpe in *classpath*
          for classfile-stream = (open-java-classfile cpe class)
          when classfile-stream
            return classfile-stream)))

(defun initform-from-descriptor (descriptor)
  (cond
    ((string= descriptor "I")
     0)
    ((string= descriptor "J")
     0)
    ((string= descriptor "F")
     0.0)
    ((string= descriptor "D")
     0.0d0)
    ((string= descriptor "S")
     0)
    ((string= descriptor "B")
     0)
    ((string= descriptor "C")
     0)
    ((string= descriptor "Z")
     0)
    (t nil)))

(defun emit-<class> (class)
  (let ((defclass-code (with-slots (name super interfaces fields) class
                         (list
                          'progn
                          (list
                           'defclass (intern name :openldk)
                           (if (or super interfaces) (append (if super (list (intern super :openldk)) nil)
                                                             (let ((ifaces (mapcar (lambda (i) (intern i :openldk))
                                                                                   (coerce interfaces  'list))))
                                                               (sort (copy-list ifaces) #'subtypep)))
                               (list))
                           (map 'list
                                (lambda (f)
                                  (list (intern (slot-value f 'name) :openldk)
                                        :initform (let ((cf (gethash "ConstantValue" (slot-value f 'attributes))))
                                                    (if cf
                                                        (value (emit (aref (constant-pool class) cf) (constant-pool class)))
                                                        (initform-from-descriptor (slot-value f 'descriptor))))
                                        :allocation
                                        (if (eq 0 (logand 8 (slot-value f 'access-flags))) :instance :class)))
                                fields))
                          (list
                           'defparameter (intern (format nil "+static-~A+" (intern name)) :openldk)
                           (list
                            'make-instance (list 'quote (intern name :openldk)))))))
        (methods-code
          (let ((method-index 0)
                (done-method-table (make-hash-table :test #'equal)))
            (if (find (name class) '("java/util/jar/JarInputStream" "java/util/zip/ZipFile" "java/util/zip/ZipInputStream") :test #'equal)
                nil
                (with-slots (name super methods) class
                  (remove nil (map 'list
                                   (lambda (m)
                                     (if (or (native-p m)
                                             (null (gethash "Code" (attributes m)))
                                             (and (bridge-p m)
                                                  (gethash (lispize-method-name
                                                            (format nil "~A~A"
                                                                    (slot-value m 'name)
                                                                    (slot-value m 'descriptor)))
                                                           done-method-table)))
                                         (progn
                                           (incf method-index)
                                           nil)
                                         (progn
                                           (setf (gethash (lispize-method-name (format nil "~A~A" (slot-value m 'name) (slot-value m 'descriptor))) done-method-table) t)
                                           (if (static-p m)
                                               (list 'defun
                                                     (intern (format nil "~A.~A"
                                                                     (slot-value class 'name)
                                                                     (lispize-method-name
                                                                      (format nil "~A~A"
                                                                              (slot-value m 'name)
                                                                              (slot-value m 'descriptor))))
                                                             :openldk)
                                                     (loop for i from 1 upto (count-parameters (slot-value m 'descriptor))
                                                           collect (intern (format nil "arg~A" i) :openldk))
                                                     (list '%compile-method (slot-value class 'name) (incf method-index))
                                                     (cons (intern (format nil "~A.~A"
                                                                           (slot-value class 'name)
                                                                           (lispize-method-name
                                                                            (format nil "~A~A"
                                                                                    (slot-value m 'name)
                                                                                    (slot-value m 'descriptor))))
                                                                   :openldk)
                                                           (loop for i from 1 upto (count-parameters (slot-value m 'descriptor))
                                                                 collect (intern (format nil "arg~A" i) :openldk))))
                                               (list 'defmethod (intern (lispize-method-name (format nil "~A~A" (slot-value m 'name) (slot-value m 'descriptor))) :openldk)
                                                     (cons (list (intern "this" :openldk) (intern (slot-value (slot-value m 'class) 'name) :openldk))
                                                           (loop for i from 1 upto (count-parameters (slot-value m 'descriptor))
                                                                 collect (intern (format nil "arg~A" i) :openldk)))
                                                     (list '%compile-method (slot-value class 'name) (incf method-index))
                                                     (cons (intern (lispize-method-name (format nil "~A~A" (slot-value m 'name) (slot-value m 'descriptor))) :openldk)
                                                           (cons (intern "this" :openldk)
                                                                 (loop for i from 1 upto (count-parameters (slot-value m 'descriptor))
                                                                       collect (intern (format nil "arg~A" i) :openldk)))))))))
                                   methods)))))))

    (append defclass-code methods-code)))


(defun %classload-from-stream (classname classfile-stream class-loader)
  (unwind-protect
       (let* ((classname-symbol (intern classname :openldk))
              (fq-classname (cl-ppcre:regex-replace-all "\\.anonymous-class"
                                                        (substitute #\. #\/ classname)
                                                        "/anonymous-class"))
              (class
                (let ((c (read-classfile classfile-stream)))
                  (setf (name c) classname)
                  (setf (gethash classname *ldk-classes-by-bin-name*) c)
                  (setf (gethash fq-classname *ldk-classes-by-fq-name*) c)
                  c))
              (super (let ((super (slot-value class 'super)))
                       (when super (classload super))))
              (interfaces (let ((interfaces (slot-value class 'interfaces)))
                            (when interfaces
                              (mapcar (lambda (i) (classload i)) (coerce interfaces 'list))))))
         (let ((klass (or (%get-java-class-by-bin-name classname t)
                          (let ((klass (make-instance '|java/lang/Class|))
                                (cname (jstring fq-classname)))
                            (with-slots (|name| |classLoader|) klass
                              (setf |name| cname)
                              (setf |classLoader| class-loader))
                            klass))))
           (setf (java-class class) klass)
           (setf (slot-value klass '|classLoader|) class-loader)
           (setf (gethash classname *java-classes-by-bin-name*) klass)
           (setf (gethash fq-classname *java-classes-by-fq-name*) klass))

         (let ((code (emit-<class> class)))
           ;; In AOT mode, extract and write class definitions separately
           (when *aot-dir*
             ;; Code is a list like (progn (defclass...) (defparameter...) method-stub1 method-stub2 ...)
             ;; Extract just the defclass and defparameter (elements 2 and 3)
             (let ((class-defs (if (and (listp code) (eq (first code) 'progn))
                                   ;; If it's a progn, take the defclass and defparameter (2nd and 3rd elements)
                                   (list 'progn (second code) (third code))
                                   ;; Otherwise, just take the first two elements
                                   (list 'progn (first code) (second code)))))
               (%write-aot-class classname class-defs)))
           (%eval code))

         (dolist (ic (gethash "InnerClasses" (attributes class)))
           (when (zerop (outer-class-info-index ic))
             (let* ((class-reference (aref (constant-pool class) (inner-class-info-index ic)))
                    (class-name (aref (constant-pool class) (index class-reference))))
               (push class-name (inner-classes class)))))

         ;; Emit the class initializer
         (let ((lisp-class (find-class (intern (substitute #\/ #\. classname) :openldk))))
           (closer-mop:finalize-inheritance lisp-class)
           (let ((icc (append (list 'defun (intern (format nil "%clinit-~A" (substitute #\/ #\. classname)) :openldk) (list))
                              ;; (list (list 'format 't ">>> clinit ~A~%" lisp-class))
                              (loop for k in (reverse (closer-mop:class-precedence-list lisp-class))
                                    for clinit-function = (intern (format nil "~a.<clinit>()" (class-name k)) :openldk)
                                    when (fboundp clinit-function)
                                      collect (let ((ldkclass (gethash (format nil "~A" (class-name k)) *ldk-classes-by-bin-name*)))
                                                (list 'unless (list 'initialized-p ldkclass)
                                                      (list 'setf (list 'initialized-p ldkclass) t)
                                                      (list clinit-function)))))))
             (%eval icc)))

         (when (and (not (string= classname "java/lang/Throwable"))
                    (subtypep classname-symbol (find-class '|java/lang/Throwable|)))
           (let ((condition-symbol (intern (format nil "condition-~A" classname) :openldk)))
             (setf (gethash (find-class (intern classname :openldk)) *condition-table*) condition-symbol)
             (let ((ccode `(define-condition ,condition-symbol (,(intern (format nil "condition-~A" (slot-value super 'name)) :openldk))
                             ())))
               (%eval ccode))
             (let ((ccode `(defmethod %lisp-condition ((throwable ,(intern (format nil "~A" classname) :openldk)))
                             (let ((c (make-condition (quote ,(intern (format nil "condition-~A" classname) :openldk)))))
                               (setf (slot-value c '|objref|) throwable)
                               c))))
               (%eval ccode))))

         ;; Load all of the field classes
         (loop for field across (fields class)
               do (classload (slot-value (slot-value field 'class) 'name)))

         class)
    (close classfile-stream)))

(defun classload (classname &optional (class-loader nil))
  (let ((classname (coerce classname 'string)))
    (assert (not (find #\. classname)))
    (assert (> (length classname) 0))
    (let ((class (gethash classname *ldk-classes-by-bin-name*)))
      (if class
          class
          (let ((classfile-stream (open-java-classfile-on-classpath classname)))
            (if classfile-stream
                (progn
                  (when *debug-load*
                    (format t "~&; LOADING ~A~%" classname))
                  (if classfile-stream
                      (%classload-from-stream classname classfile-stream class-loader)
                      nil))
                nil))))))

(defun ensure-JAVA_HOME ()
  (let ((JAVA_HOME (uiop:getenv "JAVA_HOME")))
    (unless JAVA_HOME
      (format *error-output* "~%OpenLDK Error: JAVA_HOME environment variable not set~%")
      (uiop:quit 1))

    (unless (uiop:file-exists-p (concatenate 'string JAVA_HOME "/lib/rt.jar"))
      (format *error-output* "~%OpenLDK Error: Cannot find $JAVA_HOME/lib/rt.jar~%")
      (uiop:quit 1))))

@cli:command
(defun main (mainclass &optional (args (list)) &key dump-dir classpath aot)
  (declare
   (cli:parser (list identity) args)
   (cli:parser identity classpath)
   (cli:parser identity dump-dir)
   (cli:parser identity aot))
  "openldk - copyright (C) 2023-2024 Anthony Green <green@moxielogic.com>
   Distributed under the terms of the GPLv3 + Classpath Exception

   MAINCLASS: The class with the static main method to execute.

   ARGS: Java program command line arguments

   CLASSPATH: The classpath from which classes are loaded.

   DUMP-DIR: The directory into which internal debug info is dumped.

   AOT: Ahead-of-time compilation directory (generate Lisp source files)."

  (ensure-JAVA_HOME)

  ;; If classpath isn't set on the command line, then get it
  ;; from the LDK_CLASSPATH environment variable.
  (unless classpath
    (setf classpath (or (uiop:getenv "CLASSPATH") (uiop:getenv "LDK_CLASSPATH") ".")))

  ;; Always append JAVA_HOME jars to classpath
  (setf classpath
        (concatenate 'string
                     classpath
                     ":"
                     (format nil "~{~A~^:~}"
                             (mapcar #'namestring
                                     (directory
                                      (concatenate 'string
                                                   (uiop:getenv "JAVA_HOME")
                                                   "/lib/*.jar"))))))

  (let ((LDK_DEBUG (uiop:getenv "LDK_DEBUG")))
    (when LDK_DEBUG
      (progn

        (when (find #\c LDK_DEBUG)
          (setf *debug-codegen* t))
        (when (find #\l LDK_DEBUG)
          (setf *debug-load* t))
        (when (find #\s LDK_DEBUG)
          (setf *debug-slynk* t))
        (when (find #\t LDK_DEBUG)
          (setf *debug-trace* t))
        (when (find #\T LDK_DEBUG)
          (setf *debug-trace-args* t))
        (when (find #\b LDK_DEBUG)
          (setf *debug-bytecode* t))
        (when (find #\x LDK_DEBUG)
          (setf *debug-x* t))
        (when (find #\u LDK_DEBUG)
          (setf *debug-unmuffle* t))
        (when (find #\p LDK_DEBUG)
          (setf *debug-propagation* t)))))

  ;; Reset system properties to fix things that change between
  ;; build-time and run-time.
  (|java/lang/System.initProperties(Ljava/util/Properties;)|
   (slot-value |+static-java/lang/System+| '|props|))

  (%clinit (%get-ldk-class-by-bin-name "sun/misc/Launcher"))

  (when *debug-slynk*
    (slynk:create-server :port 2025)
    (sleep 10))

  (setf *dump-dir* dump-dir)
  (setf *aot-dir* aot)

  (setf *classpath*
        (loop for cpe in (split-sequence:split-sequence (uiop:inter-directory-separator) classpath)
              collect (if (ends-with? ".jar" cpe)
                          (make-instance 'jar-classpath-entry :jarfile cpe)
                          (make-instance 'dir-classpath-entry :dir cpe))))

  ;; In AOT mode, handle JAR files, directories, or class names
  (when *aot-dir*
    (cond
      ;; Check if it's a JAR file
      ((and (stringp mainclass) (str:ends-with? ".jar" mainclass))
       (let* ((jar-path (uiop:parse-native-namestring mainclass))
              (jar-entry (make-instance 'jar-classpath-entry :jarfile (namestring jar-path)))
              (class-names nil))
         (unless (uiop:file-exists-p jar-path)
           (error "JAR file not found: ~A" mainclass))
         ;; Initialize AOT class definitions hash table
         (when aot
           (setf *aot-class-definitions* (make-hash-table :test #'equal)))
         ;; Collect class names from JAR
         (dolist (class-name (list-jar-classes jar-entry))
           (when (str:ends-with? ".class" class-name)
             (let ((bin-name (substitute #\/ #\. (subseq class-name 0 (- (length class-name) 6)))))
               (push bin-name class-names))))
         ;; Load and compile each class immediately (streaming output)
         (let ((compiled-classes (make-hash-table :test #'equal)))
           (dolist (bin-name (reverse class-names))
             (handler-case
                 (progn
                   ;; Load with AOT enabled to write class definitions
                   (setf *aot-dir* aot)
                   (classload bin-name)
                   (setf *aot-dir* nil)
                   ;; Now compile methods with AOT enabled
                   (let ((class (gethash bin-name *ldk-classes-by-bin-name*)))
                     (when (and class (not (gethash bin-name compiled-classes)))
                       (setf (gethash bin-name compiled-classes) t)
                       (format t "; Transpiling ~A~%" bin-name)
                       (force-output)
                       (setf *aot-dir* aot)
                       (handler-case
                           (loop for method-index from 1 to (length (slot-value class 'methods))
                                 do (handler-case
                                        (%compile-method bin-name method-index)
                                      (error (e)
                                        (format t ";   Warning: Failed to compile method ~A in ~A: ~A~%" method-index bin-name e))))
                         (sb-kernel::control-stack-exhausted ()
                           (format t ";   ERROR: Stack exhausted compiling ~A, skipping remaining methods~%" bin-name)))
                       (setf *aot-dir* nil))))
               (error (e)
                 (format t ";   Warning: Failed to process ~A: ~A~%" bin-name e)))))
         ;; Write all class definitions to classes.lisp and generate ASDF file
         (when aot
           (%write-all-aot-classes aot)
           (%generate-aot-asdf-file aot "aot-compiled")))
       (return-from main))

      ;; Check if it's a directory
      ((uiop:directory-exists-p mainclass)
       ;; Temporarily add directory to classpath and load all .class files recursively
       (let* ((base-dir (truename (uiop:ensure-directory-pathname mainclass)))
              (dir-entry (make-instance 'dir-classpath-entry :dir (namestring base-dir)))
              (class-names nil))
         (push dir-entry *classpath*)
         ;; Initialize AOT class definitions hash table
         (when aot
           (setf *aot-class-definitions* (make-hash-table :test #'equal)))
         ;; First pass: collect all class names
         (dolist (class-file (directory (merge-pathnames "**/*.class" base-dir)))
           (let* ((relative-path (enough-namestring class-file base-dir))
                  ;; Convert to binary name: remove .class and use / as separator
                  (bin-name (substitute #\/ (uiop:directory-separator-for-host)
                                       (subseq (namestring relative-path) 0 (- (length (namestring relative-path)) 6)))))
             (push bin-name class-names)))
         ;; Second pass: load with AOT enabled to write class definitions
         (setf *aot-dir* aot)
         (dolist (bin-name (reverse class-names))
           (format t "; Loading ~A~%" bin-name)
           (handler-case
               (classload bin-name)
             (error (e)
               (format t ";   Warning: Failed to load ~A: ~A~%" bin-name e))))
         (setf *aot-dir* nil)
         ;; Third pass: now eagerly compile methods with AOT enabled
         (setf *aot-dir* aot)
         (let ((compiled-classes (make-hash-table :test #'equal)))
           (dolist (bin-name (reverse class-names))
             (let ((class (gethash bin-name *ldk-classes-by-bin-name*)))
               (when (and class (not (gethash bin-name compiled-classes)))
                 (setf (gethash bin-name compiled-classes) t)
                 (format t "; AOT compiling ~A~%" bin-name)
                 (handler-case
                     (loop for method-index from 1 to (length (slot-value class 'methods))
                           do (handler-case
                                  (%compile-method bin-name method-index)
                                (error (e)
                                  (format t ";   Warning: Failed to compile method ~A in ~A: ~A~%" method-index bin-name e))))
                   (sb-kernel::control-stack-exhausted ()
                     (format t ";   ERROR: Stack exhausted compiling ~A, skipping remaining methods~%" bin-name)))))))
         ;; Write all class definitions to classes.lisp and generate ASDF file
         (when aot
           (%write-all-aot-classes aot)
           (%generate-aot-asdf-file aot "aot-compiled")))
       (return-from main))

      ;; Otherwise treat as class name
      (t
       (let ((class (classload (substitute #\/ #\. mainclass))))
         (assert (or class (error "Can't load ~A" mainclass)))
         (return-from main)))))

  (let* ((class (classload (substitute #\/ #\. mainclass)))
         (argv (make-java-array
                :component-class (%get-java-class-by-bin-name "java/lang/String")
                :initial-contents (mapcar #'jstring args))))

    (assert (or class (error "Can't load ~A" mainclass)))

    (%clinit class)

    ;; The `main` method may be in a superclass of CLASS.  Search for it.
    (labels ((find-main (class)
               (when class
                 (let ((main-symbol (intern (format nil "~A.main([Ljava/lang/String;)" (name class)) :openldk)))
                   (if (fboundp main-symbol)
                       main-symbol
                       (find-main (gethash (super class) *ldk-classes-by-bin-name*)))))))
      (let ((main-symbol (find-main class)))
        (if main-symbol
            (progn
              (%eval (list main-symbol argv))
              ;; Wait for all non-daemon Java threads to complete before exiting
              (loop
                (let ((java-threads (loop for java-thread being the hash-values of *lisp-to-java-threads*
                                          when (not (slot-value java-thread '|daemon|))
                                            collect java-thread)))
                  (if java-threads
                      (sleep 0.1)
                      (progn
                        ;; Give threads a moment to flush output buffers
                        (sleep 0.1)
                        (finish-output)
                        (return))))))
            (error "Main method not found in class ~A." (name class)))))))

(defun main-wrapper ()
  "Main entry point into OpenLDK. Process command line errors here."
  ;; Disable floating-point traps to match Java semantics (NaN/Infinity instead of errors)
  (sb-int:set-floating-point-modes :traps nil)
  (handler-case
      (main-command)
    (cli:wrong-number-of-args (condition)
      (declare (ignore condition))
      (format *error-output* "~%Usage: openldk MAINCLASS [ARGS...] [--classpath PATH] [--dump-dir DIR]~%~%")
      (format *error-output* "MAINCLASS: The class with the static main method to execute~%")
      (format *error-output* "ARGS:      Java program command line arguments~%")
      (format *error-output* "~%Options:~%")
      (format *error-output* "  --classpath PATH  The classpath from which classes are loaded~%")
      (format *error-output* "  --dump-dir DIR    Directory for internal debug info~%~%")
      (uiop:quit 1))
    (error (condition)
      (format *error-output* "~&Error: ~A~%" condition)
      (uiop:quit 1))))

(defun initialize (&optional (property-alist (list)))

  (assert (typep property-alist 'list))

  (ensure-JAVA_HOME)

  (let ((classpath
          (concatenate 'string
                       (uiop:getenv "LDK_CLASSPATH")
                       ":"
                       (format nil "~{~A~^:~}"
                               (mapcar #'namestring
                                       (directory
                                        (concatenate 'string
                                                     (uiop:getenv "JAVA_HOME")
                                                     "/lib/*.jar")))))))

    (setf *classpath*
          (loop for cpe in (split-sequence:split-sequence (uiop:inter-directory-separator) classpath)
                collect (if (ends-with? ".jar" cpe)
                            (make-instance 'jar-classpath-entry :jarfile cpe)
                            (make-instance 'dir-classpath-entry :dir cpe)))))

  (setf *debug-load* t)

  ;; We need to hand load these before Class.forName0 will work.
  (%clinit (classload "java/lang/Object"))
  (%clinit (classload "java/lang/String"))
  (%clinit (classload "java/lang/Class"))
  (%clinit (classload "java/lang/ClassLoader"))

  (handler-case

      (let ((boot-class-loader (make-instance '|java/lang/ClassLoader|)))

        (setf *boot-class-loader* boot-class-loader)

        (dolist (p '(("byte" . "B") ("char" . "C") ("int" . "I")
                     ("short" . "S") ("long" . "J") ("double" . "D")
                     ("float" . "F") ("boolean" . "Z") ("void" . "Z")))
          (let ((jclass (make-instance '|java/lang/Class|))
                (lclass (make-instance '<class>)))
            (setf (slot-value jclass '|name|) (ijstring (car p)))
            (setf (name lclass) (car p))
            (setf (java-class lclass) jclass)
            (setf (gethash (car p) *ldk-classes-by-fq-name*) lclass)
            (setf (gethash (car p) *ldk-classes-by-bin-name*) lclass)
            (setf (gethash (car p) *java-classes-by-fq-name*) jclass)
            (setf (gethash (car p) *java-classes-by-bin-name*) jclass)))

        ;; Preload some important classes.
        (dolist (c '("java/lang/Boolean"
                     "java/lang/Character"
                     "java/lang/Byte"
                     "java/lang/Short"
                     "java/lang/Integer"
                     "java/lang/Long"
                     "java/lang/Float"
                     "java/lang/Double"
                     "java/nio/LongBuffer"
                     "java/lang/Void"
                     "java/lang/ClassLoader"
                     "java/security/PrivilegedAction"
                     "java/lang/StackTraceElement"
                     "java/lang/System"
                     "java/lang/ThreadGroup"
                     "java/lang/Thread"
                     "java/lang/ref/SoftReference"
                     "java/util/Properties"
                     "java/lang/SecurityManager"))
          (|java/lang/Class.forName0(Ljava/lang/String;ZLjava/lang/ClassLoader;Ljava/lang/Class;)| (jstring c) nil boot-class-loader nil))

        (let ((props (make-instance '|java/util/Properties|)))
          (|<init>()| props)
          (setf (slot-value |+static-java/lang/System+| '|props|) props))

        ;; Seed essential system properties needed during early JDK init.
        ;; Many JDK components assume non-NIL encodings.
        (dolist (kv `(("file.encoding" . "UTF-8")
                      ("sun.jnu.encoding" . "UTF-8")
                      ("sun.stdout.encoding" . "UTF-8")
                      ("sun.stderr.encoding" . "UTF-8")
                      ("line.separator" . "\n")
                      ("file.separator" . "/")
                      ("path.separator" . ":")
                      ("os.name" . "Linux")
                      ("os.arch" . "amd64")
                      ("os.version" . "")
                      ("java.io.tmpdir" . "/tmp")))
          (|java/lang/System.setProperty(Ljava/lang/String;Ljava/lang/String;)| (ijstring (car kv)) (ijstring (cdr kv))))

        ;; Also set java.home if provided by the environment for code that queries it early.
        (when-let ((jh (uiop:getenv "JAVA_HOME")))
          (|java/lang/System.setProperty(Ljava/lang/String;Ljava/lang/String;)| (ijstring "java.home") (ijstring jh)))

        ;; Populate common user properties
        (when-let ((cwd (uiop:getcwd)))
          (|java/lang/System.setProperty(Ljava/lang/String;Ljava/lang/String;)| (ijstring "user.dir") (ijstring (namestring cwd))))
        (when-let ((uh (or (uiop:getenv "HOME") "/")))
          (|java/lang/System.setProperty(Ljava/lang/String;Ljava/lang/String;)| (ijstring "user.home") (ijstring uh)))
        (when-let ((un (or (uiop:getenv "USER") (uiop:getenv "LOGNAME") "openldk")))
          (|java/lang/System.setProperty(Ljava/lang/String;Ljava/lang/String;)| (ijstring "user.name") (ijstring un)))

        ;; Provide a reasonable sun.boot.class.path early for sun/misc/Launcher
        ;; based on the jars in $JAVA_HOME/lib.
        (when-let ((jh (uiop:getenv "JAVA_HOME")))
          (let* ((boot-jars (directory (concatenate 'string jh "/lib/*.jar")))
                 (bootcp (format nil "~{~A~^:~}" (mapcar #'namestring boot-jars))))
            (when (> (length bootcp) 0)
              (|java/lang/System.setProperty(Ljava/lang/String;Ljava/lang/String;)| (ijstring "sun.boot.class.path") (ijstring bootcp)))))

        ;; Initialize baseline system properties now so subsequent init paths
        ;; (e.g., charset setup) do not observe NIL values.
        (|java/lang/System.initProperties(Ljava/util/Properties;)|
         (slot-value |+static-java/lang/System+| '|props|))

        ;; Add user-provided properties...
        (dolist (prop property-alist)
          (assert (typep prop 'list))
          (|java/lang/System.setProperty(Ljava/lang/String;Ljava/lang/String;)| (ijstring (car prop)) (ijstring (cdr prop))))

        (|java/lang/System.initializeSystemClass()|)

        (|java/lang/Class.forName0(Ljava/lang/String;ZLjava/lang/ClassLoader;Ljava/lang/Class;)| (jstring "sun/misc/Launcher") nil boot-class-loader nil)

        (|<init>()| boot-class-loader)

        (dolist (c '("java/lang/invoke/MethodHandles"
                     "java/lang/invoke/MethodHandles$Lookup"
                     "java/net/Inet4Address"))
          (|java/lang/Class.forName0(Ljava/lang/String;ZLjava/lang/ClassLoader;Ljava/lang/Class;)| (jstring c) nil boot-class-loader nil)))

    (|condition-java/lang/Throwable| (c)
      (format t "~&Exception: ~A~%" c)
      (let ((objref (slot-value c '|objref|)))
        (when (slot-boundp objref '|detailMessage|)
          (format t "Message: ~A~%" (slot-value objref '|detailMessage|)))
        (when (slot-boundp objref '|backtrace|)
          (format t "~&Backtrace: ~A~%" (slot-value objref '|backtrace|)))
        (let ((cause (slot-value objref '|cause|)))
          (when cause
            (format t "   Caused by: ~A~%" cause)
            (when (slot-boundp cause '|detailMessage|)
              (format t "Cause message: ~A~%" (slot-value cause '|detailMessage|)))
            (when (slot-boundp cause '|backtrace|)
              (format t "~&Cause backtrace: ~A~%" (slot-value cause '|backtrace|))))))))

  ;; Ensure the bootstrap launcher class is present even if earlier steps signalled.
  (let ((launcher (or (gethash "sun/misc/Launcher" *ldk-classes-by-bin-name*)
                      (ignore-errors (classload "sun/misc/Launcher")))))
    (when launcher
      (%clinit launcher)))

  (setf *debug-load* nil))

(defun make-image ()
  (initialize)
  ;; Kill all Java threads before saving core (SBCL can't save with threads running)
  (loop for thread in (bt:all-threads)
        when (and (not (eq thread (bt:current-thread)))
                  (search "Java-Thread" (bt:thread-name thread)))
        do (bt:destroy-thread thread))
  (sb-ext:save-lisp-and-die "openldk" :executable t :save-runtime-options t :toplevel #'main-wrapper))
