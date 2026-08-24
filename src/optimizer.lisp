;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: OPENLDK; Base: 10 -*-
;;;
;;; Copyright (C) 2024, 2025, 2026  Anthony Green <green@moxielogic.com>
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

;;; IR-level optimizer.  After bc-to-ir transpilation, %COMPILE-METHOD
;;; (openldk.lisp) runs these passes over the basic blocks: stack-variable
;;; merging, dead-store elimination, def-use chains, reaching-definitions
;;; analysis, copy propagation, and array-initialization batching.

(defun fix-stack-variables (stack-vars)
  "Merge stack variable groups that share var-numbers across STACK-VARS.
Uses union-find with transitive closure to ensure all connected stack-vars
get the same unified var-numbers."

  ;; Use union-find to group all stack-vars that should be unified
  (let ((parent (make-hash-table :test 'eq))
        (rank (make-hash-table :test 'eq)))

    ;; Initialize each stack-var as its own parent
    (dolist (sv stack-vars)
      (setf (gethash sv parent) sv)
      (setf (gethash sv rank) 0))

    ;; Find with path compression
    (labels ((find-root (sv)
               (let ((p (gethash sv parent)))
                 (if (eq p sv)
                     sv
                     (let ((root (find-root p)))
                       (setf (gethash sv parent) root)
                       root))))
             ;; Union by rank
             (union-sets (sv1 sv2)
               (let ((root1 (find-root sv1))
                     (root2 (find-root sv2)))
                 (unless (eq root1 root2)
                   (let ((rank1 (gethash root1 rank))
                         (rank2 (gethash root2 rank)))
                     (cond
                       ((< rank1 rank2)
                        (setf (gethash root1 parent) root2))
                       ((> rank1 rank2)
                        (setf (gethash root2 parent) root1))
                       (t
                        (setf (gethash root2 parent) root1)
                        (incf (gethash root1 rank)))))))))

      ;; Group stack-vars by var-numbers and union those that share any number
      (let ((by-num (make-hash-table :test 'eql)))
        (dolist (sv stack-vars)
          (dolist (num (slot-value sv 'var-numbers))
            (let ((existing (gethash num by-num)))
              (when existing
                (union-sets sv existing))
              (setf (gethash num by-num) sv)))))

      ;; Collect all stack-vars by their final root
      (let ((groups (make-hash-table :test 'eq)))
        (dolist (sv stack-vars)
          (push sv (gethash (find-root sv) groups)))

        ;; Update var-numbers for each unified group
        (maphash (lambda (root group)
                   (declare (ignore root))
                   (let ((all-var-numbers ()))
                     (dolist (sv group)
                       (setf all-var-numbers
                             (union all-var-numbers (slot-value sv 'var-numbers) :test 'eql)))
                     (dolist (sv group)
                       (setf (slot-value sv 'var-numbers) all-var-numbers))))
                 groups))))

  stack-vars)

;; Unify all <stack-variable> instances that represent the same logical variable
;; (i.e., their var-number sets overlap), so downstream passes (like DCE) that
;; rely on EQ identity will see reads and definitions as the same object.
;; Removed attempted unify-stack-variables implementation — replaced with
;; a var-number keyed DCE read-tracking to avoid identity mismatches.

(defun stack-variable-is-live-p (stack-var blocks)
  "Returns T if STACK-VAR has at least one non-dead assignment in BLOCKS."
  (dolist (block blocks)
    (dolist (insn (slot-value block 'code))
      (when (and (typep insn 'ir-assign)
                 (eq (slot-value insn 'lvalue) stack-var)
                 (not (slot-value insn 'dead-p)))
        (return-from stack-variable-is-live-p t))))
  nil)

(defun eliminate-dead-stack-assignments (blocks)
  "Remove assignments to stack variables that are never read.
   Only removes assignments where rvalue is a literal or another stack var (no side effects).
   Returns T if any assignments were removed."
  (labels ((sv-key (sv)
             (let* ((nums (slot-value sv 'var-numbers))
                    (lst (if (listp nums) (copy-list nums) (list nums))))
               (format nil "~{~A~^,~}" (sort lst #'<)))))
    (let ((changed nil)
          ;; Key reads by var-number set to avoid EQ identity issues
          (read-keys (make-hash-table :test #'equal)))

    ;; First pass: collect ALL stack variables that are read (as rvalues)
    ;; IMPORTANT: Check specific types BEFORE generic ir-node!
    (labels ((collect-reads (ir)
               (cond
                 ((typep ir '<stack-variable>)
                  (when (and *debug-codegen*
                             (search "HashMap.resize" (fn-name *context*))
                             (search "s{5}" (format nil "~A" ir)))
                    (format t "; DCE: Recording read of ~A~%" ir))
                  (setf (gethash (sv-key ir) read-keys) t))
                 ((typep ir 'ir-assign)
                  ;; Check rvalue for reads
                  (when-let ((rval (slot-value ir 'rvalue)))
                    (when (and *debug-codegen*
                               (search "HashMap.resize" (fn-name *context*))
                               (typep rval '<stack-variable>)
                               (search "s{5}" (format nil "~A" rval)))
                      (format t "; DCE: IR-ASSIGN with lvalue=~A reads rvalue=~A~%"
                              (type-of (slot-value ir 'lvalue)) rval))
                    (collect-reads rval))
                  ;; Check lvalue - for ir-member, the objref is a READ
                  (when-let ((lval (slot-value ir 'lvalue)))
                    (when (and (typep lval 'ir-member)
                               (slot-boundp lval 'objref))
                      (collect-reads (slot-value lval 'objref)))))
                 ;; Explicit handling for array access patterns (before generic ir-node)
                 ((typep ir 'ir-xastore)
                  ;; Array stores read: arrayref, index, value
                  (when (slot-boundp ir 'arrayref)
                    (collect-reads (slot-value ir 'arrayref)))
                  (when (slot-boundp ir 'index)
                    (collect-reads (slot-value ir 'index)))
                  (when (slot-boundp ir 'value)
                    (collect-reads (slot-value ir 'value))))
                 ((typep ir 'ir-aaload)
                  ;; Array loads read: arrayref, index
                  (when (slot-boundp ir 'arrayref)
                    (collect-reads (slot-value ir 'arrayref)))
                  (when (slot-boundp ir 'index)
                    (collect-reads (slot-value ir 'index))))
                 ;; Explicit handling for method calls (before generic ir-node)
                 ((typep ir 'ir-call-special-method)
                  ;; Special methods (constructors, super): read all args including objref
                  (when (slot-boundp ir 'args)
                    (dolist (arg (slot-value ir 'args))
                      (collect-reads arg))))
                 ((typep ir 'ir-call-virtual-method)
                  ;; Virtual methods (includes static as subclass): args[0] is objref for virtual
                  (when (slot-boundp ir 'args)
                    (dolist (arg (slot-value ir 'args))
                      (collect-reads arg))))
                 ;; Generic fallback for other ir-node types (MUST BE LAST)
                 ((typep ir 'ir-node)
                  (dolist (slot-def (closer-mop:class-slots (class-of ir)))
                    (let ((slot-name (closer-mop:slot-definition-name slot-def)))
                      (when (and (slot-boundp ir slot-name)
                                 (not (eq slot-name 'address))
                                 (not (eq slot-name 'dead-p)))  ; Skip dead-p slot
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

    ;; Second pass: mark dead assignments (skip already-dead instructions)
    (dolist (block blocks)
      (dolist (insn (slot-value block 'code))
        (when (and (typep insn 'ir-assign)
                   (typep (slot-value insn 'lvalue) '<stack-variable>)
                   (not (slot-value insn 'dead-p)))  ; Don't re-process dead instructions
          (let* ((stack-var (slot-value insn 'lvalue))
                 (rvalue (slot-value insn 'rvalue))
                 ;; Only safe to remove if rvalue has no side effects
                 ;; IMPORTANT: Don't eliminate assignments from local vars, as propagation
                 ;; may have already substituted uses of the stack var, making it appear unused.
                 (safe-rvalue? (or (typep rvalue 'ir-literal)
                                  (typep rvalue '<stack-variable>))))
            (when (and safe-rvalue?
                      (not (gethash (sv-key stack-var) read-keys)))
              (when *debug-codegen*
                (format t "; DCE: Marking dead assignment to ~A (rvalue: ~A ~S, block: ~A)~%"
                        stack-var
                        (type-of rvalue)
                        rvalue
                        (id block)))
              (setf (slot-value insn 'dead-p) t)
              (setf changed t))))))
    changed)))

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
  (declare (ignore def-insn ir-code))
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
