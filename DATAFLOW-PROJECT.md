# Dataflow Project: Constant and Copy Propagation

This document outlines a staged plan to implement robust constant- and copy-
propagation in OpenLDK, building on the existing IR and compilation pipeline.
It focuses on correctness first, incremental delivery, and alignment with the
current architecture and conventions.

## Goals

- Propagate constants and pure copies to reduce temporary stack variables and
  improve readability of generated Lisp.
- Safely propagate through locals within a basic block (no control flow inside).
- Prepare the groundwork for inter-block propagation via classic dataflow
  analysis, without changing method semantics.

## Non‑Goals (Initial Phases)

- No inter-procedural analysis.
- No speculative reordering or CSE beyond copy/constant propagation.
- No SSA conversion in the first phases (may be a later milestone).

## Current State (Baseline)

- IR is linearized from bytecode; stack variables (`<stack-variable>`) are SSA-
  like and immutable after creation.
- `build-def-use-chains` exists and powers a copy propagation pass that:
  - Runs on flat IR (pre–basic block construction).
  - Propagates only from `<stack-variable>` (SSA) and `ir-literal` definitions.
  - Never propagates locals (`ir-local-variable`, `ir-long-local-variable`).
  - Dead-code elimination is not performed here (conservative).
- Basic blocks and dominance are computed after the current propagation.

## Phase Status

- Phase 1 COMPLETE (commit 2f45281): corrected copy propagation so it never
  propagates locals and only propagates literals and SSA stack variables.
  This removed the root cause of earlier bootstrap regressions.
  - Behavior now:
    - OK: `s{n} = literal`, `s{n} = s{m}` (propagate)
    - NOT OK: `s{n} = local-k` (do not propagate)
    - NOT DONE: dead-code removal within this pass (to avoid surprises)
  - Pipeline unchanged: pass remains on flat IR prior to block building.

## Definitions and Invariants

- Stack variables: SSA (immutable after creation). Always safe to propagate when
  the rvalue is side-effect‑free.
- Local variables: Mutable; correctness requires checking that no intervening
  assignment occurs between definition and each use. Safe propagation possible
  within a basic block by linear scan.
- Side effects: Determined by `side-effect-p`. Be conservative — if unsure,
  do not propagate.

## Phase 1: Strengthen Constant Propagation (Flat IR)

Objective
- Ensure all literal expressions are propagated as far as possible where safe.

Actions
- Review `can-propagate-p` heuristics to explicitly treat all `ir-literal` and
  `<stack-variable>` rvalues as propagatable when side-effect‑free.
- Keep this pass on flat IR (pre-block) as today.

Output
- No pipeline changes; just improved coverage for literals already present.

## Phase 2: Intra‑Block Propagation for Locals

Objective
- Enable constant/copy propagation that involves `ir-local-variable` within
  a single basic block, ensuring no intervening writes occur.

Key Idea
- Basic blocks are linear, single‑entry segments: instructions execute in order
  and can be scanned linearly to validate safety.

Algorithm
- After building basic blocks:
  - For each block, run a localized propagation pass over `(code block)`.
  - Build def/use tables for the block only (reuse `build-def-use-chains`).
  - Candidate assignments are `ir-assign` to `<stack-variable>` whose rvalue is
    side-effect‑free and is either a literal, `<stack-variable>`, or
    `ir-local-variable`/`ir-long-local-variable` when safe.
  - For locals: for every use site, check for intervening assignments to the same
    local between the defining assignment and that use.

Intervening Assignment Check (pseudo‑code)
```
(defun has-intervening-assignment-p (local-var def-insn use-insn block-code)
  (let ((idx (slot-value local-var 'index))
        (between nil))
    (dolist (insn block-code)
      (cond ((eq insn def-insn) (setf between t))
            ((eq insn use-insn) (return-from has-intervening-assignment-p nil))
            ((and between (typep insn 'ir-assign)
                  (or (typep (lvalue insn) 'ir-local-variable)
                      (typep (lvalue insn) 'ir-long-local-variable))
                  (= (slot-value (lvalue insn) 'index) idx))
             (return-from has-intervening-assignment-p t))))
    t))
```

Integration Points
- Add a per‑block propagation pass that allows locals when proven safe.
- Optionally guard with a small feature flag during bake‑in.
- Keep `side-effect-p` as the safety gate for expressions.

Pipeline Changes
- `%compile-method` order becomes:
  1) Build flat IR
  2) Merge stacks + fix stack variables
  3) Flat IR propagation (stack vars + literals)
  4) Array initialization folding (fixpoint)
  5) Build basic blocks
  6) New: Per‑block propagation with locals enabled
  7) Codegen blocks

Testing
- Add micro tests where locals are copied/used within the same block with and
  without intervening assignments, including long/double (2‑slot) locals.
- Include negative tests that ensure locals are not propagated when an
  intervening assignment exists.

Risks/Mitigations
- False positives in intervening check: conservative default returns true
  (i.e., do not propagate) if structure is unexpected.
- Side effects via `ir-static-member`: remain guarded by `ir-clinit` emission;
  treat the read as pure in the propagation step (status quo).

## Phase 3: Inter‑Block Copy/Constant Propagation (Reaching Definitions)

Objective
- Safely propagate through locals across basic blocks by computing which local
  definitions reach each use site.

Approach
- Classic forward dataflow (reaching definitions) per local index:
  - Lattice element: mapping `local-index -> set-of-defs` with TOP/BOT.
  - Transfer: kill on write to local index; gen on defining assignment.
  - Meet: union over predecessors.
- For each use, if the reaching set is a singleton and the rvalue is
  side-effect‑free and compatible, substitute; otherwise skip.

Data Structures
- Identify definitions as specific `ir-assign` sites to locals (or stack‑var
  definitions that copy from locals), tracked by instruction identity.
- Use existing `<basic-block>`, `predecessors`, `successors`, and `dominators`.

Integration
- Run after block construction and before codegen, following Phase 2.
- Keep propagation local to the current method.

Testing
- Craft CFGs with branches/merges; confirm propagation only when unique
  reaching def exists; ensure correctness with try/catch edges (handlers are
  predecessors in CFG — already propagated by `propagate-try-blocks`).

Risks
- Complexity and compile‑time cost: mitigate with caching and early bailouts for
  large methods.
- Exception edges: already represented in CFG; ensure dataflow includes them.

## Phase 4 (Optional): SSA for Locals

Objective
- Convert mutable locals to SSA with phi functions at merge points to simplify
  propagation and enable additional optimizations downstream.

Notes
- Requires introducing phi nodes (new IR class), renaming, and insertion at
  merge points; larger change with higher payoff. Consider as a separate track
  once Phase 3 is stable.

## Validation and Tooling

- Instrument propagation with optional debug output controlled by a new debug
  flag (e.g., extend `LDK_DEBUG=u` to dump propagation decisions or add a new
  letter).
- Keep asserts conservative (e.g., stack depth checks are already present in
  `merge-stacks`).
- Use existing `make check` and targeted tests; add unit‑style snippets where
  helpful.

## Milestones

- M1: Strengthen literal/stack propagation (Phase 1). COMPLETE.
- M2: Per‑block local propagation (Phase 2) behind a small feature flag during
  bake‑in; default on after validation.
- M3: Reaching definitions across blocks (Phase 3). Implement and ship.
- M4: SSA exploration (Phase 4) — design doc + prototype.

## Performance Expectations

- Minor improvements in generated Lisp clarity and potential SBCL optimization.
- Compile time increase for Phase 3; acceptable for correctness; revisit if
  hot methods regress.

## Rollout/Guardrails

- Consider a temporary env knob (e.g., `LDK_PROP_LOCALS=1`) for Phase 2 during
  bake‑in; remove once stable.
- Ensure `ocicl lint` remains clean and `make check` stays green at each step.
