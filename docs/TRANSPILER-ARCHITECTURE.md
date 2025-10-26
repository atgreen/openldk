# OpenLDK Transpiler Architecture

This document describes the architecture of OpenLDK's Java bytecode to Common Lisp transpiler, based on examination of the implementation.

## Overview

OpenLDK transpiles Java bytecode into Common Lisp code, which is then compiled to native code by SBCL. The transpiler operates on individual methods, transforming them through multiple intermediate representations with optimization passes.

## Compilation Pipeline

The compilation of a Java method follows this pipeline (from `%compile-method` in `src/openldk.lisp`):

```
Java Bytecode → IR (Intermediate Representation) → Basic Blocks → Lisp Code → Native Code
                     ↓                               ↓
              Optimization Passes            Per-Block Codegen
```

### Phase 1: Bytecode to IR Translation

**Location**: `src/bc-to-ir.lisp`

**Input**: Raw Java bytecode (byte array from `.class` file)

**Output**: Flat list of IR instruction objects

**Process**:
- Each bytecode instruction is transpiled by a function in the `*opcodes*` array
- Bytecode transpilers are defined using `define-bytecode-transpiler` macro
- The transpiler maintains a simulated JVM stack during translation
- Stack variables are created to represent JVM operand stack values
- Local variables are represented as `ir-local-variable` or `ir-long-local-variable`

**Key Data Structures**:
- `*context*`: Compilation context containing:
  - `bytecode`: Original bytecode array
  - `stack`: Simulated JVM operand stack (list of stack variables)
  - `pc`: Program counter (current bytecode position)
  - `stack-state-table`: Maps PC → stack state for control flow merge points
  - `insn-size`: Array mapping PC → instruction size
  - `next-insn-list`: Array mapping PC → list of successor PCs

**Example**: The bytecode sequence for `iload_0; iload_1; iadd` becomes:
```lisp
(ir-assign :lvalue s{1} :rvalue (ir-local-variable :index 0))
(ir-assign :lvalue s{2} :rvalue (ir-local-variable :index 1))
(ir-assign :lvalue s{3} :rvalue (ir-iadd :value1 s{1} :value2 s{2}))
```

### Phase 2: Stack Analysis and Merging

**Location**: Lines 533-547 in `src/openldk.lisp`

**Purpose**: Handle control flow merge points where multiple paths converge

**Process**:
1. For each PC with multiple predecessors (found in `stack-state-table`)
2. Call `merge-stacks` to unify stack variables from different paths
3. **Important side effect**: Mutates the `var-numbers` slot of stack variables
4. Stack variables that represent the "same" value from different paths get merged

**Example**: If two paths reach PC 100, one with `s{5}` and one with `s{8}` in the same stack slot:
- After merging, both variables point to the same object with `var-numbers = (5 8)`
- This variable will be named `s{5,8}` in generated code

### Phase 3: Fix Stack Variables

**Location**: Line 547 in `src/openldk.lisp`, implemented in `fix-stack-variables`

**Purpose**: Finalize stack variable naming after merging

**Process**: After `merge-stacks` mutations, ensures all references are consistent

### Phase 4: Copy Propagation (Optimization Pass)

**Location**: Lines 205-256 in `src/openldk.lisp`

**Current Status**: Only propagates stack variables and literals, NOT local variables

**Process**:
1. **Build def-use chains** (`build-def-use-chains`):
   - `def-table`: Maps each variable → the IR instruction that defines it
   - `use-list-table`: Maps each variable → list of IR instructions that use it
   - `use-def-table`: Maps each IR instruction → variables it uses

2. **Identify propagatable assignments**:
   - Check if variable has single static assignment (SSA property)
   - Check if rvalue has no side effects
   - Check usage patterns (dead code, single use, or pure copy)
   - Add to `single-assignment-table` if safe to propagate

3. **Substitute and remove** (`substitute-in-ir`):
   - Replace all uses of the variable with the rvalue expression
   - Turn the assignment into `ir-nop`

**Current Limitations**:
- Does NOT propagate local variables (lines 227, 231 were removed)
- Operates on flat IR code, not respecting basic block boundaries
- No inter-block analysis

### Phase 5: Array Initialization (Optimization Pass)

**Location**: Lines 549-555 in `src/openldk.lisp`, implemented in `initialize-arrays`

**Purpose**: Detect array initialization patterns and optimize them

**Process**: Iterates until no more changes (fixpoint)

### Phase 6: Build Basic Blocks

**Location**: Line 557 in `src/openldk.lisp`, implemented in `src/basic-block.lisp`

**Input**: Flat IR code with optimizations applied

**Output**: List of `<basic-block>` objects forming a control flow graph

**Basic Block Structure**:
```lisp
(defclass/std <basic-block> ()
  ((id)                      ; Unique ID
   (code)                    ; List of IR instructions (linear, no internal branches)
   (address)                 ; Bytecode address of first instruction
   (predecessors)            ; Set of incoming blocks (fset:set)
   (successors)              ; Set of outgoing blocks (fset:set)
   (dominators)              ; Set of blocks that dominate this one
   (fall-through-address)    ; Natural successor address
   (try-catch)               ; Exception handler info
   ...))
```

**Key Properties**:
- Each block contains LINEAR code (no internal branches/jumps)
- Blocks end at:
  - Branch instructions (goto, if, switch)
  - Return instructions
  - Throw instructions
  - Exception handler entry points
- Each block has one entry point (first instruction)
- Each block has one or more exit points (successors)

**Algorithm** (from `build-basic-blocks` in `basic-block.lisp`):
1. Scan IR code to find branch targets (addresses that are jumped to)
2. Split IR code at branch targets and branch instructions
3. Create `<basic-block>` for each segment
4. Build predecessor/successor relationships
5. Compute dominators for optimization

### Phase 7: Code Generation

**Location**: Lines 558-566 in `src/openldk.lisp`, implemented in `src/codegen.lisp`

**Input**: List of basic blocks

**Output**: Lisp code (s-expression)

**Process**:
1. For each basic block, call `codegen-block` which:
   - Generates a label for the block address
   - Calls `codegen` method on each IR instruction
   - Returns a list of Lisp forms

2. `codegen` is a generic function with methods for each IR class:
   ```lisp
   (defmethod codegen ((insn ir-iadd) context)
     (make-instance '<expression>
       :code `(+ ,(code (codegen (value1 insn) context))
                 ,(code (codegen (value2 insn) context)))
       :expression-type :INT))
   ```

3. Wraps in `tagbody` for labels/gotos

**Output Format**:
```lisp
(block nil
  (tagbody
    0        ; Label for bytecode address 0
    (setf s{1} local-0)
    (setf s{2} local-1)
    (setf s{3} (+ s{1} s{2}))
    (return-from nil s{3})))
```

### Phase 8: Wrapping and Final Assembly

**Location**: Lines 567-650 in `src/openldk.lisp`

**Process**:
1. Add debug tracing (if `*debug-trace*` enabled)
2. Add exception handlers for array bounds checking
3. Build method signature:
   - Static methods → `defun`
   - Instance methods → `defmethod` with `this` parameter
4. Generate `let` bindings for:
   - Stack variables (e.g., `s{1}`, `s{5,8}`)
   - Local variables (e.g., `local-0`, `local-1`)
5. Combine into final method definition

**Final Output** (example):
```lisp
(defun |Foo.bar(II)I| (arg0 arg1)
  (let ((s{1}) (s{2}) (s{3})
        (local-0 arg0)
        (local-1 arg1))
    (block nil
      (tagbody
        0
        (setf s{1} local-0)
        (setf s{2} local-1)
        (setf s{3} (+ s{1} s{2}))
        (return-from nil s{3})))))
```

## IR Instruction Classes

**Location**: `src/ir.lisp`

All IR instructions inherit from `ir-node`:
```lisp
(defclass/std ir-node ()
  ((address :std -1)))  ; Bytecode PC address
```

### Categories:

1. **Literals**: `ir-int-literal`, `ir-string-literal`, etc.
   - Have `value` and `type` slots
   - Side-effect free

2. **Variables**:
   - `ir-local-variable` - Java local variable (mutable)
   - `ir-long-local-variable` - Java long/double local (occupies 2 slots)
   - `<stack-variable>` - JVM operand stack slot (SSA, immutable after creation)

3. **Assignments**: `ir-assign`
   - `lvalue` - Variable being assigned to
   - `rvalue` - Expression being assigned

4. **Binary Operations**: `ir-iadd`, `ir-imul`, `ir-isub`, etc.
   - `value1`, `value2` slots
   - No side effects

5. **Unary Operations**: `ir-i2l`, `ir-ineg`, etc.
   - `value` slot

6. **Array Operations**: `ir-iaload`, `ir-iastore`, etc.
   - `arrayref`, `index`, potentially `value`

7. **Control Flow**:
   - `ir-goto` - Unconditional jump
   - `ir-if<cond>` - Conditional branch (ifeq, ifne, etc.)
   - `ir-if-xcmp<cond>` - Comparison branch (if_icmpeq, etc.)
   - `ir-return`, `ir-return-value`

8. **Method Calls**:
   - `ir-call-static-method`
   - `ir-call-virtual-method`
   - `ir-call-special-method`
   - All have `args` slot

9. **Object Operations**:
   - `ir-new` - Object creation
   - `ir-checkcast` - Type cast
   - `ir-instanceof`

## Stack Variables vs Local Variables

This is a critical distinction:

### Stack Variables (`<stack-variable>`)

- Represent JVM operand stack slots
- Follow **SSA (Static Single Assignment)** semantics
- Once created, never reassigned (immutable)
- Name format: `s{1}` or `s{5,8}` (merged variables)
- Created during bytecode→IR translation
- Safe to propagate (already in SSA form)

### Local Variables (`ir-local-variable`)

- Represent Java local variable slots (JVM local variable array)
- **Mutable** - can be reassigned multiple times
- Name format: `local-0`, `local-1`, etc.
- Map to Java source variables and method parameters
- **NOT safe to propagate without dataflow analysis**
  - Example: `local-0` might be assigned in multiple places
  - Propagating first assignment would be incorrect

**Example**:
```java
int foo(int x) {
    int y = x + 1;  // local-0=x (param), local-1=y (first assignment)
    if (x > 0) {
        y = x * 2;  // local-1 (second assignment)
    }
    return y;       // Which value of local-1?
}
```

Bytecode translation creates:
```lisp
(ir-assign s{1} (ir-local-variable 0))  ; Load x
(ir-assign s{2} (ir-iadd s{1} 1))
(ir-assign local-1 s{2})                ; Store to y
; ... conditional code ...
(ir-assign s{10} (ir-local-variable 0)) ; Load x again
(ir-assign s{11} (ir-imul s{10} 2))
(ir-assign local-1 s{11})               ; Store to y again
; ...
(ir-assign s{20} (ir-local-variable 1)) ; Load y
(ir-return-value s{20})
```

**Current problem**: Can't propagate `local-1` because it has multiple assignments.

## Copy Propagation: Current State and Requirements

### What Works Today

✅ Stack variable propagation (SSA variables)
✅ Literal propagation
✅ Dead code detection (unused variables)
✅ Single-use optimization

### What Doesn't Work

❌ Local variable propagation
❌ Intra-block local variable propagation
❌ Inter-block propagation

### Why Local Variable Propagation Was Disabled

Lines 227 and 231 in `can-propagate-p` previously allowed propagation of `ir-local-variable`:

```lisp
;; OLD CODE (INCORRECT):
(typep rvalue 'ir-local-variable)  ; Line 227, 231
```

**Problem**: This propagates ALL local variable copies, even when the local is reassigned:

```lisp
;; IR code:
(ir-assign s{1} (ir-local-variable 0))  ; s{1} = local-0
; ... intervening code that assigns to local-0 ...
(ir-assign local-0 some-value)          ; local-0 reassigned!
; ... later ...
(use s{1})                              ; Still using old value - CORRECT
```

If we propagate `s{1} → local-0`, we get:
```lisp
; ... intervening code ...
(ir-assign local-0 some-value)
; ... later ...
(use local-0)  ; WRONG! Should use old value
```

### Requirements for Correct Local Variable Propagation

To safely propagate local variables, we need:

1. **Intra-block propagation** (within a single basic block):
   - ✅ No control flow within block (linear code)
   - ✅ Can check for intervening assignments using linear scan
   - ⚠️ Must check EVERY use site for intervening assignments

2. **Inter-block propagation** (across basic blocks):
   - ❌ Requires dataflow analysis
   - ❌ Must compute reaching definitions
   - ❌ Must handle phi nodes at merge points
   - ❌ Much more complex

### Implementation Plan for Intra-Block Local Variable Propagation

Based on the architecture, here's how to implement safe intra-block propagation:

#### Step 1: Add Intervening Assignment Check

```lisp
(defun has-intervening-assignment-p (local-var def-insn use-insn block-code)
  "Check if LOCAL-VAR is assigned between DEF-INSN and USE-INSN within BLOCK-CODE."
  (let ((local-index (slot-value local-var 'index))
        (between-def-and-use nil))
    (dolist (insn block-code)
      (cond
        ;; Found definition - start checking
        ((eq insn def-insn)
         (setf between-def-and-use t))
        ;; Found use - stop checking, no intervening assignment
        ((eq insn use-insn)
         (return-from has-intervening-assignment-p nil))
        ;; Between def and use - check for assignment to same local
        ((and between-def-and-use
              (typep insn 'ir-assign)
              (or (typep (lvalue insn) 'ir-local-variable)
                  (typep (lvalue insn) 'ir-long-local-variable))
              (= (slot-value (lvalue insn) 'index) local-index))
         (return-from has-intervening-assignment-p t))))
    ;; Didn't find use - something is wrong, be conservative
    t))
```

#### Step 2: Modify Copy Propagation to Run Per-Block

Currently propagation runs on flat IR (line 548). Should run per-block AFTER blocks are built (line 557):

```lisp
;; CURRENT (line 548):
(setf code (propagate-copies code (single-assignment-table *context*)))

;; PROPOSED (after line 557):
(blocks (build-basic-blocks ir-code-0))

;; NEW: Per-block propagation with local variables
(dolist (block blocks)
  (let ((block-table (make-hash-table :test 'eq)))
    (setf (code block)
          (propagate-copies (code block) block-table :allow-locals t))))
```

#### Step 3: Update `can-propagate-p` and `propagate-copies`

Add `:allow-locals` keyword parameter and intervening assignment checking:

```lisp
(defun propagate-copies (ir-code single-assignment-table &key allow-locals)
  ;; Build def-use chains for this block's code
  (multiple-value-bind (def-table use-list-table use-def-table)
      (build-def-use-chains ir-code)
    (declare (ignore use-def-table))

    ;; Identify propagatable assignments
    (maphash (lambda (var def-insn)
               (when (typep def-insn 'ir-assign)
                 (let ((rvalue (slot-value def-insn 'rvalue)))
                   ;; Check if basic propagation criteria met
                   (when (can-propagate-p var rvalue def-insn use-list-table ir-code
                                         :allow-locals allow-locals)
                     ;; Additional check for local variables
                     (let ((safe-to-propagate t))
                       (when (and allow-locals
                                  (or (typep rvalue 'ir-local-variable)
                                      (typep rvalue 'ir-long-local-variable)))
                         ;; Check each use site for intervening assignments
                         (dolist (use-insn (gethash var use-list-table))
                           (when (has-intervening-assignment-p rvalue def-insn use-insn ir-code)
                             (setf safe-to-propagate nil)
                             (return))))
                       (when safe-to-propagate
                         (setf (gethash var single-assignment-table) rvalue)))))))
             def-table)

    ;; Substitute and remove
    (mapcar (lambda (insn)
              (let ((new-insn (substitute-in-ir insn single-assignment-table)))
                (if (and (typep new-insn 'ir-assign)
                         (gethash (lvalue new-insn) single-assignment-table))
                    (make-instance 'ir-nop :address (address new-insn))
                    new-insn)))
            ir-code)))
```

### Why This Approach is Safe

1. **Basic blocks are linear** - No internal branches means:
   - Instructions execute in order
   - Linear scan of block code is sufficient
   - No need for complex control flow analysis

2. **Intervening assignment check is precise**:
   - Uses `eq` to compare instruction objects (identity, not equality)
   - Scans linearly from def to use
   - Conservative (returns `t` if anything is unclear)

3. **Separate table per block**:
   - No cross-block propagation
   - Each block is analyzed independently
   - Avoids inter-procedural complexity

### What This Gives Us

**Before** (no local propagation):
```lisp
(let ((s{1}) (s{2}) (s{3}) (local-0 arg0) (local-1 arg1))
  (block nil
    (tagbody
      0
      (setf s{1} local-0)     ; Copy from local-0
      (setf s{2} local-1)     ; Copy from local-1
      (setf s{3} (+ s{1} s{2}))
      (return-from nil s{3}))))
```

**After** (with intra-block local propagation):
```lisp
(let ((s{3}) (local-0 arg0) (local-1 arg1))
  (block nil
    (tagbody
      0
      ; s{1} and s{2} eliminated
      (setf s{3} (+ local-0 local-1))  ; Direct use of locals
      (return-from nil s{3}))))
```

**Benefits**:
- Fewer stack variables declared
- More readable generated code
- Potentially better SBCL optimization
- Reduced stack frame size

### Future Work: Inter-Block Propagation

For inter-block propagation, would need:

1. **Reaching definitions analysis**:
   - For each block, compute which definitions reach the entry
   - Classic dataflow problem

2. **Dominator tree computation**:
   - Already computed (see `dominators` slot)
   - Can use for optimization

3. **SSA transformation for locals**:
   - Convert mutable locals to SSA form
   - Insert phi nodes at merge points
   - Then propagation becomes trivial (all variables are SSA)

This is significantly more complex and should be a separate project.

## Key Insights for Future Developers

1. **Stack variables are SSA, locals are not**
   - This is the fundamental distinction
   - Don't treat them the same in optimization passes

2. **Basic blocks are linear**
   - No internal control flow
   - Safe to do linear scans within a block
   - Cross-block analysis requires dataflow

3. **Instruction identity matters**
   - Use `eq` to compare instructions, not `equal`
   - The same instruction object can appear in multiple data structures

4. **Side effects matter**
   - `side-effect-p` method determines if an IR node can be moved/eliminated
   - Must be conservative (better to not optimize than break semantics)

5. **The pipeline is stateful**
   - `*context*` holds compilation state
   - Stack merging has side effects (mutates `var-numbers`)
   - Optimization passes must maintain invariants

6. **Exception handling affects control flow**
   - Exception handlers create implicit edges in CFG
   - Must be considered in dataflow analysis
   - See `try-catch` slot in basic blocks

## Debugging Tips

1. **Enable debug flags** (`LDK_DEBUG` environment variable):
   - `b`: Trace bytecode compilation
   - `c`: Dump all Lisp code before evaluation
   - `t`: Trace method entry/exit
   - `x`: Trace opcode execution

2. **Examine IR**:
   - Uncomment line 556: `(sdfdfd (print ir-code-0))`
   - Shows IR before basic block construction

3. **Check def-use chains**:
   - Add debug output in `build-def-use-chains`
   - Verify variables are being tracked correctly

4. **Test with simple methods**:
   - Start with straight-line code (no branches)
   - Add complexity incrementally
   - Compare generated Lisp with expected output

## References

- `src/openldk.lisp` - Main compilation pipeline
- `src/bc-to-ir.lisp` - Bytecode transpilers
- `src/ir.lisp` - IR instruction definitions
- `src/basic-block.lisp` - Control flow graph construction
- `src/codegen.lisp` - IR to Lisp code generation
- `src/context.lisp` - Compilation context
