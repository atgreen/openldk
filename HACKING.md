# OpenLDK Hacking Guide

## Critical Architecture Knowledge

### JIT Compilation Flow

1. **Class Loading** (`classload` in src/openldk.lisp)
   - Parses Java class file
   - Creates CLOS class definition with method stubs
   - Stubs trigger JIT compilation on first invocation

2. **Bytecode → IR Translation** (src/bc-to-ir.lisp)
   - Converts Java bytecode to intermediate representation
   - Builds stack state tracking for type inference
   - Creates IR instructions (ir-assign, ir-goto, etc.)

3. **Control Flow Analysis** (src/basic-block.lisp)
   - Splits bytecode into basic blocks
   - Computes dominance relationships
   - Handles exception handlers as control flow

4. **Code Generation** (src/codegen.lisp)
   - Converts IR to Lisp S-expressions
   - Generates CLOS method definitions
   - Emits tagbody/go for control flow

5. **Evaluation** (`%eval` in src/openldk.lisp)
   - Compiles generated Lisp to native code via SBCL
   - Replaces method stub with compiled version

### Static Class Instances

Java static members are handled via special CLOS instances:

```lisp
;; Created during class load (line 429 of src/openldk.lisp):
(defparameter |+static-java/lang/System+|
  (make-instance '|java/lang/System|))
```

This allows static fields to be accessed like:
```lisp
(slot-value |+static-java/lang/System+| '|props|)
```

**Important**: These are created at class load time, so direct references in code
cause compile-time undefined variable warnings, but work correctly at runtime.

### Stack Variable Merging

Stack merging relies on **side effects** (src/openldk.lisp lines 260-263):

```lisp
;; Discards return value, but side effects are the important part
(maphash (lambda (k v)
           (when (> (length v) 1)
             (reduce #'merge-stacks v)))
         (stack-state-table *context*))
```

The `merge-stacks` function (and `merge-stack-variables` it calls) **mutate**
the `var-numbers` slot of stack-variable objects. Since these objects are
shared across all references in the IR code, the mutations propagate everywhere.

This is correct but subtle - the return value of `reduce` is discarded because
the real work happens via mutation of shared objects.

### Exception Handling

Exception handlers are integrated into control flow:
- Exception table entries create basic block boundaries
- Handler PC becomes a basic block start
- Exception object pushed onto stack as ir-condition-exception
- Lisp HANDLER-CASE wraps blocks with exception handlers

### Type Checking Pitfalls

**Use `typep` not `type-of` for type checks:**

```lisp
;; FRAGILE - breaks with class redefinition
(eq (type-of x) 'ir-goto)

;; ROBUST - works correctly
(typep x 'ir-goto)
```

**Use `eql` or `=` for number comparison, not `eq`:**

```lisp
;; WRONG - fails for bignums
(eq address1 address2)

;; CORRECT
(eql address1 address2)
;; or
(= address1 address2)
```

### Debug Flags (LDK_DEBUG environment variable)

- `b` - trace bytecode compilation
- `c` - dump all Lisp code prior to evaluation
- `t` - trace method entry/exit at runtime
- `T` - trace method entry/exit with arguments and return values
- `s` - start slynk server at startup (port 2025)
- `u` - unmuffle the Lisp compiler
- `x` - trace opcode execution (use with `t`)

## Reference Sources

- **JDK 8 Source**: `~/git/jdk` - Use this to understand correct Java semantics
- **Java VM Spec**: Essential for bytecode instruction behavior
- **Java Language Spec**: For language-level semantics

## Common Pitfall Patterns

### 1. Unequal Stack Lengths in merge-stacks

The `merge-stacks` function (src/bc-to-ir.lisp:1943) assumes both stacks have
equal length. If control flow paths produce different stack depths, the merge
silently truncates to the shorter stack, losing information.

**Solution**: Add assertion or padding logic to handle unequal lengths.

### 2. Missing Stack State Recording

The bytecode interpretation loop skips stack recording for GOTO instructions
(line 225). Ensure this is intentional and doesn't break stack merging at
jump targets.

### 3. Side-Effect Detection

The `side-effect-p` function determines if an IR node can be eliminated.
Be conservative - marking something without side effects when it has them
can break program semantics.

## Testing Strategy

1. **Run specific test**: `make check RUNTESTFLAGS="Hello.exp"`
2. **Check expected failures**: `testsuite/expected-failures.txt`
3. **Enable debug output**: `LDK_DEBUG=bctux ./openldk ClassName`
4. **Use slynk for live debugging**: `LDK_DEBUG=s ./openldk ClassName`
   - Connect Emacs/Sly to port 2025
   - Set breakpoints in generated code

## Build System Notes

- **SBCL only**: Uses SBCL-specific features (sb-ext, sb-debug)
- **Java 8 only**: Class file parser supports up to version 52.0
- **JAVA_HOME required**: Must point to Java 8 JRE for boot classpath
- **ocicl**: Package manager - run `ocicl install` before building

## Code Organization

Files are loaded in specific order (see openldk.asd):
1. package.lisp - Package and symbol imports
2. global-state.lisp - Runtime globals
3. debug.lisp - Debug infrastructure
4. monitor.lisp - Threading primitives
5. context.lisp - Execution context
6. bootstrap.lisp - Runtime initialization
7. opcodes.lisp - Bytecode definitions
8. ir.lisp - IR node definitions
9. bc-to-ir.lisp - Bytecode translation
10. basic-block.lisp - Control flow graph
11. codegen.lisp - Code generation
12. classfile.lisp - Class file parsing
13. native.lisp - Native methods
14. reflection.lisp - Reflection API
15. openldk.lisp - Main runtime

**Order matters** - later files depend on earlier definitions.

## Performance Considerations

- **Incremental JIT**: Methods compiled on first call, not class load
- **No optimization**: Code prioritizes correctness over speed
- **Method caching**: Compiled methods replace stubs permanently
- **No inlining**: Each Java method is a separate Lisp function

OpenLDK is not designed for high performance - it's for embedding Java
libraries in Lisp applications when you need "that one Java library."
