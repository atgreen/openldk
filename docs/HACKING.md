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

### Primitive Storage and Entry Guards

`%method-let` emits a guarded branch when a method has primitive parameters.
Canonical signed integers and single/double floats enter `%typed-let` with
parameter type information. Other Lisp values use the generic branch. Keep
this fallback: native implementations can place Lisp objects in nominally
primitive Java fields and pass them through Java methods.

`src/primitive-code.lisp` lowers known long locals to unsigned 64-bit storage.
Modular arithmetic uses `ldb` so SBCL can keep intermediate values unboxed.
Signed uses, including comparisons, division, right shifts, conversions and
Java/Lisp boundaries, decode the word with `%signed-long`. An opaque producer
still runs through the primitive normalizer; do not remove that conversion
without proving its input is a canonical integer. Preserve operand evaluation
order when introducing word conversions, including on the exceptional path.

The differential `PrimitiveOptimizationProbe` checks these boundaries against
JDK 25. For a generated-code allocation benchmark, run from the repository root:

```sh
CL_SOURCE_REGISTRY="$PWD//:" LDK_CODE_CACHE=off \
  sbcl --dynamic-space-size 8192 --script bench/primitive-code/run.lisp
```

This compiles identical Java loops with primitive entry guards, word storage
and result arithmetic disabled and enabled, checks boundary cases and checksums,
and reports time and allocated bytes for ten million iterations. It asserts that the optimized loop does not
allocate per iteration. Timing is informational and depends on the host.

`src/primitive-results.lisp` restores useful primitive types when opaque call
results enter arithmetic. It checks the actual operands before using typed
operations, preserving the original arithmetic as the fallback. Type facts
come from canonical entry guards, checked conversions and specialized arrays;
a native call's descriptor alone never becomes an unchecked Lisp declaration.
Both operands are evaluated in their original order before a new guard runs.
Known long operands use explicit unsigned-word modular operations.
Initializer and branch-test facts apply only to locals with no writes in the
generated form, including writes captured by nested functions. Reassigned
locals require fresh operand guards unless an explicit storage declaration
already constrains their representation. Unmodeled lexical binding forms
remain unchanged so outer facts cannot leak into shadowed variables.

Ordinary Lisp call boundaries can still box long and double return values.
The pass removes avoidable arithmetic allocation while keeping those public
calling conventions and opaque-value passthrough intact. To compare generated
callers of non-inlineable primitive methods with this pass off and on, run:

```sh
CL_SOURCE_REGISTRY="$PWD//:" LDK_CODE_CACHE=off \
  sbcl --dynamic-space-size 8192 --script bench/primitive-results/run.lisp
```

### Small Leaves and Guarded Array Loops

`src/inline-code.lisp` recognizes short, straight-line integer expressions
in static, private and final Java methods. Primitive argument guards and the current function
identity select the inline path; native replacements use the original call.
Receiver caches and method generations also guard instance inlining. Lisp
replacements and around methods use the original dispatch, while private
selection stays on the defining class. The original linkage, access and
class-initialization checks remain in place.
The caller owns the inline site and its statically resolved class dependency.
The process registry holds only weak function keys and weak method identities.

`src/array-loops.lisp` versions closed primitive reduction loops with an index
initialized to zero and incremented by one up to an array's length. Nonthrowing
guards check backing-vector types, a signed-32-bit bound and the lengths of all
accessed arrays. A successful guard hoists vectors and length, and proves each
loop-body index is in range. Only those array reads use local safety-zero code;
initializers, code after the loop and all unrecognized control flow retain
their checks. The complete original loop is the fallback, including for null
arrays, short secondary arrays and zero-iteration cases.

`InlineLeafProbe`, `InstanceInlineProbe` and `ArrayLoopProbe` compare boundary
values and exception timing with JDK 25. Interop tests also replace methods,
add subclass and around methods, and pass opaque Lisp values through integer
descriptors. Benchmark actual generated Java methods with these two
optimizations disabled and enabled using:

```sh
CL_SOURCE_REGISTRY="$PWD//:" LDK_CODE_CACHE=off \
  sbcl --dynamic-space-size 8192 --script bench/generated-code/run.lisp
```

### Virtual Call-Site Caches

Each generated virtual call keeps its last receiver class and effective
function. A hit bypasses the generic function's shared dispatch hash table.
Method addition or removal increments a generation; replacing the generic
function also invalidates the site. Around methods remain part of the cached
effective function, and complex dispatch uses the ordinary dispatcher.

Sites hold weak references to classes, generic functions and effective
functions so a caller in a permanent loader cannot retain a receiver's loader.
The concurrent replacement tests exercise a cache miss during the temporary
method-removal gap. `bench/virtual-calls/run.lisp` compares the site cache with
shared generic-function dispatch, checking results and allocation.

### Compiled Method-Handle Graphs

`src/native/lambda-compiler.lisp` compiles LambdaForm wiring into a Lisp
function with ordinary temporaries. Shared fixed-arity dispatchers call each
NamedFunction without building argument lists or unboxing lists. Mutable
call-site targets, resolved handles and method symbols are read at invocation
time, so Java call-site updates and Lisp function replacement remain visible.

The cache has weak form keys. Its compiled functions contain no Java object
constants: they receive the live form as an argument and read constants through
it. This matters because SBCL weak-key hash tables can retain a key when their
value points back to it. Capturing a form's constants could retain the form
and its defining loader indefinitely.

`MethodHandleGraphProbe` compares combinators, mutable targets, fields,
constructors, boxing and mixed primitive arguments with JDK 25. To compare
graph interpretation and compilation with matching checksums and allocation
measurements, run:

```sh
CL_SOURCE_REGISTRY="$PWD//:" LDK_CODE_CACHE=off \
  sbcl --dynamic-space-size 8192 --script bench/method-handles/run.lisp
```

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
- `e` - trace exceptions
- `l` - trace class loading
- `L` - trace class loading and compilation (includes `l`)
- `p` - trace data-flow propagation
- `s` - start slynk server at startup (port 2025)
- `t` - trace method entry/exit at runtime
- `T` - trace method entry/exit with arguments and return values
- `u` - unmuffle the Lisp compiler
- `x` - trace opcode execution (use with `t`)

### Debugging Techniques

#### Inspecting Generated Lisp Code

When debugging unexpected behavior, especially with comparisons or type issues,
**always examine the generated Lisp code** using `LDK_DEBUG=c`:

```bash
LDK_DEBUG=c ./openldk ClassName 2>&1 | grep -A 50 "compiling MethodName"
```

**Example - Float_2 LCMP Bug:**
Looking at generated code revealed that long comparison used `EQ` (object identity)
instead of `=` (numeric equality). This caused comparisons like
`computed_long == Long.MAX_VALUE` to fail because Common Lisp creates different
objects for the same large number (9223372036854775807).

```lisp
;; WRONG - uses EQ (object identity)
(cond ((eq value1 value2) 0)
      ((> value1 value2) 1)
      (t -1))

;; CORRECT - uses = (numeric equality)
(cond ((= value1 value2) 0)
      ((> value1 value2) 1)
      (t -1))
```

**Key Insight**: When Java primitives behave unexpectedly, check if the generated
Lisp code uses appropriate comparison/conversion functions. Common issues:
- Using `EQ` instead of `=` or `EQL` for numbers
- Missing `unsigned-to-signed-integer`/`unsigned-to-signed-long` conversions
- Incorrect type coercion between FLOAT and DOUBLE

## Modern JDK Notes

OpenLDK targets JDK 25 (JDK 8 support has been dropped). The move past JDK 8
changed several native method implementations and how classes are loaded.

### JDK Class Files

JDK 9+ classes come from jigsaw modules (`java.base`, etc.) rather than a flat
`rt.jar`. OpenLDK reads class files directly from JMOD files in
`$JAVA_HOME/jmods/`, or from `$JAVA_HOME/lib/modules` when the JDK has no
JMODs. All module classes are available. Code must still handle
missing classes gracefully:

- `classload` may return nil for unavailable classes
- `emit` for `constant-class-reference` creates a stub `<class>` with just the name
- `%instanceof-check` returns 0 when the target CLOS class doesn't exist
- `checkcast` codegen skips the type check when `find-class` returns nil

### Native Method Signature Changes (JDK 8 → 25)

Several native methods changed signatures after JDK 8:

- **`defineClass1`**: Changed from instance method on `ClassLoader` to a static
  method with an explicit `ClassLoader` parameter.
- **`initStackTraceElements`**: New static native in JDK 9+ for populating
  stack trace element arrays from throwable backtrace data.
- **`Reference.refersTo0`**: New in JDK 16+. Used by `ThreadLocalMap.getEntry()`
  for `WeakReference` key matching. Must perform identity comparison (`eq`).

### sun.misc.Unsafe → jdk.internal.misc.Unsafe

Since JDK 9, `sun.misc.Unsafe` is a bytecoded wrapper that delegates to
`jdk.internal.misc.Unsafe`. Raw memory operations (`allocateMemory0`,
`freeMemory0`, `putLong0`, `getByte0`, `copyMemory0`) are native methods
on `jdk/internal/misc/Unsafe`, not `sun/misc/Unsafe`.

### DirectMethodHandle$Holder Trampolines

The JVM dynamically generates `DirectMethodHandle$Holder` methods at startup
via `GenerateJLIClassesHelper` — they don't exist in class files. OpenLDK
defines trampolines for 6 method types × 10 arities:

- `invokeStatic`, `invokeStaticInit`, `invokeSpecial`
- `invokeVirtual`, `invokeInterface`, `newInvokeSpecial`

Each trampoline extracts the target `MemberName` from the `DirectMethodHandle`
(arg0 per LambdaForm calling convention) and dispatches via
`%invoke-from-member-name`.

### Class Loader Packages

OpenLDK uses separate Lisp packages for different class loaders:

- `OPENLDK.SYSTEM` — boot/system class loader
- `:openldk` — default package (native methods are always found here first)
- `OPENLDK.APP` — application class loader

`static-method-symbol` checks `:openldk` first for native methods, then falls
back to the loader's package. This is how native method overrides work.

## Reference Sources

- **JDK 25 Source**: Use OpenJDK 25 source to understand correct Java semantics
- **Java VM Spec**: Essential for bytecode instruction behavior
- **Java Language Spec**: For language-level semantics

## Common Pitfall Patterns

### Side-Effect Detection

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

### Testing with Kawa Scheme

[Kawa](https://www.gnu.org/software/kawa/) is a Scheme implementation targeting the JVM.
It exercises many JVM features (reflection, class generation, module loading) and serves
as a good integration test for OpenLDK.

```bash
# Build the warm Kawa SBCL image (downloads lib/kawa-3.1.1.jar; see docs/IMAGES.md)
make kawa

# Run Kawa interactively
./kawa

# Test arithmetic
echo '(+ 2 3)' | ./kawa

# Debug class loading during Kawa startup
LDK_DEBUG=L ./kawa
```

## Build System Notes

- **SBCL only**: Uses SBCL-specific features (sb-ext, sb-debug)
- **JDK 25**: Class file parser supports JDK 25 class files
- **JAVA_HOME**: Should point to a JDK 25 (e.g., `/usr/lib/jvm/java-25-openjdk`). If it doesn't, `openldk` searches the usual install locations and, failing that, downloads one (see README.md).
- **ocicl**: Package manager - run `ocicl install` before building
- **Build targets**: `make` builds openldk and javacl; `make kawa` and `make abcl` build warm Kawa and ABCL images; `make check` runs the test suite

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
- **Correctness first**: The JIT types locals, arrays and arithmetic where it
  can prove the types, but correctness always wins over speed
- **Method caching**: Compiled methods replace stubs permanently
- **No inlining**: Each Java method is a separate Lisp function

OpenLDK is not designed to compete with HotSpot - it's for embedding Java
libraries in Lisp applications when you need "that one Java library."
