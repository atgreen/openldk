# OpenLDK Clojure Bug Hunt

## Current Status

**UPDATE 2024-11-29**: Investigating per-loader class isolation bug. The `shiftLeft` static method resolution error is being debugged. Debug output has been added to `class-package` and `static-method-symbol` functions.

**UPDATE 2024-11-28**: Fixed the `integer?` returning null issue! The root cause was incomplete transitive merging of stack variables at control flow merge points.

**UPDATE 2024-11-28 (later)**: Added `invoke-special` method dispatch caching to fix a 9-minute hang during Clojure compilation. The hang was caused by repeated `compute-applicable-methods-using-classes` calls for `|<init>()|` with 851+ methods.

OpenLDK can now correctly execute Clojure's `integer?` function. Testing continues to see if Clojure fully loads.

---

## ACTIVE BUG: Static Method Symbol Resolution (Per-Loader Class Isolation)

### Problem Summary

When running Clojure on OpenLDK, static method calls can fail with "undefined function" errors because the generated code looks for method symbols in the wrong Lisp package:

```
The function OPENLDK::|clojure/lang/Numbers.shiftLeft(JLjava/lang/Object;)| is undefined.
```

The method should be in `OPENLDK.APP` (application class loader's package), but the generated code looks in `OPENLDK` (bootstrap class loader's package).

### Background: Class Loader Hierarchy

OpenLDK implements JVM class loader isolation using Lisp packages:

| Class Loader | Lisp Package | Purpose |
|--------------|--------------|---------|
| Bootstrap | `OPENLDK` | JDK core classes (java/*, sun/*, etc.) |
| Application | `OPENLDK.APP` | Application classpath classes |
| DynamicClassLoader | `OPENLDK.L1`, `L2`, ... | Clojure's dynamic class loading |

Each class loaded by a specific loader has its CLOS class definition and static method stubs defined in that loader's package.

### How to Reproduce

```bash
# Build OpenLDK
make

# Run Clojure Hello World (triggers the bug)
JAVA_CMD=./openldk LDK_DEBUG=L clojure -M -e '(println "Hello World!")'
```

The error occurs during Clojure's `bit_shift_left` compilation, which calls `clojure.lang.Numbers.shiftLeft()`.

### Key Code Paths

#### 1. Static Method Codegen (`src/codegen.lisp:378-401`)

```lisp
(defmethod codegen ((insn ir-call-static-method) context)
  (with-slots (class method-name args return-type) insn
    (make-instance '<expression>
      :code (let* ((loader (slot-value context 'ldk-loader))
                   (_ (classload class))
                   (declaring-class (or (%find-declaring-class class method-name loader) class))
                   (pkg (class-package declaring-class))  ; <-- Gets package for the class
                   (full-name (format nil "~A.~A" declaring-class method-name))
                   (method-sym (static-method-symbol full-name pkg)))  ; <-- Resolves symbol
              ...))))
```

#### 2. class-package (`src/global-state.lisp:169-199`)

```lisp
(defun class-package (class-name)
  "Get the Lisp package for a class by its binary name.
   Searches boot loader, then app loader for the class.
   Falls back to :openldk if class not found or has no loader."
  (if-let ((class (%get-ldk-class-by-bin-name class-name t)))
    (if-let ((loader (slot-value class 'ldk-loader)))
      (loader-package loader)
      (find-package :openldk))
    ;; Not in boot loader - check app loader
    (if (and *app-ldk-class-loader*
             (gethash class-name (slot-value *app-ldk-class-loader* 'ldk-classes-by-bin-name)))
        (loader-package *app-ldk-class-loader*)
        (find-package :openldk))))
```

#### 3. static-method-symbol (`src/global-state.lisp:213-229`)

```lisp
(defun static-method-symbol (method-name loader-pkg)
  "Get the symbol for a static method name.
   First checks :openldk for native method implementations,
   then falls back to the loader's package for Java-defined methods."
  (let ((openldk-sym (find-symbol method-name (find-package :openldk))))
    (if (and openldk-sym (fboundp openldk-sym))
        openldk-sym  ; Native method in :openldk
        (intern method-name loader-pkg))))  ; Java method in loader's package
```

### Debug Output Added

Debug statements have been added to trace the bug:

**In `class-package`** (triggers for `clojure/lang/Numbers`):
```lisp
(when (str:starts-with? "clojure/lang/Numbers" class-name)
  (format t "~&; DEBUG: class-package called for ~A~%" class-name)
  (format t "~&; DEBUG: class-package for ~A: loader=~A pkg=~A~%"
          class-name loader pkg))
```

**In `static-method-symbol`** (triggers for `shiftLeft`):
```lisp
(when (str:contains? "shiftLeft" method-name)
  (format t "~&; DEBUG: static-method-symbol: method-name=~A loader-pkg=~A~%"
          method-name loader-pkg)
  (format t "~&; DEBUG: static-method-symbol: openldk-sym=~A fboundp=~A~%"
          openldk-sym (and openldk-sym (fboundp openldk-sym)))
  (format t "~&; DEBUG: static-method-symbol: returning ~A (package ~A)~%"
          result (symbol-package result)))
```

### Observations from Debug Output

Sample output:
```
; DEBUG: class-package called for clojure/lang/Numbers
; DEBUG: class-package for clojure/lang/Numbers: loader=#<<LDK-CLASS-LOADER> 1 pkg=#<PACKAGE "OPENLDK.APP">> pkg=#<PACKAGE "OPENLDK.APP">
```

This shows `class-package` IS returning `OPENLDK.APP` correctly. The bug may involve:

1. **Timing**: Class not loaded when `class-package` is called during codegen
2. **Symbol already exists**: Symbol may exist in `:openldk` from previous reference
3. **Call site mismatch**: Stub defined correctly but call site uses wrong symbol

### Related Fix: Duplicate DEFCLASS Bug

A related bug was fixed where `clojure/core$dotimes` was defined multiple times. The fix added `find-class-in-loader-hierarchy` to check parent loaders before defining a class.

### Next Steps for Investigation

1. Verify timing: Add debug to see if `classload` completes before `class-package` is called
2. Check symbol creation: Trace when `OPENLDK::|..shiftLeft..|` symbol is first created
3. Inspect compiled code: Use `--dump-dir` to see generated code for `bit_shift_left`
4. Compare with working case: Check why other static methods work

### Test Commands

```bash
# Basic test with debug output
JAVA_CMD=./openldk LDK_DEBUG=L clojure -M -e '(println "Hello World!")'

# Capture full output
JAVA_CMD=./openldk LDK_DEBUG=L clojure -M -e '(println "Hello World!")' 2>&1 > debug.log

# Filter for debug lines
grep -E "DEBUG|shiftLeft|bit_shift_left" debug.log

# Simple Hello World (fewer classes)
./openldk Hello
```

---

## How to Reproduce

### Prerequisites
- Java 8 JDK (tested with temurin-jdk)
- Clojure 1.12.3 JAR

### Build and Run
```bash
# Build OpenLDK
cd /home/green/git/cl-openldk/openldk
JAVA_HOME=/usr/lib/jvm/java-8-temurin-jdk/jre make

# Run Clojure
JAVA_HOME=/usr/lib/jvm/java-8-temurin-jdk/jre \
LDK_CLASSPATH=/tmp:~/.m2/repository/org/clojure/clojure/1.12.3/clojure-1.12.3.jar \
./openldk clojure.main -e "(println \"Hello Clojure\")"
```

### Test the integer? fix
```bash
# Compile the test
/usr/lib/jvm/java-8-temurin-jdk/bin/javac -cp ~/.m2/repository/org/clojure/clojure/1.12.3/clojure-1.12.3.jar:/tmp /tmp/CallClojureIntegerQ.java

# Run in OpenLDK - should print "Result: true"
JAVA_HOME=/usr/lib/jvm/java-8-temurin-jdk/jre \
LDK_CLASSPATH=/tmp:~/.m2/repository/org/clojure/clojure/1.12.3/clojure-1.12.3.jar \
./openldk CallClojureIntegerQ
```

### Debug Flags
- `LDK_DEBUG=c` - Enable codegen debug output (very verbose, prints generated Lisp code)
- `LDK_DEBUG=e` - Enable exception debug output
- `LDK_DEBUG=l` - Enable class loading debug output

## Fixed: Stack Variable Merging Bug (2024-11-28)

### Problem
Clojure's `integer?` function was returning `null` instead of `Boolean.TRUE`, causing `even?` to throw "Argument must be an integer: 2".

### Root Cause Analysis

The `integer?` bytecode has multiple paths that all converge at an `areturn`:

```
13: getstatic Boolean.TRUE
16: goto 22
19: getstatic Boolean.FALSE
22: goto 154     ; merge point 1: paths from 16 and 19
...
39: getstatic Boolean.TRUE
42: goto 48
45: getstatic Boolean.FALSE
48: goto 154     ; merge point 2: paths from 42 and 45
...
154: areturn     ; final merge: paths from 22, 48, etc.
```

Each path creates a `<stack-variable>` with its own `var-numbers` set (addresses where it's used). At merge points, `merge-stacks` unifies the var-numbers of converging paths.

**The bug**: When stack states merge at intermediate points (22, 48) and then at the final point (154), the transitive closure wasn't computed. For example:

1. At PC 22: `SV_13` and `SV_19` merge → both get var-numbers {13, 19}
2. At PC 154: `SV_19` (from 22) and `SV_45` (from 48) merge → `SV_19` gets {13, 19, 39, 45, ...}
3. But `SV_13` still has only {13, 19} - the later merge doesn't propagate back!

This caused the assignment at address 13 to use variable `s{13,19}` while the return at address 154 used variable `s{13,19,39,45,...}` - different variables!

### Solution

Rewrote `fix-stack-variables` in `src/openldk.lisp` (lines 145-208) to use a **union-find algorithm** with path compression:

1. Initialize each stack-variable as its own equivalence class
2. For each var-number, union all stack-variables that share it
3. After all unions, all transitively connected stack-variables are in the same class
4. Update all stack-variables in each class to have the same (union of all) var-numbers

This ensures that if `SV_A` shares var-numbers with `SV_B`, and `SV_B` shares with `SV_C`, then `SV_A`, `SV_B`, and `SV_C` all get the same unified var-numbers.

### Key Files Changed

**src/openldk.lisp** (lines 145-208):
- `fix-stack-variables` - now uses union-find for proper transitive closure

**src/codegen.lisp** (line 850-855):
- `ir-if-acmpeq` - fixed to use direct `eq` comparison instead of `sxhash` (earlier fix in this session)

## Previous Error (Now Fixed)

```
Caused by: java.lang.IllegalArgumentException: Argument must be an integer: 2
```

This was caused by `integer?` returning null (due to the stack variable bug), which `even?` then compared with `Boolean.FALSE` using `if_acmpeq`. Since null != FALSE, it went to the error branch.

## Recently Fixed Issues

### 1. invoke-special Method Dispatch Caching (this session)

**Problem**: Clojure compilation hung for 9+ minutes at "COMPILING clojure/lang/ChunkedCons"

**Root Cause**: The `invoke-special` function calls `compute-applicable-methods-using-classes` to find the correct method to invoke. For constructor methods like `|<init>()|`, there can be 851+ methods across all loaded classes. Each call to this CLOS function is expensive, and during class loading, `invoke-special` is called repeatedly for the same method/owner combinations.

**Solution**: Added `*invoke-special-cache*` hash table in `src/openldk.lisp` (lines 94-145):
- Cache key: `(cons method-symbol owner-symbol)`
- Cache value: `(cons method-function next-methods)`
- On cache hit, directly call the cached method function
- On cache miss, compute methods, cache the result, then call

```lisp
(defvar *invoke-special-cache* (make-hash-table :test 'equal))

(defun invoke-special (method-symbol owner-symbol args)
  (let* ((cache-key (cons method-symbol owner-symbol))
         (cached (gethash cache-key *invoke-special-cache*)))
    (if cached
        (funcall (car cached) args (cdr cached))
        ;; Cache miss - compute and cache...
```

### 2. Stack Variable Transitive Merging (this session)

**Problem**: `integer?` returned null instead of Boolean.TRUE

**Root Cause**: `fix-stack-variables` didn't properly compute transitive closure when merging stack variables across multiple control flow merge points.

**Solution**: Rewrote using union-find algorithm with path compression for O(α(n)) performance.

### 3. if_acmpeq Using sxhash (this session)

**Problem**: Object reference comparison used `sxhash` instead of `eq`

**Root Cause**: The `ir-if-acmpeq` codegen was comparing hash codes instead of object identity.

**Solution**: Changed to use direct `eq` comparison:
```lisp
(defmethod codegen ((insn ir-if-acmpeq) context)
  (with-slots (offset value1 value2) insn
    (make-instance '<expression>
                   :insn insn
                   :code (list 'when (list 'eq (code (codegen value1 context))
                                               (code (codegen value2 context)))
                               (list 'go (intern (format nil "branch-target-~A" offset)))))))
```

### 4. Lambda Functional Interface Support (commit c10a7a3)

**Problem**: `The function OPENLDK::|test(Ljava/lang/Object;)| is undefined`

**Root Cause**: `%lambda-metafactory` only created `LambdaSupplier` (implementing `Supplier.get()`), but Java lambdas use many functional interfaces like `Predicate.test()`, `Function.apply()`, `Consumer.accept()`, etc.

**Solution**: Added multiple lambda implementation classes in `src/native.lisp`:
- `LambdaPredicate` - `test(Object)` for filtering
- `LambdaFunction` - `apply(Object)` for transformation
- `LambdaConsumer` - `accept(Object)` for side effects
- `LambdaBiConsumer` - `accept(Object, Object)` for two-arg consumers
- `LambdaBinaryOperator` - `apply(Object, Object)` for combining

Modified `%lambda-metafactory` to select the appropriate class based on the interface method name.

### 5. Virtual Method Invocation in Method Handles

**Problem**: Virtual methods called via `linkToVirtual` used fully qualified names like `java/util/List.add(Ljava/lang/Object;)` but instance methods are defined as `defmethod |add(Ljava/lang/Object;)|`.

**Solution**: Modified `%invoke-from-member-name` in `src/native.lisp` to check the reference kind (ref-kind) from MemberName flags:
- `ref-kind = 6` (REF_invokeStatic): Use fully qualified `class.method(desc)` name
- `ref-kind = 5` (REF_invokeVirtual) or `7` (REF_invokeSpecial): Use simple `method(desc)` name

### 6. Constructor Invocation via Method Handles

**Problem**: Constructors called via `findConstructor` needed special handling because:
- MethodType has the class as return type, but `<init>` methods return void
- Constructor methods are `defmethod |<init>()|`, not `defun |Class.<init>()|`

**Solution**: Added constructor handling in `%invoke-from-member-name` that:
1. Creates a new instance with `make-instance`
2. Extracts params from MethodType and appends `V` for void return
3. Calls the instance method `|<init>(...)|` (not fully qualified)
4. Returns the constructed instance

### 7. MethodHandles$Lookup Security Check Bypass

**Problem**: `checkUnprivilegedlookupClass` threw `IllegalArgumentException` for bootstrap classes (java.*, sun.*) when creating lookup objects for lambda metafactory.

**Solution**: Added native no-op implementation for `checkUnprivilegedlookupClass` and registered it in `native-override-p` in `src/openldk.lisp`.

## Key Files and Locations

### src/openldk.lisp
- **Lines 145-208**: `fix-stack-variables` - union-find based stack variable merging
- **Lines 63-78**: `native-override-p` function that determines which methods use native implementations
- **Lines 1165-1192**: `<clinit>` exception handling that wraps errors in `ExceptionInInitializerError`

### src/codegen.lisp
- **Lines 850-855**: `ir-if-acmpeq` codegen (fixed to use `eq`)
- **Lines 857-862**: `ir-if-acmpne` codegen
- **Lines 959-976**: `ir-instanceof` codegen with integer type handling
- **Lines 1124-1155**: `ir-call-dynamic-method` codegen for `invokedynamic`

### src/native.lisp
- **Lines 2195-2330**: `findStatic`, `findSpecial`, `findConstructor`, `findVirtual` implementations
- **Lines 2395-2472**: `%invoke-from-member-name` - core method handle invocation logic
- **Lines 2583-2662**: Lambda implementation classes and `%lambda-metafactory`
- **Lines 2474-2580**: `linkToStatic`, `linkToVirtual`, `linkToSpecial` intrinsics

### src/bc-to-ir.lisp
- **Lines 1957-1975**: `merge-stack-variables` and `merge-stacks` - stack unification at branch points

## Understanding Stack Variable Merging

### The Problem Domain

When bytecode has multiple paths that converge (like if/else branches), each path may push values onto the operand stack. At the merge point, these values must be treated as the "same" variable in the generated code.

Example:
```
if (cond) {
    push A;  // stack var SV_A
} else {
    push B;  // stack var SV_B
}
// merge point: SV_A and SV_B must become the same variable
use(top_of_stack);
```

### How OpenLDK Handles This

1. **During bc-to-ir**: Each bytecode instruction creates `<stack-variable>` objects with `var-numbers` = {address}. Branch instructions record stack states at targets via `%record-stack-state`.

2. **After bc-to-ir**: `merge-stacks` is called for each PC with multiple incoming states. This unifies `var-numbers` of corresponding stack positions.

3. **After merging**: `fix-stack-variables` computes transitive closure using union-find.

4. **During codegen**: Stack variable names are generated from `var-numbers`: `s{1,5,9}` means this variable is used at addresses 1, 5, and 9.

### Why Union-Find?

If SV_A shares address with SV_B (merged at point X), and SV_B shares address with SV_C (merged at point Y), then SV_A and SV_C must also share all addresses (transitive property).

Union-find efficiently computes these equivalence classes in near-linear time.

## Reference: JVM Method Handle Reference Kinds

From `java.lang.invoke.MethodHandleInfo`:
```
REF_getField         = 1
REF_getStatic        = 2
REF_putField         = 3
REF_putStatic        = 4
REF_invokeVirtual    = 5
REF_invokeStatic     = 6
REF_invokeSpecial    = 7
REF_newInvokeSpecial = 8  (constructor)
REF_invokeInterface  = 9
```

## Reference: Lambda Metafactory Arguments

For `LambdaMetafactory.metafactory(Lookup, String, MethodType, MethodType, MethodHandle, MethodType)`:
1. `Lookup` - caller's lookup context
2. `String` - interface method name (e.g., "test", "apply")
3. `MethodType` - CallSite signature (functional interface type + captured args)
4. `MethodType` - interface method signature
5. `MethodHandle` - implementation method handle
6. `MethodType` - instantiated method type (after type erasure)
