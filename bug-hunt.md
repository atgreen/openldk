# Lambda/invokedynamic Support Investigation

## Current Status: Lambda Support In Progress

**Working:**
- ✅ Basic MethodHandle support (invokeExact)
- ✅ MethodHandle intrinsics (linkToStatic, linkToVirtual, linkToSpecial, linkToInterface)
- ✅ DirectMethodHandle creation
- ✅ invokedynamic bytecode parsing

**Not Working:**
- ❌ Lambda expressions (hang during LambdaMetafactory bootstrap)
- ❌ Full invokedynamic support for lambda generation

## Latest Commits
- `7ba983e` - feat(native): add polymorphic signature variants for linkTo* intrinsics
- `2a070c9` - refactor(native): remove debug output from MemberName initialization
- `c8aad19` - test(aaa): add MethodHandle test to aaa test suite
- Latest (uncommitted): Use DirectMethodHandle instead of plain MethodHandle in findStatic

---

## The Problem

### Test Case: SimpleLambdaTest.java
```java
import java.util.function.Supplier;

public class SimpleLambdaTest {
    public static void main(String[] args) {
        Supplier<String> supplier = () -> "Hello from lambda!";
        String result = supplier.get();
        System.out.println(result);
    }
}
```

### Current Behavior
The test hangs indefinitely during lambda creation. No error is thrown - it just hangs.

### Previous Error (Fixed)
**Before DirectMethodHandle fix:**
```
java.lang.IllegalArgumentException: not a direct method handle
```

This was fixed by changing `findStatic` to create `DirectMethodHandle` instead of plain `MethodHandle`, and setting the `member` field.

---

## Architecture Overview

### How invokedynamic Works

1. **Bytecode**: Java compiler generates `invokedynamic` instruction for lambdas
2. **Bootstrap Method**: Points to `LambdaMetafactory.metafactory()`
3. **CallSite Creation**: Bootstrap method returns a `CallSite` with target `MethodHandle`
4. **Invocation**: Future calls use the cached `CallSite.getTarget()`

### OpenLDK Implementation

**Bytecode Parsing** (bc-to-ir.lisp:1494):
- Parses `invokedynamic` instruction
- Extracts bootstrap method reference
- Creates `ir-call-dynamic-method` IR node

**Code Generation** (codegen.lisp:1119):
- Generates call to `%resolve-invokedynamic`
- Caches CallSite by (bootstrap-method, address)
- Invokes via `CallSite.getTarget().invokeWithArguments()`

**Resolution** (codegen.lisp:1108):
```lisp
(defun %resolve-invokedynamic (method-name bootstrap-method-name address fname &rest args)
  (let* ((key (list bootstrap-method-name address))
         (cached (gethash key *invokedynamic-cache*)))
    (if cached
        cached
        (let ((resolved (apply bootstrap-method-name
                               (append (list (|java/lang/invoke/MethodHandles.lookup()|) fname)
                                       args))))
          (setf (gethash key *invokedynamic-cache*) resolved)
          resolved))))
```

---

## Investigation Steps

### Step 1: Fixed DirectMethodHandle ✅

**File: src/native.lisp (line 1924)**

**Before:**
```lisp
(defun |java/lang/invoke/MethodHandles$Lookup.findStatic(...)| (...)
  ...
  (let* ((mh (make-instance '|java/lang/invoke/MethodHandle|)))
    ...))
```

**After:**
```lisp
(defun |java/lang/invoke/MethodHandles$Lookup.findStatic(...)| (...)
  (classload "java/lang/invoke/DirectMethodHandle")
  (let* ((mh (make-instance '|java/lang/invoke/DirectMethodHandle|)))
    ...
    (when (slot-exists-p mh '|member|)
      (setf (slot-value mh '|member|) member))
    mh))
```

**Result:** No more "not a direct method handle" error, but now hangs instead of failing.

### Step 2: Current Issue - Lambda Creation Hangs

**Symptoms:**
- Test runs indefinitely with no output
- No error messages
- Likely infinite loop or circular dependency

**Hypothesis:**
LambdaMetafactory is calling methods that:
1. Aren't implemented and cause infinite loops
2. Trigger compilation that recursively needs lambdas
3. Require Java classes that aren't being loaded properly

---

## Investigation Progress

### Latest Findings (2025-11-22 afternoon/evening)

**Progress:**
1. ✅ Added debug output to %resolve-invokedynamic
2. ✅ Found infinite setProperty loop with LDK_DEBUG=t
3. ✅ **FIXED System.setProperty recursion!**
4. ⚠️ New issue: hanging in SocketPermission initialization

**Fix Applied:** Implemented native System.setProperty/getProperty
- Added *ldk-system-properties* hash table in global-state.lisp (line 53)
- Implemented |java/lang/System.setProperty(...)| in native.lisp (line 838)
- Implemented |java/lang/System.getProperty(...)| in native.lisp (line 852)
- These methods store/retrieve from Lisp hash table, avoiding Java class loading

**Result:** The infinite setProperty loop is completely fixed! Now we can progress much further:
- %resolve-invokedynamic is REACHED (line 1154 in trace)
- LambdaMetafactory.metafactory() is CALLED (line 1171)
- InnerClassLambdaMetafactory constructor runs (line 1175)

**New Issue:** Now hangs during SocketPermission static initialization
```
* <13> trace: entering java/net/SocketPermission.<clinit>()()
* <13> trace: getProperty(Ljava/lang/String;)
* <13> trace: entering java/net/SocketPermission.getHost(Ljava/lang/String;)()
[hangs here]
```

The bootstrap method is being called successfully, but something in the security/permission initialization is now blocking progress.

### Theory

The lambda bootstrap process triggers security permission checks, which load SocketPermission. That class's initialization is somehow hanging (not an infinite loop, just stuck/slow).

### Latest Findings (2025-11-22 evening - Session 2)

**MAJOR FIX APPLIED:**
Fixed missing `emit` method for `ir-string-literal` in classfile.lisp (line 152-155):
```lisp
(defmethod emit ((v ir-string-literal) cp)
  "Return the string value from a UTF8 constant pool entry."
  (declare (ignore cp))
  (slot-value v 'value))
```

**Root Cause:**
When processing CONSTANT-METHOD-HANDLE in the constant pool:
1. Code called `(emit (aref cp (class-index m)) cp)` to get class name
2. This retrieved a constant-class-reference pointing to a UTF8 string (`ir-string-literal`)
3. Then called `(emit (aref cp index) cp)` on the UTF8 string
4. **BUG**: No `emit` method existed for `ir-string-literal` → returned NIL
5. Calling `(ir-class-class NIL)` → returned NIL
6. Calling `(java-class NIL)` → ERROR: "NO-APPLICABLE-METHOD for (JAVA-CLASS NIL)"

**Result:** The NIL error is now FIXED! Lambda bootstrap method is being called successfully.

**Current Status:** Code now hangs inside `LambdaMetafactory.metafactory()` call.

### Latest Findings (2025-11-22 continued - FROM PREVIOUS SESSION)

**Major Progress:**
1. ✅ Bootstrap returns successfully: `#<java/lang/invoke/ConstantCallSite>`
2. ✅ Got target MethodHandle: `#<java/lang/invoke/BoundMethodHandle$Species_L>`
3. ❌ Error when invoking lambda via `invokeWithArguments()`

**Root Cause Discovery (ARRAY[255] IS NOT THE BUG):**
The array[255] access happens in `Integer.valueOf(127)` for the Integer cache (valid operation).

**Root Cause Discovery (INVOKEWITHAR GUMENTS):**
The error occurs when `invokeWithArguments()` is called on the lambda MethodHandle:
- `invokeWithArguments([Ljava/lang/Object;])` is called
- This triggers `spreadInvoker(0)` → `asSpreader(Object[].class, 0)`
- Which calls `spreadArgumentsForm()` in LambdaFormEditor
- This triggers `MethodHandleImpl$Lazy.<clinit>()`
- Which calls `makeArrays()` to build collector array (size 256, only indices 0-10 filled)
- Then `varargsArray(255)` is called for some reason
- It tries to create collector via `findCollector("array", 255, ...)`
- `findStatic()` tries to find a method with 255 parameters
- `DirectMethodHandle.makePreparedLambdaForm()` appends MemberName parameter → 256 params
- **boom: "bad parameter count 256"**

**Analysis:**
- Java 8's `makeArrays()` creates ARRAYS with size MAX_ARITY+1 (256 slots)
- Only indices 0-10 are pre-populated with collectors
- `varargsArray(arity)` checks if `ARRAYS[arity]` is NULL
- If NULL, tries to create collector on-demand via `findCollector()`
- For arity 255, this fails because 255 + MemberName = 256 parameters (over JVM limit of 255)

**Why arity 255?**
Investigation findings:
- `makeArrays()` only calls `findCollector()` in a loop with arities 0-10 (confirmed via bytecode)
- The call to `findCollector("array", 255, ...)` comes from `varargsArray(255)`
- `varargsArray(arity)` is being called with arity=255
- ~~**Hypothesis**: 255 = -1 as unsigned byte~~ **DISPROVEN**
- **Confirmed via logging**: The index is legitimately 255 (positive integer), NOT -1!
  - Type: `(INTEGER 0 4611686018427387903)`
  - `(< index 0)` returns NIL
- MAX_ARITY field reads correctly as 255 in Java bytecode (tested)
- **New hypothesis**: Somewhere is using MAX_ARITY value (255) as a parameter count instead of actual arity
- The lambda `() -> "Hello"` should have arity 0, but 255 is being passed

### Next Investigation Steps

1. ✅ **Fix setProperty recursion** - Done with native implementation
2. ✅ **Reach LambdaMetafactory** - Done, bootstrap is being called!
3. ✅ **Bootstrap returns CallSite** - Working perfectly!
4. **Fix varargsArray(255) issue**
   - Option A: Implement native `varargsArray()` that only supports arities 0-10
   - Option B: Fix whatever is causing arity 255 to be requested
   - Option C: Implement native `invokeWithArguments()` to avoid spreadInvoker path
5. **Alternative**: Use direct lambda invocation instead of `invokeWithArguments()`

---

## Test Results

### MethodHandleTest ✅
```bash
$ ./openldk MethodHandleTest
=== MethodHandle Test ===
...
Result: 42
Result: Hello, World
=== All tests passed! ===
```

### SimpleLambdaTest ❌
```bash
$ timeout 30 ./openldk SimpleLambdaTest
<hangs after 30 seconds>
```

---

## Technical Details

### DirectMethodHandle vs MethodHandle

**DirectMethodHandle** (Java internal class):
- Extends MethodHandle
- Has `final MemberName member` field
- Represents direct method invocation (no transformations)
- Required by LambdaMetafactory for introspection

**Key Fields:**
- `type`: MethodType descriptor
- `form`: LambdaForm (execution strategy)
- `member`: MemberName (method metadata) - DirectMethodHandle only

### MemberName Flags

Our implementation sets:
```lisp
(logior #x10000        ; MN_IS_METHOD
        (ash 6 24)     ; REF_invokeStatic << 24
        #x0008)        ; ACC_STATIC
```

This marks it as a static method invocation reference.

---

## Related Files

- **src/native.lisp**: MethodHandle/MemberName implementation
- **src/bc-to-ir.lisp**: invokedynamic bytecode parsing (line 1494)
- **src/codegen.lisp**: invokedynamic code generation (line 1119)
- **testsuite/SimpleLambdaTest.java**: Test case
- **testsuite/MethodHandleTest.java**: Working MethodHandle test

---

## Open Questions

1. What Java methods is LambdaMetafactory calling that might not be implemented?
2. Is there a circular dependency in the class loading/compilation?
3. Are there simpler invokedynamic use cases we should test first?
4. Does LambdaMetafactory need special initialization?

---

*Last updated: 2025-11-22*
