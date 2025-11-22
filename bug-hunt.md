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

## Next Investigation Steps

1. **Add debug output** to track what LambdaMetafactory is doing
2. **Run with LDK_DEBUG=t** to see method entry/exit
3. **Check for missing Java methods** in LambdaMetafactory chain
4. **Look for circular dependencies** in class loading
5. **Test simpler invokedynamic** (not lambda-based)

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
