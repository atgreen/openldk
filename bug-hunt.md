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

### Latest Findings (2025-11-22 afternoon)

**Added debug output to %resolve-invokedynamic** - Debug prints show that the function is NEVER reached. The infinite loop happens during class initialization, before any lambda code runs.

**Traced with LDK_DEBUG=t** - Shows infinite loop calling:
```
* trace: System.setProperty(String, String)
* trace: System.getSecurityManager()
* trace: hashCode()
* trace: equals(Object)
* trace: put(Object, Object)
```

**Root Cause**: Loading `DirectMethodHandle` or related lambda classes triggers class initialization that has a circular dependency with `System.setProperty()`.

### Theory

1. SimpleLambdaTest is compiled with `invokedynamic` instruction
2. OpenLDK starts loading the test class
3. Class loading triggers static initialization of java.lang.invoke classes
4. Something in that initialization calls `System.setProperty()`
5. setProperty triggers more initialization that calls setProperty again → infinite loop

### Next Investigation Steps

1. ✅ **Add debug output** - Done, shows %resolve-invokedynamic never reached
2. ✅ **Run with LDK_DEBUG=t** - Done, found setProperty loop
3. **Identify which class initialization triggers the loop**
   - Add classload debug to see what class is being loaded when loop starts
   - Check static initializers of DirectMethodHandle, LambdaForm, etc.
4. **Fix the circular dependency**
   - Likely need to stub out or reimplement some initialization method
   - Or defer property setting until after critical classes are loaded
5. **Alternative**: Test with pre-Java-8 invokedynamic (string concat, etc.)

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
