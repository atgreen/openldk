# OpenLDK Clojure Bug Hunt

## Current Status

OpenLDK can now load Clojure's main class and execute lambda/method handle operations, but encounters a `clojure.lang.Compiler$CompilerException` during `clojure.main.<clinit>()`.

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

### Debug Flags
- `LDK_DEBUG=c` - Enable codegen debug output (very verbose, prints generated Lisp code)
- `LDK_DEBUG=e` - Enable exception debug output
- `LDK_DEBUG=l` - Enable class loading debug output

## Current Error

```
Unhandled Java exception:
java.lang.ExceptionInInitializerError
    at clojure/main.unknown(((LABELS CLINIT IN %CLINIT) #<<CLASS> clojure/main>))

Caused by: Unexpected error macroexpanding binding at (clojure/core.clj:2126:16).

Caused by: java.lang.IllegalArgumentException: Argument must be an integer: 2
```

The error occurs during macroexpansion of the `binding` macro in `clojure/core.clj`. The macro calls `(even? (count bindings))` which fails because `integer?` returns false for the Integer object.

### Analysis

The `integer?` function checks `(instance? Integer n)` etc. for various numeric types. The compiled bytecode uses `instanceof` instructions directly. Simple Java tests show that instanceof works correctly for Integer CLOS objects. However, when Clojure's pre-compiled `integer?` class is loaded and executed, something causes instanceof to fail.

**Key Finding**: The value being passed to `even?` is a boxed Integer CLOS object (like `#<java/lang/Integer {1202CD76C3}>`), and the error prints it as "2". But somehow the instanceof check fails.

**Status**: Under investigation. Simple instanceof tests pass, but Clojure's loading sequence triggers the failure.

### Partial Fix Applied

Added handling for native Lisp integers in instanceof checks:

**src/codegen.lisp**:
- Added `%instanceof-integer-check` helper function that returns 1 for native Lisp integers when checking Integer/Long/Short/Byte types
- Modified `ir-instanceof` codegen to use `%instanceof-integer-check` for integral wrapper types

**src/native.lisp**:
- Enhanced `isInstance` method to return 1 for native Lisp integers when checking Integer/Long/Short/Byte/Number classes

These changes make simple Java instanceof tests work correctly, but the issue persists in Clojure's loading sequence.

## Recently Fixed Issues

### 1. Lambda Functional Interface Support (commit c10a7a3)

**Problem**: `The function OPENLDK::|test(Ljava/lang/Object;)| is undefined`

**Root Cause**: `%lambda-metafactory` only created `LambdaSupplier` (implementing `Supplier.get()`), but Java lambdas use many functional interfaces like `Predicate.test()`, `Function.apply()`, `Consumer.accept()`, etc.

**Solution**: Added multiple lambda implementation classes in `src/native.lisp`:
- `LambdaPredicate` - `test(Object)` for filtering
- `LambdaFunction` - `apply(Object)` for transformation
- `LambdaConsumer` - `accept(Object)` for side effects
- `LambdaBiConsumer` - `accept(Object, Object)` for two-arg consumers
- `LambdaBinaryOperator` - `apply(Object, Object)` for combining

Modified `%lambda-metafactory` to select the appropriate class based on the interface method name.

### 2. Virtual Method Invocation in Method Handles

**Problem**: Virtual methods called via `linkToVirtual` used fully qualified names like `java/util/List.add(Ljava/lang/Object;)` but instance methods are defined as `defmethod |add(Ljava/lang/Object;)|`.

**Solution**: Modified `%invoke-from-member-name` in `src/native.lisp` to check the reference kind (ref-kind) from MemberName flags:
- `ref-kind = 6` (REF_invokeStatic): Use fully qualified `class.method(desc)` name
- `ref-kind = 5` (REF_invokeVirtual) or `7` (REF_invokeSpecial): Use simple `method(desc)` name

### 3. Constructor Invocation via Method Handles

**Problem**: Constructors called via `findConstructor` needed special handling because:
- MethodType has the class as return type, but `<init>` methods return void
- Constructor methods are `defmethod |<init>()|`, not `defun |Class.<init>()|`

**Solution**: Added constructor handling in `%invoke-from-member-name` that:
1. Creates a new instance with `make-instance`
2. Extracts params from MethodType and appends `V` for void return
3. Calls the instance method `|<init>(...)|` (not fully qualified)
4. Returns the constructed instance

### 4. MethodHandles$Lookup Security Check Bypass

**Problem**: `checkUnprivilegedlookupClass` threw `IllegalArgumentException` for bootstrap classes (java.*, sun.*) when creating lookup objects for lambda metafactory.

**Solution**: Added native no-op implementation for `checkUnprivilegedlookupClass` and registered it in `native-override-p` in `src/openldk.lisp`.

## Key Files and Locations

### src/native.lisp
- **Lines 2195-2330**: `findStatic`, `findSpecial`, `findConstructor`, `findVirtual` implementations
- **Lines 2395-2472**: `%invoke-from-member-name` - core method handle invocation logic
- **Lines 2583-2662**: Lambda implementation classes and `%lambda-metafactory`
- **Lines 2474-2580**: `linkToStatic`, `linkToVirtual`, `linkToSpecial` intrinsics

### src/codegen.lisp
- **Lines 1124-1155**: `ir-call-dynamic-method` codegen for `invokedynamic`

### src/openldk.lisp
- **Lines 63-78**: `native-override-p` function that determines which methods use native implementations
- **Lines 1165-1192**: `<clinit>` exception handling that wraps errors in `ExceptionInInitializerError`

## Theories for the CompilerException

### Theory 1: Missing Method or Class
Clojure's compiler is trying to call a method or load a class that OpenLDK hasn't implemented. The CompilerException wraps the underlying cause.

**Investigation**: Run with `LDK_DEBUG=c` and look at what's being loaded/compiled right before the exception.

### Theory 2: Reflection Issues
Clojure heavily uses reflection. There may be issues with:
- `Class.getDeclaredMethods()`
- `Class.getDeclaredFields()`
- `Method.invoke()`
- `Constructor.newInstance()`

**Investigation**: Add debug output to reflection native methods in `src/native.lisp`.

### Theory 3: ClassLoader Issues
Clojure dynamically generates and loads classes. If the classloader isn't properly returning generated classes, compilation would fail.

**Investigation**: Check `defineClass` and `loadClass` implementations.

### Theory 4: Missing Functional Interfaces
There may be other functional interfaces beyond the ones we implemented that Clojure uses internally.

**Investigation**: Search for `invokedynamic` calls with method names other than `get`, `test`, `apply`, `accept`.

## Next Steps

1. **Get the full exception chain**: The CompilerException should have a cause. Check if OpenLDK is properly preserving exception causes by examining the `|exception|` or `|cause|` slot.

2. **Add exception cause printing**: Modify the exception handler in `src/openldk.lisp` around line 1171 to also print the cause chain:
   ```lisp
   (when (and (slot-boundp e '|objref|) (slot-value e '|objref|))
     (let ((throwable (slot-value e '|objref|)))
       (when (slot-exists-p throwable '|cause|)
         (format t "~%; Cause: ~A~%" (slot-value throwable '|cause|)))))
   ```

3. **Trace Clojure initialization**: Clojure's `main` class loads `clojure.core` which triggers massive compilation. Narrow down which specific compilation step fails.

4. **Check for unimplemented natives**: Run with debug and grep for "not implemented" or similar warnings that indicate missing functionality.

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
