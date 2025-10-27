## javac ClassReader setEnclosingType failure in OpenLDK

### Summary

Running `./openldk --classpath /usr/lib/jvm/temurin-8-jdk/lib/tools.jar com.sun.tools.javac.Main /tmp/Hello.java` presently aborts with:

```
java.lang.UnsupportedOperationException
    at com/sun/tools/javac/jvm/ClassReader$2.setEnclosingType(...)
    ...
```

The stack trace shows the call originates from Javac’s `ClassReader.readEnclosingMethodAttr`, where it executes `((ClassType)sym.type).setEnclosingType(...)` while wiring up the enclosing type for nested classes.

### Why this differs from HotSpot

In the upstream JDK, `ClassReader` creates an anonymous subclass of `Type.ClassType` (named `ClassReader$2`) while parsing generic signatures. That helper overrides `setEnclosingType` to throw `UnsupportedOperationException`—the JDK never calls the override because, by the time `readEnclosingMethodAttr` runs, the symbol’s `type` field has been replaced with the canonical `Type.ClassType` instance. HotSpot therefore reaches the base implementation that simply writes `outer_field`.

In OpenLDK, our mirror of Javac leaves the temporary `ClassReader$2` instance attached to the symbol when `readEnclosingMethodAttr` executes. The same `setEnclosingType` call therefore hits the anonymous helper’s override and we observe the `UnsupportedOperationException`.

### Possible fixes

1. **Normalize `sym.type` before `setEnclosingType`** – audit our ClassReader port so that, whenever we resolve a class symbol for an inner/anonymous class, we immediately replace `sym.type` with `sym.tsym.type` (the canonical `Type.ClassType`). That mirrors HotSpot’s state and guarantees the later call to `setEnclosingType` lands on the base implementation instead of the temporary helper. This likely means:
   - Tracking where we assign `ClassReader$2` instances (the generic-signature/parsing path) and making sure we invoke something akin to `sym.type = sym.tsym.type` once the symbol is fully initialized.
   - Verifying `readEnclosingMethodAttr` (and any other code that manipulates `sym.type`) runs after that normalization step.
   - Adding assertions/tests so we catch future regressions where a helper `ClassType` leaks past normalization.
2. Override `com/sun/tools/javac/code/Type$ClassType.setEnclosingType` (and the ClassReader helper) inside OpenLDK to allow the call and update `outer_field` directly.

Option 1 keeps behaviour closest to upstream but demands more work in the Javac bridge. Option 2 is a targeted workaround we can implement immediately in `src/native.lisp` if we want compilation to proceed while we chase the structural discrepancy.

### Touch points for option 1

To pursue the “normalize `sym.type`” approach we would need to hook the following Java entry points (via native overrides in `src/native.lisp`):

1. `com/sun/tools/javac/jvm/ClassReader.readSignature` / `classSigToType`
   * After the helper `ClassReader$2` instances are produced, explicitly set `sym.type = sym.tsym.type` before the reader proceeds. This mirrors the state HotSpot leaves behind once signature processing finishes.

2. `com/sun/tools/javac/jvm/ClassReader.readEnclosingMethodAttr`
   * Before the call to `((ClassType)sym.type).setEnclosingType(...)`, normalize `sym.type` to the canonical `Type.ClassType`. This prevents the guard in the anonymous helper from being triggered during `EnclosingMethod` attribute processing.

3. `com/sun/tools/javac/jvm/ClassReader.readInnerClasses`
   * The `InnerClasses` attribute uses the same `setEnclosingType` logic (lines 3163–3165 in the JDK sources). We should normalize `member.type` here as well before `setEnclosingType` runs.

4. Add regression checks (e.g. assertions or logging under `LDK_DEBUG`) so we can verify at runtime that any ClassType reaching those call sites is the canonical `tsym.type` instance.

Implementing these overrides should re-create the state that HotSpot maintains internally and let the stock bytecode run without hitting the `UnsupportedOperationException` guard.

### Preferred path forward

Mirror HotSpot’s behaviour directly so we do **not** need any native overrides:

1. Ensure `classload` (`src/openldk.lisp` / `src/classfile.lisp`) always leaves `sym.type` pointing at the canonical `TypeClass` object shared with `sym.tsym.type`. Temporary `ClassReader$2` helpers should be treated as transient values and never written back into the symbol.
2. Audit the code that handles `Signature`, `EnclosingMethod`, and `InnerClasses` attributes to confirm they run after the canonical type has been restored. If necessary, explicitly reset `sym.type` to `sym.tsym.type` just before those handlers execute.
3. Add runtime assertions (guarded by `LDK_DEBUG`) that flag any case where a `ClassType` reaching `setEnclosingType` is not the canonical instance. This catches regressions early without relying on exceptions.

With those adjustments, OpenLDK’s state should match the JDK’s and the existing Java bytecode will operate without tripping the guard that throws `UnsupportedOperationException`.

### Root-cause investigation plan

To fix the underlying bug (instead of patching javac) we will:

1. **Observe the real JVM** – run a small Java harness under HotSpot that exercises `ClassReader` on a nested/anonymous class and log when `sym.type` is reassigned to the canonical `Type.ClassType`. Record the sequence of method calls around `classSigToType`, `readEnclosingMethodAttr`, etc.

2. **Instrument OpenLDK** – temporarily add logging to the bytecode interpreter (e.g., around the `putfield` that writes `Symbol.type`) so we can see exactly when `sym.type` changes while javac runs inside OpenLDK.

3. **Compare traces** – identify the first divergence between HotSpot and OpenLDK. Typical suspects: a missed method invocation, incorrect class-initialization order, or an implementation bug in our `Type.ClassType` helpers.

4. **Fix the interpreter/runtime** – once we’ve pinpointed the divergence, patch the relevant part of the bytecode translator/interpreter so javac follows the same path as on HotSpot (i.e., canonical `sym.type` is restored before `setEnclosingType` runs).

5. **Validate and clean up** – rerun `./openldk … javac` without any native overrides and confirm the `UnsupportedOperationException` is gone. Remove any temporary logging.

This workflow keeps OpenLDK aligned with upstream behaviour and lets us discard the stopgap override once the runtime bug is fixed.
