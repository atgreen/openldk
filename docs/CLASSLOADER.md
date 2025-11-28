
• Here are viable ways to add class‑loader identity, from smallest surface change to largest refactor:

  1. Name‑mangle by loader ID (minimal invasive)

  - Keep one global package, but prefix/suffix every Lisp symbol we intern for a class/method/field with an opaque loader ID (e.g., |clojure/core$dotimes@L123|, |<init>()@L123|).
  - All caches and hash tables (*ldk-classes-by-bin-name*, *java-classes-by-bin-name*, method stubs, invoke-special keys) are keyed on (loader-id . bin-name) instead of just bin-name.
  - invoke-special and compute-* stay as-is because the GF symbol itself is loader‑scoped via its name.
  - Pros: Minimal structural change; keeps current CLOS dispatch model.
  - Cons: Loader IDs leak into symbol names; cross-loader resolution (e.g., parent delegation) must carefully choose the correct loader ID when constructing names.

  2. Per-loader packages (cleaner symbol space)

  - Create a fresh package per class loader; intern all class/method symbols in that package. Keep existing naming (no mangling), but the package segregates loader identity.
  - Store class maps per loader (*classes-by-loader*, *java-classes-by-loader*).
  - Method symbols for the same binary name in different loaders are distinct because they live in different packages.
  - Pros: Avoids ugly name mangling; clearer separation; CLOS dispatch still works per-symbol.
  - Cons: More package plumbing; any place we intern or find-class needs the loader’s package.

  3. Per-loader GFs without symbol changes (dispatch table indirection)

  - Keep method symbols global, but don’t use them directly as GFs. Instead, keep a per-loader GF table keyed by (loader-id, method-symbol).
  - invoke-special looks up the GF object from that table instead of (symbol-function method-symbol).
  - Pros: Limits surface changes to dispatch lookup; symbols stay stable.
  - Cons: Touches more runtime paths; must ensure all defmethod/defun go to the right GF object (likely via a macro wrapper).

  4. Full loader-aware class objects

  - Thread class-loader through everything: class objects, method compilation keys, caches, reflection tables.
  - classload table keys become (loader-id . name).
  - invoke-special gets owner-class that already carries its loader; lookup uses that loader to pick the GF/method.
  - Pros: Closest to JVM semantics; future-proof for delegation and sealing.
  - Cons: Biggest refactor; requires auditing all global tables, caches, and symbol generation.

  Cross-cutting concerns (whichever approach):

  - Parent delegation: resolve open-java-classfile-on-classpath via the loader’s classpath + parent chain.
  - AOT/Ahead-of-time: AOT filenames need loader ID to avoid collisions.
  - Reflection/java/lang/Class: class identity (==) must include loader; hash/equals should be loader-aware.
  - Debug/printing: keep a stable printable name (bin name + loader ID) for diagnostics.
  - invoke-special ordering: once loader isolation exists, repeated stubs from another loader won’t collide with compiled methods in the current loader.

  Recommendation to start:

  - Choose option 2 (per-loader packages) as the balance of cleanliness and scope:
      - Add a loader structure carrying a package and maps.
      - Change classload to look up (loader, name) and intern symbols in loader-package.
      - Change codegen to use that package when interning method/field symbols.
      - Change invoke-special/find-class calls to the loader’s package.
  - Implement incrementally: first make classload return existing class by loader+name; then move symbol interning to a loader-specific package; finally adjust invoke-special to use symbol-function in that package.

  If you want, I can draft the skeleton for per-loader packages and the lookup changes in classload/codegen/invoke-special`.


› Explain how this would work in more detail.  The system class loader loads java.* classes.  Then, let's say I load classes with a different class loader, but need to call methods in java.*


• Here’s how per‑loader packages would work, and how inter‑loader calls to java.* would function:

  1. Representing loaders

  - Keep a loader object (we already have class-loader as a concept). Add:
      - id (unique integer/string)
      - pkg (a Common Lisp package dedicated to this loader)
      - parent (another loader or NIL)
      - classpath (list of classpath entries)
  - For the bootstrap “system” loader, create package OPENLDK.SYSTEM. For user loaders, create packages like OPENLDK.L<id>.

  2. Class identity

  - A class is keyed by (loader-id, binary-name). The hash tables that map names → class objects become per-loader or keyed by this tuple:
      - *ldk-classes* => hash-table keyed by loader → (name → class)
      - Likewise for *java-classes*.
  - classload now accepts a loader argument; it first checks that loader’s map. Parent delegation: if not found, ask the parent loader (like JVM) before loading locally.

  3. Symbol/package for classes and methods

  - When we emit a class, we intern the class symbol in the loader’s package: (intern binary-name loader-pkg).
  - Method symbols use the same loader package: (intern (lispize-method-name name+descriptor) loader-pkg).
  - Result: |<init>()| in SYSTEM loader is a different symbol than |<init>()| in a user loader, so methods/GFs don’t collide across loaders.

  4. Generic functions and defmethod

  - Because method symbols are package-qualified, each loader gets its own GFs (still one per signature per loader). CLOS dispatch stays the same; symbol-function will fetch the GF bound to that loader’s symbol.

  5. invoke-special / dispatch

  - invoke-special receives method-symbol and owner-symbol; these already carry the package (loader). So when it does (symbol-function method-symbol) it gets the correct loader-local GF. No cross-loader bleed.
  - The argument classes (find-class) are also interned in the loader’s package; the first specializer matches the right class object.

  6. Cross-loader calls (java.* from a user loader)

  - JVM rules: user loader delegates to parent (system) for java.*. We mimic this by:
      - When loading "java/..." from a non-system loader, first try parent loader (system). The class returned is the system loader’s class object and symbols.
      - So when user code calls a java/lang/String method, the method symbol is in SYSTEM package; the GF and methods live there; dispatch uses that GF.
  - Because the system loader’s classes are cached per loader, the user loader won’t create its own copy; it just reuses the parent’s (delegation).

  7. AOT / names / diagnostics

  - AOT filenames can include loader-id to avoid collisions.
  - Printing can show loader-id:binary-name for clarity.

  8. Migration steps (incremental)

  - Add loader arg everywhere we classload (default to system loader to keep current behavior).
  - Introduce per-loader package creation and pass it to intern in emit/codegen.
  - Update global maps to be per-loader (or tuple key).
  - Update invoke-special to rely on package-qualified symbols (likely already fine once interning changes).
  - Parent delegation for java.*: in classload, if name starts with java/, force system loader.

  This lets you load user classes in a different loader while calling java.* classes defined by the system loader, matching JVM semantics, without global symbol collisions.

