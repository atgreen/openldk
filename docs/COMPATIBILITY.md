# Java Compatibility

OpenLDK targets JDK 25 and uses its Java runtime libraries. The focused
comparisons below describe tested behavior, rather than complete JVM or API
conformance. Known test failures are tracked in
[expected-failures.txt](../testsuite/expected-failures.txt).
For JNI and FFM, see [native libraries](NATIVE.md).

## Records, sealed types, and patterns

Record-generated equality and hashing use each component's declared Java type,
including signed-zero/NaN floating semantics and reference identity for arrays.
Record reflection includes empty records, generic components, annotations, and
accessors. Sealed types expose their permitted direct subclasses without
initializing them; class loading rejects unpermitted subclasses and implementations
and checks defining-loader and package/module membership.

Run `make check-language` for comparisons with JDK 25 using class files compiled
for Java 16 (records), 17 (sealed types), 21 (record patterns and pattern switches),
and 25. The suite also covers Java 25 primitive patterns with `--enable-preview`,
including exact conversions, guarded switches, nulls, and qualified enum cases.

## Modules, Java 25 constructors, and runtime defaults

The boot module graph is ready before application code runs. Application module
layers associate packages with their defining loaders and named modules.
Reflection uses the JDK's access checks for methods, constructors, and fields,
including exports, opens, and authorized accessibility overrides. Ordinary
MethodHandles lookups retain their caller and access modes. Calls, allocations,
field references, and class literals check package/module visibility; a failed
resolution stays failed when a read edge is later added. Full bytecode verification remains incomplete.

Run `make check-compat` for comparisons with JDK 25 covering:

- Named module layers, read/export/open updates, concealed JDK classes, reflective
  access failures, and method-handle lookup permissions.
- Java 25 module imports and flexible constructor bodies, including early field
  assignments, delegation, and failures, in compiled and source-file launches.
- Java 17/25 strict floating-point behavior: rounding, overflow, subnormals,
  signed zero, NaNs, truncating remainders, and saturating integer conversions.
- Java 18/25 UTF-8 defaults, non-ASCII filenames, readers/writers, explicit
  charsets, and the `-Dfile.encoding=ISO-8859-1` launch override. Native pathname
  encoding remains UTF-8 independently of the content encoding.

Floating remainder uses allocation-free integer significand reduction in Lisp.
Run `make check-remainder` for 201,058 JDK-generated cases and
`make bench-remainder` for comparisons with libm and the exact rational oracle.
See [remainder implementation and measurements](REMAINDER.md).

## Modern library APIs

Run `make check-modern` to compare these library behaviors with JDK 25:

- Class-File API (Java 24/25): generate and transform executable methods with
  branches, stack maps, and exception handlers; resolve dynamic constants with
  defining-class identity, shared constant-pool entries, and cached failures.
- Gatherers (Java 24/25): fixed/sliding windows, scan/fold, composition, parallel
  reduction, short-circuiting, and bounded concurrent mapping.
- Sequenced collections (Java 21/25): backed reversed views, endpoint mutations,
  encounter order, entry snapshots, and unmodifiable wrappers.
- Stable values (Java 25 preview): null contents, retries after supplier failures,
  recursive initialization rejection, platform/virtual thread races, and lazy
  suppliers, functions, lists, and maps. These require `--enable-preview`.

These APIs use the installed JDK's Java implementations. The comparisons cover
specific behaviors; they do not establish complete API or JVM conformance.

## Scoped values and structured concurrency

Scoped values support nested bindings, restoration after exceptions, and thread
isolation. Structured task scopes support scoped-value inheritance, cancellation,
timeouts, owner checks, and waiting for child cleanup. Structured concurrency is
a Java 25 preview API: compile with `javac --enable-preview --release 25` and run
with `openldk --enable-preview`. Preview class files are rejected without the flag;
only the Java 25 preview class-file version is accepted.

Run `make check-concurrency` for JDK/OpenLDK comparisons. These checks use the
existing virtual-thread backend; they do not establish virtual-thread scalability
or validate the optional SBCL fiber backend.

## Remaining limitations

Volatile fields and ordered Unsafe/VarHandle accesses use a conservative
shared synchronization protocol. Scoped Value caches are isolated per Java
thread. Weak, soft, and phantom references use SBCL weak pointers; a reference
processor delivers collected references to Java queues and Cleaner callbacks.
Soft references currently use an eager-clearing policy, rather than retaining
referents according to memory pressure.

Classfiles undergo structural checks before loading. Methods in classfile
versions 50 through 69 also undergo stack-map and bytecode type checking before
initialization or compilation, including operand/local types, control flow,
exception handlers, and uninitialized constructor objects. This is a partial
verifier: legacy type inference for older classfiles and complete JVM access
and linking constraints remain unimplemented. It is not yet a complete security
boundary for untrusted classfiles.

Other unimplemented features include:

- complete JVM bytecode verification
- Java finalization and object resurrection
