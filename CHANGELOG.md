# Changelog

All notable changes to OpenLDK are documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

### Added

- An OpenJDK jtreg integration for running upstream `java.base` regression
  tests against OpenLDK, including a tested-JDK shim, a bounded smoke-test
  target, and the WhiteBox VM-property support needed by the harness.
- Support for `ToolProvider.getSystemJavaCompiler()` and
  `getSystemDocumentationTool()`, allowing Java code and test infrastructure to
  invoke the stock JDK compiler and documentation tool under OpenLDK.
- Native support for the Linux `Files.copy` fast path, boot-loader package
  enumeration, class-loader assertion directives, and the common no-deadlock
  result from `ThreadMXBean` deadlock queries.
- Java module-system launching and class loading now support `--add-opens`,
  `--limit-modules`, automatic modules, named-module main classes, and
  `--patch-module` classes defined by their module's real loader.
- Recursive custom system class loaders can initialize through
  `ClassLoader.getSystemClassLoader()` without observing a half-constructed
  loader.

### Changed

- The launcher now accepts the assertion, VM-selection, splash, and boot-class
  path options used by jtreg. It also rejects non-empty
  `java.endorsed.dirs` and `java.ext.dirs` settings, matching modern JDK
  launchers.
- Field reflection reuses OpenLDK's native class metadata instead of repeatedly
  reconstructing JDK reflection state, substantially reducing its overhead.

### Fixed

- Exceptions thrown after a nested try/catch no longer escape an enclosing
  Java exception handler when the continuation block is emitted on both the
  normal and handler paths.
- `Class.forName` now rejects malformed binary names that mix `/` and `.`
  separators instead of silently resolving them.
- `Unsafe.getChar` now returns an integer UTF-16 code unit consistently with
  the rest of OpenLDK, fixing arithmetic on values read from direct char
  buffers.
- `ClassLoader.registerAsParallelCapable()` now runs its stock Java
  implementation, so parallel-capable registration and superclass checks are
  recorded correctly.
- `Class.forName` validates array binary names and loads named-module classes
  from the module path without confusing them with same-named classpath
  classes.
- Generated lambda classes now inherit their caller's package, class loader,
  and module identity, including package-private access rights.
- Promoting a metadata-only class to a full definition now preserves its
  runtime roots, preventing live mirrors and loader metadata from being lost.

## [1.0.2] - 2026-10-31

### Fixed

- A static field that shadows an inherited instance field of the same name no
  longer collapses into a single class-allocated CLOS slot. Previously the
  static declaration's class allocation flipped the inherited instance slot, so
  every instance read the static's value instead of its own — this broke EC key
  generation and so every TLS handshake. Such a field now gets its own slot.

## [1.0.1] - 2026-10-31

### Added

- A single `initialize` entry point for embedding OpenLDK as a Common Lisp
  library. It runs the core bootstrap and the per-launch runtime setup (boot
  and application class loaders, the live classpath, system properties, and
  Java floating-point semantics) that the `openldk` launcher performs, so an
  embedder with no saved image gets working class loaders and native bridge in
  one call. Image builds call the core bootstrap directly, re-establishing
  per-launch state fresh at each launch.

### Fixed

- Creating a Java instance from Lisp now triggers initialization of its class
  per JVMS §5.5, running `<clinit>` the way the `new` bytecode does. Without it
  a Java object built from Lisp could see unset statics (e.g.
  `MMapDirectory.PROVIDER` null, causing an NPE in its constructor).

## [1.0.0] - 2026-10-31

First release. OpenLDK is a JIT compiler and Java runtime written in Common
Lisp. It translates Java bytecode into Common Lisp, which SBCL compiles to
native machine code, and maps Java classes to CLOS classes — so Java libraries
run inside SBCL with Java and Lisp code sharing a single process, and no
separate JVM.

### Added

- A bytecode-to-Common-Lisp translator and runtime that loads real Java
  libraries and runs them natively under SBCL.
- Java classes mapped to CLOS classes, callable from Lisp.
- An interop layer for driving Java from Common Lisp, and a preloaded `javacl`
  executable.
- Language interop systems for embedding Clojure, Scheme, Scala, Kotlin,
  Groovy, and ABCL through OpenLDK (see `docs/INTEROP.md`).

### Notes

- Development targets SBCL on Linux and JDK 25.
- See [cl-jdbc](https://github.com/atgreen/cl-jdbc) and
  [cl-lucene](https://github.com/atgreen/cl-lucene) for examples of Common Lisp
  libraries built on OpenLDK.
