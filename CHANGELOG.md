# Changelog

All notable changes to OpenLDK are documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

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
