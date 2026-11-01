# Changelog

All notable changes to OpenLDK are documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

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
