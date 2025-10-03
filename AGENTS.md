# Agent Instructions for OpenLDK Development

## Project Overview

OpenLDK is a Just-In-Time (JIT) compiler and runtime environment for Java,
implemented entirely in Common Lisp. It translates Java bytecode into Lisp
code, which is then compiled into native machine code by SBCL. Java classes
are mapped to CLOS (Common Lisp Object System) classes, enabling seamless
integration between Java and Common Lisp.

**Key Points:**
- Only Java 8 bytecode is supported
- SBCL only (Linux tested)
- Incremental JIT compilation: methods compiled lazily on first call
- Uses OpenJDK runtime libraries via JAVA_HOME environment variable
- Performance not competitive with modern JVMs - designed for embedding Java libraries in Lisp applications

## Environment Setup

**Required Environment Variables:**
- `JAVA_HOME`: Must point to Java 8 JRE (e.g., `/usr/lib/jvm/java-1.8.0-openjdk-1.8.0.432.b06-3.fc40.x86_64/jre`)
- `LDK_CLASSPATH`: Additional classpath elements (optional)
- `LDK_DEBUG`: Debug flags (optional, see Debugging section)

**Build Requirements:**
- SBCL (Steel Bank Common Lisp)
- Java 8 JDK/JRE
- ocicl package manager - run `ocicl install` before building

## Architecture

**Core Components (load order via openldk.asd):**
1. package.lisp - Package definitions and symbol imports
2. global-state.lisp - Global runtime state
3. debug.lisp - Debugging infrastructure
4. monitor.lisp - Threading/synchronization
5. context.lisp - Execution context
6. bootstrap.lisp - Initial runtime setup
7. opcodes.lisp - Java bytecode operation definitions
8. ir.lisp - Intermediate representation
9. bc-to-ir.lisp - Bytecode to IR translation
10. basic-block.lisp - Control flow analysis
11. codegen.lisp - Lisp code generation from IR
12. classfile.lisp - Class file parsing
13. native.lisp - Native method implementations
14. reflection.lisp - Java reflection API

**Key Design Patterns:**
- Initial class load generates CLOS definition with method stubs
- Method stubs trigger JIT compilation on first call
- Java exceptions mapped to Common Lisp conditions
- CLOS provides reflection capabilities
- SBCL backtrace used for security model

## Coding Style

**SPDX License Headers:**
Place SPDX identifier below copyright notice with a blank line in between:
```lisp
;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: OPENLDK; Base: 10 -*-
;;; SPDX-License-Identifier: GPL-3.0-or-later WITH Classpath-exception-2.0
;;;
;;; Copyright (C) 2023, 2024, 2025  Anthony Green <green@moxielogic.com>
```

**Package Prefixes:**
Don't prefix symbols with PACKAGE: if the symbol is imported into the
current package. Make an effort to import commonly-used symbols to avoid
prefixes. See src/package.lisp for current imports.

**Prefer:**
```lisp
(when-let ((x (find-thing)))
  (starts-with? "foo" x))
```

**Not:**
```lisp
(alexandria:when-let ((x (find-thing)))
  (str:starts-with? "foo" x))
```

**Common Lisp Conventions:**
- Use `*special-variables*` not `+constants+` for defvar/defparameter
- Use `#'function-name` instead of `(lambda (x) ...)` where possible
- Prefer `when-let` over nested `let`/`when` combinations
- Use `eql` for symbol comparison, not `eq`

## Linting

Lint this code with `ocicl lint openldk.asd`.

**Important guidelines:**
- Lint frequently as you fix problems to ensure you aren't introducing new issues
- The linter must not find any problems before committing
- Run `make` periodically to verify the build still works
- Address all linting errors and warnings before finalizing changes

## Commit Practices

Commit frequently with well-structured, multi-line commit messages:

```
Short summary of change (50 chars or less)

Detailed explanation of what was changed and why. Use proper
line wrapping at 72 characters. This is NOT just inserting \n
characters into commit strings, but properly formatted text.

- Use bullet points if listing multiple changes
- Reference issue numbers if applicable
- Explain the rationale behind non-obvious changes
```

## Development Workflow

1. **Before making changes:**
   - Understand the affected components and architecture
   - Identify which files need modification

2. **While making changes:**
   - Lint after each significant change using `~/git/ocicl/ocicl lint openldk.asd`
   - Run `make` to verify compilation (requires `JAVA_HOME=/usr/lib/jvm/temurin-8-jdk/jre/`)
   - Test relevant functionality

3. **Before committing:**
   - Final lint check with `~/git/ocicl/ocicl lint openldk.asd`
   - Verify `JAVA_HOME=/usr/lib/jvm/temurin-8-jdk/jre/ make` succeeds
   - Ensure all tests still pass (if applicable)
   - Write a descriptive multi-line commit message

## Debugging

Set `LDK_DEBUG` environment variable to enable debug output:

- `b` - trace bytecode compilation
- `c` - dump all Lisp code prior to evaluation
- `t` - trace method entry/exit at runtime
- `T` - trace method entry/exit with arguments and return values
- `s` - start slynk server at startup (port 2025)
- `u` - unmuffle the Lisp compiler
- `x` - trace opcode execution (use with `t`)

Example: `LDK_DEBUG=bctux ./openldk Hello`

## Testing

Run `make check` to execute the dejagnu-based test suite.

The test suite includes:
- Basic functionality tests in `testsuite/`
- GCJ compatibility tests in `testsuite/gcj/`
- Mauve test suite integration in `testsuite/mauve/`
- Expected failures tracked in `testsuite/expected-failures.txt`

## Dependencies

Core libraries (managed by ocicl):
- cl-annot, whereiseveryone.command-line-args, flexi-streams, zip, str
- defclass-std, fast-io, bitio, pathname-utils, cl-store
- trivial-backtrace, fset, bordeaux-threads, float-features
- local-time, closer-mop, slynk, file-attributes, trivial-garbage
- precise-time, trivial-gray-streams, cl-murmurhash


## Hacking

The source for JDK8 is found in ~/git/jdk.  Use it as a reference for
how this is supposed to work.

