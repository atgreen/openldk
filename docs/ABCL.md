# Running ABCL (Armed Bear Common Lisp) on OpenLDK

## Overview

ABCL is a Common Lisp implementation that runs on the JVM. Getting it to run
on OpenLDK is a significant milestone: a Lisp running on a Lisp-based JVM.

See also: https://github.com/atgreen/openldk/issues/4

## Setup

### Prerequisites

- OpenLDK binary (built from this repo)
- Java 8 JDK (for rt.jar)
- ABCL 1.9.2 jar (already Java 8 / class version 52.0)

On this system:
```
JAVA_HOME=/home/linuxbrew/.linuxbrew/Cellar/openjdk@8/1.8.0-482/libexec/jre
JAVA8_JAVAC=/home/linuxbrew/.linuxbrew/Cellar/openjdk@8/1.8.0-482/libexec/bin/javac
```

### Getting the ABCL jar

Download the binary release:
```bash
curl -sL "https://abcl.org/releases/1.9.2/abcl-bin-1.9.2.tar.gz" | tar xz -C /tmp/
cp /tmp/abcl-bin-1.9.2/abcl.jar lib/abcl/
```

The release jar is already compiled for Java 8 (class version 52.0).

### The Thread Problem

ABCL's stock `org.armedbear.lisp.Main` spawns a new thread with a 4MB stack
to run the interpreter:

```java
new Thread(null, r, "interpreter", 4194304L).start();
```

OpenLDK needs the interpreter to run on the **main thread**. Rather than
patching ABCL itself, we wrote a custom entry point: `lib/abcl/ABCLMain.java`
in the `org.armedbear.lisp` package. This calls
`Interpreter.createDefaultInstance(args)` and `interpreter.run()` directly.

### Compiling the custom entry point

```bash
cd lib/abcl
$JAVA8_JAVAC -source 1.8 -target 1.8 -cp abcl.jar -d . ABCLMain.java
```

This creates `org/armedbear/lisp/ABCLMain.class` under `lib/abcl/`.

### Running

```bash
export JAVA_HOME=/home/linuxbrew/.linuxbrew/Cellar/openjdk@8/1.8.0-482/libexec/jre
export LDK_CLASSPATH="lib/abcl/abcl.jar:lib/abcl"
./openldk org.armedbear.lisp.ABCLMain
```

## Current Status (2026-02-20)

### What Works

- Custom ABCLMain entry point runs the interpreter on the main thread
- ABCL loads 600+ classes successfully
- `Interpreter.createDefaultInstance()` runs
- `Interpreter.initializeLisp()` starts executing
- ABCL's bootstrap Lisp file (`boot.lisp`) is loaded and parsed
- ABCL's Lisp reader/evaluator actually executes Lisp forms
- Further Lisp files loaded: `macros`, `read-circle`, `early-defuns`, etc.
- Macro expansion (e.g., `destructuring-bind`) works
- ABCL banner prints:
  ```
  Armed Bear Common Lisp 1.9.2
  Java 8.0 OpenLDK
  OpenLDK
  Low-level initialization completed in -0.001 seconds.
  ```
- ABCL begins loading FASL-compiled Lisp files via `FaslClassLoader`

### Current Blockers

**1. Null class reference in codegen (`slot-value NIL 'name`)**

When loading `.cls` files from ABCL's jar (ABCL uses `.cls` for compiled
Lisp class files), some classes reference other classes that can't be found.
The `classload` returns NIL, which propagates to codegen where
`(slot-value nil 'name)` crashes.

Fix (in `src/codegen.lisp`): Check for null class in `ir-new` codegen and
generate `NoClassDefFoundError` instead of crashing.

**2. "nonexistent tag" compiler error**

SBCL fails compiling generated Lisp code for methods like
`destructuring_bind_10.execute()`. The generated `tagbody` code references
`|branch-target-23|` but the label was never emitted because the dead-code
eliminator incorrectly skipped it.

**Root cause**: The bytecode-to-IR pass detects dead code after unconditional
branches (GOTO, RETURN, ATHROW). When a GOTO at PC 20 jumps to PC 43, the
code at PC 23 is marked as dead. But a later instruction at PC 60 branches
backward to PC 23 (a loop). Since the backward branch hasn't been processed
yet when PC 23 is encountered, the stack-state-table has no entry for it,
and the code is incorrectly eliminated.

**Fix needed**: Pre-scan bytecode for all branch targets before the main IR
pass. Any PC that is a branch target should never be treated as dead code.
A `%find-branch-targets` function has been written (in the full working tree
changes) but causes build failures when the dead code recovery generates
invalid IR for some JDK methods. Needs more careful integration.

### ABCL `.cls` Extension

ABCL stores compiled Lisp code as `.cls` files inside its jar (not `.class`).
For example: `org/armedbear/lisp/destructuring_bind_10.cls`. The classpath
loader needs to try both `.class` and `.cls` extensions when looking up
entries in jar files.

Fix (in `src/classpath.lisp`): Try `.cls` as fallback in `jar-classpath-entry`.

## Changes Made for ABCL Support

### src/classfile.lisp
- Warning when `classload` returns NIL (instead of silently proceeding)

### src/classpath.lisp
- Try `.cls` extension in addition to `.class` when loading from jars

### src/codegen.lisp
- Null class check in `ir-new` codegen: generate `NoClassDefFoundError`

### src/openldk.lisp
- `sb-c:compiler-error` handler in `%eval`: logs error, dumps generated
  code to `/tmp/abcl-codegen-error.lisp` for debugging
- Recursive compilation detection: track compiling thread identity (not
  just boolean `t`) to detect same-thread recursion vs different-thread
  compilation
- `handler-case` around `%eval` in `%compile-method`: catch compiler errors
  and skip the method (log which method failed) instead of crashing

### lib/abcl/ABCLMain.java
- Custom entry point that runs ABCL on the main thread

## Debugging Tips

### SIGQUIT thread dump
```bash
kill -3 $(pgrep -f ABCLMain)
```
This dumps all thread stacks to stderr. Useful to see where compilation is
stuck (often in `SB-WALKER::WALK-METHOD-LAMBDA` for large methods).

### Verbose output
```bash
./openldk -verbose:class org.armedbear.lisp.ABCLMain    # class loading
./openldk -verbose:compile org.armedbear.lisp.ABCLMain  # method compilation
```

### Codegen debugging
If a "nonexistent tag" compiler error occurs, the generated Lisp code is
dumped to `/tmp/abcl-codegen-error.lisp`.

## ABCL Bootstrap Sequence

1. `ABCLMain.main()` -> `Interpreter.createDefaultInstance(args)`
2. `Interpreter.initializeLisp()` - initializes the Lisp runtime
3. `Load.loadSystemFile("boot.lisp")` - loads the core Lisp system
4. `boot.lisp` loads dozens more Lisp files via `loadSystemFile`:
   - `setf`, `fdefinition`, `macros`, `read-circle`, `early-defuns`, etc.
5. Many of these are FASL files (`.abcl` format) containing compiled `.cls`
   class files that get loaded via `FaslClassLoader`
6. Eventually `interpreter.run()` starts the REPL

Each loaded Lisp file triggers class loading and JIT compilation for the
Java classes implementing ABCL's built-in functions.

## Performance Notes

- Low-level init takes ~30s (JIT compilation of JDK + ABCL core classes)
- Large methods like `Stream.readString()` (37+ stack variables) cause slow
  SBCL compilation (visible as "hangs" during init)
- `kill -3` is essential for diagnosing apparent hangs

## Known Issues / TODO

1. **Dead code elimination for backward branch targets** - needs pre-scan
   pass (`%find-branch-targets` written but needs careful integration)
2. **Null class references** - some `.cls` classes reference missing classes
3. **Thread support** - ABCL may try to create threads during later init
4. **Performance** - JIT compilation of ABCL's many small compiled classes
   is slow; lazy compilation could help
