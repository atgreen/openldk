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

## Current Status (2026-02-22)

### What Works

- **Full ABCL bootstrap completes successfully**
- Custom ABCLMain entry point runs the interpreter on the main thread
- ABCL loads 3700+ classes successfully (JDK + ABCL core + CLOS + compiled Lisp)
- `Interpreter.createDefaultInstance()` and `initializeLisp()` run to completion
- ABCL's bootstrap Lisp file (`boot.lisp`) is fully loaded
- All FASL-compiled Lisp files (`.cls`) load via `FaslClassLoader`:
  - `macros`, `read-circle`, `early-defuns`, `defstruct`, `defpackage`, etc.
  - Full CLOS implementation (`clos_1` through `clos_1642`)
  - `print_object`, `format`, `ldb`, etc.
- ABCL banner prints with both init phases completing:
  ```
  Armed Bear Common Lisp 1.9.2
  Java 8.0 OpenLDK
  OpenLDK
  Low-level initialization completed in -0.001 seconds.
  Startup completed in 42.173 seconds.
  ```

### Current Blocker

**REPL stdin interaction**

After full startup, `interpreter.run()` enters the REPL but hangs waiting
for stdin. When stdin is `/dev/null`, the REPL eventually throws an
`org.armedbear.lisp.Go` exception (ABCL's `tagbody`/`go` control transfer)
which escapes to the top level. With a pipe or FIFO, the REPL simply hangs
without producing a prompt or processing input.

The `--batch` and `--eval` flags are passed through to ABCL's
`Interpreter.createDefaultInstance(args)` but don't appear to take effect,
suggesting the argument processing or stream setup during `interpreter.run()`
has an issue (possibly related to OpenLDK's stdin stream implementation).

### ABCL `.cls` Extension

ABCL stores compiled Lisp code as `.cls` files inside its jar (not `.class`).
For example: `org/armedbear/lisp/destructuring_bind_10.cls`. The classpath
loader tries both `.class` and `.cls` extensions when looking up entries in
jar files.

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
LDK_DEBUG=L ./openldk org.armedbear.lisp.ABCLMain       # loading + compile timing
```

## ABCL Bootstrap Sequence

1. `ABCLMain.main()` -> `Interpreter.createDefaultInstance(args)`
2. `Interpreter.initializeLisp()` - initializes the Lisp runtime
3. `Load.loadSystemFile("boot.lisp")` - loads the core Lisp system
4. `boot.lisp` loads dozens more Lisp files via `loadSystemFile`:
   - `setf`, `fdefinition`, `macros`, `read-circle`, `early-defuns`, etc.
5. Many of these are FASL files (`.abcl` format) containing compiled `.cls`
   class files that get loaded via `FaslClassLoader`
6. Full CLOS bootstrap: 1600+ `clos_*` compiled classes
7. `interpreter.run()` starts the REPL

Each loaded Lisp file triggers class loading and JIT compilation for the
Java classes implementing ABCL's built-in functions.

## Performance Notes

- Full startup takes ~42 seconds (3700+ classes JIT-compiled)
- Java single-dispatch optimization (`java-generic-function` metaclass)
  was critical for reaching this point — the previous PCL discrimination-net
  approach caused exponential slowdowns with thousands of classes
- `kill -3` is essential for diagnosing apparent hangs during compilation

## Known Issues / TODO

1. **REPL stdin interaction** - REPL hangs after startup; `--eval`/`--batch`
   flags don't appear to work; `Go` exception on EOF
2. **Thread support** - ABCL may try to create threads during later use
3. **Performance** - 42s startup is acceptable but could improve with
   lazy/deferred compilation
