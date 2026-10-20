# Running ABCL (Armed Bear Common Lisp) on OpenLDK

## Overview

ABCL is a Common Lisp implementation that runs on the JVM. Getting it to run
on OpenLDK is a significant milestone: a Lisp running on a Lisp-based JVM.

See also: https://github.com/atgreen/openldk/issues/4

## Setup

### Prerequisites

- OpenLDK binary (built from this repo with `make`)
- JDK 25, with `JAVA_HOME` pointing at it
- ABCL 1.9.2 jar (class version 52.0, which OpenLDK loads as-is)

### Getting the ABCL jar

Download the binary release:
```bash
curl -sL "https://abcl.org/releases/1.9.2/abcl-bin-1.9.2.tar.gz" | tar xz -C /tmp/
cp /tmp/abcl-bin-1.9.2/abcl.jar lib/abcl/
```

`lib/abcl/abcl.jar` is in `.gitignore`.

### The Thread Problem

ABCL's stock `org.armedbear.lisp.Main` spawns a new thread with a 4MB stack
to run the interpreter:

```java
new Thread(null, r, "interpreter", 4194304L).start();
```

OpenLDK needed the interpreter to run on the **main thread**. Rather than
patching ABCL itself, we wrote a custom entry point: `lib/abcl/ABCLMain.java`
in the `org.armedbear.lisp` package. This calls
`Interpreter.createDefaultInstance(args)` and `interpreter.run()` directly.

### Compiling the custom entry point

`javac` can compile it with the same JDK 25 (`java -m jdk.compiler/...`
works where only a JRE-style `bin/` is installed):
```bash
$JAVA_HOME/bin/javac -cp lib/abcl/abcl.jar -d lib/abcl lib/abcl/ABCLMain.java
```

This creates `org/armedbear/lisp/ABCLMain.class` under `lib/abcl/`.

### A warm image

`make abcl` builds `./abcl`, an image with ABCL already booted and its Java
code compiled.  It downloads the jar and compiles `ABCLMain` itself.  See
[IMAGES.md](IMAGES.md).

### Running

```bash
export JAVA_HOME=/usr/lib/jvm/java-25-openjdk
export LDK_CLASSPATH="lib/abcl/abcl.jar:lib/abcl"
./openldk org.armedbear.lisp.ABCLMain
```

The REPL reads from stdin, so a script can be piped in:
```bash
echo '(progn (print (+ 1 2)) (quit))' | ./openldk org.armedbear.lisp.ABCLMain
```

## Current Status (2026-10-20, JDK 25)

### What Works

- **Full bootstrap and a working REPL.** ABCL starts, prints its prompt,
  reads forms from stdin, evaluates them and exits cleanly with `(quit)`.
- Checked by piping in a script: recursive `defun` with bignum results,
  `defclass`/`defmethod`/`make-instance`, `handler-case` around `error`,
  `mapcar` with a lambda, `format` directives, `compile` (closures
  included), and Java interop
  (`java:jstatic "getProperty" "java.lang.System" "java.version"` returns
  `"25"`).
- All FASL-compiled Lisp files (`.cls`) load via `FaslClassLoader`,
  including the full CLOS implementation.
- Banner:
  ```
  Armed Bear Common Lisp 1.9.2
  Java 25 OpenLDK
  OpenLDK
  Low-level initialization completed in -0.002 seconds.
  Startup completed in 62.45 seconds.
  Type ":help" for a list of available commands.
  CL-USER(1):
  ```

### What Fixed the REPL

The February blocker (the REPL hanging on stdin, a stray `Go` on EOF) no
longer reproduces; it went away with the intervening runtime work. Moving
to JDK 25 then exposed two gaps, fixed alongside this update:

- `java.util.zip.ZipFile` (implemented in Lisp in `src/zip.lisp`) kept its
  state in JDK 8's `name` and `jzfile` fields, which JDK 25's class does not
  have, so ABCL died opening its own jar right after low-level
  initialization.
- `UnixFileSystem`'s natives `getLastModifiedTime0`, `getLength0` and
  `list0` were only defined under their JDK 8 names.

Compiling a function that contains a closure used to fail: ABCL compiles
it into several classes that refer to each other, which its in-memory
`MemoryClassLoader` defines only when asked through `findClass`.  OpenLDK
now resolves class references through the defining class loader's
`loadClass` when no other lookup finds them (`%resolve-class` in
`src/openldk.lisp`).

### Performance

It is slow, almost all of it first-use JIT compilation of Java methods:

| Phase | Time |
|---|---|
| OpenLDK start to ABCL banner | ~26 s |
| ABCL startup (its own figure) | ~62 s |
| Startup to first prompt | ~2 min |
| Each new kind of form, first time | 10–60 s |
| First `compile` (loads ABCL's compiler) | ~4 min |

Repeated forms are fast once their code paths are compiled.
Java single-dispatch (`java-generic-function` metaclass) was critical to
getting this far: the earlier PCL discrimination-net approach slowed down
exponentially with thousands of classes.

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

### src/zip.lisp
- `ZipFile` state (path and open zip) kept in a weak side table, since JDK 25's
  `ZipFile` has no `name`/`jzfile` fields; `ZipFile(String)` constructor;
  `getEntry` loads `ZipEntry` before instantiating it

### src/native/system.lisp
- JDK 25 names for `UnixFileSystem` natives: `getLastModifiedTime0`,
  `getLength0`, `list0` (returning 0 for a missing file, as the JDK does);
  the JDK 8 names delegate to them

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

## Known Issues / TODO

1. **Performance** - see the table above; startup to first prompt is about
   3.5 minutes, and the first `compile` about 4 more.  The warm image from
   `make abcl` starts in under a second ([IMAGES.md](IMAGES.md))
2. **Thread support** - ABCL's stock `org.armedbear.lisp.Main`, which runs
   the REPL on a new thread, exits at once with status 0 and no output on
   JDK 25: the interpreter thread never runs.  `ABCLMain` is still needed
3. **Missing file-system natives** - `createDirectory0`, `rename0`,
   `createFileExclusively0`, `setLastModifiedTime0`, `setPermission0`,
   `setReadOnly0`, `getSpace0` and `getNameMax0` are not implemented, so
   Lisp code that creates directories, renames files or makes temp files
   will fail
