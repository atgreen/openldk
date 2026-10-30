# Building and Running OpenLDK

[Project overview](../README.md) · [Java compatibility](COMPATIBILITY.md)

## Requirements

OpenLDK is developed and tested with SBCL on Linux. Building the JNI bridge
also requires a C compiler, Python 3, JDK 25 headers, and the libffi development
package; see [Native Libraries](NATIVE.md).

`openldk` targets JDK 25. It reads JDK class files directly from the
JMOD files in `$JAVA_HOME/jmods/`, or — when those are absent, as in
"headless" JDK builds (e.g. Fedora's `java-25-openjdk-headless`) — from
the jimage container at `$JAVA_HOME/lib/modules`. Set `JAVA_HOME` to
point at your JDK 25 installation:
```
$ export JAVA_HOME=/usr/lib/jvm/java-25-openjdk
```

If `JAVA_HOME` is unset or isn't a JDK 25, `openldk` looks for one where
JDKs are usually installed (the `java` on your `PATH`, `/usr/lib/jvm`,
SDKMAN, and so on), and failing that downloads a pinned Temurin JDK 25
into `$XDG_CACHE_HOME/openldk/` (`~/.cache/openldk/`).  The [interop
languages](INTEROP.md) fetch their jars there too when you don't provide them.
Set `LDK_CACHE_DIR` to use another directory, or `LDK_OFFLINE=1` to never
download.

You can provide additional classpath elements through the
`LDK_CLASSPATH` environment variable.

The OpenLDK project uses [`ocicl`](https://github.com/ocicl/ocicl) for
package management.  Be sure to run `ocicl install` before building
`openldk`.

## Building

`make` requires `JAVA_HOME` to point at an installed JDK 25 with JNI headers.
Put its `bin` directory on `PATH` so `javac` is available. The runtime
discovery/download behavior above does not replace this build prerequisite.

Running `make` produces two executables:

- `openldk` - the JIT compiler and runtime for Java
- `javacl` - Java's `javac` compiler transpiled to Lisp and dumped as an executable

Each image reserves the SBCL heap it was built with, 32 GB by default.
SBCL's startup grows with it, so `make DYNAMIC_SPACE_SIZE=4096` builds
images with a 4 GB heap that start faster, when your programs fit.

## Running

`openldk` accepts Java-style launcher arguments:
```
$ ./openldk -cp classes:lib/foo.jar com.example.Main arg...
$ ./openldk -jar app.jar arg...
```
`-D<name>=<value>` sets a system property, and `-version` and `-help` do
what they do for `java`.  As on a JVM, `openldk` exits once `main` has
returned and every non-daemon thread has ended;
`LDK_THREAD_WAIT_SECONDS=N` stops waiting after N seconds.

Java 25 entry points are supported: `main(String[])` takes precedence over
`main()`, and either can be static or an instance method with public,
protected, or package access. Instance mains construct the launched class
using its non-private no-argument constructor. Inherited methods and interface
defaults are considered; static interface methods are not inherited. This
also works for compiled compact-source classes and JAR manifest entry points.

Launch source files directly with `openldk Hello.java [args...]`, including compact
sources and instance mains. OpenLDK uses the installed JDK compiler in a separate
process, then runs the resulting classes itself. Compiler output stays in a
private temporary directory and is removed on completion, errors, or System.exit.
The compiler helper is embedded in the executable; no source checkout is needed.
Source dependencies are compiled from the source root, and `-cp` supplies compiled
dependencies. Use `--source 25` (or `--source=25`) for scripts without a `.java`
extension; an initial shebang line is ignored. Annotation processing is disabled.
Source launches cannot be combined with `--aot`.
Dependencies must be discoverable during compilation; loading additional source
files dynamically through reflection is not implemented.

Run `make check-launcher` and `make check-source-launcher` to compare compiled
and source entry-point behavior with JDK 25.

Use `--add-exports java.desktop/sun.awt=ALL-UNNAMED` to export a module's
package to additional modules. The `--add-exports=module/package=targets`
form and repeated options are supported. Source launches pass these options
to the compiler as well as applying them to the runtime module graph.
Explicit `--source` releases retain the JDK compiler's restrictions on
exporting internal packages.

## Code cache

`openldk` keeps the methods it compiles in a cache, `code/` in the
download cache directory, so later runs load them instead of compiling
them again: the second run of a program typically starts two to three
times faster.  An entry is used only while the classes its code was
compiled against are unchanged, and each build of OpenLDK has entries of
its own.  Building an image uses the cache too, so the images built
from the same sources (`openldk`, `javacl`, `clojure` and the rest) share
the JDK methods their warmups compile.  Set `LDK_CODE_CACHE=off` to turn the cache off, or to a
directory to keep it there; `LDK_CODE_CACHE_STATS=1` prints what the
cache did when the program exits.  With `LDK_CODE_CACHE_VERIFY=1`, every
method is compiled anyway and any cache entry whose code differs from
the compiler's is reported, a check on the cache itself.
