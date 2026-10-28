# OpenLDK
## A JIT Compiler and Runtime for Java in Common Lisp

<p align="center" width="100%">
    <img width="33%" alt="A grinning three-eyed green alien waving a pennant reading LISP, beside Java's mascot Duke" src="docs/images/openldk-logo.png">
</p>

OpenLDK is a Just-In-Time (JIT) compiler and runtime environment for
Java, implemented entirely in Common Lisp. It bridges the gap between
Java and Common Lisp by incrementally translating Java bytecode into
Lisp, which is then compiled into native machine code for
execution. This unique approach allows Java classes to be seamlessly
mapped to Common Lisp Object System (CLOS) classes, enabling
effortless integration between Java and Common Lisp codebases.

## Key Features

- **Java Bytecode to Lisp Translation**: OpenLDK translates Java bytecode into Common Lisp, making it possible to execute Java code within a Lisp environment.
- **Native Machine Code Compilation**: The translated Lisp code is compiled into native machine code, ensuring efficient execution.
- **CLOS Integration**: Java classes are mapped to CLOS classes, allowing for smooth interoperability between Java and Common Lisp.
- **OpenJDK Runtime Libraries**: OpenLDK leverages OpenJDK's core runtime libraries, made possible by the GNU Classpath Exception to the GPL.

## Use Cases

OpenLDK is not designed to be a high-performance Java
runtime. Instead, it's for when you want to use SBCL, but need that
one Java library. It provides a practical solution for integrating
Java libraries into a Lisp-based workflow without the need for an
out-of-process Java runtime environment.

## Requirements

`openldk` has only been tested with sbcl.  It's possible that other
Common Lisp implementations could be made to work with it, but I am
only developing with sbcl for now.

`openldk` has only been tested in Linux.

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
into `$XDG_CACHE_HOME/openldk/` (`~/.cache/openldk/`).  The interop
languages below fetch their jars there too when you don't provide them.
Set `LDK_CACHE_DIR` to use another directory, or `LDK_OFFLINE=1` to never
download.

You can provide additional classpath elements through the
`LDK_CLASSPATH` environment variable.

The OpenLDK project uses [`ocicl`](https://github.com/ocicl/ocicl) for
package management.  Be sure to run `ocicl install` before building
`openldk`.

## Building

Running `make` produces two executables:
- `openldk` - the JIT compiler and runtime for Java
- `javacl` - Java's `javac` compiler transpiled to Lisp and dumped as an executable

Each image reserves the SBCL heap it was built with, 32 GB by default.
SBCL's startup grows with it, so `make DYNAMIC_SPACE_SIZE=4096` builds
images with a 4 GB heap that start faster, when your programs fit.

## Running

`openldk` takes the same arguments as `java`:
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

### Scoped values and structured concurrency

Scoped values support nested bindings, restoration after exceptions, and thread
isolation. Structured task scopes support scoped-value inheritance, cancellation,
timeouts, owner checks, and waiting for child cleanup. Structured concurrency is
a Java 25 preview API: compile with `javac --enable-preview --release 25` and run
with `openldk --enable-preview`. Preview class files are rejected without the flag;
only the Java 25 preview class-file version is accepted.

Run `make check-concurrency` for JDK/OpenLDK comparisons. These checks use the
existing virtual-thread backend; they do not establish virtual-thread scalability
or validate the optional SBCL fiber backend.

To launch Eclipse on OpenLDK, optionally download the SDK, or repeat the
headless editor/build check, see [the Eclipse runner](test/eclipse/README.md).

### JNI libraries

OpenLDK can load ordinary third-party JNI libraries alongside its existing
Common Lisp native implementations. Build the bridge with `make jni` (also
included in `make`). This requires a C compiler, Python 3, JDK 25 headers, and
the libffi development package. The current bridge targets 64-bit little-endian
Linux and uses the platform C ABI.

Java code can use `System.load("/absolute/path/libexample.so")`, or
`System.loadLibrary("example")` with `-Djava.library.path=/path/to/libraries`.
The bridge is loaded lazily from `libopenldk-jni.so` in the build directory;
set `OPENLDK_JNI_BRIDGE` to its absolute path when relocating an executable.
It uses standard JNI headers and does not load HotSpot's JDK libraries.
Existing Lisp native implementations retain precedence.

The implementation supports native symbol lookup and registration, loader
ownership, initialization hooks, local/global/weak references, object creation,
Java callbacks, fields, reflection, exceptions, strings, primitive/object arrays,
direct buffers, monitors, and native-thread attachment. Strings and primitive
arrays use native copies with release/copy-back semantics. A native thread keeps
its Java identity across callbacks, including daemon status and monitor ownership.

JNI libraries remain loaded for the process lifetime; class-loader unloading and
`JNI_OnUnload` are not implemented. Creating or destroying a standalone JVM via
the Invocation API is not supported. Saved images must load JNI libraries after
restart: saving with loaded JNI libraries is rejected because arbitrary native
library state cannot be preserved in an SBCL image. JNI compatibility is tested
with the included C/C++ probes and zstd-jni; it is not a claim of complete JNI
conformance for every library.

### Foreign Function & Memory API

OpenLDK supports native linking through the JDK 25 Java fallback linker and
its own libffi bridge. It does not load the JDK's native fallback-linker library.
`Linker.nativeLinker()` supports downcalls and arena-owned upcall stubs,
including callbacks from native threads. Symbol lookup supports system symbols,
arena-owned C libraries, and libraries loaded through JNI.

The integration probes cover scalar types, pointers, structs passed and returned
by value, variadic calls, errno capture, heap-array arguments to critical calls,
and arena lifetime and thread checks. Heap arguments use native copies, sharing
copies for aliases. The fallback linker rejects unions passed by value. The
current bridge targets 64-bit little-endian Linux; other platforms have not
been validated. Run `make check-foreign` to compare the probes with JDK 25.

## How it Works

`openldk` reads class and jar files, and translates them into lisp
code, which sbcl's compiler then turns into machine code for
execution.

Java classes and objects are mapped to CLOS classes.  The exception
hierarchy is mirrored by a Common Lisp condition hierarchy.  CLOS
provides everything we need to support reflection, and SBCL's
backtrace capabilities allow us check calling classes to support
Java's security model.

The first time we read a class definition, we generate a CLOS
definition, and install stubs for its methods.

For example...

```
public class demo
{
    public static int add (int x, int y)
    {
        return x + y;
    }

    public static int x;
    public static int y;

    public static void main (String[] args)
    {
        System.out.println ("Hello, World");
        System.out.println (add (x, y));
    }
}
```

...becomes...

```
(progn
 (defclass openldk.app::|demo| (openldk::|java/lang/Object|)
   ((openldk::|x| :initform 0 :allocation :class)
    (openldk::|y| :initform 0 :allocation :class)))
 (defparameter openldk.app::|+static-demo+|
   (make-instance 'openldk.app::|demo|)))
```

Java fields become slots, with static fields allocated on the class
and reached through the `+static-demo+` instance.  Methods are named by
their full signature, so overloads stay distinct.  Instance methods are
methods of a generic function shared by every class that defines that
signature; static methods are plain functions.

Each method starts as a stub, a closure installed without running the
compiler: `openldk.app::|demo.add(II)I|` is a function that compiles
`add` and then calls the compiled definition, which has replaced it.
This is how we support incremental JIT compilation: most methods of most
classes are never called, and never compiled.

When the `add` method is called, the compiler reads `add`'s bytecode
and generates the following:

```
(defun openldk.app::|demo.add(II)I| (openldk::|arg0| openldk::|arg1|)
  "bridge=NIL"
  (let ((openldk::|local-0| openldk::|arg0|)
        (openldk::|local-1| openldk::|arg1|))
    (openldk::with-java-conditions (:arrays nil)
      (block nil
        (tagbody
          (return-from openldk.app::|demo.add(II)I|
            (let ((openldk::value2 openldk::|local-1|)
                  (openldk::value1 openldk::|local-0|))
              (sb-c::mask-signed-field 32
                                       (+ openldk::value1
                                          openldk::value2)))))))))
```

Java's operand stack and local variable slots become Lisp variables,
and copy propagation removes the ones that only shuffle values around.
`mask-signed-field` gives Java's 32-bit wrap-around semantics, and with
typed operands SBCL compiles it to a single machine add.
`with-java-conditions` turns Lisp errors that mean null dereferences or
bad array indexes into Java's `NullPointerException` and
`ArrayIndexOutOfBoundsException`.

### The code cache

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

## Hacking

### Testing

Run `make check` to run through the
[dejagnu](https://www.gnu.org/software/dejagnu/dejagnu)-based testsuite.
`make check-regression` runs its core suites and fails if any test that
passes in `testsuite/baseline.sum` no longer does: run it before sending
a change. `make check-reference-image` checks collection and queue delivery
of references created before saving an application image.
`make check-verifier` compares generated valid and malformed classfiles with
HotSpot, checking both acceptance and Java verification exceptions.
`make check-jni` builds C/C++ JNI libraries and runs identical integration probes
on HotSpot and OpenLDK. `make check-jni-zstd` also downloads a checksum-pinned
zstd-jni release and tests byte-array and direct-buffer compression round trips.

### Debugging

The `openldk` runtime will generate useful debug info if you set your
`LDK_DEBUG` environment variable.  `LDK_DEBUG` should be set to a
string of characters that are interpreted as below:

- `b` - trace bytecode compilation
- `c` - dump all Lisp code prior to evaluation
- `e` - trace Java exceptions as they are caught
- `l` - show class loading (prints "; LOADING classname" for each class)
- `L` - show class loading and method compilation with timing
- `p` - debug propagation
- `s` - start a slynk server at startup (port 2025)
- `t` - trace method entry/exit at runtime
- `T` - trace method entry/exit with arguments and return values
- `u` - unmuffle the Lisp compiler
- `x` - trace opcode execution (use with `t`)

More specifically, running `LDK_DEBUG=bcltux openldk Hello` will enable
most debug output while running `Hello`.

## Status

OpenLDK can run non-trivial Java applications:

- **javac** - Java's own compiler runs on OpenLDK and can compile Java
  source to class files.  A pre-warmed `javacl` image is built by `make`.
- **Kawa** - The [Kawa](https://www.gnu.org/software/kawa/) Scheme
  implementation (a JVM language) boots and runs its REPL on OpenLDK.
  `make kawa` builds a warm image that starts in seconds.
- **ABCL** - [Armed Bear Common Lisp](https://abcl.org/) 1.9.2 boots
  on OpenLDK and runs its REPL: CLOS, conditions, the compiler and Java
  interop all work.  Cold, it takes minutes to the first prompt (most of
  it first-use JIT compilation); `make abcl` builds a warm image that
  starts in under a second.  See [docs/ABCL.md](docs/ABCL.md).
- **Clojure** - [Clojure](https://clojure.org/) 1.12.4 boots and runs its
  REPL, including protocols, records, atoms and futures.  Cold startup
  takes about six minutes; `make clojure` builds a warm image with
  `clojure.core` loaded that starts in under a second.
- **JScheme** - Peter Norvig's [JScheme](https://norvig.com/jscheme.html)
  passes its R4RS test suite.  `make jscheme` builds a warm image.
- **Scala** - The [Scala](https://www.scala-lang.org/) 2.13 compiler and
  its REPL run: `scala -e` scripts, and the REPL's script engine, which
  `openldk/scala` evaluates Scala through.
- **Kotlin** - The [Kotlin](https://kotlinlang.org/) 2.2 compiler compiles
  Kotlin to class files that run the same as `kotlinc`'s.
- **Groovy** - [Groovy](https://groovy-lang.org/) 4 runs scripts, with
  closures, metaclasses and invokedynamic, and `@CompileStatic` classes.
- **Spring Boot** - [Spring Boot](https://spring.io/projects/spring-boot)
  4 applications run, including
  [Spring PetClinic](https://github.com/spring-projects/spring-petclinic)
  serving its pages from embedded Tomcat, with Hibernate and H2.  The
  sample application in `bench/spring-boot` starts in about two minutes
  cold, nearly all of it first-use compilation, and in about 35 seconds
  once the code cache holds its methods; a snapshot image starts it in
  about 80 ms (see below).
- **Quarkus** - [Quarkus](https://quarkus.io/) applications run, and start
  in about 70 ms from a snapshot image.

The warm images are described in [docs/IMAGES.md](docs/IMAGES.md).

### Starting Spring Boot in a fraction of a second

A GraalVM native image starts Spring Boot quickly by compiling it ahead of
time.  OpenLDK gets there differently: it starts Boot once, at build time,
and saves the running application, much as a CRaC checkpoint does.  Give
the application a main class that starts Boot the first time and only
reruns the application's work after that:

```java
public class Snapshot {
    static ConfigurableApplicationContext ctx;

    public static void main(String[] args) throws Exception {
        if (ctx == null) {
            ctx = SpringApplication.run(MyApp.class, args);
        } else {
            for (CommandLineRunner runner : ctx.getBeansOfType(CommandLineRunner.class).values())
                runner.run(args);
        }
    }
}
```

Then warm it up twice and save it with `:reset-classes nil`, so the
started context stays in the image:

```lisp
(openldk::dump-warm-image "myapp" "com.example.Snapshot"
                          :classpath (uiop:getenv "CLASSPATH")
                          :warmups (list nil nil)
                          :reset-classes nil)
```

Set `CLASSPATH` to the application's classes followed by every jar it
needs, one by one: OpenLDK doesn't expand `lib/*`.  Then load OpenLDK into
an SBCL with a large heap and run this, as
[docs/IMAGES.md](docs/IMAGES.md) shows.  The first warmup starts Boot and
the second compiles the runners' code; running `./myapp` then skips Boot's
startup entirely.  Threads don't survive in an image, so this suits
command-line applications.  A web server such as Tomcat has to start its
threads again on each run.

`make bench-spring-boot` builds this for a sample application and times it
against HotSpot, a JDK AOT cache and a GraalVM native image.  On the
development host the snapshot started in 82 ms, against 72 ms for the
native image and 3.4 s for `java -jar`.  See
[bench/spring-boot/README.md](bench/spring-boot/README.md), and
`make bench-quarkus` for Quarkus.

Clojure, Kawa, JScheme, Scala, Kotlin, Groovy and ABCL can also be used
from Lisp, in the same process: each `openldk/<language>` system evaluates
code, calls the language's functions, lets it call Lisp functions, and
converts values both ways, and `#@scala{ ... }`, `#@kt{ ... }`,
`#@groovy{ ... }` and friends embed each language's source, as it writes
it, in Lisp code.  `make sbcl-<language>` builds an SBCL with a language
started, and `make sbcl-polyglot` one with all of them.  See
[docs/INTEROP.md](docs/INTEROP.md).

OpenLDK compiles Java to typed Lisp that SBCL compiles at `(speed 3)`,
and caches what it compiles, but it is not competitive with modern Java
implementations, and is not meant to be: HotSpot starts the Spring Boot
sample above in about three seconds.  OpenLDK is meant to fill the gap
for when you want to code in Common Lisp, but you need that one Java
library.  See [cl-log4j](https://github.com/atgreen/cl-log4j) for an
example of wrapping a Java library for Common Lisp.

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

Here's an incomplete list of what's not implemented:
- complete JVM bytecode verification
- Java finalization and object resurrection

Author and License
-------------------

OpenLDK was written by [Anthony
Green](https://github.com/atgreen), and is distributed under the terms
of the GNU General Public License, Version 3, modified by the
"CLASSPATH" exception to the GPL.  See [COPYING](COPYING) for details.
