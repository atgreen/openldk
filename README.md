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
definition, with stubs for methods.

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
   (make-instance 'openldk.app::|demo|))
 (progn
  (openldk::%ensure-java-gf 'openldk::|<init>()V| 1)
  (defmethod openldk::|<init>()V| ((openldk::|this| openldk.app::|demo|))
    (openldk::%compile-method #<openldk::<class> demo> 1)
    (openldk::invoke-special 'openldk::|<init>()V| 'openldk.app::|demo|
                             (list openldk::|this|))))
 (defun openldk.app::|demo.add(II)I| (openldk::|arg1| openldk::|arg2|)
   (openldk::%compile-method #<openldk::<class> demo> 2)
   (openldk.app::|demo.add(II)I| openldk::|arg1| openldk::|arg2|))
 (defun openldk.app::|demo.main([Ljava/lang/String;)V| (openldk::|arg1|)
   (openldk::%compile-method #<openldk::<class> demo> 3)
   (openldk.app::|demo.main([Ljava/lang/String;)V| openldk::|arg1|)))
```

Java fields become slots, with static fields allocated on the class
and reached through the `+static-demo+` instance.  Methods are named by
their full signature, so overloads stay distinct.  Instance methods are
methods of a generic function shared by every class that defines that
signature; static methods are plain functions.

Note that the methods are all stubs that invoke the compiler and then
call the method again, now compiled.  This is how we support
incremental JIT compilation.

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
         |branch-target-0|
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

## Hacking

### Testing

See [cl-log4j](https://github.com/atgreen/cl-log4j) for an example of how
to wrap a Java library for Common Lisp usage.

Run `make check` to run through the
[dejagnu](https://www.gnu.org/software/dejagnu/dejagnu)-based testsuite.

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

The warm images are described in [docs/IMAGES.md](docs/IMAGES.md).

Clojure, Kawa, JScheme, Scala, Kotlin, Groovy and ABCL can also be used
from Lisp, in the same process: each `openldk/<language>` system evaluates
code, calls the language's functions, lets it call Lisp functions, and
converts values both ways, and `#@scala{ ... }`, `#@kt{ ... }`,
`#@groovy{ ... }` and friends embed each language's source, as it writes
it, in Lisp code.  `make sbcl-<language>` builds an SBCL with a language
started, and `make sbcl-polyglot` one with all of them.  See
[docs/INTEROP.md](docs/INTEROP.md).

The code is not optimized.  Even with heavy optimization, OpenLDK's
performance will not be competitive to modern Java implementations.
It is not meant to be competitive.  OpenLDK is meant to fill the gap
for when you want to code in Common Lisp, but you need that one Java
library.

Here's an incomplete list of what's not implemented:
- bytecode verification

Author and License
-------------------

OpenLDK was written by [Anthony
Green](https://github.com/atgreen), and is distributed under the terms
of the GNU General Public License, Version 3, modified by the
"CLASSPATH" exception to the GPL.  See
[COPYING](https://github.com/atgreen/OpenLDK/blob/main/COPYING)
for details.
