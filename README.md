# OpenLDK

A JIT compiler and Java runtime in Common Lisp.

<p align="center" width="100%">
    <img width="33%" alt="A grinning three-eyed green alien waving a pennant reading LISP, beside Java's mascot Duke" src="docs/images/openldk-logo.png">
</p>

OpenLDK runs Java libraries inside SBCL. It translates Java bytecode into
Common Lisp, which SBCL compiles to native machine code, and maps Java
classes to CLOS classes. Java and Lisp code share a process.

It's for when you want to write Common Lisp but need a Java library.
See [cl-log4j](https://github.com/atgreen/cl-log4j) for an example.
Clojure, Scheme, Scala, Kotlin, Groovy, and ABCL can also be embedded through
OpenLDK's [language interop systems](docs/INTEROP.md).

## Quick start

Development targets **SBCL on Linux and JDK 25**. Install SBCL,
[ocicl](https://github.com/ocicl/ocicl), JDK 25, a C compiler, Python 3, and
the libffi development package. Set `JAVA_HOME` before building:

```sh
git clone https://github.com/atgreen/openldk.git
cd openldk
export JAVA_HOME=/path/to/jdk-25
export PATH="$JAVA_HOME/bin:$PATH"
ocicl install
make

./openldk -cp classes:lib/foo.jar com.example.Main
./openldk -jar app.jar
./openldk Hello.java
```

`make` builds `openldk`, the JNI bridge, and `javacl`, a saved executable
of the Java compiler running on OpenLDK. The default image heap is 32 GB;
use `make DYNAMIC_SPACE_SIZE=4096` for a 4 GB heap.

See [Building and Running](docs/RUNNING.md) for JDK discovery, offline
setup, launcher options, source-file execution, and the code cache.

## Clojure in the Lisp REPL

Load the interop system and enable its reader syntax. `#[ ... ]` accepts
Clojure source, and Clojure functions can be called with Lisp's `funcall`
and `mapcar`:

```lisp
CL-USER> (asdf:load-system :openldk/clojure)
CL-USER> (openldk.clojure:enable-syntax)
CL-USER> (funcall #[(fn [x] (+ x 4))] 4)
8
CL-USER> (mapcar #[#(* % %)] '(1 2 3 4))
(1 4 9 16)
```

Similar interop is available for Scheme (Kawa and JScheme), Scala, Kotlin,
Groovy, and Common Lisp through ABCL. See
[Language interop](docs/INTEROP.md) for setup and more examples.

## Status

OpenLDK targets Java 25 and can run applications including javac, JVM
language compilers and REPLs, Spring Boot, and Quarkus. It supports
ordinary JNI libraries and FFM through its own libffi bridge.
See [Applications](docs/APPLICATIONS.md) for examples and
[Java Compatibility](docs/COMPATIBILITY.md) for tested features and limits.

OpenLDK is under development and is not performance-competitive with modern
JVMs. Bytecode verification is incomplete, so it is not a security boundary
for untrusted class files. Java finalization and object resurrection are
not implemented.

## Documentation

- [Language interop](docs/INTEROP.md): call Java and JVM languages from Lisp.
- [Executable images](docs/IMAGES.md): save applications and warmed runtimes.
- [Native libraries](docs/NATIVE.md): JNI, FFM, and native-access policy.
- [Eclipse runner](test/eclipse/README.md): try the GUI or headless checks.
- [Hacking guide](docs/HACKING.md): compiler internals, debugging, and tests.

For development, run `ocicl lint openldk.asd` and
`make check-regression`. See the hacking guide for additional checks.

## Author and license

Written by [Anthony Green](https://github.com/atgreen).
Distributed under the GNU GPL, version 3 or later, with the Classpath
exception. See [COPYING](COPYING).
