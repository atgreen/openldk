# OpenLDK
## A JIT Compiler and Runtime for Java in Common Lisp

<p align="center" width="100%">
    <img width="33%" src="https://github.com/user-attachments/assets/eb723bd3-f676-44d0-97ce-9bbfb11f2cd0">
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

`openldk` uses pre-extracted JDK 17 class files for its boot classpath.
Set `JAVA_HOME` to point at your JDK 17 installation and extract the
class files:
```
$ export JAVA_HOME=/home/linuxbrew/.linuxbrew/opt/openjdk@17/libexec
$ make jdk17-classes
```

The `LDK_JDK_CLASSES` environment variable points to the extracted
class files (defaults to `jdk17-classes/` in the project directory).

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
 (defclass openldk::|demo| (openldk::|java/lang/Object|)
    ((openldk::|x| :initform 0 :allocation :class)
     (openldk::|y| :initform 0 :allocation :class)))
 (defparameter openldk::|+static-demo+| (make-instance 'openldk::|demo|))
 (defmethod openldk::|<init>()| ((openldk::|this| openldk::|demo|))
   (openldk::%compile-method "demo" 1)
   (openldk::|<init>()| openldk::|this|))
 (defun openldk::|demo.add(ii)| (openldk::|arg1| openldk::|arg2|)
   (openldk::%compile-method "demo" 2)
   (openldk::|demo.add(ii)| openldk::|arg1| openldk::|arg2|))
 (defun openldk::|demo.main([ljava/lang/String;)| (openldk::|arg1|)
   (openldk::%compile-method "demo" 3)
   (openldk::|demo.main([ljava/lang/string;)| openldk::|arg1|)))))
```

Note that the methods are all stubs that invoke the compiler and then
themselves.  This is how we support incremental JIT compilation.

When the `add` method is called, the compiler will read `add`'s
bytecode and generates something like the following:

```
(defun openldk::|demo.add(ii)| (openldk::|arg0| openldk::|arg1|)
  (let ((openldk::|s{3}|)
        (openldk::|s{2}|)
        (openldk::|s{1}|)
        (openldk::|local-0| openldk::|arg0|)
        (openldk::|local-1| openldk::|arg1|))
    (block nil
      (tagbody
       |branch-target-0|
        (setf openldk::|s{1}| openldk::|local-0|)
        (setf openldk::|s{2}| openldk::|local-1|)
        (setf openldk::|s{3}|
          (let* ((openldk::value2 openldk::|s{2}|)
                 (openldk::value1 openldk::|s{1}|)
                 (openldk::result
                   (logand (+ openldk::value1 openldk::value2)
                           4294967295)))
            (if (> openldk::result 2147483647)
                (- openldk::result 4294967296)
                openldk::result)))
        (return-from openldk::|demo.add(ii)| openldk::|s{3}|)))))
```

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
- **ABCL** - [Armed Bear Common Lisp](https://abcl.org/) 1.9.2 fully
  bootstraps on OpenLDK (~3700 classes, ~42s startup), printing its
  banner and completing both low-level and high-level initialization.
  REPL interaction is not yet working.  See [docs/ABCL.md](docs/ABCL.md).
- **Clojure** - [Clojure](https://clojure.org/) 1.12.0 fully
  bootstraps and runs its REPL on OpenLDK.

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
of the GNU General Public License, Version 2, modified by the
"CLASSPATH" exception to the GPL.  See
[LICENSE](https://github.com/atgreen/OpenLDK/blob/main/LICENSE)
for details.
