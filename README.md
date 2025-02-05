# OpenLDK
## A Java JIT Compiler and Runtime in Common Lisp

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

`openldk` uses the `LDK_CLASSPATH` environment variable rather than
`CLASSPATH`.  Be sure to point it at your Java 8 runtime jar file.  On
my Fedora Linux system that looks like:
```
$ export LDK_CLASSPATH=/usr/lib/jvm/java-1.8.0-openjdk-1.8.0.432.b06-3.fc40.x86_64/jre/lib/rt.jar
```

## How it Works

`openldk` reads class and jar files, and translates them into lisp
code, which sbcl's compiler then turns into machine code for
execution.

Java classes and objects are mapped to CLOS classes and object.  The
exception hierarchy is mirrored by a Common Lisp condition hierarchy.
CLOS provides everything we need to support reflection, and SBCL's
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
(PROGN
 (DEFCLASS OPENLDK::|demo| (OPENLDK::|java/lang/Object|)
           ((OPENLDK::|x| :INITFORM 0 :ALLOCATION :CLASS)
            (OPENLDK::|y| :INITFORM 0 :ALLOCATION :CLASS)))
 (DEFPARAMETER OPENLDK::|+static-demo+| (MAKE-INSTANCE 'OPENLDK::|demo|))
 (DEFMETHOD OPENLDK::|<init>()| ((OPENLDK::|this| OPENLDK::|demo|))
   (OPENLDK::%COMPILE-METHOD "demo" 1)
   (OPENLDK::|<init>()| OPENLDK::|this|))
 (DEFUN OPENLDK::|demo.add(II)| (OPENLDK::|arg1| OPENLDK::|arg2|)
   (OPENLDK::%COMPILE-METHOD "demo" 2)
   (OPENLDK::|demo.add(II)| OPENLDK::|arg1| OPENLDK::|arg2|))
 (DEFUN OPENLDK::|demo.main([Ljava/lang/String;)| (OPENLDK::|arg1|)
   (OPENLDK::%COMPILE-METHOD "demo" 3)
   (OPENLDK::|demo.main([Ljava/lang/String;)| OPENLDK::|arg1|)))
```

Note that the methods are all stubs that invoke the compiler and then
themselves.  This is how we support incremental JIT compilation.

When the `add` method is called, the compiler will read `add`'s
bytecode and generate the following:

```
(DEFUN OPENLDK::|demo.add(II)| (OPENLDK::|arg0| OPENLDK::|arg1|)
  "bridge=NIL"
  (LET ((OPENLDK::|s{3}|)
        (OPENLDK::|s{2}|)
        (OPENLDK::|s{1}|)
        (OPENLDK::|local-0| OPENLDK::|arg0|)
        (OPENLDK::|local-1| OPENLDK::|arg1|)
        (OPENLDK::|local-2|)
        (OPENLDK::|local-3|)
        (OPENLDK::|local-4|))
    (BLOCK NIL
      (TAGBODY
       |branch-target-0|
        (SETF OPENLDK::|s{1}| OPENLDK::|local-0|)
        (SETF OPENLDK::|s{2}| OPENLDK::|local-1|)
        (SETF OPENLDK::|s{3}|
                (LET* ((OPENLDK::VALUE2 OPENLDK::|s{2}|)
                       (OPENLDK::VALUE1 OPENLDK::|s{1}|)
                       (OPENLDK::RESULT
                        (LOGAND (+ OPENLDK::VALUE1 OPENLDK::VALUE2)
                                4294967295)))
                  (IF (> OPENLDK::RESULT 2147483647)
                      (- OPENLDK::RESULT 4294967296)
                      OPENLDK::RESULT)))
        (RETURN-FROM OPENLDK::|demo.add(II)| OPENLDK::|s{3}|)))))
```

## Hacking

### Testing

Run `make check` to run through the dejagnu-based testsuite.
As of Feb 4 2025, the results look like this
```
		=== openldk Summary ===

# of expected passes		721
# of unexpected failures	355
# of unresolved testcases	9
```

### Debugging

The `openldk` runtime will generate useful debug info if you set your
`LDK_DEBUG` environment variable.  `LDK_DEBUG` should be set to a
string of characters that are interpreted as below:

- `b` - trace bytecode compilation
- `c` - dump all Lisp code prior to evaluation
- `t` - trace method entries at runtime
- `s` - start a slynk server at startup (port 2025)
- `u` - unmuffle the Lisp compiler
- `x` - trace opcode execution (use with `t`)

More specifically, running `LDK_DEBUG=bctux openldk Hello` will enable
all debug output while running `Hello`.

## Status

Very basic programs work.  This includes the whole runtime startup
process, covering class loading, reflection, exceptions, file IO, and
more.

Not much more than that works yet.  You are looking at a work in
progress that may never be completed.

The code is not optimized.  Even with heavy optimization, OpenLDK's
performance will not be competitive to modern Java implementations.
It is not meant to be competitive.  OpenLDK is meant to fill the gap
for when you want to code in Common Lisp, but you need that one Java
library.

Here's an incomplete list of what's not implemented:
- support for class files beyond Java 8
- a handful of instructions
- bytecode verification

Author and License
-------------------

OpenLDK was written by [Anthony
Green](https://github.com/atgreen), and is distributed under the terms
of the GNU General Public License, Version 2, modified by the
"CLASSPATH" exception to the GPL.  See
[LICENSE](https://github.com/atgreen/OpenLDK/blob/main/LICENSE)
for details.
