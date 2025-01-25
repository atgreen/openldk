# OpenLDK

> [!WARNING]
> This does not work and may never work.  Please don't tell anyone
> about this.  I just prefer to tinker in public.

OpenLDK is a JIT compiler and runtime for Java written in Common Lisp.
It works by incrementally translating Java bytecode into Lisp, and
then compiling that into native machine code for execution.  Java
classes are mapped to CLOS classes, opening the door for easy
integration between Java and Common Lisp code.

OpenLDK makes use of OpenJDK's core runtime libraries.  This is made
possible by the GNU Classpath Exception to the GPL, under which
OpenJDK code is distributed.

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

## Hacking

### Testing

Run `make check` to run through the dejagnu-based testsuite.

### Debugging

The `openldk` runtime will generate useful debug info if you set your
`LDK_DEBUG` environment variable.  `LDK_DEBUG` should be set to a
string of characters that are interpreted as below:

- `b` - trace bytecode compilation
- `c` - dump all Lisp code prior to evaluation
- `t` - trace method entries at runtime
- `u` - unmuffle the Lisp compiler
- `x` - trace opcode execution (use with `t`)

More specifically, running `LDK_DEBUG=bctux openldk Hello` will enable
all debug output while running `Hello`.

## Status

It still doesn't work.  You are looking at a work in progress that may
never be completed.  I don't want to see this on hackernews or reddit
prematurely.

Here's an incomplete list of what's not implemented:
- support for class files beyond Java 8
- bytecode verification

Author and License
-------------------

OpenLDK was written by [Anthony
Green](https://github.com/atgreen), and is distributed under the terms
of the GNU General Public License, Version 2, modified by the
"CLASSPATH" exception to the GPL.  See
[LICENSE](https://github.com/atgreen/OpenLDK/blob/main/LICENSE)
for details.
