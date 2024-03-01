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

Requirements
------------

`openldk` has only been tested with `sbcl`.  It's possible that other
Common Lisp implementation could be made to work with it, but I am
only developing with `sbcl` for now.

`openldk` has only been tested in Linux.

Debugging
---------

The `openldk` runtime will generate useful debug info if you set your
`LDK_DEBUG` environment variable.  `LDK_DEBUG` should be set to a
string of characters that are interpreted as below:

- `c` - dump all Lisp code prior to evalation
- `u` - unmuffle the Lisp compiler

More specifically, running `LDK_DEBUG=cu openldk Hello` will enable
all debug output while running `Hello`.

Status
------

Here's an incomplete list of what's not implemented:
- support for class files beyond Java 8
- bytecode verification
- `monitorenter`/`monitorexit` for object synchronization

Author and License
-------------------

OpenLDK was written by [Anthony
Green](https://github.com/atgreen), and is distributed under the terms
of the GNU General Public License, Version 2, modified by the
"CLASSPATH" exception to the GPL.  See
[LICENSE](https://github.com/atgreen/OpenLDK/blob/main/LICENSE)
for details.
