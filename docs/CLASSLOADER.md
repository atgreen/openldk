# Class Loaders in OpenLDK

Java identifies a class by its name **and** its defining class loader: two
loaders can each define `com.example.Foo`, and those are different classes.
This describes how OpenLDK models that.

## Loaders and packages

Every Java class loader is paired with an `<ldk-class-loader>`
(`src/global-state.lisp`), created on demand by
`get-ldk-loader-for-java-loader` and found again through
`*java-to-ldk-loaders*`.  Each has:

- its own tables of classes by name (`ldk-classes-by-bin-name` for OpenLDK's
  `<class>` objects, `java-classes-by-bin-name` for `java.lang.Class`
  objects);
- a parent, mirroring the Java loader's parent;
- a Lisp package, in which its classes' symbols are interned.

| Loader | Package |
|---|---|
| Boot (JDK classes) | `OPENLDK` (and `OPENLDK.SYSTEM`) |
| App (the classpath) | `OPENLDK.APP` |
| Each user-defined loader | `OPENLDK.L<n>` |

Because each user loader has its own package, two loaders' classes named
`Foo` are different symbols, so different CLOS classes, static storage
(`+static-Foo+`) and static-method functions.  Instance methods are generic
functions in `OPENLDK`, with methods specialized on those distinct classes.
`*ldk-loaders-by-package*` maps a package back to its loader, which is how
`getClass()` finds an object's class from its CLOS class.

## The global tables

Every class is also recorded in the global tables (`*ldk-classes-by-bin-name*`
and friends), by name alone, so code that only has a name, such as natives
looking up JDK classes, can find it.  With several loaders, the last class
defined under a name wins there.

The boot loader's own tables *are* the global tables.  So:

- delegation to the boot loader (`%lookup-class-with-delegation`) never
  returns a class a user loader defined;
- `classload`, which loads like the boot and app loaders, never returns one
  either, and loads from the classpath instead;
- anything that has a class object or a `java.lang.Class` uses its loader,
  not the name: `ldk-class-package`, `%java-class-package`,
  `%make-java-instance` with a loader, and method stubs, which pass their
  `<class>` to `%compile-method`.

## Resolving a reference

When OpenLDK compiles a method, or defines a class and needs its superclass
and interfaces, it resolves each class name through the defining class's
loader with `%resolve-class` (`src/openldk.lisp`):

1. the loader's own tables, then its parents';
2. for a loader that overrides `loadClass` (a child-first loader), its Java
   `loadClass`;
3. the JDK and the classpath (`%classload-system`);
4. for any user-defined loader, its Java `loadClass`.

Step 4 finds classes a loader defines only when asked, from `findClass`, as
ABCL's `MemoryClassLoader` does.  Step 2 lets a child-first loader supply
its own class of a name the classpath also has.  A parent-first loader that
only overrides `findClass` gets the same answer from step 3 as Java would
give, without a call into Java for every reference.

Calling Java from inside the compiler is guarded: `loadClass` runs with
`*context*` bound to nil so it can't see the compile in progress,
`*classes-being-resolved*` stops resolution cycles, and a compile claim
records its thread so a thread that asks for a method it is already
compiling gets an error instead of waiting on itself.

`Class.forName` with a loader calls its `loadClass`, as the JVM does.

## Tests

- `aaa/lazyloader.java`: classes defined only from `findClass`, a superclass
  only the loader can find, `Class.forName` through `loadClass`.
- `aaa/siblingloaders.java`: two loaders defining classes of the same name.
- `aaa/childfirst.java`: a child-first loader shadowing a classpath class.

All three make classes that exist only inside a loader by renaming a nested
class's bytecode to another name of the same length.
