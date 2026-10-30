# Applications and JVM Languages

These examples record applications exercised during development; they do not
establish compatibility with every application or library version. Timings
below were measured on the development host and depend on the workload, heap,
and cache state. For Eclipse GUI and headless checks, see the
[Eclipse runner](../test/eclipse/README.md).

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
  starts in under a second.  See [docs/ABCL.md](ABCL.md).
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

The warm images are described in [docs/IMAGES.md](IMAGES.md).

## Starting Spring Boot in a fraction of a second

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
[docs/IMAGES.md](IMAGES.md) shows.  The first warmup starts Boot and
the second compiles the runners' code; running `./myapp` then skips Boot's
startup entirely.  Threads don't survive in an image, so this suits
command-line applications.  A web server such as Tomcat has to start its
threads again on each run.

`make bench-spring-boot` builds this for a sample application and times it
against HotSpot, a JDK AOT cache and a GraalVM native image.  On the
development host the snapshot started in 82 ms, against 72 ms for the
native image and 3.4 s for `java -jar`.  See
[bench/spring-boot/README.md](../bench/spring-boot/README.md), and
`make bench-quarkus` for Quarkus.

Clojure, Kawa, JScheme, Scala, Kotlin, Groovy and ABCL can also be used
from Lisp, in the same process: each `openldk/<language>` system evaluates
code, calls the language's functions, lets it call Lisp functions, and
converts values both ways, and `#@scala{ ... }`, `#@kt{ ... }`,
`#@groovy{ ... }` and friends embed each language's source, as it writes
it, in Lisp code.  `make sbcl-<language>` builds an SBCL with a language
started, and `make sbcl-polyglot` one with all of them.  See
[docs/INTEROP.md](INTEROP.md).
