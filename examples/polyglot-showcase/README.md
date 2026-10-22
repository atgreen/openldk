# The OpenLDK polyglot showcase

A small bookshop that uses seven JVM languages from Common Lisp, all in one
SBCL process with one heap, and builds with ASDF.

- `polyglot-showcase.asd` declares two Java libraries from Maven Central
  and a Kotlin source file as ASDF components.
- `catalog.kt` is the catalog, written in Kotlin. ASDF compiles it at
  `compile-op` against the Maven library.
- `showcase.lisp` is the program. Scala, Clojure, Groovy, Kawa, JScheme
  and ABCL appear inline as `#@LANGUAGE{ ... }` blocks, each written in
  its own syntax.

## Running it

From the OpenLDK source directory, build the SBCL with every language
started (`make sbcl-polyglot`), then:

```sh
./sbcl-polyglot --eval '(push #p"examples/polyglot-showcase/" asdf:*central-registry*)' \
                --eval '(asdf:load-system "polyglot-showcase")' \
                --eval '(polyglot-showcase:run)' --quit
```

The first build downloads the two jars to OpenLDK's cache, checking each
against the SHA-1 Maven publishes. It also compiles `catalog.kt`.

## What each section shows

1. **ASDF builds foreign code.** `(:maven-jar ...)` and `(:kotlin-file ...)`
   are ordinary components. The Maven library is on the classpath for every
   language, and Lisp calls it directly too.
2. **Records.** `java-fields` turns a Kotlin data class into an alist.
3. **Collections without copying.** `java-sequence` makes the Kotlin `List`
   a Lisp sequence, so `length`, `find`, `reduce` and `position` work on it.
4. **Foreign functions are Lisp functions.** A Scala lambda goes to
   `mapcar`, and a Clojure fn is `sort`'s `:key`.
5. **Java classes written in Lisp.** `define-java-class` makes a real
   `java.util.function.Function`. Kotlin calls it through the interface,
   and Groovy calls it by reflection.
6. **Errors cross languages.** Kotlin's `NumberFormatException` arrives as
   a `foreign-number-format-error`, and a handler answers it with
   `use-value`. Scala's `ArithmeticException` is an `arithmetic-error`.
   Going the other way, a Lisp `division-by-zero` reaches Kotlin code as an
   `ArithmeticException`, which it catches.
7. **Precompiled blocks.** A `#@kt{ }` block whose splice has a static type,
   `(the (signed-byte 32) cents)`, was compiled together with
   `showcase.lisp`. Its classes are in the fasl.
8. **Re-entrant blocks.** A `#[ ]` Clojure block calls back into Lisp,
   which runs the block again: `(fib 15)` crosses the boundary 1973 times.
9. **Coroutines.** Kotlin prices the catalog with `async` and `runBlocking`.
10. **Macros writing foreign code.** Lisp macros expand to Groovy and Scala
    blocks, splicing in their arguments with `$,`.
11. **Lisp data, read by Java.** `lisp-list-view` hands a Lisp vector to
    Groovy as a `java.util.List`, and Groovy renders a receipt from it.
12. **The rest of the table.** Kawa escapes a search with `call/cc` and
    divides exactly. JScheme reverses a string. ABCL, a Common Lisp running
    inside the JVM, writes numbers in English and calls Java. Groovy's
    decimals come back to Lisp as exact rationals.

See [docs/INTEROP.md](../../docs/INTEROP.md) for each feature.
