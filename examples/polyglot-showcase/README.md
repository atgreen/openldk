# Wonderland Books: a polyglot application

Wonderland Books' order processing reads an order file, prices it, and
prints an invoice and some recommendations. The application is written in
Common Lisp. Each piece is written in whichever JVM language suits it, and
all of them run on OpenLDK in a single SBCL process:

| Piece | Language | Where |
|---|---|---|
| Catalog, order-line parsing, pricing engine (concurrent, with coroutines) | Kotlin | `catalog.kt`, compiled by ASDF |
| ISBN-13 check digits | JScheme | `valid-isbn-p` |
| Discount rules, as a pattern match | Scala | `discount` |
| Finding an in-stock substitute, escaping the search with `call/cc` | Kawa | `substitute-for` |
| "Readers who bought this also enjoyed" | Clojure | `*also-bought*` |
| The invoice template, wrapping titles with commons-text | Groovy | `invoice` |
| The amount in words, with `FORMAT` | ABCL | `in-words` |
| The store's pricing policy, a Java class Kotlin calls | Lisp | `store-policy` |

`polyglot-showcase.asd` declares two Java libraries from Maven Central and
the Kotlin source file as ASDF components. Comments in `showcase.lisp`
point out the OpenLDK features each part relies on.

## Running it

From the OpenLDK source directory, build the SBCL with every language
started (`make sbcl-polyglot`), then:

```sh
./sbcl-polyglot --eval '(push #p"examples/polyglot-showcase/" asdf:*central-registry*)' \
                --eval '(asdf:load-system "polyglot-showcase")' \
                --eval '(polyglot-showcase:run)' --quit
```

This processes `order-1042.txt`. That order has a quantity that isn't a
number, an ISBN with a wrong check digit, and a book that's out of stock,
so the program has something to handle. To process another file, run
`(polyglot-showcase:run #p"my-order.txt")`.

The first build downloads the jars to OpenLDK's cache, checking each one
against the SHA-1 that Maven publishes, and compiles `catalog.kt`.

## The OpenLDK features it uses

- **ASDF builds foreign code.** The system's components include
  `(:maven-jar ...)` and `(:kotlin-file ...)`. The jar is on every
  language's classpath, and the Kotlin file is compiled against it.
- **Inline source:** `#@kt{ }`, `#@sc{ }`, `#@gv{ }`, `#@kawa{ }`,
  `#@jscheme{ }` and `#@abcl{ }` blocks, each in its own language's
  syntax. `$x` splices in a Lisp value.
- **Precompiled blocks.** `round-cents`'s Kotlin block has a statically
  typed splice, so it was compiled together with the Lisp file.
- **Java classes written in Lisp.** `store-policy` is a real
  `ToDoubleBiFunction`, and Kotlin's pricing engine calls it.
- **Foreign functions are Lisp functions.** `*also-bought*` is a Clojure fn
  that the program `funcall`s.
- **Typed conditions and restarts.** Kotlin's `NumberFormatException`
  arrives as a `foreign-number-format-error`. Each order line is parsed
  inside a `skip-line` restart.
- **Collections without copying.** `java-sequence` gives `find` and the
  other sequence functions the Kotlin catalog. `lisp-list-view` gives the
  Groovy template the Lisp invoice rows.
- **Records.** `java-fields` reads Kotlin data classes.

See [docs/INTEROP.md](../../docs/INTEROP.md).
