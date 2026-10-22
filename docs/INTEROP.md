# Using Other JVM Languages from Lisp

OpenLDK runs Java bytecode inside SBCL, so a language that runs on the JVM
runs in the same Lisp process and heap as your Lisp code.  The systems
described here make such a language convenient to use from Lisp: evaluate
its code, call its functions, let it call Lisp functions, and convert values
both ways, with no serialization and no process boundary.

| System | Language | Package |
|---|---|---|
| `openldk/interop` | (shared core) | `openldk.interop` |
| `openldk/jscheme` | Peter Norvig's [JScheme](https://norvig.com/jscheme.html) | `openldk.jscheme`, nickname `jscheme` |
| `openldk/clojure` | [Clojure](https://clojure.org/) 1.12.4 | `openldk.clojure`, nickname `clojure` |
| `openldk/kawa` | [Kawa](https://www.gnu.org/software/kawa/) Scheme 3.1.1 | `openldk.kawa`, nickname `kawa` |
| `openldk/scala` | [Scala](https://www.scala-lang.org/) 2.13.16 | `openldk.scala`, nickname `scala` |
| `openldk/kotlin` | [Kotlin](https://kotlinlang.org/) 2.2.20 | `openldk.kotlin`, nickname `kotlin` |
| `openldk/groovy` | [Groovy](https://groovy-lang.org/) 4.0.27 | `openldk.groovy`, nickname `groovy` |

ABCL is next, on the same core.

## SBCL images with a language started

Starting a language can take a while: Clojure loads `clojure.core` for about
six minutes cold.  These targets build an SBCL executable with the language
started and its common code paths compiled, whose top level is the ordinary
Lisp REPL:

```sh
make sbcl-clojure && ./sbcl-clojure      # Clojure ready at once
make sbcl-jscheme && ./sbcl-jscheme
make sbcl-kawa && ./sbcl-kawa
make sbcl-scala && ./sbcl-scala
```

```lisp
* (clojure:eval '(map inc #(1 2 3)))
(2 3 4)
```

They are built with `openldk.interop:dump-lisp-image`; see below.  (The
`./clojure`, `./jscheme` and `./kawa` images of [IMAGES.md](IMAGES.md) are different:
they run the language's own REPL.)

## JScheme

Build JScheme's classes first: `make jscheme` downloads and compiles them
(and also builds the `./jscheme` warm image, see [IMAGES.md](IMAGES.md)).

```lisp
(asdf:load-system "openldk/jscheme")

(jscheme:startup)                                   ; start the interpreter

(jscheme:eval '(+ 2 3))                             ; => 5
(jscheme:eval '(map (lambda (x) (* x x)) '(1 2 3))) ; => (1 4 9)
(jscheme:eval "(define (fact n) (if (< n 2) 1 (* n (fact (- n 1))))) (fact 20)")
                                                    ; => 2432902008176640000
(jscheme:funcall "fact" 10)                         ; => 3628800
(jscheme:load "program.scm")

;; Lisp functions callable from Scheme
(jscheme:define "square" (lambda (x) (* x x)))
(jscheme:eval '(map square '(1 2 3)))               ; => (1 4 9)
(jscheme:define "small?" (lambda (x) (< x 10)) :predicate t)
(jscheme:eval '(if (small? 50) 'small 'big))        ; => BIG
```

`eval` takes a Lisp s-expression or a string of Scheme source.  An
s-expression is turned into JScheme's own data (pairs and interned symbols)
and evaluated directly, not printed and re-read.  Symbols are lowercased, so
`'(string-length "abc")` is Scheme's `(string-length "abc")`.

| Function | |
|---|---|
| `startup &key classpath` | Start JScheme (by default from `lib/jscheme/classes/`). Calling it again does nothing. |
| `eval form` | Evaluate an s-expression or a source string; return Lisp data. |
| `eval-string source` | Evaluate every form in a string; return the last value. |
| `load file` | Load a Scheme source file. |
| `funcall procedure &rest args` | Call a procedure, given as an object or by name. |
| `lookup name` | The value a name is bound to, unconverted (a procedure stays a procedure object). |
| `define name value &key predicate` | Bind a name; a Lisp function becomes a Scheme procedure. |
| `to-lisp object`, `from-lisp value` | Convert values. |
| `*interpreter*` | The `jscheme.Scheme` interpreter object. |

### Values

| Scheme | Lisp |
|---|---|
| number (JScheme has only doubles) | integer when whole, as JScheme prints it; otherwise `double-float` |
| string | string |
| symbol | symbol, upcased and interned in `*package*` (`foo` ↔ `FOO`) |
| character | character |
| list, pair, `'()` | list, cons, `NIL` |
| vector | simple-vector |
| `#t` | `T` |
| `#f` | `NIL` |
| procedure | left as is; call it with `jscheme:funcall` |

Both `#f` and `'()` become `NIL`, and `NIL` goes back as `'()`.  Write
`:false` for `#f`.  Since Scheme treats `'()` as true, a Lisp predicate given
to Scheme should be defined with `:predicate t`, which turns its `NIL` into
`#f`.

### Errors

A Scheme error, such as an unbound variable or a call to `error`, is
signalled in Lisp as `openldk.interop:foreign-error`, whose
`foreign-error-language` is `"JScheme"`, `foreign-error-message` the
message, and `foreign-error-object` the Java exception.

A Lisp error inside a Lisp function that Scheme called comes back out of
`jscheme:eval` as the original Lisp condition, a `type-error` stays a
`type-error`, so Lisp code can handle it as usual.

## Clojure

`make clojure` downloads Clojure's jars.  Starting Clojure from a plain SBCL
takes minutes; `make sbcl-clojure` builds an SBCL with it started.

```lisp
(asdf:load-system "openldk/clojure")
(clojure:startup)                                  ; already done in sbcl-clojure

(clojure:eval '(+ 1 2))                            ; => 3
(clojure:eval '(map inc #(1 2 3)))                 ; => (2 3 4)
(clojure:eval "(defn fact [n] (if (< n 2) 1 (*' n (fact (dec n))))) (fact 25)")
                                                   ; => 15511210043330985984000000
(clojure:funcall 'clojure.string/upper-case "hi")  ; => "HI"
(clojure:require "clojure.set")

;; Lisp functions callable from Clojure, with any number of arguments
(clojure:define "square" (lambda (x) (* x x)))
(clojure:eval '(mapv square (range 5)))            ; => #(0 1 4 9 16)
```

In an s-expression, Lisp vectors are Clojure vectors and hash tables are
maps, so `(defn add3 [a b c] ...)` is written `(defn add3 #(a b c) ...)`.
Symbols are lowercased, so a case-sensitive name, such as a Java class or
method, needs bars: `(|Exception.| "oops")`, `(|.getMessage| e)`.  Or pass
the code as a string.

| Function | |
|---|---|
| `startup &key classpath` | Start Clojure (by default from the jars `make clojure` downloads) in the `user` namespace. |
| `eval form` | Evaluate an s-expression or a source string; return Lisp data. |
| `eval-string source` | Evaluate every form in a string; return the last value. |
| `load file`, `require namespace` | Load a source file; load a namespace. |
| `funcall fn &rest args` | Call a function or var, given as an object or by name (`"clojure.string/upper-case"`, or a name resolved in `user`). |
| `lookup name` | The var a name resolves to, unconverted. |
| `define name value` | Define a var in `user`; a Lisp function becomes a Clojure function. |
| `ex-data condition` | The `ex-data` map of the Clojure exception behind a `foreign-error`. |
| `to-lisp object`, `from-lisp value` | Convert values. |

| Clojure | Lisp |
|---|---|
| `nil` | `NIL` |
| `true`, `false` | `T`, `NIL` (write `:false` for `false`) |
| long, BigInt, Ratio | integer, ratio |
| double | `double-float` |
| string, character | string, character |
| keyword | keyword (`:foo` ↔ `:FOO`) |
| symbol | symbol, upcased, interned in `*package*` |
| list, seq | list |
| vector | simple-vector |
| map | `equal` hash table |
| set | list |
| function, var, other objects | left as is |

A lazy sequence is realized when converted, so don't convert an infinite
one.  Clojure errors are signalled as `foreign-error`, whose message
includes the causes (`Syntax error compiling ... (caused by: Unable to
resolve symbol: foo in this context)`); `clojure:ex-data` gets an
`ex-info`'s data.  A Lisp error in a Lisp function that Clojure called comes
back as the original condition, and Clojure code can catch it as an
`Exception`.

Tests: `make check-clojure` runs them in `sbcl-clojure`.

### Clojure syntax in Lisp source

After `(clojure:enable-syntax)`, `#[ ... ]` holds Clojure source written as
Clojure writes it (vectors, maps, sets, `#()` functions, `@`, `\c`
characters) and evaluates it where it appears.  `$name` and `$(form)`
splice in Lisp values, passed as objects rather than printed, so lexical
variables and Lisp functions work too:

```lisp
(clojure:enable-syntax)
#[ (->> (range 10) (filter even?) (map #(* % %))) ]   ; => (0 4 16 36 64)
(let ((xs '(3 1 2))) #[ (sort > $xs) ])                ; => (3 2 1)
#[ (map $#'1+ [1 2 3]) ]                                ; => (2 3 4)
```

In a backquoted macro template, `$,x` splices in the value of the macro
argument `x`:

```lisp
(defmacro clj-max (form) `#[ (apply max $,form) ])
(clj-max (list 3 9 4))                                 ; => 9
```

`enable-syntax` changes the current readtable, or the one it is given:
`(setf *readtable* (clojure:enable-syntax (copy-readtable)))` keeps the
change to one file.

## Kawa

`make kawa` downloads Kawa's jar; `make sbcl-kawa` builds an SBCL with Kawa
started.

```lisp
(asdf:load-system "openldk/kawa")
(kawa:startup)                                     ; already done in sbcl-kawa

(kawa:eval '(+ 1 2))                               ; => 3
(kawa:eval '(map (lambda (x) (* x x)) '(1 2 3)))   ; => (1 4 9)
(kawa:eval "(define (fact n) (if (< n 2) 1 (* n (fact (- n 1))))) (fact 25)")
                                                   ; => 15511210043330985984000000
(kawa:funcall 'string-append "foo" "bar")          ; => "foobar"
(kawa:eval "(guard (e (#t 'caught)) (raise 'oops))") ; => CAUGHT

;; Lisp functions callable from Scheme, with any number of arguments
(kawa:define "square" (lambda (x) (* x x)))
(kawa:eval '(map square '(1 2 3)))                 ; => (1 4 9)
(kawa:define "small?" (lambda (x) (< x 10)) :predicate t)
```

Code is evaluated in Kawa's user environment, so a definition can shadow a
built-in (Kawa has its own `square`).  Symbols are lowercased.  Kawa's
colon notation for Java, `(java.lang.Math:max 3 7)`, is read by Kawa's
reader, so write such code as a string.

| Function | |
|---|---|
| `startup &key classpath` | Start Kawa (by default from the jar `make kawa` downloads). |
| `eval form` | Evaluate an s-expression or a source string; return Lisp data. |
| `eval-string source` | Evaluate every form in a string; return the last value. |
| `load file` | Load a Scheme source file. |
| `funcall procedure &rest args` | Call a procedure, given as an object or by name.  A name bound to something that isn't a procedure object, such as `list` (a type in Kawa), is called as Scheme code would call it. |
| `lookup name` | The value a name is bound to, unconverted. |
| `define name value &key predicate` | Bind a name; a Lisp function becomes a Scheme procedure. |
| `to-lisp object`, `from-lisp value` | Convert values. |
| `*environment*` | The Kawa environment code is evaluated in. |

| Kawa | Lisp |
|---|---|
| integer, rational | integer, ratio |
| real | `double-float` |
| string, character | string, character |
| symbol | symbol, upcased, interned in `*package*` |
| keyword (`foo:`) | keyword |
| list, pair, `'()` | list, cons, `NIL` |
| vector | simple-vector |
| `#t`, `#f` | `T`, `NIL` (write `:false` for `#f`) |
| procedure, other objects | left as is |

As with JScheme, `NIL` goes to Scheme as `'()`, which is true, so give a
Lisp predicate `:predicate t`.  Kawa errors are signalled as
`foreign-error`, and a Lisp error in a Lisp function that Kawa called comes
back as the original condition.

Tests: `make check-kawa` runs them in `sbcl-kawa`.

## Scala

`make sbcl-scala` downloads Scala's jars and builds an SBCL with Scala
started.  Starting Scala means compiling the Scala compiler's bytecode,
which takes OpenLDK several minutes, so use the image.

```lisp
(asdf:load-system "openldk/scala")
(scala:startup)                                    ; already done in sbcl-scala

(scala:eval "List(1, 2, 3).map(_ * 2)")            ; => (2 4 6)
(scala:eval "def fact(n: Int): BigInt = if (n < 2) 1 else n * fact(n - 1); fact(25)")
                                                   ; => 15511210043330985984000000
(scala:eval "Map(\"a\" -> 1)")                     ; => a hash table
(scala:funcall (scala:eval-raw "(a: Int, b: Int) => a * b") 6 7)   ; => 42

;; Lisp functions callable from Scala, of their arity's FunctionN type
(scala:define "square" (lambda (x) (* x x)))
(scala:eval "(1 to 4).map(square).toList")         ; => (1 4 9 16)
(scala:define "greeting" "hello" :type "String")
(scala:eval "greeting.toUpperCase")                ; => "HELLO"
```

Code is evaluated by the Scala REPL's script engine (JSR 223), so
definitions persist from one `eval` to the next, as in the REPL.

| Function | |
|---|---|
| `startup &key classpath` | Start Scala (by default from the jars `make sbcl-scala` downloads). |
| `eval source` | Evaluate a string of Scala; return the value as Lisp data. |
| `eval-raw source` | Evaluate a string of Scala; return the value unconverted. |
| `funcall function &rest args` | Call a Scala function, given as an object or by the name of a val. |
| `define name value &key type arity` | Bind a `val`; a Lisp function becomes a Scala function. |
| `to-lisp object`, `from-lisp value` | Convert values. |
| `*engine*` | The script engine. |

| Scala | Lisp |
|---|---|
| `Int`, `Long`, `BigInt` | integer |
| `Double`, `Float` | `double-float`, `single-float` |
| `String`, `Char`, `Boolean` | string, character, `T`/`NIL` (write `:false`) |
| `List` and other `Seq`s, `Set`s | list (`from-lisp` makes a `List`) |
| `Vector`, `Array` | vector (`from-lisp` makes a `Vector`) |
| `Map` | hash table, test `equal` |
| `Option` | `None` is `NIL`, `Some(x)` is `x` |
| `Unit` | `NIL` |
| `TupleN` | list |
| functions, other objects | left as is |

Scala errors, compile errors included, are signalled as `foreign-error`.

Tests: `make check-scala` runs them in `sbcl-scala`.

## Kotlin

`make sbcl-kotlin` downloads the Kotlin compiler's jars and builds an SBCL
with the compiler started and warm.

```lisp
(asdf:load-system "openldk/kotlin")
(kotlin:startup)                                   ; already done in sbcl-kotlin

(kotlin:eval "listOf(1, 2, 3).map { it * 2 }")     ; => (2 4 6)
(let ((class (kotlin:compile "fun fact(n: Int): Long = if (n < 2) 1 else n * fact(n - 1)")))
  (kotlin:call class "fact" 20))                   ; => 2432902008176640000
(kotlin:call (kotlin:compile "fun twice(f: (Int) -> Int, x: Int) = f(f(x))")
             "twice" (lambda (x) (* x 3)) 2)       ; => 18
```

`compile` compiles a source string as a file of its own, with the in-process
Kotlin compiler, into a directory on the classpath, and returns the class
holding its top-level functions; `call` calls one, converting arguments by
its parameter types.  `eval` compiles and runs an expression.  Compiler
errors are signalled as `foreign-error`, with the compiler's messages.

| Function | |
|---|---|
| `startup &key classpath` | Start the compiler (by default from the jars `make sbcl-kotlin` downloads). |
| `compile source &key name` | Compile a file; return its top-level class's name. |
| `call class function &rest args` | Call a top-level function. |
| `eval expression` | Compile and evaluate an expression. |
| `funcall function &rest args` | Call a Kotlin function object. |
| `to-lisp object`, `from-lisp value` | Convert values. |

Tests: `make check-kotlin` runs them in `sbcl-kotlin`.

## Groovy

`make sbcl-groovy` downloads Groovy's jar and builds an SBCL with Groovy
started.

```lisp
(asdf:load-system "openldk/groovy")
(groovy:startup)                                   ; already done in sbcl-groovy

(groovy:eval "[3, 1, 2].sort().collect { it * 10 }")    ; => (10 20 30)
(groovy:eval "1.5 + 1")                                 ; => 5/2, exactly
(groovy:funcall (groovy:eval-raw "{ a, b -> a * b }") 6 7)   ; => 42

;; Lisp functions as Groovy closures
(groovy:define "square" (lambda (x) (* x x)))
(groovy:eval "(1..4).collect { square(it) }.sum()")     ; => 30
```

Code runs in one `GroovyShell`; variables a script assigns without `def`,
and those `define` binds, live in its binding.  Groovy's `BigDecimal`
literals (`1.5`) become exact rationals.  A Lisp function becomes an
instance of a small closure class Groovy compiles statically at startup,
whose `doCall` calls the Lisp function.

| Function | |
|---|---|
| `startup &key classpath` | Start Groovy (by default from the jar `make sbcl-groovy` downloads). |
| `eval source`, `eval-raw source` | Evaluate Groovy; return Lisp data, or the value unconverted. |
| `funcall closure &rest args` | Call a closure, given as an object or by the name of a binding. |
| `define name value` | Bind a variable; a Lisp function becomes a closure. |
| `lookup name` | A binding's value, unconverted. |
| `to-lisp object`, `from-lisp value` | Convert values. |

Tests: `make check-groovy` runs them in `sbcl-groovy`.

## Source syntax: `#@scala{ }`, `#@kotlin{ }`, `#@clj{ }`

After `(openldk.interop:enable-source-syntax)`, `#@LANGUAGE{ ... }` holds
source in that language, written as the language writes it, and evaluates
it where it appears.  `$name`, `$(form)` and `$#'function` splice in Lisp
values (and `$,x` in a backquoted macro template); they are passed as
objects, never printed into the source.

| Syntax | Language | System |
|---|---|---|
| `#@scala{ }`, `#@sc{ }` | Scala | `openldk/scala` |
| `#@kotlin{ }`, `#@kt{ }` | Kotlin | `openldk/kotlin` |
| `#@clojure{ }`, `#@clj{ }` | Clojure (also `#[ ]`) | `openldk/clojure` |
| `#@kawa{ }` | Kawa Scheme | `openldk/kawa` |
| `#@jscheme{ }` | JScheme | `openldk/jscheme` |
| `#@groovy{ }`, `#@gv{ }` | Groovy | `openldk/groovy` |

```lisp
(setf *readtable* (openldk.interop:enable-source-syntax (copy-readtable)))

(let ((xs '(5 3 8)))
  #@scala{ $xs.map { case n: Int => n * 2 }.sum })     ; => 32
(let ((n 4))
  #@kt{ (1..$n).map { it * it } })                      ; => (1 4 9 16)
#@clj{ (zipmap [:a :b] [1 2]) }                         ; => a hash table
(defmacro kt-square (x) `#@kt{ $,x * $,x })
```

In Scala and Kotlin, each splice is a parameter typed by its Lisp value
(`Int`, `Long`, `Double`, `String`, `List`, `Map`, a function type), so it
needs no cast; a block is compiled once for each set of parameter types and
then called directly.  Splices work outside the language's strings and
comments, so `s"$x"` and `"${x}"` stay the language's own.  A Kawa or
JScheme block becomes a Scheme procedure of the spliced values, evaluated
once and then called; definitions in it are local to it.

## The shared core: `openldk/interop`

The language systems are built on a small set of functions, also usable
directly for calling Java:

| Function | |
|---|---|
| `start-runtime &key classpath` | Start OpenLDK's Java runtime, or add to its classpath. Several language systems can share one runtime. |
| `java-load class` | Load and initialize a class (`"java.lang.String"` or `"java/lang/String"`). |
| `java-new class constructor &rest args` | Construct an object: `(java-new "java.lang.StringBuilder" "(Ljava/lang/String;)" (jstring "x"))`. |
| `java-call method object &rest args` | Call an instance method: `(java-call "length()" s)`. |
| `java-static class method &rest args` | Call a static method: `(java-static "java.lang.Math" "max(II)" 3 4)`. |
| `java-static-field class field` | Read a static field. |
| `java-instance-p object class`, `java-class-symbol class` | Type checks. |
| `jstring`, `lstring`, `java-double`, `java-array-list`, `make-java-array-of` | Values. |
| `with-foreign-errors (language) &body` | Signal a Java exception escaping the body as `foreign-error`. |
| `define-java-subclass`, `make-java-subclass-instance`, `call-from-java` | Callbacks; see below. |
| `dump-lisp-image path &key init` | Save an SBCL with the Lisp REPL as its top level, after calling `init` (start a language, warm it up). |

Methods are named as OpenLDK names them: the method name and its argument
descriptor, like `"valueOf(D)"`.

### Callbacks: Lisp classes that extend Java classes

OpenLDK's Java classes are CLOS classes, so a Lisp class can extend a Java
class and implement its methods with `defmethod`.  Java code calling those
methods runs the Lisp ones.  A language system makes a Lisp function
callable from its language this way, with no bytecode generated.  For
JScheme:

```lisp
(define-java-subclass lisp-procedure "jscheme.Procedure"
    ((function :initarg :function :reader lisp-procedure-function))
  ("apply(Ljscheme/Scheme;Ljava/lang/Object;)" (self interpreter args)
   (call-from-java (lambda () (from-lisp (apply (lisp-procedure-function self)
                                                (to-lisp args)))))))
```

The Java superclass exists only once its jar is loaded, so the macro defines
an installer, `install-lisp-procedure`, which the language system calls at
startup.  `make-java-subclass-instance` creates an instance and runs the
Java superclass's constructor, which initializes its Java fields.
`call-from-java` carries a Lisp error out through the Java code that called
the method, so that `with-foreign-errors` can re-signal it.
