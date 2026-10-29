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
| `openldk/abcl` | [ABCL](https://abcl.org/) (Armed Bear Common Lisp) 1.9.2 | `openldk.abcl`, nickname `abcl` |
| `openldk/polyglot` | all of the above, in one SBCL | `openldk.polyglot`, nickname `polyglot` |

## Finding the jars

Each language's `startup` takes its classes, by default, from the first of:

1. the `CLASSPATH` environment variable, if it already has them;
2. the copies `make <language>` (or `make sbcl-<language>`) leaves in `lib/`;
3. pinned, checksummed downloads into OpenLDK's cache,
   `$XDG_CACHE_HOME/openldk/` (`~/.cache/openldk/`), or `$LDK_CACHE_DIR`.
   JScheme ships as source, so its classes are compiled there with the JDK's
   compiler.  Set `LDK_OFFLINE=1` to never download.

`default-classpath` in each language's package returns what that finds.

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
make sbcl-kotlin && ./sbcl-kotlin
make sbcl-groovy && ./sbcl-groovy
make sbcl-abcl && ./sbcl-abcl
make sbcl-polyglot && ./sbcl-polyglot    # all of them
```

```lisp
* (clojure:eval '(map inc #(1 2 3)))
(2 3 4)
```

They are built with `openldk.interop:dump-lisp-image`; see below.  (The
`./clojure`, `./jscheme`, `./kawa` and `./abcl` images of [IMAGES.md](IMAGES.md) are different:
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
| `startup &key classpath` | Start JScheme (by default from `default-classpath`, see [Finding the jars](#finding-the-jars)). Calling it again does nothing. |
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

To retain a Clojure result without converting maps to Lisp hash tables,
call the function's `IFn` interface directly:

```lisp
(clojure:eval-string "(defn make-map [] {:answer 42})")
(defparameter *clj-map*
  (openldk.interop:java-call
   "invoke()Ljava/lang/Object;" (clojure:lookup "user/make-map")))

(clojure:to-lisp *clj-map*)                 ; convert later, if wanted
```

For arguments, use the corresponding `invoke` descriptor, such as
`"invoke(Ljava/lang/Object;)Ljava/lang/Object;"` for one argument, and
convert each argument with `clojure:from-lisp`. For a function object
returned by `clojure:eval-string`, use `openldk.interop:foreign-object` to
unwrap it before `java-call`. `defun-clojure` wrappers currently convert
results automatically and have no raw-result option.

In an s-expression, Lisp vectors are Clojure vectors and hash tables are
maps, so `(defn add3 [a b c] ...)` is written `(defn add3 #(a b c) ...)`.
Symbols are lowercased, so a case-sensitive name, such as a Java class or
method, needs bars: `(|Exception.| "oops")`, `(|.getMessage| e)`.  Or pass
the code as a string.

| Function | |
|---|---|
| `startup &key classpath` | Start Clojure (by default from `default-classpath`, see [Finding the jars](#finding-the-jars)) in the `user` namespace. |
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
| `startup &key classpath` | Start Kawa (by default from `default-classpath`, see [Finding the jars](#finding-the-jars)). |
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

## Functions with native language bodies

Enable a language's syntax in a copy of the current readtable, then write
`defun-clojure`, `defun-kawa`, `defun-jscheme`, `defun-kotlin`, or
`defun-scala`. The body goes directly to that language's reader; no `#[ ]` or `#@LANGUAGE{ }` enclosure is needed.

```lisp
(eval-when (:compile-toplevel :load-toplevel :execute)
  (asdf:load-system "openldk/clojure"))
(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf *readtable* (clojure:enable-syntax (copy-readtable))))

(defun-clojure foo (a b)
  (let [sum (+ a b)]
    {:sum sum :inputs [a b]}))

(gethash :sum (foo 3 4))                    ; => 7
(funcall #'foo 3 4)                        ; => a Lisp hash table
```

For Scheme, load `openldk/kawa` or `openldk/jscheme`, then call
`kawa:enable-syntax` or `jscheme:enable-syntax` the same way:

```lisp
(defun-kawa factorial (n)
  (if (< n 2) 1 (* n (factorial (- n 1)))))

(defun-jscheme pair (a b)
  (if #t (vector a b) #f))
```

The definition names also have package-qualified spellings:
`clojure:defun-clojure`, `kawa:defun-kawa`, and `jscheme:defun-jscheme`.
`openldk.interop:enable-source-syntax` enables definitions for the loaded
adapters along with the existing `#@` source blocks. Ordinary CL reading,
including dotted lists, quoting, reader labels, and `#.` evaluation, is
preserved. Changes stay within the chosen readtable.

Clojure and Scheme definitions have a CL name and a list of distinct
required CL parameters.
Their native names are lowercase; escape a mixed-case parameter with CL's
`|name|` syntax to preserve its case. Body spelling and native literals are
preserved. The native function can call itself by its lowercase CL name.
Arguments and results use the adapter's existing value conversions, and the
resulting CL function works with `funcall`, `apply`, and `mapcar`.

The native procedure is created once when the definition executes, and is
retained by the CL function. Clojure and Kawa compile it then; JScheme
creates its interpreted procedure then. Calls do not re-read or compile
the source. With `compile-file`, native creation happens when the FASL is
loaded, not while the CL file is compiled. Reloading or redefining the
function creates a new native procedure.

An explicit `$value` or `$(form)` in the body captures a CL value once when
the definition executes. Ordinary parameters need no `$` prefix:

```lisp
(let ((factor 3))
  (defun-clojure triple (n) (* n $factor)))
(triple 7)                                 ; => 21
```

### Kotlin and Scala definitions

Load `openldk/kotlin` or `openldk/scala` and enable `kotlin:enable-syntax`
or `scala:enable-syntax` in a copied readtable as above. Both provide typed
function definitions with native bodies:

```lisp
(defun-kotlin add ((a :int) (b :int))
  a + b)

(defun-scala greet ((name :string))
  name.toUpperCase)

(defun-kotlin factorial ((n :int)) :returns :int
  if (n < 2) 1 else n * factorial(n - 1))

(defun-scala factorial ((n :int)) :returns :int
  if (n < 2) 1 else n * factorial(n - 1))

(add 20 22)                                ; => 42
(greet "hello")                            ; => "HELLO"
```

Each required parameter is `(NAME TYPE)`. Type keywords are `:int`, `:long`,
`:short`, `:byte`, `:float`, `:double`, `:boolean`, `:char`, `:string`, and
`:unit`. A string supplies an arbitrary native type, for example Kotlin's
`"List<Int>"` or Scala's `"List[Int]"`. An optional `:returns TYPE` follows
the parameter list; otherwise the native compiler infers the result type.
Recursive Scala definitions generally need the return annotation.

Names must be native identifiers (letters, digits and underscores, starting
with a letter or underscore). Uppercase CL names become lowercase; use
`|mixedCase|` to preserve case. Bodies accept native strings, comments,
parentheses and brace blocks directly. Kotlin string templates and Scala
interpolated strings retain their native meaning.

As with Clojure and Scheme, `$value` or `$(form)` outside strings captures a
Lisp value once when the definition executes. Capture types are inferred
from those values. Required parameter types are explicit, independent of
the first call. Arguments and results use the adapters' existing conversions.
The native compiler runs when the definition executes or its FASL loads;
ordinary calls use the retained compiled function without invoking it again.
The macros are also exported as `kotlin:defun-kotlin` and `scala:defun-scala`.


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
| `#@abcl{ }` | ABCL | `openldk/abcl` |

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
## ABCL

A second Common Lisp, running on the JVM inside SBCL.  `make abcl`
downloads ABCL's jar.  Starting ABCL takes about a minute and a half
cold; `make sbcl-abcl` builds an SBCL with it started.

```lisp
(asdf:load-system "openldk/abcl")
(abcl:startup)                                     ; already done in sbcl-abcl

(abcl:eval '(lisp-implementation-type))            ; => "Armed Bear Common Lisp"
(abcl:eval '(mapcar (lambda (x) (* x x)) '(1 2 3))) ; => (1 4 9)
(abcl:eval "(defun fact (n) (if (< n 2) 1 (* n (fact (1- n))))) (fact 25)")
                                                   ; => 15511210043330985984000000
(abcl:funcall 'fact 10)                            ; => 3628800
(abcl:eval "(java:jstatic \"max\" \"java.lang.Math\" 3 7)") ; => 7

;; SBCL functions callable from ABCL, with any number of arguments
(abcl:define 'square (lambda (x) (* x x)))
(abcl:eval '(mapcar 'square '(1 2 3)))             ; => (1 4 9)
(abcl:define '*limit* 10)                          ; a special variable
```

ABCL has its own packages, symbols and objects.  An s-expression given to
`eval` is converted to ABCL data: keywords and CL symbols become ABCL's,
another symbol goes to ABCL's package of the same name if it has one, and
otherwise to its CL-USER.  A symbol in a package only ABCL has, such as
`java:jstatic`, can't be read by SBCL, so write such code as a string.  A
string is read by ABCL's reader one form at a time, in ABCL's current
package (CL-USER unless the code changes it).

| Function | |
|---|---|
| `startup &key classpath` | Start ABCL (by default from `default-classpath`, see [Finding the jars](#finding-the-jars)). |
| `eval form` | Evaluate an s-expression or a source string; return the primary value as SBCL data. |
| `eval-string source` | Read and evaluate the forms in a string; return the last value. |
| `load file` | Load an ABCL source or fasl file. |
| `funcall function &rest args` | Call a function, given as an object or by name (a string is read by ABCL's reader). |
| `lookup name` | The function a name names, unconverted. |
| `define name value` | Make a symbol name an SBCL function, or a special variable bound to a value. |
| `to-lisp object`, `from-lisp value` | Convert values. |

| ABCL | SBCL |
|---|---|
| integer, ratio, single-float, double-float | the same |
| string, character | string, character |
| `NIL`, `T`, keyword, CL symbol | the same symbol |
| other symbol | symbol of the same name: in `*package*` for ABCL's CL-USER, otherwise in the SBCL package of the same name if there is one |
| cons, list, simple-vector | cons, list, simple-vector |
| function, other objects | left as is |

An error ABCL code doesn't handle is signalled in SBCL as `foreign-error`
(`foreign-error-object` is the ABCL condition), after ABCL has unwound
its own stack, so its special bindings and `unwind-protect` cleanups are
done.  A Lisp error in an SBCL function that ABCL called comes back as the
original condition.

Tests: `make check-abcl` runs them in `sbcl-abcl`.

## Every language at once: `openldk/polyglot`

`openldk/polyglot` loads every language system; `polyglot:startup` starts
them all (reporting and skipping any that fail) and `polyglot:enable-syntax`
turns on `#@LANGUAGE{ ... }` and Clojure's `#[ ... ]`.  `make sbcl-polyglot`
builds an SBCL with all of them started, so a program can use any of them
at once:

```lisp
(polyglot:enable-syntax)
(let ((primes #[ (take 5 (filter (fn [n] (not-any? #(zero? (rem n %)) (range 2 n))) (iterate inc 2))) ]))
  #@kt{ $primes.map { (it as Number).toLong() * 2 }.sum() })    ; => 56
```

`examples/tea-party.lisp` sets every language at one table:
`./sbcl-polyglot --load examples/tea-party.lisp`.

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

### Foreign functions are Lisp functions

A language's function object -- a Clojure fn, a Scala or Kotlin function, a
Groovy closure, a Kawa or JScheme procedure, an ABCL function -- comes to
Lisp as a `foreign-function`, a funcallable instance, so the ordinary
operators call it:

```lisp
(let ((square (scala:eval "(x: Int) => x * x")))
  (funcall square 7)                    ; => 49
  (mapcar square '(1 2 3))              ; => (1 4 9)
  (sort (list 1 2 3) #'> :key square))  ; => (3 2 1)
```

Passed back to a language -- to `from-lisp`, `define`, or a `$` splice --
it is the original object again.  `foreign-function-object` returns it.

### Errors: typed conditions and restarts

A Java exception escaping a foreign call is signalled as a `foreign-error`
of the subclass its Java class maps to, which is also an instance of the
standard condition type that fits, so handlers can name either:

| Java | Lisp |
|---|---|
| `ArithmeticException` | `foreign-arithmetic-error`, an `arithmetic-error` |
| `ClassCastException` | `foreign-class-cast-error`, a `type-error` |
| `NumberFormatException` | `foreign-number-format-error`, a `parse-error` (and `foreign-illegal-argument-error`) |
| `FileNotFoundException` | `foreign-file-not-found-error`, a `file-error` (and `foreign-io-error`) |
| `StackOverflowError`, `OutOfMemoryError` | `foreign-stack-overflow`, `foreign-out-of-memory`, `storage-condition`s |
| `IndexOutOfBoundsException`, `NullPointerException`, `IllegalStateException`, `UnsupportedOperationException` | `foreign-index-error`, `foreign-null-pointer-error`, ... |

`foreign-error-java-class` names the throwable's class.  The restarts
`use-value` (return a value instead of the call's) and `retry` (make the
call again) are offered; a handler that retries should count its tries:

```lisp
(handler-bind ((foreign-number-format-error (lambda (e) (use-value 0))))
  (groovy:eval "Integer.parseInt('x')"))        ; => 0
```

The other way, a Lisp error in a Lisp function a language called is thrown
into it as the Java exception its type maps to -- a `division-by-zero` as
an `ArithmeticException`, a `type-error` as a `ClassCastException` -- so
the language's code can catch it by type; it comes back out of the foreign
call as the original Lisp condition, even from another thread.

### Collections without copying

`java-sequence` presents a `java.util.List` -- which Clojure's vectors and
lists, Kotlin's and Groovy's lists, and Java's are -- as a Lisp sequence:
`elt`, `length`, `map`, `reduce`, `find`, `subseq` and the rest work on it,
converting only the elements they touch.  `do-java-iterator` and
`java-take` walk any `Iterable` lazily, so an infinite one can be taken
from.  The other way, `lisp-list-view` and `lisp-map-view` present a Lisp
sequence or hash table to Java as a `java.util.List` or `java.util.Map`
backed by it.

```lisp
(let ((v (java-sequence (groovy:eval-raw "(0..<1000000).toList()") :convert #'groovy:to-lisp)))
  (elt v 999999))                       ; => 999999, converting one element
```

### Foreign source files in ASDF systems

A system can hold source files in any of the languages, as components, once
it `:defsystem-depends-on` the language's system (or `openldk/polyglot`):

```lisp
(asdf:defsystem "my-app"
  :defsystem-depends-on ("openldk/polyglot")
  :serial t
  :components ((:maven-jar "commons-text" :coordinates "org.apache.commons:commons-text:1.12.0")
               (:scala-file "stats")      ; compiled by scalac
               (:kotlin-file "shapes")    ; compiled by kotlinc, against Stats and commons-text
               (:groovy-file "greet")     ; compiled by groovyc
               (:clojure-file "words")    ; loaded by Clojure
               (:kawa-file "fact") (:jscheme-file "jfib") (:abcl-file "tower")
               (:file "main")))
```

Kotlin, Scala and Groovy files compile at `compile-op` into directories of
class files beside the system's fasls, against the classes of the
components before them, and go on the classpath at `load-op`; the others
are loaded by their language at `load-op`.  `define-foreign-file-type`
adds a language.

`(:maven-jar "name" :coordinates "group:artifact:version" :sha256 "...")`
puts a Java library from Maven Central on the classpath -- for every
language in the image -- downloading it to OpenLDK's cache the first time
and checking it against `:sha256`, or else the SHA-1 the repository
publishes.  Dependencies of a jar are not followed: list each one.
`add-maven-jar` does the same from Lisp.  `test/polyglot-demo/` is a
complete example (`make check-polyglot`).

### Precompiled blocks

A `#@kotlin{ }` or `#@scala{ }` block whose splices have static types --
none, literals, or `(the type form)` -- is compiled when the Lisp code
around it is, if the language is running then (as in `sbcl-kotlin` or
`sbcl-polyglot`), and its class files go into the fasl:

```lisp
(defun double-it (n)
  (declare (type (signed-byte 32) n))
  #@kt{ $(the (signed-byte 32) n) * 2 })    ; compiled with this file
```

Loading the fasl defines the classes from those bytes, so the compiler
isn't needed at run time, and a compile error is reported by
`compile-file`.  `(signed-byte 32)` is an `Int`, `(signed-byte 64)` and
`fixnum` a `Long`, `double-float` a `Double`, `string` a `String`, `list`
a `List`.  Other blocks compile the first time they run.

### CLOS methods on Java methods

A Java instance method is a CLOS generic function in OpenLDK, so Lisp can
add methods to it: `:before`, `:after`, `:around`, or a primary method,
for a given Java class.  They run on every call, including calls from
compiled Java, Kotlin, Scala or Groovy code.  `define-java-method` loads
the class and defines the method on the full JVM method name:

```lisp
(define-java-method :around ("wonderland.Book" "getPrice()D") (book)
  (* (call-next-method) 0.8d0))     ; a sale, wherever the price is read

(define-java-method :before ("java.util.ArrayList" "clear()V") (list)
  (format t "~&clearing ~D elements~%" (java-call "size()I" list)))
```

### Java classes defined in Lisp

`define-java-class` defines a real Java class whose methods are Lisp code:

```lisp
(define-java-class greeter (:implements ("java.util.function.Function"))
    ((greeting :initform "hello") (count :initform 0))
  ("apply(Ljava/lang/Object;)Ljava/lang/Object;" (self name)
    (incf count)
    (jstring (format nil "~A, ~A (~D)" greeting (lstring name) count))))

(make-java-object 'greeter :greeting "hi")
```

The class is generated as a class file and defined through the application
class loader, so `getClass`, reflection and `Class.forName` see an ordinary
Java class (`lisp.my_package.greeter`): Java code, and languages that call
methods by reflection, can call it like any other object.  Slots are fields
(the method bodies refer to them by name); each method is the full JVM name
of a method the class declares or implements; `:extends` names a superclass
with a public no-argument constructor.  A Lisp error in a method is thrown
into Java and comes back out of `with-foreign-errors` as the original
condition, even from another thread.  `make check-interop` runs the core's
tests.
