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

Kawa and ABCL are next, on the same core.

## SBCL images with a language started

Starting a language can take a while: Clojure loads `clojure.core` for about
six minutes cold.  These targets build an SBCL executable with the language
started and its common code paths compiled, whose top level is the ordinary
Lisp REPL:

```sh
make sbcl-clojure && ./sbcl-clojure      # Clojure ready at once
make sbcl-jscheme && ./sbcl-jscheme
```

```lisp
* (clojure:eval '(map inc #(1 2 3)))
(2 3 4)
```

Both are built with `openldk.interop:dump-lisp-image`; see below.  (The
`./clojure` and `./jscheme` images of [IMAGES.md](IMAGES.md) are different:
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
