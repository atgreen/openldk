# Building Executable Images of Java Programs

OpenLDK compiles Java to native code through SBCL, so a Java program can be
saved as a standalone SBCL executable with `save-lisp-and-die`.  There are
three ways to do it, depending on how much startup work you want the image
to have done already.

| Builder | What the image contains | Example |
|---|---|---|
| `make-image` | The OpenLDK runtime, JDK bootstrap done | `openldk` |
| `dump-app-image` | The above, plus a default main class and classpath | `make kawa` |
| `dump-warm-image` | The above, plus the program's code already compiled | `make abcl` |

All three are in `src/cli.lisp`.  OpenLDK compiles each Java method the
first time it is called, and for a large program that is most of its startup
time.  Only `dump-warm-image` saves that work.

## `dump-app-image`: a program with its classpath baked in

```lisp
(openldk::dump-app-image "kawa" "kawa.repl" :classpath "lib/kawa-3.1.1.jar")
```

The executable runs `kawa.repl` from that classpath, and `-cp` on the
command line overrides the classpath.  The first other argument is taken as
a main class that overrides the default, so to pass arguments to the
default `main`, name the class first:

```bash
./kawa kawa.repl -e '(display 42)'
```

The program still compiles its own code as it runs.

`make kawa` builds this example.

## `dump-warm-image`: a program whose code is already compiled

```lisp
(openldk::dump-warm-image "abcl" "org.armedbear.lisp.ABCLMain"
                          :classpath "lib/abcl/abcl.jar:lib/abcl"
                          :warmups (list (list :input "(print (+ 1 2))
(quit)
")))
```

This runs the program at build time, once for each entry in `:warmups`, and
then saves.  Each warmup is a plist:

- `:args` – a list of strings passed to `main`
- `:input` – a string the program reads as its standard input

Every method a warmup calls is compiled into the image, so pick warmups that
exercise the code paths real runs will take.  Each warmup is limited to
`:timeout` seconds (600 by default).  A warmup that throws, times out or
calls `System.exit` is reported in the build log, and the build carries on.

**Warm code, cold state.**  Running `main` at build time also runs the
program's static initializers and leaves whatever state `main` built in
static fields.  If that were saved, `main` in the image would start where
the warmup ended: a singleton would already exist, a registry would already
be full.  So by default, after warming up, `dump-warm-image` resets every
class from the application classpath: its static fields go back to their
initial values and its `<clinit>` runs again on first use.  Compiled
methods are kept.  JDK classes keep their state, as they do in every image.

For a program with `static int runs` that `main` increments, warmed up once
and then run from the image:

| | Prints |
|---|---|
| Fresh JVM | `runs=1` |
| `dump-warm-image` (default) | `runs=1` |
| `dump-warm-image :reset-classes nil` | `runs=2` |

**Warm code, warm state.**  The reset only works when each class's
`<clinit>` rebuilds all of its static state.  Some programs fill in static
fields from outside the class, through reflection or a loader, and those
fields come back null after a reset.  For them, pass `:reset-classes nil`
to keep the state the warmups built, and write `main` to reuse it.  This
can also make startup much faster, since the program's own initialization
is saved too.  ABCL is such a program; see below.

Before saving, `%reset-runtime-for-save` kills leftover threads, drops
process-local handles (open jars, mmapped JDK images, off-heap memory), and
gives every lock a fresh mutex.  `make-javac-image` uses the same function.

### Example: `make abcl`

`make abcl` downloads ABCL 1.9.2, compiles its entry point, and runs
`lib/abcl/build-image.lisp`.  That script warms ABCL up with one REPL
session covering the reader and printer, `defun`, CLOS, conditions,
`format`, `loop`, hash tables, strings and ABCL's compiler, closures
included, then saves `./abcl`.

ABCL's compiled Lisp classes get their static fields from its fasl loader,
so the script uses `:reset-classes nil`: the image holds a fully booted
Lisp, and `lib/abcl/ABCLMain.java` reuses the interpreter it finds instead
of creating one.  The warmup works in a scratch package and deletes it, so
its definitions don't end up in the image.  ABCL's command-line options
were processed at build time, so the image ignores them.

```bash
make abcl
echo '(print (+ 1 2))' | ./abcl
```

Measured on one machine:

| | Cold (`openldk` + `ABCLMain`) | Warm image (`./abcl`) |
|---|---|---|
| Launch to prompt, then `(quit)` | ~3.5 min | 0.1 s |
| A short session (`defun`, `defclass`, `defgeneric`, `defmethod`, `loop`) | over 3.5 min (startup alone) | 14 s |
| Compiling closures (`compile` on two closure-making functions, then CLOS) | over 7 min | 11 s |
| Image size | — | 740 MB |
| Build time (`make abcl`) | — | ~8.5 min |

Forms the warmup didn't cover still compile their Java code on first use,
so they take a few seconds the first time.  With ABCL's compiler loaded,
ABCL compiles forms such as `defclass` into new Java classes, and those
always go through OpenLDK's JIT when first used.

The warmup's last form resets ABCL's prompt counter
(`top-level::*cmd-number*`), so the image's first prompt is `CL-USER(1)`.

### Example: `javacl`

`javacl`, built by `make`, is the original warm image and predates
`dump-warm-image`.  `make-javac-image` in `src/javac.lisp` preloads a list
of javac classes, compiles `Hello.java` and the mauve test harness at build
time, and saves.  javac keeps no state between runs that matters, so it
skips the class reset.

## Writing your own

1. Load OpenLDK into SBCL with a large heap:
   ```bash
   sbcl --dynamic-space-size 32768 \
        --eval '(progn (push (uiop:getcwd) asdf:*central-registry*) (asdf:load-system :openldk))' \
        --load build-image.lisp
   ```
2. In `build-image.lisp`, call `openldk::dump-warm-image` with your main
   class, your classpath, and warmups that run your program the way users
   will.
3. Check the log for `;; WARMUP ... caught` or `timed out`.  A failed warmup
   still produces an image; it just has less code compiled.

Things to watch for:

- **The image is tied to its build.**  The classpath is stored as given, so
  use paths that exist where the image runs.  `JAVA_HOME` must point at the
  same JDK 25 at runtime.
- **Program state outside its own statics survives.**  Files the warmup
  wrote, shutdown hooks it registered and system properties it set stay as
  the warmup left them.
- **Open jars reopen.**  A `java.util.zip.ZipFile` the program opened
  during a warmup reopens its file on first use in the image, so keep the
  file where it was.
- **Threads don't survive.**  Any thread a warmup started is killed before
  saving.  A thread pool the warmup created keeps believing in its dead
  workers, and a task handed to one never runs, so with `:reset-classes
  nil`, don't let a warmup create pools the program will use again.  The
  JDK's own process reaper pool (`ProcessHandleImpl`) is reset before
  saving, so warmups may start child processes.
