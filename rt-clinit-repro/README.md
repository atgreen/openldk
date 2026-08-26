# Reproducer: clojure.lang.RT.<clinit> fails under OpenLDK (JDK 25)

Minimal, self-contained repro — no cl-clojure, no `clojure.main`. `RTInit`
just forces `clojure.lang.RT` to initialize via `Class.forName`, which runs
its static initializer.

## Files
- `RTInit.java` / `RTInit.class` — the trigger (compiled with `--release 11`)
- `clojure-1.12.0.jar` — stock Clojure 1.12.0

## Run
```sh
JAVA_HOME=/usr/lib/jvm/java-25-openjdk \
  ../openldk -cp .:clojure-1.12.0.jar RTInit
```

Expected (on success): `clojure.lang.RT initialized OK`

## Actual
```
;; <clinit> ERROR in clojure/lang/RT: The function
   OPENLDK::|compareAndSet(Ljava/util/concurrent/atomic/AtomicReference;Ljava/lang/Object;Ljava/lang/Object;)|
   is undefined.
Unhandled Java exception: java.lang.ExceptionInInitializerError
```

`RT.<clinit>` calls `AtomicReference.compareAndSet(...)`, which OpenLDK has
not implemented (a CAS intrinsic, ultimately `Unsafe.compareAndSetReference`).
Because `RT` never finishes initializing, every downstream Clojure operation
fails. When driven through a fuller load path (e.g. cl-clojure warming
namespaces) the same failure also manifests as a cascade of undefined
`%clinit-*` stubs and `no applicable method for JAVA-CLASS with arguments
(NIL)`.

See `repro.log` for a full backtrace.
