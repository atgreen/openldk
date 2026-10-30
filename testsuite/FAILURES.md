# Expected-failure audit (2026-10-30)

The audit covers all **1,623 entries** in the historical expected-failure
file, all **85 Jikes cases**, and the **three unresolved GCJ sources**: 1,711
unique test IDs. It does not claim coverage of the rest of Mauve.

The VM under test was built from `c482328`, using SBCL 2.6.9-1.fc43 on Linux
x86-64. The reference was JDK 25.0.4.1+1. Each test compiled into its own class
directory and each engine ran in a separate working directory. Graphical
Mauve cases used independent Xvfb displays (1280x1024x24), rather than being
misclassified because no display was available. Both engines had 65-second
execution limits; timeouts are recorded explicitly.

The complete case inventory, exit statuses, Java source hashes, and reasons
are in [failure-audit.tsv](failure-audit.tsv). Raw compiler/runtime logs and
resumable JSON results are generated under `.cache/failure-audit/` and are
not committed. They are reproducible using `audit-failures.py`.

## Fixes after the audit

The cloning and array fixes resolve five audited failures:

- `Tryexcept.e`: non-Cloneable objects now throw `CloneNotSupportedException`.
- `array5.e`: negative primitive indexes throw `ArrayIndexOutOfBoundsException`.
- `checkcast6.e` and Mauve `java/lang/System/arraycopy.java`: arraycopy checks
  primitive kinds and reference elements, preserving a copied prefix on failure.
- Mauve `java/lang/Object/ObjectTest.java`: cloning behavior now matches the JDK.

These cases pass fresh JDK 25 comparisons and have been removed from the
expectation file, leaving 332 runtime expectations before the reflection fixes below. The original audit
inventory below is retained as historical evidence. `ArrayExceptionProbe`
adds 217 differential checks, including all primitive kinds, null/bounds
precedence, partial copies, overlap, and shallow cloning. The full core gate
passes with **774 PASS, 1 XFAIL, 7 UNSUPPORTED**; the pinned baseline includes
all 774 passing assertions. Unit checks (818), interop checks (165), and all
48 runtime-standards comparisons pass. Optimized generated-code benchmarks
retain allocation-free array loops.

The reflection conversion fixes additionally resolve Mauve `Array/set`,
`Field/promotion`, `Constructor/newInstance`, and the Beans `Expression/check`
and `Statement/check` cases. Five fresh comparisons pass with the isolated
reflection image. They have been removed from the expectation file, leaving
**327 runtime expectations**. `ReflectionConversionProbe` covers 329 checks
of primitive widening, rejected narrowing/nulls, exact boxing classes,
reference assignment, receiver/arity checks, unchanged argument arrays, and
exception wrapping. Validation passes: build, 818 unit checks, 165 interop
checks, 50 standards comparisons, 20 compatibility comparisons, and 2,153
Clojure checks. The full core gate retains 774 PASS, 1 XFAIL, 7 UNSUPPORTED.

The reflection dispatch fix additionally resolves Mauve `Method/invoke`.
Private methods invoke their declaring implementation; package-private
methods select only genuine Java overrides, including
[transitive overrides](https://docs.oracle.com/javase/specs/jvms/se25/html/jvms-5.html#jvms-5.4.5)
and defining-loader package identity. A fresh isolated JDK 25 comparison
passes, removing one expectation and leaving **326 runtime expectations**.
`ReflectionDispatchProbe` adds 39 differential checks, including a generated
class-file case where an intervening package-private method hides a public
bridge without preventing later transitive overriding. Validation passes:
full build, lint with zero issues, 818 unit checks, 165 interop checks,
52 standards comparisons, 20 compatibility comparisons, and all 2,153
Clojure checks in a rebuilt image. The core gate retains 783 PASS,
1 XFAIL, and 7 UNSUPPORTED without regressions.

The JAR resource-management port adds nine core assertions covering URL
cache identity, uncached archive ownership, stored/deflated streaming,
invalid reads, parent closure, and repeated resource opens. The core gate
now passes **783 PASS, 1 XFAIL, 7 UNSUPPORTED**, all pinned in the baseline.
The ZIP differential suite also passes with a 128-descriptor limit,
abandoned file streams, concurrent archive reads, and saved-image restart
of partially read entries and an ordinary file stream. Build, lint, 818
unit checks, 165 interop checks, 50 standards comparisons, 20 compatibility
comparisons, and a cold Clojure startup smoke test pass.

The floating-point lowering fix resolves all 21 remaining audited Math and
StrictMath expectations, including `MathTest` and the inverse/trigonometric
functions with and without `strictfp`. Fresh isolated JDK 25 comparisons
pass all 788 assertions. Removing those expectations leaves **305 runtime
expectations**. `FloatingArithmeticProbe` adds 3,612 differential results
covering NaNs, signed zeros, infinities, subnormals, ordinary arithmetic,
self-expressions, and StrictMath calls. Unit checks cover reentrant interpreter
arithmetic and allocation-free typed loops. On x86-64, one million single- and
double-precision additions each allocate zero bytes in standalone checks.
Expanded Math/StrictMath validation passes all 50 runnable historical cases;
the other six retain their reference-side or obsolete-version-parser results.
Full validation passes: build, lint, 840 unit checks, 165 interop checks,
54 standards comparisons, 20 compatibility comparisons, and 2,153 Clojure
checks in a rebuilt image. The core gate retains 783 PASS, 1 XFAIL, and
7 UNSUPPORTED without regressions.

The AWT field-ID initializer fix resolves **127 additional expectations**.
Quiet harness reporting additionally resolves the exhaustive color test,
leaving **177 runtime expectations**. A fresh audit of all 276 remaining
AWT-related cases used four independent Xvfb displays with graphical mode
enabled. Those 127 cases pass 1,522 assertions on both JDK 25 and OpenLDK;
constructors, geometry, layouts, listeners, events, and reflection over AWT
classes now proceed through their Java implementations. The initializer
entry points only cache native field/method IDs, which CLOS storage does not
require; no HotSpot AWT library is loaded.

The other 148 AWT cases remain recorded: 146 reach the missing
`X11GraphicsEnvironment.initDisplay` and two require LCMS profile loading.
`ColorClass/brighter` initially timed out while logging successful checks.
With pass-line formatting disabled, all 786,432 assertions complete in under
four seconds, within the original timeout. The runners request quiet pass
reporting; failures, debug messages, assertion totals, and exit statuses are
preserved. `--verbose-checks` restores individual PASS lines in the auditor.
Audit cache identities now include harness sources and reporting mode. `ComponentProbe` adds 14 geometry, event,
and layout checks in each headless mode, and both modes run without an X
server. The complete Java2D pixel, clip, convolution, and font suite passes
with `DISPLAY` unset, as do 840 unit and 165 interop checks. A second audit
with the final quiet harness verifies all 128 resolved cases, preserving
787,954 checks per runtime and the exact counts of every prior verbose run.
Two negative controls retain their native-link failures. `python3 test/harness/check.py` runs twelve harness
checks preserving success/failure accounting, checkpoints, debug output, and
uncaught exceptions on both runtimes; six audit accounting tests pass, and
reporting-mode changes invalidate cached results. The normal DejaGNU runner
also passes the exhaustive color case. Build and lint pass, and the full
core gate retains 783 PASS, 1 XFAIL, and 7 UNSUPPORTED without regressions.

The X11 investigation also uncovered two independent runtime defects.
Registered Lisp bodies now implement native declarations directly, including
private and instance methods, and lazy stubs with replacements cannot inline
the original Java bytecode. `test/native/check-registered.py` exercises these
paths in a fresh ASDF runtime. Escaping Java thread exceptions now reach the
thread's uncaught-exception handler before termination is published.
`ThreadFailureProbe` checks default, custom, group, and virtual-thread
handlers, handler failures, and `join()` waiting for handler completion.
The full build, zero-issue lint, 840 unit checks, 165 interop checks, 56
standards comparisons, and ordinary JNI and FFM probes pass. The core gate
retains 783 PASS, 1 XFAIL, and 7 UNSUPPORTED without regressions. These runtime
fixes do not remove any of the remaining 177 historical expectations; the
X11 backend is still being implemented and validated separately.

A further Java2D initialization defect caused `FontRenderContext` to reject a
null antialiasing hint when default font metrics were requested before
`RenderingHints` constants were accessed. The Lisp replacement for
`GraphicsPrimitiveMgr.initIDs` now initializes the supplied classes, preserving
the initialization performed by JNI field and method lookups. A fresh-process
`FontContextProbe` checks default hints, metrics, and hint-cache updates on both
the JDK and OpenLDK as part of `test/awt/check.py`.

The `PipedReaderWriter` failure exposed the main Java thread being left in
`NEW` state with zero `eetop`, so a writer believed its reader had died.
`currentThread()` now records an already-running thread in both directions
and initializes the Java liveness/state fields. CLI launches retire the main
thread and notify joiners before waiting for non-daemon children; Lisp calls
retain their attached context. The reference processor uses a private
bootstrap identity, and saved images retire build-time thread identities.
`MainThreadProbe`, `MainJoinProbe`, and `make check-thread-image` check character
and byte pipes, joining main after return, and saved-thread state after restart.
The isolated pipe audit passes both checks on each runtime within the normal
65-second limit, so its expectation is removed. The experimental X11 backend
continues to be validated separately.

The main-thread lifecycle repair also resolves the `Thread/daemon`,
`Thread/interrupt`, `Thread/isAlive`, and `Thread/join` expectations. Fresh
isolated comparisons pass all 35 assertions on both runtimes within the normal
65-second limit. Removing those four expectations leaves **172 runtime
expectations**.

Linux UDP support resolves six more expectations: the three `DatagramPacket`
receive/offset cases, `DatagramSocketTest`, binding a datagram socket, and
`MulticastSocketTest`. Fresh serial comparisons pass all 37 assertions on
both runtimes within the normal 65-second limit. Removing these expectations
leaves **166 runtime expectations**. Native socket-address layout queries,
scalar and vector datagram I/O, disconnect, and IPv4 multicast membership
use libc; no HotSpot networking library is loaded.

`DatagramProbe` additionally checks offsets, truncation, packet boundaries,
empty packets, nonblocking reads, receive timeouts, direct/heap scatter-gather
buffers, oversized-packet rejection and subsequent recovery, disconnect and
rebinding, multicast interface/TTL settings, source filtering, and TCP accept.
Socket-address comparison also exposed an `ArraysSupport.vectorizedMismatch`
override that assumed byte arrays. It now honors byte offsets and element
widths for primitive storage, accepts native addresses, and compares tails
without reading outside the requested range. `MismatchProbe` covers primitive
scales, NaNs, signed zero, mixed array storage, native slices, and heap/direct
buffer comparisons against JDK 25.

The byte-array character accessor now returns a numeric UTF-16 code unit,
matching the JVM primitive calling convention. Returning a Lisp character had
made data-stream character comparisons fail despite reading the correct bytes.
Fresh isolated comparisons pass the `DataInputStream/ReadReference` and
`DataOutputStream/WriteRead` cases, with 27 assertions on each runtime. Removing
these two expectations leaves **164 runtime expectations**. `DataCharacterProbe`
checks stream character promotion and arithmetic, boxing, unsigned shorts,
unaligned heap/direct buffers in both byte orders, object-stream characters,
and modified UTF including NUL and surrogate code units. The pre-fix runtime
fails this probe at its first numeric character comparison.

## Results

| Suite audited | Passes | OpenLDK-only differences | Legacy version parser | Rejected by javac 25 | Reference failure | Reference timeout | Helpers without a main |
| --- | ---: | ---: | ---: | ---: | ---: | ---: | ---: |
| Historical Mauve expectations | 873 | 333 | 215 | 31 | 142 | 1 | 0 |
| All Jikes cases | 79 | 4 | 0 | 0 | 0 | 0 | 2 |
| Listed/unresolved GCJ cases | 26 | 0 | 0 | 3 | 2 | 0 | 0 |

At audit completion, `expected-failures.txt` contained **337 verified OpenLDK-only
runtime differences**, replacing 1,623 historical entries and embedded CI
log fragments. Test IDs are exact and case-sensitive.

The **394 excluded legacy/reference-sensitive cases** are listed separately
in [unsupported-tests.txt](unsupported-tests.txt), with reasons. Reference
timeouts are reported as **UNRESOLVED**, not PASS or XFAIL. Jikes' two
helper-only sources are identified automatically and reported UNSUPPORTED.
Excluded or reference-failing cases provide no evidence that OpenLDK is
correct; they need usable tests before compatibility can be judged.

For Mauve, a pass requires both engines to pass. Three tests which passed on
OpenLDK but failed on the reference (two `tanh` cases and `Object/oom`) are
therefore reference-sensitive, not counted as passes. GCJ preserves its
explicit golden-output assertions; its thread scheduling tests can also
be flaky on the reference. Those individual reference statuses remain in
the inventory.

At audit completion, the corrected core gate passed with **771 PASS, 4 XFAIL, and
7 UNSUPPORTED**, with no unexpected failures or unresolved cases. The
baseline then included all 771 passing assertions. The seven exclusions
are the five legacy GCJ cases below and two helper-only Jikes sources.

## Genuine core differences

| Test | Observed difference |
| --- | --- |
| `jikestst/hpj/ok/exec/Tryexcept.e` | Cloning a non-Cloneable object leaks a Lisp assertion instead of throwing `CloneNotSupportedException`. |
| `jikestst/hpj/ok/exec/array5.e` | A negative int-array store throws `NullPointerException` instead of `ArrayIndexOutOfBoundsException`. |
| `jikestst/hpj/ok/exec/checkcast6.e` | `System.arraycopy` accepts incompatible reference elements instead of throwing `ArrayStoreException`. The JDK prints/exits 3; OpenLDK prints/exits 11. Instrumentation confirms the casts themselves reject correctly; the incompatible copy returns normally. |
| `jikestst/derek/ok/exec/TestStackTrace.e` | Stack traces omit source locations (`Unknown Source`). This is a diagnostic compatibility gap, distinct from the three exception/array semantics problems above. |

The audit recorded all four as XFAILs. The subsequent cloning/array fixes
resolve the three semantic failures; the stack-trace source-location gap
remains expected to fail.

## Mauve runtime differences

| Area | OpenLDK-only failing cases |
| --- | ---: |
| AWT / Java2D | 274 |
| `java.lang` | 34 |
| Networking | 12 |
| I/O | 5 |
| Beans | 4 |
| Utilities | 2 |
| NIO | 1 |
| Text | 1 |

These counts include unsupported runtime facilities as well as incorrect
results; they are not 333 independent root causes. For example, 225 cases
stop at the missing `java/awt/Component.initIDs()` hook, and 33 stop at
`sun/awt/X11GraphicsEnvironment.initDisplay(boolean)`. Other repeated missing
hooks include AWT `Dimension` and `MenuItem`, and NIO
`NativeSocketAddress.AFINET()`. The language group includes Math/StrictMath
result checks. The inventory identifies each failing test and its first
failure, so remaining failures can be investigated without rerunning the
entire legacy suite.

## Obsolete and reference-sensitive tests

* **215 version-parser cases:** copied helpers split `java.version` on dots
  and parse `parts[1]` as the feature version. They crash on the valid string
  `"25"` and interpret the reference's `"25.0.x"` as Java zero. These tests
  cannot judge current annotation/reflection behavior until ported, for
  example to `Runtime.version().feature()`. They are not reflection XFAILs.
* **31 Mauve compilation rejections:** old sources use removed/inaccessible
  APIs, including AWT peer access. Their individual javac diagnostics are
  recorded in the exclusion file and raw logs.
* **Three GCJ compilation rejections:** `Divide_1` and `Throw_2` use `_`
  as an identifier, which Java 25 rejects. `PR25535` references the removed
  `javax.rmi.CORBA` API.
* **Two obsolete GCJ assumptions:** `bytearray` expects
  `ClassLoader.loadClass` to accept array descriptors, and `pr26990` installs
  a SecurityManager. JDK 25 rejects both operations too.
* **142 Mauve reference failures:** these include old expected results,
  environment-sensitive assertions, and resource assumptions. They are
  explicitly inconclusive for OpenLDK conformance, rather than assigned to
  the VM's runtime-bug list.
* **One unresolved reference timeout:** AWT `Container/addImpl` exceeded
  65 seconds on the reference. It remains UNRESOLVED pending investigation.

GCJ's two overflow goldens are refreshed to the observed JDK 25 output,
which OpenLDK already matched. No runtime change was needed for either case.

## Harness corrections

The old Jikes runner always launched `Test`, even when the source declared
another main class, and retained classes from earlier cases. Its output
comparison also tested the literal `{expected_failure}` instead of the
variable, silently making every mismatch an XFAIL.

Jikes now compiles in isolation, discovers the actual main (including
`throws` declarations and modifiers), and compares output and exit status
with JDK 25. Intentional uncaught-exception fixtures compare the expected
exception class and preceding output, rather than optional helpful-NPE
messages or 1990s stack-trace formatting. Wrong exception classes and escaped
Lisp assertions still fail. Intentional nonzero `System.exit` values are
compared too. Helper-only declarations are not executed as stale classes.

The shared expectation lookup uses an exact dictionary, not case-insensitive
regular-expression grep or a global last-test name. Malformed and duplicate
expectation entries are rejected. The Mauve temporary-directory typo
`/tmp)` is corrected, and `TestHarness` recompiles with its generated config
under `.cache/mauve-harness`, so the corrected constant is actually used
without modifying tracked legacy bytecode. Each normal Mauve case also
compiles into a fresh cache directory, so a failed compilation cannot run
a stale class from an earlier execution.

## Reproducing the audit

Requirements: Python 3.9+, a built `openldk`, and `JAVA_HOME` pointing to
JDK 25. From the repository root:

```sh
python3 testsuite/test_failure_audit.py
python3 testsuite/audit-failures.py --inventory testsuite/failure-audit.tsv \
  --suite jikestst --jobs 4 --fresh
python3 testsuite/audit-failures.py --inventory testsuite/failure-audit.tsv \
  --suite gcj --jobs 4 --fresh
```

For graphical Mauve tests, provide one already-running, isolated X display
per worker, for example:

```sh
python3 testsuite/audit-failures.py --inventory testsuite/failure-audit.tsv \
  --suite mauve --jobs 4 --displays :97,:98,:99,:100 --fresh
```

Without `--displays`, the audit uses the current environment and may have
different GUI results. Use `--fresh` after changing the JDK, dependencies,
harness, or environment; resumption is for an interrupted unchanged run.
The audit includes excluded cases too, so repaired tests can be re-enabled.

Run one Jikes case with the same machinery as DejaGnu:

```sh
python3 testsuite/audit-failures.py --suite jikestst \
  --case jikestst/hpj/ok/exec/checkcast6.e --fresh
```

To select a normal Mauve DejaGnu case rather than all testlets:

```sh
cd testsuite
MAUVE_TEST_GLOB='mauve/gnu/testlet/java/awt/BasicStroke/createStrokeShape*' \
  runtest --tool openldk mauve/run-mauve.exp
```
