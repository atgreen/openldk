---
name: grind
description: OpenLDK-local autonomous work loop. Survey the Beads queue, pick the highest-value next task (prioritizing correctness and progress toward running real Java on the CLOS/JIT runtime), reprioritize the queue accordingly, then execute it end-to-end with the project's build + regression-gate discipline. Trigger when the user says "/grind", "grind", "pick the next thing and do it", "keep making progress", or wants an autonomous session that decides and works without hand-holding.
---

# grind — the OpenLDK progress loop

You are advancing **OpenLDK**: a JIT compiler and runtime for **Java (JDK 25)**
implemented in **Common Lisp (SBCL)**. It translates Java bytecode to Lisp,
compiles that to native code, maps Java classes to **CLOS** classes, and runs on
the real headless OpenJDK 25 runtime libraries (jimage/jrt classpath,
field-backed `Class` metadata, invokedynamic/record/typeSwitch support). The
converging goal: run real, unmodified Java programs correctly — up to and
including javac itself (the `javacl` image).

`/grind` = **decide what to work on next, reprioritize the queue so that
decision is legible, then do the work to completion.** One good increment,
committed and closed, beats a half-finished heroic epic. Prefer correctness and
demonstrable progress toward running real Java over breadth.

## 0. Orient (every run)

```bash
bd prime            # workflow + memories (if context is stale)
bd ready            # claimable work
bd stats            # open/blocked/in-progress shape
bd memories java    # persistent knowledge (build gotchas, native rules, gate)
```

Skim the recalled Beads memories — especially the **build gotcha** (JAVA_HOME)
and the **regression-baseline** notes — before touching anything. Pull the
relevant `src/*.lisp` file on demand for the subsystem you touch; do **not** read
all of `src/`. The runtime core is large:

- `src/openldk.lisp` — main entry, class loading, classpath, bytecode→Lisp
- `src/native/*.lisp` — native JDK method implementations (very large)
- `src/global-state.lisp` — global state / special vars
- `src/monitor.lisp`, `src/fiber.lisp` — monitors, virtual-thread fibers

## 1. Choose the next task — selection rubric

Score candidates in this order and pick the highest that is **tractable now**:

1. **Correctness first.** A bug that produces *wrong results* (silent
   miscompiles, wrong values, spurious exceptions, aborts, hangs) outranks any
   feature or perf work. Wrong answers poison everything built on top.
2. **On the critical path to running real Java.** Prefer work that lets more
   real, unmodified Java run correctly on JDK 25 — class loading, reflection,
   invokedynamic/lambda, records, the `javacl`/javac self-host path.
3. **Serves the engine goal.** Bytecode→CLOS fidelity, JIT correctness, native
   JDK method coverage, monitor/thread/fiber semantics. Value work that makes the
   *engine* real, not incidental surface.
4. **Tractable and verifiable.** A clear done-signal and a way to *observe* it
   end-to-end (a Java program that now prints the right value, a test that flips,
   a gate that ratchets up). Favor a well-scoped bug or a decomposable slice over
   an open-ended epic.

Deprioritize: cosmetic cleanups, speculative features, anything with no line to
running-real-Java, and **tracking/rollup epics** — don't "work" a rollup;
decompose it into a concrete child and do that.

When the top candidate is a large epic, carve off the smallest child that
delivers real, verifiable value and do *that* this run.

## 2. Anti-rat-hole guardrails

- **Time-box the investigation.** If after a bounded dig the task reveals itself
  as an architecture epic (multi-subsystem, no clear done-signal), **stop**:
  write findings + a decomposition as Beads, pick a smaller adjacent win, and
  proceed. Don't sink the session into a bottomless path.
- **Chase a done-signal, not a rabbit.** Every task needs an observable "it
  works now" (a Java class that returns the right value, a testsuite test that
  flips PASS, a gate that ratchets). If you can't state it, you're rat-holing.
- **Commit validated increments.** Don't stockpile a giant uncommitted change.
  Land each proven step; the next session/agent benefits.
- **A decision that's genuinely the maintainer's** (a semantics policy, a big
  irreversible direction) — surface it briefly and pick the safe default or ask,
  rather than guessing and building the wrong thing at length.

## 3. Lisp/OpenLDK discipline (non-negotiable)

- **Build with the right JDK, every time.** The shell env exports a stale
  `JAVA_HOME` (openjdk@21, no `lib/modules`) that silently breaks the build.
  Always build explicitly and with `pipefail` so make's exit code isn't masked:

  ```bash
  set -o pipefail
  JAVA_HOME=/usr/lib/jvm/java-25-openjdk make openldk 2>&1 | tail -40
  ```

  The real binary is `./openldk` (rebuilt by the target above). `make check-jdk`
  fails fast if `JAVA_HOME` isn't a JDK 25.

- **Balance parens with the pipe-symbol hazard in mind.** `|java/lang/Foo|`
  symbols contain literal parens inside the pipes; reader conditionals
  (`#+sb-fiber`) skip one form but parens must still balance. A naive paren
  checker will lie — account for `|...|` and reader conditionals.

- **Follow the native-method naming rule** (see the recalled Beads memory) when
  adding a `src/native/*.lisp` method that shadows a JDK-provided native.

- **Don't run heavy openldk work concurrently with the regression gate.** The
  testsuite uses a 40s per-test timeout; a competing load causes spurious
  thread/exit-test failures (Thread.interrupted, System.exit, Process_*). Run the
  gate alone.

## 4. Reprioritize the queue

Make the decision legible by aligning priorities with the rubric — *before* you
start coding:

- Raise correctness bugs and critical-path/goal work that are currently
  underranked; lower speculative or off-goal items. `bd update <id> --priority N`.
- Leave a one-line `bd comment` on anything you re-rank, saying why (e.g.
  "raise: wrong-result bug on the reflection path" / "lower: cosmetic, off
  critical path"). Keep churn minimal — reprioritize to *reflect* the plan, not
  to reshuffle the whole board.
- File newly discovered work as Beads immediately (never a `// TODO` or a mental
  note): `bd create "…" -t bug|task -p N`. Model blockers with `bd dep add`.

## 5. Execute end-to-end

```bash
bd update <id> --claim
```

1. Implement the smallest correct change. Match surrounding Lisp idiom.
2. Rebuild the real binary (§3) and **drive the affected flow** — the /verify
   discipline, not just tests. Wrong-result bugs demand you *see* the right value
   now: compile a tiny Java repro and run it:

   ```bash
   JAVA_HOME=/usr/lib/jvm/java-25-openjdk /usr/lib/jvm/java-25-openjdk/bin/javac Repro.java
   ./openldk -cp . Repro        # or: LDK_CLASSPATH=. ./openldk Repro
   ```

3. Run the fast regression gate and confirm no dropped pass / new failure:

   ```bash
   JAVA_HOME=/usr/lib/jvm/java-25-openjdk make check-regression
   ```

   Use `make check` for the full mauve suite when the change warrants it. If you
   legitimately increase PASS count, consider ratcheting `testsuite/baseline.sum`
   and say so.
4. Commit with an imperative subject citing the bead id (match the repo's
   `ldk-xxx: …` subject style); end the message with:
   `Co-Authored-By: Claude Opus 4.8 (1M context) <noreply@anthropic.com>`
5. `bd close <id>` with the commit hash and what was verified; `bd sync`.
   `git push` only when the user asks.

## 6. Loop or hand off

Report: what you picked and **why** (the rubric line it satisfied), the change,
how you verified it (the Java repro output + gate result), what you re-ranked,
and any new Beads filed. Then pick the next task (repeat) until told to stop or
the queue has no tractable correctness/goal work left — at which point say so
plainly rather than inventing busywork.
