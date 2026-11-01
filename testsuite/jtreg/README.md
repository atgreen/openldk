<!--
SPDX-License-Identifier: GPL-3.0-or-later WITH Classpath-exception-2.0
This file is part of OpenLDK.
-->
# Running the OpenJDK jtreg suite against OpenLDK

This directory holds tooling to drive the upstream OpenJDK regression suite
([jtreg](https://openjdk.org/jtreg/)) with OpenLDK as the *tested JDK*.

## Prerequisites

- A built `openldk` launcher (`make openldk`).
- A real JDK 25 for its class library. The default is the Temurin image
  OpenLDK caches under `$XDG_CACHE_HOME/openldk/` (see `src/fetch.lisp`);
  override with `TEMURIN=<jdk-home>`.
- jtreg 7.x (built from [openjdk/jtreg](https://github.com/openjdk/jtreg) or a
  prebuilt binary). jtreg 7.x matches JDK 25's `java.class.version` (69.0).
- OpenJDK test sources checked out at the **`jdk-25`** tag
  ([openjdk/jdk](https://github.com/openjdk/jdk)); `test/jdk` carries a
  `TEST.ROOT`.

## 1. Build the shim JDK image

jtreg's `-jdk:<home>` wants a JDK *directory*. OpenLDK is one launcher that
reads a stock JDK payload, so `make-openldk-jdk.sh` assembles a directory that
*is* that payload (release + lib symlinked from Temurin) with `bin/java`
overlaid to invoke OpenLDK:

```sh
testsuite/jtreg/make-openldk-jdk.sh          # writes $HOME/openldk-jdk
# or: SHIM=/path/out TEMURIN=/path/jdk-25 testsuite/jtreg/make-openldk-jdk.sh
```

It finishes by running `bin/java -version`, which should report the OpenLDK
identity (`java.vm.name = OpenLDK`, `java.version = 25`).

## 2. Run jtreg

```sh
JDKSRC="$HOME/git/jdk"          # openjdk/jdk at the jdk-25 tag
SHIM="$HOME/openldk-jdk"
TEMURIN="$HOME/.cache/openldk/jdk/jdk-25.0.4.1+1"

jtreg \
  -jdk:"$SHIM" \                # run the tests on OpenLDK
  -compilejdk:"$TEMURIN" \      # compile them with stock javac (isolates execution from javacl)
  -othervm \                    # fresh VM per test; OpenLDK's ~0.12s warm start makes this viable
  -conc:auto \
  -ignore:quiet \               # don't choke on @ignore'd tests
  -va \                         # verbose: show every result
  -w:"$PWD/jtreg-work" -r:"$PWD/jtreg-report" \
  "$JDKSRC/test/jdk/java/lang" "$JDKSRC/test/jdk/java/util"
```

Notes baked into this invocation:

- jtreg itself adds `-ea -esa` to the tested VM. The launcher accepts these
  (see `src/cli.lisp`); **they are currently a no-op** — assertions are not
  yet actually enabled, so a test that relies on `-ea` to detect failure will
  pass vacuously until real assertion support lands.
- `-compilejdk` separates javac maturity from what is under test (OpenLDK's
  *execution* of already-compiled classes).
- `-othervm` avoids the agentvm agent's socket/serialization surface.
- Start narrow (`java/lang`, `java/util`, plain `@run main` tests) before
  widening into JUnit/TestNG groups or `test/lib`-dependent suites. Skip
  `test/hotspot` entirely.
- Capture the first report as a baseline and triage the failure tail into
  "real" vs. "assertion-dependent / unimplemented-native" buckets.
