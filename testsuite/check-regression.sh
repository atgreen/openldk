#!/usr/bin/env bash
#
# Regression gate for OpenLDK.
#
# Runs the fast "core" DejaGnu suites (aaa/df/gcj/jikestst) and compares the
# result against the pinned baseline in testsuite/baseline.sum. Exits non-zero
# if any test that PASSed in the baseline no longer passes, or if a new
# unexpected failure appears. The full mauve suite (~7000 tests) is intentionally
# excluded here; run it manually with `make check`.
#
# Requires: JAVA_HOME set to a JDK 25; `runtest` (DejaGnu) and a javac on PATH.
# JAVAC defaults to "javac --release 25" so a newer javac still emits class-file
# version 69 (which OpenLDK accepts).

set -u
cd "$(dirname "$0")" || exit 2   # testsuite/

BASELINE="baseline.sum"
SUITES="aaa/run.exp df.exp gcj/gcj.exp jikestst/jikestst.exp"

: "${JAVA_HOME:?set JAVA_HOME to a JDK 25 installation}"
export JAVAC="${JAVAC:-javac --release 25}"

if [ ! -f "$BASELINE" ]; then
    echo "ERROR: missing baseline $BASELINE" >&2
    exit 2
fi

rm -f openldk.sum openldk.log
# runtest exits non-zero whenever any test FAILs, which is expected here; the
# real pass/fail decision is the baseline comparison below.
runtest --tool openldk $SUITES >/dev/null 2>&1 || true

if [ ! -f openldk.sum ]; then
    echo "ERROR: test run produced no openldk.sum" >&2
    exit 2
fi

lost=$(comm -23 <(grep '^PASS:' "$BASELINE" | sort -u) \
                <(grep '^PASS:' openldk.sum | sort -u))
newfail=$(comm -13 <(grep '^FAIL:' "$BASELINE" | sort -u) \
                   <(grep '^FAIL:' openldk.sum | sort -u))

status=0
if [ -n "$lost" ]; then
    echo "REGRESSION: tests that passed in the baseline no longer pass:"
    echo "$lost" | sed 's/^/  /'
    status=1
fi
if [ -n "$newfail" ]; then
    echo "REGRESSION: new unexpected failures:"
    echo "$newfail" | sed 's/^/  /'
    status=1
fi

base_pass=$(grep -c '^PASS:' "$BASELINE")
cur_pass=$(grep -c '^PASS:' openldk.sum)
echo "core suites: baseline PASS=$base_pass  current PASS=$cur_pass"
if [ "$status" -eq 0 ]; then
    echo "GATE OK: no regressions vs $BASELINE"
else
    echo "GATE FAILED"
fi
exit "$status"
