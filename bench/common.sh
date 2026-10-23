# Shared by the startup benchmarks (bench/*/run.sh): the tools, OpenLDK
# image builds and the timing table.  A run.sh sets HERE (its directory),
# TARGET (where it builds) and TITLE, defines build, variants (lines of
# "name<TAB>command") and expected (the reference run), sources this file
# and calls bench_main "$@".
#
# Environment:
#   JAVA_HOME     a JDK 25 (required)
#   GRAALVM_HOME  a GraalVM for JDK 25; downloaded into .cache/bench if unset
#   MVN           Maven; `mvn` on PATH, or downloaded into .cache/bench
#   RUNS          timed runs per variant (default 20)
#   SKIP_NATIVE   set to skip the GraalVM native image
#   SKIP_OPENLDK  set to skip the OpenLDK images
#
# Needs ./openldk built (`make openldk`), sbcl on PATH, curl, and gcc for
# native-image.

set -euo pipefail

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
CACHE="$ROOT/.cache/bench"
RUNS="${RUNS:-20}"

MAVEN_VERSION=3.9.11
GRAALVM_TAG=graal-25.4.4.1.1
GRAALVM_TARBALL=graalvm-community-jdk-25i4-25.0.4.1.1_linux-x64_bin.tar.gz

: "${JAVA_HOME:?set JAVA_HOME to a JDK 25 installation}"
JAVA="$JAVA_HOME/bin/java"
export CL_SOURCE_REGISTRY="${CL_SOURCE_REGISTRY:-$ROOT//:}"

log() { printf '==> %s\n' "$*" >&2; }

# Maven on $HERE/pom.xml, with its repository in .cache/bench.
maven() {
    if [ -n "${MVN:-}" ]; then :
    elif command -v mvn >/dev/null; then MVN=mvn
    else
        MVN="$CACHE/apache-maven-$MAVEN_VERSION/bin/mvn"
        if [ ! -x "$MVN" ]; then
            log "Downloading Maven $MAVEN_VERSION"
            mkdir -p "$CACHE"
            curl -sSfL "https://repo1.maven.org/maven2/org/apache/maven/apache-maven/$MAVEN_VERSION/apache-maven-$MAVEN_VERSION-bin.tar.gz" \
                | tar xz -C "$CACHE"
        fi
    fi
    "$MVN" -q -B -Dmaven.repo.local="$CACHE/m2" -f "$HERE/pom.xml" "$@"
}

graalvm() {
    if [ -z "${GRAALVM_HOME:-}" ]; then
        GRAALVM_HOME="$(ls -d "$CACHE"/graalvm-community-* 2>/dev/null | head -1 || true)"
        if [ -z "$GRAALVM_HOME" ]; then
            log "Downloading GraalVM ($GRAALVM_TAG)"
            mkdir -p "$CACHE"
            curl -sSfL "https://github.com/graalvm/graalvm-ce-builds/releases/download/$GRAALVM_TAG/$GRAALVM_TARBALL" \
                | tar xz -C "$CACHE"
            GRAALVM_HOME="$(ls -d "$CACHE"/graalvm-community-* | head -1)"
        fi
    fi
    export GRAALVM_HOME
}

# Extra native-image arguments for this host.  native-image links with -lz;
# without zlib's development package there is only libz.so.1, so point the
# linker at a libz.so that links to it.
native_build_args() {
    local libz
    for d in /usr/lib64 /usr/lib/x86_64-linux-gnu /usr/lib /lib64; do
        [ -e "$d/libz.so" ] && return 0
    done
    libz="$(/sbin/ldconfig -p 2>/dev/null | awk '/libz\.so\.1 / {print $NF; exit}')"
    [ -n "$libz" ] || return 0
    mkdir -p "$CACHE/libz"
    ln -sf "$libz" "$CACHE/libz/libz.so"
    printf '%s' "-H:NativeLinkerOption=-L$CACHE/libz"
}

# openldk_images CLASSPATH NAME:MAINCLASS[:reset]...
# Build $TARGET/openldk-NAME, a warm image running MAINCLASS from CLASSPATH,
# for each argument, in parallel, with bench/build-image.lisp.  With :reset,
# the application's classes are reset after the warmups, so main starts
# from scratch; without, the image keeps the state the warmups left.
openldk_images() {
    local classpath="$1" pids=() names=() status=0 variant name main reset
    shift
    for variant in "$@"; do
        name="${variant%%:*}"; main="${variant#*:}"; reset=""
        case "$main" in *:reset) main="${main%:reset}"; reset=1 ;; esac
        log "Building the OpenLDK $name image"
        (cd "$ROOT" &&
         BENCH_IMAGE="$TARGET/openldk-$name" BENCH_MAIN="$main" BENCH_RESET="$reset" CLASSPATH="$classpath" \
         sbcl --dynamic-space-size 32768 --disable-debugger \
              --eval "(progn (push (uiop:getcwd) asdf:*central-registry*) (asdf:load-system :openldk))" \
              --load "$ROOT/bench/build-image.lisp" > "$TARGET/openldk-$name.log" 2>&1) &
        pids+=($!); names+=("$name")
    done
    for i in "${!pids[@]}"; do
        wait "${pids[$i]}" || { log "OpenLDK ${names[$i]} image failed; see $TARGET/openldk-${names[$i]}.log"; status=1; }
    done
    return $status
}

# What every variant must print: the application's JSON and round trip.
# (The snapshot images print no banner or startup log, and ahead-of-time
# builds can register fewer framework beans, so the rest differs.)
runner_output() {
    sed -n '/^\[ {$/,/^roundtrip=/p'
}

now_ns() { date +%s%N; }
ms() { awk -v us="$1" 'BEGIN { printf "%.1f ms", us / 1000 }'; }

# The results table: each variant run once to check its output against
# `expected`, then RUNS times, timed, then once more for its peak RSS.
bench() {
    local reference name cmd out times t0 t1 median rss
    reference="$(cd "$TARGET" && expected 2>/dev/null | runner_output)"
    printf '# %s startup, %s runs each\n\n' "$TITLE" "$RUNS"
    printf -- '- Host: %s, %s CPUs\n' "$(sed -n 's/^model name\t*: //p' /proc/cpuinfo | head -1)" "$(nproc)"
    printf -- '- JDK: %s\n' "$("$JAVA" -version 2>&1 | head -1)"
    if [ -z "${SKIP_NATIVE:-}" ]; then
        graalvm
        printf -- '- GraalVM: %s\n' "$("$GRAALVM_HOME/bin/native-image" --version | head -1)"
    fi
    printf -- '- OpenLDK: %s\n' "$(git -C "$ROOT" describe --always --dirty)"
    printf -- '- Load average before: %s\n\n' "$(cut -d' ' -f1-3 /proc/loadavg)"
    printf '| Variant | Median | Min | Max | Peak RSS | Output |\n|---|---|---|---|---|---|\n'
    while IFS=$'\t' read -r name cmd; do
        out="$(cd "$TARGET" && $cmd 2>&1 </dev/null | runner_output)" || true
        times=()
        for _ in $(seq "$RUNS"); do
            t0=$(now_ns); (cd "$TARGET" && $cmd >/dev/null 2>&1 </dev/null) || true; t1=$(now_ns)
            times+=($(( (t1 - t0) / 1000 )))
        done
        mapfile -t times < <(printf '%s\n' "${times[@]}" | sort -n)
        median=${times[$(( ${#times[@]} / 2 ))]}
        rss="n/a"
        if [ -x /usr/bin/time ] &&
           (cd "$TARGET" && /usr/bin/time -o "$TARGET/.rss" -f %M $cmd >/dev/null 2>&1 </dev/null); then
            rss="$(( $(tail -1 "$TARGET/.rss") / 1024 )) MB"
        fi
        printf '| %s | %s | %s | %s | %s | %s |\n' "$name" \
               "$(ms "$median")" "$(ms "${times[0]}")" "$(ms "${times[-1]}")" "$rss" \
               "$([ -n "$out" ] && [ "$out" = "$reference" ] && echo same || echo DIFFERENT)"
    done < <(variants)
    printf '\nLoad average after: %s\n' "$(cut -d' ' -f1-3 /proc/loadavg)"
}

bench_main() {
    case "${1:-all}" in
        build) mkdir -p "$TARGET" && build ;;
        bench) bench | tee "$TARGET/results.md" ;;
        all) mkdir -p "$TARGET" && build && bench | tee "$TARGET/results.md" ;;
        *) echo "usage: $0 [build|bench|all]" >&2; exit 2 ;;
    esac
}
