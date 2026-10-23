#!/usr/bin/env bash
#
# Spring Boot startup benchmark: one small Boot application started on
# HotSpot (plain, Spring AOT, the JDK's AOT cache), as a GraalVM native image,
# and from OpenLDK warm and snapshot images.  See bench/spring-boot/README.md.
#
#   bench/spring-boot/run.sh [build|bench|all]     (default: all)
#
# build  builds every variant into bench/spring-boot/target
# bench  checks each variant's output against HotSpot's and times it
#
# Environment:
#   JAVA_HOME     a JDK 25 (required)
#   GRAALVM_HOME  a GraalVM for JDK 25; downloaded into .cache/bench if unset
#   MVN           Maven; `mvn` on PATH, or downloaded into .cache/bench
#   RUNS          timed runs per variant (default 20)
#   SKIP_NATIVE   set to skip the GraalVM native image
#   SKIP_OPENLDK  set to skip the OpenLDK images
#
# Needs ./openldk built (`make openldk`; `make bench-spring-boot` does both),
# sbcl on PATH, curl, and gcc for native-image.

set -euo pipefail

HERE="$(cd "$(dirname "$0")" && pwd)"
ROOT="$(cd "$HERE/../.." && pwd)"
TARGET="$HERE/target"
CACHE="$ROOT/.cache/bench"
RUNS="${RUNS:-20}"

MAVEN_VERSION=3.9.11
GRAALVM_TAG=graal-25.4.4.1.1
GRAALVM_TARBALL=graalvm-community-jdk-25i4-25.0.4.1.1_linux-x64_bin.tar.gz

: "${JAVA_HOME:?set JAVA_HOME to a JDK 25 installation}"
JAVA="$JAVA_HOME/bin/java"
export CL_SOURCE_REGISTRY="${CL_SOURCE_REGISTRY:-$ROOT//:}"

log() { printf '==> %s\n' "$*" >&2; }

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

# native-image links with -lz.  Without zlib's development package there is
# only libz.so.1, so point the linker at a libz.so that links to it.
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

build() {
    mkdir -p "$TARGET"
    log "Building the application (Spring AOT included)"
    JAVA_HOME="$JAVA_HOME" maven -Pnative package -DskipTests
    JAVA_HOME="$JAVA_HOME" maven dependency:copy-dependencies -DincludeScope=runtime -DoutputDirectory="$TARGET/lib"
    cp "$TARGET/boot-demo-1.0.jar" "$TARGET/boot-demo-fat.jar"

    log "Extracting the jar, as Spring Boot recommends for production"
    rm -rf "$TARGET/extracted"
    "$JAVA" -Djarmode=tools -jar "$TARGET/boot-demo-fat.jar" extract --destination "$TARGET/extracted"

    log "Training the JDK AOT caches"
    (cd "$TARGET/extracted" &&
     "$JAVA" -XX:AOTCacheOutput=boot.aot -jar boot-demo-1.0.jar >/dev/null &&
     "$JAVA" -XX:AOTCacheOutput=boot-springaot.aot -Dspring.aot.enabled=true -jar boot-demo-1.0.jar >/dev/null)

    if [ -z "${SKIP_NATIVE:-}" ]; then
        graalvm
        log "Building the GraalVM native image"
        local args
        args="$(native_build_args)"
        JAVA_HOME="$GRAALVM_HOME" maven -Pnative native:compile -DskipTests ${args:+"-DbuildArgs=$args"}
    fi

    if [ -z "${SKIP_OPENLDK:-}" ]; then
        local classpath pids=() names=() status=0
        classpath="$TARGET/classes:$(ls "$TARGET"/lib/*.jar | tr '\n' ':')"
        for variant in warm:demo.boot.BootApp snapshot:demo.boot.Snapshot; do
            log "Building the OpenLDK ${variant%%:*} image"
            (cd "$ROOT" &&
             BOOT_IMAGE="$TARGET/openldk-${variant%%:*}" BOOT_MAIN="${variant#*:}" CLASSPATH="$classpath" \
             sbcl --dynamic-space-size 32768 --disable-debugger \
                  --eval "(progn (push (uiop:getcwd) asdf:*central-registry*) (asdf:load-system :openldk))" \
                  --load "$HERE/build-image.lisp" > "$TARGET/openldk-${variant%%:*}.log" 2>&1) &
            pids+=($!); names+=("${variant%%:*}")
        done
        for i in "${!pids[@]}"; do
            wait "${pids[$i]}" || { log "OpenLDK ${names[$i]} image failed; see $TARGET/openldk-${names[$i]}.log"; status=1; }
        done
        return $status
    fi
}

variants() {
    local x="$TARGET/extracted"
    printf '%s\t%s\n' \
        "HotSpot, fat jar" "$JAVA -jar $TARGET/boot-demo-fat.jar" \
        "HotSpot, extracted jar" "$JAVA -jar $x/boot-demo-1.0.jar" \
        "HotSpot + Spring AOT" "$JAVA -Dspring.aot.enabled=true -jar $x/boot-demo-1.0.jar" \
        "HotSpot + JDK AOT cache" "$JAVA -XX:AOTCache=$x/boot.aot -jar $x/boot-demo-1.0.jar" \
        "HotSpot + Spring AOT + AOT cache" "$JAVA -Dspring.aot.enabled=true -XX:AOTCache=$x/boot-springaot.aot -jar $x/boot-demo-1.0.jar"
    [ -n "${SKIP_NATIVE:-}" ] || printf '%s\t%s\n' "GraalVM native image" "$TARGET/boot-demo"
    [ -n "${SKIP_OPENLDK:-}" ] || printf '%s\t%s\n' \
        "OpenLDK warm image" "$TARGET/openldk-warm" \
        "OpenLDK snapshot image" "$TARGET/openldk-snapshot"
}

# What every variant must print: the runner's JSON and round trip.  (The
# snapshot image prints no banner or startup log, and Spring AOT registers
# fewer infrastructure beans, so the rest differs.)
runner_output() {
    sed -n '/^\[ {$/,/^roundtrip=/p'
}

now_ns() { date +%s%N; }
ms() { awk -v us="$1" 'BEGIN { printf "%.1f ms", us / 1000 }'; }

bench() {
    local expected name cmd out times t0 t1 median rss
    expected="$($JAVA -jar "$TARGET/boot-demo-fat.jar" | runner_output)"
    printf '# Spring Boot startup, %s runs each\n\n' "$RUNS"
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
        if [ -x /usr/bin/time ]; then
            rss="$( { (cd "$TARGET" && /usr/bin/time -f %M $cmd >/dev/null 2>/dev/null </dev/null); } 2>&1 | tail -1)"
            rss="$(( rss / 1024 )) MB"
        fi
        printf '| %s | %s | %s | %s | %s | %s |\n' "$name" \
               "$(ms "$median")" "$(ms "${times[0]}")" "$(ms "${times[-1]}")" "$rss" \
               "$([ "$out" = "$expected" ] && echo same || echo DIFFERENT)"
    done < <(variants)
    printf '\nLoad average after: %s\n' "$(cut -d' ' -f1-3 /proc/loadavg)"
}

case "${1:-all}" in
    build) build ;;
    bench) bench | tee "$TARGET/results.md" ;;
    all) build && bench | tee "$TARGET/results.md" ;;
    *) echo "usage: $0 [build|bench|all]" >&2; exit 2 ;;
esac
