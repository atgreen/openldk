#!/usr/bin/env bash
#
# Quarkus startup benchmark: one small Quarkus application started on
# HotSpot (fast-jar, uber-jar, the JDK's AOT cache), as a GraalVM native
# image, and from OpenLDK warm and snapshot images.  See
# bench/quarkus/README.md and, for the environment it reads,
# bench/common.sh.
#
#   bench/quarkus/run.sh [build|bench|all]     (default: all)
#
# build  builds every variant into bench/quarkus/target
# bench  checks each variant's output against HotSpot's and times it

HERE="$(cd "$(dirname "$0")" && pwd)"
TARGET="$HERE/target"
TITLE="Quarkus"
. "$HERE/../common.sh"

# Each packaging rebuilds target/, so build them one at a time and keep
# each one's output.
build() {
    if [ -z "${SKIP_NATIVE:-}" ]; then
        graalvm
        log "Building the GraalVM native image"
        local args
        args="$(native_build_args)"
        JAVA_HOME="$GRAALVM_HOME" maven -Pnative package -DskipTests \
                 ${args:+"-Dquarkus.native.additional-build-args=$args"}
        mv "$TARGET/quarkus-demo-1.0-runner" "$TARGET/quarkus-native"
    fi

    # The OpenLDK images run the uber-jar: the fast-jar's classes are
    # defined by Quarkus's RunnerClassLoader, a new loader each run, so an
    # image could not keep their compiled code.  The warm image resets the
    # application's classes, as Quarkus's static state outlives a run.
    log "Building the uber-jar"
    maven package -DskipTests -Dquarkus.package.jar.type=uber-jar
    mv "$TARGET/quarkus-demo-1.0-runner.jar" "$TARGET/quarkus-uber.jar"

    log "Building the fast-jar, Quarkus's default packaging"
    rm -rf "$TARGET/quarkus-app"
    maven package -DskipTests

    # The cache records the classpath as given, so train with the command
    # line the benchmark runs.
    log "Training the JDK AOT cache"
    (cd "$TARGET" && "$JAVA" -XX:AOTCacheOutput="$TARGET/quarkus-app/app.aot" \
                             -jar "$TARGET/quarkus-app/quarkus-run.jar" >/dev/null)

    if [ -z "${SKIP_OPENLDK:-}" ]; then
        openldk_images "$TARGET/quarkus-uber.jar" \
                       warm:io.quarkus.runner.GeneratedMain:reset snapshot:demo.quarkus.Snapshot
    fi
}

expected() {
    "$JAVA" -jar "$TARGET/quarkus-app/quarkus-run.jar"
}

variants() {
    local app="$TARGET/quarkus-app"
    printf '%s\t%s\n' \
        "HotSpot, fast-jar" "$JAVA -jar $app/quarkus-run.jar" \
        "HotSpot, uber-jar" "$JAVA -jar $TARGET/quarkus-uber.jar" \
        "HotSpot + JDK AOT cache" "$JAVA -XX:AOTMode=on -XX:AOTCache=$app/app.aot -jar $app/quarkus-run.jar"
    [ -n "${SKIP_NATIVE:-}" ] || printf '%s\t%s\n' "GraalVM native image" "$TARGET/quarkus-native"
    [ -n "${SKIP_OPENLDK:-}" ] || printf '%s\t%s\n' \
        "OpenLDK warm image" "$TARGET/openldk-warm" \
        "OpenLDK snapshot image" "$TARGET/openldk-snapshot"
}

bench_main "$@"
