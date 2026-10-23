#!/usr/bin/env bash
#
# Spring Boot startup benchmark: one small Boot application started on
# HotSpot (plain, Spring AOT, the JDK's AOT cache), as a GraalVM native image,
# and from OpenLDK warm and snapshot images.  See bench/spring-boot/README.md
# and, for the environment it reads, bench/common.sh.
#
#   bench/spring-boot/run.sh [build|bench|all]     (default: all)
#
# build  builds every variant into bench/spring-boot/target
# bench  checks each variant's output against HotSpot's and times it

HERE="$(cd "$(dirname "$0")" && pwd)"
TARGET="$HERE/target"
TITLE="Spring Boot"
. "$HERE/../common.sh"

build() {
    log "Building the application (Spring AOT included)"
    maven -Pnative package -DskipTests
    maven dependency:copy-dependencies -DincludeScope=runtime -DoutputDirectory="$TARGET/lib"
    cp "$TARGET/boot-demo-1.0.jar" "$TARGET/boot-demo-fat.jar"

    log "Extracting the jar, as Spring Boot recommends for production"
    rm -rf "$TARGET/extracted"
    # The extracted jar is named after the jar extracted, here boot-demo-1.0.jar.
    "$JAVA" -Djarmode=tools -jar "$TARGET/boot-demo-1.0.jar" extract --destination "$TARGET/extracted"

    # A cache records the classpath as given, so train with the command
    # lines the benchmark runs.
    log "Training the JDK AOT caches"
    local x="$TARGET/extracted"
    (cd "$TARGET" &&
     "$JAVA" -XX:AOTCacheOutput="$x/boot.aot" -jar "$x/boot-demo-1.0.jar" >/dev/null &&
     "$JAVA" -XX:AOTCacheOutput="$x/boot-springaot.aot" -Dspring.aot.enabled=true \
             -jar "$x/boot-demo-1.0.jar" >/dev/null)

    if [ -z "${SKIP_NATIVE:-}" ]; then
        graalvm
        log "Building the GraalVM native image"
        local args
        args="$(native_build_args)"
        JAVA_HOME="$GRAALVM_HOME" maven -Pnative native:compile -DskipTests ${args:+"-DbuildArgs=$args"}
    fi

    if [ -z "${SKIP_OPENLDK:-}" ]; then
        openldk_images "$TARGET/classes:$(ls "$TARGET"/lib/*.jar | tr '\n' ':')" \
                       warm:demo.boot.BootApp snapshot:demo.boot.Snapshot
    fi
}

expected() {
    "$JAVA" -jar "$TARGET/boot-demo-fat.jar"
}

variants() {
    local x="$TARGET/extracted"
    printf '%s\t%s\n' \
        "HotSpot, fat jar" "$JAVA -jar $TARGET/boot-demo-fat.jar" \
        "HotSpot, extracted jar" "$JAVA -jar $x/boot-demo-1.0.jar" \
        "HotSpot + Spring AOT" "$JAVA -Dspring.aot.enabled=true -jar $x/boot-demo-1.0.jar" \
        "HotSpot + JDK AOT cache" "$JAVA -XX:AOTMode=on -XX:AOTCache=$x/boot.aot -jar $x/boot-demo-1.0.jar" \
        "HotSpot + Spring AOT + AOT cache" "$JAVA -Dspring.aot.enabled=true -XX:AOTMode=on -XX:AOTCache=$x/boot-springaot.aot -jar $x/boot-demo-1.0.jar"
    [ -n "${SKIP_NATIVE:-}" ] || printf '%s\t%s\n' "GraalVM native image" "$TARGET/boot-demo"
    [ -n "${SKIP_OPENLDK:-}" ] || printf '%s\t%s\n' \
        "OpenLDK warm image" "$TARGET/openldk-warm" \
        "OpenLDK snapshot image" "$TARGET/openldk-snapshot"
}

bench_main "$@"
