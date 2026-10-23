# Spring Boot startup benchmark

How fast one small Spring Boot application starts, run every way it can be:
on HotSpot, as a GraalVM native image, and from OpenLDK images.

```bash
make bench-spring-boot              # build everything, then time it
bench/spring-boot/run.sh build      # just build
bench/spring-boot/run.sh bench      # just time what is built (RUNS=20 by default)
```

The first run downloads Maven and GraalVM CE into `.cache/bench`, unless
`mvn` is on `PATH` and `GRAALVM_HOME` is set; Maven's repository goes there
too.  Builds go to `bench/spring-boot/target`, about 2 GB with the OpenLDK
images.  `SKIP_NATIVE=1` or `SKIP_OPENLDK=1` leave out the slow builds.  The
results are printed as a Markdown table and saved in `target/results.md`.

## The application

`demo.boot.BootApp` (Spring Boot 4.1.2, Spring Framework 7) uses
auto-configuration, binds a `@ConfigurationProperties` record from
`application.properties`, injects an `@Value`, and has a `CommandLineRunner`
write JSON with the auto-configured Jackson 3 mapper and read it back.  It
logs through Logback and prints the banner.  A run is the whole process,
from exec to exit.

## The variants

| Variant | What runs |
|---|---|
| HotSpot, fat jar | `java -jar` on the jar `spring-boot-maven-plugin` builds |
| HotSpot, extracted jar | The jar extracted with `-Djarmode=tools extract`, as Spring Boot recommends for production |
| HotSpot + Spring AOT | The extracted jar with `-Dspring.aot.enabled=true`: the bean definitions Spring AOT generated at build time instead of classpath scanning |
| HotSpot + JDK AOT cache | The extracted jar with a JDK 25 AOT cache (JEP 483/514/515) from one training run |
| HotSpot + Spring AOT + AOT cache | Both |
| GraalVM native image | `mvn -Pnative native:compile`, which also uses Spring AOT |
| OpenLDK warm image | `dump-warm-image` after two runs of `BootApp`: the code is compiled, but Boot starts from scratch |
| OpenLDK snapshot image | `dump-warm-image` of `demo.boot.Snapshot`, whose first warmup run starts Boot and keeps the context, so the image only runs the `CommandLineRunner`s |

The OpenLDK snapshot is the counterpart of a CRaC checkpoint, not of a native
image: Boot's startup happened when the image was built.  It prints the
runner's output but no banner or startup log.  The script checks every
variant's runner output (the JSON and the round trip) against plain
HotSpot's and reports `same` or `DIFFERENT`; Spring AOT variants report 61
beans rather than 65, which is expected.

## Results on the development host

A 16-CPU Xeon E5-2670 (2.6 GHz), JDK 25.0.4.1 (Fedora OpenJDK), GraalVM CE
25.0.4.1.1, load average 5 to 8 from other work, 20 runs each:

| Variant | Median | Peak RSS | On disk |
|---|---|---|---|
| HotSpot, fat jar | 3.4 s | 261 MB | 13 MB jar |
| HotSpot, extracted jar | 2.9 s | 224 MB | |
| HotSpot + Spring AOT | 2.5 s | 195 MB | |
| HotSpot + JDK AOT cache | 1.4 s | 182 MB | 45 MB cache |
| HotSpot + Spring AOT + AOT cache | 955 ms | 160 MB | 42 MB cache |
| GraalVM native image | 72 ms | 41 MB | 45 MB |
| OpenLDK warm image | 4.3 s | 496 MB | 628 MB |
| OpenLDK snapshot image | 82 ms | 177 MB | 624 MB |

A HotSpot run spends about 13 s of CPU in 3 to 4 s, mostly the JIT compiling
Spring's startup path, so HotSpot's times depend on how busy the machine is;
note the load averages the script prints.  The native image and the OpenLDK
snapshot barely change with load.  The native image still runs Boot's
startup, compiled ahead of time; the snapshot skips it, and most of its time
is mapping the 624 MB image (a bare `openldk -version` takes about 48 ms).

A JDK AOT cache is only used with the classpath it was trained on, as
given: the script trains each with the exact command line it times, and
runs them with `-XX:AOTMode=on` so that a rejected cache fails the run
instead of quietly running without it.
