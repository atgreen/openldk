# Quarkus startup benchmark

How fast one small Quarkus application starts, run every way it can be: on
HotSpot, as a GraalVM native image, and from OpenLDK images.  The
counterpart of [the Spring Boot benchmark](../spring-boot/README.md).

```bash
make bench-quarkus              # build everything, then time it
bench/quarkus/run.sh build      # just build
bench/quarkus/run.sh bench      # just time what is built (RUNS=20 by default)
```

The tools, environment variables and output are those of the Spring Boot
benchmark, described in its README and in `bench/common.sh`.  Builds go to
`bench/quarkus/target`.

## The application

`demo.quarkus.QuarkusApp` (Quarkus 3.40.1) is a command-mode application
(`@QuarkusMain`): CDI beans, a `@ConfigMapping` interface bound from
`application.properties`, a `@ConfigProperty`, and the Quarkus-managed
Jackson mapper writing JSON and reading it back.  It logs through JBoss
Logging and prints the banner.  A run is the whole process, from exec to
exit.

## The variants

| Variant | What runs |
|---|---|
| HotSpot, fast-jar | `java -jar quarkus-app/quarkus-run.jar`, Quarkus's default packaging: its RunnerClassLoader loads the application from an index built at build time |
| HotSpot, uber-jar | One jar with every class, on the JDK's application class loader |
| HotSpot + JDK AOT cache | The fast-jar with a JDK 25 AOT cache from one training run |
| GraalVM native image | `mvn -Pnative package` |
| OpenLDK warm image | `dump-warm-image` after two runs of the uber-jar: the code is compiled, but Quarkus starts from scratch |
| OpenLDK snapshot image | `dump-warm-image` of `demo.quarkus.Snapshot`, whose first warmup run starts Quarkus (`io.quarkus.runner.ApplicationImpl`) and keeps it, so the image only runs the application |

The OpenLDK images run the uber-jar, not the fast-jar.  The fast-jar's
classes are defined by a RunnerClassLoader each run creates afresh, and
OpenLDK compiles a class loader's classes for that loader, so an image
could not keep their compiled code.  OpenLDK runs the fast-jar too
(`openldk -jar quarkus-app/quarkus-run.jar`), compiling as it goes.

As with Spring Boot, the snapshot is the counterpart of a CRaC checkpoint:
Quarkus's startup happened when the image was built, and it prints the
application's output but no banner or startup log.

## Results

On the same host as the Spring Boot benchmark (a 16-CPU Xeon E5-2670, load
average 5 to 7 from other work), JDK 25.0.4.1, GraalVM CE 25.0.4.1.1, 20
runs each:

| Variant | Median | Min | Max | Peak RSS | On disk |
|---|---|---|---|---|---|
| HotSpot, fast-jar | 1184 ms | 1088 ms | 1238 ms | 95 MB | |
| HotSpot, uber-jar | 1281 ms | 1210 ms | 1442 ms | 108 MB | 7 MB jar |
| HotSpot + JDK AOT cache | 703 ms | 633 ms | 763 ms | 87 MB | 31 MB cache |
| GraalVM native image | 19.9 ms | 18.2 ms | 27.0 ms | 23 MB | 32 MB |
| OpenLDK warm image | 1547 ms | 1448 ms | 3108 ms | 352 MB | 467 MB |
| OpenLDK snapshot image | 71.8 ms | 65.8 ms | 91.8 ms | 154 MB | 460 MB |

Every variant printed the same JSON and round trip as HotSpot.  Without an
image, `openldk -jar quarkus-run.jar` takes about two minutes, nearly all
of it compiling.

Quarkus does at build time much of what Spring Boot does at startup, and it
shows: HotSpot starts it in a third of Spring Boot's time, and the OpenLDK
warm image, which still runs all of Quarkus's startup, comes within 30% of
HotSpot.  The snapshot is within a few times of the native image: what it
runs is the application, not Quarkus's startup, and most of its time is
mapping the image.
