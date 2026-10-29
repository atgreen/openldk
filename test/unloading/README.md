# Class-loader lifetime probes

With `JAVA_HOME` pointing to JDK 25:

```sh
python3 test/unloading/check.py --roots-only
python3 test/unloading/check.py
python3 test/unloading/check.py --jni
```

The root probe checks live classes, instances, arrays, reflected methods, method
handles, and VarHandles, including shadowed fields, static state, and
caller-sensitive lookup identity. The full probe releases those roots, checks
reference-queue delivery, repeats same-name loader creation, and exercises an
executing method. Static self references intentionally form cycles.

The JNI fixture checks that global references retain a loader while weak
references and method/field IDs do not. It records initialization/unload events,
checks `GetEnv` during `JNI_OnUnload`, and reloads the same library path in three
successive loaders. It uses ordinary JNI headers and no HotSpot libraries.

Use `--reference-only` to run either full probe against the reference JDK.
Generated classes and logs are under `.cache/unloading-test/`.

The full Java probe also checks generated lambdas and method-handle proxies,
a lambda Class mirror, synchronized objects, and unloaded-class management
accounting. Lambdas and proxies target JDK methods so that the wrapper itself
must retain its defining loader. Use `--verbose` to report individual collection
cases while diagnosing a failure.

Both full probes pass on OpenLDK and JDK 25. The native probe additionally uses
cached member IDs before collection and checks that `FindClass` during
`JNI_OnUnload` uses the system loader, as required by the
[JNI specification](https://docs.oracle.com/en/java/javase/25/docs/specs/jni/functions.html#findclass).
