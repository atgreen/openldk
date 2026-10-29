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

The current ownership checkpoint passes the root probe on OpenLDK and all full
probes on JDK 25. Full OpenLDK collection and JNI teardown remain in development;
relation caches and native ownership still need conversion. The full probes
are retained here to verify that work, rather than being reported as passing.
