# Native Libraries: JNI and FFM

[Project overview](../README.md) · [Java compatibility](COMPATIBILITY.md)

## JNI libraries

OpenLDK can load ordinary third-party JNI libraries alongside its existing
Common Lisp native implementations. Build the bridge with `make jni` (also
included in `make`). This requires a C compiler, Python 3, JDK 25 headers, and
the libffi development package. The current bridge targets 64-bit little-endian
Linux and uses the platform C ABI.

Java code can use `System.load("/absolute/path/libexample.so")`, or
`System.loadLibrary("example")` with `-Djava.library.path=/path/to/libraries`.
The bridge is loaded lazily from `libopenldk-jni.so` in the build directory;
set `OPENLDK_JNI_BRIDGE` to its absolute path when relocating an executable.
It uses standard JNI headers and does not load HotSpot's JDK libraries.
Existing Lisp native implementations retain precedence.

JNI and FFM use the JDK 25 native-access policy. For classpath applications,
pass `--enable-native-access=ALL-UNNAMED`; resolved named modules can be granted
access with a comma-separated module list. Unpermitted restricted operations
warn by default. `--illegal-native-access=allow|warn|deny` selects the policy.
Permissions also work through `JDK_JAVA_OPTIONS`, argument files, and an
executable JAR's `Enable-Native-Access: ALL-UNNAMED` manifest attribute.
Run `make check-native-access` for [policy comparisons](../test/native-access/README.md).

The implementation supports native symbol lookup and registration, loader
ownership, initialization hooks, local/global/weak references, object creation,
Java callbacks, fields, reflection, exceptions, strings, primitive/object arrays,
direct buffers, monitors, and native-thread attachment. Strings and primitive
arrays use native copies with release/copy-back semantics. A native thread keeps
its Java identity across callbacks, including daemon status and monitor ownership.

JNI libraries owned by custom class loaders unload after their loader becomes
unreachable, including `JNI_OnUnload` notification. JNI global references retain
Java objects; weak references and cached method/field IDs do not prevent class
unloading. Creating or destroying a standalone JVM via
the Invocation API is not supported. Saved images must load JNI libraries after
restart: saving with loaded JNI libraries is rejected because arbitrary native
library state cannot be preserved in an SBCL image. JNI compatibility is tested
with the included C/C++ probes and zstd-jni; it is not a claim of complete JNI
conformance for every library.

## Foreign Function & Memory API

OpenLDK supports native linking through the JDK 25 Java fallback linker and
its own libffi bridge. It does not load the JDK's native fallback-linker library.
`Linker.nativeLinker()` supports downcalls and arena-owned upcall stubs,
including callbacks from native threads. Symbol lookup supports system symbols,
arena-owned C libraries, and libraries loaded through JNI.

The integration probes cover scalar types, pointers, structs passed and returned
by value, variadic calls, errno capture, heap-array arguments to critical calls,
and arena lifetime and thread checks. Heap arguments use native copies, sharing
copies for aliases. The fallback linker rejects unions passed by value. The
current bridge targets 64-bit little-endian Linux; other platforms have not
been validated. Run `make check-foreign` to compare the probes with JDK 25.
