# Native-access policy comparisons

`make check-native-access` builds ordinary JNI fixture libraries and compares
93 launcher/policy cases with JDK 25. `JAVA_HOME` must identify a JDK 25;
the bridge uses the existing Linux/libffi build requirements.

The cases compare Java results and counts of restricted-method, native-binding,
and unknown-module warnings. They cover:

- Default warning behavior and explicit allow, warn, and deny policies.
- JNI loading, automatic binding, FFM reinterpretation, and shared warning state.
- Missing symbols, null filenames, relative paths, failed loads, and Runtime loads.
- Classpath permissions, resolved boot-module grants, repeated options, and
  unknown modules.
- Named modules in an application layer, programmatic permission grants,
  library ownership, and automatic versus registered JNI bindings.
- Launcher argument files, quoted environment options, rejected environment
  main classes, disabled argument-file expansion, and malformed options.
- Source launches and executable JAR manifest permissions.

OpenLDK delegates the policy to the installed JDK's `Module` and `Reflection`
implementations. Launcher permissions update actual module flags, and JNI
loading and automatic symbol resolution enter the same checks as FFM. The
image's unnamed-module warning state is reset at process startup.

Two ordering details matter: a missing native symbol produces
`UnsatisfiedLinkError` even under deny mode, and native code remains free to
use `RegisterNatives`. Registering a binding is not an automatic native-library
lookup and does not itself require granting the declaring module native access.
The named-module fixtures exercise both paths independently.

For a focused run, use:

```sh
python3 test/native-access/check.py --match named
python3 test/native-access/check.py --reference-only
```

Logs and generated artifacts are in `.cache/native-access-test/`. This suite
covers native-access policy; JNI class-loader unloading is tested separately
when implemented.
