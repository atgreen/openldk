The Eclipse integration test launches Eclipse SDK 4.41 with Xvfb and a small
standard Eclipse startup extension. It opens a Java editor, saves an invalid
program and checks JDT's diagnostics, corrects the program, builds it, runs
the compiled class through OpenLDK, captures the workbench, and requests a
normal workbench shutdown. A process that merely remains alive does not pass.

Current status: the HotSpot control passes. OpenLDK runs SWT and reaches
workbench construction, but the full Eclipse check does not yet pass.
SVG icon rendering currently reaches unsupported Java2D native methods;
Eclipse can then overflow its stack while formatting the resulting error.
The test remains a completion target, rather than a claim that Eclipse works.

Requires Linux x86_64, GTK 3, fonts, Xvfb with its keyboard compiler/data,
Python 3.12 or newer, and `JAVA_HOME` pointing to a full JDK 25.

```sh
make check-eclipse
```

The runner downloads the official Eclipse SDK archive (276 MiB) and verifies
its SHA-512. It runs the same extension on HotSpot as a control and OpenLDK.
It uses separate, fresh configurations and workspaces and does not modify
the SDK's plug-ins. The bundled SWT JNI libraries are loaded by OpenLDK's
ordinary JNI bridge; the native Eclipse JVM launcher is bypassed.
The OpenLDK run records its loaded native libraries and checks that SWT
is loaded without using native libraries from `JAVA_HOME`.

For an existing build, or when isolating a failure:

```sh
python3 test/eclipse/check.py --engine openldk --timeout 1800
```

`--runtime` selects an alternate OpenLDK executable. `XVFB` specifies an
alternative Xvfb executable and arguments. The runner inherits GTK/font
environment variables, including `LD_LIBRARY_PATH` and `FONTCONFIG_PATH`,
so dependencies can reside in a temporary installation.

Logs, the saved Java project, and screenshots remain in `.cache/eclipse/runs/`.
`native-libraries.txt` records the OpenLDK process's native-library mappings.
`.cache/eclipse/latest.json` points to each engine's latest successful run.
