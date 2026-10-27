# Run Eclipse on OpenLDK

Requires Linux x86_64, Python 3.12 or newer, a full JDK 25, GTK 3,
Cairo, Fontconfig, and installed fonts. A desktop launch needs an X11 or
Wayland session. The automated headless check additionally needs Xvfb,
its keyboard compiler/data, and GDK's PNG support.

Build OpenLDK and its JNI bridge, then launch Eclipse:

```sh
export JAVA_HOME=/path/to/jdk-25
ocicl install
make openldk jni DYNAMIC_SPACE_SIZE=8192
python3 test/eclipse/run.py --download
```

`--download` permits downloading and installing the official Eclipse SDK
4.41 archive (276 MiB), verified against a pinned SHA-512 checksum. The
installation is cached in `.cache/eclipse/eclipse`. Subsequent launches
can omit `--download`; an existing installation works without network
access or an archive. To install without launching:

```sh
python3 test/eclipse/run.py --download --install-only
```

To use your own Eclipse installation and workspace:

```sh
python3 test/eclipse/run.py --install-dir /path/to/eclipse --workspace ~/openldk-workspace
```

The default desktop workspace is `.cache/eclipse/workspace` and persists
across launches. Each launch copies the SDK configuration to its own
`.cache/eclipse/launches/` directory, which also contains `command.txt`
and `console.log`. Console output is also shown in the terminal. The
SDK's plug-ins are not changed and the desktop launcher adds no test plug-in.
`--dry-run` prints the launch command without starting Eclipse.
`--runtime PATH` selects another OpenLDK executable.

Cold startup can take 15 minutes or longer while OpenLDK compiles Java
methods. The native Eclipse JVM launcher is bypassed: OpenLDK runs the
Equinox launcher JAR and loads Eclipse's bundled SWT JNI libraries through
its ordinary JNI bridge. It uses `-Djava.awt.headless=true` for offscreen
Java2D icons; SWT still opens the GTK workbench on your display.

Current status: a full OpenLDK run has opened the Java editor, saved invalid
source and checked JDT diagnostics, saved corrected source, built it, and
executed the resulting class on OpenLDK. Clean workbench shutdown remains
under investigation. Treat the script as a reproducible development setup.

## Repeat the headless editing/build check

```sh
python3 test/eclipse/run.py --download --smoke-test
```

`--headless` is an alias for `--smoke-test`. This mode launches Xvfb,
checks SWT SVG rendering (paths, alpha, gradients, scaling, text, blur, and
curved clipping), then runs Eclipse with a small standard startup extension.
The extension opens a Java editor, saves invalid source, checks its error
markers, corrects and saves the source, and builds `Hello.class`. The runner
executes that class on OpenLDK, captures the workbench, audits loaded native
libraries, and requests normal workbench shutdown. Merely starting Eclipse
does not pass; shutdown failures are reported as failures too.

Each invocation has its own `.cache/eclipse/local-smoke-*/` directory for
compiled test plug-ins, logs, a fresh configuration and workspace, and the
screenshot. The directory is printed when the check starts and retained on
failure. `latest.json` inside it records a completed successful check.
The timeout defaults to 3600 seconds; change it with `--timeout SECONDS`.
The OpenLDK native-library audit requires SWT JNI libraries and rejects
native libraries loaded from `JAVA_HOME`.

`XVFB` specifies an alternative Xvfb executable and arguments. Both modes
inherit GTK/font environment variables such as `LD_LIBRARY_PATH`,
`XDG_DATA_DIRS`, and `FONTCONFIG_PATH`, so dependencies can live in a
user-owned installation. `OPENLDK_JNI_BRIDGE` can override the default
`libopenldk-jni.so` in the repository.

The original integration test, including a HotSpot control, remains available:

```sh
make check-eclipse
# Or check just OpenLDK with an existing build:
python3 test/eclipse/check.py --engine openldk --timeout 3600
```

That runner downloads Eclipse if missing and retains its artifacts in
`.cache/eclipse/runs/`; `.cache/eclipse/latest.json` records successful runs.
