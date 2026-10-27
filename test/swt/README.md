The SWT smoke test runs a real GTK window on a virtual X display. It checks
window creation, an editable text widget, text round trips, a native timer
callback into Java, event dispatch, and disposal. It does not test Eclipse.

On Linux x86_64, install GTK 3, Xvfb (including its keyboard compiler and
keyboard data), and fonts. With `JAVA_HOME` pointing to JDK 25 and OpenLDK
already built, run from the repository root:

```sh
python3 test/swt/check.py
```

The runner downloads checksum-pinned SWT 3.135.0 from Maven Central, extracts
its ordinary JNI libraries, and runs the same probe on HotSpot and OpenLDK.
It needs no physical display, desktop session, or HotSpot GUI libraries for
the OpenLDK run. Artifacts and logs are kept in `.cache/swt/`.

`XVFB` can specify an alternative Xvfb executable and arguments. Standard
environment variables such as `LD_LIBRARY_PATH` and `FONTCONFIG_PATH` are
inherited, allowing GTK and Xvfb to live in a temporary installation.
