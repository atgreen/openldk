# Lucene vector regression

Run `make check-lucene` with `JAVA_HOME` pointing to JDK 25. The driver downloads
Lucene core 10.6.0 from Maven Central and checks its pinned SHA-256. For an offline
run, use `python3 test/lucene/check.py --jar /path/to/lucene-core-10.6.0.jar`.
Use `--runtime /path/to/openldk` to select a saved runtime.

The same probe runs on JDK 25 and OpenLDK. It checks all four vector similarities,
neighbor identities, finite scores, ordinary document counts, and successful
reader/directory closure with both `MMapDirectory` and `ByteBuffersDirectory`.
Temporary indexes are removed after each run; logs stay in `.cache/lucene-test`.
This covers issue #106, where FFM bulk copies selected byte-array scaling for
float arrays because `Class.isInstance` accepted incompatible primitive arrays.
