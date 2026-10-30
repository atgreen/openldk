Run from the repository root with JDK 25 and the installed SBCL dependencies:

```sh
export JAVA_HOME=/path/to/jdk-25
export CL_SOURCE_REGISTRY="$PWD//:"
export XDG_CACHE_HOME="$PWD/.cache"
sbcl --dynamic-space-size 512 --noinform --disable-debugger --load test/oom/build-small.lisp
python3 test/oom/check.py --runtime .cache/openldk-oom-small --native-recovery
```

The test image uses the normal runtime bootstrap and saved-image reset. It
omits the optional broad JDK warm-up because compiling its security methods
can exceed this small heap before applications run. The normal OpenLDK build
retains its warm-up and configured heap size.

Each Java case runs in a fresh process. The reference JDK uses a 128 MiB heap;
the OpenLDK test image uses a real 512 MiB SBCL dynamic space. Cases cover
oversized requests, three cycles of retained-array exhaustion and recovery,
primitive and reference arrays, reflection, multidimensional arrays, four
concurrent allocators, and the original GCJ newarray_overflow regression.
Every case checks that execution continues and small allocations still work.
Zero dimensions and negative-size exception precedence are checked too.

The native recovery probe checks an impossible host request, then crowds the
heap with retained Java arrays while preserving the production GC reserve.
It bypasses the preflight for a half-heap host vector, which fits before the
heap is crowded, and forces a real SBCL allocation failure three times using
the production recovery macro. Each failure must become the preconstructed
Java OutOfMemoryError condition. It then releases the retained arrays,
collects, allocates the half-heap vector again, and makes small Java arrays.
The GC reserve matters: bypassing it for all allocations can exhaust the
space SBCL needs to copy live objects, making collection itself fatal.
SBCL's low-level heap diagnostics are expected in this probe's log; an
aborted process or missing recovery marker is a failure.

Logs are written under `.cache/oom-test/`. The runner rejects large-heap test
runtimes before making huge allocations.
