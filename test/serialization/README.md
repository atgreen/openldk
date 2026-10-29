Run with JDK 25 and a freshly built OpenLDK:

```sh
JAVA_HOME=/path/to/jdk-25 python3 test/serialization/check.py
```

The runner compares output with the reference JDK for the original GCJ
serialization and reflection failures, plus a probe covering default
serialVersionUID computation (declared versus inherited static initializers),
cyclic object graphs, inherited fields, custom-loader deserialization,
interned static String constants including non-ASCII text, and reflective
invocation of a method throwing plain Throwable. Logs are saved under
`.cache/serialization-test/`.

The runner also exercises the 8,201-field GCJ class-loading regression and a
generated wide-class probe covering zero defaults, inherited fields, static
storage, custom-loader construction and loader collection. Wide Java classes
use an interpreted CLOS definition and loop-based slot initialization to avoid
SBCL compiling a huge fast initializer.
