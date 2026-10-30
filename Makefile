JAVA_HOME ?= /usr/lib/jvm/java-25-openjdk

# Compiler used to build the testsuite's Java sources. Decoupled from JAVA_HOME
# because a headless JDK ships no javac; a newer javac targeting release 25
# produces class-file version 69, which OpenLDK accepts.
JAVAC ?= javac --release 25

XDG_CACHE_HOME ?= $(CURDIR)/.cache

# Dynamic space reserved by SBCL, in MB.  save-lisp-and-die bakes this into
# every image we dump (:save-runtime-options t), so it cannot be overridden on
# the image's own command line -- and SBCL startup scales with it: a bare SBCL
# starts in ~19ms at 512, ~43ms at 2048 and ~81ms at 32768.  Large enough here
# for the JIT to compile big applications; lower it for an image whose Java
# heap is modest and whose startup time matters (see bench/spring-boot).
DYNAMIC_SPACE_SIZE ?= 32768

# Let ASDF find this project's systems without a manual central-registry push.
# The trailing ':' keeps the inherited (ocicl) source registry.
CL_SOURCE_REGISTRY ?= $(CURDIR)//:

export JAVA_HOME JAVAC XDG_CACHE_HOME CL_SOURCE_REGISTRY

KAWA_VERSION = 3.1.1
KAWA_JAR = lib/kawa-$(KAWA_VERSION).jar
KAWA_URL = https://repo1.maven.org/maven2/com/github/arvyy/kawa/$(KAWA_VERSION)/kawa-$(KAWA_VERSION).jar

CLOJURE_JARS = lib/clojure/clojure-1.12.4.jar lib/clojure/spec.alpha-0.5.238.jar lib/clojure/core.specs.alpha-0.4.74.jar
MAVEN_URL = https://repo1.maven.org/maven2

JSCHEME_URL = https://norvig.com/jscheme
JSCHEME_SRC_JAR = lib/jscheme/jscheme-src.jar
JSCHEME_MAIN = lib/jscheme/classes/jscheme/Scheme.class

SCALA_VERSION = 2.13.16
SCALA_JARS = lib/scala/scala-library-$(SCALA_VERSION).jar lib/scala/scala-reflect-$(SCALA_VERSION).jar lib/scala/scala-compiler-$(SCALA_VERSION).jar

KOTLIN_VERSION = 2.2.20
KOTLIN_JARS = lib/kotlin/kotlin-stdlib-$(KOTLIN_VERSION).jar lib/kotlin/kotlin-compiler-embeddable-$(KOTLIN_VERSION).jar \
  lib/kotlin/kotlin-reflect-$(KOTLIN_VERSION).jar lib/kotlin/kotlin-script-runtime-$(KOTLIN_VERSION).jar \
  lib/kotlin/kotlin-daemon-embeddable-$(KOTLIN_VERSION).jar lib/kotlin/kotlinx-coroutines-core-jvm-1.8.0.jar \
  lib/kotlin/trove4j-1.0.20200330.jar lib/kotlin/annotations-13.0.jar

GROOVY_VERSION = 4.0.27
GROOVY_JAR = lib/groovy/groovy-$(GROOVY_VERSION).jar

ABCL_VERSION = 1.9.2
ABCL_JAR = lib/abcl/abcl.jar
ABCL_URL = https://abcl.org/releases/$(ABCL_VERSION)/abcl-bin-$(ABCL_VERSION).tar.gz
ABCL_MAIN = lib/abcl/org/armedbear/lisp/ABCLMain.class

all: jni openldk javacl

JNI_OS = $(shell uname -s | tr A-Z a-z)
JNI_BRIDGE = libopenldk-jni.so
.PHONY: jni
jni: $(JNI_BRIDGE)

.cache/jni/table.inc: src/jni/generate.py $(JAVA_HOME)/include/jni.h | check-jdk
	mkdir -p .cache/jni
	python3 $< "$(JAVA_HOME)/include/jni.h" $@

$(JNI_BRIDGE): src/jni/bridge.c src/jni/ffi.c .cache/jni/table.inc
	$(CC) $(CPPFLAGS) $(CFLAGS) -std=c11 -D_GNU_SOURCE -fPIC -shared -Wall -Wextra -Werror \
	  -I"$(JAVA_HOME)/include" -I"$(JAVA_HOME)/include/$(JNI_OS)" -I.cache/jni \
	  src/jni/bridge.c src/jni/ffi.c $(LDFLAGS) -lffi -ldl -o $@

# Fail fast when JAVA_HOME is not a JDK 25 (e.g. the environment exports an
# older JDK) instead of erroring deep inside the Lisp build.  Order-only
# prerequisite so it never makes the image targets out of date.
.PHONY: check-jdk
check-jdk:
	@test -n "$(JAVA_HOME)" || { echo "error: JAVA_HOME is not set; OpenLDK requires a JDK 25 (e.g. /usr/lib/jvm/java-25-openjdk)"; exit 1; }
	@grep -qs '^JAVA_VERSION="25' "$(JAVA_HOME)/release" || { echo "error: JAVA_HOME=$(JAVA_HOME) is not a JDK 25 installation (OpenLDK requires JDK 25)"; exit 1; }

openldk: src/*.lisp src/native/*.lisp src/SourceCompiler.java *.asd Makefile | check-jdk
	sbcl --dynamic-space-size $(DYNAMIC_SPACE_SIZE) --disable-debugger --eval "(progn (push (uiop:getcwd) asdf:*central-registry*) (asdf:load-system :openldk))" --eval "(openldk::make-image)"

javacl: src/*.lisp src/native/*.lisp src/SourceCompiler.java *.asd Makefile | check-jdk
	sbcl --dynamic-space-size $(DYNAMIC_SPACE_SIZE) --disable-debugger --eval "(progn (push (uiop:getcwd) asdf:*central-registry*) (asdf:load-system \"javacl\"))" --eval "(openldk::make-javac-image)"

$(KAWA_JAR):
	mkdir -p lib
	curl -L -o $(KAWA_JAR) $(KAWA_URL)

# A warm image: Kawa's Java code is compiled at build time (see docs/IMAGES.md).
kawa: src/*.lisp src/native/*.lisp *.asd Makefile lib/kawa/build-image.lisp $(KAWA_JAR) | check-jdk
	sbcl --dynamic-space-size $(DYNAMIC_SPACE_SIZE) --disable-debugger --eval "(progn (push (uiop:getcwd) asdf:*central-registry*) (asdf:load-system :openldk))" --load lib/kawa/build-image.lisp

$(ABCL_JAR):
	curl -sSfL $(ABCL_URL) | tar xzO abcl-bin-$(ABCL_VERSION)/abcl.jar > $@.tmp
	mv $@.tmp $@

# javac through the JDK's own module, so a JDK without bin/javac works too.
$(ABCL_MAIN): lib/abcl/ABCLMain.java $(ABCL_JAR)
	$(JAVA_HOME)/bin/java -m jdk.compiler/com.sun.tools.javac.Main -cp $(ABCL_JAR) -d lib/abcl lib/abcl/ABCLMain.java

# A warm image: ABCL's Java code is compiled at build time (see docs/IMAGES.md).
abcl: src/*.lisp src/native/*.lisp *.asd Makefile lib/abcl/build-image.lisp $(ABCL_MAIN) | check-jdk
	sbcl --dynamic-space-size $(DYNAMIC_SPACE_SIZE) --disable-debugger --eval "(progn (push (uiop:getcwd) asdf:*central-registry*) (asdf:load-system :openldk))" --load lib/abcl/build-image.lisp

lib/clojure/clojure-1.12.4.jar:
	curl -sSfL -o $@.tmp $(MAVEN_URL)/org/clojure/clojure/1.12.4/clojure-1.12.4.jar
	mv $@.tmp $@

lib/clojure/spec.alpha-0.5.238.jar:
	curl -sSfL -o $@.tmp $(MAVEN_URL)/org/clojure/spec.alpha/0.5.238/spec.alpha-0.5.238.jar
	mv $@.tmp $@

lib/clojure/core.specs.alpha-0.4.74.jar:
	curl -sSfL -o $@.tmp $(MAVEN_URL)/org/clojure/core.specs.alpha/0.4.74/core.specs.alpha-0.4.74.jar
	mv $@.tmp $@

# A warm image: clojure.core loaded and Clojure's Java code compiled at
# build time (see docs/IMAGES.md).
clojure: src/*.lisp src/native/*.lisp *.asd Makefile lib/clojure/build-image.lisp $(CLOJURE_JARS) | check-jdk
	sbcl --dynamic-space-size $(DYNAMIC_SPACE_SIZE) --disable-debugger --eval "(progn (push (uiop:getcwd) asdf:*central-registry*) (asdf:load-system :openldk))" --load lib/clojure/build-image.lisp

lib/scala/scala-%-$(SCALA_VERSION).jar:
	mkdir -p lib/scala
	curl -sSfL -o $@.tmp $(MAVEN_URL)/org/scala-lang/scala-$*/$(SCALA_VERSION)/scala-$*-$(SCALA_VERSION).jar
	mv $@.tmp $@

lib/kotlin/kotlin-%-$(KOTLIN_VERSION).jar:
	mkdir -p lib/kotlin
	curl -sSfL -o $@.tmp $(MAVEN_URL)/org/jetbrains/kotlin/kotlin-$*/$(KOTLIN_VERSION)/kotlin-$*-$(KOTLIN_VERSION).jar
	mv $@.tmp $@

lib/kotlin/kotlinx-coroutines-core-jvm-1.8.0.jar:
	mkdir -p lib/kotlin
	curl -sSfL -o $@.tmp $(MAVEN_URL)/org/jetbrains/kotlinx/kotlinx-coroutines-core-jvm/1.8.0/kotlinx-coroutines-core-jvm-1.8.0.jar
	mv $@.tmp $@

lib/kotlin/trove4j-1.0.20200330.jar:
	mkdir -p lib/kotlin
	curl -sSfL -o $@.tmp $(MAVEN_URL)/org/jetbrains/intellij/deps/trove4j/1.0.20200330/trove4j-1.0.20200330.jar
	mv $@.tmp $@

lib/kotlin/annotations-13.0.jar:
	mkdir -p lib/kotlin
	curl -sSfL -o $@.tmp $(MAVEN_URL)/org/jetbrains/annotations/13.0/annotations-13.0.jar
	mv $@.tmp $@

$(GROOVY_JAR):
	mkdir -p lib/groovy
	curl -sSfL -o $@.tmp $(MAVEN_URL)/org/apache/groovy/groovy/$(GROOVY_VERSION)/groovy-$(GROOVY_VERSION).jar
	mv $@.tmp $@

$(JSCHEME_SRC_JAR):
	curl -sSfL -o $@.tmp $(JSCHEME_URL)/jscheme.jar
	mv $@.tmp $@

lib/jscheme/r4rstest.scm:
	curl -sSfL -o $@.tmp $(JSCHEME_URL)/r4rstest.scm
	mv $@.tmp $@

# jscheme.jar holds JScheme's Java sources (Java 1.4); compile them here.
$(JSCHEME_MAIN): $(JSCHEME_SRC_JAR)
	rm -rf lib/jscheme/src lib/jscheme/classes
	mkdir -p lib/jscheme/src lib/jscheme/classes
	cd lib/jscheme/src && unzip -q ../jscheme-src.jar
	$(JAVAC) -nowarn -d lib/jscheme/classes lib/jscheme/src/jscheme/*.java

# Tests for openldk/interop, the shared core (Java classes defined in Lisp, ...).
check-interop: | check-jdk
	sbcl --dynamic-space-size $(DYNAMIC_SPACE_SIZE) --non-interactive --eval "(progn (push (uiop:getcwd) asdf:*central-registry*) (asdf:test-system \"openldk/interop\"))"

# Tests for openldk/jscheme, using JScheme from Lisp (see docs/INTEROP.md).
check-jscheme: $(JSCHEME_MAIN) | check-jdk
	sbcl --dynamic-space-size $(DYNAMIC_SPACE_SIZE) --non-interactive --eval "(progn (push (uiop:getcwd) asdf:*central-registry*) (asdf:test-system \"openldk/jscheme\"))"

# Run openldk/LANGUAGE's tests in sbcl-LANGUAGE, where the language is
# started.  The image's systems are left as they are: reloading OpenLDK into
# an image whose Java classes are loaded fails, so tests run as built.
define check-in-image
./sbcl-$(1) --non-interactive --eval "(progn (push (uiop:getcwd) asdf:*central-registry*) (asdf:load-system \"openldk/$(1)/tests\" :force-not (asdf:already-loaded-systems)) (unless (uiop:symbol-call :fiveam (quote #:run!) :openldk-$(1)-tests) (uiop:quit 1)))"
endef

# Tests for openldk/clojure, run in sbcl-clojure, where Clojure is started.
check-clojure: sbcl-clojure
	$(call check-in-image,clojure)

# Tests for openldk/kawa, run in sbcl-kawa, where Kawa is started.
check-kawa: sbcl-kawa
	$(call check-in-image,kawa)

# Tests for openldk/scala, run in sbcl-scala, where Scala is started.
check-scala: sbcl-scala
	$(call check-in-image,scala)

# Tests for openldk/kotlin, run in sbcl-kotlin, where Kotlin is started.
check-kotlin: sbcl-kotlin
	$(call check-in-image,kotlin)

# Tests for openldk/groovy, run in sbcl-groovy, where Groovy is started.
check-groovy: sbcl-groovy
	$(call check-in-image,groovy)

# Tests for openldk/polyglot, run in sbcl-polyglot: every language at once,
# and a system of foreign source files as ASDF components (test/polyglot-demo).
check-polyglot: sbcl-polyglot
	$(call check-in-image,polyglot)

# Tests for openldk/abcl, run in sbcl-abcl, where ABCL is started.
check-abcl: sbcl-abcl
	$(call check-in-image,abcl)

# SBCLs with a language started, for using it from Lisp (see docs/INTEROP.md).
sbcl-jscheme: src/*.lisp src/native/*.lisp interop/*.lisp *.asd Makefile $(JSCHEME_MAIN) | check-jdk
	sbcl --dynamic-space-size $(DYNAMIC_SPACE_SIZE) --disable-debugger --eval "(progn (push (uiop:getcwd) asdf:*central-registry*) (asdf:load-system \"openldk/jscheme\"))" --eval "(openldk.interop:dump-lisp-image \"sbcl-jscheme\" :init (lambda () (jscheme:startup) (jscheme:eval-string \"(define (f n) (if (< n 2) 1 (* n (f (- n 1))))) (f 10) (map (lambda (x) (* x x)) (list 1 2 3))\")))"

sbcl-kawa: src/*.lisp src/native/*.lisp interop/*.lisp *.asd Makefile $(KAWA_JAR) | check-jdk
	sbcl --dynamic-space-size $(DYNAMIC_SPACE_SIZE) --disable-debugger --eval "(progn (push (uiop:getcwd) asdf:*central-registry*) (asdf:load-system \"openldk/kawa\"))" --eval "(openldk.interop:dump-lisp-image \"sbcl-kawa\" :init (lambda () (kawa:startup) (kawa:eval-string \"(define (f n) (if (< n 2) 1 (* n (f (- n 1))))) (f 20) (map (lambda (x) (* x x)) (list 1 2 3)) (vector 1 2)\")))"

# Every language started, in one SBCL (openldk/polyglot).
sbcl-polyglot: src/*.lisp src/native/*.lisp interop/*.lisp *.asd Makefile $(KAWA_JAR) $(CLOJURE_JARS) $(JSCHEME_MAIN) $(SCALA_JARS) $(KOTLIN_JARS) $(GROOVY_JAR) $(ABCL_JAR) | check-jdk
	sbcl --dynamic-space-size $(DYNAMIC_SPACE_SIZE) --disable-debugger --eval "(progn (push (uiop:getcwd) asdf:*central-registry*) (asdf:load-system \"openldk/polyglot\"))" --eval "(openldk.interop:dump-lisp-image \"sbcl-polyglot\" :init (lambda () (polyglot:warm-up (polyglot:startup))))"

sbcl-abcl: src/*.lisp src/native/*.lisp interop/*.lisp *.asd Makefile $(ABCL_JAR) | check-jdk
	sbcl --dynamic-space-size $(DYNAMIC_SPACE_SIZE) --disable-debugger --eval "(progn (push (uiop:getcwd) asdf:*central-registry*) (asdf:load-system \"openldk/abcl\"))" --eval "(openldk.interop:dump-lisp-image \"sbcl-abcl\" :init (lambda () (abcl:startup) (abcl:eval-string \"(defun f (n) (if (< n 2) 1 (* n (f (1- n))))) (f 20) (mapcar (lambda (x) (* x x)) (list 1 2 3)) (prin1-to-string (list 1 2.5 (quote sym))) (handler-case (error (quote type-error) :datum 1 :expected-type (quote string)) (error (e) (princ-to-string e))) (defclass pt () ((x :initarg :x))) (make-instance (quote pt) :x 1) (compile (quote f))\")))"

sbcl-clojure: src/*.lisp src/native/*.lisp interop/*.lisp *.asd Makefile $(CLOJURE_JARS) | check-jdk
	sbcl --dynamic-space-size $(DYNAMIC_SPACE_SIZE) --disable-debugger --eval "(progn (push (uiop:getcwd) asdf:*central-registry*) (asdf:load-system \"openldk/clojure\"))" --eval "(openldk.interop:dump-lisp-image \"sbcl-clojure\" :init (lambda () (clojure:startup) (clojure:eval-string \"(defn f [n] (if (< n 2) 1 (*' n (f (dec n))))) (f 20) (mapv inc (range 5)) (into {} [[:a 1]]) (str 1 :k)\")))"

sbcl-scala: src/*.lisp src/native/*.lisp interop/*.lisp *.asd Makefile $(SCALA_JARS) | check-jdk
	sbcl --dynamic-space-size $(DYNAMIC_SPACE_SIZE) --disable-debugger --eval "(progn (push (uiop:getcwd) asdf:*central-registry*) (asdf:load-system \"openldk/scala\"))" --eval "(openldk.interop:dump-lisp-image \"sbcl-scala\" :init (lambda () (scala:startup) (scala:eval \"def f(n: Int): BigInt = if (n < 2) 1 else n * f(n - 1); f(20); List(1, 2, 3).map(_ * 2).sum; Vector(1, 2); Map(1 -> 2); Some((1, 2))\")))"

sbcl-kotlin: src/*.lisp src/native/*.lisp interop/*.lisp *.asd Makefile $(KOTLIN_JARS) | check-jdk
	sbcl --dynamic-space-size $(DYNAMIC_SPACE_SIZE) --disable-debugger --eval "(progn (push (uiop:getcwd) asdf:*central-registry*) (asdf:load-system \"openldk/kotlin\"))" --eval "(openldk.interop:dump-lisp-image \"sbcl-kotlin\" :init (lambda () (kotlin:startup) (kotlin:eval \"listOf(1, 2, 3).map { it * 2 }.sum()\") (kotlin:eval \"mapOf(1 to \\\"a\\\").entries.joinToString()\")))"

sbcl-groovy: src/*.lisp src/native/*.lisp interop/*.lisp *.asd Makefile $(GROOVY_JAR) | check-jdk
	sbcl --dynamic-space-size $(DYNAMIC_SPACE_SIZE) --disable-debugger --eval "(progn (push (uiop:getcwd) asdf:*central-registry*) (asdf:load-system \"openldk/groovy\"))" --eval "(openldk.interop:dump-lisp-image \"sbcl-groovy\" :init (lambda () (groovy:startup) (groovy:eval \"def f(n) { n < 2 ? 1G : n * f(n - 1) }; f(20); [3, 1, 2].sort().collect { it * 2 }; [a: 1].collectEntries { k, v -> [k, v + 1] }\")))"

# A warm image: JScheme's Java code is compiled at build time (see docs/IMAGES.md).
jscheme: src/*.lisp src/native/*.lisp *.asd Makefile lib/jscheme/build-image.lisp $(JSCHEME_MAIN) lib/jscheme/r4rstest.scm | check-jdk
	sbcl --dynamic-space-size $(DYNAMIC_SPACE_SIZE) --disable-debugger --eval "(progn (push (uiop:getcwd) asdf:*central-registry*) (asdf:load-system :openldk))" --load lib/jscheme/build-image.lisp

check: openldk testsuite/mauve/gnu/testlet/config.class
	(cd testsuite; runtest --tool openldk $(RUNTESTFLAGS))

# Fast regression gate: runs the core suites (aaa/df/gcj/jikestst) and compares
# against testsuite/baseline.sum. Fails on any dropped pass or new failure.
# Used by CI. Excludes the slow mauve suite (use `make check` for that).
check-regression: openldk
	testsuite/check-regression.sh

# Spring Boot startup on HotSpot, as a GraalVM native image and from OpenLDK
# images, timed side by side.  Downloads Maven and GraalVM into .cache/bench
# when they are missing.  See bench/spring-boot/README.md.
bench-spring-boot: openldk
	bench/spring-boot/run.sh

# The same for Quarkus.  See bench/quarkus/README.md.
bench-quarkus: openldk
	bench/quarkus/run.sh

SRCDIR := $(shell pwd)/testsuite/mauve
BUILDDIR := $(shell pwd)

testsuite/mauve/gnu/testlet/config.class: testsuite/mauve/gnu/testlet/config.java.in Makefile
	sed -e 's|@SRCDIR@|$(SRCDIR)|g' \
	    -e 's|@BUILDDIR@|$(BUILDDIR)|g' \
	    -e 's|@TMPDIR@|/tmp|g' \
	    < testsuite/mauve/gnu/testlet/config.java.in > testsuite/mauve/gnu/testlet/config.java
	(cd testsuite/mauve; $(JAVAC) gnu/testlet/config.java)

clean:
	-rm -rf openldk .*~ *~

# Exercise weak references created before SAVE-LISP-AND-DIE, including restored
# locks, the reference delivery service, and collection of core-backed objects.
.PHONY: check-verifier
check-verifier: openldk | check-jdk
	python3 test/verifier/check.py

.PHONY: check-jni check-jni-zstd
.PHONY: check-native-access
check-native-access: openldk jni | check-jdk
	python3 test/native-access/check.py

check-jni: openldk jni | check-jdk
	python3 test/jni/check.py

check-jni-zstd: openldk jni | check-jdk
	python3 test/jni/check.py --zstd

.PHONY: check-zip check-nio check-modules check-reentrant check-launcher check-eclipse check-foreign check-concurrency check-source-launcher
check-concurrency: openldk | check-jdk
	python3 test/concurrency/check.py

check-source-launcher: openldk | check-jdk
	python3 test/source-launcher/check.py

.PHONY: check-remainder bench-remainder
check-remainder: openldk | check-jdk
	python3 test/remainder/check.py

bench-remainder:
	sbcl --dynamic-space-size 8192 --non-interactive --load bench/remainder.lisp

.PHONY: check-language check-modern check-compat
check-compat: openldk | check-jdk
	python3 test/compat/check.py

.PHONY: check-standards check-loader-constraints
check-standards: openldk | check-jdk
	python3 test/standards/check.py

check-loader-constraints: openldk | check-jdk
	python3 test/standards/check.py --probe LoaderConstraintProbe

check-modern: openldk | check-jdk
	python3 test/modern/check.py

check-language: openldk | check-jdk
	python3 test/language/check.py

check-foreign: openldk jni | check-jdk
	python3 test/foreign/check.py

check-zip: openldk | check-jdk
	python3 test/zip/check.py

check-nio: openldk | check-jdk
	python3 test/nio/check.py

check-modules: openldk | check-jdk
	python3 test/modules/check.py

check-reentrant: openldk | check-jdk
	python3 test/reentrant/check.py

check-launcher: openldk | check-jdk
	python3 test/launcher/check.py

check-eclipse: openldk jni | check-jdk
	python3 test/eclipse/check.py

.PHONY: check-thread-image
check-thread-image: | check-jdk
	@image_test_dir=$$(mktemp -d); \
	trap 'rm -rf "$$image_test_dir"' EXIT; \
	$(JAVAC) --release 25 -g -d "$$image_test_dir" test/ThreadImage.java test/standards/MainThreadProbe.java && \
	LDK_THREAD_IMAGE_DIR="$$image_test_dir" \
	  sbcl --dynamic-space-size $(DYNAMIC_SPACE_SIZE) --disable-debugger \
	  --eval '(progn (push (uiop:getcwd) asdf:*central-registry*) (asdf:load-system :openldk))' \
	  --load test/thread-image.lisp && \
	"$$image_test_dir/thread-image"

.PHONY: check-reference-image
check-reference-image: | check-jdk
	@image_test_dir=$$(mktemp -d); \
	trap 'rm -rf "$$image_test_dir"' EXIT; \
	$(JAVAC) -d "$$image_test_dir" test/ReferenceImage.java && \
	LDK_REFERENCE_IMAGE_DIR="$$image_test_dir" \
	  sbcl --dynamic-space-size $(DYNAMIC_SPACE_SIZE) --disable-debugger \
	  --eval '(progn (push (uiop:getcwd) asdf:*central-registry*) (asdf:load-system :openldk))' \
	  --load test/reference-image.lisp && \
	"$$image_test_dir/reference-image"

# Preserve font scalers and native glyph caches across a saved-image restart.
.PHONY: check-font-image
check-font-image: | check-jdk
	@font_image_dir=$$(mktemp -d); \
	trap 'rm -rf "$$font_image_dir"' EXIT; \
	$(JAVAC) -d "$$font_image_dir" test/FontImage.java && \
	LDK_FONT_IMAGE_DIR="$$font_image_dir" \
	  sbcl --dynamic-space-size $(DYNAMIC_SPACE_SIZE) --disable-debugger \
	  --eval '(progn (push (uiop:getcwd) asdf:*central-registry*) (asdf:load-system :openldk))' \
	  --load test/font-image.lisp && \
	"$$font_image_dir/font-image"
