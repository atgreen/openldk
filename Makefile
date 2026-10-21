JAVA_HOME ?= /usr/lib/jvm/java-25-openjdk

# Compiler used to build the testsuite's Java sources. Decoupled from JAVA_HOME
# because a headless JDK ships no javac; a newer javac targeting release 25
# produces class-file version 69, which OpenLDK accepts.
JAVAC ?= javac --release 25

XDG_CACHE_HOME ?= $(CURDIR)/.cache

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

ABCL_VERSION = 1.9.2
ABCL_JAR = lib/abcl/abcl.jar
ABCL_URL = https://abcl.org/releases/$(ABCL_VERSION)/abcl-bin-$(ABCL_VERSION).tar.gz
ABCL_MAIN = lib/abcl/org/armedbear/lisp/ABCLMain.class

all: openldk javacl

# Fail fast when JAVA_HOME is not a JDK 25 (e.g. the environment exports an
# older JDK) instead of erroring deep inside the Lisp build.  Order-only
# prerequisite so it never makes the image targets out of date.
.PHONY: check-jdk
check-jdk:
	@test -n "$(JAVA_HOME)" || { echo "error: JAVA_HOME is not set; OpenLDK requires a JDK 25 (e.g. /usr/lib/jvm/java-25-openjdk)"; exit 1; }
	@grep -qs '^JAVA_VERSION="25' "$(JAVA_HOME)/release" || { echo "error: JAVA_HOME=$(JAVA_HOME) is not a JDK 25 installation (OpenLDK requires JDK 25)"; exit 1; }

openldk: src/*.lisp src/native/*.lisp *.asd Makefile | check-jdk
	sbcl --dynamic-space-size 32768 --disable-debugger --eval "(progn (push (uiop:getcwd) asdf:*central-registry*) (asdf:load-system :openldk))" --eval "(openldk::make-image)"

javacl: src/*.lisp src/native/*.lisp *.asd Makefile | check-jdk
	sbcl --dynamic-space-size 32768 --disable-debugger --eval "(progn (push (uiop:getcwd) asdf:*central-registry*) (asdf:load-system \"javacl\"))" --eval "(openldk::make-javac-image)"

$(KAWA_JAR):
	mkdir -p lib
	curl -L -o $(KAWA_JAR) $(KAWA_URL)

# A warm image: Kawa's Java code is compiled at build time (see docs/IMAGES.md).
kawa: src/*.lisp src/native/*.lisp *.asd Makefile lib/kawa/build-image.lisp $(KAWA_JAR) | check-jdk
	sbcl --dynamic-space-size 32768 --disable-debugger --eval "(progn (push (uiop:getcwd) asdf:*central-registry*) (asdf:load-system :openldk))" --load lib/kawa/build-image.lisp

$(ABCL_JAR):
	curl -sSfL $(ABCL_URL) | tar xzO abcl-bin-$(ABCL_VERSION)/abcl.jar > $@.tmp
	mv $@.tmp $@

# javac through the JDK's own module, so a JDK without bin/javac works too.
$(ABCL_MAIN): lib/abcl/ABCLMain.java $(ABCL_JAR)
	$(JAVA_HOME)/bin/java -m jdk.compiler/com.sun.tools.javac.Main -cp $(ABCL_JAR) -d lib/abcl lib/abcl/ABCLMain.java

# A warm image: ABCL's Java code is compiled at build time (see docs/IMAGES.md).
abcl: src/*.lisp src/native/*.lisp *.asd Makefile lib/abcl/build-image.lisp $(ABCL_MAIN) | check-jdk
	sbcl --dynamic-space-size 32768 --disable-debugger --eval "(progn (push (uiop:getcwd) asdf:*central-registry*) (asdf:load-system :openldk))" --load lib/abcl/build-image.lisp

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
	sbcl --dynamic-space-size 32768 --disable-debugger --eval "(progn (push (uiop:getcwd) asdf:*central-registry*) (asdf:load-system :openldk))" --load lib/clojure/build-image.lisp

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

# Tests for openldk/jscheme, using JScheme from Lisp (see docs/INTEROP.md).
check-jscheme: $(JSCHEME_MAIN) | check-jdk
	sbcl --dynamic-space-size 32768 --non-interactive --eval "(progn (push (uiop:getcwd) asdf:*central-registry*) (asdf:test-system \"openldk/jscheme\"))"

# Tests for openldk/clojure, run in sbcl-clojure, where Clojure is started.
check-clojure: sbcl-clojure
	./sbcl-clojure --non-interactive --eval "(progn (push (uiop:getcwd) asdf:*central-registry*) (asdf:test-system \"openldk/clojure\"))"

# SBCLs with a language started, for using it from Lisp (see docs/INTEROP.md).
sbcl-jscheme: src/*.lisp src/native/*.lisp interop/*.lisp *.asd Makefile $(JSCHEME_MAIN) | check-jdk
	sbcl --dynamic-space-size 32768 --disable-debugger --eval "(progn (push (uiop:getcwd) asdf:*central-registry*) (asdf:load-system \"openldk/jscheme\"))" --eval "(openldk.interop:dump-lisp-image \"sbcl-jscheme\" :init (lambda () (jscheme:startup) (jscheme:eval-string \"(define (f n) (if (< n 2) 1 (* n (f (- n 1))))) (f 10) (map (lambda (x) (* x x)) (list 1 2 3))\")))"

sbcl-clojure: src/*.lisp src/native/*.lisp interop/*.lisp *.asd Makefile $(CLOJURE_JARS) | check-jdk
	sbcl --dynamic-space-size 32768 --disable-debugger --eval "(progn (push (uiop:getcwd) asdf:*central-registry*) (asdf:load-system \"openldk/clojure\"))" --eval "(openldk.interop:dump-lisp-image \"sbcl-clojure\" :init (lambda () (clojure:startup) (clojure:eval-string \"(defn f [n] (if (< n 2) 1 (*' n (f (dec n))))) (f 20) (mapv inc (range 5)) (into {} [[:a 1]]) (str 1 :k)\")))"

# A warm image: JScheme's Java code is compiled at build time (see docs/IMAGES.md).
jscheme: src/*.lisp src/native/*.lisp *.asd Makefile lib/jscheme/build-image.lisp $(JSCHEME_MAIN) lib/jscheme/r4rstest.scm | check-jdk
	sbcl --dynamic-space-size 32768 --disable-debugger --eval "(progn (push (uiop:getcwd) asdf:*central-registry*) (asdf:load-system :openldk))" --load lib/jscheme/build-image.lisp

check: openldk testsuite/mauve/gnu/testlet/config.class
	(cd testsuite; runtest --tool openldk $(RUNTESTFLAGS))

# Fast regression gate: runs the core suites (aaa/df/gcj/jikestst) and compares
# against testsuite/baseline.sum. Fails on any dropped pass or new failure.
# Used by CI. Excludes the slow mauve suite (use `make check` for that).
check-regression: openldk
	testsuite/check-regression.sh

SRCDIR := $(shell pwd)/testsuite/mauve
BUILDDIR := $(shell pwd)

testsuite/mauve/gnu/testlet/config.class: testsuite/mauve/gnu/testlet/config.java.in Makefile
	sed -e 's|@SRCDIR@|$(SRCDIR)|g' \
	    -e 's|@BUILDDIR@|$(BUILDDIR)|g' \
	    -e 's|@TMPDIR@|/tmp)|g' \
	    < testsuite/mauve/gnu/testlet/config.java.in > testsuite/mauve/gnu/testlet/config.java
	(cd testsuite/mauve; $(JAVAC) gnu/testlet/config.java)

clean:
	-rm -rf openldk .*~ *~
