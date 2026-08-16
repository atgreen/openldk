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

all: openldk javacl

openldk: src/*.lisp *.asd Makefile
	sbcl --dynamic-space-size 32768 --disable-debugger --eval "(progn (push (uiop:getcwd) asdf:*central-registry*) (asdf:load-system :openldk))" --eval "(openldk::make-image)"

javacl: src/*.lisp *.asd Makefile
	sbcl --dynamic-space-size 32768 --disable-debugger --eval "(progn (push (uiop:getcwd) asdf:*central-registry*) (asdf:load-system \"javacl\"))" --eval "(openldk::make-javac-image)"

$(KAWA_JAR):
	mkdir -p lib
	curl -L -o $(KAWA_JAR) $(KAWA_URL)

kawa: src/*.lisp *.asd Makefile $(KAWA_JAR)
	sbcl --dynamic-space-size 32768 --disable-debugger --eval "(progn (push (uiop:getcwd) asdf:*central-registry*) (asdf:load-system :openldk))" --eval "(openldk::dump-app-image \"kawa\" \"kawa.repl\" :classpath \"$(KAWA_JAR)\")"

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
