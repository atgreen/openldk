JAVA_HOME ?= /home/linuxbrew/.linuxbrew/opt/openjdk@21/libexec

XDG_CACHE_HOME ?= $(CURDIR)/.cache

export JAVA_HOME XDG_CACHE_HOME

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

SRCDIR := $(shell pwd)/testsuite/mauve
BUILDDIR := $(shell pwd)

testsuite/mauve/gnu/testlet/config.class: testsuite/mauve/gnu/testlet/config.java.in Makefile
	sed -e 's|@SRCDIR@|$(SRCDIR)|g' \
	    -e 's|@BUILDDIR@|$(BUILDDIR)|g' \
	    -e 's|@TMPDIR@|/tmp)|g' \
	    < testsuite/mauve/gnu/testlet/config.java.in > testsuite/mauve/gnu/testlet/config.java
	(cd testsuite/mauve; javac gnu/testlet/config.java)

clean:
	-rm -rf openldk .*~ *~
