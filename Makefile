# Set JAVA_HOME to /usr/lib/jvm/java-1.8.0-openjdk-1.8.0.392.b08-4.fc39.x86_64/jre or similar

ifndef JAVA_HOME
  $(error The JAVA_HOME environment variable must be set)
endif

openldk: src/*.lisp *.asd Makefile
	sbcl --dynamic-space-size 32768 --disable-debugger --eval "(progn (push (uiop:getcwd) asdf:*central-registry*) (asdf:load-system :openldk))" --eval "(openldk::make-image)"

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
	-rm -rf openldk .*~ *~ systems
