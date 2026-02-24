JAVA_HOME ?= /home/linuxbrew/.linuxbrew/opt/openjdk@17/libexec

XDG_CACHE_HOME ?= $(CURDIR)/.cache
LDK_JDK_CLASSES ?= $(CURDIR)/jdk17-classes
JDK17_CLASSPATH ?= $(LDK_JDK_CLASSES)/classes

export JAVA_HOME LDK_JDK_CLASSES XDG_CACHE_HOME

KAWA_VERSION = 3.1.1
KAWA_JAR = lib/kawa-$(KAWA_VERSION).jar
KAWA_URL = https://repo1.maven.org/maven2/com/github/arvyy/kawa/$(KAWA_VERSION)/kawa-$(KAWA_VERSION).jar

all: openldk javacl

openldk: src/*.lisp *.asd Makefile
	sbcl --dynamic-space-size 32768 --disable-debugger --eval "(progn (push (uiop:getcwd) asdf:*central-registry*) (asdf:load-system :openldk))" --eval "(openldk::make-image)"

javacl: src/*.lisp *.asd Makefile
	CLASSPATH=$(JDK17_CLASSPATH) sbcl --dynamic-space-size 32768 --disable-debugger --eval "(progn (push (uiop:getcwd) asdf:*central-registry*) (asdf:load-system \"javacl\"))" --eval "(openldk::make-javac-image)"

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

jdk17-classes:
	@echo "Extracting JDK 17 class files from $(JAVA_HOME)/jmods/..."
	mkdir -p jdk17-classes
	@for jmod in $(JAVA_HOME)/jmods/*.jmod; do \
		modname=$$(basename "$$jmod" .jmod); \
		echo "  $$modname"; \
		jmod extract --dir jdk17-classes "$$jmod"; \
		mkdir -p "jdk17-classes/modules/$$modname"; \
		jmod extract --dir "jdk17-classes/modules/$$modname" "$$jmod"; \
		rm -rf "jdk17-classes/modules/$$modname/bin" \
		       "jdk17-classes/modules/$$modname/conf" \
		       "jdk17-classes/modules/$$modname/include" \
		       "jdk17-classes/modules/$$modname/legal" \
		       "jdk17-classes/modules/$$modname/lib" \
		       "jdk17-classes/modules/$$modname/man"; \
		if [ -d "jdk17-classes/modules/$$modname/classes" ]; then \
			mv jdk17-classes/modules/$$modname/classes/* "jdk17-classes/modules/$$modname/" 2>/dev/null; \
			rmdir "jdk17-classes/modules/$$modname/classes" 2>/dev/null; \
		fi; \
	done
	@echo "Done. Classes in jdk17-classes/classes/ and jdk17-classes/modules/"

clean:
	-rm -rf openldk .*~ *~
