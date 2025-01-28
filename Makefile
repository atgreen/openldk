# Consider running with --dynamic-space-size 2560

# Set LDK_CLASSPATH to /usr/lib/jvm/java-1.8.0-openjdk-1.8.0.392.b08-4.fc39.x86_64/jre/lib/rt.jar or similar

openldk: src/*.lisp *.asd Makefile
	sbcl --dynamic-space-size 32768 --eval "(progn (push (uiop:getcwd) asdf:*central-registry*) (asdf:make :openldk) (sb-ext:quit))"

check: openldk
	(cd testsuite; runtest --tool openldk $(RUNTESTFLAGS))

clean:
	-rm -rf openldk .*~ *~ systems
