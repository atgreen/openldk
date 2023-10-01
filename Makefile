# Consider running with --dynamic-space-size 2560

openldk: *.lisp *.asd Makefile
	sbcl --dynamic-space-size 2048 --eval "(progn (push (uiop:getcwd) asdf:*central-registry*) (asdf:make :openldk) (sb-ext:quit))"

clean:
	-rm -rf openldk .*~ *~ systems systems.csv
