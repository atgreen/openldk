# This software is subject to the terms of the IBM Jikes Compiler
# License Agreement available at the following URL:
# http://www.ibm.com/research/jikes.
# Copyright (C) 1996, 1999, International Business Machines Corporation
# and others.  All Rights Reserved.
# You must accept the terms of that agreement to use this software.
#
exec:
	rm -fr this.exec
	mkdir this.exec
	jt exec
	mv *.e this.exec
chk:
	diff ../ok/exec this.exec >exec.dif
clean:
	rm -fr this.exec
	rm -fr *.java
	rm -fr *.class
	rm -fr *.[cer]
	rm -fr *.dif

