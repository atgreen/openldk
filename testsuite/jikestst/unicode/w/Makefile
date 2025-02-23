# $Id: Makefile,v 1.3 2000/02/04 07:35:33 lord Exp $
#
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
	../../bin/jt exec
	mv *.e this.exec
chk:	
	rm -f exec.dif
	diff ../ok/exec this.exec >exec.dif
clean:
	rm -fr this.exec
	rm -f *.java
	rm -f *.class
	rm -f *.[cer]
	rm -f *.dif
	rm -f core

