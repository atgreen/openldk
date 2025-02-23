# $Id: exec.d,v 1.6 2000/03/14 00:17:50 lord Exp $
# This software is subject to the terms of the IBM Jikes Compiler
# License Agreement available at the following URL:
# http://www.ibm.com/research/jikes.
# Copyright (C) 1996, 1999, International Business Machines Corporation
# and others.  All Rights Reserved.
# You must accept the terms of that agreement to use this software.
#
#
a -encoding euc-jp
e euc_jp euc_jp
a -encoding gb2312
e gb2312 gb2312
a -encoding hebrew
e Hebrew Hebrew
a -encoding greek
e Greek Greek
a -encoding SJIS
e ShiftJisIdentifiers ShiftJisIdentifiers
e ShiftJisLiterals ShiftJisLiterals
e ShiftJisMethods ShiftJisMethods
a -encoding big5
e big5 big5
a -encoding utf-8
e utf8 utf8
a -encoding koi8-r
e test0 test0
e test1 test1
c test2 test2
a -encoding UnicodeLittle
c utf16le utf16le
c utf16lex utf16lex
a -encoding latin1
e umlaut umlaut
#
