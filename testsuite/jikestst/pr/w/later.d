# $Id: later.d,v 1.4 1999/11/04 15:01:21 shields Exp $
#
# This software is subject to the terms of the IBM Jikes Compiler
# License Agreement available at the following URL:
# http://www.ibm.com/research/jikes.
# Copyright (C) 1996, 1999, International Business Machines Corporation
# and others.  All Rights Reserved.
# You must accept the terms of that agreement to use this software.
#
# tests not runnable under current scheme
# pr109 must be run in special way
g pr109a . a
g pr109b . b
g pr109c . c
# 110 involves bad line numbers and -g
# 129 need 1.2 beta2 to run
e pr129 X
e pr137 tclone


# 139
