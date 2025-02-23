# $Id: exec.d,v 1.4 1999/11/04 15:01:21 shields Exp $
# This software is subject to the terms of the IBM Jikes Compiler
# License Agreement available at the following URL:
# http://www.ibm.com/research/jikes.
# Copyright (C) 1996, 1999, International Business Machines Corporation
# and others.  All Rights Reserved.
# You must accept the terms of that agreement to use this software.
#
#Q
e pr102 TestChaos
e pr103 TestSer
m pr114 abc abc/def
g pr114a abc Def
g pr114b abc/def Test
f abc Def
f abc/def Test
x abc.def.Test
m pr120
g pr120a . A
g pr120b . B
g pr120c . C
g pr120d . D
f . A
f . B
f . C
f . D
x D
e pr128 GaspodeParameters
e pr130 core
e pr131 t
r test/subpackage test
# 132 yields java vm verify error
m pr132 test test/subpackage
g pr132b test/subpackage C
g pr132a test B
f test/subpackage C
f test B
x test.Protect
e pr133 neil1
e pr136 Test
e pr138 Main
#m pr138 cg10
#g pr138 cg10 Main
#f cg10 Main
#x cg10.Main
e pr141 ThrowTest
#m pr141
#g pr141a . T1
#g pr141b . T2
#g pr141c . ThrowTest
#f . T1
#f . T2
#f . ThrowTest
#x ThrowTest
r test
e pr142 test
e pr146 Test
e pr147 Test
e pr160 jikes34bug
e pr163 P
e pr167 sub
e pr172 Test
e pr176 Test
#e pr173 super22
#e pr174 Jitbug
e pr186 Test
e pr189 B
m pr190 x
g pr190a x A
g pr190b x X
f x A
f x X
x x.A
e pr195 Test
e pr196 Test
#m pr195
#g pr195a . Abstract
#g pr195b . Test
#f . Abstract
#f . Test
#x Test
e pr199la InitTest
e pr199lb InitTest2
r pack1
m pr199m pack1
g pr199mb pack1 P1
g pr199ma . CMain
f  pack1 P1
f . Cmain
x CMain
e pr200 Test
e pr215 External
e pr226 TestArrayClass
e pr228 Test
e pr229 Test
m pr236
g pr236a . SuperC
g pr236b . SubC
f . SuperC
f . SubC
x SubC
e pr247 boom
# 252 is "Big array problem"
#e pr252 
e pr257 JikesExceptions
e pr259 PrintTest
#ok above here
m pr275
g pr275a . Main
g pr275b . Main2
f . Main2
f . Main
x Main
e pr286 BlankFinals
e pr287 B##later.d##
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
