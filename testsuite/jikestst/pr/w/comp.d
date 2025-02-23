# $Id: comp.d,v 1.4 1999/11/04 15:01:21 shields Exp $
# This software is subject to the terms of the IBM Jikes Compiler
# License Agreement available at the following URL:
# http://www.ibm.com/research/jikes.
# Copyright (C) 1996, 1999, International Business Machines Corporation
# and others.  All Rights Reserved.
# You must accept the terms of that agreement to use this software.
#
#Q
m pr100
g pr100a . Whoops
g pr100b . Derived
f . Whoops
f . Derived
c pr104  . C1
c pr111  ChildEnumeration ChildEnumeration
c pr119  Myserver
m pr121
g pr121a . SubClass
g pr121b . SuperClass
f . SubClass
f . SuperClass
m pr122
g pr122a . x
g pr122b .  y
g pr122c . z
f . x
f . y
f . z
c pr123 Super
m pr124 test
g pr124 test Crash
f test Crash
m pr125 jalama
g pr125 jalama Thing
f jalama Thing
m pr143 com com/compname
g pr143 com/compname Foo
f com/compname Foo
c pr145 Bug
c pr152 Test1
c pr153 t5
c pr155 crash
c pr156 bug156
c pr159 thr1
c pr164 prog
c pr165 OuterClass
c pr166 T
c pr168 afinal
m pr169
g pr169a . se1
g pr169b . se2
f . se1
f . se2
c pr175 float1
c pr177 instanceof1
c pr178 Locals
c pr183 switcheroo
m pr184
g pr184a . set1
g pr184b . set2
f . set1
f . set2
c pr185 foo
c pr187 final1
c pr188 final2
m pr191
g pr191a . A
g pr191b . B
g pr191c . TestShadow
f . A
f . B
f . TestShadow
c pr192 JikesTest
c pr193 Tx
m pr197 jvs jvs.one jvs.two
g pr197a jvs.one P1
g pr197b jvs.two P2
f jvs.one P1
f jvs.two P2
c pr198 JTest
r foo
c pr199a foo
c pr199b Test
c pr199c Test
c pr199d Test
c pr199e Test
c pr199f Test
c pr199g Test
c pr199h Test
c pr199i Test
c pr199j InterFaceCast
c pr199k Test
r one two
m pr199n one two
g pr199na one Parent
g pr199nb two Child
f one Parent
f two Child
c pr199o Test
c pr199p Test
# pr199 tests require java subdirectory
# must erase it after running these tests
r java
m pr199qa java java/lang java/lang/String
g pr199qa java/lang/String Illegal
f java/lang/String Illegal
m pr199qb java
g pr199qb java util
f java util
r java
c pr203 testclass
c pr206 myMethod
m pr207 Jikes36
g pr207a Jikes36 S
g pr207b Jikes36 T
g pr207c Jikes36 VT
g pr207d Jikes36 V
g pr207e Jikes36 PT
f Jikes36 S
f Jikes36 T
f Jikes36 VT
f Jikes36 V
f Jikes36 PT
c pr209 aa2
c pr210 p149
c pr217 JikesBug
c pr218a test
c pr218b test
c pr219 Test
c pr222 Test
m pr230
g pr230a . d1
g pr230b . superclass
g pr230c . und
g pr230d . thistwo
f . d1
f . superclass
f . und
f . thistwo
c pr231 CoreDump
c pr241 Test
c pr243 cont1c
m pr246 foo bar 
g pr246a foo Super
g pr246b . Sub
f foo Super
f . Sub
c pr249 Ez1Dict
m pr253 aaa bbb
g pr253a aaa A
g pr253b bbb B
f aaa A
f bbb B
c pr254 Test
c pr256 aw1
c pr261 Test
m pr265 Y
g pr265b Y Z
g pr265a . X
f Y Z
f . X
c pr267 DATest
c pr272 Bug
c pr273 Test
c pr277 Jikes
c pr278 DATest
c pr281 Foo
c pr282 JikesBug
c pr283 T1
m pr285 one two
g pr285a one A
g pr285b two C
f one A
f two C
