# This software is subject to the terms of the IBM Jikes Test Suite
# License Agreement available at the following URL:
# http://www.ibm.com/research/jikes.
# Copyright (C) 1996, 1999, International Business Machines Corporation
# and others.  All Rights Reserved.
# You must accept the terms of that agreement to use this software.
#
m AbsClassMain
g AbsClass0 . AbsClass0
g AbsClass1 . AbsClass1
g AbsClassMain . AbsClassMain
f . AbsClass0
f . AbsClass1
f . AbsClassMain
x AbsClassMain
e BigComp
e Const
e DTest 
# don't execute this, get problems with both sun and jp
# when execute java with -verify specified
m DgramTest
g Dgram1Thread . Dgram1Thread
g Dgram2Thread . Dgram2Thread
g DgramTest . DgramTest
f . DgramTest
e FISTest 
e FOSTest
e FTest
e LoopTest
m QuesStmt
g ques . ques
g QuesStmt . QuesStmt
f . ques
f . QuesStmt
x QuesStmt
e RAFTest 
e RefComp
e Try1
# don't run this until PC fixes final_varaible allocation
# e Try2
e Try3
e Trychk1
e Trychk2
e Trychk3
e Trychk6
e Trychk7
e Trychk8
m Tryexcept
g Tryexcept . Tryexcept
g Tryesub . Tryesub
f . Tryexcept
f . Tryesub
x Tryexcept
m array1
g Truck . Truck
g Vehicle . Vehicle
g array1 . array1
f . Vehicle
f . Truck
f . array1
e array2 array2
e array3 
m array4
g Truck . Truck
g Vehicle . Vehicle
g array4 . array4
f . Vehicle
f . Truck
f . array4
x array4
e array5
e arraymethod
m bigi
g initinter . initinter
g initintersuper . initintersuper
g bigi . bigi
f . initinter
f . initintersuper
f . bigi
x bigi
m callmm
g mmult . mmult
g callmm . callmm
f . mmult
f . callmm
x callmm
m checkarray
g Truck . Truck
g Pickup . Pickup
g Vehicle . Vehicle
g checkarray . checkarray
f . Vehicle
f . Truck
f . Pickup
f . checkarray
x checkarray
m checkcast1
g Truck . Truck
g Pickup . Pickup
g Vehicle . Vehicle
g checkcast1 . checkcast1
f . Vehicle
f . Truck
f . Pickup
f . checkcast1
x checkcast1
m checkcast2
g checkcast2 . checkcast2
g Vehicle . Vehicle
g Truck . Truck
g Pickup . Pickup
f . Vehicle
f . Truck
f . Pickup
f . checkcast2
x checkcast2
m checkcast6
g Cans . Cans
g Food . Food
g cost . cost
g order . order
g checkcast6 . checkcast6
f . Cans
f . Food
f . cost
f . order
f . checkcast6
x checkcast6
m checkcast7
g Bottle . Bottle
g foreign . foreign
g Cans . Cans
g Food . Food
g cost . cost
g order . order
g checkcast7 . checkcast7
f . Cans
f . Food
f . cost
f . order
f . Bottle
f . foreign
f . checkcast7
x checkcast7
m checkcastjp
g Truck . Truck
g Pickup . Pickup
g Vehicle . Vehicle
g checkcastjp . checkcastjp
f . Vehicle
f . Truck
f . Pickup
f . checkcastjp
x checkcastjp
m cinit
g Fruit . Fruit
g Basket . Basket
g cinit . cinit
f . Fruit
f . Basket
f . cinit
x cinit
m classname
g Cans . Cans
g cost . cost
g order . order
g classname . classname
f . Cans
f . cost
f . order
f . classname
x classname
m clinitrep
g initrep . initrep
g clinitrep . clinitrep
f . initrep
f . clinitrep
x clinitrep
c clientsock
e cmplx1
e cmplx2
e cnvi2b_1
e cnvi2b_2
e cnvi2c_1
e cnvi2c_2
e cnvi2l_1
e cnvi2l_2
e cnvi2s_1
e cnvi2s_2
e cnvl2i_1
e cont1
e cont2
e cost
m ctestinit
g Fruit . Fruit
g Basket . Basket
g cinit . cinit
g ctestinit . ctestinit
f . Fruit
f . Basket
f . cinit
f . ctestinit
x ctestinit
c dgram1 
c dgram2 
e float1
e for1
e for2
e lptry1
e lptry2
e implement
m instance
g Cans . Cans
g Food . Food
g cost . cost
g order . order
g instance . instance
f . Cans
f . Food
f . cost
f . order
f . instance
m instance
m instance1
g Truck . Truck
g Vehicle . Vehicle
g Pickup . Pickup
g instance1 . instance1
f . Vehicle
f . Truck
f . Pickup
f . instance1
x instance1
e multarg
m multmain
g multchild . multchild
g multparent . multparent
g multgrand . multgrand
g multmain . multmain
f . multchild
f . multparent
f . multgrand
f . multmain
x multmain
e recur recur
c serversock 
e shift
m simparray
g Simpa . Simpa
g simparray . simparray
f . Simpa
f . simparray
x simparray
m syncm1
g CNotFound . CnotFound
g Truck . Truck
g Vehicle . Vehicle
g syncm1 . syncm1
f . CnotFound
f . Vehicle
f . Truck
f . syncm1
x syncm1
m testtrains
g Train . Train
g Mfg . Mfg
g Vehicle . Vehicle
g testtrains . testtrains
f . Mfg
f . Vehicle
f . Train
f . testtrains
x testtrains
m truckarray
g Vehicle . Vehicle
g Truck . Truck
g truckarray . truckarray
f . Vehicle
f . Truck
f . truckarray
x truckarray
e while1
e while2
