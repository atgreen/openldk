Unhandled SB-SYS:INTERACTIVE-INTERRUPT in thread #<SB-THREAD:THREAD tid=56603 "main thread" RUNNING
                                                    {1005E80093}>:
  Interactive interrupt at #xB8017E3AC2.

Backtrace for: #<SB-THREAD:THREAD tid=56603 "main thread" RUNNING {1005E80093}>
0: ((LAMBDA (SB-PCL::.ARG0.) :IN "SYS:SRC;PCL;BRAID.LISP") #<OPENLDK::<CONTEXT> {101333EF83}>)
1: (:WIDE #<OPENLDK::<CONTEXT> {101333EF83}> #<unused argument>)
2: (OPENLDK::%COMPILE-METHOD "sun/misc/FloatingDecimal" 10)
3: (OPENLDK::|sun/misc/FloatingDecimal.getBinaryToASCIIConverter(DZ)| 8.0d0 1)
4: (OPENLDK::|sun/misc/FloatingDecimal.getBinaryToASCIIConverter(D)| 8.0d0)
5: (OPENLDK::|sun/misc/FloatingDecimal.getBinaryToASCIIConverter(D)| 8.0d0)
6: (OPENLDK::|sun/misc/FloatingDecimal.toJavaFormatString(D)| 8.0d0)
7: (OPENLDK::|sun/misc/FloatingDecimal.toJavaFormatString(D)| 8.0d0)
8: (OPENLDK::|java/lang/Double.toString(D)| 8.0d0)
9: (OPENLDK::|java/lang/Double.toString(D)| 8.0d0)
10: (OPENLDK::|java/lang/String.valueOf(D)| 8.0d0)
11: (OPENLDK::|java/lang/String.valueOf(D)| 8.0d0)
12: ((:METHOD OPENLDK::|print(D)| (OPENLDK::|java/io/PrintStream| T)) #<OPENLDK::|java/io/PrintStream| {100167D223}> 8.0d0) [fast-method]
13: ((:METHOD OPENLDK::|print(D)| (OPENLDK::|java/io/PrintStream| T)) #<OPENLDK::|java/io/PrintStream| {100167D223}> 8.0d0) [fast-method]
14: ((:METHOD OPENLDK::|println(D)| (OPENLDK::|java/io/PrintStream| T)) #<OPENLDK::|java/io/PrintStream| {100167D223}> 8.0d0) [fast-method]
15: ((:METHOD OPENLDK::|println(D)| (OPENLDK::|java/io/PrintStream| T)) #<OPENLDK::|java/io/PrintStream| {100167D223}> 8.0d0) [fast-method]
16: (OPENLDK::|Test.main([Ljava/lang/String;)| #<unused argument>)
17: (OPENLDK::|Test.main([Ljava/lang/String;)| #())
18: (SB-INT:SIMPLE-EVAL-IN-LEXENV (OPENLDK::|Test.main([Ljava/lang/String;)| #()) #<NULL-LEXENV>)
19: (EVAL (OPENLDK::|Test.main([Ljava/lang/String;)| #()))
20: (OPENLDK::%EVAL (OPENLDK::|Test.main([Ljava/lang/String;)| #()))
21: (OPENLDK::MAIN "Test" NIL :DUMP-DIR NIL :CLASSPATH NIL)
22: (OPENLDK::MAIN-COMMAND "openldk" NIL)
23: (OPENLDK::MAIN-COMMAND "openldk") [optional]
24: (OPENLDK:MAIN-WRAPPER)
25: ((FLET SB-UNIX::BODY :IN SB-IMPL::START-LISP))
26: ((FLET "WITHOUT-INTERRUPTS-BODY-3" :IN SB-IMPL::START-LISP))
27: (SB-IMPL::%START-LISP)

unhandled condition in --disable-debugger mode, quitting

want: true
 got: true

want: 2
 got: 2

want: A
 got: A

want: 4
 got: 4

want: 5
 got: 5

want: 6
 got: 6

want: 7
 got: 7.0

want: 8
 got: 