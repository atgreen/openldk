Unhandled OPENLDK::|condition-java/lang/AssertionError| in thread #<SB-THREAD:THREAD tid=56053 "main thread" RUNNING
                                                                     {1005E80003}>:
  Condition OPENLDK::|condition-java/lang/AssertionError| was signalled.

Backtrace for: #<SB-THREAD:THREAD tid=56053 "main thread" RUNNING {1005E80003}>
0: (SB-DEBUG::DEBUGGER-DISABLED-HOOK #<OPENLDK::|condition-java/lang/AssertionError| {1068A46C63}> #<unused argument> :QUIT T)
1: (SB-DEBUG::RUN-HOOK *INVOKE-DEBUGGER-HOOK* #<OPENLDK::|condition-java/lang/AssertionError| {1068A46C63}>)
2: (INVOKE-DEBUGGER #<OPENLDK::|condition-java/lang/AssertionError| {1068A46C63}>)
3: (ERROR #<OPENLDK::|condition-java/lang/AssertionError| {1068A46C63}>)
4: ((:METHOD OPENLDK::|println(F)| (OPENLDK::|java/io/PrintStream| T)) #<OPENLDK::|java/io/PrintStream| {100167D223}> -2.0) [fast-method]
5: ((:METHOD OPENLDK::|println(F)| (OPENLDK::|java/io/PrintStream| T)) #<OPENLDK::|java/io/PrintStream| {100167D223}> -2.0) [fast-method]
6: (OPENLDK::|Test.i2f()|)
7: (OPENLDK::|Test.i2f()|)
8: (OPENLDK::|Test.main([Ljava/lang/String;)| #<unused argument>)
9: (OPENLDK::|Test.main([Ljava/lang/String;)| #())
10: (SB-INT:SIMPLE-EVAL-IN-LEXENV (OPENLDK::|Test.main([Ljava/lang/String;)| #()) #<NULL-LEXENV>)
11: (EVAL (OPENLDK::|Test.main([Ljava/lang/String;)| #()))
12: (OPENLDK::%EVAL (OPENLDK::|Test.main([Ljava/lang/String;)| #()))
13: (OPENLDK::MAIN "Test" NIL :DUMP-DIR NIL :CLASSPATH NIL)
14: (OPENLDK::MAIN-COMMAND "openldk" NIL)
15: (OPENLDK::MAIN-COMMAND "openldk") [optional]
16: (OPENLDK:MAIN-WRAPPER)
17: ((FLET SB-UNIX::BODY :IN SB-IMPL::START-LISP))
18: ((FLET "WITHOUT-INTERRUPTS-BODY-3" :IN SB-IMPL::START-LISP))
19: (SB-IMPL::%START-LISP)

unhandled condition in --disable-debugger mode, quitting

want: 127
 got: 127

want: -1
 got: -1

want: -1
 got: -1

want: 127
 got: 127

want: 255
 got: 255

want: 65535
 got: 65535

want: 32767
 got: 32767

want: -1
 got: -1

want: 2147483647
 got: 2147483647

want: -1
 got: -1

want: 2147483647
 got: 2147483647

want: -1
 got: -1

want: -2
 got: 