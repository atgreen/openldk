Unhandled OPENLDK::|condition-java/lang/AssertionError| in thread #<SB-THREAD:THREAD tid=55683 "main thread" RUNNING
                                                                     {106EC20133}>:
  Condition OPENLDK::|condition-java/lang/AssertionError| was signalled.

Backtrace for: #<SB-THREAD:THREAD tid=55683 "main thread" RUNNING {106EC20133}>
0: (SB-DEBUG::DEBUGGER-DISABLED-HOOK #<OPENLDK::|condition-java/lang/AssertionError| {10085DE1C3}> #<unused argument> :QUIT T)
1: (SB-DEBUG::RUN-HOOK *INVOKE-DEBUGGER-HOOK* #<OPENLDK::|condition-java/lang/AssertionError| {10085DE1C3}>)
2: (INVOKE-DEBUGGER #<OPENLDK::|condition-java/lang/AssertionError| {10085DE1C3}>)
3: (ERROR #<OPENLDK::|condition-java/lang/AssertionError| {10085DE1C3}>)
4: ((:METHOD OPENLDK::|println(F)| (OPENLDK::|java/io/PrintStream| T)) #<OPENLDK::|java/io/PrintStream| {100167D223}> 1.0) [fast-method]
5: (OPENLDK::|Test.fconst()|)
6: (OPENLDK::|Test.fconst()|)
7: (OPENLDK::|Test.main([Ljava/lang/String;)| #<unused argument>)
8: (OPENLDK::|Test.main([Ljava/lang/String;)| #())
9: (SB-INT:SIMPLE-EVAL-IN-LEXENV (OPENLDK::|Test.main([Ljava/lang/String;)| #()) #<NULL-LEXENV>)
10: (EVAL (OPENLDK::|Test.main([Ljava/lang/String;)| #()))
11: (OPENLDK::%EVAL (OPENLDK::|Test.main([Ljava/lang/String;)| #()))
12: (OPENLDK::MAIN "Test" NIL :DUMP-DIR NIL :CLASSPATH NIL)
13: (OPENLDK::MAIN-COMMAND "openldk" NIL)
14: (OPENLDK::MAIN-COMMAND "openldk") [optional]
15: (OPENLDK:MAIN-WRAPPER)
16: ((FLET SB-UNIX::BODY :IN SB-IMPL::START-LISP))
17: ((FLET "WITHOUT-INTERRUPTS-BODY-3" :IN SB-IMPL::START-LISP))
18: (SB-IMPL::%START-LISP)

unhandled condition in --disable-debugger mode, quitting

want: null
 got: null

want: -1
 got: -1

want: 0
 got: 0

want: 1
 got: 1

want: 2
 got: 2

want: 3
 got: 3

want: 4
 got: 4

want: 5
 got: 5

want: 0
 got: 0

want: 1
 got: 1

want: 0
 got: 0.0

want: 1
 got: 