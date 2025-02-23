Unhandled OPENLDK::|condition-java/lang/NullPointerException| in thread #<SB-THREAD:THREAD tid=55147 "main thread" RUNNING
                                                                           {1005E80003}>:
  Condition OPENLDK::|condition-java/lang/NullPointerException| was signalled.

Backtrace for: #<SB-THREAD:THREAD tid=55147 "main thread" RUNNING {1005E80003}>
0: (SB-DEBUG::DEBUGGER-DISABLED-HOOK #<OPENLDK::|condition-java/lang/NullPointerException| {100B2390C3}> #<unused argument> :QUIT T)
1: (SB-DEBUG::RUN-HOOK *INVOKE-DEBUGGER-HOOK* #<OPENLDK::|condition-java/lang/NullPointerException| {100B2390C3}>)
2: (INVOKE-DEBUGGER #<OPENLDK::|condition-java/lang/NullPointerException| {100B2390C3}>)
3: (ERROR #<OPENLDK::|condition-java/lang/NullPointerException| {100B2390C3}>)
4: ((:METHOD OPENLDK::|<init>(Ljava/io/File;Ljava/lang/String;)| (OPENLDK::|java/io/RandomAccessFile| T T)) #<OPENLDK::|java/io/RandomAccessFile| {1007DB2053}> NIL #<OPENLDK::|java/lang/String| "r">) [fast-method]
5: ((:METHOD OPENLDK::|<init>(Ljava/io/File;Ljava/lang/String;)| (OPENLDK::|java/io/RandomAccessFile| T T)) #<OPENLDK::|java/io/RandomAccessFile| {1007DB2053}> NIL #<OPENLDK::|java/lang/String| "r">) [fast-method]
6: ((:METHOD OPENLDK::|<init>(Ljava/lang/String;Ljava/lang/String;)| (OPENLDK::|java/io/RandomAccessFile| T T)) #<OPENLDK::|java/io/RandomAccessFile| {1007DB2053}> NIL #<OPENLDK::|java/lang/String| "r">) [fast-method]
7: ((:METHOD OPENLDK::|<init>(Ljava/lang/String;Ljava/lang/String;)| (OPENLDK::|java/io/RandomAccessFile| T T)) #<OPENLDK::|java/io/RandomAccessFile| {1007DB2053}> NIL #<OPENLDK::|java/lang/String| "r">) [fast-method]
8: (OPENLDK::|Test.foo()|)
9: (OPENLDK::|Test.foo()|)
10: (OPENLDK::|Test.main([Ljava/lang/String;)| #<unused argument>)
11: (OPENLDK::|Test.main([Ljava/lang/String;)| #())
12: (SB-INT:SIMPLE-EVAL-IN-LEXENV (OPENLDK::|Test.main([Ljava/lang/String;)| #()) #<NULL-LEXENV>)
13: (EVAL (OPENLDK::|Test.main([Ljava/lang/String;)| #()))
14: (OPENLDK::%EVAL (OPENLDK::|Test.main([Ljava/lang/String;)| #()))
15: (OPENLDK::MAIN "Test" NIL :DUMP-DIR NIL :CLASSPATH NIL)
16: (OPENLDK::MAIN-COMMAND "openldk" NIL)
17: (OPENLDK::MAIN-COMMAND "openldk") [optional]
18: (OPENLDK:MAIN-WRAPPER)
19: ((FLET SB-UNIX::BODY :IN SB-IMPL::START-LISP))
20: ((FLET "WITHOUT-INTERRUPTS-BODY-3" :IN SB-IMPL::START-LISP))
21: (SB-IMPL::%START-LISP)

unhandled condition in --disable-debugger mode, quitting
