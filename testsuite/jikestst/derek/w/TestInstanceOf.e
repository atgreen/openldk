Unhandled SIMPLE-ERROR in thread #<SB-THREAD:THREAD tid=56516 "main thread" RUNNING
                                    {1005E80003}>:
  unknown type specifier: OPENLDK::|[Ljava/lang/Object;|

Backtrace for: #<SB-THREAD:THREAD tid=56516 "main thread" RUNNING {1005E80003}>
0: (SB-DEBUG::DEBUGGER-DISABLED-HOOK #<SIMPLE-ERROR "unknown type specifier: ~S" {101AD3FA03}> #<unused argument> :QUIT T)
1: (SB-DEBUG::RUN-HOOK *INVOKE-DEBUGGER-HOOK* #<SIMPLE-ERROR "unknown type specifier: ~S" {101AD3FA03}>)
2: (INVOKE-DEBUGGER #<SIMPLE-ERROR "unknown type specifier: ~S" {101AD3FA03}>)
3: (ERROR "unknown type specifier: ~S" OPENLDK::|[Ljava/lang/Object;|)
4: ((LABELS SB-KERNEL::RECURSE :IN SB-KERNEL:%%TYPEP) #<OPENLDK::|Test| {1006D0C603}> #<SB-KERNEL:UNKNOWN-TYPE OPENLDK::|[Ljava/lang/Object;|>)
5: (OPENLDK::|Test.test(Ljava/lang/Object;)| #<OPENLDK::|Test| {1006D0C603}>)
6: (OPENLDK::|Test.test(Ljava/lang/Object;)| #<OPENLDK::|Test| {1006D0C603}>)
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

want: true false false false
 got: 