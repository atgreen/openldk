Unhandled UNDEFINED-FUNCTION in thread #<SB-THREAD:THREAD tid=55285 "main thread" RUNNING
                                          {1005E80003}>:
  The function :FNEG is undefined.

Backtrace for: #<SB-THREAD:THREAD tid=55285 "main thread" RUNNING {1005E80003}>
0: ("undefined function" #<OPENLDK::<CONTEXT> {101F794093}> #(12 67 13 68 178 0 6 18 40 182 0 8 ...))
1: (OPENLDK::%COMPILE-METHOD "Test" 5)
2: (OPENLDK::|Test.ftest()|)
3: (OPENLDK::|Test.main([Ljava/lang/String;)| #<unused argument>)
4: (OPENLDK::|Test.main([Ljava/lang/String;)| #())
5: (SB-INT:SIMPLE-EVAL-IN-LEXENV (OPENLDK::|Test.main([Ljava/lang/String;)| #()) #<NULL-LEXENV>)
6: (EVAL (OPENLDK::|Test.main([Ljava/lang/String;)| #()))
7: (OPENLDK::%EVAL (OPENLDK::|Test.main([Ljava/lang/String;)| #()))
8: (OPENLDK::MAIN "Test" NIL :DUMP-DIR NIL :CLASSPATH NIL)
9: (OPENLDK::MAIN-COMMAND "openldk" NIL)
10: (OPENLDK::MAIN-COMMAND "openldk") [optional]
11: (OPENLDK:MAIN-WRAPPER)
12: ((FLET SB-UNIX::BODY :IN SB-IMPL::START-LISP))
13: ((FLET "WITHOUT-INTERRUPTS-BODY-3" :IN SB-IMPL::START-LISP))
14: (SB-IMPL::%START-LISP)

unhandled condition in --disable-debugger mode, quitting

want: 4
 got: 4

want: 2
 got: 2

want: 9
 got: 9

want: 1
 got: 1

want: 1
 got: 1

want: -3
 got: -3

want: 4
 got: 4

want: 1
 got: 1

want: 273
 got: 273

want: 272
 got: 272

want: -6
 got: -6

want: -2
 got: -2

want: 2147483646
 got: 2147483646

want: 10000000002
 got: 5705032706

want: 9999999998
 got: 5705032702

want: 20000000000
 got: 11410065408

want: 5000000000
 got: -1442450944

want: 0
 got: 0

want: -2
 got: -2

want: -10000000000
 got: -5705032704

want: 4503599627370497
 got:  165248866713601

want: 1229482698272145681
 got:  795647617911488785

want: 1224979098644775184
 got:  791144022579085584

want: -6
 got: -6

want: -2
 got: -2

want: 9223372036854775806
 got: 8789536960789086206
