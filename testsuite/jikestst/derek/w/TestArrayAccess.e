Unhandled TYPE-ERROR in thread #<SB-THREAD:THREAD tid=55377 "main thread" RUNNING
                                  {1005E80003}>:
  The value
    -25
  is not of type
    (UNSIGNED-BYTE 8)

Backtrace for: #<SB-THREAD:THREAD tid=55377 "main thread" RUNNING {1005E80003}>
0: (SB-IMPL::OUTPUT-UNSIGNED-BYTE-FULL-BUFFERED #<SB-SYS:FD-STREAM for "standard output" {1005E7C3E3}> -25)
1: (SB-IMPL::WRITE-SEQ-IMPL #(-25 -67 -127 110 116 58 32 65 10 32 103 111 ...) #<SB-SYS:FD-STREAM for "standard output" {1005E7C3E3}> 0 3)
2: (SB-IMPL::WRITE-SEQ-IMPL #(-25 -67 -127 110 116 58 32 65 10 32 103 111 ...) #<SYNONYM-STREAM :SYMBOL SB-SYS:*STDOUT* {100000C643}> 0 3)
3: (WRITE-SEQUENCE #(-25 -67 -127 110 116 58 32 65 10 32 103 111 ...) #<SYNONYM-STREAM :SYMBOL SB-SYS:*STDOUT* {100000C643}> :START 0 :END 3)
4: ((:METHOD OPENLDK::|writeBytes([BIIZ)| (OPENLDK::|java/io/FileOutputStream| T T T T)) #<OPENLDK::|java/io/FileOutputStream| {100224B073}> #(-25 -67 -127 110 116 58 32 65 10 32 103 111 ...) 0 3 #<unused argument>) [fast-method]
5: ((:METHOD OPENLDK::|write([BII)| (OPENLDK::|java/io/FileOutputStream| T T T)) #<OPENLDK::|java/io/FileOutputStream| {100224B073}> #(-25 -67 -127 110 116 58 32 65 10 32 103 111 ...) 0 3) [fast-method]
6: ((:METHOD OPENLDK::|flushBuffer()| (OPENLDK::|java/io/BufferedOutputStream|)) #<OPENLDK::|java/io/BufferedOutputStream| {1002268013}>) [fast-method]
7: ((:METHOD OPENLDK::|flush()| (OPENLDK::|java/io/BufferedOutputStream|)) #<OPENLDK::|java/io/BufferedOutputStream| {1002268013}>) [fast-method]
8: ((:METHOD OPENLDK::|write([BII)| (OPENLDK::|java/io/PrintStream| T T T)) #<OPENLDK::|java/io/PrintStream| {100167D223}> #(-25 -67 -127 110 116 58 32 65 10 32 103 111 ...) 0 3) [fast-method]
9: ((:METHOD OPENLDK::|writeBytes()| (OPENLDK::|sun/nio/cs/StreamEncoder|)) #<OPENLDK::|sun/nio/cs/StreamEncoder| {1003AB0013}>) [fast-method]
10: ((:METHOD OPENLDK::|implFlushBuffer()| (OPENLDK::|sun/nio/cs/StreamEncoder|)) #<OPENLDK::|sun/nio/cs/StreamEncoder| {1003AB0013}>) [fast-method]
11: ((:METHOD OPENLDK::|flushBuffer()| (OPENLDK::|sun/nio/cs/StreamEncoder|)) #<OPENLDK::|sun/nio/cs/StreamEncoder| {1003AB0013}>) [fast-method]
12: ((:METHOD OPENLDK::|flushBuffer()| (OPENLDK::|java/io/OutputStreamWriter|)) #<OPENLDK::|java/io/OutputStreamWriter| {1002268033}>) [fast-method]
13: ((:METHOD OPENLDK::|write(Ljava/lang/String;)| (OPENLDK::|java/io/PrintStream| T)) #<OPENLDK::|java/io/PrintStream| {100167D223}> #<OPENLDK::|java/lang/String| "罁">) [fast-method]
14: ((:METHOD OPENLDK::|print(C)| (OPENLDK::|java/io/PrintStream| T)) #<OPENLDK::|java/io/PrintStream| {100167D223}> 32577) [fast-method]
15: ((:METHOD OPENLDK::|print(C)| (OPENLDK::|java/io/PrintStream| T)) #<OPENLDK::|java/io/PrintStream| {100167D223}> 32577) [fast-method]
16: ((:METHOD OPENLDK::|println(C)| (OPENLDK::|java/io/PrintStream| T)) #<OPENLDK::|java/io/PrintStream| {100167D223}> 32577) [fast-method]
17: ((:METHOD OPENLDK::|println(C)| (OPENLDK::|java/io/PrintStream| T)) #<OPENLDK::|java/io/PrintStream| {100167D223}> 32577) [fast-method]
18: (OPENLDK::|Test.char_array()|)
19: (OPENLDK::|Test.char_array()|)
20: (OPENLDK::|Test.main([Ljava/lang/String;)| #<unused argument>)
21: (OPENLDK::|Test.main([Ljava/lang/String;)| #())
22: (SB-INT:SIMPLE-EVAL-IN-LEXENV (OPENLDK::|Test.main([Ljava/lang/String;)| #()) #<NULL-LEXENV>)
23: (EVAL (OPENLDK::|Test.main([Ljava/lang/String;)| #()))
24: (OPENLDK::%EVAL (OPENLDK::|Test.main([Ljava/lang/String;)| #()))
25: (OPENLDK::MAIN "Test" NIL :DUMP-DIR NIL :CLASSPATH NIL)
26: (OPENLDK::MAIN-COMMAND "openldk" NIL)
27: (OPENLDK::MAIN-COMMAND "openldk") [optional]
28: (OPENLDK:MAIN-WRAPPER)
29: ((FLET SB-UNIX::BODY :IN SB-IMPL::START-LISP))
30: ((FLET "WITHOUT-INTERRUPTS-BODY-3" :IN SB-IMPL::START-LISP))
31: (SB-IMPL::%START-LISP)

unhandled condition in --disable-debugger mode, quitting

want: false
 got: false

want: true
 got: true

want: 127
 got: 127

want: -1
 got: -1

want: A
 got: 