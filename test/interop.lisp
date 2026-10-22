;;;; Tests for openldk/interop, the shared core of the language systems
;;;; (FiveAM).  `make check-interop` runs them.

(defpackage :openldk-interop-tests
  (:use :cl :fiveam :openldk.interop))

(in-package :openldk-interop-tests)

(def-suite :openldk-interop-tests
  :description "The shared core: Java classes defined in Lisp, ...")

(in-suite :openldk-interop-tests)

(define-java-class greeter (:implements ("java.util.function.Function"))
    ((greeting :initform "hello") (count :initform 0))
  ("apply(Ljava/lang/Object;)Ljava/lang/Object;" (self name)
    (incf count)
    (jstring (format nil "~A, ~A (~D)" greeting (lstring name) count)))
  ("toString()Ljava/lang/String;" (self)
    (jstring (format nil "#<greeter ~S>" greeting))))

(define-java-class failing-runnable (:implements ("java.lang.Runnable")) ()
  ("run()V" (self) (error "from Lisp")))

(test define-java-class
  (let* ((g (make-java-object 'greeter :greeting "hi"))
         (class (java-call "getClass()" g)))
    (is (string= "lisp.openldk_interop_tests.greeter" (lstring (java-call "getName()" class))))
    (is (string= "hi, lisp (1)" (lstring (java-call "apply(Ljava/lang/Object;)" g (jstring "lisp")))))
    (is (string= "hi, java (2)"
                 (lstring (java-call "get()" (java-call "map(Ljava/util/function/Function;)"
                                                        (java-static "java.util.Optional" "of(Ljava/lang/Object;)"
                                                                     (jstring "java"))
                                                        g))))
        "Java code calls the Lisp method")
    (let ((method (java-call "getMethod(Ljava/lang/String;[Ljava/lang/Class;)" class (jstring "apply")
                             (make-java-array-of "java.lang.Class"
                                                 (list (openldk::java-class (java-load "java.lang.Object")))))))
      (is (string= "hi, reflection (3)"
                   (lstring (java-call "invoke(Ljava/lang/Object;[Ljava/lang/Object;)" method g
                                       (make-java-array-of "java.lang.Object" (list (jstring "reflection"))))))
          "reflection finds and calls it"))
    (is (string= "#<greeter \"hi\">" (lstring (java-static "java.lang.String" "valueOf(Ljava/lang/Object;)" g))))
    (is (eql 1 (java-call "isInstance(Ljava/lang/Object;)"
                          (openldk::java-class (java-load "java.util.function.Function")) g))
        "an instance of its interface")
    (is (string= "hello, x (1)" (lstring (java-call "apply(Ljava/lang/Object;)" (make-java-object 'greeter) (jstring "x"))))
        "initforms")))

(test define-java-class-errors
  (signals simple-error
    (with-foreign-errors ("Java")
      (java-call "run()V" (make-java-object 'failing-runnable))))
  (signals simple-error
    (with-foreign-errors ("Java")
      (java-call "join()" (java-static "java.util.concurrent.CompletableFuture" "runAsync(Ljava/lang/Runnable;)"
                                       (make-java-object 'failing-runnable)))))
  (signals error (make-java-object 'no-such-class)))
