;;;; Tests for openldk/polyglot: every language in one SBCL, and foreign
;;;; source files as ASDF components (FiveAM).  `make check-polyglot` runs
;;;; them in sbcl-polyglot.

(defpackage :openldk-polyglot-tests
  (:use :cl :fiveam))

(in-package :openldk-polyglot-tests)

(def-suite :openldk-polyglot-tests
  :description "Every JVM language at once, and ASDF components in each.")

(in-suite :openldk-polyglot-tests)

(test every-language-started
  (is (equal (polyglot:languages) (polyglot:startup :verbose nil))))

(test asdf-components
  (let ((asdf:*central-registry*
          (cons (asdf:system-relative-pathname "openldk" "test/polyglot-demo/")
                asdf:*central-registry*)))
    (asdf:load-system "polyglot-demo" :force '("polyglot-demo")))
  (is (= 14 (openldk.interop:java-static "Stats" "sumOfSquares(I)" 3)) "Scala")
  (is (< 3.14 (kotlin:call "ShapesKt" "circleArea" 1d0) 3.15) "Kotlin")
  (is (= 15 (kotlin:call "ShapesKt" "squaresPlusOne" 3)) "Kotlin, calling Scala")
  (is (string= "Hello, Lisp!"
               (openldk.interop:lstring
                (openldk.interop:java-static "Greet" "hello(Ljava/lang/String;)" (openldk.interop:jstring "lisp"))))
      "Groovy")
  (is (string= "HI!" (clojure:funcall "words/shout" "hi")) "Clojure")
  (is (= 2432902008176640000 (kawa:funcall "kawa-fact" 20)) "Kawa")
  (is (= 55 (jscheme:funcall "jfib" 10)) "JScheme")
  (is (= 3 (abcl:funcall "TOWER-HEIGHT")) "ABCL")
  (is (search (string #\Newline) (kotlin:call "ShapesKt" "wrapped" "a library from Maven, wrapped"))
      "Kotlin, calling a Maven dependency")
  (is (string= "Hello"
               (openldk.interop:lstring
                (openldk.interop:java-static "org.apache.commons.text.WordUtils" "capitalize(Ljava/lang/String;)"
                                             (openldk.interop:jstring "hello"))))
      "Lisp, calling it"))
