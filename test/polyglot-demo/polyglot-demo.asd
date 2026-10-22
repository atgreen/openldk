;;;; A system mixing every JVM language OpenLDK runs, as ASDF components:
;;;; test/polyglot.lisp (make check-polyglot) loads it and calls into each.

(asdf:defsystem "polyglot-demo"
  :description "One system, seven languages"
  :defsystem-depends-on ("openldk/polyglot")
  :serial t
  :components ((:maven-jar "commons-text"    ; a Java library, from Maven Central
                :coordinates "org.apache.commons:commons-text:1.12.0")
               (:maven-jar "commons-lang3"
                :coordinates "org.apache.commons:commons-lang3:3.14.0")
               (:scala-file "stats")        ; compiled by scalac
               (:kotlin-file "shapes")      ; compiled by kotlinc, calling Stats
               (:groovy-file "greet")       ; compiled by groovyc
               (:clojure-file "words")      ; loaded by Clojure
               (:kawa-file "fact")          ; loaded by Kawa
               (:jscheme-file "jfib")       ; loaded by JScheme
               (:abcl-file "tower")))       ; loaded by ABCL
