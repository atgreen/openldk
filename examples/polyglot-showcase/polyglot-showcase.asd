;;;; The OpenLDK polyglot showcase: an ASDF system with a Java library from
;;;; Maven Central, a Kotlin source file compiled against it, and a Lisp
;;;; program that uses them and six other JVM languages in one process.
;;;; See README.md.

(asdf:defsystem "polyglot-showcase"
  :description "Every OpenLDK polyglot feature, in one small bookshop"
  :defsystem-depends-on ("openldk/polyglot")
  :serial t
  :components ((:maven-jar "commons-lang3"          ; a Java library, from Maven Central,
                :coordinates "org.apache.commons:commons-lang3:3.14.0")
               (:maven-jar "commons-text"           ; checked against Maven's SHA-1
                :coordinates "org.apache.commons:commons-text:1.12.0")
               (:kotlin-file "catalog")             ; compiled by kotlinc, against commons-text
               (:file "showcase")))                 ; Lisp, with six languages inline
