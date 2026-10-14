;;;; OpenLDK unit tests (FiveAM).
;;;;
;;;; These exercise the pure, deterministic transformation helpers (JVM
;;;; descriptor parsing, etc.) directly, without needing a running JVM or the
;;;; DejaGnu suite. Run with: (asdf:test-system :openldk)  or
;;;; (asdf:load-system :openldk/tests) then (fiveam:run! :openldk-tests).

(defpackage :openldk-tests
  (:use :cl :fiveam))

(in-package :openldk-tests)

(def-suite :openldk-tests
  :description "OpenLDK pure-function unit tests.")

(in-suite :openldk-tests)

(test count-parameters
  "count-parameters counts JVM method-descriptor parameters (category-2 types
count as one parameter)."
  (is (= 0 (openldk::count-parameters "()V")))
  (is (= 2 (openldk::count-parameters "(II)I")))
  (is (= 2 (openldk::count-parameters "(JD)V")))
  (is (= 2 (openldk::count-parameters "(Ljava/lang/String;I)V")))
  (is (= 1 (openldk::count-parameters "([[I)V")))
  (is (= 3 (openldk::count-parameters "([Ljava/lang/String;IJ)V"))))

(test get-return-type
  "get-return-type maps the descriptor's return type to a jtype keyword."
  (is (eq :VOID      (openldk::get-return-type "()V")))
  (is (eq :INTEGER   (openldk::get-return-type "(I)I")))
  (is (eq :LONG      (openldk::get-return-type "()J")))
  (is (eq :BOOLEAN   (openldk::get-return-type "()Z")))
  (is (eq :REFERENCE (openldk::get-return-type "()Ljava/lang/String;")))
  (is (eq :ARRAY     (openldk::get-return-type "()[I"))))

(test parse-parameter-types
  "parse-parameter-types returns the parameter type names in order."
  (is (equal '() (openldk::parse-parameter-types "()V")))
  (is (equal '("int" "short") (openldk::parse-parameter-types "(IS)V")))
  (is (equal '("long" "double") (openldk::parse-parameter-types "(JD)V")))
  (is (equal '("int" "short" "java/lang/String[]")
             (openldk::parse-parameter-types "(IS[Ljava/lang/String;)V"))))

(test descriptor-param-type-chars
  "%descriptor-param-type-chars returns the top-level parameter type chars
(primitives as their char, L for object refs, [ for arrays)."
  (is (equal '() (openldk::%descriptor-param-type-chars "()V")))
  (is (equal '(#\Z #\C) (openldk::%descriptor-param-type-chars "(ZC)Ljava/lang/String;")))
  (is (equal '(#\I #\L #\J) (openldk::%descriptor-param-type-chars "(ILjava/lang/String;J)V")))
  (is (equal '(#\[) (openldk::%descriptor-param-type-chars "([I)V"))))

(test string-value-bytes-are-java-bytes
  ;; Java's String.value is a byte[], so its bytes are signed: e-acute (233)
  ;; is -23.  JDK code relies on that, e.g. String.encodeUTF8 copies a LATIN1
  ;; value unchanged when StringCoding.hasNegatives finds no negative byte.
  (multiple-value-bind (bytes coder) (openldk::%string-value-bytes (coerce '(#\h #\LATIN_SMALL_LETTER_E_WITH_ACUTE) 'string))
    (is (= 0 coder))
    (is (equalp '(104 -23) (coerce bytes 'list))))
  ;; UTF16 is little-endian code units, each byte signed too: U+65E5 is E5 65.
  (multiple-value-bind (bytes coder) (openldk::%string-value-bytes (coerce (list #\a (code-char #x65E5)) 'string))
    (is (= 1 coder))
    (is (equalp '(97 0 -27 101) (coerce bytes 'list))))
  (multiple-value-bind (bytes coder) (openldk::%string-value-bytes "plain")
    (is (= 0 coder))
    (is (equalp '(112 108 97 105 110) (coerce bytes 'list)))))
