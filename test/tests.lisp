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

(test vm-nano-time-adjustment
  ;; VM.getNanoTimeAdjustment(offset): nanoseconds from OFFSET (epoch
  ;; seconds) to now, or -1 when OFFSET is 2^32 seconds or more away.  It
  ;; backs Clock.currentInstant, so Instant.now().
  (let* ((epoch-now (- (get-universal-time) (encode-universal-time 0 0 0 1 1 1970 0)))
         (adjust (lambda (offset)
                   (openldk::|jdk/internal/misc/VM.getNanoTimeAdjustment(J)| offset))))
    (is (<= (* 9 1000000000) (funcall adjust (- epoch-now 10)) (* 12 1000000000)))
    (is (<= (* (- epoch-now 2) 1000000000) (funcall adjust 0) (* (+ epoch-now 2) 1000000000))
        "1970 is within 2^32 seconds")
    (is (= -1 (funcall adjust (- epoch-now (expt 2 32)))))
    (is (= -1 (funcall adjust (+ epoch-now (expt 2 32)))))))
