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

(defun %zip-with-prefix (prefix-length cd-offset)
  "Octets of a fake file: PREFIX-LENGTH bytes of prefix, a 10-byte stand-in
for the local entries, a 46-byte stand-in for the central directory, then an
end record giving the central directory's size and CD-OFFSET."
  (let ((v (make-array (+ prefix-length 10 46 22) :element-type '(unsigned-byte 8)
                                                  :initial-element 0))
        (eocd (+ prefix-length 10 46)))
    (replace v #(#x50 #x4B 5 6) :start1 eocd)
    (loop for k below 4
          do (setf (aref v (+ eocd 12 k)) (ldb (byte 8 (* 8 k)) 46)
                   (aref v (+ eocd 16 k)) (ldb (byte 8 (* 8 k)) cd-offset)))
    v))

(test zip-prefix-length
  "zip-prefix-length finds the bytes in front of a zip archive, which for a
JMOD is its 4-byte header or, when its offsets count from the start of the
file, nothing."
  ;; Offsets counted from after a 4-byte header, as in OpenJDK's JMODs.
  (let ((v (%zip-with-prefix 4 10)))
    (is (= 4 (openldk::zip-prefix-length v (length v)))))
  ;; Offsets counted from the start of the file, as in some of GraalVM's.
  (let ((v (%zip-with-prefix 4 14)))
    (is (= 0 (openldk::zip-prefix-length v (length v)))))
  ;; Only the last bytes of the file are read.
  (let ((v (%zip-with-prefix 4 10)))
    (is (= 4 (openldk::zip-prefix-length (subseq v (- (length v) 22)) (length v)))))
  ;; No end record, or a zip64 one: NIL, and the caller keeps its default.
  (is (null (openldk::zip-prefix-length (make-array 30 :element-type '(unsigned-byte 8)
                                                       :initial-element 0)
                                        30)))
  (let ((v (%zip-with-prefix 4 #xFFFFFFFF)))
    (is (null (openldk::zip-prefix-length v (length v))))))
