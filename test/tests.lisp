;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: OPENLDK-TESTS; Base: 10 -*-
;;;
;;; Copyright (C) 2026  Anthony Green <green@moxielogic.com>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later WITH Classpath-exception-2.0
;;;
;;; This file is part of OpenLDK.

;;; OpenLDK is free software; you can redistribute it and/or modify it
;;; under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 3, or (at your
;;; option) any later version.

;;; OpenLDK is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; General Public License for more details.

;;; You should have received a copy of the GNU General Public License
;;; along with OpenLDK; see the file COPYING.  If not, please see
;;; <http://www.gnu.org/licenses/>.

;;; Linking this library statically or dynamically with other modules is
;;; making a combined work based on this library.  Thus, the terms and
;;; conditions of the GNU General Public License cover the whole
;;; combination.

;;; As a special exception, the copyright holders of this library give
;;; you permission to link this library with independent modules to
;;; produce an executable, regardless of the license terms of these
;;; independent modules, and to copy and distribute the resulting
;;; executable under terms of your choice, provided that you also
;;; meet, for each linked independent module, the terms and conditions
;;; of the license of that module.  An independent module is a module
;;; which is not derived from or based on this library.  If you modify
;;; this library, you may extend this exception to your version of the
;;; library, but you are not obligated to do so.  If you do not wish
;;; to do so, delete this exception statement from your version.

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

(test native-strings-are-simple
  ;; Latin-1 and UTF-16 strings must both cross SBCL's C-string boundary.
  ;; An adjustable UTF-16 result with a fill pointer fails in STRING-TO-C-STRING.
  (dolist (source (list "plain" (string (code-char #xe9))
                       (string (code-char #x3bb)) (string (code-char #x1f680))))
    (let ((result (openldk::lstring (openldk::jstring source))))
      (is (string= source result))
      (is (typep result 'simple-string)))))

(test method-let-drops-unused-variables
  ;; Unmentioned variables without an initial value are dropped; bound
  ;; parameters and mentioned variables stay.
  (is (equal '((p arg0) (b))
             (openldk::%prune-unused-bindings '((p arg0) (a) (b) (c)) '((setf b p))))))

(test primitive-arrays-are-specialized
  ;; Primitive arrays use specialized vectors so JIT'd code can AREF them
  ;; unboxed.  Component classes given as descriptors need no JVM.
  (flet ((data (component &rest args)
           (openldk::java-array-data
            (apply #'openldk::make-java-array :component-class component args))))
    (is (equal '(signed-byte 32) (array-element-type (data "I" :size 3))))
    (is (equal '(signed-byte 8) (array-element-type (data "Z" :size 3))))
    (is (equal '(unsigned-byte 16) (array-element-type (data "C" :size 3))))
    (is (eq 'double-float (array-element-type (data "D" :size 3))))
    (is (eq t (array-element-type (data "[I" :size 3))))
    (is (equalp #(0 0) (data "J" :size 2)))
    (is (equalp #(0d0 0d0) (data "D" :size 2)))
    ;; Values are normalized as Java's narrowing stores would.
    (is (equalp #(104 105 0) (data "C" :initial-contents (coerce '(#\h #\i #\Null) 'string))))
    (is (equalp #(-56 1 0) (data "B" :initial-contents #(200 t nil))))
    (is (equalp #(1.5f0) (data "F" :initial-contents #(1.5d0))))
    (let ((array (openldk::make-java-array :component-class "S" :size 1)))
      (setf (openldk::jaref array 0) 40000)
      (is (= -25536 (openldk::jaref array 0))))))

(test unsafe-array-memory-view
  ;; Unsafe byte offsets address a primitive array's memory as the JVM
  ;; lays it out: little-endian, two's complement, IEEE.
  (let ((ints (openldk::make-java-array :component-class "I" :initial-contents #(1 -2))))
    (is (= 4 (openldk::%unsafe-array-element-scale ints)))
    (is (= 1 (openldk::%unsafe-array-read-byte ints 0)))
    (is (= #xFE (openldk::%unsafe-array-read-byte ints 4)))
    (is (= #xFFFFFFFE00000001 (openldk::%unsafe-read-array-bits ints 0 8)))
    (openldk::%unsafe-write-array-bits ints 2 2 #x1234)
    (is (= #x12340001 (openldk::jaref ints 0))))
  (let ((bytes (openldk::make-java-array :component-class "B" :size 4)))
    (openldk::%unsafe-array-write-byte bytes 3 #xFF)
    (is (= -1 (openldk::jaref bytes 3)))
    (is (= #xFF000000 (openldk::%unsafe-read-array-bits bytes 0 4))))
  (let ((doubles (openldk::make-java-array :component-class "D" :initial-contents #(1d0))))
    (is (= #x3FF0000000000000 (openldk::%unsafe-read-array-bits doubles 0 8)))))

(test method-variable-typing
  (flet ((categories (&rest sources)
           (let ((table (make-hash-table)))
             (loop for (variable . list) in sources
                   do (setf (gethash variable table) list))
             (sort (loop for v being the hash-keys of (openldk::%variable-categories table)
                           using (hash-value c)
                         collect (list v c))
                   #'string< :key (lambda (e) (symbol-name (first e)))))))
    ;; A loop counter copied through a stack variable stays typed.
    (is (equal '((i :int) (s :int))
               (categories '(i :int (:copy . s)) '(s (:copy . i)))))
    ;; Unknown values (field reads, parameters) and mixed categories don't.
    (is (equal '() (categories '(p nil :int) '(q :int :long))))
    ;; Nor does a copy of an untyped variable, or a copy-only cycle.
    (is (equal '((b :long)) (categories '(a :int (:copy . p)) '(p nil) '(b :long))))
    (is (equal '() (categories '(x :int (:copy . y)) '(y (:copy . z)) '(z (:copy . y))))))
  ;; Assignments to declared variables are normalized; others untouched.
  (is (equal '(progn (setf a (openldk::%as-int (+ a 1)))
                     (setf b '(setf a 2))
                     (setf (aref v 0) (setf a (openldk::%as-int 3))))
             (openldk::%normalize-typed-assignments
              '(progn (setf a (+ a 1))
                      (setf b '(setf a 2))
                      (setf (aref v 0) (setf a 3)))
              '((a signed-byte 32)))))
  (is (= 97 (openldk::%as-int #\a)))
  (is (= 1 (openldk::%as-int t)))
  (is (= -1 (openldk::%as-int #xFFFFFFFF))))

(test stack-variables-fold-into-their-single-use
  (let ((s1 (intern "s{1}" :openldk))
        (s2 (intern "s{2}" :openldk))
        (s3 (intern "s{3}" :openldk))
        (local (intern "local-1" :openldk)))
    (flet ((fold (&rest statements)
             (rest (second (openldk::%fold-stack-variables
                            (list 'progn (cons 'tagbody statements)) nil)))))
      ;; A value read once, first, by the next statement moves into it.
      (is (equal `((setf ,local (f (g))))
                 (fold `(setf ,s1 (g)) `(setf ,local (f ,s1)))))
      ;; Chains fold too.
      (is (equal `((h (f (g))))
                 (fold `(setf ,s1 (g)) `(setf ,s2 (f ,s1)) `(h ,s2))))
      ;; A call evaluated before the read keeps the value where it was:
      ;; (g) must still run before (k).
      (is (equal `((setf ,s1 (g)) (f (k) ,s1))
                 (fold `(setf ,s1 (g)) `(f (k) ,s1))))
      ;; Both values move when they are read in the order they were made...
      (is (equal `((h (+ (a) (b))))
                 (fold `(setf ,s1 (a)) `(setf ,s2 (b)) `(h (+ ,s1 ,s2)))))
      ;; ...but only (b) when reading s2 first would run (b) before (a).
      (is (equal `((setf ,s1 (a)) (h (+ (b) ,s1)))
                 (fold `(setf ,s1 (a)) `(setf ,s2 (b)) `(h (+ ,s2 ,s1)))))
      ;; Not across a tag, into a conditional branch, or when read twice.
      (is (equal `((setf ,s1 (g)) tag (f ,s1))
                 (fold `(setf ,s1 (g)) 'tag `(f ,s1))))
      (is (equal `((setf ,s1 (g)) (when (p) (f ,s1)))
                 (fold `(setf ,s1 (g)) `(when (p) (f ,s1)))))
      (is (equal `((setf ,s3 (g)) (f ,s3 ,s3))
                 (fold `(setf ,s3 (g)) `(f ,s3 ,s3)))))))

(test method-body-cleanup
  (let* ((this (intern "local-0" :openldk))
         (check '(unless (openldk::initialized-p c) (clinit-c)))
         (body `(tagbody
                   used unused
                   ,check (f) ,check
                   (g (openldk::%receiver ,this))
                   (go used)
                   used2 ,check))
         (targets (openldk::%go-targets `(progn ,body (go used2)))))
    ;; Unused tags go; a repeated check in a straight run goes, but one
    ;; after a jump target stays; THIS needs no receiver check.
    (is (equal `(tagbody used ,check (f) (g ,this) (go used) used2 ,check)
               (openldk::%clean-method-body body targets t)))
    ;; Without THIS-P (a static method, or local-0 reassigned) the
    ;; receiver check stays.
    (is (equal `(g (openldk::%receiver ,this))
               (fifth (openldk::%clean-method-body body targets nil))))))

(test vm-nano-time-adjustment
  ;; VM.getNanoTimeAdjustment(offset): nanoseconds from OFFSET (epoch
  ;; seconds) to now, or -1 when OFFSET is 2^32 seconds or more away.  It
  ;; backs Clock.currentInstant, so Instant.now().
  (let* ((epoch-now (- (get-universal-time) (encode-universal-time 0 0 0 1 1 1970 0)))
         (adjust (lambda (offset)
                   (openldk::|jdk/internal/misc/VM.getNanoTimeAdjustment(J)J| offset))))
    (is (<= (* 9 1000000000) (funcall adjust (- epoch-now 10)) (* 12 1000000000)))
    (is (<= (* (- epoch-now 2) 1000000000) (funcall adjust 0) (* (+ epoch-now 2) 1000000000))
        "1970 is within 2^32 seconds")
    (is (= -1 (funcall adjust (- epoch-now (expt 2 32)))))
    (is (= -1 (funcall adjust (+ epoch-now (expt 2 32)))))))

(defmacro with-fetch-environment ((cache &rest variables) &body body)
  "Run BODY with LDK_CACHE_DIR set to a fresh directory bound to CACHE,
and the environment VARIABLES ((name value) ...) set, restoring all after."
  (let ((saved (gensym "SAVED")))
    `(let* ((,cache (merge-pathnames (format nil "openldk-fetch-test-~36R/"
                                             (random (expt 36 8) (make-random-state t)))
                                     (uiop:temporary-directory)))
            (,saved (mapcar (lambda (name) (cons name (uiop:getenv name)))
                            (list "LDK_CACHE_DIR" ,@(mapcar #'first variables)))))
       (unwind-protect
            (progn
              (setf (uiop:getenv "LDK_CACHE_DIR") (uiop:native-namestring ,cache))
              ,@(loop for (name value) in variables
                      collect `(setf (uiop:getenv ,name) ,value))
              ,@body)
         (loop for (name . value) in ,saved
               do (if value (setf (uiop:getenv name) value) (sb-posix:unsetenv name)))
         (uiop:delete-directory-tree ,cache :validate t :if-does-not-exist :ignore)))))

(test fetch-url
  "fetch-url downloads into the cache, verifying the checksum, and leaves
nothing behind when it refuses."
  (with-fetch-environment (cache ("LDK_OFFLINE" "0"))
    (is (equal cache (openldk::ldk-cache-dir)))
    (let ((source (merge-pathnames "source.txt" cache))
          (destination (merge-pathnames "fetched/copy.txt" cache))
          (*error-output* (make-broadcast-stream)))
      (ensure-directories-exist source)
      (alexandria:write-string-into-file "abc" source)
      (let ((url (format nil "file://~A" (uiop:native-namestring source)))
            (sha256 "ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad"))
        (signals openldk::fetch-error
          (openldk::fetch-url url destination :sha256 (substitute #\0 #\b sha256)))
        (is (null (directory (merge-pathnames "fetched/*.*" cache))))
        (openldk::fetch-url url destination :sha256 (string-upcase sha256))
        (is (equal "abc" (alexandria:read-file-into-string destination)))
        (setf (uiop:getenv "LDK_OFFLINE") "1")
        (signals openldk::fetch-error (openldk::fetch-url url destination))))))

(test unsafe-fill-memory
  ;; Unsafe.setMemory's fill value is a Java byte, and so signed.  Native
  ;; memory holds it unsigned, so the whole -128..127 range has to be
  ;; storable: filling with (byte) 0xFF is an ordinary thing for Java code
  ;; to ask for, and arrives here as -1.
  (let* ((size 4)
         (mem (sb-alien:make-alien sb-alien:char size))
         (sap (sb-alien:alien-sap mem)))
    (unwind-protect
         (flet ((bytes ()
                  (loop for i below size collect (sb-sys:sap-ref-8 sap i))))
           (openldk::%unsafe-fill-memory (sb-sys:sap-int sap) size -1)
           (is (equal '(#xFF #xFF #xFF #xFF) (bytes)))
           (openldk::%unsafe-fill-memory (sb-sys:sap-int sap) size -128)
           (is (equal '(#x80 #x80 #x80 #x80) (bytes)))
           (openldk::%unsafe-fill-memory (sb-sys:sap-int sap) size 127)
           (is (equal '(#x7F #x7F #x7F #x7F) (bytes)))
           (openldk::%unsafe-fill-memory (sb-sys:sap-int sap) size 0)
           (is (equal '(0 0 0 0) (bytes)))
           ;; A fill shorter than the block leaves the rest alone.
           (openldk::%unsafe-fill-memory (sb-sys:sap-int sap) 2 -1)
           (is (equal '(#xFF #xFF 0 0) (bytes))))
      (sb-alien:free-alien mem))))

(test fasl-cache-externalize
  ;; Code the FASL cache can write as it is comes back unchanged; a NaN,
  ;; which can't be printed readably, is made at load time from its bits;
  ;; and a user loader's package, which the next run won't have, makes the
  ;; code uncacheable.
  (let ((code '(defun f (x) (if (eql x 1.5d0) '(a "b" #\c #(1 2)) (list x :k)))))
    (is (equal code (openldk::%externalize-definition code)))
    ;; EQUALP, which compares vectors by their elements.
    (is (equalp code (let ((*package* (find-package :openldk)))
                      (read-from-string (openldk::%print-definition code))))))
  (let* ((nan (float-features:bits-double-float #x7FF8000000000000))
         (ext (openldk::%externalize-definition `(foo ,nan))))
    (is (eq 'load-time-value (first (second ext))))
    (is (float-features:float-nan-p (eval (second (second ext))))))
  (let ((package (or (find-package "OPENLDK.L999") (make-package "OPENLDK.L999" :use nil))))
    (unwind-protect
         (signals openldk::uncacheable
           (openldk::%externalize-definition `(foo ,(intern "BAR" package))))
      (delete-package package))))

(test volatile-field-resolution
  ;; The JIT must preserve volatile semantics when the Fieldref names an
  ;; inheriting class, but must stop when a subclass shadows that field.
  (let* ((openldk::*ldk-classes-by-bin-name* (make-hash-table :test 'equal))
         (parent (make-instance 'openldk::<class> :name "ordering/Parent"
                                :super nil :ldk-loader nil
                                :fields (list (make-instance 'openldk::<field>
                                                             :name "flag" :access-flags #x40))))
         (child (make-instance 'openldk::<class> :name "ordering/Child"
                               :super "ordering/Parent" :ldk-loader nil :fields nil)))
    (setf (gethash "ordering/Parent" openldk::*ldk-classes-by-bin-name*) parent)
    (is (openldk::%volatile-field-p child "flag"))
    (is (not (openldk::%volatile-field-p child "missing")))
    (setf (openldk::fields child)
          (list (make-instance 'openldk::<field> :name "flag" :access-flags 0)))
    (is (not (openldk::%volatile-field-p child "flag")))))

(defun %verification-fixture (bytes &key (descriptor "()V") (max-stack 4) (max-locals 4) maps
                                       (method-name "test") super pool (version 69) (static t))
  "Construct raw metadata for verifier tests without loading Java or running the JIT."
  (let* ((class (make-instance 'openldk::<class> :name "verify/Fixture" :super super
                               :interfaces nil :ldk-loader nil :major-version version :access-flags #x21
                               :fields #() :constant-pool (or pool #(nil))))
         (attributes (make-hash-table :test 'equal))
         (code-attributes (make-hash-table :test 'equal))
         (method (make-instance 'openldk::<method> :class class :name method-name
                                :descriptor descriptor :access-flags (if static 8 0) :attributes attributes)))
    (when maps (setf (gethash "StackMapTable" code-attributes) maps))
    (setf (gethash "Code" attributes)
          (make-instance 'openldk::<code> :code bytes :max-stack max-stack :max-locals max-locals
                         :exceptions nil :attributes code-attributes)
          (openldk::methods class) (vector method))
    class))

(defun %verify-fixture (bytes &rest options)
  "Run both independent verifier passes on a synthetic method."
  (let ((class (apply #'%verification-fixture bytes options)))
    (openldk::verify-class-structure class)
    (openldk::verify-class-bytecode class)))

(test verifier-structure
  (is (%verify-fixture #(177) :max-stack 0 :max-locals 0))
  (signals openldk::bytecode-verification-error (%verify-fixture #(16)))
  (signals openldk::bytecode-verification-error (%verify-fixture #(203)))
  (signals openldk::bytecode-verification-error (%verify-fixture #(167 0 2 177)))
  (signals openldk::bytecode-verification-error (%verify-fixture #(21 4 87 177) :max-locals 4))
  (signals openldk::bytecode-verification-error (%verify-fixture #(22 0 173) :descriptor "(J)J" :max-locals 1))
  (signals openldk::bytecode-verification-error (%verify-fixture #(196 0 0 0 177)))
  (signals openldk::bytecode-verification-error (%verify-fixture #(18 0 87 177)))
  (signals openldk::bytecode-verification-error (%verify-fixture #(188 99 87 177)))
  (signals openldk::bytecode-verification-error (%verify-fixture #(168 0 0))))

(test verifier-types
  (is (%verify-fixture #(26 4 96 172) :descriptor "(I)I" :max-stack 2 :max-locals 1))
  (is (%verify-fixture #(30 173) :descriptor "(J)J" :max-stack 2 :max-locals 2))
  (is (%verify-fixture #(42 190 172) :descriptor "([I)I" :max-locals 1))
  (signals openldk::bytecode-verification-error (%verify-fixture #(1 172) :descriptor "()I"))
  (signals openldk::bytecode-verification-error (%verify-fixture #(87 177)))
  (signals openldk::bytecode-verification-error (%verify-fixture #(3 4 96 172) :descriptor "()I" :max-stack 1))
  (signals openldk::bytecode-verification-error (%verify-fixture #(26 172) :descriptor "()I"))
  (signals openldk::bytecode-verification-error (%verify-fixture #(9 89 173) :descriptor "()J"))
  (signals openldk::bytecode-verification-error (%verify-fixture #(42 3 46 172) :descriptor "([J)I"))
  (signals openldk::bytecode-verification-error (%verify-fixture #(3 177) :descriptor "()I"))
  (signals openldk::bytecode-verification-error (%verify-fixture #(0)))
  (signals openldk::bytecode-verification-error
    (%verify-fixture #(177) :method-name "<init>" :static nil :super "java/lang/Object")))

(test verifier-stack-maps
  (let ((code #(26 153 0 7 4 167 0 4 5 172)))
    (is (%verify-fixture code :descriptor "(I)I" :maps #(0 2 8 64 1)))
    (signals openldk::bytecode-verification-error (%verify-fixture code :descriptor "(I)I"))
    (signals openldk::bytecode-verification-error (%verify-fixture code :descriptor "(I)I" :maps #(0 2 8 64 2)))
    (signals openldk::bytecode-verification-error (%verify-fixture code :descriptor "(I)I" :maps #(0 1 9)))
    (signals openldk::bytecode-verification-error (%verify-fixture code :descriptor "(I)I" :maps #(0 1 247)))
    (signals openldk::bytecode-verification-error (%verify-fixture code :descriptor "(I)I" :maps #(0 1 200)))
    (signals openldk::bytecode-verification-error (%verify-fixture code :descriptor "(I)I" :maps #(0 0 0)))))

(test concurrent-method-replacement
  "A dispatch during lazy replacement must not observe the removed method's gap."
  (let* ((base-name (gensym "DISPATCH-BASE-"))
         (child-name (gensym "DISPATCH-CHILD-"))
         (gf-name (gensym "DISPATCH-PROBE-"))
         (removed (sb-thread:make-semaphore))
         (attempting (sb-thread:make-semaphore))
         (completed (sb-thread:make-semaphore))
         (resume (sb-thread:make-semaphore))
         (reader nil) (writer nil) (result nil) (writer-error nil))
    (eval `(defclass ,base-name () ()))
    (eval `(defclass ,child-name (,base-name) ()))
    (let* ((gf (ensure-generic-function gf-name :generic-function-class 'openldk::java-generic-function
                                               :lambda-list '(object)))
           (base (find-class base-name))
           (child (find-class child-name))
           (object (make-instance child))
           (old (make-instance 'standard-method :lambda-list '(object) :specializers (list child)
                                                :function (lambda (args next) (declare (ignore args next)) :old)))
           (hook (make-instance 'standard-method :qualifiers '(:after) :lambda-list '(gf method)
                               :specializers (list (sb-mop:intern-eql-specializer gf) (find-class t))
                               :function (lambda (args next)
                                           (declare (ignore next))
                                           (when (eq (second args) old)
                                             (sb-thread:signal-semaphore removed)
                                             (sb-thread:wait-on-semaphore resume :timeout 5))))))
      (add-method gf (make-instance 'standard-method :lambda-list '(object) :specializers (list base)
                                                    :function (lambda (args next) (declare (ignore args next)) :base)))
      (add-method gf old)
      (is (eq :old (funcall gf object)))
      (add-method #'remove-method hook)
      (unwind-protect
           (progn
             (setf writer (sb-thread:make-thread
                           (lambda ()
                             (handler-case
                                 (add-method gf (make-instance 'standard-method :lambda-list '(object)
                                                        :specializers (list child)
                                                        :function (lambda (args next)
                                                                    (declare (ignore args next)) :new)))
                               (error (error) (setf writer-error error))))))
             (is (sb-thread:wait-on-semaphore removed :timeout 5))
             (setf reader (sb-thread:make-thread
                           (lambda ()
                             (sb-thread:signal-semaphore attempting)
                             (setf result (handler-case (funcall gf object) (error () :error)))
                             (sb-thread:signal-semaphore completed))))
             (is (sb-thread:wait-on-semaphore attempting :timeout 5))
             ;; The old dispatcher returns :BASE here. The atomic dispatcher
             ;; waits until the replacement is complete instead.
             (sb-thread:wait-on-semaphore completed :timeout 0.1)
             (sb-thread:signal-semaphore resume)
             (sb-thread:join-thread writer)
             (sb-thread:join-thread reader)
             (is (null writer-error))
             (is (eq :new result))
             (is (eq :new (funcall gf object))))
        (sb-thread:signal-semaphore resume)
        (when writer (sb-thread:join-thread writer))
        (when reader (sb-thread:join-thread reader))
        (remove-method #'remove-method hook)
        (fmakunbound gf-name)
        (setf (find-class child-name) nil (find-class base-name) nil)))))

(test native-allocation-keeps-occupied-pages
  "A native allocation must skip a mapped address without overwriting its data."
  (let ((openldk::*unsafe-memory-table* (make-hash-table :synchronized t))
        (openldk::*native-allocation-next-address* #x610000000000)
        (openldk::*native-allocation-free-blocks* nil)
        (first nil) (second nil))
    (unwind-protect
         (progn
           (setf first (openldk::%allocate-unsafe-memory 8))
           (setf (sb-sys:sap-ref-8 (sb-sys:int-sap first) 0) #x5a)
           ;; Force the next allocation to encounter this live mapping.
           (setf openldk::*native-allocation-next-address* first)
           (setf second (openldk::%allocate-unsafe-memory 8))
           (is (/= first second))
           (is (= #x5a (sb-sys:sap-ref-8 (sb-sys:int-sap first) 0)))
           (is (= 2 (hash-table-count openldk::*unsafe-memory-table*))))
      (when first (openldk::%free-unsafe-memory first))
      (when second (openldk::%free-unsafe-memory second)))))

(test floating-arithmetic-ieee-semantics
  (sb-int:with-float-traps-masked (:invalid :divide-by-zero :overflow :underflow :inexact)
    (dolist (entry '((single-float openldk::%java-fsub openldk::%java-fdiv
                     float-features:bits-single-float #x7fc12345 #x80000000)
                    (double-float openldk::%java-dsub openldk::%java-ddiv
                     float-features:bits-double-float #x7ff8123456789abc #x8000000000000000)))
      (destructuring-bind (type subtract divide from-bits nan-bits sign-bit) entry
        (let* ((self (compile nil `(lambda (x) (declare (type ,type x)) (,subtract x x))))
               (nan (funcall from-bits nan-bits))
               (infinity (funcall divide (coerce 1 type) (coerce 0 type))))
          (is (float-features:float-nan-p (funcall self nan)))
          (is (float-features:float-nan-p (funcall self infinity)))
          (is (float-features:float-nan-p
               (funcall divide (coerce 0 type) (coerce 0 type))))
          (is (minusp (float-sign (funcall divide (coerce 1 type) (funcall from-bits sign-bit))))))))))

#+x86-64
(test floating-arithmetic-unboxed-loops
  (dolist (entry '((double-float openldk::%java-dadd) (single-float openldk::%java-fadd)))
    (destructuring-bind (type operator) entry
      (let* ((one (coerce 1 type))
             (loop-function
               (compile nil `(lambda (n)
                               (declare (fixnum n) (optimize (speed 3) (safety 3)))
                               (let ((x ,one))
                                 (declare (type ,type x))
                                 (dotimes (i n) (setf x (,operator x ,one)))
                                 (= x ,(coerce 1000001 type)))))))
        (funcall loop-function 10)
        (let* ((before (sb-ext:get-bytes-consed))
               (correct (funcall loop-function 1000000))
               (bytes (- (sb-ext:get-bytes-consed) before)))
          (is (eq t correct))
          (is (< bytes 65536)))))))

(test interpreter-floating-arithmetic
  (sb-int:with-float-traps-masked (:invalid :divide-by-zero :overflow :underflow :inexact)
    (dolist (double-p '(nil t))
      (let ((nan (if double-p float-features:double-float-nan float-features:single-float-nan))
            (zero (if double-p 0d0 0f0)))
        (dolist (opcode '(98 102 106 110 114))
          (let ((class (%verification-fixture
                        (if double-p (vector 38 40 (1+ opcode) 175) (vector 34 35 opcode 174))
                        :descriptor (if double-p "(DD)D" "(FF)F"))))
            (openldk::verify-class-structure class)
            (openldk::verify-class-bytecode class)
            (is (float-features:float-nan-p
                 (openldk::%execute-verified-bytecode class 1 nil (list nan zero))))))))))

(test floating-remainder-exact-reference
  (is (= 201058 (openldk::%check-remainder-functions))))

(defun %owned-entry-probe (function &optional keep-owner native-p)
  "A loader-owned function graph with no Java runtime needed."
  (let* ((loader (openldk::make-ldk-class-loader))
         (class (make-instance 'openldk::<class> :name "OwnershipProbe" :ldk-loader loader))
         (symbol (gensym "OWNED-ENTRY")))
    (if native-p
        (let* ((key "OwnershipProbe.entry()I")
               (method (make-instance 'openldk::<method> :name "entry" :descriptor "()I" :access-flags #x109))
               (openldk::*native-overrides* (make-hash-table :test #'equal))
               (openldk::*methods-being-compiled* (make-hash-table :test #'equal)))
          (setf symbol (intern key (openldk::loader-package loader))
                (gethash key openldk::*native-overrides*) function)
          (openldk::%install-native-override class method key))
        (openldk::%install-owned-java-function class symbol function 0))
    (sb-sys:with-pinned-objects (class)
      (values (sb-ext:make-weak-pointer class) (fdefinition symbol)
              (when keep-owner (vector class))))))

(declaim (notinline %ownership-probe-live-p))
(defun %ownership-probe-live-p (reference)
  "Do not expose a class object to the assertion library's result storage."
  (not (null (sb-ext:weak-pointer-value reference))))

(test owned-method-entry-does-not-root-class
  (dolist (native-p '(nil t))
    (multiple-value-bind (reference function) (%owned-entry-probe (lambda () 42) nil native-p)
      ;; Keeping the process entry point itself must not keep a class alive.
      (dotimes (i 3) (sb-ext:gc :full t))
      (is (not (%ownership-probe-live-p reference)))
      (is (functionp function)))))

(test executing-owned-method-retains-class
  (dolist (native-p '(nil t))
    (let ((lock (sb-thread:make-mutex))
          (condition (sb-thread:make-waitqueue))
          (entered nil) (release nil))
      (multiple-value-bind (reference function keeper)
          (%owned-entry-probe
           (lambda ()
             (sb-thread:with-mutex (lock)
               (setf entered t)
               (sb-thread:condition-notify condition)
               (loop until release do (sb-thread:condition-wait condition lock)))
             42) t native-p)
        (let ((worker (sb-thread:make-thread function)))
          (unwind-protect
               (progn
                 (sb-thread:with-mutex (lock)
                   (loop until entered
                         do (unless (sb-thread:condition-wait condition lock :timeout 15)
                              (unless entered (error "Owned method did not enter")))))
                 (setf (aref keeper 0) nil)
                 (dotimes (i 3) (sb-ext:gc :full t))
                 (is (%ownership-probe-live-p reference)))
            (setf (aref keeper 0) nil)
            (sb-thread:with-mutex (lock)
              (setf release t)
              (sb-thread:condition-notify condition)))
          (is (= 42 (sb-thread:join-thread worker))))
        (dotimes (i 3) (sb-ext:gc :full t))
        (is (not (%ownership-probe-live-p reference)))))))

(declaim (notinline %monitor-cache-retention-probe))
(defun %monitor-cache-retention-probe ()
  "Return only weak references and a lookup result, without retaining test objects."
  (let* ((object (list :monitor-owner))
         (monitor (openldk::%get-monitor object)))
    (values (sb-ext:make-weak-pointer object) (sb-ext:make-weak-pointer monitor)
            (eq monitor (openldk::%get-monitor object)))))

(test monitor-shortcut-does-not-root-object
  ;; Keep the cache global, but discard the allocating thread's stack before GC.
  ;; A conservative stack slot must not be mistaken for a cache-owned root.
  (let ((worker (sb-thread:make-thread #'%monitor-cache-retention-probe)))
    (multiple-value-bind (object monitor same) (sb-thread:join-thread worker)
      (is (eql t same))
      (dotimes (i 3) (sb-ext:gc :full t))
      (is (not (%ownership-probe-live-p object)))
      (is (not (%ownership-probe-live-p monitor))))))

(declaim (notinline %loader-cleanup-probe))
(defun %loader-cleanup-probe ()
  "Return observable cleanup results and a weak class reference, without its stack."
  (let* ((package (make-package (symbol-name (gensym "UNLOADING-PROBE-")) :use '(:cl)))
         (class-name (intern "Probe" package))
         (function-name (intern "entry" package))
         (generic-name (gensym "SHARED-DISPATCH-"))
         (openldk::*unloaded-java-class-count* 0)
         (openldk::*condition-table* (make-hash-table))
         (resources (openldk::make-loader-resources :id -1 :package package
                      :classes (list class-name) :functions (list function-name))))
    (unwind-protect
         (progn
           (eval `(defclass ,class-name () ()))
           (eval `(defmethod ,generic-name ((object ,class-name)) (declare (ignore object)) 42))
           (setf (fdefinition function-name) (lambda () 42))
           (let ((class (find-class class-name)))
             (setf (gethash class openldk::*condition-table*) :condition)
             (openldk::%cleanup-loader-resources resources)
             (values (sb-ext:make-weak-pointer class)
                     (list (cons :function-unbound (not (fboundp function-name)))
                           (cons :class-unbound (null (find-class class-name nil)))
                           (cons :class-unlinked
                                 (notany (lambda (parent) (member class (sb-mop:class-direct-subclasses parent)))
                                         (sb-mop:class-direct-superclasses class)))
                           (cons :shared-dispatch-retained (not (null (fboundp generic-name))))
                           (cons :methods-removed (null (sb-mop:generic-function-methods (fdefinition generic-name))))
                           (cons :condition-removed (null (gethash class openldk::*condition-table*)))
                           (cons :package-deleted (null (package-name package)))
                           (cons :unloaded-count (= 1 openldk::*unloaded-java-class-count*))))))
      (when (fboundp generic-name) (fmakunbound generic-name))
      (when (fboundp function-name) (fmakunbound function-name))
      (when (package-name package) (openldk::%cleanup-loader-resources resources)))))

(test loader-cleanup-removes-runtime-definitions
  (let ((worker (sb-thread:make-thread #'%loader-cleanup-probe)))
    (multiple-value-bind (class checks) (sb-thread:join-thread worker)
      (dolist (check checks) (is (eql t (cdr check)) "Cleanup failed: ~A" (car check)))
      (dotimes (i 3) (sb-ext:gc :full t))
      (is (not (%ownership-probe-live-p class))))))

(defclass |TestAsInts$ByteBufferHandle| () ())
(defclass |TestIndirectVarHandle| () ())

(test varhandle-adaptation-lock-order
  "MethodHandle adaptation can enter ReferenceQueue monitors; it must not hold
  the memory lock that ReferenceQueue's volatile fields acquire in reverse order."
  (let* ((bindings
           (list
            (cons 'openldk::|hasInvokeExactBehavior()Z| (lambda (handle) (declare (ignore handle)) 0))
            (cons 'openldk::jstring #'identity)
            (cons 'openldk::|java/lang/invoke/VarHandle$AccessMode.valueFromMethodName(Ljava/lang/String;)Ljava/lang/invoke/VarHandle$AccessMode;|
                  #'identity)
            (cons 'openldk::|toMethodHandle(Ljava/lang/invoke/VarHandle$AccessMode;)Ljava/lang/invoke/MethodHandle;|
                  (lambda (handle mode)
                    (declare (ignore handle mode))
                    (is (not (sb-thread:holding-mutex-p openldk::*java-memory-lock*)))
                    (throw 'adapted :adapted)))
            (cons 'openldk::%vh-buffer-view
                  (lambda (&rest arguments)
                    (declare (ignore arguments))
                    (is (not (sb-thread:holding-mutex-p openldk::*java-memory-lock*)))
                    (openldk::%vh-run-op
                     :getadd
                     (lambda ()
                       (is (sb-thread:holding-mutex-p openldk::*java-memory-lock*))
                       98)
                     (lambda (value)
                       (is (sb-thread:holding-mutex-p openldk::*java-memory-lock*))
                       (is (= 99 value)))
                     '(1))))
            (cons 'openldk::%varhandle-invoke
                  (lambda (&rest arguments)
                    (declare (ignore arguments))
                    (is (not (sb-thread:holding-mutex-p openldk::*java-memory-lock*)))
                    88))))
         (saved (mapcar (lambda (entry)
                          (cons (car entry) (and (fboundp (car entry)) (fdefinition (car entry)))))
                        bindings))
         (operation (symbol-function 'openldk::|%VARHANDLE-OP-getAndAdd|))
         (buffer-handle (make-instance '|TestAsInts$ByteBufferHandle|)))
    (unwind-protect
         (progn
           (dolist (entry bindings) (setf (fdefinition (car entry)) (cdr entry)))
           (is (eq :adapted
                   (catch 'adapted
                     (funcall operation buffer-handle "(Ljava/nio/ByteBuffer;II)V" nil nil 0 1))))
           (is (= 98 (funcall operation buffer-handle "(Ljava/nio/ByteBuffer;II)I" nil nil 0 1)))
           (is (= 88 (funcall operation (make-instance '|TestIndirectVarHandle|) "()V" nil))))
      (dolist (entry saved)
        (if (cdr entry) (setf (fdefinition (car entry)) (cdr entry))
            (fmakunbound (car entry)))))))

(test array-allocation-storage-sizing
  "Use actual SBCL vector sizes, including padding and alignment, for every backing type."
  (dolist (type '(t (signed-byte 8) (unsigned-byte 16) (signed-byte 16)
                  (signed-byte 32) (signed-byte 64) single-float double-float
                  bit base-char character))
    (dolist (size '(0 1 2 3 7 8 15 16 31 64 257))
      (let ((vector (make-array size :element-type type)))
        (is (= (sb-ext:primitive-object-size vector)
               (openldk::%array-storage-bytes size type))))))
  (is (> (openldk::%array-storage-bytes (ash 1 30) t) (ash 1 33))))

(test array-allocation-error-unwinds-first
  "Translate storage exhaustion after leaving the allocator, reusing the prepared condition."
  (let* ((emergency (make-condition 'simple-error :format-control "emergency"))
         (openldk::*array-allocation-error* emergency)
         (cleaned nil)
         (caught
           (handler-case
               (openldk::with-array-allocation-recovery
                 (unwind-protect
                      (error 'sb-kernel::heap-exhausted-error :available 0 :requested 1024)
                   (setf cleaned t)))
             (simple-error (condition)
               (is (eql t cleaned))
               condition))))
    (is (eq emergency caught))))

(test word-long-lowering
  (let* ((x 'x) (n 'n)
         (body '(let ((x initial) (i 0))
                  (declare (type (signed-byte 64) x) (type (signed-byte 32) i))
                  (tagbody again
                     (when (>= i n) (go done))
                     (setf x (sb-c::mask-signed-field 64
                              (+ (sb-c::mask-signed-field 64 (* x 3)) 1)))
                     (incf i) (go again) done)
                  x))
         (lowered (openldk::%lower-word-longs body (list x)))
         (function (compile nil `(lambda (initial ,n)
                                   (declare (optimize (speed 3) (safety 1))
                                            (type (signed-byte 64) initial) (type (signed-byte 32) ,n))
                                   ,lowered))))
    (dolist (initial '(0 1 -1 9223372036854775807 -9223372036854775808 4611686018427387904))
      (dolist (count '(0 1 2 63 1000))
        (let ((expected initial))
          (dotimes (i count) (setf expected (sb-c::mask-signed-field 64 (+ (* expected 3) 1))))
          (is (= expected (funcall function initial count))))))
    (funcall function 1 100)
    (let ((before (sb-ext:get-bytes-consed)))
      (funcall function 1 100000)
      ;; Allow a return-value box/allocation-region accounting granularity;
      ;; a boxed accumulator would allocate megabytes instead.
      (is (< (- (sb-ext:get-bytes-consed) before) 65536)))
    (dolist (expression '((< x 0) (ash x -1) (truncate x 3) (rem x 3)
                          (coerce x 'double-float) (coerce x 'single-float)))
      (let* ((original `(let ((x initial)) (declare (type (signed-byte 64) x)) ,expression))
             (lowered (openldk::%lower-word-longs original '(x)))
             (before (compile nil `(lambda (initial) ,original)))
             (after (compile nil `(lambda (initial) ,lowered))))
        (dolist (initial '(0 1 -1 9223372036854775807 -9223372036854775808))
          (is (equalp (multiple-value-list (funcall before initial))
                      (multiple-value-list (funcall after initial)))))))))

(test primitive-entry-native-fallback
  (let* ((openldk::*context* (make-instance 'openldk::<context>))
         (openldk::*method-primitive-parameters* '((x . :int)))
         (form (openldk::%method-let '((x argument)) '(x)))
         (function (compile nil `(lambda (argument) ,form))))
    (is (= 42 (funcall function 42)))
    (is (eq t (funcall function t)))
    (is (null (funcall function nil)))
    (is (string= "opaque native value" (funcall function "opaque native value")))))

(test word-long-native-normalization
  (let* ((form '(let ((x 0))
                 (declare (type (signed-byte 64) x))
                 (setf x (openldk::%as-long argument))
                 x))
         (lowered (openldk::%lower-word-longs form '(x)))
         (function (compile nil `(lambda (argument) ,lowered))))
    (loop for argument in '(nil t #\A 18446744073709551615)
          for expected in '(0 1 65 -1)
          do (is (= expected (funcall function argument))))))

(test word-long-operand-order
  (let* ((form '(let ((x initial))
                 (declare (type (signed-byte 64) x))
                 (sb-c::mask-signed-field 64
                   (let ((value2 (progn (push :second events) operand))
                         (value1 (progn (push :first events) x)))
                     (* value1 value2)))))
         (lowered (openldk::%lower-word-longs form '(x)))
         (function (compile nil `(lambda (initial operand)
                                   (let ((events nil))
                                     (handler-case ,lowered (type-error () nil))
                                     events)))))
    ;; Invalid foreign operands must not fail before the remaining operand
    ;; has executed, even though canonical operands use modular words.
    (is (equal '(:first :second) (funcall function 1 'invalid)))))

(test safe-integer-leaf-handlers
  (let* ((safe '(openldk::with-java-conditions (:arrays nil)
                  (block done
                    (tagbody
                      (return-from done
                        (let ((a x) (b 1))
                          (sb-c::mask-signed-field 32 (+ a b))))))))
         (types '((x . (signed-byte 32))))
         (lowered (openldk::%omit-safe-leaf-handlers (list safe) types)))
    (is (eq 'block (caar lowered)))
    (is (= -2147483648
           (funcall (compile nil `(lambda (x) ,@lowered)) 2147483647)))
    ;; An unknown parameter must retain native error translation.
    (is (equal (list safe) (openldk::%omit-safe-leaf-handlers (list safe) nil)))
    (let ((openldk::*enable-safe-leaves* nil))
      (is (equal (list safe) (openldk::%omit-safe-leaf-handlers (list safe) types)))))
  (dolist (expression '((truncate x 0) (ash x y) (aref x 0)
                        (slot-value x 'field) (funcall x)
                        (openldk::%as-long unknown)))
    (let ((body `((openldk::with-java-conditions (:arrays nil) ,expression))))
      (is (equal body (openldk::%omit-safe-leaf-handlers body '((x . (signed-byte 64)))))))))

(defclass test-lf-member ()
  ((openldk::|flags| :initform (ash 6 24))
   (openldk::|clazz| :initform nil)))
(defclass test-lf-function ()
  ((openldk::|member| :initarg :member)
   (openldk::|resolvedHandle| :initform nil)))

(test compiled-lambda-dispatch-observes-replacement
  (let* ((member (make-instance 'test-lf-member))
         (function (make-instance 'test-lf-function :member member))
         (symbol (gensym "TARGET"))
         (openldk::*member-name-invoke-cache* (make-hash-table :test 'eq))
         (openldk::*last-member-plan* (cons nil nil))
         (entry (symbol-function (openldk::%lf-fixed-invoker 1))))
    (unwind-protect
         (progn
           (setf (symbol-function symbol) (lambda (x) (+ x 1))
                 (gethash member openldk::*member-name-invoke-cache*)
                 (list :static symbol nil #\I nil))
           (is (= 43 (funcall entry function 42)))
           ;; Fixed-arity dispatch must consult the symbol's current function.
           (setf (symbol-function symbol) (lambda (x) (+ x 7)))
           (is (= 49 (funcall entry function 42))))
      (fmakunbound symbol))))

(defclass test-lf-name ()
  ((openldk::|index| :initform 0)
   (openldk::|function$| :initarg :function)
   (openldk::|arguments| :initarg :arguments)))
(defclass test-lf-form ()
  ((openldk::|names| :initform nil)
   (openldk::|arity| :initform 0)
   (openldk::|result| :initform 0)))

(defun %weak-lambda-cache-fixture (table)
  (let* ((key (make-instance 'test-lf-form))
         (name (make-instance 'test-lf-name
                             :function (make-instance 'test-lf-function :member (make-instance 'test-lf-member))
                             :arguments (openldk::make-java-array-default :data (vector key)))))
    ;; A constant points back to the form. Capturing it in compiled code
    ;; would turn the weak-key cache into a permanent loader root.
    (setf (slot-value key 'openldk::|names|) (openldk::make-java-array-default :data (vector name)))
    (let ((function (openldk::%compile-lambda-form key (find-class 'test-lf-name))))
      (setf (gethash key table) function)
      (values (sb-ext:make-weak-pointer key) function))))

(test compiled-lambda-cache-does-not-root-forms
  (let ((table (make-hash-table :test 'eq :weakness :key :synchronized t)))
    (multiple-value-bind (reference function) (%weak-lambda-cache-fixture table)
      (dotimes (i 3) (sb-ext:gc :full t))
      (is (functionp function))
      (is (null (sb-ext:weak-pointer-value reference)))
      (is (zerop (hash-table-count table))))))

(test virtual-call-site-invalidation
  (let* ((base-name (gensym "SITE-BASE-"))
         (child-name (gensym "SITE-CHILD-"))
         (name (gensym "SITE-GF-"))
         (site (openldk::make-java-virtual-site :name name)))
    (eval `(defclass ,base-name () ()))
    (eval `(defclass ,child-name (,base-name) ()))
    (let* ((base (find-class base-name)) (child (find-class child-name))
           (object (make-instance child))
           (gf (ensure-generic-function name :generic-function-class 'openldk::java-generic-function
                                             :lambda-list '(object))))
      (flet ((method (class value &optional qualifiers)
               (make-instance 'standard-method :lambda-list '(object) :specializers (list class)
                              :qualifiers qualifiers
                              :function (lambda (args next) (declare (ignore args next)) value))))
        (unwind-protect
             (progn
               (add-method gf (method base :base))
               (is (eq :base (openldk::%invoke-virtual-at site object)))
               (let ((snapshot (openldk::java-virtual-site-entry site)))
                 (is (eq :base (openldk::%invoke-virtual-at site object)))
                 (is (eq snapshot (openldk::java-virtual-site-entry site))))
               (let ((specific (method child :child)))
                 (add-method gf specific)
                 (is (eq :child (openldk::%invoke-virtual-at site object)))
                 (remove-method gf specific)
                 (is (eq :base (openldk::%invoke-virtual-at site object))))
               (let ((around (method base :around '(:around))))
                 (add-method gf around)
                 (is (eq :around (openldk::%invoke-virtual-at site object)))
                 (remove-method gf around)
                 (is (eq :base (openldk::%invoke-virtual-at site object))))
               (add-method gf (method base :replacement))
               (is (eq :replacement (openldk::%invoke-virtual-at site object)))
               (setf (fdefinition name) (lambda (receiver) (declare (ignore receiver)) :plain))
               (is (eq :plain (openldk::%invoke-virtual-at site object)))
               (setf (fdefinition name) gf)
               (is (eq :replacement (openldk::%invoke-virtual-at site object))))
          (fmakunbound name)
          (setf (find-class child-name) nil (find-class base-name) nil))))))

(test concurrent-call-site-method-replacement
  "A dispatch during lazy replacement must not observe the removed method's gap."
  (let* ((base-name (gensym "DISPATCH-BASE-"))
         (child-name (gensym "DISPATCH-CHILD-"))
         (gf-name (gensym "DISPATCH-PROBE-"))
         (removed (sb-thread:make-semaphore))
         (attempting (sb-thread:make-semaphore))
         (completed (sb-thread:make-semaphore))
         (resume (sb-thread:make-semaphore))
         (reader nil) (writer nil) (result nil) (writer-error nil))
    (eval `(defclass ,base-name () ()))
    (eval `(defclass ,child-name (,base-name) ()))
    (let* ((gf (ensure-generic-function gf-name :generic-function-class 'openldk::java-generic-function
                                               :lambda-list '(object)))
           (base (find-class base-name))
           (child (find-class child-name))
           (object (make-instance child))
           (site (openldk::make-java-virtual-site :name gf-name))
           (old (make-instance 'standard-method :lambda-list '(object) :specializers (list child)
                                                :function (lambda (args next) (declare (ignore args next)) :old)))
           (hook (make-instance 'standard-method :qualifiers '(:after) :lambda-list '(gf method)
                               :specializers (list (sb-mop:intern-eql-specializer gf) (find-class t))
                               :function (lambda (args next)
                                           (declare (ignore next))
                                           (when (eq (second args) old)
                                             (sb-thread:signal-semaphore removed)
                                             (sb-thread:wait-on-semaphore resume :timeout 5))))))
      (add-method gf (make-instance 'standard-method :lambda-list '(object) :specializers (list base)
                                                    :function (lambda (args next) (declare (ignore args next)) :base)))
      (add-method gf old)
      (is (eq :old (openldk::%invoke-virtual-at site object)))
      (add-method #'remove-method hook)
      (unwind-protect
           (progn
             (setf writer (sb-thread:make-thread
                           (lambda ()
                             (handler-case
                                 (add-method gf (make-instance 'standard-method :lambda-list '(object)
                                                        :specializers (list child)
                                                        :function (lambda (args next)
                                                                    (declare (ignore args next)) :new)))
                               (error (error) (setf writer-error error))))))
             (is (sb-thread:wait-on-semaphore removed :timeout 5))
             (setf reader (sb-thread:make-thread
                           (lambda ()
                             (sb-thread:signal-semaphore attempting)
                             (setf result (handler-case (openldk::%invoke-virtual-at site object) (error () :error)))
                             (sb-thread:signal-semaphore completed))))
             (is (sb-thread:wait-on-semaphore attempting :timeout 5))
             ;; The old dispatcher returns :BASE here. The atomic dispatcher
             ;; waits until the replacement is complete instead.
             (sb-thread:wait-on-semaphore completed :timeout 0.1)
             (sb-thread:signal-semaphore resume)
             (sb-thread:join-thread writer)
             (sb-thread:join-thread reader)
             (is (null writer-error))
             (is (eq :new result))
             (is (eq :new (openldk::%invoke-virtual-at site object))))
        (sb-thread:signal-semaphore resume)
        (when writer (sb-thread:join-thread writer))
        (when reader (sb-thread:join-thread reader))
        (remove-method #'remove-method hook)
        (fmakunbound gf-name)
        (setf (find-class child-name) nil (find-class base-name) nil)))))

(test small-integer-method-recognition
  (multiple-value-bind (expression success)
      (openldk::%small-integer-method-expression #(26 27 96 172) '(a b))
    (is (eq t success))
    (let ((function (compile nil `(lambda (a b) ,expression))))
      (is (= 42 (funcall function 19 23)))
      (is (= -2147483648 (funcall function 2147483647 1)))))
  (dolist (bytes '(#(26 27 108 172) #(26 153 0 4 172) #(184 0 1 172)
                   #(178 0 1 172) #(26) #(96 172) #(29 172) #(26 172 0)))
    (is (null (nth-value 1 (openldk::%small-integer-method-expression bytes '(a b))))))
  (multiple-value-bind (expression success)
      (openldk::%small-integer-method-expression #(26 145 172) '(a))
    (is (eq t success))
    (is (= -1 (funcall (compile nil `(lambda (a) ,expression)) 255)))))

(test inline-static-target-observes-replacement
  (let* ((name (gensym "INLINE-TARGET"))
         (method (list :method))
         (original (lambda (a) a))
         (replacement (lambda (a) (+ a 1)))
         (site (openldk::make-java-inline-site :name name :class method :method method))
         (openldk::*java-inline-functions* (make-hash-table :test 'eq :weakness :key)))
    (unwind-protect
         (progn
           (setf (fdefinition name) original
                 (gethash original openldk::*java-inline-functions*) (sb-ext:make-weak-pointer method))
           (is (eq t (openldk::%inline-static-target-p site)))
           (is (eq t (openldk::%inline-static-target-p site)))
           (setf (fdefinition name) replacement)
           (is (null (openldk::%inline-static-target-p site)))
           ;; A new Java compilation of the same method may refresh a site.
           (setf (gethash replacement openldk::*java-inline-functions*) (sb-ext:make-weak-pointer method))
           (is (eq t (openldk::%inline-static-target-p site))))
      (fmakunbound name))))

(defun %array-loop-fixture ()
  '(tagbody
      (setf sum (openldk::%as-int 0))
      (setf i (openldk::%as-int 0))
    again
      (progn
        (when (>= i (openldk::%as-int
                     (let ((arrayref a))
                       (if (openldk::java-array-p arrayref)
                           (length (openldk::java-array-data arrayref))
                           (openldk::%array-length-slow arrayref)))))
          (go done)))
      (setf sum (openldk::%as-int
                 (sb-c::mask-signed-field 32
                   (+ sum (let ((index i) (arrayref b))
                            (aref (the (simple-array (signed-byte 32) (*))
                                       (openldk::java-array-data arrayref)) index))))))
      (setf i (openldk::%as-int (sb-c::mask-signed-field 32 (+ i 1))))
      (go again)
    done
      (return-from result sum)))

(defun %compile-array-loop-fixture (form)
  (compile nil `(lambda (a b)
                  (block result
                    (let ((i 0) (sum 0))
                      (declare (type (signed-byte 32) i sum))
                      ,form)))))

(defun %array-loop-outcome (function a b)
  (handler-case (list :value (funcall function a b))
    (error (condition) (list :error (type-of condition)))))

(test guarded-array-loop-boundaries
  (let ((original (%array-loop-fixture)))
    (multiple-value-bind (lowered success) (openldk::%version-array-tagbody original)
      (is (eq t success))
      (let ((before (%compile-array-loop-fixture original))
            (after (%compile-array-loop-fixture lowered)))
        (dolist (a-length '(0 1 2 3 5))
          (let ((a (openldk::make-java-array-default :data (make-array a-length))))
            (dolist (b-length '(0 1 2 3 5))
              (let ((b (openldk::make-java-array-default
                        :data (make-array b-length :element-type '(signed-byte 32) :initial-element 7))))
                (is (equal (%array-loop-outcome before a b) (%array-loop-outcome after a b)))))
            ;; No iteration may touch a missing second array.
            (when (zerop a-length) (is (= 0 (funcall after a nil)))))))))
  (let* ((original (%array-loop-fixture))
         (load '(let ((index i) (arrayref b))
                  (aref (the (simple-array (signed-byte 32) (*)) (openldk::java-array-data arrayref)) index)))
         (empty (openldk::make-java-array-default :data (make-array 0 :element-type '(signed-byte 32)))))
    ;; Prefix and suffix array reads retain bounds checks, even when the
    ;; loop itself has no iterations and the fast range guard succeeds.
    (dolist (form (list (cons 'tagbody (cons `(setf sum ,load) (cddr original)))
                       (append (butlast original) (list `(return-from result ,load)))))
      (multiple-value-bind (lowered success) (openldk::%version-array-tagbody form)
        (is (eq t success))
        (let ((before (%array-loop-outcome (%compile-array-loop-fixture form) empty empty))
              (after (%array-loop-outcome (%compile-array-loop-fixture lowered) empty empty)))
          (is (eq :error (first before)))
          (is (equal before after)))))))

(test guarded-array-loop-rejections
  (let* ((form (%array-loop-fixture))
         (forms (cdr form)))
    (dolist (bad (list '(tagbody a b)
                      (append (butlast form) '((go again)))
                      (cons 'tagbody (cons '(setf a b) forms))
                      (cons 'tagbody (cons '(funcall native-hook) forms))))
      (is (equal bad (openldk::%version-array-tagbody bad))))))

(defun %primitive-arithmetic-outcome (function a b)
  (handler-case
      (let ((value (funcall function a b)))
        (if (typep value 'double-float)
            (list :double (float-features:double-float-bits value))
            (list :value value)))
    (error (condition) (list :error (type-of condition)))))

(test guarded-primitive-result-arithmetic
  (dolist (body '((sb-c::mask-signed-field 32 (+ value1 value2))
                  (sb-c::mask-signed-field 64 (* value1 value2))
                  (openldk::%long-word (- value1 value2))
                  (openldk::%as-double (+ value1 value2))))
    (let* ((form `(let ((value2 b) (value1 a)) ,body))
           (lowered (openldk::%specialize-primitive-result-arithmetic form nil))
           (before (compile nil `(lambda (a b) ,form)))
           (after (compile nil `(lambda (a b) ,lowered))))
      (is (not (equal form lowered)))
      (dolist (a '(0 1 -1 2147483647 -2147483648 9223372036854775807 -9223372036854775808
                   1208925819614629174706176 1.5d0 -0.0d0 2.5f0 1/3 :opaque))
        (dolist (b '(1 -1 3 2.5d0 :opaque))
          (is (equalp (%primitive-arithmetic-outcome before a b)
                      (%primitive-arithmetic-outcome after a b))))))))

(test primitive-result-operands-evaluated-once
  (let* ((form '(let ((value2 (progn (push :second events) b))
                     (value1 (progn (push :first events) a)))
                 (sb-c::mask-signed-field 32 (+ value1 value2))))
         (lowered (openldk::%specialize-primitive-result-arithmetic form nil))
         (function (compile nil `(lambda (a b)
                                   (let ((events nil))
                                     (handler-case ,lowered (type-error () nil))
                                     events)))))
    (is (equal '(:first :second) (funcall function 1 2)))
    (is (equal '(:first :second) (funcall function :opaque 2)))))

(test primitive-result-proven-types-skip-guards
  (let ((form '(let ((value2 b) (value1 a))
                 (declare (type (signed-byte 32) value1 value2))
                 (sb-c::mask-signed-field 32 (+ value1 value2)))))
    (is (equal form (openldk::%specialize-primitive-result-arithmetic form nil)))
    (let ((openldk::*enable-primitive-result-arithmetic* nil))
      (is (eq form (openldk::%specialize-primitive-result-arithmetic form nil))))))

(test primitive-result-mutations-and-shadowing
  ;; Initializer facts and branch tests cease to prove a type after a write.
  ;; Nested binding forms must not inherit facts about shadowed names.
  (dolist (form '((let ((value1 0) (value2 b))
                   (setf value1 a)
                   (sb-c::mask-signed-field 32 (+ value1 value2)))
                 (let ((value1 0) (value2 b))
                   (setf value1 a)
                   (sb-c::mask-signed-field 64 (* value1 value2)))
                 (let ((value1 0) (value2 b))
                   (setf value1 a)
                   (openldk::%long-word (+ value1 value2)))
                 (let ((value1 0.0d0) (value2 b))
                   (setf value1 a)
                   (openldk::%as-double (+ value1 value2)))
                 (let ((value1 0) (value2 b))
                   (if (typep value1 '(signed-byte 32))
                       (progn (setf value1 a)
                              (sb-c::mask-signed-field 32 (+ value1 value2)))
                       0))
                 (let ((value1 0) (value2 b))
                   (labels ((leaf (value1)
                              (sb-c::mask-signed-field 32 (+ value1 value2))))
                     (leaf a)))
                 (let ((value1 0) (value2 b))
                   (flet ((change () (setf value1 a))) (change))
                   (sb-c::mask-signed-field 32 (+ value1 value2)))
                 (let ((value1 0) (value2 b))
                   (handler-case (error "test")
                     (error (value1)
                       (sb-c::mask-signed-field 32 (+ value1 value2)))))))
    (let* ((lowered (openldk::%specialize-primitive-result-arithmetic form nil))
           (before (compile nil `(lambda (a b) (declare (ignorable a b)) ,form)))
           (after (compile nil `(lambda (a b) (declare (ignorable a b)) ,lowered))))
      (dolist (a '(1 1208925819614629174706176 3/4 :opaque))
        (dolist (b '(1 1.25d0 :opaque))
          (is (equalp (%primitive-arithmetic-outcome before a b)
                      (%primitive-arithmetic-outcome after a b))))))))

(test adjacent-linkage-check-elision
  (let* ((owner (make-instance 'openldk::<class>))
         (site `(load-time-value (openldk::%member-linkage-site ,owner 7)))
         (other `(load-time-value (openldk::%member-linkage-site ,owner 8)))
         (form `(tagbody
                  (openldk::%with-member-constraints ,site (initialize-target))
                  (setf result (openldk::%with-class-access caller target
                                 (openldk::%with-member-constraints ,site (invoke-target))))))
         (expected `(tagbody
                      (openldk::%with-member-constraints ,site (initialize-target))
                      (setf result (openldk::%with-class-access caller target (invoke-target))))))
    (is (equal expected (openldk::%elide-adjacent-member-checks form)))
    (dolist (barrier (list 'entry '(when condition (go elsewhere)) '(unrelated-call)))
      (let ((guarded `(tagbody ,(second form) ,barrier ,(third form))))
        (is (equal guarded (openldk::%elide-adjacent-member-checks guarded)))))
    (let ((different `(tagbody ,(second form) (openldk::%with-member-constraints ,other (invoke-target)))))
      (is (equal different (openldk::%elide-adjacent-member-checks different))))
    (let ((openldk::*enable-redundant-linkage-elision* nil))
      (is (eq form (openldk::%elide-adjacent-member-checks form))))))
