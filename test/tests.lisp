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
