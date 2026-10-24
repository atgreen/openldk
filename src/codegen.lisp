;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: OPENLDK; Base: 10 -*-
;;;
;;; Copyright (C) 2024, 2025  Anthony Green <green@moxielogic.com>
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


(in-package :openldk)

;;; Lower the IR produced by bc-to-ir.lisp to Lisp forms.  Each IR node
;;; class has a CODEGEN method returning an <EXPRESSION> whose CODE slot
;;; is the Lisp form to evaluate; codegen-block stitches these together
;;; per basic block, wrapping try/catch scopes as HANDLER-CASE.

(defgeneric codegen (insn context)
  (:documentation "Return an <EXPRESSION> containing the Lisp form that
implements IR node INSN, generating operand code recursively."))

(declaim (notinline %java-slot-value (setf %java-slot-value)))

(defun %java-slot-value (object slot-name)
  "Read a dynamically defined Java field without baking in an SBCL global slot accessor."
  (slot-value (%non-null object) slot-name))

(defun (setf %java-slot-value) (value object slot-name)
  "Write a dynamically defined Java field without baking in an SBCL global slot accessor."
  (setf (slot-value (%non-null object) slot-name) value))

(defun %non-null (object)
  "OBJECT, or a NullPointerException if it is null."
  (or object (%throw '|java/lang/NullPointerException|)))

;;; Field accesses cache, per access site, the layout of the last object
;;; seen and where that layout keeps the slot: a fixnum index into the
;;; instance's slot vector, or for a static field (a class-allocated slot
;;; of the +static-X+ instance) the class's slot cell.  A hit costs a
;;; layout compare and one load, against SLOT-VALUE's lookup by name.
;;; Java classes are plain STANDARD-CLASSes with no slot protocol methods,
;;; so going around SLOT-VALUE is safe.  Each CACHE is a site's own
;;; (LOAD-TIME-VALUE (LIST NIL)); its car is replaced whole, never
;;; mutated, so other threads see a consistent (LAYOUT . LOCATION).
;;; The access stays an ordinary call: inlined at every site, it made
;;; SBCL's compile time, which dominates most programs' run, 20% longer.

(declaim (inline %slot-cache-location))

(defun %slot-cache-location (object cache)
  "OBJECT's slot location from the site CACHE, or NIL on a miss."
  (let ((entry (car cache)))
    (and entry
         (sb-kernel:%instancep object)
         (eq (sb-kernel:%instance-layout object) (car entry))
         (cdr entry))))

(defun %slot-cache-fill (object slot-name cache)
  "Remember where OBJECT's layout keeps SLOT-NAME in the site CACHE."
  (when (and (sb-kernel:%instancep object) (typep object 'standard-object))
    (let ((layout (sb-kernel:%instance-layout object)))
      (unless (sb-kernel:layout-invalid layout)
        (let ((slotd (find slot-name (sb-mop:class-slots (class-of object))
                           :key #'sb-mop:slot-definition-name)))
          (when slotd
            (setf (car cache)
                  (cons layout (sb-mop:slot-definition-location slotd)))))))))

(defun %cached-slot-value-miss (object slot-name cache)
  (prog1 (slot-value (%non-null object) slot-name)
    (%slot-cache-fill object slot-name cache)))

(defun %set-cached-slot-value-miss (value object slot-name cache)
  (prog1 (setf (slot-value (%non-null object) slot-name) value)
    (%slot-cache-fill object slot-name cache)))

(defun %cached-slot-value (object slot-name cache)
  "SLOT-VALUE of the Java field SLOT-NAME of OBJECT, via the site CACHE."
  (let* ((location (%slot-cache-location object cache))
         (value (typecase location
                  (fixnum (sb-mop:standard-instance-access object location))
                  (cons (cdr location))
                  (t sb-pcl:+slot-unbound+))))
    (if (eq value sb-pcl:+slot-unbound+)
        (%cached-slot-value-miss object slot-name cache)
        value)))

(defun (setf %cached-slot-value) (value object slot-name cache)
  (let ((location (%slot-cache-location object cache)))
    (typecase location
      (fixnum (setf (sb-mop:standard-instance-access object location) value))
      (cons (setf (cdr location) value))
      (t (%set-cached-slot-value-miss value object slot-name cache)))))

(defclass/std <expression> ()
  ((insn
    code
    expression-type)))

(defmethod print-object ((expr <expression>) out)
  (print-unreadable-object (expr out :type t)
    (format out "{~A : ~A}" (insn expr) (code expr))))

(defun trace-insn (insn code)
  (if *debug-x*
      (list 'progn
            (list 'format t (format nil "~&; x[~A]~%" (address insn)))
            code)
      code))

(defmethod codegen ((insn ir-literal) context)
  (declare (ignore context))
  (make-instance '<expression>
                 :insn insn
                 :code (slot-value insn 'value)
                 :expression-type (slot-value insn 'type)))

(defmethod codegen ((insn ir-array-literal) context)
  (let* ((values (slot-value insn 'value))
         (size (length values)))
    (cond
      ;; Large constant array - precompute the Java strings NOW and embed reference
      ((and (vectorp values) (> size 50))
       (let* ((java-strings (coerce (loop for val across values
                                          collect (cond
                                                    ((null val) nil)
                                                    ((stringp val) (ijstring val))
                                                    (t val)))
                                   'vector)))
         (make-instance '<expression>
                        :insn insn
                        ;; Reference the pre-computed vector directly
                        :code `(make-java-array :component-class ,(component-class insn)
                                                :element-type ',(%component-element-type (component-class insn))
                                                :initial-contents (copy-seq ',java-strings))
                        :expression-type (slot-value insn 'type))))
      ;; Small constant array or vectorp - inline it
      ((vectorp values)
       (let ((wrapped-values (loop for val across values
                                   collect (cond
                                             ((null val) nil)
                                             ((stringp val) `(ijstring ,val))
                                             (t val)))))
         (make-instance '<expression>
                        :insn insn
                        :code `(make-java-array :component-class ,(component-class insn)
                                                :element-type ',(%component-element-type (component-class insn))
                                                :initial-contents (vector ,@wrapped-values))
                        :expression-type (slot-value insn 'type))))
      ;; IR nodes - must codegen each one
      (t
       (let ((codegenned-values (mapcar (lambda (ir-val)
                                          (code (codegen ir-val context)))
                                        values)))
         (make-instance '<expression>
                        :insn insn
                        :code `(make-java-array :component-class ,(component-class insn)
                                                :element-type ',(%component-element-type (component-class insn))
                                                :initial-contents (vector ,@codegenned-values))
                        :expression-type (slot-value insn 'type)))))))

(defmethod codegen ((insn ir-int-literal) context)
  (declare (ignore context))
  (make-instance '<expression>
                 :insn insn
                 :code (unsigned-to-signed-integer (slot-value insn 'value))
                 :expression-type :INTEGER))

(defmethod codegen ((insn ir-double-literal) context)
  (declare (ignore context))
  (make-instance '<expression>
                 :insn insn
                 :code (coerce (slot-value insn 'value) 'double-float)
                 :expression-type :DOUBLE))

(defmethod codegen ((insn ir-long-literal) context)
  (declare (ignore context))
  (make-instance '<expression>
                 :insn insn
                 :code (unsigned-to-signed-long (slot-value insn 'value))
                 :expression-type :LONG))

(defmethod codegen ((insn ir-string-literal) context)
  (declare (ignore context))
  (make-instance '<expression>
                 :insn insn
                 :code (ijstring (slot-value insn 'value))
                 :expression-type :REFERENCE))

(defparameter *trace-npe* nil
  "When true, %make-throwable prints a Lisp backtrace each time it constructs a
   NullPointerException. Diagnostic aid for pinning the OpenLDK throw site of an
   NPE that escapes into a Clojure compile (see ldk-c7c).")
(defparameter *npe-counter* 0)

(defun %make-throwable (throwable-class &optional message)
  (when (and *trace-npe*
             (eq throwable-class '|java/lang/NullPointerException|))
    ;; Print only each frame's function designator (first element), never its
    ;; arguments: printing a java-array arg re-enters %STRINGIZE-ARRAY which can
    ;; itself throw NPE, cascading into nested errors.
    (format *error-output* "~&;;NPE-CREATE #~D~%" (incf *npe-counter*))
    (ignore-errors
      (let ((*print-level* 3) (*print-length* 4))
        (dolist (frame (sb-debug:list-backtrace :count 25))
          (ignore-errors
            (format *error-output* "  ~S~%"
                    (if (consp frame) (first frame) frame))))))
    (force-output *error-output*))
  ;; Nothing may have loaded the exception class yet: a ClassCastException
  ;; from a checkcast in a method with no handler for it, say.
  (unless (find-class throwable-class nil)
    (classload (symbol-name throwable-class)))
  (let ((throwable (make-instance throwable-class)))
    (if message
        (|<init>(Ljava/lang/String;)V| throwable (jstring message))
        (|<init>()V| throwable))
    throwable))

(defun %throw (throwable-class &optional message)
  "Throw a new THROWABLE-CLASS, initialized with MESSAGE if given.  Generated
code calls this rather than inlining the construction at every site."
  (error (%lisp-condition (%make-throwable throwable-class message))))

;;; WITH-JAVA-CONDITIONS' handlers.

(defun %array-index-error->java (e)
  (%throw-array-index-error (type-error-datum e)))

(defun %memory-fault->java (e)
  (declare (ignore e))
  (%throw '|java/lang/NullPointerException|))

(defun %type-error->java (e)
  ;; LDK_DEBUG=e: the Lisp error a NullPointerException stands for.
  (when *debug-exceptions*
    (format *error-output* "~&Lisp ~A becomes NullPointerException: ~A~%" (type-of e) e)
    (sb-debug:print-backtrace :count 25 :stream *error-output*))
  (%throw '|java/lang/NullPointerException|))

(defun %undefined-function->java (e)
  ;; A call to a Java method with no definition -- in practice a native
  ;; method OpenLDK doesn't implement -- is UnsatisfiedLinkError, as on a
  ;; JVM, so Java code can catch it instead of the Lisp error unwinding
  ;; past every Java handler.
  (let ((name (cell-error-name e)))
    (when (%java-method-name-p name)
      (error (%lisp-condition (%make-unsatisfied-link-error name))))))

(defun %lisp-error->java (e)
  "Signal the Java throwable that E, a Lisp error signalled in Java code,
stands for, or decline: the handler WITH-JAVA-CONDITIONS establishes."
  (typecase e
    (sb-sys:memory-fault-error (%memory-fault->java e))
    (type-error (%type-error->java e))
    (undefined-function (%undefined-function->java e))))

(defun %lisp-error->java/arrays (e)
  "%LISP-ERROR->JAVA, with an array index error an
ArrayIndexOutOfBoundsException rather than, as a TYPE-ERROR, a
NullPointerException."
  (if (typep e 'sb-int:invalid-array-index-error)
      (%array-index-error->java e)
      (%lisp-error->java e)))

(defmethod codegen ((insn ir-aastore) context)
  (with-slots (arrayref index value) insn
    (make-instance '<expression>
                   :insn insn
                   :code `(let ((value ,(code (codegen value context)))
                                (index ,(code (codegen index context)))
                                (arrayref ,(code (codegen arrayref context))))
                            (%aastore arrayref index value)))))

;;; Primitive array loads and stores compile to an inline AREF on the
;;; array's specialized data vector (see arrays.lisp).  A null array
;;; reference fails JAVA-ARRAY-DATA's type check and an out-of-range index
;;; fails AREF's bounds check; WITH-JAVA-CONDITIONS turns these into
;;; NullPointerException and ArrayIndexOutOfBoundsException.

(defun %array-data-form (arrayref-form element-type)
  `(the (simple-array ,element-type (*)) (java-array-data ,arrayref-form)))

(defun %codegen-array-store (insn element-type value-form context)
  "Codegen a primitive xASTORE into an ELEMENT-TYPE array, storing
VALUE-FORM (which may refer to VALUE, the operand).  Compact code calls
the generic (SETF JAREF), which normalizes the value itself."
  (with-slots (arrayref index value) insn
    (make-instance '<expression>
                   :insn insn
                   :code `(let ((value ,(code (codegen value context)))
                                (index ,(code (codegen index context)))
                                (arrayref ,(code (codegen arrayref context))))
                            ,(if (compact-p context)
                                 '(setf (jaref arrayref index) value)
                                 `(setf (aref ,(%array-data-form 'arrayref element-type) index)
                                        ,value-form))))))

(defmethod codegen ((insn ir-iastore) context)
  (%codegen-array-store insn '(signed-byte 32) '(sb-c::mask-signed-field 32 value) context))

(defmethod codegen ((insn ir-lastore) context)
  (%codegen-array-store insn '(signed-byte 64) '(sb-c::mask-signed-field 64 value) context))

(defmethod codegen ((insn ir-fastore) context)
  (%codegen-array-store insn 'single-float '(float value 1f0) context))

(defmethod codegen ((insn ir-sastore) context)
  (%codegen-array-store insn '(signed-byte 16) '(sb-c::mask-signed-field 16 value) context))

(defmethod codegen ((insn ir-bastore) context)
  ;; Shared by byte[] and boolean[]; a boolean may arrive as T/NIL.
  (%codegen-array-store insn '(signed-byte 8)
                        '(sb-c::mask-signed-field 8 (case value ((nil) 0) ((t) 1) (t value)))
                        context))

(defmethod codegen ((insn ir-dastore) context)
  (%codegen-array-store insn 'double-float '(float value 1d0) context))

(defmethod codegen ((insn ir-castore) context)
  (%codegen-array-store insn '(unsigned-byte 16)
                        '(ldb (byte 16 0) (if (characterp value) (char-code value) value))
                        context))

(defmethod codegen ((insn ir-idiv) context)
  ;; Java integer division truncates toward zero (not floor toward -infinity)
  (make-instance '<expression>
                 :insn insn
                 :code `(let ((value2 ,(code (codegen (value2 insn) context)))
                              (value1 ,(code (codegen (value1 insn) context))))
                          (if (eql value2 0)
                              (%throw '|java/lang/ArithmeticException| "/ by zero")
                              (sb-c::mask-signed-field 32 (truncate value1 value2))))
                 :expression-type :INTEGER))

(defmethod codegen ((insn ir-ldiv) context)
  ;; Java long division truncates toward zero (not floor toward -infinity)
  (make-instance '<expression>
                 :insn insn
                 :code `(let ((value2 ,(code (codegen (value2 insn) context)))
                              (value1 ,(code (codegen (value1 insn) context))))
                          (if (eql value2 0)
                              (%throw '|java/lang/ArithmeticException| "/ by zero")
                              (sb-c::mask-signed-field 64 (truncate value1 value2))))
                 :expression-type :LONG))

(defun %codegen-binop (insn operator jtype context)
  "Codegen a plain two-operand op: (OPERATOR value1 value2), typed JTYPE."
  (make-instance '<expression>
                 :insn insn
                 :code (list 'let (list (list 'value2 (code (codegen (value2 insn) context)))
                                        (list 'value1 (code (codegen (value1 insn) context))))
                             (list operator 'value1 'value2))
                 :expression-type jtype))

(defun %codegen-integer-binop (insn operator context)
  "Codegen an int op with Java 32-bit wrap-around overflow semantics.
MASK-SIGNED-FIELD is SBCL's modular arithmetic: with typed operands the
whole form compiles to a single machine instruction."
  (make-instance '<expression>
                 :insn insn
                 :code `(let ((value2 ,(code (codegen (value2 insn) context)))
                              (value1 ,(code (codegen (value1 insn) context))))
                          (sb-c::mask-signed-field 32 (,operator value1 value2)))
                 :expression-type :INTEGER))

(defun %codegen-long-binop (insn operator context)
  "Codegen a long op with Java 64-bit wrap-around overflow semantics."
  (make-instance '<expression>
                 :insn insn
                 :code `(let ((value2 ,(code (codegen (value2 insn) context)))
                              (value1 ,(code (codegen (value1 insn) context))))
                          (sb-c::mask-signed-field 64 (,operator value1 value2)))
                 :expression-type :LONG))

(defmacro %define-binop-codegen-methods (&rest opcodes)
  "Define CODEGEN methods for two-operand arithmetic and logical opcodes.
Each entry is (IR-CLASS OPERATOR JTYPE KIND) where KIND is :WRAPPING for
ops that must wrap around on 32/64-bit overflow, or :PLAIN for ops whose
result always stays in range (bitwise ops, floats, doubles)."
  `(progn
     ,@(mapcar (lambda (opcode)
                 (destructuring-bind (ir-class operator jtype kind) opcode
                   (if (eq kind :WRAPPING)
                       (ecase jtype
                         (:INTEGER
                          `(defmethod codegen ((insn ,ir-class) context)
                             (%codegen-integer-binop insn ,operator context)))
                         (:LONG
                          `(defmethod codegen ((insn ,ir-class) context)
                             (%codegen-long-binop insn ,operator context))))
                       `(defmethod codegen ((insn ,ir-class) context)
                          (%codegen-binop insn ,operator ,jtype context)))))
               opcodes)))

(%define-binop-codegen-methods
  (ir-dmul '* :DOUBLE :PLAIN)
  (ir-dadd '+ :DOUBLE :PLAIN)
  (ir-dsub '- :DOUBLE :PLAIN)
  (ir-fadd '+ :FLOAT :PLAIN)
  (ir-fdiv '/ :FLOAT :PLAIN)
  (ir-fmul '* :FLOAT :PLAIN)
  (ir-fsub '- :FLOAT :PLAIN)
  (ir-iadd '+ :INTEGER :WRAPPING)
  (ir-imul '* :INTEGER :WRAPPING)
  (ir-isub '- :INTEGER :WRAPPING)
  (ir-ladd '+ :LONG :WRAPPING)
  (ir-lmul '* :LONG :WRAPPING)
  (ir-lsub '- :LONG :WRAPPING)
  (ir-iand 'logand :INTEGER :PLAIN)
  (ir-ior 'logior :INTEGER :PLAIN)
  (ir-ixor 'logxor :INTEGER :PLAIN)
  (ir-land 'logand :LONG :PLAIN)
  (ir-lxor 'logxor :LONG :PLAIN))

(defmethod codegen ((insn ir-lor) context)
  ;; Unlike LAND/LXOR, LOR masks its operands to their unsigned 64-bit
  ;; representation before combining; some producers hand it non-canonical
  ;; (unsigned) long values.  The result is a signed long again: left
  ;; unsigned, FLAG | n with FLAG's sign bit set compared as positive (Kawa's
  ;; SimpleVector.isVerySimple, info < 0).
  (make-instance '<expression>
                 :insn insn
                 :code `(let ((result (logior (logand ,(code (codegen (value1 insn) context)) #xFFFFFFFFFFFFFFFF)
                                              (logand ,(code (codegen (value2 insn) context)) #xFFFFFFFFFFFFFFFF))))
                          (if (> result 9223372036854775807)
                              (- result 18446744073709551616)
                              result))
                 :expression-type :LONG))

(defmethod codegen ((insn ir-array-length) context)
  (make-instance '<expression>
                 :insn insn
                 :code `(let ((arrayref ,(code (codegen (slot-value insn 'arrayref) context))))
                          (if (java-array-p arrayref)
                              (length (java-array-data arrayref))
                              (%array-length-slow arrayref)))
                 :expression-type :INTEGER))

;;; Typed variables.  Codegen records where each value assigned to a
;;; local or stack variable comes from; a variable whose values all have
;;; one primitive category is declared with that Lisp type (see %TYPED-LET
;;; in openldk.lisp), letting SBCL keep it unboxed and open-code its
;;; arithmetic.  Only values JIT'd code computes itself count -- arithmetic,
;;; conversions, numeric literals, primitive array loads -- plus copies of
;;; other typed variables.  Field reads, call results and parameters don't:
;;; natives store all sorts in Java fields (an int fd field holds a Lisp
;;; stream).  Every assignment to a declared variable also goes through a
;;; %AS-<category> normalizer, which compiles to nothing when the value's
;;; type is already known.

(defun %jtype-category (jtype)
  "Variable type category for the Java stack type JTYPE, or NIL."
  (case jtype
    ((:INTEGER :CHAR :SHORT :BYTE :BOOLEAN) :int)
    (:LONG :long)
    (:FLOAT :float)
    (:DOUBLE :double)
    (t nil)))

(defun %note-variable-source (symbol source context)
  "Record that SYMBOL is assigned a value from SOURCE: a category
keyword, (:COPY . VARIABLE), or NIL for a value of unknown type."
  (push source (gethash symbol (variable-sources context))))

(defun %value-source (expression)
  "The variable source (see %NOTE-VARIABLE-SOURCE) of EXPRESSION's value."
  (let ((code (code expression))
        (insn (insn expression)))
    (cond
      ((and code (symbolp code) (not (eq code t))) (cons :copy code))
      ((typep insn '(and (or ir-binop ir-unop ir-literal ir-array-length
                             ir-array-index ir-instanceof)
                         (not ir-i2l)))
       (%jtype-category (expression-type expression)))
      (t nil))))

(defmethod codegen ((insn ir-assign) context)
  (with-slots (lvalue rvalue) insn
    (let ((place (code (codegen lvalue context)))
          (value (codegen rvalue context)))
      (when (symbolp place)
        (%note-variable-source place (%value-source value) context))
      (make-instance '<expression>
                     :insn insn
                     :code (list 'setf place (code value))))))

(declaim (inline %as-int %as-long %as-float %as-double))

(defun %as-primitive-slow (value bits)
  "VALUE as a BITS-wide Java integer: chars become code units, booleans 0/1."
  (sb-c::mask-signed-field
   bits
   (typecase value
     (integer value)
     (character (char-code value))
     (null 0)
     ((eql t) 1)
     (t (internal-error "~S assigned to a ~D-bit integer variable" value bits)))))

(defun %as-int (value)
  (if (typep value '(signed-byte 32)) value (the (signed-byte 32) (%as-primitive-slow value 32))))

(defun %as-long (value)
  (if (typep value '(signed-byte 64)) value (the (signed-byte 64) (%as-primitive-slow value 64))))

(defun %as-float-slow (value)
  (float value 1f0))

(defun %as-double-slow (value)
  (float value 1d0))

;;; The slow paths stay out of line: inlined, SBCL can derive an operand
;;; type FLOAT rejects (Lisp's SQRT of a negative double is complex) and
;;; print a compile-time warning into the program's output.
(defun %as-float (value)
  (if (typep value 'single-float) value (the single-float (%as-float-slow value))))

(defun %as-double (value)
  (if (typep value 'double-float) value (the double-float (%as-double-slow value))))

(defmethod codegen ((insn ir-call-dynamic) context)
  (with-slots (class) insn
    (make-instance '<expression>
                   :insn insn
                   :code `(let ((lookup (%make-java-instance "java/lang/invoke/MethodHandles$Lookup")))
                            (|<init>(Ljava/lang/Class;)V| lookup ,class)
                            (unimplemented "IR-CALL-DYNAMIC codegen")))))

(defun %class-method-name-table (ldk-class)
  "Hash set of LDK-CLASS's own methods' lispized name+descriptor
strings, built on first use.  A class's method vector never changes
after load, so the table is cached on the <class> object."
  (or (method-name-table ldk-class)
      (setf (method-name-table ldk-class)
            (let ((table (make-hash-table :test #'equal)))
              (map nil (lambda (method)
                         (setf (gethash (lispize-method-name
                                         (format nil "~A~A" (name method) (descriptor method)))
                                        table)
                                t))
                   (or (methods ldk-class) #()))
              table))))

(defun %find-field-declaring-class (class field-name &optional loader)
  "Bin-name of the class/interface that actually declares FIELD-NAME,
searching CLASS then its superclass and superinterfaces.  A static field
access initializes only the declaring class (JLS 12.4.1), not the class
named in the reference."
  (let ((ldk-class (%get-ldk-class-by-bin-name class t loader)))
    (when ldk-class
      (if (some (lambda (f) (and f (string= (name f) field-name)))
                (coerce (fields ldk-class) 'list))
          class
          (loop for parent in (remove nil (cons (super ldk-class)
                                                (coerce (interfaces ldk-class) 'list)))
                for result = (%find-field-declaring-class parent field-name loader)
                when result return result)))))

(defun %field-declaring-ir-class (ref-ir-class fieldname)
  "ir-class for the class that DECLARES FIELDNAME, searching up from the
referenced class REF-IR-CLASS.  Falls back to REF-IR-CLASS when it declares
the field itself or the declarer can't be resolved.  Used so a static field
access initializes the declaring class, not the referenced one."
  (let* ((ref-name (name (ir-class-class ref-ir-class)))
         (decl-name (%find-field-declaring-class ref-name fieldname)))
    (if (and decl-name (not (string= decl-name ref-name)))
        (let ((lc (%resolve-class-in-context decl-name)))
          (if lc (make-instance 'ir-class :class lc) ref-ir-class))
        ref-ir-class)))

(defun %declares-default-method-p (ldk-class)
  "True when LDK-CLASS is an interface declaring a default method (a
non-abstract, non-static instance method).  Per JLS 12.4.2, initializing a
class/interface initializes only those superinterfaces that declare a
default method -- not every superinterface."
  (and (interface-p ldk-class)
       (some (lambda (m)
               (and m (not (abstract-p m)) (not (static-p m))
                    (let ((n (name m)))
                      (not (or (string= n "<clinit>") (string= n "<init>"))))))
             (coerce (methods ldk-class) 'list))))

(defun %find-declaring-class (class method-name &optional loader)
  "Find the class that declares METHOD-NAME, searching class hierarchy.
   LOADER is the <ldk-class-loader> to use for class lookups."
  (let ((ldk-class (%get-ldk-class-by-bin-name class t loader)))
    (when ldk-class
      (if (gethash method-name (%class-method-name-table ldk-class))
          class
          (loop for parent in (remove nil (cons (super ldk-class) (coerce (interfaces ldk-class) 'list)))
                for result = (%find-declaring-class parent method-name loader)
                when result return result)))))

(defun %linkto-varargs-symbol (full-name method-sym)
  "METHOD-SYM, or for a MethodHandle.linkTo<Kind> intrinsic with a
signature OpenLDK defines no fixed-arity variant for (primitive arguments,
as in DirectMethodHandle$Holder.invokeStatic's), its varargs variant: all
take the method's arguments followed by the MemberName."
  (let ((prefix "java/lang/invoke/MethodHandle.linkTo"))
    (if (and (starts-with? prefix full-name)
             (not (fboundp method-sym)))
        (intern (format nil "~A(Ljava/lang/invoke/MemberName;)"
                        (subseq full-name 0 (position #\( full-name)))
                :openldk)
        method-sym)))

(defmethod codegen ((insn ir-call-static-method) context)
  (with-slots (class method-name args return-type) insn
    (make-instance '<expression>
                   :insn insn
                   :code (let* ((loader (slot-value context 'ldk-loader))
                                ;; Ensure class is loaded before package lookup
                                (_ (%resolve-class-in-context class))
                                (declaring-class (or (%find-declaring-class class method-name loader) class))
                                (pkg (class-package declaring-class loader))
                                (full-name (format nil "~A.~A" declaring-class method-name))
                                ;; Use static-method-symbol to check :openldk first (for native methods)
                                (method-sym (%linkto-varargs-symbol
                                             full-name (static-method-symbol full-name pkg)))
                                (nargs (length args))
                                (call (cond
                                        ((eq nargs 0)
                                         (list method-sym))
                                        ((eq nargs 1)
                                         (list method-sym (code (codegen (car args) context))))
                                        (t
                                         (cons method-sym
                                               (reverse (mapcar (lambda (a) (code (codegen a context))) args)))))))
                           call)
                   :expression-type return-type)))

(defun %codegen-array-load (insn element-type jtype context)
  "Codegen a primitive xALOAD from an ELEMENT-TYPE array, typed JTYPE."
  (with-slots (index arrayref) insn
    (make-instance '<expression>
                   :insn insn
                   :code `(let ((index ,(code (codegen index context)))
                                (arrayref ,(code (codegen arrayref context))))
                            ,(if (compact-p context)
                                 '(jaref arrayref index)
                                 `(aref ,(%array-data-form 'arrayref element-type) index)))
                   :expression-type jtype)))

(defmethod codegen ((insn ir-caload) context)
  (%codegen-array-load insn '(unsigned-byte 16) :CHAR context))

(defmethod codegen ((insn ir-iaload) context)
  (%codegen-array-load insn '(signed-byte 32) :INTEGER context))

(defmethod codegen ((insn ir-saload) context)
  (%codegen-array-load insn '(signed-byte 16) :SHORT context))

(defmethod codegen ((insn ir-laload) context)
  (%codegen-array-load insn '(signed-byte 64) :LONG context))

(defmethod codegen ((insn ir-baload) context)
  (%codegen-array-load insn '(signed-byte 8) :BYTE context))

(defmethod codegen ((insn ir-daload) context)
  (%codegen-array-load insn 'double-float :DOUBLE context))

(defmethod codegen ((insn ir-faload) context)
  (%codegen-array-load insn 'single-float :FLOAT context))

(defmethod codegen ((insn ir-aaload) context)
  (with-slots (index arrayref) insn
    (make-instance '<expression>
                   :insn insn
                   :code `(let ((index ,(code (codegen index context)))
                                (arrayref ,(code (codegen arrayref context))))
                            (jaref arrayref index))
                   :expression-type :REFERENCE)))

(defmethod codegen ((insn ir-checkcast) context)
  (with-slots (classname) insn
    ;; Ensure class is loaded before package lookup (unless it's an array type)
    (unless (eq (char classname 0) #\[)
      (%resolve-class-in-context classname))
    (make-instance '<expression>
                   :insn insn
                   :code (progn
                           (if (eq (char classname 0) #\[)
                             `(let ((objref ,(code (codegen (objref insn) context))))
                                (when objref
                                  (unless (%array-assignable-to-p objref ,classname)
                                    (%throw '|java/lang/ClassCastException|))))
                             `(let ((objref ,(code (codegen (objref insn) context))))
                                (when objref
                                  (unless (or (not (find-class (quote ,(intern (slot-value insn 'classname) (class-package classname))) nil))
                                              (typep objref (quote ,(intern (slot-value insn 'classname) (class-package classname))))
                                              (%native-type-castable-p objref ,classname))
                                    (%throw '|java/lang/ClassCastException|))))))
                   :expression-type nil)))

(defun %self-referential-hidden-class-p (compiled-name classname)
  "True when COMPILED-NAME is the uniquified registration name of a hidden or
anonymous class whose own constant pool still refers to itself by CLASSNAME.
%classload-from-stream registers such classes as <orig>/hidden-N or
<orig>/anonymous-class-N, so a class constant naming <orig> from inside the
class is a self-reference to the class currently being compiled."
  (and compiled-name classname
       (let ((prefix (concatenate 'string classname "/")))
         (and (> (length compiled-name) (length prefix))
              (string= prefix compiled-name :end2 (length prefix))
              (let ((suffix (subseq compiled-name (length prefix))))
                (or (%string-prefix-p "hidden-" suffix)
                    (%string-prefix-p "anonymous-class-" suffix)))))))

(defun %string-prefix-p (prefix string)
  (and (>= (length string) (length prefix))
       (string= prefix string :end2 (length prefix))))

(defmethod codegen ((insn ir-class) context)
  (let* ((classname (slot-value (slot-value insn 'class) 'name))
         (loader (slot-value context 'ldk-loader)))
    ;; Ensure the referenced class is loaded before resolving it (unless it's
    ;; an array type).  Mirrors ir-checkcast/ir-new; without this, a class that
    ;; is referenced but not yet loaded (e.g. java/lang/Enum from
    ;; clojure/lang/RT.<clinit>) resolves to NIL and (java-class NIL) aborts
    ;; the JIT compile.
    (let ((ldk-class (or (%get-ldk-class-by-bin-name classname t loader)
                         ;; Resolved through the loader, which can delegate to
                         ;; another user loader that defined it (one Groovy
                         ;; script's class, referenced from the next).
                         (and (plusp (length classname)) (not (eql (char classname 0) #\[))
                              (%resolve-class-in-context classname))
                         ;; A hidden/anonymous class references itself by its
                         ;; original name, but it is registered under a
                         ;; uniquified name; resolve that to the class being
                         ;; compiled rather than failing with (java-class NIL).
                         (let ((cc (slot-value context 'class)))
                           (when (and cc (%self-referential-hidden-class-p (name cc) classname))
                             cc)))))
      (make-instance '<expression>
                     :insn insn
                     ;; A class that isn't there (Spring's DefaultConversionService
                     ;; references kotlin/text/Regex behind a KotlinDetector check)
                     ;; throws NoClassDefFoundError only if the ldc executes.
                     :code (if ldk-class
                               (java-class ldk-class)
                               `(%throw '|java/lang/NoClassDefFoundError|
                                        ,(substitute #\. #\/ classname)))
                     :expression-type :REFERENCE))))

(defmethod codegen ((insn ir-branch-target) context)
  (declare (ignore context))
  (let ((expr (make-instance '<expression>
                             :insn insn
                             :code (intern (format nil "branch-target-~A" (slot-value insn 'index)))
                             :expression-type nil)))
    expr))

(defmethod codegen ((insn ir-irem) context)
  (make-instance '<expression>
                 :insn insn
                 :code (list 'handler-case
                             (list 'let (list (list 'value2 (code (codegen (value2 insn) context)))
                                              (list 'value1 (code (codegen (value1 insn) context))))
                                   (list 'rem 'value1 'value2))
                             (list 'division-by-zero ()
                                   (list '%throw (list 'quote '|java/lang/ArithmeticException|) "/ by zero")))
                 :expression-type :INTEGER))

(defmethod codegen ((insn ir-lrem) context)
  (make-instance '<expression>
                 :insn insn
                 :code (list 'handler-case
                             (list 'let (list (list 'value2 (code (codegen (value2 insn) context)))
                                              (list 'value1 (code (codegen (value1 insn) context))))
                                   (list 'rem 'value1 'value2))
                             (list 'division-by-zero ()
                                   (list '%throw (list 'quote '|java/lang/ArithmeticException|) "/ by zero")))
                 :expression-type :LONG))

(defmethod codegen ((insn ir-fdiv) context)
  ;; FIXME - handle all weird conditions
  (make-instance '<expression>
                 :insn insn
                 :code `(let ((value2 ,(code (codegen (value2 insn) context)))
                              (value1 ,(code (codegen (value1 insn) context))))
                          (if (eq value2 0.0)
                              (cond
                                ((< value1 0.0) float-features:single-float-negative-infinity)
                                ((> value1 0.0) float-features:single-float-positive-infinity)
                                (t float-features:single-float-nan))
                              (/ value1 value2)))
                 :expression-type :FLOAT))

(defmethod codegen ((insn ir-ddiv) context)
  ;; FIXME - handle all weird conditions
  (make-instance '<expression>
                 :insn insn
                 :code `(let ((value2 ,(code (codegen (value2 insn) context)))
                              (value1 ,(code (codegen (value1 insn) context))))
                          (if (eq value2 0.0d0)
                              (cond
                                ((< value1 0.0d0) float-features:double-float-negative-infinity)
                                ((> value1 0.0d0) float-features:double-float-positive-infinity)
                                (t float-features:double-float-nan))
                              (/ value1 value2)))
                 :expression-type :DOUBLE))

(defmethod codegen ((insn ir-frem) context)
  (make-instance '<expression>
                 :insn insn
                 :code `(let ((value2 ,(code (codegen (value2 insn) context)))
                              (value1 ,(code (codegen (value1 insn) context))))
                          (cond
                            ;; If either value1 or value2 is NaN, the result is NaN
                            ((or (float-features:float-nan-p value1)
                                 (float-features:float-nan-p value2))
                             float-features:single-float-nan)

                            ;; If the dividend is an infinity or the divisor is a zero or both, the result is NaN
                            ((or (float-features:float-infinity-p value1)
                                 (zerop value2)
                                 (and (float-features:float-infinity-p value1)
                                      (zerop value2)))
                             float-features:single-float-nan)

                            ;; If the dividend is finite and the divisor is an infinity, the result equals the dividend
                            ((and (not (float-features:float-infinity-p value1))
                                  (float-features:float-infinity-p value2))
                             value1)

                            ;; If the dividend is a zero and the divisor is finite, the result equals the dividend
                            ((and (zerop value1)
                                  (not (float-features:float-infinity-p value2)))
                             value1)

                            ;; In the remaining cases, compute the remainder
                            (t (let ((q (truncate value1 value2)))
                                 (- value1 (* value2 q))))))
                 :expression-type :FLOAT))

(defmethod codegen ((insn ir-drem) context)
  (make-instance '<expression>
                 :insn insn
                 :code `(let ((value2 ,(code (codegen (value2 insn) context)))
                              (value1 ,(code (codegen (value1 insn) context))))
                          (cond
                            ;; If either value1 or value2 is NaN, the result is NaN
                            ((or (float-features:float-nan-p value1)
                                 (float-features:float-nan-p value2))
                             float-features:double-float-nan)

                            ;; If the dividend is an infinity or the divisor is a zero or both, the result is NaN
                            ((or (float-features:float-infinity-p value1)
                                 (zerop value2)
                                 (and (float-features:float-infinity-p value1)
                                      (zerop value2)))
                             float-features:double-float-nan)

                            ;; If the dividend is finite and the divisor is an infinity, the result equals the dividend
                            ((and (not (float-features:float-infinity-p value1))
                                  (float-features:float-infinity-p value2))
                             value1)

                            ;; If the dividend is a zero and the divisor is finite, the result equals the dividend
                            ((and (zerop value1)
                                  (not (float-features:float-infinity-p value2)))
                             value1)

                            ;; In the remaining cases, compute the remainder
                            (t (let ((q (truncate value1 value2)))
                                 (- value1 (* value2 q))))))
                 :expression-type :DOUBLE))

(defmethod codegen ((insn ir-fcmpg) context)
  (make-instance '<expression>
                 :insn insn
                 :code (list 'let (list (list 'value2 (code (codegen (value2 insn) context)))
                                        (list 'value1 (code (codegen (value1 insn) context))))
                             (list 'if (list 'or (list 'float-features:float-nan-p 'value1) (list 'float-features:float-nan-p 'value2))
                                   1
                                   (list 'if (list '> 'value1 'value2)
                                         1
                                         (list 'if (list '< 'value1 'value2)
                                               -1
                                               0))))
                 :expression-type :INTEGER))

(defmethod codegen ((insn ir-dcmpg) context)
  (make-instance '<expression>
                 :insn insn
                 :code (list 'let (list (list 'value2 (code (codegen (value2 insn) context)))
                                        (list 'value1 (code (codegen (value1 insn) context))))
                             (list 'if (list 'or (list 'float-features:float-nan-p 'value1) (list 'float-features:float-nan-p 'value2))
                                   1
                                   (list 'if (list '> 'value1 'value2)
                                         1
                                         (list 'if (list '< 'value1 'value2)
                                               -1
                                               0))))
                 :expression-type :INTEGER))

(defmethod codegen ((insn ir-fcmpl) context)
  (make-instance '<expression>
                 :insn insn
                 :code (list 'let (list (list 'value2 (code (codegen (value2 insn) context)))
                                        (list 'value1 (code (codegen (value1 insn) context))))
                             (list 'if (list 'or (list 'float-features:float-nan-p 'value1) (list 'float-features:float-nan-p 'value2))
                                   -1
                                   (list 'if (list '> 'value1 'value2)
                                         1
                                         (list 'if (list '< 'value1 'value2)
                                               -1
                                               0))))
                 :expression-type :INTEGER))

(defmethod codegen ((insn ir-dcmpl) context)
  (make-instance '<expression>
                 :insn insn
                 :code `(let ((value2 ,(code (codegen (value2 insn) context)))
                              (value1 ,(code (codegen (value1 insn) context))))
                          (if (or (float-features:float-nan-p value1) (float-features:float-nan-p value2))
                              -1
                              (if (> value1 value2)
                                  1
                                  (if (< value1 value2)
                                      -1
                                      0))))
                 :expression-type :INTEGER))

(defmethod codegen ((insn ir-i2f) context)
  (make-instance '<expression>
                 :insn insn
                 :code `(coerce ,(code (codegen (value insn) context))
                                'single-float)
                 :expression-type :FLOAT))

(defmethod codegen ((insn ir-i2d) context)
  (make-instance '<expression>
                 :insn insn
                 :code `(coerce ,(code (codegen (value insn) context))
                                'double-float)
                 :expression-type :DOUBLE))

(defmethod codegen ((insn ir-l2d) context)
  (make-instance '<expression>
                 :insn insn
                 :code `(coerce ,(code (codegen (value insn) context))
                                'double-float)
                 :expression-type :DOUBLE))

(defmethod codegen ((insn ir-f2d) context)
  (make-instance '<expression>
                 :insn insn
                 :code `(coerce ,(code (codegen (value insn) context))
                                'double-float)
                 :expression-type :DOUBLE))

(defmethod codegen ((insn ir-d2f) context)
  (make-instance '<expression>
                 :insn insn
                 :code `(coerce ,(code (codegen (value insn) context))
                                'single-float)
                 :expression-type :FLOAT))

(defmethod codegen ((insn ir-d2i) context)
  (make-instance '<expression>
                 :insn insn
                 :code `(let ((value ,(code (codegen (value insn) context))))
                          (unsigned-to-signed-integer
                           (logand
                            (cond
                              ((float-features:float-nan-p value) 0)
                              ((float-features:float-infinity-p value)
                               (if (plusp value) #x7FFFFFFF #x80000000))
                              (t (floor value)))
                            #xFFFFFFFF)))
                 :expression-type :INTEGER))

(defmethod codegen ((insn ir-d2l) context)
  (make-instance '<expression>
                 :insn insn
                 :code `(let ((value ,(code (codegen (value insn) context))))
                          (unsigned-to-signed-long
                           (logand
                            (cond
                              ((float-features:float-nan-p value) 0)
                              ((float-features:float-infinity-p value)
                               (if (plusp value) #x7FFFFFFFFFFFFFFF #x8000000000000000))
                              (t (floor value)))
                            #xFFFFFFFFFFFFFFFF)))
                 :expression-type :LONG))

(defmethod codegen ((insn ir-l2f) context)
  (make-instance '<expression>
                 :insn insn
                 :code `(coerce ,(code (codegen (value insn) context))
                                'single-float)
                 :expression-type :FLOAT))

(defmethod codegen ((insn ir-lneg) context)
  (make-instance '<expression>
                 :insn insn
                 :code `(sb-c::mask-signed-field 64 (- ,(code (codegen (value insn) context))))
                 :expression-type :LONG))

(defmethod codegen ((insn ir-ineg) context)
  (make-instance '<expression>
                 :insn insn
                 :code `(sb-c::mask-signed-field 32 (- ,(code (codegen (value insn) context))))
                 :expression-type :INTEGER))

(defmethod codegen ((insn ir-dneg) context)
  (make-instance '<expression>
                 :insn insn
                 :code `(- ,(code (codegen (value insn) context)))
                 :expression-type :DOUBLE))

(defmethod codegen ((insn ir-fneg) context)
  (make-instance '<expression>
                 :insn insn
                 :code `(- ,(code (codegen (value insn) context)))
                 :expression-type :FLOAT))

(defmethod codegen ((insn ir-i2l) context)
  (make-instance '<expression>
                 :insn insn
                 :code (code (codegen (value insn) context))
                 :expression-type :LONG))

(defmethod codegen ((insn ir-i2b) context)
  ;; FIXME - char width, also maybe use :INTEGER?
  (make-instance '<expression>
                 :insn insn
                 :code `(sb-c::mask-signed-field 8 ,(code (codegen (value insn) context)))
                 :expression-type :BYTE))

(defmethod codegen ((insn ir-i2s) context)
  ;; FIXME - maybe use :INTEGER?
  (make-instance '<expression>
                 :insn insn
                 :code `(sb-c::mask-signed-field 16 ,(code (codegen (value insn) context)))
                 :expression-type :SHORT))

(defmethod codegen ((insn ir-l2i) context)
  ;; FIXME - review this
  (make-instance '<expression>
                 :insn insn
                 :code `(sb-c::mask-signed-field 32 ,(code (codegen (value insn) context)))
                 :expression-type :INTEGER))

(defmethod codegen ((insn ir-i2c) context)
  ;; FIXME - char width, also maybe use :INTEGER?
  (make-instance '<expression>
                 :insn insn
                 :code (list 'logand (code (codegen (value insn) context)) #xFFFF)
                 :expression-type :CHAR))

(defmethod codegen ((insn ir-f2i) context)
  (make-instance '<expression>
                 :insn insn
                 :code `(let ((value ,(code (codegen (value insn) context))))
                          (unsigned-to-signed-integer
                           (logand
                            (cond
                              ((float-features:float-nan-p value) 0)
                              ((float-features:float-infinity-p value)
                               (if (plusp value) #x7FFFFFFF #x80000000))
                              (t (floor value)))
                            #xFFFFFFFF)))
                 :expression-type :INTEGER))

(defmethod codegen ((insn ir-f2l) context)
  (make-instance '<expression>
                 :insn insn
                 :code `(let ((value ,(code (codegen (value insn) context))))
                          (unsigned-to-signed-long
                           (logand
                            (cond
                              ((float-features:float-nan-p value) 0)
                              ((float-features:float-infinity-p value)
                               (if (plusp value) #x7FFFFFFFFFFFFFFF #x8000000000000000))
                              (t (floor value)))
                            #xFFFFFFFFFFFFFFFF)))
                 :expression-type :LONG))

(defmethod codegen ((insn ir-iinc) context)
  ;; Local variables use :openldk package - they don't need per-loader isolation
  (with-slots (index const) insn
    (let ((local-var (intern (format nil "local-~A" index) :openldk)))
      (%note-variable-source local-var :int context)
      (make-instance '<expression>
                     :insn insn
                     :code `(setf ,local-var
                                  (sb-c::mask-signed-field 32 (+ ,local-var ,const)))))))

(defmethod codegen ((insn ir-if-acmpeq) context)
  (with-slots (offset value1 value2) insn
    (make-instance '<expression>
                   :insn insn
                   :code (list 'when (list 'eq (code (codegen value1 context)) (code (codegen value2 context)))
                               (list 'go (intern (format nil "branch-target-~A" offset)))))))

(defun %codegen-ir-if-xcmpne (insn context)
  (with-slots (offset value1 value2) insn
    (make-instance '<expression>
                   :insn insn
                   :code (list 'when (list 'not (list 'eq (code (codegen value1 context)) (code (codegen value2 context))))
                               (list 'go (intern (format nil "branch-target-~A" offset)))))))

(defmethod codegen ((insn ir-if-icmpne) context)
  (%codegen-ir-if-xcmpne insn context))

(defmethod codegen ((insn ir-if-acmpne) context)
  (%codegen-ir-if-xcmpne insn context))

(defmacro %define-if-icmp<cond>-codegen-methods (&rest opcodes)
  `(progn
     ,@(mapcar (lambda (opcode)
                 (let ((ir-class (car opcode))
                       (comparison (cadr opcode)))
                   `(defmethod codegen ((insn ,ir-class) context)
                      (with-slots (offset value1 value2) insn
                        (make-instance '<expression>
                                       :insn insn
                                       :code (list 'progn
                                                   (list 'when (list ',comparison (code (codegen value1 context)) (code (codegen value2 context)))
                                                         (list 'go (intern (format nil "branch-target-~A" offset))))))))))
               opcodes)))

(%define-if-icmp<cond>-codegen-methods
  (ir-if-icmpeq eq)
  (ir-if-icmpge >=)
  (ir-if-icmpgt >)
  (ir-if-icmple <=)
  (ir-if-icmplt <))

(defmacro %define-if<cond>-codegen-methods (&rest opcodes)
  `(progn
     ,@(mapcar (lambda (opcode)
                 (let ((ir-class (car opcode))
                       (comparison (cadr opcode)))
                   `(defmethod codegen ((insn ,ir-class) context)
                      (with-slots (offset value) insn
                        (make-instance '<expression>
                                       :insn insn
                                       :code (list 'when (list ',comparison (code (codegen value context)) 0)
                                                   (list 'go (intern (format nil "branch-target-~A" offset)))))))))
               opcodes)))

(%define-if<cond>-codegen-methods
 (ir-ifeq eq)
 (ir-ifge >=)
 (ir-ifgt >)
 (ir-ifle <=)
 (ir-iflt <))

(defmethod codegen ((insn ir-ifnull) context)
  (with-slots (offset value) insn
    (make-instance '<expression>
                   :insn insn
                   :code (list 'when (list 'null (code (codegen value context)))
                               (list 'go (intern (format nil "branch-target-~A" offset)))))))

(defmethod codegen ((insn ir-ifne) context)
  (with-slots (offset value) insn
    (make-instance '<expression>
                   :insn insn
                   :code (list 'when (list 'not (list 'eq (code (codegen value context)) 0))
                               (list 'go (intern (format nil "branch-target-~A" offset)))))))

(defun %handler-entry-flag (handler-block)
  "Code for a HANDLER-CASE clause that jumps to HANDLER-BLOCK: tell a
handler that normal code also reaches that it was entered for an exception."
  (when (gethash (address handler-block) (shared-handler-table *context*))
    (list '(setf |handler-entry| t))))

(defmethod codegen ((insn ir-condition-exception) context)
  ;; condition-cache and objref are runtime symbols in :openldk
  (let ((exception (list 'slot-value '|condition-cache| (list 'quote 'openldk::|objref|))))
    (make-instance '<expression>
                   :insn insn
                   :code (if-let ((normal (normal-value insn)))
                           ;; HANDLER-ENTRY is set by the clause that jumped here.
                           `(if |handler-entry|
                                (progn (setf |handler-entry| nil) ,exception)
                                ,(code (codegen normal context)))
                           exception))))

(defmethod codegen ((insn ir-ifnonnull) context)
  (with-slots (offset value) insn
    (make-instance '<expression>
                   :insn insn
                   :code (list 'when (list 'not (list 'null (code (codegen value context))))
                               (list 'go (intern (format nil "branch-target-~A" offset)))))))

(defun %receiver (obj)
  "OBJ as the receiver of a virtual call: boxed if it is a native Lisp
value, and a NullPointerException if it is null."
  (typecase obj
    (null (%throw '|java/lang/NullPointerException|))
    ((or number character) (%box-if-native obj))
    (t obj)))

(defun %box-if-native (obj)
  "Auto-box native Lisp values to Java wrapper objects for virtual method dispatch.
   Native integers become java/lang/Long, floats become java/lang/Float, etc."
  (cond
    ((integerp obj)
     (let ((boxed (%make-java-instance "java/lang/Long")))
       (setf (slot-value boxed '|value|) obj)
       boxed))
    ((typep obj 'single-float)
     (let ((boxed (%make-java-instance "java/lang/Float")))
       (setf (slot-value boxed '|value|) obj)
       boxed))
    ((typep obj 'double-float)
     (let ((boxed (%make-java-instance "java/lang/Double")))
       (setf (slot-value boxed '|value|) obj)
       boxed))
    ((characterp obj)
     (let ((boxed (%make-java-instance "java/lang/Character")))
       (setf (slot-value boxed '|value|) obj)
       boxed))
    (t obj)))

(defun %array-assignable-to-p (objref target-descriptor)
  "True when array OBJREF's runtime type is assignable to the array type
named by TARGET-DESCRIPTOR (a JVM array descriptor, e.g. \"[I\", \"[[I\",
or \"[Lp/C;\"), per JLS array covariance/identity: the object's component
type must be assignable to the target's component type.  Primitive
component types must match exactly (short[] is not an int[])."
  (and (typep objref 'java-array)
       (let ((obj-comp (%array-component-class objref))
             (target (subseq target-descriptor 1)))
         (if (char= (char target 0) #\L)
             ;; A class or interface component: checked by name, without
             ;; loading the target.  Resolving it ran Java class loaders
             ;; (Quarkus's RunnerClassLoader checks its own arrays while
             ;; loading classes, and recursed), and when OBJ-COMP is a
             ;; subtype the target is loaded already.
             (and obj-comp
                  (%component-subtype-p obj-comp (subseq target 1 (1- (length target)))))
             (let ((target-comp (%bin-type-name-to-class target)))
               (and obj-comp target-comp
                    ;; target-comp.isAssignableFrom(obj-comp): obj's component is a
                    ;; subtype of (or equal to) the target's component.
                    (eql 1 (|isAssignableFrom(Ljava/lang/Class;)Z| target-comp obj-comp))))))))

(defun %component-subtype-p (java-class target-name)
  "True when JAVA-CLASS, an array's component type, is the class or
interface TARGET-NAME (a binary name) or a subtype of it."
  (cond
    ((eql 1 (|isPrimitive()Z| java-class)) nil)
    ((string= target-name "java/lang/Object") t)
    ((eql 1 (|isArray()Z| java-class))
     (member target-name '("java/lang/Cloneable" "java/io/Serializable") :test #'string=))
    (t
     (labels ((subtype-p (class)
                (and class
                     (or (string= (name class) target-name)
                         (flet ((named (name)
                                  (subtype-p (%get-ldk-class-by-bin-name
                                              name t (and (slot-boundp class 'ldk-loader)
                                                          (ldk-loader class))))))
                           (or (and (super class) (named (super class)))
                               (some #'named (coerce (or (interfaces class) #()) 'list))))))))
       (subtype-p (get-ldk-class-for-java-class java-class))))))

(defun %instanceof-array (objref typename)
  "instanceof against an array target type TYPENAME."
  (if (%array-assignable-to-p objref typename) 1 0))

;; Check if a native Lisp value is compatible with a Java class type.
(defun %native-type-castable-p (obj classname)
  "Return T if native Lisp value OBJ is compatible with Java CLASSNAME."
  (cond
    ((integerp obj)
     (member classname '("java/lang/Object" "java/lang/Number"
                          "java/lang/Long" "java/lang/Integer"
                          "java/lang/Short" "java/lang/Byte"
                          "java/lang/Comparable" "java/io/Serializable")
             :test #'string=))
    ((typep obj 'single-float)
     (member classname '("java/lang/Object" "java/lang/Number"
                          "java/lang/Float"
                          "java/lang/Comparable" "java/io/Serializable")
             :test #'string=))
    ((typep obj 'double-float)
     (member classname '("java/lang/Object" "java/lang/Number"
                          "java/lang/Double"
                          "java/lang/Comparable" "java/io/Serializable")
             :test #'string=))
    ((characterp obj)
     (member classname '("java/lang/Object" "java/lang/Character"
                          "java/lang/Comparable" "java/io/Serializable")
             :test #'string=))
    (t nil)))

;; Helper function for instanceof check
(defun %instanceof-check (obj target-class-name target-class)
  "Check instanceof. Returns 1 or 0.
   Returns 0 when the target class is not loaded (e.g. AWT classes not on classpath)."
  (cond
    ((%native-type-castable-p obj target-class-name) 1)
    ;; Every array is an instance of Object, Cloneable, and Serializable.
    ((and (typep obj 'java-array)
          (member target-class-name
                  '("java/lang/Object" "java/lang/Cloneable" "java/io/Serializable")
                  :test #'string=))
     1)
    ((not (find-class target-class nil)) 0)
    ((typep obj target-class) 1)
    (t 0)))

;; Helper for integer instanceof check
(defun %instanceof-integer-check (obj target-class-name target-class)
  "Check instanceof for integer types - handles native Lisp integers."
  (declare (ignore target-class-name))
  (cond
    ;; Plain Lisp integer - treat as matching all integral types
    ((integerp obj) 1)
    ;; CLOS instances - use typep
    (t (if (typep obj target-class) 1 0))))

(defmethod codegen ((insn ir-instanceof) context)
  (with-slots (class objref) insn
    (make-instance '<expression>
                   :insn insn
                   :code (let* ((cname (name (slot-value (slot-value insn 'class) 'class)))
                                ;; Ensure class is loaded before package lookup (unless array type)
                                (_ (unless (eq (char cname 0) #\[) (%resolve-class-in-context cname)))
                                (pkg (class-package cname))
                                (obj (code (codegen objref context))))
                           (cond
                             ;; Array checks
                             ((eq (char cname 0) #\[)
                              `(%instanceof-array ,obj ,cname))
                             ;; Treat native Lisp integers as instances of Java integral wrappers.
                             ((member cname '("java/lang/Integer" "java/lang/Long"
                                              "java/lang/Short" "java/lang/Byte")
                                      :test #'string=)
                              `(%instanceof-integer-check ,obj ,cname (quote ,(intern cname pkg))))
                             (t
                              `(%instanceof-check ,obj ,cname (quote ,(intern cname pkg))))))
                   :expression-type :INTEGER)))

;; Signed views of raw 32/64-bit patterns (float bits in frames.lisp).
(defun %int-to-signed (n)
  "Convert a 32-bit unsigned integer to signed representation"
  (if (logbitp 31 n)
      (- n #x100000000)
      n))

(defun %long-to-signed (n)
  "Convert a 64-bit unsigned integer to signed representation"
  (if (logbitp 63 n)
      (- n #x10000000000000000)
      n))

(defmethod codegen ((insn ir-ishl) context)
  (make-instance '<expression>
                 :insn insn
                 :code `(sb-c::mask-signed-field
                         32 (ash ,(code (codegen (value1 insn) context))
                                 (logand ,(code (codegen (value2 insn) context)) #x1F)))
                 :expression-type :INTEGER))

(defmethod codegen ((insn ir-ishr) context)
  (make-instance '<expression>
                 :insn insn
                 :code `(let ((shift-amount (logand ,(code (codegen (value2 insn) context)) #x1F)))
                          (ash (sb-c::mask-signed-field 32 ,(code (codegen (value1 insn) context)))
                               (- shift-amount)))
                 :expression-type :INTEGER))

(defmethod codegen ((insn ir-iushr) context)
  (make-instance '<expression>
                 :insn insn
                 :code `(let ((shift-amount (logand ,(code (codegen (value2 insn) context)) #x1F)))
                          (sb-c::mask-signed-field
                           32 (ash (logand ,(code (codegen (value1 insn) context)) #xFFFFFFFF)
                                   (- shift-amount))))
                 :expression-type :INTEGER))

(defmethod codegen ((insn ir-lshl) context)
  (make-instance '<expression>
                 :insn insn
                 :code `(sb-c::mask-signed-field
                         64 (ash ,(code (codegen (value1 insn) context))
                                 (logand ,(code (codegen (value2 insn) context)) #x3F)))
                 :expression-type :LONG))

(defmethod codegen ((insn ir-lshr) context)
  (make-instance '<expression>
                 :insn insn
                 :code `(let ((shift-amount (logand ,(code (codegen (value2 insn) context)) #x3F)))
                          (ash (sb-c::mask-signed-field 64 ,(code (codegen (value1 insn) context)))
                               (- shift-amount)))
                 :expression-type :LONG))

(defmethod codegen ((insn ir-lushr) context)
  (make-instance '<expression>
                 :insn insn
                 :code `(let ((shift-amount (logand ,(code (codegen (value2 insn) context)) #x3F)))
                          (sb-c::mask-signed-field
                           64 (ash (logand ,(code (codegen (value1 insn) context)) #xFFFFFFFFFFFFFFFF)
                                   (- shift-amount))))
                 :expression-type :LONG))

(defmethod codegen ((insn ir-lcmp) context)
  (make-instance '<expression>
                 :insn insn
                 :code (list 'let (list (list 'value2 (code (codegen (value2 insn) context)))
                                        (list 'value1 (code (codegen (value1 insn) context))))
                             (list 'cond
                                   (list (list '= 'value1 'value2)
                                         0)
                                   (list (list '> 'value1 'value2)
                                         1)
                                   (list 't
                                         -1)))
                 :expression-type :INTEGER))

(defmethod codegen ((insn ir-goto) context)
  (declare (ignore context))
  (with-slots (offset) insn
    (let ((expr (make-instance '<expression>
                               :insn insn
                               :code (list 'go (intern (format nil "branch-target-~A" offset))))))
      expr)))

(defmethod codegen ((insn ir-tableswitch) context)
  (declare (ignore context))
  (with-slots (default-offset low high jump-offsets) insn
    (let ((cases (loop for index from low to high
                       for offset in jump-offsets
                       collect (list index (list 'go (intern (format nil "branch-target-~A" offset))))))
          (default-target (list 'go (intern (format nil "branch-target-~A" default-offset)))))
      (make-instance '<expression>
                     :insn insn
                     :code (append (list 'case (code (codegen (index insn) context)))
                                   cases
                                   (list `(otherwise ,default-target)))))))

(defmethod codegen ((insn ir-lookupswitch) context)
  (declare (ignore context))
  (with-slots (default-offset match-offset-pairs) insn
    (let ((cases (loop for (match . offset) in match-offset-pairs
                       collect (list match (list 'go (intern (format nil "branch-target-~A" offset))))))
          (default-target (list 'go (intern (format nil "branch-target-~A" default-offset)))))
      (make-instance '<expression>
                     :insn insn
                     :code (append (list 'case (code (codegen (index insn) context)))
                                   cases
                                   (list `(otherwise ,default-target)))))))

(defun %call-site-gf (method-name nargs)
  "The generic function a virtual call to METHOD-NAME (a full name, or an
OpenLDK-internal %NAME) with NARGS arguments calls: a Java method's is
created if need be, so the call finds a function to forward from."
  (let ((symbol (intern (format nil "~A" method-name) :openldk)))
    (if (char= #\% (char (symbol-name symbol) 0))
        symbol
        (%ensure-java-gf symbol nargs))))

(defmethod codegen ((insn ir-call-virtual-method) context)
  (with-slots (method-name args) insn
    ;; Virtual method dispatch uses :openldk package for generic function names
    ;; Instance methods are defined as CLOS generic functions in :openldk for cross-loader dispatch
    (make-instance '<expression>
                   :insn insn
                   :code (let* ((nargs (length args))
                                (receiver-code (code (codegen (car args) context)))
                                (null-checked-receiver `(%receiver ,receiver-code))
                                (call (cond
                                        ((eq nargs 0)
                                         (internal-error "virtual call ~A has no receiver argument" method-name))
                                        ((eq nargs 1)
                                         (list (%call-site-gf method-name nargs) null-checked-receiver))
                                        (t
                                         `(funcall (function ,(%call-site-gf method-name nargs))
                                                   ,null-checked-receiver
                                                   ,@(mapcar (lambda (a) (code (codegen a context))) (cdr args)))))))
                           call))))

(defparameter *invokedynamic-cache* (make-hash-table :test #'equal))

(defun %call-bootstrap-method (bootstrap-method-name caller name args)
  "Call the bootstrap method BOOTSTRAP-METHOD-NAME (the full
\"class/name.method(descriptor)\" string) with a lookup on the class CALLER
(a binary name), the Java string NAME, and ARGS (the call site's type, then
the static arguments).  The bootstrap class is always a boot/library class
or the caller's, so load it and resolve the static function in its own
defining package rather than the caller's."
  (let* ((paren (position #\( bootstrap-method-name))
         (dot (position #\. bootstrap-method-name :end paren :from-end t))
         (bootstrap-class (subseq bootstrap-method-name 0 dot)))
    (%resolve-class-in-context bootstrap-class)
    (let* ((pkg (class-package bootstrap-class))
           (fn-sym (static-method-symbol (lispize-method-name bootstrap-method-name) pkg))
           (param-types (parse-parameter-types bootstrap-method-name))
           (nparams (length param-types))
           (full-args (append (list (%make-trusted-lookup (%get-java-class-by-bin-name caller t)) name)
                              args))
           ;; Bootstrap methods are frequently varargs (last parameter an
           ;; Object[]), e.g. SwitchBootstraps.typeSwitch and
           ;; ObjectMethods.bootstrap. Collect the trailing arguments into a
           ;; single Object[] so the fixed-arity Lisp function receives them
           ;; the way the JVM would after varargs packing.
           (final-args
             (if (and (plusp nparams)
                      (let ((last (car (last param-types))))
                        (and (stringp last) (>= (length last) 2)
                             (string= "[]" (subseq last (- (length last) 2)))))
                      (>= (length full-args) nparams))
                 (let ((fixed (1- nparams)))
                   (append (subseq full-args 0 fixed)
                           (list (make-java-array
                                  :component-class (%get-java-class-by-bin-name "java/lang/Object")
                                  :initial-contents (coerce (nthcdr fixed full-args) 'vector)))))
                 full-args)))
      (apply fn-sym final-args))))

(defun %resolve-invokedynamic (method-name bootstrap-method-name site-key caller fname &rest args)
  "Resolve an invokedynamic call site by invoking its bootstrap method, once:
SITE-KEY identifies the call site; CALLER is the binary name of its class."
  (declare (ignore method-name))
  (or (gethash site-key *invokedynamic-cache*)
      (setf (gethash site-key *invokedynamic-cache*)
            (%call-bootstrap-method bootstrap-method-name caller fname args))))

(defvar *constant-dynamic-cache* (make-hash-table :test #'eq :synchronized t)
  "ldc site key -> (value), once its CONSTANT_Dynamic is resolved.")

(defun %resolve-constant-dynamic (site-key bootstrap-method-name caller name descriptor &rest args)
  "The value of a CONSTANT_Dynamic: its bootstrap method's result, called
once per ldc site with the constant's type, unboxed if primitive."
  (car (or (gethash site-key *constant-dynamic-cache*)
           (setf (gethash site-key *constant-dynamic-cache*)
                 (let ((value (%call-bootstrap-method
                               bootstrap-method-name caller (jstring name)
                               (cons (%bin-type-name-to-class descriptor) args))))
                   (list (if (find (char descriptor 0) "ZBCSIJFD")
                             (%maybe-unbox value)
                             value)))))))

(defmethod codegen ((insn ir-constant-dynamic) context)
  (with-slots (bootstrap-method-name name descriptor args caller) insn
    (make-instance '<expression>
                   :insn insn
                   :code `(%resolve-constant-dynamic ',(gensym "CONDY-SITE") ,bootstrap-method-name
                                                     ,caller ,name ,descriptor
                                                     ,@(mapcar (lambda (a) (code (codegen a context))) args))
                   :expression-type (get-stack-type-from-descriptor descriptor))))

(defun %extract-string-concat-recipe (args)
  "Extract the recipe string from StringConcatFactory bootstrap args.
The recipe is the second element (first bootstrap arg after the MethodType)."
  (let ((recipe-node (second args)))
    (when (typep recipe-node 'ir-string-literal)
      (slot-value recipe-node 'value))))

(defun %descriptor-param-type-chars (descriptor)
  "Return the top-level parameter type chars of a JVM method DESCRIPTOR, in order:
one of Z B S C I J F D for primitives, #\\L for object refs, #\\[ for arrays."
  (when descriptor
    (let ((chars nil)
          (i (1+ (position #\( descriptor)))
          (end (position #\) descriptor)))
      (loop while (< i end)
            do (let ((ch (char descriptor i)))
                 (cond
                   ((find ch "ZBSCIJFD") (push ch chars) (incf i))
                   ((char= ch #\L) (push #\L chars) (setf i (1+ (position #\; descriptor :start i))))
                   ((char= ch #\[) (push #\[ chars)
                    (loop while (char= (char descriptor i) #\[) do (incf i))
                    (if (char= (char descriptor i) #\L)
                        (setf i (1+ (position #\; descriptor :start i)))
                        (incf i)))
                   (t (incf i)))))
      (nreverse chars))))

(defun %string-concat-arg-form (code type-char)
  "Wrap a dynamic string-concat argument CODE so it renders per its Java static
TYPE-CHAR: boolean -> true/false, char -> the character; otherwise via
%to-java-string (which handles numbers, strings, null, and Object.toString)."
  (case type-char
    (#\Z `(if (or (null ,code) (eql ,code 0)) "false" "true"))
    (#\C `(let ((c ,code)) (string (if (characterp c) c (code-char c)))))
    (t `(%to-java-string ,code))))

(defun %generate-string-concat-code (recipe dynamic-arg-codes &optional type-chars)
  "Generate Lisp code for StringConcatFactory.makeConcatWithConstants.
RECIPE is the template string with \\x01 placeholders for dynamic args.
DYNAMIC-ARG-CODES is a list of codegen'd expressions for the dynamic arguments.
TYPE-CHARS, if given, are the call-site parameter type chars (aligned with
DYNAMIC-ARG-CODES) used to render boolean/char args the Java way."
  (let ((parts nil)
        (arg-idx 0)
        (start 0))
    (loop for i below (length recipe)
          for ch = (char recipe i)
          do (cond
               ((char= ch (code-char 1))
                (when (< start i)
                  (push (subseq recipe start i) parts))
                (when (< arg-idx (length dynamic-arg-codes))
                  ;; Pre-format each dynamic arg per its call-site static type so
                  ;; boolean prints true/false and char prints the character.
                  (push (%string-concat-arg-form (nth arg-idx dynamic-arg-codes)
                                                 (nth arg-idx type-chars))
                        parts))
                (incf arg-idx)
                (setf start (1+ i)))
               ((char= ch (code-char 2))
                (when (< start i)
                  (push (subseq recipe start i) parts))
                (setf start (1+ i)))))
    (when (< start (length recipe))
      (push (subseq recipe start (length recipe)) parts))
    (setf parts (nreverse parts))
    ;; PARTS holds string literals and pre-wrapped code forms (each yielding a
    ;; string at runtime), so no further per-part conversion is needed.
    `(jstring (format nil "~{~A~}" (list ,@parts)))))

(defun %to-java-string (val)
  "Convert a value to its Java string representation for StringConcatFactory."
  (cond
    ((null val) "null")
    ((typep val '|java/lang/String|) (lstring val))
    ((integerp val) (format nil "~D" val))
    ((floatp val) (format nil "~F" val))
    ((characterp val) (string val))
    ((typep val '|java/lang/Object|)
     (let ((str (|toString()Ljava/lang/String;| val)))
       (if str (lstring str) "null")))
    (t (format nil "~A" val))))

(defun %type-switch-match (target label)
  "True if TARGET matches a single SwitchBootstraps.typeSwitch LABEL, which is a
Class (instanceof), a boxed integral constant, or a String constant."
  (cond
    ((typep label '|java/lang/Class|)
     (= 1 (|isInstance(Ljava/lang/Object;)Z| label target)))
    ((integerp label)
     (and (typep target '(or |java/lang/Integer| |java/lang/Short|
                          |java/lang/Byte| |java/lang/Character|))
          (slot-boundp target '|value|)
          (eql (slot-value target '|value|) label)))
    ((typep label '|java/lang/String|)
     (and (typep target '|java/lang/String|)
          (string= (lstring target) (lstring label))))
    (t (and target (typep target '|java/lang/Object|)
            (= 1 (|equals(Ljava/lang/Object;)Z| target label))))))

(defun %type-switch (target restart labels)
  "Native implementation of java.lang.runtime.SwitchBootstraps.typeSwitch's target
method handle: returns the index of the first LABEL (at or after RESTART) that
matches TARGET, LABELS's length if none match, or -1 when TARGET is null."
  (if (null target)
      -1
      (let* ((v (coerce labels 'simple-vector))
             (n (length v)))
        (loop for i from (max restart 0) below n
              when (%type-switch-match target (svref v i))
                do (return-from %type-switch i))
        n)))

(defun %enum-switch-match (target label)
  "True if enum constant TARGET matches an enumSwitch LABEL (an enum constant name
as a String, or an Enum$EnumDesc)."
  (let ((name (cond
                ((typep label '|java/lang/String|) (lstring label))
                ((and (typep label '|java/lang/Object|)
                      (slot-exists-p label '|constantName|)
                      (slot-boundp label '|constantName|))
                 (lstring (slot-value label '|constantName|)))
                (t nil))))
    (and name
         (let ((tn (|name()| target)))
           (and tn (string= (lstring tn) name))))))

(defun %enum-switch (target restart labels)
  "Native implementation of SwitchBootstraps.enumSwitch's target method handle."
  (if (null target)
      -1
      (let* ((v (coerce labels 'simple-vector))
             (n (length v)))
        (loop for i from (max restart 0) below n
              when (%enum-switch-match target (svref v i))
                do (return-from %enum-switch i))
        n)))

(defun %record-component-names (names-jstring)
  "Split a record's ';'-separated component-name string into a list of names."
  (remove "" (uiop:split-string (lstring names-jstring) :separator ";") :test #'string=))

(defun %record-component-value (record name)
  (slot-value record (intern (mangle-field-name name) :openldk)))

(defun %record-component-descriptor (class name)
  "Return the field descriptor string for record component NAME in CLASS, or NIL."
  (let ((lc (get-ldk-class-for-java-class class)))
    (when lc
      (loop for f in (coerce (fields lc) 'list)
            when (string= (name f) name)
              return (descriptor f)))))

(defun %format-record-component (value descriptor)
  "Render a record component VALUE for toString, honouring its DESCRIPTOR so that
boolean prints true/false and char prints the character rather than an int."
  (case (and descriptor (plusp (length descriptor)) (char descriptor 0))
    (#\Z (if (or (null value) (eql value 0)) "false" "true"))
    (#\C (string (if (characterp value) value (code-char value))))
    (t (%to-java-string value))))

(defun %class-simple-name (class)
  "Return the simple (unqualified) name of a Class object."
  (let* ((n (lstring (slot-value class '|name|)))
         (dot (position #\. n :from-end t))
         (n (if dot (subseq n (1+ dot)) n))
         (dollar (position #\$ n :from-end t))
         (n (if dollar (subseq n (1+ dollar)) n)))
    ;; Local/anonymous classes have binary names like Outer$1Name; getSimpleName
    ;; strips the compiler-added leading digits (Outer$1Name -> Name, Outer$1 -> "").
    (string-left-trim "0123456789" n)))

(defun %record-to-string (record class names-jstring)
  "Native implementation of a record's ObjectMethods-generated toString()."
  (jstring
   (format nil "~A[~{~A~^, ~}]"
           (%class-simple-name class)
           (loop for name in (%record-component-names names-jstring)
                 collect (format nil "~A=~A" name
                                 (%format-record-component
                                  (%record-component-value record name)
                                  (%record-component-descriptor class name)))))))

(defun %record-field-hash (v)
  (cond ((null v) 0)
        ((typep v '|java/lang/Object|) (or (|hashCode()I| v) 0))
        ((integerp v) v)
        ((characterp v) (char-code v))
        (t (sxhash v))))

(defun %record-hash-code (record class names-jstring)
  "Native implementation of a record's ObjectMethods-generated hashCode()."
  (declare (ignore class))
  (let ((h 0))
    (dolist (name (%record-component-names names-jstring))
      (setf h (logand (+ (* 31 h) (%record-field-hash (%record-component-value record name)))
                      #xFFFFFFFF)))
    (if (>= h #x80000000) (- h #x100000000) h)))

(defun %record-field-equal (a b)
  (cond ((and (null a) (null b)) t)
        ((or (null a) (null b)) nil)
        ((typep a '|java/lang/Object|) (= 1 (|equals(Ljava/lang/Object;)Z| a b)))
        (t (eql a b))))

(defun %record-equals (record other class names-jstring)
  "Native implementation of a record's ObjectMethods-generated equals()."
  (if (and other (= 1 (|isInstance(Ljava/lang/Object;)Z| class other)))
      (if (every (lambda (name)
                   (%record-field-equal (%record-component-value record name)
                                        (%record-component-value other name)))
                 (%record-component-names names-jstring))
          1 0)
      0))

(defmethod codegen ((insn ir-call-dynamic-method) context)
  (with-slots (method-name args dynamic-args bootstrap-method-name address interface-type-name call-site-descriptor) insn
    (let ((pkg (context-package context)))
      (cond
        ;; Fast path for ObjectMethods.bootstrap (record toString/hashCode/equals).
        ;; The real JDK generates a hidden class; instead we compute directly from
        ;; the record's components. args[1] is the record Class, args[2] is the
        ;; ';'-separated component-name string; dynamic-args are (this) or (this, other).
        ((search "ObjectMethods.bootstrap" bootstrap-method-name)
         (let ((mname (if (stringp method-name) method-name (lstring method-name)))
               (class-code (code (codegen (second args) context)))
               (names-code (code (codegen (third args) context)))
               (this-code (code (codegen (first dynamic-args) context))))
           (cond
             ((string= mname "toString")
              (make-instance '<expression> :insn insn
                             :code `(%record-to-string ,this-code ,class-code ,names-code)
                             :expression-type :REFERENCE))
             ((string= mname "hashCode")
              (make-instance '<expression> :insn insn
                             :code `(%record-hash-code ,this-code ,class-code ,names-code)
                             :expression-type :INTEGER))
             ((string= mname "equals")
              (make-instance '<expression> :insn insn
                             :code `(%record-equals ,this-code
                                                    ,(code (codegen (second dynamic-args) context))
                                                    ,class-code ,names-code)
                             :expression-type :INTEGER))
             (t (unimplemented "ObjectMethods bootstrap method ~A" mname)))))
        ;; Fast path for SwitchBootstraps.typeSwitch / enumSwitch (pattern-matching
        ;; switch). The real JDK generates a hidden class via the ClassFile API;
        ;; instead we compute the case index directly. args[0] is the call-site
        ;; MethodType and (rest args) are the case labels; dynamic-args are
        ;; (selector, restartIndex).
        ((search "SwitchBootstraps.typeSwitch" bootstrap-method-name)
         (make-instance '<expression>
                        :insn insn
                        :code `(%type-switch ,(code (codegen (first dynamic-args) context))
                                             ,(code (codegen (second dynamic-args) context))
                                             (list ,@(mapcar (lambda (a) (code (codegen a context))) (rest args))))
                        :expression-type :INTEGER))
        ((search "SwitchBootstraps.enumSwitch" bootstrap-method-name)
         (make-instance '<expression>
                        :insn insn
                        :code `(%enum-switch ,(code (codegen (first dynamic-args) context))
                                             ,(code (codegen (second dynamic-args) context))
                                             (list ,@(mapcar (lambda (a) (code (codegen a context))) (rest args))))
                        :expression-type :INTEGER))
        ;; Fast path for StringConcatFactory (JDK 9+ string concatenation)
        ((search "StringConcatFactory.makeConcatWithConstants" bootstrap-method-name)
         (let ((recipe (%extract-string-concat-recipe args))
               (type-chars (%descriptor-param-type-chars call-site-descriptor))
               (dyn-codes (mapcar (lambda (a) (code (codegen a context))) dynamic-args)))
           (if recipe
               (make-instance '<expression>
                              :insn insn
                              :code (%generate-string-concat-code recipe dyn-codes type-chars)
                              :expression-type :REFERENCE)
               ;; No recipe: just concatenate the dynamic args in order (still
               ;; honouring boolean/char static types).
               (make-instance '<expression>
                              :insn insn
                              :code `(jstring (format nil "~{~A~}"
                                              (list ,@(loop for c in dyn-codes
                                                            for tc in (append type-chars
                                                                              (make-list (max 0 (- (length dyn-codes)
                                                                                                   (length type-chars)))))
                                                            collect (%string-concat-arg-form c tc)))))
                              :expression-type :REFERENCE))))
        ;; Fast path for LambdaMetafactory.  altMetafactory (serializable
        ;; lambdas, such as Comparator.comparing's) takes the same first three
        ;; static arguments, then flags, marker interfaces and bridges.
        ((or (search "LambdaMetafactory.metafactory" bootstrap-method-name)
             (search "LambdaMetafactory.altMetafactory" bootstrap-method-name))
         (make-instance '<expression>
                        :insn insn
                        :code `(openldk::%lambda-metafactory
                                ,(code (codegen (third args) context))
                                (list ,@(mapcar (lambda (a) (code (codegen a context))) dynamic-args))
                                ,method-name
                                ,(code (codegen (second args) context))
                                ,interface-type-name
                                ,@(when (search "altMetafactory" bootstrap-method-name)
                                    `((list ,@(mapcar (lambda (a) (code (codegen a context)))
                                                      (nthcdr 4 args))))))))
        ;; Fallback: generic invokedynamic handling
        (t
         (make-instance '<expression>
                        :insn insn
                        :code
                        ;; Each call site resolves once: its own key, and a
                        ;; lookup on the class it is in, as the bootstrap
                        ;; method expects (Groovy dispatches on it).  The
                        ;; target has the call site's type: invoke it exactly.
                        `(let ((callsite (%resolve-invokedynamic ,method-name
                                                                 ,bootstrap-method-name
                                                                 ',(gensym "INDY-SITE")
                                                                 ,(slot-value (slot-value context 'class) 'name)
                                                                 ,(jstring method-name)
                                                                 ,@(remove nil (mapcar (lambda (a)
                                                                                        (when a (code (codegen a context))))
                                                                                      args)))))
                           (%mh-invoke (|getTarget()| callsite)
                                       (list ,@(mapcar (lambda (a) (code (codegen a context))) dynamic-args))))))))))

(defmethod codegen ((insn ir-clinit) context)
  (with-slots (class) insn
    (make-instance '<expression>
                   :insn insn
                   :code (let* ((class (ir-class-class class)))
                           (if class
                               (let ((pkg (class-package (slot-value class 'name))))
                                 (list 'unless (list 'initialized-p class)
                                       (list (intern (format nil "%clinit-~A" (slot-value class 'name)) pkg))))
                               nil)))))

(defmethod codegen ((insn ir-local-variable) context)
  (with-slots (index) insn
    ;; FIXME: track type of local vars
    ;; Local variables use :openldk package - they don't need per-loader isolation
    (let ((expr (make-instance '<expression>
                               :insn insn
                               :code (intern (format nil "local-~A" index) :openldk))))
      expr)))

(defmethod codegen ((insn ir-long-local-variable) context)
  (with-slots (index) insn
    ;; Local variables use :openldk package - they don't need per-loader isolation
    (let ((expr (make-instance '<expression>
                               :insn insn
                               :code (intern (format nil "local-~A" index) :openldk)
                               :expression-type :LONG)))
      expr)))

(defmethod codegen ((insn ir-monitorenter) context)
  (make-instance '<expression>
                 :insn insn
                 :code `(monitor-enter ,(code (codegen (slot-value insn 'objref) context)))))

(defmethod codegen ((insn ir-monitorexit) context)
  (make-instance '<expression>
                 :insn insn
                 :code `(monitor-exit ,(code (codegen (slot-value insn 'objref) context)))))

(defmethod codegen ((insn ir-new) context)
  (declare (ignore context))
  (with-slots (class) insn
    (with-slots (class) class
      (when (null class)
        (return-from codegen
          (make-instance '<expression>
                         :insn insn
                         :code `(%throw '|java/lang/NoClassDefFoundError|)
                         :expression-type :REFERENCE)))
      ;; Ensure class is loaded before package lookup
      (let* ((classname (slot-value class 'name))
             (_ (%resolve-class-in-context classname))
             (pkg (class-package classname)))
        (make-instance '<expression>
                       :insn insn
                       ;; A new object's fields are all default: a Java field
                       ;; named clazz too (Groovy's ClassNode has one).
                       :code `(make-instance ',(intern classname pkg))
                       :expression-type :REFERENCE)))))

(defmethod codegen ((insn ir-new-array) context)
  (make-instance '<expression>
                 :insn insn
                 :code `(make-java-array :size ,(code (codegen (size insn) context))
                                         :component-class ,(component-class insn)
                                         :element-type ',(case (atype insn)
                                                           ((4 8) '(signed-byte 8)) ; boolean, byte
                                                           (5 '(unsigned-byte 16))  ; char
                                                           (6 'single-float)
                                                           (7 'double-float)
                                                           (9 '(signed-byte 16))    ; short
                                                           (10 '(signed-byte 32))   ; int
                                                           (11 '(signed-byte 64))   ; long
                                                           (t t)))                  ; anewarray
                 :expression-type :ARRAY))

(defun %multi-array-leaf-default (component-name)
  "Default element for a leaf array whose component descriptor is
COMPONENT-NAME: the primitive zero/false/NUL, or nil for references."
  (case (char component-name 0)
    (#\D 0.0d0)
    (#\F 0.0)
    ((#\I #\J #\S #\B #\Z) 0)
    (#\C #\Null)
    (t nil)))

(defun %make-multi-array (type-name dimensions)
  "Build a multidimensional array for the JVM array descriptor TYPE-NAME
(slash-separated, e.g. \"[[Ljava/lang/Long;\") allocating the leading
DIMENSIONS.  Each level's component class is TYPE-NAME with one leading
'[' removed, so the true element type is preserved -- without it aastore
covariance checks (ArrayStoreException) can't see the real component
type."
  (if (null dimensions)
      nil
      (let* ((size (car dimensions))
             (component-name (subseq type-name 1))
             (rest-dims (cdr dimensions)))
        ;; Check for negative array size
        (when (< size 0)
          (let ((exc (%make-java-instance "java/lang/NegativeArraySizeException")))
            (|<init>()V| exc)
            (error (%lisp-condition exc))))
        (let ((component-class (%bin-type-name-to-class component-name)))
          (if rest-dims
              (make-java-array :size size
                               :component-class component-class
                               :initial-contents
                               (loop repeat size
                                     collect (%make-multi-array component-name rest-dims)))
              (make-java-array :size size
                               :component-class component-class
                               :initial-element (%multi-array-leaf-default component-name)))))))

(defmethod codegen ((insn ir-multi-new-array) context)
  ;; The MULTIANEWARRAY operand is the full array type (e.g. "[[Ljava/lang/Long;");
  ;; pass it so %make-multi-array can set the real component class at each level.
  (let ((type-name (substitute #\/ #\.
                               (lstring (slot-value (java-class (ir-class-class (slot-value insn 'class)))
                                                    '|name|)))))
    (make-instance '<expression>
                   :insn insn
                   :code `(%make-multi-array
                           ,type-name
                           ;; Reverse sizes because bytecode pops dimensions in reverse order
                           (list ,@(mapcar (lambda (c) (code (codegen c context))) (reverse (sizes insn)))))
                   :expression-type :ARRAY)))

(defmethod codegen ((insn ir-nop) context)
  (declare (ignore context))
  nil)

(defmethod codegen ((insn ir-stop-marker) context)
  (declare (ignore context))
  (make-instance '<expression>
                 :insn insn
                 :code (list 'return-from 'try-body nil)))

(defmethod codegen ((insn ir-call-special-method) context)
  (with-slots (class method-name args) insn
    (when (null class)
      (return-from codegen
        (make-instance '<expression>
                       :insn insn
                       :code `(%throw '|java/lang/NoClassDefFoundError|)
                       :expression-type :REFERENCE)))
    (let* ((class-name (slot-value class 'name))
           ;; Ensure class is loaded before package lookup
           (_ (%resolve-class-in-context class-name))
           (pkg (class-package class-name))
           ;; Method symbol goes in :openldk - constructors are generic functions
           ;; shared across loaders for CLOS dispatch, like instance methods
           (method-symbol (intern (format nil "~A" method-name) :openldk))
           ;; Owner class symbol goes in the class's package
           (owner-symbol (intern class-name pkg))
           (arg-code (mapcar (lambda (a) (code (codegen a context))) args)))
      (make-instance '<expression>
                     :insn insn
                     :code `(openldk::invoke-special ',method-symbol ',owner-symbol
                                                     (list ,@arg-code))))))

(defun %resolve-field-slot (ref-class-name member-name)
  "Resolve a Fieldref (REF-CLASS-NAME, MEMBER-NAME) to its CLOS slot
symbol, honoring JVM field resolution: walk up from the referenced class
to the first class declaring the field; a shadowing declaration has its
own class-qualified slot recorded in *FIELD-SHADOW-SLOTS*."
  (let ((plain (intern (mangle-field-name member-name) :openldk))
        ;; The Fieldref's class arrives as whatever EMIT produced for the
        ;; class constant: an IR-CLASS wrapping <CLASS> metadata (or, in
        ;; principle, a bare string) -- normalize to the binary name.
        (ref-class-name (typecase ref-class-name
                          (string ref-class-name)
                          (ir-class (let ((c (ir-class-class ref-class-name)))
                                      (and c (name c))))
                          (<class> (name ref-class-name))
                          (t nil))))
    (if (null ref-class-name)
        plain
        (loop with cname = ref-class-name
              for depth from 0 below 64
              while cname
              do (let ((shadow (gethash (format nil "~A.~A" cname member-name)
                                        *field-shadow-slots*)))
                   (when shadow (return shadow))
                   (let ((cls (gethash cname *ldk-classes-by-bin-name*)))
                     (unless cls (return plain))
                     (when (find member-name (fields cls)
                                 :key (lambda (fl) (slot-value fl 'name))
                                 :test #'string=)
                       (return plain))
                     (setf cname (slot-value cls 'super))))
              finally (return plain)))))

(defun %slot-accessor (context)
  "The operator for a field access: a per-site cached access, or for
compact code a plain call."
  (if (compact-p context) '(%java-slot-value) '(%cached-slot-value)))

(defun %slot-cache-argument (context)
  (unless (compact-p context) '((load-time-value (list nil)))))

(defmethod codegen ((insn ir-member) context)
  (with-slots (objref member-name ref-class) insn
    (make-instance '<expression>
                   :insn insn
                   ;; The accessor throws NullPointerException for a null OBJREF.
                   :code `(,@(%slot-accessor context)
                           ,(code (codegen objref context))
                           ;; Field names stay in :openldk for CLOS slot inheritance
                           (quote ,(%resolve-field-slot ref-class member-name))
                           ,@(%slot-cache-argument context)))))

(defmethod codegen ((insn ir-static-member) context)
  (with-slots (class member-name) insn
    (let ((class-name (slot-value (slot-value class 'class) 'name)))
      ;; Ensure class is loaded before looking up its package
      ;; This is necessary because the class might be in a different loader's package
      (%resolve-class-in-context class-name)
      (let* ((pkg (class-package class-name))
             (static-sym-name (format nil "+static-~A+" class-name))
             (sym (intern static-sym-name pkg)))
        (make-instance '<expression>
                       :insn insn
                       :code `(,@(%slot-accessor context)
                               ,sym
                               ;; Field names stay in :openldk for CLOS slot inheritance
                               (quote ,(intern (mangle-field-name member-name) :openldk))
                               ,@(%slot-cache-argument context)))))))

(defmethod codegen ((insn ir-throw) context)
  (make-instance '<expression>
                 :insn insn
                 :code `(let* ((obj ,(code (codegen (slot-value insn 'objref) context)))
                               (c (%lisp-condition obj)))
                          (setf |condition-cache| c)
                          (error c))))

(defmethod codegen ((insn ir-return) context)
  (declare (ignore context))
  (make-instance '<expression>
                 :insn insn
                 :code `(return)))

(defmethod codegen ((insn ir-return-value) context)
  ;; Static methods have format "class/name.method()" with "." before "("
  ;; Instance methods have format "method(Ljava/lang/Object;)" - "/" only in descriptors
  ;; Static methods use loader's package, instance methods use :openldk (generic functions)
  (let* ((fn-name (slot-value insn 'fn-name))
         (fn-pkg (if (find #\. fn-name)
                     (context-package context)
                     (find-package :openldk)))
         (ret-sym (intern fn-name fn-pkg))
         (value-code (code (codegen (slot-value insn 'value) context))))
    (make-instance '<expression>
                   :insn insn
                   ;; Only emit the per-return trace when a debug-trace flag is set
                   ;; at (JIT) compile time; otherwise a bare return, so production
                   ;; code doesn't pay two special-variable reads on every return.
                   :code (if (or *debug-trace-args* *debug-trace*)
                             `(let ((result ,value-code))
                                (cond
                                  (*debug-trace-args*
                                   (format t "~&~V@A <~A> trace: ~A result = ~A~%"
                                           *call-nesting-level* "*" *call-nesting-level*
                                           ,(fn-name *context*) result))
                                  (*debug-trace*
                                   (format t "~&~V@A <~A> trace: ~A~%"
                                           *call-nesting-level* "*" *call-nesting-level* ,(fn-name *context*))))
                                (return-from ,ret-sym result))
                             `(return-from ,ret-sym ,value-code)))))

(defvar *current-block* nil
  "Dynamic variable holding the current <basic-block> during codegen.
Used to consult block-local substitutions in addition to global ones.")

(defmethod codegen ((insn <stack-variable>) context)
  ;; First check global substitutions, then block-local
  ;; This order prevents issues with local variables that may not exist in final code
  (let ((v (gethash insn (single-assignment-table context))))
    ;; Only check block-local if not in global table
    (unless v
      (when (and *current-block*
                 (slot-boundp *current-block* 'local-substitutions))
        (let ((local-subs (slot-value *current-block* 'local-substitutions)))
          (when local-subs
            (setf v (gethash insn local-subs))))))
    (if v
        (codegen v context)
        ;; Stack variables use :openldk package - they don't need per-loader isolation
        (make-instance '<expression>
                       :insn insn
                       :code (intern (format nil "s{~{~A~^,~}}" (sort (copy-list (slot-value insn 'var-numbers)) #'<)) :openldk)))))

(defmethod codegen-block ((basic-block <basic-block>) dominator-block)
  "Generate Lisp code for a basic block, handling exception scopes and control flow."

  (let ((new-scope nil))
    ;;  (when (or handler-start (fset:contains? (dominators basic-block) dominator-block))
    (when (fset:contains? (dominators basic-block) dominator-block)
      (unless (find basic-block (car (emitted-block-scopes *context*)))
        (when (try-catch basic-block)
          (push basic-block (first (emitted-block-scopes *context*)))
          (push (list) (emitted-block-scopes *context*)))
        (let* ((stop-emitting-blocks? nil)
               (lisp-code
                 ;; Bind *current-block* so codegen can access local substitutions
                 (let ((*current-block* basic-block))
                   (cons (intern (format nil "branch-target-~A" (address (car (slot-value basic-block 'code)))))
                         (loop for insn in (slot-value basic-block 'code)
                               do (when (and *debug-codegen* (slot-value insn 'dead-p))
                                    (format t "; Skipping dead instruction: ~A~%" (type-of insn)))
                               when (not (slot-value insn 'dead-p))  ; Skip dead instructions
                                 append (let ((expr (codegen insn *context*)))
                                          (when (typep insn 'ir-stop-marker)
                                            (setf stop-emitting-blocks? t))
                                          (if expr
                                              (list (trace-insn insn (code expr)))
                                              nil)))))))
          (push basic-block (first (emitted-block-scopes *context*)))
          (pop (slot-value *context* 'blocks))

          ;; The last block of a catch handler doesn't emit its fall-through
          ;; successor (the merge point after the try/catch), which is
          ;; emitted on another path -- possibly earlier, as when the
          ;; try/catch sits inside an IF whose false branch skips to the same
          ;; place.  Jump there explicitly, or the handler would run off the
          ;; end of the TAGBODY and the method would return NIL.
          (when (and (end-of-handler? basic-block)
                     (not stop-emitting-blocks?)
                     (fall-through-address basic-block))
            (setf lisp-code
                  (nconc lisp-code
                         (list (list 'go (intern (format nil "branch-target-~A"
                                                         (address (car (code (fall-through-address basic-block)))))))))))

          (unless (end-of-handler? basic-block)

            ;; Emit code for successors if not stopping
            (unless stop-emitting-blocks?
              (when (fall-through-address basic-block)
                (let* ((ft-block (fall-through-address basic-block))
                       (ft-tag (intern (format nil "branch-target-~A" (address (car (code ft-block)))))))
                  (setf lisp-code
                        (nconc lisp-code
                               (if (or (find ft-block (car (emitted-block-scopes *context*)))
                                       (gethash (address ft-block) (try-end-table *context*)))
                                   (list (list 'go ft-tag))
                                   ;; If codegen-block returns NIL (e.g. dominator check fails),
                                   ;; generate a GO to the fall-through block's tag in an enclosing TAGBODY.
                                   (or (codegen-block ft-block (if (try-catch basic-block) basic-block dominator-block))
                                       (list (list 'go ft-tag))))))))
              (let ((successor-list (sort (fset:convert 'list (successors basic-block))
                                          (lambda (a b) (< (address a) (address b))))))
                (dolist (successor successor-list)
                  (unless (gethash (address successor) (try-end-table *context*))
                    (setf lisp-code (nconc lisp-code (codegen-block successor (if (try-catch basic-block) basic-block dominator-block)))))))))

          ;; Handle exception handlers (try-catch)
          (let ((try-catch-handlers (try-catch basic-block))
                (ctx-pkg (context-package *context*)))
            (when try-catch-handlers
              (pop (emitted-block-scopes *context*))
              ;; Compute a GO to the try-end block to prevent fall-through
              ;; after the HANDLER-CASE (JVM semantics: normal completion
              ;; of a try block continues at the end-pc).
              (let ((fall-through-go
                      (when-let ((end-block (first (exception-end-blocks basic-block))))
                        (list (list 'go (intern (format nil "branch-target-~A"
                                                        (address (car (code end-block))))))))))
                ;; Wrap the block's code in HANDLER-CASE
                ;; Pull any branch target out of the HANDLER-CASE first.
                (setf lisp-code
                      (if (and lisp-code
                               (starts-with? "branch-target-" (format nil "~A" (car lisp-code))))
                          (let ((bt (car lisp-code))
                                (lisp-code (cdr lisp-code)))
                            `(,bt
                              (HANDLER-CASE
                                  ;; Convert Lisp conditions to Java throwables
                                  ;; *inside* the protected form: a throwable
                                  ;; signalled from a handler established
                                  ;; outside this HANDLER-CASE could never be
                                  ;; caught by these clauses.
                                  (openldk::with-java-conditions
                                      (:arrays ,(and (needs-array-bounds-check *context*) t))
                                    (BLOCK TRY-BODY
                                      (TAGBODY ,@lisp-code)))
                                ;; Condition symbols always in :openldk for cross-loader catching
                                ,@(loop for (exception-type . handler-block) in try-catch-handlers
                                        when (> (length exception-type) 0)
                                          do (%resolve-class-in-context exception-type)
                                        collect `(,(intern (format nil "condition-~A" (or exception-type
                                                                                          "java/lang/Throwable")) :openldk)
                                                  (|condition|)
                                                  (when *debug-exceptions*
                                                    (format *error-output*
                                                            "~&Caught Java exception in ~A.~A~%"
                                                            ,(slot-value
                                                              (slot-value *context* 'class)
                                                              'name)
                                                            ,(fn-name *context*))
                                                    (%print-java-stack-trace
                                                     (slot-value |condition| '|objref|)
                                                     :stream *error-output*))
                                                  (setf |condition-cache| |condition|)
                                                  ,@(%handler-entry-flag handler-block)
                                                  (go ,(intern (format nil "branch-target-~A" (address handler-block)))))))
                              ,@fall-through-go))
                          `((HANDLER-CASE
                                ;; See the note above: the conversion must be
                                ;; established inside the protected form.
                                (openldk::with-java-conditions
                                    (:arrays ,(and (needs-array-bounds-check *context*) t))
                                  (BLOCK TRY-BODY
                                    (TAGBODY ,@lisp-code)))
                              ;; Condition symbols always in :openldk for cross-loader catching
                              ,@(loop for (exception-type . handler-block) in try-catch-handlers
                                      do (%resolve-class-in-context exception-type)
                                      collect `(,(intern (format nil "condition-~A" (or exception-type
                                                                                        "java/lang/Throwable")) :openldk)
                                                (|condition|)
                                                (when *debug-exceptions*
                                                  (format *error-output*
                                                          "~&Caught Java exception in ~A.~A~%"
                                                          ,(slot-value
                                                            (slot-value *context* 'class)
                                                            'name)
                                                          ,(fn-name *context*))
                                                  (%print-java-stack-trace
                                                   (slot-value |condition| '|objref|)
                                                   :stream *error-output*))
                                                (setf |condition-cache| |condition|)
                                                ,@(%handler-entry-flag handler-block)
                                                (go ,(intern (format nil "branch-target-~A" (address handler-block)))))))
                            ,@fall-through-go))))))
          lisp-code)))))

