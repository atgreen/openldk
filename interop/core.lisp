;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: OPENLDK.INTEROP; Base: 10 -*-
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

;;; The shared core of OpenLDK's language systems (openldk/jscheme, ...):
;;; starting the Java runtime, calling Java from Lisp, converting the basic
;;; values, Lisp classes that extend Java classes (how Lisp functions become
;;; callable from another language), and Java exceptions as Lisp conditions.

(defpackage #:openldk.interop
  (:use #:cl)
  (:export #:start-runtime
           #:find-classpath
           #:classpath-from-environment
           #:runtime-started-p
           #:java-load
           #:java-new
           #:java-call
           #:java-static
           #:java-static-field
           #:java-instance-p
           #:java-class-symbol
           #:jstring
           #:lstring
           #:java-double
           #:java-array-list
           #:make-java-array-of
           #:foreign-error
           #:foreign-error-language
           #:foreign-error-message
           #:foreign-error-object
           #:with-foreign-errors
           #:make-foreign-error
           #:foreign-error-java-class
           #:retry
           #:foreign-arithmetic-error
           #:foreign-index-error
           #:foreign-null-pointer-error
           #:foreign-class-cast-error
           #:foreign-number-format-error
           #:foreign-illegal-argument-error
           #:foreign-illegal-state-error
           #:foreign-unsupported-operation-error
           #:foreign-io-error
           #:foreign-file-not-found-error
           #:foreign-stack-overflow
           #:foreign-out-of-memory
           #:define-java-subclass
           #:make-java-subclass-instance
           #:call-from-java
           #:define-source-syntax
           #:enable-source-syntax
           #:foreign-function
           #:foreign-function-object
           #:foreign-function-language
           #:make-foreign-function
           #:foreign-object
           #:java-sequence
           #:java-sequence-object
           #:do-java-iterator
           #:java-take
           #:lisp-list-view
           #:lisp-map-view
           #:java-fields
           #:add-to-classpath
           #:maven-jar
           #:add-maven-jar
           #:compile-foreign-file
           #:load-foreign-file
           #:define-foreign-file-type
           #:define-java-method
           #:define-java-class
           #:make-java-object
           #:java-class-of-lisp-class
           #:dump-lisp-image))

(in-package #:openldk.interop)

;;; Runtime

(defvar *runtime-started* nil)

(defvar *classpath-entries* '()
  "Classpath entries added by START-RUNTIME, in order.")

(defun runtime-started-p ()
  *runtime-started*)

(defun start-runtime (&key classpath)
  "Start the OpenLDK Java runtime if it isn't running, and add CLASSPATH
(a list of jar files and directories, or a colon-separated string) to the
classpath.  Several language systems can each add their own entries."
  (let ((entries (if (stringp classpath)
                     (split-sequence:split-sequence #\: classpath :remove-empty-subseqs t)
                     (mapcar #'namestring classpath))))
    (unless *runtime-started*
      (openldk:initialize)
      (setf *runtime-started* t))
    (dolist (entry entries)
      (pushnew entry *classpath-entries* :test #'string=))
    ;; The setup MAIN performs before running a class: class loaders,
    ;; system properties, and the classpath (rebuilt from all entries).
    (openldk::%main-runtime-setup (format nil "~{~A~^:~}" (reverse *classpath-entries*))
                                  nil nil)
    t))

;;; Finding a language's jars
;;;
;;; A language's classes come from, in order: the CLASSPATH environment
;;; variable, if it already has them; the copies `make <language>` leaves in
;;; lib/; or else pinned downloads into OpenLDK's cache (see src/fetch.lisp).

(defun %entry-provides-p (entry resource)
  "True if the classpath ENTRY (a jar or directory) contains RESOURCE."
  (if (uiop:directory-exists-p entry)
      (uiop:file-exists-p (merge-pathnames resource (uiop:ensure-directory-pathname entry)))
      (and (uiop:file-exists-p entry)
           (ignore-errors
            (zip:with-zipfile (zip entry)
              (and (zip:get-zipfile-entry resource zip) t))))))

(defun classpath-from-environment (class)
  "CLASSPATH's entries, if one of them provides the Java class CLASS."
  (let ((resource (concatenate 'string class ".class"))
        (entries (split-sequence:split-sequence #\: (or (uiop:getenv "CLASSPATH") "")
                                                :remove-empty-subseqs t)))
    (when (some (lambda (entry) (%entry-provides-p entry resource)) entries)
      entries)))

(defun find-classpath (class jars)
  "Classpath entries that provide the Java class CLASS (\"kawa/standard/Scheme\").
JARS lists (LIB-PATH CACHE-PATH URL SHA256) for each jar the language
needs: where `make` puts it, relative to OpenLDK's sources; where to
keep it in OpenLDK's cache; and where to download it from."
  (or (classpath-from-environment class)
      (loop for (lib-path cache-path url sha256) in jars
            collect (namestring
                     (or (uiop:file-exists-p (asdf:system-relative-pathname "openldk" lib-path))
                         (openldk::ensure-cached-file cache-path url sha256))))))

;;; Calling Java
;;;
;;; Methods and constructors are named as OpenLDK names them: the method
;;; name and argument descriptor, e.g. "valueOf(D)" or "<init>(Ljava/lang/String;)".

(defun %binary-name (class-name)
  (substitute #\/ #\. class-name))

(defun java-load (class-name)
  "Load and initialize the Java class CLASS-NAME (\"java.lang.String\" or
\"java/lang/String\"), and return OpenLDK's class object for it."
  (let ((class (openldk::classload (%binary-name class-name))))
    (unless class
      (error "Java class ~A not found" class-name))
    (openldk::%clinit class)
    class))

(defun java-new (class-name constructor &rest args)
  "Construct an instance of CLASS-NAME with the constructor CONSTRUCTOR,
\"<init>(...)\" or just its argument descriptor \"(...)\"."
  (let* ((class (java-load class-name))
         (object (openldk::%make-java-instance (openldk::name class)
                                               (openldk::ldk-loader class)))
         (init (if (char= (char constructor 0) #\()
                   (concatenate 'string "<init>" constructor)
                   constructor))
         ;; Constructors return void: method names carry return types.
         (init (if (char= (char init (1- (length init))) #\)) (concatenate 'string init "V") init)))
    (apply (symbol-function (intern init :openldk)) object args)
    object))

(defun java-call (method object &rest args)
  "Call the instance method METHOD, e.g. \"toString()\" (the return type
may be given too), on OBJECT."
  (apply (symbol-function
          (intern (if (typep object 'standard-object)
                      ;; A DEFINE-JAVA-SUBCLASS instance resolves through its Java superclass.
                      (or (loop for class in (closer-mop:class-precedence-list (class-of object))
                                for name = (class-name class)
                                for full = (and (symbolp name)
                                                (or (eq class (class-of object)) (find #\/ (symbol-name name)))
                                                (openldk::%resolve-method-name (symbol-name name) method))
                                when (and full (string/= full method))
                                  return full)
                          method)
                      method)
                  :openldk))
         object args))

(defun java-static (class-name method &rest args)
  "Call the static method METHOD of CLASS-NAME, e.g. (java-static
\"java.lang.Double\" \"valueOf(D)\" 1d0)."
  (let* ((class (java-load class-name))
         (symbol (openldk::static-method-symbol
                  (format nil "~A.~A" (openldk::name class)
                          (openldk::%resolve-method-name (openldk::name class) method))
                  (openldk::ldk-class-package class))))
    (apply (symbol-function symbol) args)))

(defun java-static-field (class-name field)
  "The value of the static field FIELD of CLASS-NAME."
  (let ((class (java-load class-name)))
    (slot-value (openldk::%static-storage (openldk::name class))
                (intern (openldk::mangle-field-name field) :openldk))))

(defun java-class-symbol (class-name)
  "The CLOS class name of the Java class CLASS-NAME, for TYPEP."
  (let ((class (java-load class-name)))
    (intern (openldk::name class) (openldk::ldk-class-package class))))

(defun java-instance-p (object class-name)
  "True when OBJECT is an instance of the Java class CLASS-NAME."
  (typep object (java-class-symbol class-name)))

;;; Values

(defun jstring (string)
  "A java.lang.String for the Lisp STRING."
  (openldk::jstring string))

(defun lstring (java-string)
  "The Lisp string for a java.lang.String."
  (openldk::lstring java-string))

(defun java-double (number)
  "A java.lang.Double for NUMBER."
  (java-static "java.lang.Double" "valueOf(D)" (coerce number 'double-float)))

(defun java-array-list (array)
  "The elements of the Java array ARRAY, as a list."
  (coerce (openldk::java-array-data array) 'list))

(defun make-java-array-of (component-class-name contents)
  "A Java array of COMPONENT-CLASS-NAME (\"char\", \"java.lang.Object\", ...)
holding the elements of the sequence CONTENTS."
  (openldk::make-java-array
   :component-class (or (openldk::%get-java-class-by-fq-name component-class-name t)
                        (openldk::java-class (java-load component-class-name)))
   :initial-contents (coerce contents 'vector)))

;;; Errors
;;;
;;; A Lisp error raised in a callback (a Lisp method called from Java) is
;;; carried back out through the Java frames as a java.lang.RuntimeException
;;; that remembers it; WITH-FOREIGN-ERRORS re-signals the original condition.
;;; Left alone, the generated Java code's handlers would turn it into a Java
;;; exception on the way (a TYPE-ERROR into a NullPointerException).

(defvar *lisp-errors-in-java* (make-hash-table :test #'eq :weakness :key :synchronized t)
  "java.lang.RuntimeException -> the Lisp condition it carries.")

(define-condition foreign-error (error)
  ((language :initarg :language :reader foreign-error-language)
   (message :initarg :message :reader foreign-error-message)
   (object :initarg :object :initform nil :reader foreign-error-object
           :documentation "The Java throwable, for language-specific details."))
  (:report (lambda (condition stream)
             (format stream "~A error: ~A"
                     (foreign-error-language condition)
                     (foreign-error-message condition)))))

(macrolet ((define-foreign-errors (&rest specs)
             `(progn
                ,@(loop for (name . supers) in specs
                        collect `(define-condition ,name (,@supers) ())))))
  (define-foreign-errors
    (foreign-arithmetic-error foreign-error arithmetic-error)
    (foreign-index-error foreign-error)
    (foreign-null-pointer-error foreign-error)
    (foreign-class-cast-error foreign-error type-error)
    (foreign-illegal-argument-error foreign-error)
    (foreign-number-format-error foreign-illegal-argument-error parse-error)
    (foreign-illegal-state-error foreign-error)
    (foreign-unsupported-operation-error foreign-error)
    (foreign-io-error foreign-error)
    (foreign-file-not-found-error foreign-io-error file-error)
    (foreign-stack-overflow foreign-error storage-condition)
    (foreign-out-of-memory foreign-error storage-condition)))

(defun %throwable-message (throwable)
  "THROWABLE's message, followed by those of its causes: a wrapper's own
message (Clojure's \"Syntax error compiling ...\") often says less than
its cause's."
  (flet ((message-of (throwable)
           (let ((message (ignore-errors (java-call "getMessage()" throwable))))
             (lstring (or message (java-call "toString()" throwable))))))
    (let ((messages (loop for cause = throwable
                            then (ignore-errors (java-call "getCause()" cause))
                          for depth below 10
                          while cause
                          collect (message-of cause))))
      (format nil "~A~{ (caused by: ~A)~}" (first messages)
              (remove-duplicates (rest messages) :test #'string= :from-end t)))))

(defun %carried-lisp-condition (throwable)
  "The Lisp condition THROWABLE, or an exception it was wrapped in on the
way out (a CompletionException from another thread), carries."
  (loop for cause = throwable then (ignore-errors (java-call "getCause()" cause))
        for depth below 10
        while cause
        thereis (gethash cause *lisp-errors-in-java*)))

(defparameter *foreign-error-classes*
  '(("java.lang.ArithmeticException" foreign-arithmetic-error)
    ("java.lang.IndexOutOfBoundsException" foreign-index-error)
    ("java.lang.NullPointerException" foreign-null-pointer-error)
    ("java.lang.ClassCastException" foreign-class-cast-error)
    ("java.lang.NumberFormatException" foreign-number-format-error)
    ("java.lang.IllegalArgumentException" foreign-illegal-argument-error)
    ("java.lang.IllegalStateException" foreign-illegal-state-error)
    ("java.lang.UnsupportedOperationException" foreign-unsupported-operation-error)
    ("java.io.FileNotFoundException" foreign-file-not-found-error)
    ("java.io.IOException" foreign-io-error)
    ("java.lang.StackOverflowError" foreign-stack-overflow)
    ("java.lang.OutOfMemoryError" foreign-out-of-memory))
  "Java exception class -> the FOREIGN-ERROR subclass a throwable of it (or
a subclass) is signalled as; the first that matches, so a subclass comes
before its superclass.")

(defun %foreign-error-class (throwable)
  (or (loop for (java-class condition-class) in *foreign-error-classes*
            when (ignore-errors (java-instance-p throwable java-class))
              return condition-class)
      'foreign-error))

(defun make-foreign-error (language throwable)
  "The FOREIGN-ERROR (of the subclass THROWABLE's Java class maps to) for
the Java THROWABLE in LANGUAGE."
  (let ((class (%foreign-error-class throwable)))
    (apply #'make-condition class
           :language language :message (%throwable-message throwable) :object throwable
           ;; The standard condition types it is also an instance of.
           (case class
             (foreign-class-cast-error (list :datum throwable :expected-type t))
             (foreign-file-not-found-error (list :pathname nil))
             (foreign-arithmetic-error (list :operation nil :operands nil))))))

(defun foreign-error-java-class (condition)
  "The name of the Java class of the throwable behind CONDITION, a
FOREIGN-ERROR (\"java.lang.ArithmeticException\"), or NIL."
  (let ((object (foreign-error-object condition)))
    (and object (typep object 'standard-object)
         (ignore-errors (lstring (java-call "getName()" (java-call "getClass()" object)))))))

(defun %signal-foreign-error (condition)
  "Signal CONDITION with the USE-VALUE restart, whose value the foreign
call then returns; WITH-FOREIGN-ERRORS adds RETRY."
  (restart-case (error condition)
    (use-value (value)
      :report "Return a value instead of the foreign call's."
      :interactive (lambda () (list (cl:eval (read *query-io*))))
      value)))

(defmacro with-foreign-errors ((language) &body body)
  "Run BODY, turning a Java exception that escapes it into a FOREIGN-ERROR
for LANGUAGE (a string) -- of the subclass its Java class maps to, such as
FOREIGN-ARITHMETIC-ERROR, also an ARITHMETIC-ERROR -- or into the Lisp
condition it carries if it came from CALL-FROM-JAVA.  The restarts
USE-VALUE (return a value instead) and RETRY (run BODY again) are offered."
  (let ((retry (gensym "RETRY")) (block (gensym "WITH-FOREIGN-ERRORS")))
    `(block ,block
       (tagbody
          ,retry
          (return-from ,block
            (restart-case
                (handler-case (progn ,@body)
                  (openldk::|condition-java/lang/Throwable| (condition)
                    (let* ((throwable (slot-value condition 'openldk::|objref|))
                           (lisp-condition (%carried-lisp-condition throwable)))
                      (%signal-foreign-error
                       (or lisp-condition (make-foreign-error ,language throwable))))))
              (retry ()
                :report "Make the foreign call again."
                (go ,retry))))))))

(defparameter *lisp-error-java-classes*
  '((division-by-zero "java.lang.ArithmeticException")
    (arithmetic-error "java.lang.ArithmeticException")
    (type-error "java.lang.ClassCastException")
    (parse-error "java.lang.NumberFormatException")
    (file-error "java.io.UncheckedIOException")
    (storage-condition "java.lang.OutOfMemoryError"))
  "Lisp condition type -> the Java exception class a Lisp error of it is
thrown into Java as; others are java.lang.RuntimeException.")

(defun %java-exception-for (condition)
  "The Java exception CONDITION, a Lisp error, is thrown into Java as."
  (let ((class (or (loop for (type java-class) in *lisp-error-java-classes*
                         when (typep condition type) return java-class)
                   "java.lang.RuntimeException"))
        (message (jstring (princ-to-string condition))))
    (if (string= class "java.io.UncheckedIOException")
        (java-new class "(Ljava/lang/String;Ljava/io/IOException;)" message
                  (java-new "java.io.IOException" "(Ljava/lang/String;)" message))
        (java-new class "(Ljava/lang/String;)" message))))

(defun call-from-java (function &rest args)
  "Call FUNCTION with ARGS on behalf of Java code (a callback).  A Lisp
error is thrown into the Java code as the Java exception its type maps to
-- a DIVISION-BY-ZERO as an ArithmeticException, a TYPE-ERROR as a
ClassCastException, others as a RuntimeException -- carrying it, which
WITH-FOREIGN-ERRORS turns back into the Lisp condition."
  (handler-case (apply function args)
    (error (condition)
      (let ((exception (%java-exception-for condition)))
        (setf (gethash exception *lisp-errors-in-java*) condition)
        (error (openldk::%lisp-condition exception))))))

;;; Lisp classes that extend Java classes
;;;
;;; OpenLDK's Java classes are CLOS classes, so a Lisp class can extend one
;;; and implement its methods with DEFMETHOD: Java code calling those
;;; methods dispatches to the Lisp ones.  This is how a language system
;;; makes a Lisp function callable from that language, with no bytecode
;;; generated.  The Java superclass only exists once its jar is loaded, so
;;; DEFINE-JAVA-SUBCLASS defines an installer, INSTALL-<NAME>, which the
;;; language system calls after loading it.

(defmacro define-java-subclass (name java-superclass slots &body methods)
  "Define INSTALL-NAME, which defines the Lisp class NAME extending the Java
class JAVA-SUPERCLASS, with SLOTS, and METHODS, each (\"method(descriptor)\"
(self arg...) body...), implementing Java methods."
  (let ((installer (intern (format nil "INSTALL-~A" (symbol-name name)))))
    `(defun ,installer ()
       (let* ((java-class (java-load ,java-superclass))
              (super (intern (openldk::name java-class)
                             (openldk::ldk-class-package java-class))))
         (eval `(defclass ,',name (,super) ,',slots))
         ,@(loop for (method lambda-list . body) in methods
                 collect `(eval `(defmethod ,(intern (openldk::%resolve-method-name
                                                      (openldk::name java-class) ,method)
                                                     :openldk)
                                     ((,',(first lambda-list) ,',name) ,@',(rest lambda-list))
                                   ,@',body)))
         ',name))))

(defun make-java-subclass-instance (class &rest initargs)
  "Make an instance of CLASS, defined with DEFINE-JAVA-SUBCLASS, and run
its Java superclass's no-argument constructor, which initializes the Java
fields MAKE-INSTANCE alone leaves unset."
  (let ((object (apply #'make-instance class initargs)))
    (java-call "<init>()" object)
    object))

;;; Development images

(defun %restart-runtime ()
  "SB-EXT:*INIT-HOOKS* function: redo OpenLDK's per-run setup (system
properties such as user.dir, the VM start time) in a restarted image."
  (when *runtime-started*
    (openldk::%main-runtime-setup (format nil "~{~A~^:~}" (reverse *classpath-entries*))
                                  nil nil)))

(defvar *image-systems* nil
  "In an image DUMP-LISP-IMAGE saved, a hash set of the names of the
OpenLDK systems it holds.")

(defmethod asdf:perform :around ((operation asdf:operation) (component asdf:component))
  ;; ASDF plans actions on an immutable system when its fasls are newer than
  ;; the image's record of loading them -- after any other build shares the
  ;; fasl cache -- and recompiling or reloading OpenLDK into the image fails.
  ;; The image's systems are complete: do nothing to them.
  (unless (and *image-systems*
               (gethash (asdf:component-name (asdf:component-system component)) *image-systems*))
    (call-next-method)))

(defun dump-lisp-image (path &key init)
  "Save an SBCL executable at PATH whose top level is the Lisp REPL, after
calling INIT, which typically starts a language and evaluates some code so
its Java methods are compiled into the image.  A language that takes
minutes to start (Clojure) is then ready in under a second."
  (openldk::%fasl-cache-start-build)
  (when init (cl:funcall init))
  ;; JDK code programs run once (SecureRandom, time zones, String.format...)
  ;; compiled into the image too, its per-run state reset; see
  ;; OPENLDK::%WARM-UP-JDK.  The classpath the languages built stays.
  (when *runtime-started*
    (openldk::%warm-up-jdk
     (lambda (directory)
       (add-to-classpath directory)
       (java-static "openldk.warmup.JdkWarmup" "main([Ljava/lang/String;)V"
                    (make-java-array-of "java.lang.String" '())))))
  (pushnew '%restart-runtime sb-ext:*init-hooks*)
  ;; OpenLDK's systems are part of the image now.  Reloading one into it --
  ;; as a system that :DEFSYSTEM-DEPENDS-ON a language would, once any
  ;; source is newer than its fasl (after a git pull) -- fails, its Java
  ;; classes being loaded: ASDF leaves an immutable system as it is.
  (setf *image-systems* (make-hash-table :test #'equal))
  (dolist (name (asdf:already-loaded-systems))
    (when (or (string= name "openldk") (eql 0 (search "openldk/" name)))
      (asdf:register-immutable-system name)
      (setf (gethash name *image-systems*) t)))
  ;; The languages' compiled Java methods fill most of immobile space, where
  ;; SBCL loads a fasl's code by default and has no fallback when it's full:
  ;; load users' fasls into dynamic space.
  (setf sb-c:*compile-file-to-memory-space* :dynamic)
  (openldk::%reset-runtime-for-save)
  (sb-ext:save-lisp-and-die path :executable t :save-runtime-options t))


;;; Source syntax for languages Lisp's reader can't read
;;;
;;; #@scala{ ... } and #@kotlin{ ... } (or #@sc{ ... }, #@kt{ ... }) hold
;;; Scala or Kotlin source, as those languages write it, and evaluate it
;;; where it appears; #@clojure{ ... } (#@clj{ ... }) holds Clojure.  $name, $(form)
;;; and $#'function splice in Lisp values (and $,x in a backquoted macro
;;; template), outside the language's strings and comments.  Each language
;;; system registers itself with DEFINE-SOURCE-SYNTAX;
;;; (ENABLE-SOURCE-SYNTAX) installs #@ in a readtable.

(defvar *source-syntaxes* '()
  "Alist of language name (downcased) -> (expander . reader).")

(defun define-source-syntax (names expander &key reader)
  "Make #@NAME{ ... } expand with EXPANDER, for NAME (a string) or each of
NAMES (a language and its abbreviations).  EXPANDER is a function of the
source (each splice replaced by a parameter name), the parameter names,
and the Lisp forms whose values they take.  READER, a function of a stream
after the { returning those three, reads the source; by default it reads
C-family source (Scala, Kotlin)."
  (dolist (name (if (listp names) names (list names)) names)
    (let ((entry (assoc name *source-syntaxes* :test #'string-equal))
          (value (cons expander (or reader #'%read-braced-source-and-names))))
      (if entry
          (setf (cdr entry) value)
          (push (cons (string-downcase name) value) *source-syntaxes*)))))

(defun %splice-name-char-p (char)
  (or (alphanumericp char) (find char "-*_?!")))

(defun %read-braced-source (stream &key (close #\}) (string-quotes '(#\")))
  "Read C-family source (Scala, Kotlin, Groovy) from STREAM up to the } that
closes the { already read.  STRING-QUOTES are the characters that delimit
strings (and, tripled, raw strings); a ' that isn't one starts a character
literal.  Return the source with each $ splice replaced by a parameter name
(lispArg1, ...), and the list of Lisp forms spliced in."
  (let ((depth 0)
        (forms '()))
    (labels ((next () (read-char stream t nil t))
             (peek () (peek-char nil stream nil nil t))
             (copy-string (quote out)
               ;; QUOTE has been written: copy a string, or a raw string
               ;; tripled QUOTEs delimit, through its end.
               (if (eql (peek) quote)
                   (progn
                     (write-char (next) out)
                     (when (eql (peek) quote) ; tripled: a raw string
                       (write-char (next) out)
                       (loop with quotes = 0
                             for c = (next)
                             do (write-char c out)
                                (setf quotes (if (char= c quote) (1+ quotes) 0))
                             until (>= quotes 3))))
                   (loop for c = (next)
                         do (write-char c out)
                            (cond ((char= c #\\) (write-char (next) out))
                                  ((char= c quote) (return)))))))
      (values
       (with-output-to-string (out)
         (loop for char = (next)
               do (cond
                    ((member char '(#\{ #\( #\[)) (incf depth) (write-char char out))
                    ((and (char= char close) (zerop depth)) (loop-finish))
                    ((member char '(#\) #\])) (decf depth) (write-char char out))
                    ((char= char #\})
                     (if (zerop depth)
                         (loop-finish)
                         (progn (decf depth) (write-char char out))))
                    ((member char string-quotes)
                     (write-char char out)
                     (copy-string char out))
                    ((char= char #\') ; a character literal
                     (write-char char out)
                     (let ((c (next)))
                       (write-char c out)
                       (when (char= c #\\) (write-char (next) out)))
                     (when (eql (peek) #\') (write-char (next) out)))
                    ((char= char #\/) ; a comment, or division
                     (case (peek)
                       (#\/ (loop for c = (read-char stream nil #\Newline t)
                                  until (char= c #\Newline))
                            (write-char #\Newline out))
                       (#\* (next)
                            (loop with star = nil
                                  for c = (next)
                                  until (and star (char= c #\/))
                                  do (setf star (char= c #\*)))
                            (write-char #\Space out))
                       (t (write-char char out))))
                    ((char= char #\$) ; a Lisp value
                     (let ((form (if (member (peek) '(#\( #\, #\#))
                                     (read stream t nil t)
                                     (read-from-string
                                      (with-output-to-string (token)
                                        (loop for c = (peek)
                                              while (and c (%splice-name-char-p c))
                                              do (write-char (next) token)))))))
                       (push form forms)
                       (format out "lispArg~D" (length forms))))
                    (t (write-char char out)))))
       (nreverse forms)))))

(defun %lisp-token-end-p (char)
  (or (member char '(#\( #\) #\[ #\] #\{ #\} #\" #\; #\,))
      (member char '(#\Space #\Tab #\Newline #\Return #\Page))))

(defun %copy-block-comment (stream output)
  "Copy a nested #| comment after its opening delimiter has been copied."
  (loop with depth = 1
        for char = (read-char stream t nil t)
        do (write-char char output)
           (cond ((and (char= char #\#) (eql (peek-char nil stream nil nil t) #\|))
                  (write-char (read-char stream) output)
                  (incf depth))
                 ((and (char= char #\|) (eql (peek-char nil stream nil nil t) #\#))
                  (write-char (read-char stream) output)
                  (decf depth)))
        until (zerop depth)))

(defun %read-lisp-family-source (stream close name-for &key escaped-symbols)
  "Read Clojure or Scheme source from STREAM up to the CLOSE (], } or )) that
closes it.  Return the source, with the Ith $ splice replaced by (funcall
NAME-FOR I), the list of those names, and the list of the Lisp forms whose
values they take."
  (let ((depth 0)
        (splices '()))
    (values
     (with-output-to-string (out)
       (loop for char = (read-char stream t nil t)
             do (case char
                  ((#\( #\[ #\{) (incf depth) (write-char char out))
                  ((#\) #\] #\}) (if (and (zerop depth) (char= char close))
                                     (loop-finish)
                                     (progn (decf depth) (write-char char out))))
                  (#\" ; a string: copy to its closing quote
                   (write-char char out)
                   (loop for c = (read-char stream t nil t)
                         do (write-char c out)
                            (case c
                              (#\\ (write-char (read-char stream t nil t) out))
                              (#\" (return)))))
                  (#\\ ; a character literal: \( isn't an open paren
                   (write-char char out)
                   (write-char (read-char stream t nil t) out))
                  (#\#
                   (write-char char out)
                   (when (eql (peek-char nil stream nil nil t) #\|)
                     (write-char (read-char stream) out)
                     (%copy-block-comment stream out)))
                  (#\|
                   (write-char char out)
                   (when escaped-symbols
                     (loop for c = (read-char stream t nil t)
                           do (write-char c out)
                              (cond ((char= c #\\) (write-char (read-char stream t nil t) out))
                                    ((char= c #\|) (return))))))
                  (#\; ; a comment, to the end of the line
                   (loop for c = (read-char stream nil #\Newline t)
                         until (char= c #\Newline))
                   (write-char #\Newline out))
                  (#\$ ; a Lisp value
                   ;; $(form), $#'f or $#@lang{ ... }, or $,x in a
                   ;; backquoted macro template, is read as Lisp; $name
                   ;; up to a delimiter.
                   (let ((form (if (member (peek-char nil stream t nil t) '(#\( #\, #\#))
                                   (read stream t nil t)
                                   (read-from-string
                                    (with-output-to-string (token)
                                      (loop for c = (peek-char nil stream nil nil t)
                                            while (and c (not (%lisp-token-end-p c)))
                                            do (write-char (read-char stream) token))))))
                         (name (funcall name-for (1+ (length splices)))))
                     (push (cons name form) splices)
                     (write-string name out)))
                  (t (write-char char out)))))
     (mapcar #'car (reverse splices))
     (mapcar #'cdr (reverse splices)))))

(defun %read-lisp-family-source-and-names (stream)
  "The source of #@LANGUAGE{ ... } for a Lisp-family language, with
splices named lispArg1, ..."
  (%read-lisp-family-source stream #\} (lambda (i) (format nil "lispArg~D" i))))

(defun %read-braced-source-and-names (stream &rest options)
  (multiple-value-bind (source forms) (apply #'%read-braced-source stream options)
    (values source
            (loop for i from 1 to (length forms) collect (format nil "lispArg~D" i))
            forms)))

(defun %read-source-syntax (stream subchar arg)
  (declare (ignore subchar arg))
  (let ((name (with-output-to-string (out)
                (loop for c = (read-char stream t nil t)
                      until (char= c #\{)
                      unless (member c '(#\Space #\Tab #\Newline))
                        do (write-char c out)))))
    (let ((syntax (cdr (assoc name *source-syntaxes* :test #'string-equal))))
      (unless syntax
        (error "#@~A{ ... }: no such language; load its system (~{~A~^, ~} are known)"
               name (mapcar #'car *source-syntaxes*)))
      (multiple-value-call (car syntax) (funcall (cdr syntax) stream)))))

(defun enable-source-syntax (&optional (readtable *readtable*))
  "Make #@LANGUAGE{ ... } read LANGUAGE's source in READTABLE (by default,
the current one); see above."
  (set-dispatch-macro-character #\# #\@ #'%read-source-syntax readtable)
  (%enable-function-definition-syntax readtable)
  readtable)

;;; Native function bodies, without an enclosing #@LANGUAGE{ } block.

(defvar *function-syntaxes* nil
  "Definition name -> (macro symbol . dialect), registered by language adapters.")

(defvar *function-list-readers* (make-hash-table :test #'eq :weakness :key)
  "Installed list reader functions, so enabling a copied readtable is idempotent.")

(defun %register-function-syntax (macro dialect)
  (setf (getf *function-syntaxes* (intern (symbol-name macro) :keyword))
        (cons macro dialect)))

(defun %function-syntax (token)
  (when token
    (let* ((colon (position #\: token :from-end t))
           (name (string-upcase (subseq token (if colon (1+ colon) 0))))
           (entry (loop for (key value) on *function-syntaxes* by #'cddr
                        when (string= name (symbol-name key)) return value)))
      ;; Bare names are syntax keywords; qualified names must name the actual
      ;; exported macro. Do not commandeer another package's same-named function.
      (when (and entry
                 (or (not colon)
                     (let ((package (find-package (string-upcase (subseq token 0 (position #\: token))))))
                       (and package (eq (find-symbol name package) (car entry))))))
        entry))))

(defun %function-definition-prefix (stream)
  "Look ahead only through whitespace, comments and a plain head token.
Return the exact consumed text for replay by the original list reader.
No Lisp form is evaluated here, including #. forms."
  (let ((output (make-string-output-stream)))
    (labels ((next () (let ((c (read-char stream t nil t))) (write-char c output) c))
             (peek () (peek-char nil stream t nil t)))
      (loop
        (let ((char (peek)))
          (cond ((find char '(#\Space #\Tab #\Newline #\Return #\Page)) (next))
                ((char= char #\;) (loop until (char= (next) #\Newline)))
                ((char= char #\#)
                 (next)
                 (if (eql (peek) #\|)
                     (progn (next) (%copy-block-comment stream output))
                     (return (values (get-output-stream-string output) nil))))
                (t
                 (let ((token (make-string-output-stream)))
                   (loop for c = (peek)
                         while (or (alphanumericp c) (find c "-_:./"))
                         do (write-char (next) token))
                   (return (values (get-output-stream-string output)
                                   (when (or (find (peek) '(#\Space #\Tab #\Newline #\Return #\Page))
                                             (and (get-macro-character (peek))
                                                  (not (nth-value 1 (get-macro-character (peek))))))
                                     (get-output-stream-string token))))))))))))

(defun %read-function-definition (stream char original)
  (multiple-value-bind (prefix token) (%function-definition-prefix stream)
    (let ((syntax (%function-syntax token)))
      (if syntax
          (let* ((name (read stream t nil t))
                 (parameters (read stream t nil t))
                 (typed (member (cdr syntax) '(:scala :kotlin)))
                 (return-type
                   (when (and typed (eql (peek-char t stream t nil t) #\:))
                     (unless (or (eq (read stream t nil t) :returns) *read-suppress*)
                       (error "Expected :RETURNS before a native function body."))
                     (read stream t nil t))))
            (multiple-value-bind (source captures forms)
                (if typed
                    (%read-braced-source-and-names stream :close #\))
                    (%read-lisp-family-source stream #\)
                                              (lambda (i) (format nil "lispCapture~D" i))
                                              :escaped-symbols (eql (cdr syntax) :scheme)))
              (unless *read-suppress*
                (append (list (car syntax) name parameters source captures forms)
                        (when typed (list return-type))))))
          ;; Replay bytes, not printed objects: dotted lists, reader labels,
          ;; backquotes, custom readers and read-time evaluation keep their
          ;; original semantics. In particular, #. runs exactly once.
          (funcall original (make-concatenated-stream (make-string-input-stream prefix) stream) char)))))

(defun %enable-function-definition-syntax (readtable)
  (let ((original (get-macro-character #\( readtable)))
    (unless (gethash original *function-list-readers*)
      (let ((reader (lambda (stream char) (%read-function-definition stream char original))))
        (setf (gethash reader *function-list-readers*) t)
        (set-macro-character #\( reader nil readtable))))
  readtable)

(defun %expand-function-definition (name parameters source captures forms evaluator dialect)
  "Expand a native definition into a CL function closing over one compiled procedure."
  (unless (and (symbolp name) name (not (constantp name))
               (listp parameters) (alexandria:proper-list-p parameters)
               (every (lambda (p) (and (symbolp p) p (not (constantp p)) (plusp (length (symbol-name p)))
                                      (not (find (char (symbol-name p) 0) "&"))))
                      parameters)
               (= (length parameters) (length (remove-duplicates parameters)))
               (stringp source))
    (error "Native definitions need a function name, distinct required parameters, and source: ~S ~S"
           name parameters))
  (let* ((native-name (string-downcase (symbol-name name)))
         (names (mapcar (lambda (p) (let ((s (symbol-name p)))
                                     (if (string= s (string-upcase s)) (string-downcase s) s)))
                        parameters))
         (function-source
           (if (eql dialect :clojure)
               (format nil "(fn ~A [~{~A~^ ~}]~%~A~%)" native-name names source)
               (format nil "(letrec ((~A (lambda (~{~A~^ ~})~%~A~%))) ~A)"
                       native-name names source native-name)))
         (compiled
           (if captures
               `(cl:funcall (,evaluator ,(if (eql dialect :clojure)
                                            (format nil "(fn [~{~A~^ ~}] ~A)" captures function-source)
                                            (format nil "(lambda (~{~A~^ ~}) ~A)" captures function-source)))
                            ,@forms)
               `(,evaluator ,function-source)))
         (procedure (gensym "PROCEDURE")))
    `(let ((,procedure ,compiled))
       (defun ,name ,parameters
         (cl:funcall ,procedure ,@parameters)))))

;;; Java classes defined in Lisp
;;;
;;; DEFINE-JAVA-CLASS defines a Java class whose methods are Lisp code:
;;;
;;;   (define-java-class greeter (:extends "java.lang.Object"
;;;                               :implements ("java.util.function.Function"))
;;;     ((greeting :initform "hello"))
;;;     ("apply(Ljava/lang/Object;)Ljava/lang/Object;" (self name)
;;;       (jstring (format nil "~A, ~A" (slot-value self 'greeting) (lstring name)))))
;;;
;;;   (make-java-object 'greeter :greeting "hi")
;;;
;;; It is a real Java class, defined from a class file generated here: Lisp
;;; slots are fields of type Object, each method is a native method that
;;; the Lisp body implements (OpenLDK calls a native instance method
;;; through the generic function of its name), and a constructor calls the
;;; superclass's no-argument one.  So getClass, reflection, Class.forName
;;; and languages that call methods by reflection (Groovy, Clojure, Kotlin's
;;; reflection) see an ordinary Java object.

(defun %u2 (n) (list (ldb (byte 8 8) n) (ldb (byte 8 0) n)))
(defun %u4 (n) (append (%u2 (ldb (byte 16 16) n)) (%u2 (ldb (byte 16 0) n))))

(defun %modified-utf8 (string)
  "STRING in the class file's modified UTF-8, as a list of octets."
  (loop for char across string
        for code = (char-code char)
        append (cond ((and (<= 1 code) (<= code #x7F)) (list code))
                     ((<= code #x7FF) (list (logior #xC0 (ash code -6)) (logior #x80 (logand code #x3F))))
                     ((<= code #xFFFF) (list (logior #xE0 (ash code -12))
                                             (logior #x80 (logand (ash code -6) #x3F))
                                             (logior #x80 (logand code #x3F))))
                     (t ;; a supplementary character, as its UTF-16 surrogate pair
                      (let* ((v (- code #x10000))
                             (high (logior #xD800 (ash v -10)))
                             (low (logior #xDC00 (logand v #x3FF))))
                        (append (%modified-utf8 (string (code-char high)))
                                (%modified-utf8 (string (code-char low)))))))))

(defun %class-file (name super interfaces fields methods)
  "The octets of a class file for the class NAME (binary form) extending
SUPER, implementing INTERFACES, with FIELDS (names, of type Object) and
METHODS ((name . descriptor), native), and a no-argument constructor."
  (let ((pool '()) (count 0) (index (make-hash-table :test #'equal)))
    (labels ((entry (key octets)
               (or (gethash key index)
                   (progn (setf pool (append pool (list octets)))
                          (setf (gethash key index) (incf count)))))
             (utf8 (s) (let ((b (%modified-utf8 s))) (entry (list :utf8 s) (append '(1) (%u2 (length b)) b))))
             (class (n) (let ((u (utf8 n))) (entry (list :class n) (cons 7 (%u2 u)))))
             (name-and-type (n d) (let ((a (utf8 n)) (b (utf8 d)))
                                    (entry (list :nat n d) (append '(12) (%u2 a) (%u2 b)))))
             (methodref (c n d) (let ((a (class c)) (b (name-and-type n d)))
                                  (entry (list :methodref c n d) (append '(10) (%u2 a) (%u2 b))))))
      (let* ((this (class name))
             (super-index (class super))
             (interface-indices (mapcar #'class interfaces))
             (code-name (utf8 "Code"))
             (init-name (utf8 "<init>"))
             (init-type (utf8 "()V"))
             (super-init (methodref super "<init>" "()V"))
             (field-octets (loop for f in fields
                                 append (append (%u2 #x0001) (%u2 (utf8 f)) (%u2 (utf8 "Ljava/lang/Object;")) (%u2 0))))
             (init-code (append '(#x2A #xB7) (%u2 super-init) '(#xB1))) ; aload_0, invokespecial, return
             (init-octets (append (%u2 #x0001) (%u2 init-name) (%u2 init-type) (%u2 1)
                                  (%u2 code-name) (%u4 (+ 12 (length init-code)))
                                  (%u2 1) (%u2 1) (%u4 (length init-code)) init-code (%u2 0) (%u2 0)))
             (method-octets (loop for (n . d) in methods
                                  append (append (%u2 #x0101) (%u2 (utf8 n)) (%u2 (utf8 d)) (%u2 0)))))
        (coerce (append '(#xCA #xFE #xBA #xBE) (%u2 0) (%u2 52)
                        (%u2 (1+ count)) (apply #'append pool)
                        (%u2 #x0021) (%u2 this) (%u2 super-index)
                        (%u2 (length interface-indices)) (mapcan #'%u2 interface-indices)
                        (%u2 (length fields)) field-octets
                        (%u2 (1+ (length methods))) init-octets method-octets
                        (%u2 0))
                '(vector (unsigned-byte 8)))))))

(defun %java-class-name (symbol)
  "The binary name of the Java class DEFINE-JAVA-CLASS makes for SYMBOL:
lisp/<package>/<name>, lowercased, other characters than letters and
digits becoming _ (OPENLDK.INTEROP::GREETER: lisp/openldk_interop/greeter)."
  (flet ((munge (s)
           (map 'string (lambda (c) (if (alphanumericp c) (char-downcase c) #\_)) s)))
    (format nil "lisp/~A/~A" (munge (package-name (symbol-package symbol))) (munge (symbol-name symbol)))))

(defvar *java-classes* (make-hash-table :test #'eq)
  "Lisp symbol -> the <class> DEFINE-JAVA-CLASS made for it.")

(defun %define-java-class (symbol super interfaces slots methods)
  "Define the Java class for SYMBOL from a generated class file, through
the application class loader, and return its <class>."
  (let* ((name (%java-class-name symbol))
         (bytes (%class-file name (%binary-name super) (mapcar #'%binary-name interfaces)
                             (mapcar #'%java-field-name slots) methods))
         (class (openldk::%classload-from-stream
                 name (flexi-streams:make-in-memory-input-stream bytes)
                 openldk::*boot-class-loader* openldk::*app-ldk-class-loader*)))
    (unless class
      (error "Couldn't define the Java class ~A" name))
    (setf (gethash symbol *java-classes*) class)))

(defun java-class-of-lisp-class (symbol)
  "The <class> of the Java class DEFINE-JAVA-CLASS defined for SYMBOL."
  (or (gethash symbol *java-classes*)
      (error "~S names no class defined with DEFINE-JAVA-CLASS" symbol)))

(defun %method-parts (method)
  "METHOD, \"name(descriptor)return\", as (name . descriptor)."
  (let ((open (position #\( method)))
    (cons (subseq method 0 open) (subseq method open))))

(defmacro define-java-class (name (&key (extends "java.lang.Object") implements) slots &body methods)
  "Define NAME as a Java class extending EXTENDS (which needs a public
no-argument constructor) and implementing IMPLEMENTS, with SLOTS -- each a
symbol or (symbol &key initform) -- and METHODS, each
(\"name(arguments)return\" (self arg...) body...), the full JVM name of a
method the class declares or implements.  Starts the runtime if needed;
make instances with MAKE-JAVA-OBJECT."
  (let ((slot-names (mapcar (lambda (s) (if (consp s) (first s) s)) slots)))
    `(progn
       (unless (runtime-started-p) (start-runtime))
       (%define-java-class ',name ,extends ',implements
                           ',(mapcar #'symbol-name slot-names)
                           ',(mapcar (lambda (m) (%method-parts (first m))) methods))
       (setf (get ',name 'java-slot-initforms)
             (list ,@(loop for s in slots
                           collect (if (consp s)
                                       `(cons ',(first s) (lambda () ,(getf (rest s) :initform)))
                                       `(cons ',s (lambda () nil))))))
       (%install-java-methods
        ',name
        (list ,@(loop for (method lambda-list . body) in methods
                      for declarations = (loop while (and (consp (first body)) (eq (first (first body)) 'declare))
                                               collect (pop body))
                      collect `(list ,method ,(length lambda-list)
                                     (lambda ,lambda-list
                                       (declare (ignorable ,(first lambda-list)))
                                       ,@declarations
                                       (symbol-macrolet ,(loop for s in slot-names
                                                               collect `(,s (%java-slot ,(first lambda-list) ',s)))
                                         (call-from-java (lambda () ,@body))))))))
       ',name)))

(defun %install-java-methods (name methods)
  "Make each of METHODS, (jvm-name arity function), the method of NAME's
Java class: a method on the generic function of JVM-NAME for the class's
CLOS class, calling FUNCTION."
  (let* ((class (java-class-of-lisp-class name))
         (class-symbol (intern (openldk::name class) (openldk::ldk-class-package class))))
    (loop for (jvm-name arity function) in methods
          for gf = (intern jvm-name :openldk)
          for params = (loop for i from 1 below arity collect (intern (format nil "ARG~D" i) :openldk))
          do (openldk::%ensure-java-gf gf arity)
             (cl:eval `(defmethod ,gf ((this ,class-symbol) ,@params)
                         (funcall ,function this ,@params))))))

(defun %java-field-name (slot-name)
  "The Java field holding the Lisp slot SLOT-NAME: prefixed, so it
doesn't shadow a superclass field."
  (concatenate 'string "lisp$" slot-name))

(defun %java-slot-symbol (slot)
  (intern (openldk::mangle-field-name (%java-field-name (symbol-name slot))) :openldk))

(defun %java-slot (object slot)
  (slot-value object (%java-slot-symbol slot)))

(defun (setf %java-slot) (value object slot)
  (setf (slot-value object (%java-slot-symbol slot)) value))

(defun make-java-object (name &rest initargs)
  "A new instance of the Java class NAME defined with DEFINE-JAVA-CLASS,
its constructor run and its slots set from INITARGS (:slot value ...) or
their initforms."
  (let* ((class (java-class-of-lisp-class name))
         (object (openldk::%make-java-instance (openldk::name class) (openldk::ldk-loader class))))
    (java-call "<init>()V" object)
    (loop for (slot . initform) in (get name 'java-slot-initforms)
          do (setf (%java-slot object slot)
                   (getf initargs (intern (symbol-name slot) :keyword) (funcall initform))))
    object))

;;; Foreign functions as Lisp functions
;;;
;;; A language's function object -- a Clojure fn, a Scala or Kotlin
;;; function, a Groovy closure, a Scheme procedure, an ABCL function -- comes
;;; to Lisp as a FOREIGN-FUNCTION: a funcallable instance, so FUNCALL, APPLY,
;;; MAPCAR and SORT call it like any Lisp function, converting arguments and
;;; result as its language's FUNCALL does.  Passed back to a language, it is
;;; the original object again.

(defclass foreign-function ()
  ((object :initarg :object :reader foreign-function-object
           :documentation "The language's function object.")
   (language :initarg :language :reader foreign-function-language))
  (:metaclass closer-mop:funcallable-standard-class))

(defmethod print-object ((function foreign-function) stream)
  (print-unreadable-object (function stream :type t :identity t)
    (princ (foreign-function-language function) stream)))

(defun make-foreign-function (object language caller)
  "A FOREIGN-FUNCTION for the function OBJECT of LANGUAGE (a string),
calling (CALLER OBJECT arg...) when called."
  (let ((function (make-instance 'foreign-function :object object :language language)))
    (closer-mop:set-funcallable-instance-function
     function (lambda (&rest args) (apply caller object args)))
    function))

(defun foreign-object (value)
  "VALUE's foreign object if it is a FOREIGN-FUNCTION, else VALUE."
  (if (typep value 'foreign-function) (foreign-function-object value) value))

;;; Collections, without copying
;;;
;;; JAVA-SEQUENCE presents a java.util.List -- which Clojure's vectors and
;;; lists, Kotlin's and Groovy's lists, and Java's own are -- as a Lisp
;;; sequence (SBCL's extensible sequences): ELT, LENGTH, MAP, REDUCE, FIND,
;;; SUBSEQ and the rest work on it, converting only the elements they touch
;;; with CONVERT.  DO-JAVA-ITERATOR and JAVA-TAKE walk any java.lang.Iterable
;;; lazily, so an infinite one (a lazy Clojure seq) can be taken from.
;;;
;;; The other way, LISP-LIST-VIEW and LISP-MAP-VIEW present a Lisp sequence
;;; or hash table to Java as a java.util.List or java.util.Map backed by it.

(defclass java-sequence (sequence standard-object)
  ((object :initarg :object :reader java-sequence-object
           :documentation "The java.util.List.")
   (convert :initarg :convert :initform #'identity :reader java-sequence-convert
            :documentation "Converts an element to Lisp.")
   (unconvert :initarg :unconvert :initform #'identity :reader java-sequence-unconvert
              :documentation "Converts a Lisp value to an element, for (SETF ELT)."))
  (:documentation "A java.util.List as a Lisp sequence, converting elements as they are read."))

(defun java-sequence (list &key (convert #'identity) (unconvert #'identity))
  "LIST, a java.util.List, as a Lisp sequence whose elements are converted
with CONVERT (and, stored with (SETF ELT), with UNCONVERT)."
  (make-instance 'java-sequence :object list :convert convert :unconvert unconvert))

(defmethod print-object ((sequence java-sequence) stream)
  (print-unreadable-object (sequence stream :type t :identity t)
    (format stream "of ~D" (length sequence))))

(defmethod sb-sequence:length ((sequence java-sequence))
  (java-call "size()I" (java-sequence-object sequence)))

(defmethod sb-sequence:elt ((sequence java-sequence) index)
  (funcall (java-sequence-convert sequence)
           (java-call "get(I)Ljava/lang/Object;" (java-sequence-object sequence) index)))

(defmethod (setf sb-sequence:elt) (value (sequence java-sequence) index)
  (java-call "set(ILjava/lang/Object;)Ljava/lang/Object;" (java-sequence-object sequence) index
             (funcall (java-sequence-unconvert sequence) value))
  value)

(defmethod sb-sequence:make-sequence-like ((sequence java-sequence) length &rest args)
  ;; Results of SUBSEQ, MAP and the like are Lisp vectors.
  (apply #'make-array length args))

(defmethod sb-sequence:adjust-sequence ((sequence java-sequence) length &rest args)
  (declare (ignore args))
  (error "A JAVA-SEQUENCE has the length of its list (~D), not ~D." (length sequence) length))

(defmacro do-java-iterator ((var iterable &optional (convert '#'identity)) &body body)
  "Run BODY with VAR bound to each element of the java.lang.Iterable (or
java.util.Iterator) ITERABLE in turn, converted with CONVERT; as lazy as
ITERABLE, so RETURN can stop an infinite one."
  (let ((iterator (gensym "ITERATOR")) (object (gensym)) (fn (gensym)))
    `(let* ((,object ,iterable)
            (,fn ,convert)
            (,iterator (if (java-instance-p ,object "java.util.Iterator")
                           ,object
                           (java-call "iterator()Ljava/util/Iterator;" ,object))))
       (loop while (eql 1 (java-call "hasNext()Z" ,iterator))
             do (let ((,var (funcall ,fn (java-call "next()Ljava/lang/Object;" ,iterator))))
                  ,@body)))))

(defun java-take (n iterable &optional (convert #'identity))
  "The first N elements of the java.lang.Iterable ITERABLE, converted with
CONVERT, as a list; reads no further, so ITERABLE can be infinite."
  (let ((elements '()) (count 0))
    (when (plusp n)
      (do-java-iterator (element iterable convert)
        (push element elements)
        (when (>= (incf count) n) (return))))
    (nreverse elements)))

;; Lisp collections as Java ones, through DEFINE-JAVA-CLASS: subclasses of
;; java.util.AbstractList and AbstractMap whose methods read the Lisp data.

(defvar *view-classes-defined* nil)

(defun %ensure-view-classes ()
  (unless *view-classes-defined*
    (cl:eval
     '(progn
        (define-java-class lisp-list-view-class (:extends "java.util.AbstractList") (data convert)
          ("get(I)Ljava/lang/Object;" (self index)
            (funcall convert (elt data index)))
          ("size()I" (self) (length data)))
        (define-java-class lisp-map-view-class (:extends "java.util.AbstractMap") (data convert unconvert)
          ("get(Ljava/lang/Object;)Ljava/lang/Object;" (self key)
            (multiple-value-bind (value found) (gethash (funcall unconvert key) data)
              (and found (funcall convert value))))
          ("containsKey(Ljava/lang/Object;)Z" (self key)
            (if (nth-value 1 (gethash (funcall unconvert key) data)) 1 0))
          ("size()I" (self) (hash-table-count data))
          ("entrySet()Ljava/util/Set;" (self)
            ;; A snapshot of the entries; GET and CONTAINS-KEY read the table.
            (let ((set (java-new "java.util.LinkedHashSet" "()")))
              (maphash (lambda (k v)
                         (java-call "add(Ljava/lang/Object;)Z" set
                                    (java-new "java.util.AbstractMap$SimpleImmutableEntry"
                                              "(Ljava/lang/Object;Ljava/lang/Object;)"
                                              (funcall convert k) (funcall convert v))))
                       data)
              set)))))
    (setf *view-classes-defined* t)))

(defun lisp-list-view (sequence &key (convert #'identity))
  "A read-only java.util.List backed by the Lisp SEQUENCE, its elements
converted to Java with CONVERT as they are read."
  (%ensure-view-classes)
  (make-java-object 'lisp-list-view-class :data sequence :convert convert))

(defun lisp-map-view (table &key (convert #'identity) (unconvert #'identity))
  "A read-only java.util.Map backed by the Lisp hash TABLE: keys and values
converted to Java with CONVERT, keys looked up converted back with
UNCONVERT."
  (%ensure-view-classes)
  (make-java-object 'lisp-map-view-class :data table :convert convert :unconvert unconvert))

;;; Foreign source files as ASDF components
;;;
;;; Each language system defines a component class for its source files --
;;; (:kotlin-file "geometry"), (:scala-file "parser"), (:groovy-file ...),
;;; (:clojure-file ...), (:kawa-file ...), (:jscheme-file ...),
;;; (:abcl-file ...) in a system that :defsystem-depends-on the language's
;;; system.  Compiled languages compile at COMPILE-OP into a directory of
;;; class files beside the system's fasls, put on the classpath at LOAD-OP;
;;; the others load the source at LOAD-OP.

(defvar *compiled-class-directories* '()
  "Directories of class files ASDF components compiled, on the classpath,
in the order loaded: each later component compiles against them.")

(defun add-to-classpath (&rest entries)
  "Add ENTRIES (jars or directories of class files) to the running
classpath, starting the runtime if needed."
  (start-runtime :classpath (mapcar (lambda (e) (namestring e)) entries)))

(defun %compiled-classpath ()
  (format nil "~{~A~^:~}" (reverse *compiled-class-directories*)))

(defclass foreign-source-file (asdf:source-file) ()
  (:documentation "A foreign language's source file.  Subclasses that
compile name their output directory in OUTPUT-FILES."))

(defclass compiled-foreign-file (foreign-source-file) ()
  (:documentation "A foreign source file compiled to a directory of class
files by COMPILE-FOREIGN-FILE, which subclasses implement."))

(defgeneric compile-foreign-file (component source output-directory)
  (:documentation "Compile the foreign SOURCE file of COMPONENT into the
directory OUTPUT-DIRECTORY, against the classes earlier components
compiled; signal an error if it doesn't compile."))

(defmethod asdf:output-files ((op asdf:compile-op) (component compiled-foreign-file))
  ;; A directory beside the source file, catalog.kt's catalog-classes/.
  (list (uiop:merge-pathnames* (format nil "~A-classes/" (asdf:component-name component))
                               (uiop:pathname-directory-pathname (asdf:component-pathname component)))))

(defmethod asdf:perform ((op asdf:compile-op) (component compiled-foreign-file))
  (let ((output (first (asdf:output-files op component))))
    (uiop:delete-directory-tree output :validate t :if-does-not-exist :ignore)
    (ensure-directories-exist output)
    (compile-foreign-file component (asdf:component-pathname component) output)))

(defmethod asdf:perform ((op asdf:load-op) (component compiled-foreign-file))
  (let ((output (namestring (first (asdf:output-files (asdf:make-operation 'asdf:compile-op) component)))))
    (pushnew output *compiled-class-directories* :test #'string=)
    (add-to-classpath output)))

(defclass loaded-foreign-file (foreign-source-file) ()
  (:documentation "A foreign source file its language loads, at LOAD-OP,
with LOAD-FOREIGN-FILE."))

(defgeneric load-foreign-file (component source)
  (:documentation "Load the foreign SOURCE file of COMPONENT."))

(defmethod asdf:perform ((op asdf:compile-op) (component loaded-foreign-file))
  nil)

(defmethod asdf:perform ((op asdf:load-op) (component loaded-foreign-file))
  (load-foreign-file component (asdf:component-pathname component)))

(defmacro define-foreign-file-type (name superclass extension method)
  "Define NAME (interned in ASDF, so (:NAME \"file\") names it in a
DEFSYSTEM) as a component class of SUPERCLASS for files of EXTENSION, and
METHOD, (method-name (component source &optional output) body...), as its
COMPILE-FOREIGN-FILE or LOAD-FOREIGN-FILE method."
  (let ((class (intern (symbol-name name) :asdf)))
    (destructuring-bind (method-name lambda-list &body body) method
      `(progn
         (defclass ,class (,superclass) ()
           (:default-initargs :type ,extension))
         (defmethod ,method-name ((,(first lambda-list) ,class) ,@(rest lambda-list))
           ,@body)))))

;;; Maven dependencies
;;;
;;; MAVEN-JAR finds a jar by its Maven coordinates in OpenLDK's cache,
;;; downloading it from Maven Central (or REPOSITORY) the first time and
;;; checking its SHA256, if given, or else the SHA-1 the repository
;;; publishes beside it.  ADD-MAVEN-JAR puts it on the classpath, and the
;;; ASDF component (:maven-jar "name" :coordinates "group:artifact:version"
;;; :sha256 "...") does at LOAD-OP, so a system can depend on any Java
;;; library -- usable from every language in the image.  Dependencies of a
;;; jar are not followed: list each one.

(defparameter *maven-repository* "https://repo1.maven.org/maven2")

(defun %maven-parts (coordinates)
  "COORDINATES, \"group:artifact:version\", as (values group artifact version)."
  (let ((parts (uiop:split-string coordinates :separator ":")))
    (unless (= 3 (length parts))
      (error "Maven coordinates are group:artifact:version, not ~S" coordinates))
    (values-list parts)))

(defun %verify-maven-sha1 (file url)
  "Check FILE against the SHA-1 the repository publishes at URL.sha1."
  (let* ((sha1-file (uiop:tmpize-pathname (merge-pathnames "maven.sha1" (uiop:temporary-directory))))
         (expected (progn (openldk::fetch-url (concatenate 'string url ".sha1") sha1-file)
                          (prog1 (string-downcase (string-trim '(#\Space #\Newline #\Return)
                                                               (subseq (uiop:read-file-string sha1-file) 0 40)))
                            (delete-file sha1-file))))
         (output (openldk::%run-tool (list "sha1sum" (uiop:native-namestring file))))
         (actual (string-downcase (subseq output 0 (position #\Space output)))))
    (unless (string= expected actual)
      (delete-file file)
      (error "Checksum mismatch for ~A: the repository's SHA-1 is ~A, the download's ~A"
             url expected actual))))

(defun maven-jar (coordinates &key sha256 (repository *maven-repository*))
  "The local file of the jar with the Maven COORDINATES, downloaded to
OpenLDK's cache the first time and checked against SHA256 or the
repository's SHA-1."
  (multiple-value-bind (group artifact version) (%maven-parts coordinates)
    (let* ((name (format nil "~A-~A.jar" artifact version))
           (path (format nil "maven/~A/~A/~A/~A" (substitute #\/ #\. group) artifact version name))
           (url (format nil "~A/~A/~A/~A/~A" repository (substitute #\/ #\. group) artifact version name))
           (file (merge-pathnames path (openldk::ldk-cache-dir))))
      (unless (uiop:file-exists-p file)
        (openldk::fetch-url url file :sha256 sha256)
        (unless sha256
          (%verify-maven-sha1 file url)))
      file)))

(defun add-maven-jar (coordinates &key sha256 (repository *maven-repository*))
  "Put the jar with the Maven COORDINATES (see MAVEN-JAR) on the classpath;
return its file."
  (let ((jar (maven-jar coordinates :sha256 sha256 :repository repository)))
    ;; Foreign source components compile against it too.
    (pushnew (namestring jar) *compiled-class-directories* :test #'string=)
    (add-to-classpath jar)
    jar))

(defclass asdf::maven-jar (asdf:component)
  ((coordinates :initarg :coordinates :reader maven-jar-coordinates)
   (sha256 :initarg :sha256 :initform nil :reader maven-jar-sha256)
   (repository :initarg :repository :initform *maven-repository* :reader maven-jar-repository))
  (:documentation "A Java library from a Maven repository, put on the classpath at LOAD-OP."))

(defmethod asdf:component-pathname ((component asdf::maven-jar)) nil)
(defmethod asdf:input-files ((op asdf:compile-op) (component asdf::maven-jar)) nil)
(defmethod asdf:output-files ((op asdf:compile-op) (component asdf::maven-jar)) (values nil t))
(defmethod asdf:perform ((op asdf:compile-op) (component asdf::maven-jar)) nil)
(defmethod asdf:perform ((op asdf:load-op) (component asdf::maven-jar))
  (add-maven-jar (maven-jar-coordinates component)
                 :sha256 (maven-jar-sha256 component)
                 :repository (maven-jar-repository component)))

;;; Precompiled source blocks
;;;
;;; A #@kotlin{ } or #@scala{ } block whose splices have static types --
;;; none, literals, or (THE type form) -- is compiled when the Lisp code
;;; around it is (macroexpansion during COMPILE-FILE), if the language is
;;; running then, and its class files go into the fasl: at load time they
;;; are defined from those bytes, so the compiler isn't needed at run time
;;; and a compile error is reported at COMPILE-FILE.  Other blocks compile
;;; the first time they run.

(defun %static-splice-type (form types)
  "The type, from TYPES -- an alist of Lisp type specifier to the
language's type -- that FORM, a splice, has statically: a literal's, or
that of the type in (THE type FORM); or NIL."
  (flet ((lookup (type) (cdr (assoc type types :test #'equal))))
    (cond ((and (consp form) (eq (first form) 'the)) (lookup (second form)))
          ((typep form '(signed-byte 32)) (lookup '(signed-byte 32)))
          ((integerp form) (lookup '(signed-byte 64)))
          ((floatp form) (lookup 'double-float))
          ((stringp form) (lookup 'string))
          ((characterp form) (lookup 'character))
          (t nil))))

(defun %class-files-in (directory)
  "Each class file under DIRECTORY, as (binary-name . octets)."
  (let ((root (namestring (truename directory))))
    (loop for file in (directory (merge-pathnames "**/*.class" directory))
          collect (cons (let ((relative (subseq (namestring file) (length root))))
                          (subseq relative 0 (- (length relative) 6)))
                        (with-open-file (in file :element-type '(unsigned-byte 8))
                          (let ((octets (make-array (file-length in) :element-type '(unsigned-byte 8))))
                            (read-sequence octets in)
                            octets))))))

(defvar *precompiled-classes-defined* (make-hash-table :test #'equal)
  "Names of the classes precompiled blocks defined, against defining twice.")

(defun %define-precompiled-classes (classes)
  "Define CLASSES, (binary-name . octets), through the application class
loader, once."
  (dolist (class classes)
    (destructuring-bind (name . octets) class
      (unless (gethash name *precompiled-classes-defined*)
        (unless (runtime-started-p) (start-runtime))
        (openldk::%classload-from-stream name (flexi-streams:make-in-memory-input-stream octets)
                                         openldk::*boot-class-loader* openldk::*app-ldk-class-loader*)
        (setf (gethash name *precompiled-classes-defined*) t)))))

(defun %site-name (prefix source types)
  "A class name for the block of SOURCE with parameter TYPES, the same
each time: compiling it again defines no new class."
  (format nil "~A~8,'0X" prefix (logand (sxhash (format nil "~A~S" source types)) #xFFFFFFFF)))

;;; Records

(defun java-fields (object &key (convert #'identity))
  "The instance fields of the Java OBJECT -- a Kotlin data class's, a Scala
case class's, a Java record's -- as an alist of (name . value), values
converted with CONVERT, superclasses' fields after the class's own."
  (let ((class (openldk::%get-ldk-class-by-bin-name
                (symbol-name (class-name (class-of object))) t)))
    (loop while class
          append (loop for field across (or (openldk::fields class) #())
                       when (zerop (logand #x8 (slot-value field 'openldk::access-flags)))
                         collect (let ((name (slot-value field 'openldk::name)))
                                   (cons name (funcall convert
                                                       (slot-value object (intern (openldk::mangle-field-name name)
                                                                                  :openldk))))))
          do (setf class (and (openldk::super class)
                              (openldk::%get-ldk-class-by-bin-name (openldk::super class) t))))))

;;; CLOS methods on Java methods
;;;
;;; A Java instance method is a generic function in OpenLDK, so Lisp can add
;;; methods to it -- :before, :after, :around, or a primary method -- for a
;;; Java class, and they run on every call, including calls from compiled
;;; Java or Kotlin code:
;;;
;;;   (define-java-method :around ("wonderland.Book" "getPrice()D") (book)
;;;     (* (call-next-method) 0.8d0))

(defmacro define-java-method (qualifier (class-name method) lambda-list &body body)
  "Define a method, with QUALIFIER (:BEFORE, :AFTER, :AROUND, or NIL for a
primary method), on the Java method METHOD -- its full JVM name, as
\"getPrice()D\" -- of the Java class CLASS-NAME, whose instances the
first parameter of LAMBDA-LIST receives.  BODY can use CALL-NEXT-METHOD.
The class is loaded, and the method defined, when the form is evaluated."
  `(%define-java-method ',qualifier ,class-name ,method '(,lambda-list ,@body)))

(defun %define-java-method (qualifier class-name method definition)
  (unless (runtime-started-p) (start-runtime))
  (destructuring-bind (lambda-list &rest body) definition
    (let* ((class (java-load class-name))
           (class-symbol (intern (openldk::name class) (openldk::ldk-class-package class)))
           (gf (intern method :openldk)))
      (openldk::%ensure-java-gf gf (length lambda-list))
      (cl:eval `(defmethod ,gf ,@(when qualifier (list qualifier))
                    ((,(first lambda-list) ,class-symbol) ,@(rest lambda-list))
                  ,@body)))))

;;; Statically typed native definitions share the CL wrapper and header syntax.

(defun %native-identifier (symbol)
  (unless (and (symbolp symbol) symbol (not (constantp symbol)))
    (error "Expected a native function or parameter name, got ~S" symbol))
  (let ((name (symbol-name symbol)))
    (unless (and (plusp (length name))
                 (or (alpha-char-p (char name 0)) (char= (char name 0) #\_))
                 (every (lambda (c) (or (alphanumericp c) (char= c #\_))) name))
      (error "Native names must be identifiers; use an escaped CL symbol: ~S" symbol))
    (if (string= name (string-upcase name)) (string-downcase name) name)))

(defun %native-type (type)
  (let ((entry (assoc type '((:int . "Int") (:long . "Long") (:short . "Short")
                              (:byte . "Byte") (:float . "Float") (:double . "Double")
                              (:boolean . "Boolean") (:char . "Char") (:string . "String")
                              (:unit . "Unit")))))
    (cond ((and (stringp type) (plusp (length type))) type)
          (entry (cdr entry))
          (t (error "Use a native type string or a primitive type keyword, got ~S" type)))))

(defun %expand-typed-function-definition (name parameters source captures forms return-type creator)
  (%native-identifier name)
  (unless (and (alexandria:proper-list-p parameters)
               (every (lambda (p) (and (alexandria:proper-list-p p) (= (length p) 2))) parameters)
               (= (length parameters) (length (remove-duplicates parameters :key #'first)))
               (stringp source))
    (error "Native parameters must be distinct (NAME TYPE) pairs: ~S" parameters))
  (let ((procedure (gensym "PROCEDURE"))
        (args (mapcar #'first parameters)))
    `(let ((,procedure
             (,creator ,(%native-identifier name)
                       ',(mapcar #'%native-identifier args)
                       ',(mapcar (lambda (p) (%native-type (second p))) parameters)
                       ,source ',captures (list ,@forms)
                       ,(when return-type (%native-type return-type)))))
       (defun ,name ,args (cl:funcall ,procedure ,@args)))))
