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
           #:define-java-subclass
           #:make-java-subclass-instance
           #:call-from-java
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
                   constructor)))
    (apply (symbol-function (intern init :openldk)) object args)
    object))

(defun java-call (method object &rest args)
  "Call the instance method METHOD, e.g. \"toString()\", on OBJECT."
  (apply (symbol-function (intern method :openldk)) object args))

(defun java-static (class-name method &rest args)
  "Call the static method METHOD of CLASS-NAME, e.g. (java-static
\"java.lang.Double\" \"valueOf(D)\" 1d0)."
  (let* ((class (java-load class-name))
         (symbol (openldk::static-method-symbol
                  (format nil "~A.~A" (openldk::name class) method)
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

(defmacro with-foreign-errors ((language) &body body)
  "Run BODY, turning a Java exception that escapes it into a FOREIGN-ERROR
for LANGUAGE (a string), or into the Lisp condition it carries if it came
from CALL-FROM-JAVA."
  `(handler-case (progn ,@body)
     (openldk::|condition-java/lang/Throwable| (condition)
       (let* ((throwable (slot-value condition 'openldk::|objref|))
              (lisp-condition (gethash throwable *lisp-errors-in-java*)))
         (if lisp-condition
             (error lisp-condition)
             (error 'foreign-error :language ,language
                                   :message (%throwable-message throwable)
                                   :object throwable))))))

(defun call-from-java (function &rest args)
  "Call FUNCTION with ARGS on behalf of Java code (a callback).  A Lisp
error is thrown into the Java code as a java.lang.RuntimeException carrying
it, which WITH-FOREIGN-ERRORS turns back into the Lisp condition."
  (handler-case (apply function args)
    (error (condition)
      (let ((exception (java-new "java.lang.RuntimeException" "(Ljava/lang/String;)"
                                 (jstring (princ-to-string condition)))))
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
                 collect `(eval '(defmethod ,(intern method :openldk)
                                     ((,(first lambda-list) ,name) ,@(rest lambda-list))
                                   ,@body)))
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

(defun dump-lisp-image (path &key init)
  "Save an SBCL executable at PATH whose top level is the Lisp REPL, after
calling INIT, which typically starts a language and evaluates some code so
its Java methods are compiled into the image.  A language that takes
minutes to start (Clojure) is then ready in under a second."
  (when init (cl:funcall init))
  (pushnew '%restart-runtime sb-ext:*init-hooks*)
  (openldk::%reset-runtime-for-save)
  (sb-ext:save-lisp-and-die path :executable t :save-runtime-options t))

