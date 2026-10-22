;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: OPENLDK.JSCHEME; Base: 10 -*-
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

;;; JScheme from Common Lisp: Peter Norvig's JScheme, running on OpenLDK in
;;; the same SBCL process.  Evaluate Scheme, call Scheme procedures, define
;;; Lisp functions Scheme can call, and convert values both ways.
;;;
;;;   (jscheme:startup)
;;;   (jscheme:eval '(+ 2 3))                         => 5
;;;   (jscheme:define "square" (lambda (x) (* x x)))
;;;   (jscheme:eval '(map square '(1 2 3)))           => (1 4 9)
;;;
;;; Values (TO-LISP and FROM-LISP):
;;;
;;;   Scheme              Lisp
;;;   number (a double)   integer when whole, as JScheme prints it, else double-float
;;;   string              string
;;;   symbol              symbol, upcased, interned in *PACKAGE* (FOO <-> foo)
;;;   character           character
;;;   list, pair          list, cons
;;;   vector              simple-vector
;;;   #t                  T
;;;   #f, '()             NIL, which goes back as '(); write :FALSE for #f
;;;   procedure           left as is: call it with FUNCALL
;;;
;;; A Lisp function given to Scheme (DEFINE, FROM-LISP) returns '() for NIL,
;;; which Scheme treats as true; define a predicate with :PREDICATE T to
;;; turn NIL into #f.

(defpackage #:openldk.jscheme
  (:use #:cl #:openldk.interop)
  (:nicknames #:jscheme)
  (:shadow #:eval #:load #:funcall)
  (:export #:startup
           #:default-classpath
           #:*interpreter*
           #:eval
           #:eval-string
           #:load
           #:lookup
           #:funcall
           #:define
           #:to-lisp
           #:from-lisp))

(in-package #:openldk.jscheme)

(defvar *interpreter* nil
  "The jscheme.Scheme interpreter, once STARTUP has run.")

(defvar *true* nil)
(defvar *false* nil)

;; CLOS class names of the JScheme and JDK classes TO-LISP checks for.
(defvar *pair* nil)
(defvar *boolean* nil)
(defvar *number* nil)
(defvar *character* nil)
(defvar *string* nil)

(defun %scheme-class-in (directory)
  (uiop:file-exists-p (merge-pathnames "jscheme/Scheme.class" directory)))

(defun %cached-classes ()
  "JScheme's classes, compiled in OpenLDK's cache from downloaded sources."
  (let ((classes (merge-pathnames "jscheme/classes/" (openldk::ldk-cache-dir))))
    (unless (%scheme-class-in classes)
      (let* ((source-jar (openldk::ensure-cached-file
                          "jscheme/jscheme-src.jar" "https://norvig.com/jscheme/jscheme.jar"
                          "a178070302e20f43922bdbe4bc8d5eccb54c6ce988db5557ce3e1556dca73d49"))
             (build (uiop:ensure-directory-pathname (openldk::%temporary-name classes)))
             (sources (merge-pathnames "src/" build))
             (output (merge-pathnames "classes/" build)))
        (unwind-protect
             (progn
               (zip:unzip source-jar (ensure-directories-exist sources))
               (ensure-directories-exist output)
               ;; javac through the JDK's own module, as `make abcl` does,
               ;; so a JDK without bin/javac works too.
               (openldk::ensure-JAVA_HOME)
               (openldk::%run-tool
                (list* (uiop:native-namestring
                        (merge-pathnames "bin/java" (uiop:ensure-directory-pathname
                                                     (uiop:getenv "JAVA_HOME"))))
                       "-m" "jdk.compiler/com.sun.tools.javac.Main" "-nowarn"
                       "-d" (uiop:native-namestring output)
                       (mapcar #'uiop:native-namestring
                               (directory (merge-pathnames "jscheme/*.java" sources)))))
               ;; Another process may have got there first; theirs is as good.
               (unless (%scheme-class-in classes)
                 (sb-posix:rename (string-right-trim "/" (uiop:native-namestring output))
                                  (string-right-trim "/" (uiop:native-namestring classes)))))
          (uiop:delete-directory-tree build :validate t :if-does-not-exist :ignore))))
    classes))

(defun default-classpath ()
  "JScheme's classes: from CLASSPATH, as `make jscheme` builds them, or
else compiled from downloaded sources."
  (or (classpath-from-environment "jscheme/Scheme")
      (list (namestring
             (let ((made (asdf:system-relative-pathname "openldk" "lib/jscheme/classes/")))
               (if (%scheme-class-in made) made (%cached-classes)))))))

;;; Lisp functions as Scheme procedures

(define-java-subclass lisp-procedure "jscheme.Procedure"
    ((function :initarg :function :reader lisp-procedure-function)
     (predicate :initarg :predicate :initform nil :reader lisp-procedure-predicate))
  ("apply(Ljscheme/Scheme;Ljava/lang/Object;)" (self interpreter args)
   (declare (ignore interpreter))
   (call-from-java
    (lambda ()
      (let ((result (apply (lisp-procedure-function self) (to-lisp args))))
        (if (and (null result) (lisp-procedure-predicate self))
            *false*
            (from-lisp result)))))))

(defun %lisp-procedure (function name &key predicate)
  (let ((procedure (make-java-subclass-instance 'lisp-procedure
                                                :function function
                                                :predicate predicate)))
    (when name
      (setf (slot-value procedure (intern "name" :openldk)) (jstring name)))
    procedure))

;;; Startup

(defun startup (&key (classpath (default-classpath)))
  "Start JScheme, from its classes in CLASSPATH, a list of directories
and jars (by default, found or built by DEFAULT-CLASSPATH).  Calling it
again does nothing."
  (unless *interpreter*
    (start-runtime :classpath classpath)
    (setf *true* (java-static-field "jscheme.SchemeUtils" "TRUE")
          *false* (java-static-field "jscheme.SchemeUtils" "FALSE")
          *pair* (java-class-symbol "jscheme.Pair")
          *boolean* (java-class-symbol "java.lang.Boolean")
          *number* (java-class-symbol "java.lang.Number")
          *character* (java-class-symbol "java.lang.Character")
          *string* (java-class-symbol "java.lang.String"))
    (install-lisp-procedure)
    (setf *interpreter* (with-foreign-errors ("JScheme")
                          (java-new "jscheme.Scheme" "([Ljava/lang/String;)" nil))))
  *interpreter*)

(defun %ensure-started ()
  (or *interpreter* (startup)))

;;; Values

(defun %scheme-symbol (symbol)
  "The JScheme symbol, an interned java.lang.String, for SYMBOL (a Lisp
symbol or a string).  A Lisp symbol in upper case is lowercased."
  (let ((name (if (stringp symbol)
                  symbol
                  (let ((name (symbol-name symbol)))
                    (if (string= name (string-upcase name)) (string-downcase name) name)))))
    (java-call "intern()" (jstring name))))

(defun %lisp-symbol (name)
  "The Lisp symbol for the JScheme symbol NAME (a Lisp string)."
  (intern (if (string= name (string-downcase name)) (string-upcase name) name)))

(defun from-lisp (value)
  "The JScheme value for the Lisp VALUE; see the table at the top of this file."
  (%ensure-started)
  (cond
    ((null value) nil)
    ((eq value t) *true*)
    ((eq value :false) *false*)
    ((numberp value) (java-double (if (complexp value) (realpart value) value)))
    ((stringp value) (make-java-array-of "char" value))
    ;; A Java char argument is its code (though a char[] holds characters).
    ((characterp value) (java-static "java.lang.Character" "valueOf(C)" (char-code value)))
    ((symbolp value) (%scheme-symbol value))
    ((consp value)
     (java-new "jscheme.Pair" "(Ljava/lang/Object;Ljava/lang/Object;)"
               (from-lisp (car value)) (from-lisp (cdr value))))
    ((vectorp value) (make-java-array-of "java.lang.Object" (map 'vector #'from-lisp value)))
    ((functionp value) (%lisp-procedure value nil))
    (t value)))

(defun to-lisp (object)
  "The Lisp value for the JScheme OBJECT; see the table at the top of this file."
  (cond
    ((null object) nil)
    ((typep object *boolean*) (eq object *true*))
    ((typep object *pair*)
     (cons (to-lisp (slot-value object (intern "first" :openldk)))
           (to-lisp (slot-value object (intern "rest" :openldk)))))
    ((typep object *number*)
     (let ((number (java-call "doubleValue()" object)))
       (if (and (= number (ffloor number)) (< (abs number) (expt 2 53)))
           (round number)
           number)))
    ((typep object *character*)
     (let ((char (java-call "charValue()" object)))
       (if (characterp char) char (code-char char))))
    ((typep object *string*) (%lisp-symbol (lstring object)))
    ((openldk::java-array-p object)
     (let ((elements (openldk::java-array-data object)))
       (if (and (plusp (length elements))
                (equal (array-element-type elements) '(unsigned-byte 16)))
           (map 'string #'code-char elements)
           (map 'vector #'to-lisp elements))))
    (t object)))

;;; Evaluating

(defun %eval-datum (datum)
  (with-foreign-errors ("JScheme")
    (java-call "eval(Ljava/lang/Object;)" (%ensure-started) datum)))

(defun eval (form)
  "Evaluate FORM, a Lisp s-expression for a Scheme form (symbols are
lowercased: (eval '(string-length \"abc\")) => 3), or a string of Scheme
source, and return the value as Lisp data."
  (if (stringp form)
      (eval-string form)
      (to-lisp (%eval-datum (from-lisp form)))))

(defun eval-string (source)
  "Read and evaluate every form in the string SOURCE; return the last value."
  (%ensure-started)
  (let* ((reader (java-new "java.io.StringReader" "(Ljava/lang/String;)" (jstring source)))
         (port (java-new "jscheme.InputPort" "(Ljava/io/Reader;)" reader))
         (value nil))
    (loop for datum = (with-foreign-errors ("JScheme") (java-call "read()" port))
          ;; Java booleans come back as 1 and 0.
          until (eql 1 (java-static "jscheme.InputPort" "isEOF(Ljava/lang/Object;)" datum))
          do (setf value (%eval-datum datum)))
    (to-lisp value)))

(defun load (file)
  "Load the Scheme source FILE."
  (with-foreign-errors ("JScheme")
    (java-call "load(Ljava/lang/Object;)" (%ensure-started) (jstring (namestring file))))
  t)

;;; Procedures and definitions

(defun %global-environment ()
  (slot-value (%ensure-started) (intern "globalEnvironment" :openldk)))

(defun lookup (name)
  "The value Scheme binds to NAME (a string or symbol), unconverted, so a
procedure can be passed to FUNCALL."
  (with-foreign-errors ("JScheme")
    (java-call "lookup(Ljava/lang/String;)" (%global-environment) (%scheme-symbol name))))

(defun funcall (procedure &rest args)
  "Call the Scheme PROCEDURE (a procedure object, or the name of one) with
ARGS, converted with FROM-LISP; return the result as Lisp data."
  (let ((procedure (if (or (stringp procedure) (symbolp procedure))
                       (lookup procedure)
                       procedure)))
    (to-lisp (with-foreign-errors ("JScheme")
               (java-call "apply(Ljscheme/Scheme;Ljava/lang/Object;)"
                          procedure *interpreter* (from-lisp args))))))

(defun define (name value &key predicate)
  "Bind NAME (a string or symbol) in Scheme's global environment to VALUE,
converted with FROM-LISP.  A function becomes a Scheme procedure; with
PREDICATE, its NIL result is #f rather than '()."
  (let ((value (if (functionp value)
                   (%lisp-procedure value (if (stringp name) name (string-downcase name))
                                    :predicate predicate)
                   (from-lisp value))))
    (with-foreign-errors ("JScheme")
      (java-call "define(Ljava/lang/Object;Ljava/lang/Object;)"
                 (%global-environment) (%scheme-symbol name) value))
    name))
