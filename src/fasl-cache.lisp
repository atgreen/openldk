;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: OPENLDK; Base: 10 -*-
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

(in-package :openldk)

;;; A persistent cache of JIT-compiled methods.  The first time a method
;;; compiles, its generated code goes through COMPILE-FILE instead of EVAL,
;;; and the FASL is kept under *FASL-CACHE-DIRECTORY*; a later run LOADs it
;;; instead of transpiling and compiling again, which is some 30 times
;;; faster.
;;;
;;; Generated code holds live objects -- interned Java strings, <class>es,
;;; java.lang.Class objects -- which a FASL can't contain, so
;;; %EXTERNALIZE-DEFINITION replaces each with a LOAD-TIME-VALUE form that
;;; finds or makes it again.  Code it can't externalize isn't cached.
;;;
;;; What the compiler generates for a method depends on more than its own
;;; class: it looks up the classes the method names, their superclasses and
;;; interfaces, to find declaring classes, field slots and return types,
;;; and whether a class exists at all.  So every class it looks up while a
;;; method compiles is recorded (see %NOTE-CLASS-DEPENDENCY), and the FASL
;;; begins with a check that each one still has the content it had then
;;; (still exists or not), which aborts the LOAD otherwise.
;;;
;;; Entries are keyed by the class's loader package, name and content hash
;;; and the method's name and descriptor, under a directory named by a hash
;;; of OpenLDK's sources and the SBCL version, so a different build never
;;; sees them.  Only classes of the boot and application loaders are
;;; cached: user loaders' classes live in packages each run creates anew.
;;;
;;; LDK_CODE_CACHE=off turns the cache off and LDK_CODE_CACHE=<dir> moves it
;;; from its default, code/ in the download cache (see LDK-CACHE-DIR).
;;; LDK_CODE_CACHE_STATS prints what it did at exit.
;;; With LDK_CODE_CACHE_VERIFY set, every method compiles as usual and an
;;; entry whose code differs from what the compiler now generates for it,
;;; though its dependencies match, is reported: a dependency the recording
;;; missed.

(defvar *fasl-cache-directory* nil
  "Directory holding this build's cached FASLs, or NIL when the cache is off.")

(defvar *fasl-cache-verify* nil
  "True to compile every method anyway and report cache entries that differ.")

(defvar *fasl-cache-loader* nil
  "While a cached FASL loads, the <ldk-class-loader> of its method's class.")

(defvar *fasl-cache-expected-source* nil
  "In verify mode, while an entry loads, the hash of the code the compiler
generated for its method this time.")

(defvar *fasl-cache-stats* (list :hits 0 :misses 0 :stale 0 :writes 0
                                 :uncacheable 0 :mismatches 0)
  "Counts of what the cache did this run, for LDK_CODE_CACHE_STATS.")

(defun %fasl-cache-count (key)
  (sb-ext:atomic-incf (car (cdr (member key *fasl-cache-stats*)))))

(defun %compute-build-id ()
  "A hash of OpenLDK's source files and the SBCL version: the generated
code, and the runtime functions it calls, are those of one build."
  (let ((state (sb-md5:make-md5-state)))
    (flet ((add (octets) (sb-md5:update-md5-state state octets)))
      (add (sb-ext:string-to-octets (lisp-implementation-version) :external-format :utf-8))
      (dolist (file (sort (mapcar #'namestring
                                  (directory (merge-pathnames
                                              (make-pathname :directory '(:relative "src" :wild-inferiors)
                                                             :name :wild :type "lisp")
                                              (asdf:system-source-directory :openldk))))
                          #'string<))
        (add (sb-ext:string-to-octets file :external-format :utf-8))
        (add (alexandria:read-file-into-byte-vector file))))
    (format nil "~(~{~2,'0x~}~)" (coerce (sb-md5:finalize-md5-state state) 'list))))

(defvar *fasl-cache-build-id* (%compute-build-id)
  "Hash of the sources this OpenLDK was built from; see %COMPUTE-BUILD-ID.")

(defun %fasl-cache-configure ()
  "Set up the cache from the environment, for a run of a saved image."
  (let ((setting (uiop:getenv "LDK_CODE_CACHE")))
    (setf *fasl-cache-verify* (and (uiop:getenvp "LDK_CODE_CACHE_VERIFY") t)
          *fasl-cache-directory*
          (unless (member setting '("off" "0" "no" "false") :test #'equalp)
            (let ((root (if (plusp (length setting))
                            (uiop:ensure-directory-pathname setting)
                            (merge-pathnames "code/" (ldk-cache-dir)))))
              (handler-case
                  (let ((dir (merge-pathnames (format nil "~A/" *fasl-cache-build-id*) root)))
                    (ensure-directories-exist dir :mode #o700)
                    ;; Its date says when this build last ran.
                    (with-open-file (out (merge-pathnames "last-used" dir)
                                         :direction :output :if-exists :supersede)
                      (write-line (lisp-implementation-version) out))
                    (%fasl-cache-prune root dir)
                    dir)
                (error () nil)))))))

(defparameter +fasl-cache-unused-days+ 14
  "Other builds' cache directories untouched this many days are deleted.")

(defun %fasl-cache-prune (root current)
  "Delete the directories under ROOT of builds other than CURRENT's that
haven't run for +FASL-CACHE-UNUSED-DAYS+."
  (let ((cutoff (- (get-universal-time) (* +fasl-cache-unused-days+ 24 60 60))))
    (dolist (dir (uiop:subdirectories root))
      (unless (equal (namestring dir) (namestring current))
        (ignore-errors
         (when (< (file-write-date (or (probe-file (merge-pathnames "last-used" dir)) dir))
                  cutoff)
           (uiop:delete-directory-tree dir :validate t)))))))

(defun %fasl-cache-start ()
  "Turn the cache on, as the environment says, for a run of a saved image;
a build's warmups, which run without, leave no entries behind."
  (%fasl-cache-configure)
  (pushnew '%fasl-cache-report sb-ext:*exit-hooks*))

(defun %fasl-cache-report ()
  "Print the cache's counts to stderr when LDK_CODE_CACHE_STATS is set."
  (when (and *fasl-cache-directory* (uiop:getenv "LDK_CODE_CACHE_STATS"))
    (format *error-output* "~&;; FASL-CACHE ~{~(~A~) ~D~^, ~}~%" *fasl-cache-stats*)
    (force-output *error-output*)))

;;; Class content hashes and dependencies

(define-condition uncacheable (error)
  ((what :initarg :what :reader uncacheable-what))
  (:report (lambda (c s) (format s "can't cache code holding ~A" (uncacheable-what c)))))

(defparameter +primitive-class-names+
  '("boolean" "byte" "char" "short" "int" "long" "float" "double" "void"))

(defun %dependency-fingerprint (bin-name loader &optional resolve)
  "What the compiler sees of the class BIN-NAME from LOADER: its content
hash, :SYNTHETIC for a class with none (defined by OpenLDK itself), or NIL
when there is no such class.  With RESOLVE, load it first if need be, as
the compiler would.  Signals UNCACHEABLE for a user loader's class."
  (let* ((*compile-dependencies* nil)
         (class (or (%get-ldk-class-by-bin-name bin-name t loader)
                    (and resolve (ignore-errors (%resolve-class bin-name loader)))))
         (global (gethash bin-name *ldk-classes-by-bin-name*)))
    ;; The global table holds the last class of each name, which the
    ;; compiler consults too: a user loader's there makes the code depend
    ;; on that loader.
    (when (or (and class (%user-ldk-loader-p (slot-value class 'ldk-loader)))
              (and global (not (eq global class))
                   (%user-ldk-loader-p (slot-value global 'ldk-loader))))
      (error 'uncacheable :what bin-name))
    (cond ((null class) nil)
          ((content-hash class))
          (t :synthetic))))

(defun %recorded-dependencies (dependencies loader)
  "DEPENDENCIES, the names the compiler looked up, as (name . fingerprint)."
  (sort (loop for name being the hash-keys of dependencies
              unless (or (zerop (length name))
                         (char= (char name 0) #\[)
                         (member name +primitive-class-names+ :test #'string=))
                collect (cons name (%dependency-fingerprint name loader)))
        #'string< :key #'car))

(defun %fasl-cache-validate (dependencies source-hash)
  "The first form of every cached FASL: abort its LOAD, throwing to
FASL-CACHE-STALE, unless each of DEPENDENCIES still has its fingerprint.
In verify mode it always aborts, after reporting an entry whose code
SOURCE-HASH differs from the compiler's now."
  (loop for (name . fingerprint) in dependencies
        for now = (handler-case (%dependency-fingerprint name *fasl-cache-loader* t)
                    (uncacheable () :uncacheable))
        unless (equal fingerprint now)
          do (when (uiop:getenv "LDK_CODE_CACHE_STATS")
               (format *error-output* "~&;; FASL-CACHE-STALE: ~A was ~A, now ~A~%"
                       name fingerprint now))
             (throw 'fasl-cache-stale :stale))
  (when *fasl-cache-verify*
    (throw 'fasl-cache-stale
      (if (equal source-hash *fasl-cache-expected-source*) :verified :mismatch))))

(defun %fasl-cache-class (bin-name)
  "The <class> named BIN-NAME, for code loaded from the cache."
  (or (%get-ldk-class-by-bin-name bin-name t *fasl-cache-loader*)
      (if (char= (char bin-name 0) #\[)
          (%get-array-ldk-class-from-name bin-name)
          (%resolve-class bin-name *fasl-cache-loader*))
      (internal-error "FASL cache: class ~A not found" bin-name)))

;;; Externalizing generated code

(defun %cacheable-package-p (package)
  "True for a package every run has: not a user loader's (OPENLDK.L<n>)."
  (or (null package)
      (let ((name (package-name package)))
        (not (and (> (length name) 9) (string= "OPENLDK.L" name :end2 9))))))

(defun %externalizable-p (x)
  "True when X can appear in a FASL as it is."
  (typecase x
    (cons (and (%externalizable-p (car x)) (%externalizable-p (cdr x))))
    (symbol (or (%cacheable-package-p (symbol-package x))
                (error 'uncacheable :what x)))
    (float (not (or (float-features:float-nan-p x) (float-features:float-infinity-p x))))
    ((or number character string) t)
    (vector (every #'%externalizable-p x))
    (t nil)))

(defun %java-string-form (string)
  (let ((s (lstring string)))
    ;; Lone surrogates, which can't be written as UTF-8, travel as codes.
    `(ijstring ,(if (every (lambda (c) (< (char-code c) 128)) s)
                    s
                    `(map 'string #'code-char ',(map 'vector #'char-code s))))))

(defun %class-form (class)
  (when (%user-ldk-loader-p (slot-value class 'ldk-loader))
    (error 'uncacheable :what class))
  `(%fasl-cache-class ,(name class)))

(defun %construction-form (x)
  "Code that makes X, which holds objects a FASL can't, at load time."
  (cond ((%externalizable-p x) `',x)
        ((consp x) `(cons ,(%construction-form (car x)) ,(%construction-form (cdr x))))
        ((typep x '(and vector (not string)))
         `(vector ,@(map 'list #'%construction-form x)))
        (t (typecase x
             (double-float `(float-features:bits-double-float ,(float-features:double-float-bits x)))
             (single-float `(float-features:bits-single-float ,(float-features:single-float-bits x)))
             (|java/lang/String| (%java-string-form x))
             (<class> (%class-form x))
             (|java/lang/Class|
              (let ((class (get-ldk-class-for-java-class x)))
                (unless class (error 'uncacheable :what x))
                `(java-class ,(%class-form class))))
             (t (error 'uncacheable :what (type-of x)))))))

(defun %externalize-definition (form)
  "FORM, generated code, with every object a FASL can't hold replaced by a
LOAD-TIME-VALUE form that makes it.  Signals UNCACHEABLE when one can't be."
  (cond ((and (consp form) (eq (car form) 'quote))
         (if (%externalizable-p (second form))
             form
             `(load-time-value ,(%construction-form (second form)) t)))
        ((consp form)
         (cons (%externalize-definition (car form)) (%externalize-definition (cdr form))))
        ((%externalizable-p form) form)
        (t `(load-time-value ,(%construction-form form) t))))

(defun %print-definition (form)
  "FORM as text that reads back as FORM in the OPENLDK package."
  (with-standard-io-syntax
    (let ((*package* (find-package :openldk))
          (*print-circle* t)
          (*print-pretty* nil))
      (handler-case (prin1-to-string form)
        (print-not-readable (c)
          (error 'uncacheable :what (print-not-readable-object c)))))))

;;; The store

(defun %fasl-cache-eligible-p (class method length)
  "True when METHOD of CLASS, LENGTH bytes of bytecode, can be cached."
  (and *fasl-cache-directory*
       (not *aot-dir*)
       (not *debug-codegen*)
       (content-hash class)
       (<= length 5000)                 ; interpreted, see %install-method-definition
       (not (%user-ldk-loader-p (slot-value class 'ldk-loader)))
       (not (search "/hidden-" (name class)))
       (not (search "/anonymous-class-" (name class)))
       (gethash "Code" (slot-value method 'attributes))))

(defun %fasl-cache-path (class method)
  (let ((key (%md5-hex (sb-ext:string-to-octets
                        (format nil "~A~C~A~C~A~C~A~A"
                                (package-name (loader-package (slot-value class 'ldk-loader)))
                                #\Nul (name class) #\Nul (content-hash class)
                                #\Nul (slot-value method 'name) (slot-value method 'descriptor))
                        :external-format :utf-8))))
    (merge-pathnames (format nil "~A/~A.fasl" (subseq key 0 2) key) *fasl-cache-directory*)))

(defun %fasl-cache-load (class method &optional source-hash)
  "Install METHOD of CLASS from the cache, returning true; or return NIL
when it has no entry, or a stale one, which is deleted.  In verify mode,
with SOURCE-HASH the hash of its code now, just compare; see
%FASL-CACHE-VALIDATE."
  (let ((path (%fasl-cache-path class method)))
    (unless (probe-file path)
      (unless source-hash (%fasl-cache-count :misses)))
    (when (probe-file path)
      (let ((outcome
              (catch 'fasl-cache-stale
                (handler-case
                    (let ((*fasl-cache-loader* (slot-value class 'ldk-loader))
                          (*fasl-cache-expected-source* source-hash)
                          (*compile-dependencies* nil))
                      (handler-bind ((warning #'muffle-warning))
                        (load path :verbose nil :print nil))
                      :loaded)
                  (error (c)
                    (format *error-output* "~&;; FASL-CACHE-ERROR loading ~A: ~A~%" path c)
                    :stale)))))
        (case outcome
          (:loaded (%fasl-cache-count :hits) t)
          (:mismatch
           (%fasl-cache-count :mismatches)
           (format *error-output* "~&;; FASL-CACHE-MISMATCH ~A.~A~A~%"
                   (name class) (slot-value method 'name) (slot-value method 'descriptor))
           (force-output *error-output*)
           nil)
          (:verified nil)
          (t (%fasl-cache-count :stale)
             (ignore-errors (delete-file path))
             nil))))))

(defvar *fasl-cache-temp-counter* (list 0))

(defun %fasl-cache-temp-name (path type)
  (make-pathname :name (format nil "~A.~D.~D" (pathname-name path) (sb-posix:getpid)
                               (sb-ext:atomic-incf (car *fasl-cache-temp-counter*)))
                 :type type :defaults path))

(defun %fasl-cache-compile (class method definition-code dependencies)
  "Compile DEFINITION-CODE, METHOD of CLASS's, to a cached FASL whose LOAD
checks DEPENDENCIES first, and return the FASL's path; or return NIL when
it can't be cached."
  (handler-case
      (let* ((loader (slot-value class 'ldk-loader))
             (text (%print-definition (%externalize-definition definition-code)))
             (source-hash (%md5-hex (sb-ext:string-to-octets text :external-format :utf-8)))
             (deps (%recorded-dependencies dependencies loader))
             (path (%fasl-cache-path class method)))
        (cond
          (*fasl-cache-verify*
           (%fasl-cache-load class method source-hash)
           nil)
          (t
           (let ((source (%fasl-cache-temp-name path "lisp"))
                 (fasl (%fasl-cache-temp-name path "fasl")))
             (ensure-directories-exist path :mode #o700)
             (unwind-protect
                  (progn
                    (with-open-file (out source :direction :output :if-exists :supersede
                                                :external-format :utf-8)
                      (with-standard-io-syntax
                        (let ((*package* (find-package :openldk)))
                          (format out "(in-package :openldk)~%")
                          (prin1 `(%fasl-cache-validate ',deps ,source-hash) out)
                          (terpri out)
                          (write-string text out)
                          (terpri out))))
                    (multiple-value-bind (output warnings-p failure-p)
                        (handler-bind ((warning #'muffle-warning))
                          (let ((*standard-output* (make-broadcast-stream))
                                (*error-output* (make-broadcast-stream))
                                (*compile-dependencies* nil))
                            (compile-file source :output-file fasl :external-format :utf-8
                                                 :verbose nil :print nil)))
                      (declare (ignore warnings-p))
                      (when (and output (not failure-p))
                        (rename-file fasl path)
                        (%fasl-cache-count :writes)
                        path)))
               (ignore-errors (delete-file source))
               (ignore-errors (when (probe-file fasl) (delete-file fasl))))))))
    (uncacheable (c)
      (%fasl-cache-count :uncacheable)
      (when (uiop:getenv "LDK_CODE_CACHE_STATS")
        (format *error-output* "~&;; FASL-CACHE-SKIP ~A.~A~A: ~A~%"
                (name class) (slot-value method 'name) (slot-value method 'descriptor) c))
      nil)
    ;; A full disk, say: compile the method the usual way.
    (error (c)
      (format *error-output* "~&;; FASL-CACHE-ERROR writing ~A.~A~A: ~A~%"
              (name class) (slot-value method 'name) (slot-value method 'descriptor) c)
      nil)))

(defun %fasl-cache-install (class method definition-code dependencies)
  "Install METHOD of CLASS by compiling DEFINITION-CODE to a cached FASL
and loading it, returning true; or return NIL, having installed nothing,
when it can't be cached."
  (let ((path (%fasl-cache-compile class method definition-code dependencies)))
    (when path
      (let ((*fasl-cache-loader* (slot-value class 'ldk-loader))
            (*compile-dependencies* nil))
        (handler-case
            (eq t (catch 'fasl-cache-stale
                    (handler-bind ((warning #'muffle-warning))
                      (load path :verbose nil :print nil))
                    t))
          (error (c)
            (format *error-output* "~&;; FASL-CACHE-ERROR loading ~A: ~A~%" path c)
            (ignore-errors (delete-file path))
            nil))))))
