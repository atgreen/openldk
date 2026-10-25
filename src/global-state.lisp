;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: OPENLDK; Base: 10 -*-
;;;
;;; Copyright (C) 2025  Anthony Green <green@moxielogic.com>
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

;; Ensure OPENLDK.SYSTEM package exists at compile/load time
;; This is the boot loader's package - classes loaded during warm-up go here
(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (find-package "OPENLDK.SYSTEM")
    (make-package "OPENLDK.SYSTEM" :use '(:openldk))))

(defvar *classpath* nil)


;; Counter for generating unique class loader IDs
(defvar *next-loader-id* 0)
(defvar *next-loader-id-lock* (bordeaux-threads:make-lock "loader-id-lock"))

;; Class loader structure for per-loader package support
;; Each class loader has its own Lisp package where class/method symbols are interned
(defclass/std <ldk-class-loader> ()
  ((id :std 0)                            ; unique identifier
   (pkg :std nil)                         ; Lisp package for this loader
   (parent-loader :std nil)               ; parent <ldk-class-loader> for delegation (or NIL)
   (java-loader :std nil)                 ; corresponding java.lang.ClassLoader object
   (ldk-classes-by-bin-name :std nil)     ; per-loader hash table for LDK classes
   (ldk-classes-by-fq-name :std nil)      ; per-loader hash table by FQ name
   (java-classes-by-bin-name :std nil)    ; per-loader hash table for java.lang.Class
   (java-classes-by-fq-name :std nil)))   ; per-loader hash table by FQ name

(defmethod print-object ((loader <ldk-class-loader>) out)
  (print-unreadable-object (loader out :type t)
    (format out "~A pkg=~A" (slot-value loader 'id) (slot-value loader 'pkg))))

(defvar *ldk-loaders-by-package* (make-hash-table :test #'eq :synchronized t)
  "Lisp package -> the <ldk-class-loader> whose classes are named in it.")

(defun make-ldk-class-loader (&key parent-loader java-loader package-name)
  "Create a new LDK class loader with its own package and class maps.
   PARENT-LOADER is the parent <ldk-class-loader> for delegation.
   JAVA-LOADER is the corresponding java.lang.ClassLoader object.
   PACKAGE-NAME is the name for the Lisp package (auto-generated if NIL).

   Package hierarchy for class isolation:
   - OPENLDK.SYSTEM for bootstrap loader (parent=nil)
   - OPENLDK.APP for application class loader
   - OPENLDK.L1, OPENLDK.L2, etc. for user-defined loaders

   Each package :use's its parent loader's package for symbol resolution,
   implementing Java's parent delegation model at the Lisp package level."
  (let* ((id (bordeaux-threads:with-lock-held (*next-loader-id-lock*)
               (incf *next-loader-id*)))
         (pkg-name (or package-name (format nil "OPENLDK.L~A" id)))
         ;; Use the parent loader's package for symbol delegation
         ;; If no parent, use :openldk for base definitions
         (parent-pkg (if parent-loader
                         (slot-value parent-loader 'pkg)
                         (find-package :openldk)))
         (pkg (or (find-package pkg-name)
                  (make-package pkg-name :use (list parent-pkg))))
         (loader (make-instance '<ldk-class-loader>
                                :id id
                                :pkg pkg
                                :parent-loader parent-loader
                                :java-loader java-loader
                                :ldk-classes-by-bin-name (make-hash-table :test #'equal :synchronized t)
                                :ldk-classes-by-fq-name (make-hash-table :test #'equal :synchronized t)
                                :java-classes-by-bin-name (make-hash-table :test #'equal :synchronized t)
                                :java-classes-by-fq-name (make-hash-table :test #'equal :synchronized t))))
    (setf (gethash pkg *ldk-loaders-by-package*) loader)
    loader))

(defun %ldk-loader-for-symbol (symbol)
  "The <ldk-class-loader> whose package SYMBOL (a CLOS class name) is in,
or NIL for the boot loader's packages."
  (gethash (symbol-package symbol) *ldk-loaders-by-package*))

;; The boot class loader - created during initialization with OPENLDK.SYSTEM package
;; Loads core JDK classes (java/*, javax/*, sun/*, etc.) into :openldk package
(defvar *boot-ldk-class-loader* nil)

;; The application class loader - loads user classes from CLASSPATH
;; Child of boot loader, uses OPENLDK.APP package
(defvar *app-ldk-class-loader* nil)

;; Map from java.lang.ClassLoader objects to <ldk-class-loader> objects
(defvar *java-to-ldk-loaders* (make-hash-table :test #'eq :synchronized t))

(defun jdk-class-p (classname)
  "Return T if CLASSNAME is a JDK/system class that should be loaded by boot loader."
  (or (str:starts-with? "java/" classname)
      (str:starts-with? "javax/" classname)
      (str:starts-with? "sun/" classname)
      (str:starts-with? "com/sun/" classname)
      (str:starts-with? "jdk/" classname)
      (str:starts-with? "org/xml/" classname)
      (str:starts-with? "org/w3c/" classname)))

(defun get-ldk-loader-for-java-loader (java-loader)
  "Get or create the <ldk-class-loader> for a java.lang.ClassLoader object.
   Returns the boot loader if JAVA-LOADER is NIL.

   Each user-defined loader gets its own package, so classes of the same name
   defined by different loaders are distinct CLOS classes."
  (if (null java-loader)
      *boot-ldk-class-loader*
      (or (gethash java-loader *java-to-ldk-loaders*)
          ;; Create a new LDK loader for this Java loader
          (let* ((parent-java-loader (when (slot-boundp java-loader '|parent|)
                                       (slot-value java-loader '|parent|)))
                 (parent-ldk-loader (if parent-java-loader
                                        (get-ldk-loader-for-java-loader parent-java-loader)
                                        *boot-ldk-class-loader*))
                 ;; Every user-defined loader gets its own package
                 ;; (OPENLDK.L<n>).  Sharing the parent's package made two
                 ;; loaders' classes of the same name one CLOS class: the second
                 ;; definition redefined the first in place.
                 (new-loader (make-ldk-class-loader :parent-loader parent-ldk-loader
                                                    :java-loader java-loader)))
            (setf (gethash java-loader *java-to-ldk-loaders*) new-loader)
            new-loader))))

(defun loader-package (loader)
  "Get the Lisp package for a class loader. Uses :openldk for boot loader.
   Boot loader classes use :openldk to match bootstrap class definitions.
   Only user loaders get their own packages (OPENLDK.L1, etc)."
  (if loader
      (let ((pkg (slot-value loader 'pkg)))
        ;; Boot loader uses :openldk (matches bootstrap classes)
        (if (eq pkg (find-package "OPENLDK.SYSTEM"))
            (find-package :openldk)
            pkg))
      ;; No loader means boot loader - use :openldk
      (find-package :openldk)))

(defun %context-loader ()
  "The <ldk-class-loader> of the class whose method is being compiled, or NIL."
  (declare (special *context*))
  (and (boundp '*context*) *context* (slot-value *context* 'ldk-loader)))

(defun class-package (class-name &optional (fallback-loader (%context-loader)))
  "Get the Lisp package for a class by its binary name.
   When FALLBACK-LOADER is provided, searches that loader's hierarchy first;
   it defaults to the loader of the class being compiled, if any, so a
   reference resolves among that loader's classes before the global table,
   which holds the last-defined class of each name.
   Falls back to global tables, then app loader, then :openldk."
  ;; Search with fallback-loader first if provided, then global table
  (let ((class (or (and fallback-loader
                        (%get-ldk-class-by-bin-name class-name t fallback-loader))
                   (%get-ldk-class-by-bin-name class-name t))))
    (cond
      ;; Found the class - use its defining loader's package
      ((and class
            (slot-boundp class 'ldk-loader)
            (slot-value class 'ldk-loader))
       (loader-package (slot-value class 'ldk-loader)))
      ;; Class found but no loader - check OPENLDK.SYSTEM (warm-up classes)
      ((and class
            (find-package "OPENLDK.SYSTEM")
            (find-symbol (format nil "+static-~A+" class-name) (find-package "OPENLDK.SYSTEM")))
       (find-package "OPENLDK.SYSTEM"))
      ;; Class found but no loader - use :openldk
      (class
       (find-package :openldk))
      ;; Not found - check app loader
      ((and *app-ldk-class-loader*
            (gethash class-name (slot-value *app-ldk-class-loader* 'ldk-classes-by-bin-name)))
       (loader-package *app-ldk-class-loader*))
      ;; Not found anywhere - fall back to :openldk
      (t
       (find-package :openldk)))))


(defun ldk-class-package (class)
  "Get the Lisp package for CLASS, an LDK class object, from its defining
loader.  Unlike CLASS-PACKAGE, this finds classes defined by user class
loaders, whose classes are not in the global or app loader tables."
  (if (and (slot-boundp class 'ldk-loader)
           (slot-value class 'ldk-loader))
      (loader-package (slot-value class 'ldk-loader))
      (class-package (slot-value class 'name))))

(defun class-symbol-for-reference (class-name loader)
  "Get the Lisp symbol for CLASS-NAME when referenced from LOADER.
   Looks up the class in the loader hierarchy to find its defining loader's
   package, then resolves it as the JVM would (%RESOLVE-CLASS, which can ask
   the Java loader): a class from a loader outside LOADER's parent chain,
   such as one of Clojure's per-evaluation DynamicClassLoaders, is named in
   its own loader's package.  If not found, interns in LOADER's package
   (the class will be defined there)."
  (if-let ((klass (or (%get-ldk-class-by-bin-name class-name t loader)
                      (%resolve-class class-name loader))))
    ;; Class found - use its defining loader's package via loader-package
    ;; (loader-package handles boot loader -> :openldk mapping)
    (let ((pkg (loader-package (slot-value klass 'ldk-loader))))
      (intern class-name pkg))
    ;; Class not yet loaded - intern in this loader's package
    (intern class-name (loader-package loader))))

(defun static-method-symbol (method-name loader-pkg)
  "Get the symbol for a static method name (its full descriptor).
   First checks :openldk for native method implementations, by the full
   name and then without the return type (as hand-written natives may be
   named), then falls back to the loader's package for Java-defined methods."
  (let* ((openldk (find-package :openldk))
         (full (find-symbol method-name openldk))
         (short (find-symbol (subseq method-name 0 (1+ (or (position #\) method-name)
                                                          (1- (length method-name)))))
                             openldk)))
    (cond ((and full (fboundp full)) full)
          ((and short (fboundp short)) short)
          (t (intern method-name loader-pkg)))))

(defun %make-java-instance (class-name &optional ldk-loader)
  "Create an instance of a Java class using the correct class symbol.
   CLASS-NAME is the binary name (e.g. \"java/lang/String\").
   First checks :openldk for bootstrap/shared classes, then falls back to
   the class's defining loader's package, looked up through LDK-LOADER when
   given (another loader may have a class of the same name)."
  ;; First try :openldk - this handles bootstrap classes and ensures
  ;; method dispatch works for methods defined on bootstrap classes
  (let ((openldk-sym (find-symbol class-name :openldk)))
    (when (and openldk-sym (find-class openldk-sym nil))
      (return-from %make-java-instance (make-instance openldk-sym))))
  ;; Fall back to the loader's package for user-defined classes
  (let* ((pkg (if ldk-loader
                  (class-package class-name ldk-loader)
                  (class-package class-name)))
         (sym (intern class-name pkg)))
    (unless (find-class sym nil)
      ;; Not loaded yet (e.g. ArrayIndexOutOfBoundsException thrown before
      ;; anything referenced it) — load it now.  CLASSLOAD may emit the
      ;; class into the system package rather than PKG.
      (ignore-errors (classload class-name))
      (unless (find-class sym nil)
        (let ((sys-sym (find-symbol class-name
                                    (or (find-package "OPENLDK.SYSTEM") pkg))))
          (when (and sys-sym (find-class sys-sym nil))
            (setf sym sys-sym)))))
    (if (find-class sym nil)
        (make-instance sym)
        (internal-error "Class not found: ~A" class-name))))

;; BIN-NAME is the binary name of a class.  eg: java/lang/String
;; FQ-NAME is the fully qualified name of a class. eg: java.lang.String

;; These global tables are kept for backward compatibility during transition
;; They serve as the boot loader's class maps
(defvar *ldk-classes-by-bin-name* (make-hash-table :test #'equal :synchronized t))
(defvar *ldk-classes-by-fq-name* (make-hash-table :test #'equal :synchronized t))

;; System properties storage to avoid recursion during initialization
(defvar *ldk-system-properties* (make-hash-table :test #'equal :synchronized t))

;; These two tables contain java.lang.Class objects, some of which may
;; not have been loaded yet.  We will populate them when they are
;; loaded.
(defvar *java-classes-by-bin-name* (make-hash-table :test #'equal :synchronized t))
(defvar *java-classes-by-fq-name* (make-hash-table :test #'equal :synchronized t))

(defvar *packages* (make-hash-table :test #'equal :synchronized t))

(defvar *context* nil)
(defvar *condition-table* (make-hash-table :synchronized t))

(defvar *call-nesting-level* 0)

(defvar *dump-dir* nil)
(defvar *aot-dir* nil)
(defvar *aot-class-definitions* nil "Hash table storing class definitions for AOT compilation")
(defvar *debug-load* nil)
(defvar *debug-compile* nil)
(defvar *debug-bytecode* nil)
(defvar *debug-codegen* nil)
(defvar *debug-slynk* nil)
(defvar *debug-trace* nil)
(defvar *npe-fault-count* 0)
(defvar *debug-trace-args* nil)
(defvar *debug-exceptions* nil)
(defvar *debug-x* nil)
(defvar *debug-unmuffle* nil)
(defvar *debug-propagation* nil)
;; When true, log when set-enclosing-type related code paths run (see src/openldk.lisp).
(defvar *debug-set-enclosing-type* nil)

;; Experimental: basic copy/constant propagation over IR. Now enabled in Phase 1.
(defvar *enable-copy-propagation* t)

;; Enable/disable dead code elimination (DCE) over IR assignments
;; Default enabled
(defvar *enable-dce* t)

;; Phase 2: Intra-block local variable propagation. Enabled for testing.
;; Propagates local variables within basic blocks when no intervening assignments exist.
(defvar *enable-local-propagation* t)

;; Phase 3: Inter-block local variable propagation via reaching definitions. Disabled temporarily for debugging.
;; Uses dataflow analysis to propagate locals across basic blocks when a unique definition reaches.
(defvar *enable-reaching-definitions* nil)

;; Don't quit.  Used in build process during warm-up.
(defvar *ignore-quit* nil)

(defvar *cli-jvm-properties* nil
  "List of (key . value) pairs from -D command line options.")

(defvar *boot-class-loader* nil)

(defvar *unnamed-module* nil
  "An unnamed java.lang.Module for bootstrap classes (JDK 9+).")

(defvar *default-mainclass* nil
  "Default main class baked into an app image by dump-app-image.")

(defvar *default-classpath* nil
  "Default classpath string baked into an app image by dump-app-image.")

;; Map Java Thread objects to Lisp (bordeaux) threads
(defvar *java-threads* (make-hash-table :test #'eq :synchronized t)
  "Hash table mapping Java Thread objects to bordeaux-threads.")

;; Map Lisp threads to Java Thread objects (for currentThread lookup)
(defvar *lisp-to-java-threads* (make-hash-table :test #'eq :synchronized t)
  "Hash table mapping bordeaux-threads to Java Thread objects.")

;; Serializes all Unsafe compare-and-swap/exchange emulation.  A single
;; recursive lock (compareAndSwapLong delegates to compareAndSwapInt on one
;; path) is the simplest way to make the read-compare-write sequences
;; atomic with respect to each other.
(defvar *cas-lock* (bordeaux-threads:make-recursive-lock "unsafe-cas"))

;; Epoch milliseconds when this VM run started; set on the first entry to
;; MAIN, reset before image dump so restarted images re-stamp themselves.
(defvar *vm-start-time-millis* nil)

;; Track interrupted status for each Thread (not a field in Java 8)
(defvar *thread-interrupted* (make-hash-table :test #'eq :synchronized t)
  "Hash table tracking interrupted status for each Java Thread object.")

;; Identity hash code support: each object gets a unique, stable hash
;; (Java's identityHashCode semantics). Uses a weak EQ hash table so
;; objects can still be GC'd.
(defvar *identity-hash-counter* 0)
(defvar *identity-hash-counter-lock* (bordeaux-threads:make-lock "identity-hash-lock"))
(defvar *identity-hash-table* (make-hash-table :test #'eq :weakness :key :synchronized t))

;; Methods registered here will not have their native Lisp implementations
;; overwritten by bytecode compilation.  Key is the method key string
;; (e.g. "java/lang/System.console()Ljava/io/Console;").
(defvar *native-overrides* (make-hash-table :test #'equal))

(defvar *protection-domains* (make-hash-table :test #'eq :weakness :key :synchronized t)
  "java.lang.Class -> the ProtectionDomain its class loader defined it with.")

;; JVM field shadowing: a class may declare an instance field with the same
;; name as one in a superclass (javac's synthetic this$0 outer references do
;; this routinely); they are distinct fields resolved by declaring class.
;; Shadowing fields get a class-qualified CLOS slot; this table maps
;; "class-bin-name.field-name" -> that slot symbol.
(defvar *field-shadow-slots* (make-hash-table :test #'equal :synchronized t))

;; Per-class-name locks for thread-safe class loading.
;; Maps class binary name (string) -> recursive lock.
(defvar *class-load-locks* (make-hash-table :test #'equal :synchronized t))
(defvar *class-load-locks-lock* (bordeaux-threads:make-lock "class-load-locks"))

(defun %get-class-lock (classname)
  "Get or create a recursive lock for loading CLASSNAME.
   Uses double-checked locking: fast path reads from synchronized hash table,
   slow path acquires global lock to create."
  (or (gethash classname *class-load-locks*)
      (bordeaux-threads:with-lock-held (*class-load-locks-lock*)
        (or (gethash classname *class-load-locks*)
            (setf (gethash classname *class-load-locks*)
                  (bordeaux-threads:make-recursive-lock
                   (format nil "classload-~A" classname)))))))

;;; The FASL cache's record of the classes a method's compilation looks at
;;; (see fasl-cache.lisp).

(defvar *compile-dependencies* nil
  "While a method compiles for the FASL cache, a hash table whose keys are
the binary names of the classes the compiler looked up.")

(defvar *compile-call-site-gfs* nil
  "While a method compiles for the FASL cache, a hash table mapping each
virtual-call-site generic function symbol the code references to its
argument count.  Codegen creates these generic functions as a side effect
(see %CALL-SITE-GF); a cached FASL, loaded without recompiling, must
recreate them itself, so they are recorded and re-ensured on load.")

(declaim (inline %note-call-site-gf))
(defun %note-call-site-gf (symbol nargs)
  "Record that the compilation in progress calls the generic function
SYMBOL with NARGS arguments."
  (when *compile-call-site-gfs*
    (setf (gethash symbol *compile-call-site-gfs*) nargs)))

(defun %md5-hex (octets)
  "The MD5 digest of OCTETS, in hex."
  (format nil "~(~{~2,'0x~}~)" (coerce (sb-md5:md5sum-sequence octets) 'list)))

(declaim (inline %note-class-dependency))
(defun %note-class-dependency (bin-name)
  "Record that the compilation in progress looked up the class BIN-NAME."
  (when *compile-dependencies*
    (setf (gethash bin-name *compile-dependencies*) t)))

(defun %get-java-class-by-bin-name (bin-name &optional fail-ok loader)
  "Look up a Java class by its binary name BIN-NAME.
   When FAIL-OK is non-NIL, return NIL instead of asserting.
   When LOADER is provided, search the loader's class maps with parent delegation."
  (let ((bin-name (cond
                    ((stringp bin-name) bin-name)
                    ((null bin-name)
                     (if fail-ok
                         (return-from %get-java-class-by-bin-name nil)
                         (internal-error "bin-name is NIL in %get-java-class-by-bin-name")))
                    (t (%char-data-string (java-array-data bin-name))))))
    (assert (stringp bin-name))
    (assert (not (find #\. bin-name)))
    (%note-class-dependency bin-name)
    (let ((result (if loader
                      ;; Search with parent delegation
                      (%lookup-class-with-delegation loader bin-name 'java-classes-by-bin-name)
                      ;; Fall back to global tables for backward compatibility
                      (gethash bin-name *java-classes-by-bin-name*))))
      (unless (or fail-ok result)
        (internal-error "Class ~A not found in %get-java-class-by-bin-name" bin-name))
      result)))

(defun %get-java-class-by-fq-name (fq-name &optional fail-ok loader)
  "Look up a Java class by its fully-qualified Java name FQ-NAME.
   When FAIL-OK is non-NIL, return NIL instead of asserting.
   When LOADER is provided, search the loader's class maps with parent delegation."
  (let ((fq-name (cond
                   ((stringp fq-name) fq-name)
                   ((null fq-name)
                    (if fail-ok
                        (return-from %get-java-class-by-fq-name nil)
                        (internal-error "fq-name is NIL in %get-java-class-by-fq-name")))
                   (t (%char-data-string (java-array-data fq-name))))))
    (assert (stringp fq-name))
    (assert (not (find #\/ fq-name)))
    (%note-class-dependency (substitute #\/ #\. fq-name))
    (let ((result (if loader
                      (%lookup-class-with-delegation loader fq-name 'java-classes-by-fq-name)
                      (gethash fq-name *java-classes-by-fq-name*))))
      (unless (or fail-ok result)
        (internal-error "Class ~A not found in %get-java-class-by-fq-name" fq-name))
      result)))

(defun %get-ldk-class-by-bin-name (bin-name &optional fail-ok loader)
  "Look up an LDK class by its binary name BIN-NAME.
   When FAIL-OK is non-NIL, return NIL instead of asserting.
   When LOADER is provided, search the loader's class maps with parent delegation."
  (let ((bin-name (cond
                    ((stringp bin-name) bin-name)
                    ((null bin-name)
                     (if fail-ok
                         (return-from %get-ldk-class-by-bin-name nil)
                         (internal-error "bin-name is NIL in %get-ldk-class-by-bin-name")))
                    (t (%char-data-string (java-array-data bin-name))))))
    (assert (stringp bin-name))
    (assert (not (find #\. bin-name)))
    (%note-class-dependency bin-name)
    (let ((result (if loader
                      (%lookup-class-with-delegation loader bin-name 'ldk-classes-by-bin-name)
                      (gethash bin-name *ldk-classes-by-bin-name*))))
      (unless (or fail-ok result)
        (internal-error "Class ~A not found in %get-ldk-class-by-bin-name" bin-name))
      result)))

(defun %get-ldk-class-by-fq-name (fq-name &optional fail-ok loader)
  "Look up an LDK class by its fully-qualified Java name FQ-NAME.
   When FAIL-OK is non-NIL, return NIL instead of asserting.
   When LOADER is provided, search the loader's class maps with parent delegation."
  (let ((fq-name (cond
                   ((stringp fq-name) fq-name)
                   ((null fq-name)
                    (if fail-ok
                        (return-from %get-ldk-class-by-fq-name nil)
                        (internal-error "fq-name is NIL in %get-ldk-class-by-fq-name")))
                   (t (%char-data-string (java-array-data fq-name))))))
    (assert (stringp fq-name))
    (%note-class-dependency (substitute #\/ #\. fq-name))
    (let ((result (if loader
                      (%lookup-class-with-delegation loader fq-name 'ldk-classes-by-fq-name)
                      (gethash fq-name *ldk-classes-by-fq-name*))))
      (unless (or fail-ok result)
        (internal-error "Class ~A not found in %get-ldk-class-by-fq-name" fq-name))
      result)))

(defun %user-ldk-loader-p (ldk-loader)
  "True when LDK-LOADER belongs to a user-defined Java class loader, not the
boot or app loader."
  (and ldk-loader
       (not (eq ldk-loader *boot-ldk-class-loader*))
       (not (eq ldk-loader *app-ldk-class-loader*))
       (not (eq (slot-value ldk-loader 'pkg) (find-package "OPENLDK.SYSTEM")))))

(defun %defined-by-user-loader-p (class slot-name)
  "True when CLASS, found in a table named SLOT-NAME (an LDK class or a
java.lang.Class), was defined by a user-defined class loader."
  (if (member slot-name '(ldk-classes-by-bin-name ldk-classes-by-fq-name))
      (%user-ldk-loader-p (and (slot-boundp class 'ldk-loader)
                               (slot-value class 'ldk-loader)))
      (let ((java-loader (and (slot-exists-p class '|classLoader|)
                              (slot-boundp class '|classLoader|)
                              (slot-value class '|classLoader|))))
        (and java-loader
             (%user-ldk-loader-p (gethash java-loader *java-to-ldk-loaders*))))))

(defun %lookup-class-with-delegation (loader name slot-name)
  "Look up a class in LOADER's hash table (SLOT-NAME) with parent delegation.
   Returns NIL if not found in any loader in the chain.
   The boot loader's tables are the global tables, which also hold every
   user loader's classes; delegating to the boot loader never returns
   those, or a loader would find a same-named class of an unrelated one."
  (when loader
    (or (let ((class (gethash name (slot-value loader slot-name))))
          (if (and class
                   (eq loader *boot-ldk-class-loader*)
                   (%defined-by-user-loader-p class slot-name))
              nil
              class))
        (when-let ((parent (slot-value loader 'parent-loader)))
          (%lookup-class-with-delegation parent name slot-name)))))
