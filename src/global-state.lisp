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

(defvar *classpath* nil)

;; BIN-NAME is the binary name of a class.  eg: java/lang/String
;; FQ-NAME is the fully qualified name of a class. eg: java.lang.String

;; These two tables only contain entries for classes that have been
;; loaded.
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
(defvar *debug-bytecode* nil)
(defvar *debug-codegen* nil)
(defvar *debug-slynk* nil)
(defvar *debug-trace* nil)
(defvar *debug-trace-args* nil)
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

(defvar *boot-class-loader* nil)

;; Map Java Thread objects to Lisp (bordeaux) threads
(defvar *java-threads* (make-hash-table :test #'eq :synchronized t)
  "Hash table mapping Java Thread objects to bordeaux-threads.")

;; Map Lisp threads to Java Thread objects (for currentThread lookup)
(defvar *lisp-to-java-threads* (make-hash-table :test #'eq :synchronized t)
  "Hash table mapping bordeaux-threads to Java Thread objects.")

;; Track interrupted status for each Thread (not a field in Java 8)
(defvar *thread-interrupted* (make-hash-table :test #'eq :synchronized t)
  "Hash table tracking interrupted status for each Java Thread object.")

(defun %get-java-class-by-bin-name (bin-name &optional fail-ok)
  "Look up a Java class by its binary name BIN-NAME. When FAIL-OK is non-NIL, return NIL instead of asserting."
  (let ((bin-name (cond
                    ((stringp bin-name) bin-name)
                    ((null bin-name)
                     (if fail-ok
                         (return-from %get-java-class-by-bin-name nil)
                         (error "bin-name is NIL in %get-java-class-by-bin-name")))
                    (t (coerce (java-array-data bin-name) 'string)))))
    (assert (stringp bin-name))
    (assert (not (find #\. bin-name)))
    (unless fail-ok
      (assert (gethash bin-name *java-classes-by-bin-name*)))
    (gethash bin-name *java-classes-by-bin-name*)))

(defun %get-java-class-by-fq-name (fq-name &optional fail-ok)
  "Look up a Java class by its fully-qualified Java name FQ-NAME. When FAIL-OK is non-NIL, return NIL instead of asserting."
  (let ((fq-name (cond
                   ((stringp fq-name) fq-name)
                   ((null fq-name)
                    (if fail-ok
                        (return-from %get-java-class-by-fq-name nil)
                        (error "fq-name is NIL in %get-java-class-by-fq-name")))
                   (t (coerce (java-array-data fq-name) 'string)))))
    (assert (stringp fq-name))
    (assert (not (find #\/ fq-name)))
    (unless fail-ok
      (assert (gethash fq-name *java-classes-by-fq-name*)))
    (gethash fq-name *java-classes-by-fq-name*)))

(defun %get-ldk-class-by-bin-name (bin-name &optional fail-ok)
  "Look up an LDK class by its binary name BIN-NAME. When FAIL-OK is non-NIL, return NIL instead of asserting."
  (let ((bin-name (cond
                    ((stringp bin-name) bin-name)
                    ((null bin-name)
                     (if fail-ok
                         (return-from %get-ldk-class-by-bin-name nil)
                         (error "bin-name is NIL in %get-ldk-class-by-bin-name")))
                    (t (coerce (java-array-data bin-name) 'string)))))
    (assert (stringp bin-name))
    (assert (not (find #\. bin-name)))
    (unless fail-ok
      (assert (gethash bin-name *ldk-classes-by-bin-name*)))
    (gethash bin-name *ldk-classes-by-bin-name*)))

(defun %get-ldk-class-by-fq-name (fq-name &optional fail-ok)
  "Look up an LDK class by its fully-qualified Java name FQ-NAME. When FAIL-OK is non-NIL, return NIL instead of asserting."
  (let ((fq-name (cond
                   ((stringp fq-name) fq-name)
                   ((null fq-name)
                    (if fail-ok
                        (return-from %get-ldk-class-by-fq-name nil)
                        (error "fq-name is NIL in %get-ldk-class-by-fq-name")))
                   (t (coerce (java-array-data fq-name) 'string)))))
    (assert (stringp fq-name))
    (unless fail-ok
      (assert (gethash fq-name *ldk-classes-by-fq-name*)))
    (gethash fq-name *ldk-classes-by-fq-name*)))
