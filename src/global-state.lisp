;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: OPENLDK; Base: 10 -*-
;;;
;;; Copyright (C) 2025  Anthony Green <green@moxielogic.com>
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
(defvar *ldk-classes-by-bin-name* (make-hash-table :test #'equal))
(defvar *ldk-classes-by-fq-name* (make-hash-table :test #'equal))

;; These two tables contain java.lang.Class objects, some of which may
;; not have been loaded yet.  We will populate them when they are
;; loaded.
(defvar *java-classes-by-bin-name* (make-hash-table :test #'equal))
(defvar *java-classes-by-fq-name* (make-hash-table :test #'equal))

(defvar *packages* (make-hash-table :test #'equal))

(defvar *context* nil)
(defvar *condition-table* (make-hash-table))

(defvar *call-nesting-level* 0)

(defvar *dump-dir* nil)
(defvar *debug-load* nil)
(defvar *debug-bytecode* nil)
(defvar *debug-codegen* nil)
(defvar *debug-slynk* nil)
(defvar *debug-trace* nil)
(defvar *debug-trace-args* nil)
(defvar *debug-x* nil)
(defvar *debug-unmuffle* nil)

(defvar *boot-class-loader* nil)

(defun %get-java-class-by-bin-name (bin-name &optional fail-ok)
  (let ((bin-name (if (stringp bin-name)
                      bin-name
                      (coerce (java-array-data bin-name) 'string))))
    (assert (stringp bin-name))
    (assert (not (find #\. bin-name)))
    (unless fail-ok
      (assert (gethash bin-name *java-classes-by-bin-name*)))
    (gethash bin-name *java-classes-by-bin-name*)))

(defun %get-java-class-by-fq-name (fq-name &optional fail-ok)
  (let ((fq-name (if (stringp fq-name)
                     fq-name
                     (coerce (java-array-data fq-name) 'string))))
    (assert (stringp fq-name))
    (assert (not (find #\/ fq-name)))
    (unless fail-ok
      (assert (gethash fq-name *java-classes-by-fq-name*)))
    (gethash fq-name *java-classes-by-fq-name*)))

(defun %get-ldk-class-by-bin-name (bin-name &optional fail-ok)
  (let ((bin-name (if (stringp bin-name)
                      bin-name
                      (coerce (java-array-data bin-name) 'string))))
    (assert (stringp bin-name))
    (assert (not (find #\. bin-name)))
    (unless fail-ok
      (assert (gethash bin-name *ldk-classes-by-bin-name*)))
    (gethash bin-name *ldk-classes-by-bin-name*)))

(defun %get-ldk-class-by-fq-name (fq-name &optional fail-ok)
  (let ((fq-name (if (stringp fq-name)
                     fq-name
                     (coerce (java-array-data fq-name) 'string))))
    (assert (stringp fq-name))
    (assert (not (find #\/ fq-name)))
    (unless fail-ok
      (assert (gethash fq-name *ldk-classes-by-fq-name*)))
    (gethash fq-name *ldk-classes-by-fq-name*)))
