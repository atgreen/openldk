;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: OPENLDK; Base: 10 -*-
;;;
;;; Copyright (C) 2024, 2025, 2026  Anthony Green <green@moxielogic.com>
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

;;; Ahead-of-time compilation output.  When *AOT-DIR* is set, compiled
;;; method and class definitions are also written out as Lisp source
;;; files plus a generated ASDF system so they can be built into an image
;;; without transpiling at runtime.

(defun %write-aot-method (class-name method-name definition-code)
  "Write AOT compiled Lisp code to a file in the top-level AOT directory."
  (when *aot-dir*
    (let* ((path-parts (split-sequence:split-sequence #\/ class-name))
           (dir-path (format nil "~A/~{~A~^/~}"
                           *aot-dir*
                           (butlast path-parts)))
           (filename (format nil "~A/~A.lisp"
                           dir-path
                           (car (last path-parts))))
           (method-str (with-output-to-string (s)
                        (let ((*print-case* :downcase))
                          (pprint definition-code s)))))
      (ensure-directories-exist filename)
      ;; Append to file if it exists (multiple methods per class)
      (with-open-file (out filename
                          :direction :output
                          :if-exists :append
                          :if-does-not-exist :create)
        (format out "~%~A~%" method-str)))))

(defun %write-aot-class (class-name class-definition-code)
  "Store AOT class definitions in memory for later topological sorting and writing."
  (when *aot-dir*
    ;; Store the class definition along with its parent class name for sorting
    (let* ((class (gethash class-name *ldk-classes-by-bin-name*))
           (parent-name (when class (slot-value class 'super))))
      (setf (gethash class-name *aot-class-definitions*)
            (list :code class-definition-code :parent parent-name)))))

(defun %topological-sort-classes (class-defs-hash)
  "Topologically sort classes so parents come before children."
  (let ((sorted nil)
        (visited (make-hash-table :test #'equal))
        (visiting (make-hash-table :test #'equal)))
    (labels ((visit (class-name)
               (cond
                 ((gethash class-name visited)
                  ;; Already processed
                  nil)
                 ((gethash class-name visiting)
                  ;; Circular dependency - skip
                  (format t ";   Warning: Circular dependency detected for ~A~%" class-name)
                  nil)
                 (t
                  (setf (gethash class-name visiting) t)
                  (let ((class-info (gethash class-name class-defs-hash)))
                    (when class-info
                      (let ((parent (getf class-info :parent)))
                        ;; Visit parent first if it exists and is in our set
                        (when (and parent (gethash parent class-defs-hash))
                          (visit parent)))
                      ;; Now add this class
                      (push (cons class-name class-info) sorted)
                      (setf (gethash class-name visited) t)))
                  (remhash class-name visiting)))))
      ;; Visit all classes
      (maphash (lambda (class-name class-info)
                 (declare (ignore class-info))
                 (visit class-name))
               class-defs-hash)
      (reverse sorted))))

(defun %write-all-aot-classes (aot-dir)
  "Write all collected class definitions to a single classes.lisp file in topological order."
  (when (and *aot-class-definitions* (> (hash-table-count *aot-class-definitions*) 0))
    (let* ((sorted-classes (%topological-sort-classes *aot-class-definitions*))
           (classes-file (format nil "~A/classes.lisp" aot-dir)))
      (with-open-file (out classes-file
                          :direction :output
                          :if-exists :supersede
                          :if-does-not-exist :create)
        (format out ";;;; AOT-compiled class definitions~%")
        (format out ";;;; Classes are topologically sorted (parents before children)~%~%")
        (dolist (class-entry sorted-classes)
          (let ((class-name (car class-entry))
                (class-code (getf (cdr class-entry) :code)))
            (format out "~%; Class: ~A~%" class-name)
            (let ((*print-case* :downcase))
              (pprint class-code out))
            (format out "~%~%"))))
      (format t "; Wrote ~A class definitions to ~A~%"
              (length sorted-classes) classes-file))))

(defun %generate-aot-asdf-file (aot-dir system-name)
  "Generate an ASDF system definition file that loads classes.lisp then all method files."
  (let* ((aot-dir-path (uiop:ensure-directory-pathname aot-dir))
         (method-files nil))
    ;; Collect all .lisp files in aot-dir (excluding classes.lisp)
    (dolist (file (directory (merge-pathnames "**/*.lisp" aot-dir-path)))
      (let ((filename (file-namestring file)))
        (unless (string= filename "classes.lisp")
          (let* ((file-truename (truename file))
                 (dir-truename (truename aot-dir-path))
                 (relative-path (uiop:enough-pathname file-truename dir-truename))
                 ;; Remove .lisp extension and convert to forward slashes
                 (file-path (substitute #\/ (uiop:directory-separator-for-host)
                                       (subseq (uiop:native-namestring relative-path) 0
                                               (- (length (uiop:native-namestring relative-path)) 5)))))
            (push file-path method-files)))))
    ;; Generate the ASDF file
    (let ((asdf-file (format nil "~A/~A.asd" aot-dir system-name)))
      (with-open-file (out asdf-file
                          :direction :output
                          :if-exists :supersede
                          :if-does-not-exist :create)
        (format out ";;;; ASDF system definition for AOT-compiled Java classes~%~%")
        (format out "(defsystem ~S~%" system-name)
        (format out "  :description \"AOT-compiled Java bytecode to Common Lisp\"~%")
        (format out "  :serial t~%")
        (format out "  :components~%")
        (format out "  (")
        ;; First, load classes.lisp with all class definitions
        (format out "~%   ;; Class definitions (topologically sorted)~%")
        (format out "   (:file \"classes\")~%")
        ;; Then, load all method definitions
        (when method-files
          (format out "~%   ;; Method definitions (loaded after classes)~%")
          (dolist (method-file (sort method-files #'string<))
            (format out "   (:file ~S)~%" method-file)))
        (format out "))~%"))
      (format t "~%; Generated ASDF file: ~A~%" asdf-file))))
