;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: OPENLDK; Base: 10 -*-
;;;
;;; Copyright (C) 2023, 2024, 2025  Anthony Green <green@moxielogic.com>
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

(defclass classpath-entry ()
  ()
  (:documentation "Abstract base for classpath locations."))

(defclass jar-classpath-entry (classpath-entry)
  ((jarfile :initarg :jarfile)
   (zipfile :initform nil)
   (zipfile-entries))
  (:documentation "Classpath entry backed by a JAR/ZIP file."))

(defclass dir-classpath-entry (classpath-entry)
  ((dir :initarg :dir))
  (:documentation "Classpath entry for a directory tree."))

;; Modify the :around method to establish the restart at the right time
(defmethod open-java-classfile :around ((cpe jar-classpath-entry) classname)
  (restart-case
      (handler-case
          (call-next-method)
        (sb-int:closed-saved-stream-error ()
          (invoke-restart 'reopen-zipfile)))
    (reopen-zipfile ()
      :report "Reopen the zipfile and retry."
      (with-slots (zipfile zipfile-entries jarfile) cpe
        (setf zipfile (zip:open-zipfile jarfile))
        (setf zipfile-entries (zip:zipfile-entries zipfile))
        (open-java-classfile cpe classname)))))

;; And simplify the primary method - no restart needed here
(defmethod open-java-classfile ((cpe jar-classpath-entry) classname)
  "Return an input stream for a java class, CLASSNAME."
  (with-slots (jarfile zipfile zipfile-entries) cpe
    ;; Ensure the zipfile is open
    (unless zipfile
      (setf zipfile (zip:open-zipfile jarfile))
      (setf zipfile-entries (zip:zipfile-entries zipfile)))
    ;; Look up the class file in the zipfile entries
    (when-let (ze (gethash (format nil "~A.class" classname) zipfile-entries))
      ;; Create an in-memory input stream for the class file contents
      (let ((result (flexi-streams:make-in-memory-input-stream (zip:zipfile-entry-contents ze))))
        ;; Add the package to the *PACKAGE* hashtable if it doesn't already exist
        (when-let (last-slash-position (position #\/ classname :from-end t))
          (let ((package-name (take (1+ last-slash-position) classname)))
            (unless (gethash package-name *packages*)
              (setf (gethash package-name *packages*) (jstring jarfile)))))
        ;; Return the input stream
        result))))

(defmethod open-java-classfile ((cpe dir-classpath-entry) classname)
  "Return an input stream for a java class, CLASSNAME."
  (with-slots (dir) cpe
    (let ((fqn (format nil "~A~A~A.class" dir (uiop:directory-separator-for-host) classname)))
      (when (uiop:file-exists-p fqn)
        ;; Read into memory and return an in-memory input stream
        (let* ((bytes (read-file-into-byte-vector fqn))
               (result (flexi-streams:make-in-memory-input-stream bytes)))
          ;; Add this package to the *PACKAGE* hashtable.
          (when-let (last-slash-position (position #\/ classname :from-end t))
            (let ((package-name (take (1+ last-slash-position) classname)))
              (unless (gethash package-name *packages*)
                (setf (gethash package-name *packages*) (jstring fqn)))))
          result)))))
