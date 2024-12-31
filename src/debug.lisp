;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: OPENLDK; Base: 10 -*-
;;;
;;; Copyright (C) 2023, 2024  Anthony Green <green@moxielogic.com>
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

(defun dump-classes ()
	(maphash (lambda (k v)
							 (format t "~A: ~A~%" k v))
					 *classes*))

(defun dump-method-dot (blocks)
  (when *dump-dir*
    (let ((class-name (slot-value (slot-value *context* 'class) 'name))
          (fn-name (slot-value *context* 'fn-name))
          (dt (make-hash-table)))
      (let ((ffn-name (substitute #\. #\/ (if (alexandria:starts-with-subseq class-name fn-name)
                                              fn-name
                                              (format nil "~A~A" class-name fn-name)))))
        (let* ((namestring (format nil "~A~A~A.dot" *dump-dir*
                                   (uiop:directory-separator-for-host)
                                   ffn-name))
               (pathname (pathname-utils:parse-native-namestring namestring)))
          (uiop:ensure-all-directories-exist
           (list (pathname-utils:parent pathname)))
          (with-open-file (stream pathname :direction :output :if-does-not-exist :create :if-exists :supersede)
            (format stream "digraph code {~%graph [rankdir=TB];~%node [shape=box];~%")
            (dolist (b blocks)
              (dump-dot b dt stream))
            (format stream "}~%")))))))

(defun dump (id obj)
  "Dump OBJ, *CONTEXT* and *CLASSES* to disk in *DUMP-DIR*/[current-class]/ID."
  (when *dump-dir*
    (let ((class-name (slot-value (slot-value *context* 'class) 'name))
          (fn-name (slot-value *context* 'fn-name)))
      (let ((ffn-name (if (alexandria:starts-with-subseq class-name fn-name)
                          fn-name
                          (format nil "~A~A" class-name fn-name))))
        (let* ((namestring (format nil "~A~A~A.~A" *dump-dir*
                                   (uiop:directory-separator-for-host)
                                   ffn-name id))
               (pathname (pathname-utils:parse-native-namestring namestring)))
          (uiop:ensure-all-directories-exist
           (list (pathname-utils:parent pathname)))
          (cl-store:store (list obj *context* *classes*) pathname))))))

(defun restore (filename)
  "Restore an object from the given file, as well as *CONTEXT* and
*CLASSES* at the time of dump."
  (let ((v (cl-store:restore
            (pathname-utils:parse-native-namestring filename))))
    (setf *context* (cadr v))
    (setf *classes* (caddr v))
    (car v)))
