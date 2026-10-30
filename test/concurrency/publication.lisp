;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER; Base: 10 -*-
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

;;; Deterministically exercise publication before class preparation completes.
(require :asdf)
(push (uiop:getcwd) asdf:*central-registry*)
(asdf:load-system :openldk)
(openldk:initialize)
(openldk::main "PublicationSetup" '() :classpath (uiop:getenv "LDK_PUBLICATION_TEST_DIR"))

(defclass publication-failing-stream (trivial-gray-streams:fundamental-binary-input-stream)
  ((delegate :initarg :delegate :reader publication-stream-delegate)))

(defmethod trivial-gray-streams:stream-read-byte ((stream publication-failing-stream))
  (read-byte (publication-stream-delegate stream) nil :eof))

(defmethod stream-element-type ((stream publication-failing-stream))
  '(unsigned-byte 8))

(defmethod close ((stream publication-failing-stream) &key abort)
  (close (publication-stream-delegate stream) :abort abort)
  (error "Injected close failure"))

(defun load-publication-class (name close-failure)
  (if close-failure
      ;; Exercise the direct stream-definition path used by native defineClass.
      (let ((stream (make-instance 'publication-failing-stream
                                  :delegate (open (merge-pathnames (concatenate 'string name ".class")
                                                                   (uiop:ensure-directory-pathname
                                                                    (uiop:getenv "LDK_PUBLICATION_TEST_DIR")))
                                                  :element-type '(unsigned-byte 8)))))
        (openldk::%classload-from-stream name stream nil openldk::*app-ldk-class-loader*))
      (openldk::classload name)))

(defun check-publication (name fail &optional mirror-phase close-failure)
  (let* ((hook (if mirror-phase 'openldk::%record-app-class-package 'openldk::%check-class-inheritance))
         (original (fdefinition hook))
         (published (sb-thread:make-semaphore))
         (resume (sb-thread:make-semaphore))
         (started (sb-thread:make-semaphore))
         (returned (sb-thread:make-semaphore))
         (writer nil) (readers nil) (definition nil) (writer-result nil))
    (unwind-protect
         (progn
           (setf (fdefinition hook)
                 (lambda (&rest arguments)
                   (let ((class (first arguments)))
                     (when (string= (openldk::name class) name)
                       (setf definition class)
                       ;; The owning thread can resolve its provisional definition.
                       (assert (eq class (openldk::classload name)))
                       (assert (eq class (openldk::%initiated-class name (openldk::ldk-loader class))))
                       (sb-thread:signal-semaphore published)
                       (assert (sb-thread:wait-on-semaphore resume :timeout 30))
                       (when fail (error "Injected preparation failure"))))
                   (apply original arguments)))
           (setf writer
                 (sb-thread:make-thread
                  (lambda ()
                    (setf writer-result
                          (handler-case (load-publication-class name close-failure)
                            (simple-error (condition) (princ-to-string condition)))))
                  :name "publication writer"))
           (assert (sb-thread:wait-on-semaphore published :timeout 30))
           (let* ((loader (openldk::ldk-loader definition))
                  (lookups (list (lambda () (openldk::classload name))
                                 (lambda () (openldk::%get-ldk-class-by-bin-name name t loader))
                                 (lambda () (openldk::%get-ldk-class-by-fq-name name t loader))
                                 (lambda () (openldk::%initiated-class name loader))
                                 (lambda () (openldk::find-class-in-loader-hierarchy name loader)))))
             (when mirror-phase
               (dolist (lookup (list (lambda () (openldk::%get-java-class-by-bin-name name t loader))
                                     (lambda () (openldk::%get-java-class-by-fq-name name t loader))))
                 (let ((operation lookup))
                   (push (lambda ()
                           (when (funcall operation)
                             (assert (eq (funcall operation) (openldk::java-class definition)))
                             definition))
                         lookups))))
             (setf readers
                   (loop for lookup in lookups
                         collect (let ((operation lookup))
                                   (sb-thread:make-thread
                                    (lambda ()
                                      (sb-thread:signal-semaphore started)
                                      (let ((class (funcall operation)))
                                        (when class
                                          (assert (openldk::java-class class))
                                          (assert (find-class (intern name (openldk::ldk-class-package class)) nil))
                                          (assert (fboundp (intern (concatenate 'string "%clinit-" name)
                                                                   (openldk::ldk-class-package class)))))
                                        (sb-thread:signal-semaphore returned)
                                        class))
                                    :name "publication reader"))))
             (dotimes (index (length readers))
               (assert (sb-thread:wait-on-semaphore started :timeout 30)))
             (assert (not (sb-thread:wait-on-semaphore returned :timeout 0.5))
                     () "A reader returned a provisional class")
             (sb-thread:signal-semaphore resume)
             (sb-thread:join-thread writer :timeout 30)
             (dolist (reader readers)
               (let ((result (sb-thread:join-thread reader :timeout 30)))
                 (assert (if fail (null result) (eq result definition)))))
             (if fail
                 (progn
                   (assert (equal writer-result (if close-failure "Injected close failure" "Injected preparation failure")))
                   (assert (null (openldk::%initiated-class name loader)))
                   (assert (null (openldk::%get-ldk-class-by-bin-name name t loader))))
                 (assert (eq writer-result definition)))))
      (setf (fdefinition hook) original)
      (sb-thread:signal-semaphore resume)
      (dolist (thread (cons writer readers))
        (when (and thread (sb-thread:thread-alive-p thread))
          (sb-thread:terminate-thread thread))))
    ;; A failed preparation must not poison a subsequent definition.
    (when fail (assert (openldk::classload name)))
    (format t "~&Publication ~A: owner recursion, waiting readers, ~A passed~%"
            name (if fail "rollback and retry" "completed metadata"))))

(check-publication "PublicationSuccess" nil)
(check-publication "PublicationFailure" t)

(check-publication "PublicationMirrorSuccess" nil t)
(check-publication "PublicationMirrorFailure" t t)

(check-publication "PublicationCloseFailure" t nil t)
