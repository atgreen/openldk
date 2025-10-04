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

(defclass/std <java-monitor> ()
  ((mutex :std (bordeaux-threads:make-lock))
   (condition-variable :std (bordeaux-threads:make-condition-variable))
   (owner)
   (recursion-count :std 0)
   (wait-set :std nil)))

(defvar *monitors* (make-hash-table))

(defun %get-monitor (object)
  "Return (and cache) the monitor object associated with OBJECT."
  ;; FIXME: This grows forever!!
  (or (gethash object *monitors*)
      (let ((monitor (make-instance '<java-monitor>)))
        (setf (gethash object *monitors*) monitor)
        monitor)))

(defun monitor-enter (object)
  "Enter the monitor for OBJECT, acquiring its mutex."
  (when object
    (let* ((monitor (%get-monitor object))
           (mutex (mutex monitor))
           (current-thread (bordeaux-threads:current-thread)))
    (bordeaux-threads:with-lock-held (mutex)
      (if (eq (owner monitor) current-thread)
          (incf (recursion-count monitor))
          (progn
            (loop while (owner monitor)
                  do (bordeaux-threads:condition-wait (condition-variable monitor) mutex))
            (setf (owner monitor) current-thread
                  (recursion-count monitor) 1)))))))

(defun monitor-exit (object)
  "Exit the monitor for OBJECT, releasing its mutex."
  (when object
    (let* ((monitor (%get-monitor object))
           (mutex (mutex monitor))
           (current-thread (bordeaux-threads:current-thread)))
      (bordeaux-threads:with-lock-held (mutex)
        (unless (eq (owner monitor) current-thread)
          (error "Current thread does not own the monitor"))
        (decf (recursion-count monitor))
        (when (zerop (recursion-count monitor))
          (setf (owner monitor) nil)
          (bordeaux-threads:condition-notify (condition-variable monitor)))))))
