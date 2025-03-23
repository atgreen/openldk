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

(defstruct (java-array (:constructor make-java-array-default))
  (class nil :type t)
  (data #() :type vector))

(defun make-java-array (&key class (size 0) initial-element initial-contents)
  (cond
    ;; If initial-contents is provided, use it
    (initial-contents
     (let ((contents-length (length initial-contents)))
       (make-java-array-default
        :class class
        :data (if (zerop contents-length)
                  (make-array size :initial-element (or initial-element nil))
                  (make-array contents-length :initial-contents initial-contents)))))
    ;; Otherwise use size and initial-element
    (t
     (make-java-array-default
      :class class
      :data (make-array size :initial-element (or initial-element nil))))))

(defun jaref (array index)
  (aref (java-array-data array) index))

(defun (setf jaref) (new-value array index)
  (setf (aref (java-array-data array) index) new-value))

(defun java-array-length (array)
  (length (java-array-data array)))

(defun |java/lang/reflect/Array.getLength(Ljava/lang/Object;)| (obj)
  (length (java-array-data obj)))
