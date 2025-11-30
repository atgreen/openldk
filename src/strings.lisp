;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: OPENLDK; Base: 10 -*-
;;;
;;; Copyright (C) 2024, 2025  Anthony Green <green@moxielogic.com>
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

(defvar *interned-string-table* (make-hash-table :test #'equal))

(defun lstring (string)
  "Extract a Lisp string from a |java/lang/String| object."
  (when string
    (let ((value (slot-value string '|value|)))
      (when value
        (coerce (java-array-data value) 'string)))))

(defun jstring (value)
  "Construct a |java/lang/String| from a Lisp string VALUE."
  (let ((s (%make-java-instance "java/lang/String")))
    (setf (slot-value s '|value|) (make-java-array :component-class
                                                   (or (%get-java-class-by-bin-name "char" t) :early-placeholder)
                                                   :initial-contents value))
    (setf (slot-value s '|hash|) 0)
    s))

(defun ijstring (value)
  "Construct and intern a |java/lang/String| from Lisp string VALUE."
  (let ((s (%make-java-instance "java/lang/String")))
    (setf (slot-value s '|value|) (make-java-array :component-class
                                                   (or (%get-java-class-by-bin-name "char" t) :early-placeholder)
                                                   :initial-contents value))
    (setf (slot-value s '|hash|) 0)
    (|intern()| s)))

(defmethod |intern()| ((str |java/lang/String|))
  (let ((lisp-string (lstring str)))
    (let ((istr (gethash lisp-string *interned-string-table*)))
      (or istr
          (let ((istr (setf (gethash lisp-string *interned-string-table*) str)))
            istr)))))

(defmethod |intern()| ((str string))
  (let ((istr (gethash str *interned-string-table*)))
    (or istr
        (let ((istr (setf (gethash str *interned-string-table*) (jstring str))))
          istr))))

(defmethod |toString()| (str)
  str)
