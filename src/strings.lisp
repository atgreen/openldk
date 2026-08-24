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
        ;; Compact strings: value is byte[], coder is 0 (LATIN1) or 1 (UTF16)
        (let ((data (java-array-data value))
              (coder (or (ignore-errors (slot-value string '|coder|)) 0)))
          (if (zerop coder)
              ;; LATIN1: each byte is a char code
              (map 'string #'code-char data)
              ;; UTF16: pairs of bytes (little-endian), recombining
              ;; surrogate pairs into full codepoints
              (let* ((len (floor (length data) 2))
                     (result (make-array len :element-type 'character
                                             :adjustable t :fill-pointer 0))
                     (i 0))
                (flet ((unit (k) (+ (aref data (* k 2))
                                    (ash (aref data (1+ (* k 2))) 8))))
                  (loop while (< i len)
                        do (let ((u (unit i)))
                             (cond
                               ((and (<= #xD800 u #xDBFF)
                                     (< (1+ i) len)
                                     (<= #xDC00 (unit (1+ i)) #xDFFF))
                                (vector-push-extend
                                 (code-char (+ #x10000
                                               (ash (- u #xD800) 10)
                                               (- (unit (1+ i)) #xDC00)))
                                 result)
                                (incf i 2))
                               (t
                                (vector-push-extend (code-char u) result)
                                (incf i))))))
                (coerce result 'string))))))))

(defun %string-value-bytes (value)
  "Encode Lisp string VALUE per Java compact-string rules.  Returns
(values BYTES CODER): LATIN1 bytes with coder 0 when every character fits
in a byte, otherwise UTF-16LE code units (with surrogate pairs for
supplementary characters) with coder 1."
  (if (every (lambda (c) (< (char-code c) 256)) value)
      (let ((bytes (make-array (length value) :element-type '(unsigned-byte 8))))
        (dotimes (i (length value))
          (setf (aref bytes i) (char-code (char value i))))
        (values bytes 0))
      (let* ((units (loop for c across value
                          sum (if (> (char-code c) #xFFFF) 2 1)))
             (bytes (make-array (* 2 units) :element-type '(unsigned-byte 8)))
             (j 0))
        (flet ((put-unit (unit)
                 (setf (aref bytes j) (logand unit #xFF))
                 (setf (aref bytes (1+ j)) (ash unit -8))
                 (incf j 2)))
          (loop for c across value
                for code = (char-code c)
                do (if (> code #xFFFF)
                       (let ((v (- code #x10000)))
                         (put-unit (logior #xD800 (ash v -10)))
                         (put-unit (logior #xDC00 (logand v #x3FF))))
                       (put-unit code))))
        (values bytes 1))))

(defun jstring (value)
  "Construct a |java/lang/String| from a Lisp string VALUE."
  (let ((s (%make-java-instance "java/lang/String")))
    (multiple-value-bind (bytes coder) (%string-value-bytes value)
      (setf (slot-value s '|value|) (make-java-array :component-class
                                                      (or (%get-java-class-by-bin-name "byte" t) :early-placeholder)
                                                      :initial-contents bytes))
      (setf (slot-value s '|coder|) coder))
    (setf (slot-value s '|hash|) 0)
    s))

(defun ijstring (value)
  "Construct and intern a |java/lang/String| from Lisp string VALUE."
  (let ((s (%make-java-instance "java/lang/String")))
    (multiple-value-bind (bytes coder) (%string-value-bytes value)
      (setf (slot-value s '|value|) (make-java-array :component-class
                                                      (or (%get-java-class-by-bin-name "byte" t) :early-placeholder)
                                                      :initial-contents bytes))
      (setf (slot-value s '|coder|) coder))
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
