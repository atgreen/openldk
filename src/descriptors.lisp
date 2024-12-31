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

(defun count-parameters (descriptor)
  "Count the number of parameters in a Java method descriptor."
  (let ((reading-complex-p nil)
        (count 0))
    (loop for ch across (subseq descriptor (position #\( descriptor) (position #\) descriptor))
          do (cond
               ((or (char= ch #\I) (char= ch #\J) (char= ch #\S) (char= ch #\B)
                    (char= ch #\C) (char= ch #\D) (char= ch #\F) (char= ch #\Z))
                (unless reading-complex-p (incf count)))
               ((char= ch #\L)
                (setf reading-complex-p t))
               ((char= ch #\;)
                (incf count)
                (setf reading-complex-p nil))
               (t nil)))
    count))

(defun void-return-p (name)
  (let ((len (length name)))
    (and (not (zerop len))
	 (char= (char name (1- len))
		#\V))))

(defun parse-parameter-types (descriptor)
  "Parse the Java method descriptor and return a list of parameter types as strings."
  (let ((param-list nil)
        (index 0)
        (descriptor (subseq descriptor (position #\( descriptor) (position #\) descriptor))))
    (loop while (< index (length descriptor))
          do (let ((ch (char descriptor index)))
               (print index)
               (print ch)
               (cond
                 ;; For simple types
                 ((char= ch #\I) (push "int" param-list) (incf index))
                 ((char= ch #\J) (push "long" param-list) (incf index))
                 ((char= ch #\S) (push "short" param-list) (incf index))
                 ((char= ch #\B) (push "byte" param-list) (incf index))
                 ((char= ch #\C) (push "char" param-list) (incf index))
                 ((char= ch #\D) (push "double" param-list) (incf index))
                 ((char= ch #\F) (push "float" param-list) (incf index))
                 ((char= ch #\Z) (push "boolean" param-list) (incf index))

                 ;; For object types
                 ((char= ch #\L)
                  (let ((obj-end (position #\; descriptor :start index)))
                    (push (subseq descriptor (1+ index) obj-end) param-list)
                    (setf index (1+ obj-end))))

                 ;; For array types
                 ((char= ch #\[)
                  (incf index)
                  (loop until (not (member (char descriptor index) '(#\I #\J #\S #\B #\C #\D #\F #\Z #\[)))
                        do (incf index))
                  (when (char= (char descriptor index) #\L)
                    (setf index (position #\; descriptor :start index)))
                  (push (format nil "~a[]" (translate-type (char descriptor index))) param-list)
                  (incf index))

                 (t (incf index)))))
    (nreverse param-list))) ; Reverse the list before returning it, since we used push

(defun translate-type (type-char)
  (case type-char
    (#\I "int")
    (#\J "long")
    (#\S "short")
    (#\B "byte")
    (#\C "char")
    (#\D "double")
    (#\F "float")
    (#\Z "boolean")
    (t (string type-char))))

; (parse-parameter-types "(IS[Ljava/lang/String;)V")
