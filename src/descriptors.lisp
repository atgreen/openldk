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

(defun count-parameters (descriptor)
  "Count the number of parameters in a Java method descriptor."
  (let ((count 0)
        (index 0)
        (param-str (subseq descriptor
                           (1+ (position #\( descriptor))  ; lint:suppress
                           (position #\) descriptor))))    ; lint:suppress
    (loop while (< index (length param-str))
          do (let ((ch (char param-str index)))
               (cond
                 ;; Array types - skip all brackets, then handle component type
                 ((char= ch #\[)
                  (incf count)
                  ;; Skip all array dimension brackets
                  (loop while (and (< index (length param-str))
                                   (char= (char param-str index) #\[))
                        do (incf index))
                  ;; Handle the component type
                  (when (< index (length param-str))
                    (let ((component-ch (char param-str index)))
                      (cond
                        ;; Object array - skip to semicolon
                        ((char= component-ch #\L)
                         (setf index (1+ (position #\; param-str :start index))))
                        ;; Primitive array - skip the type char
                        (t (incf index))))))

                 ;; Object types - skip to semicolon
                 ((char= ch #\L)
                  (incf count)
                  (setf index (1+ (position #\; param-str :start index))))

                 ;; Primitive types - just skip one char
                 ((or (char= ch #\I) (char= ch #\J) (char= ch #\S) (char= ch #\B)
                      (char= ch #\C) (char= ch #\D) (char= ch #\F) (char= ch #\Z))
                  (incf count)
                  (incf index))

                 ;; Skip any other characters (shouldn't happen in valid descriptor)
                 (t (incf index)))))
    count))

(defun get-stack-type-from-descriptor (descriptor)
  (cond
    ((string= descriptor "I")
     :INTEGER)
    ((string= descriptor "J")
     :LONG)
    ((string= descriptor "F")
     :FLOAT)
    ((string= descriptor "D")
     :DOUBLE)
    ((string= descriptor "S")
     :INTEGER)
    ((string= descriptor "B")
     :INTEGER)
    ((string= descriptor "C")
     :INTEGER)
    ((string= descriptor "Z")
     :INTEGER)
    (t :REFERENCE)))

(defun get-return-type (name)
  (let ((return-type-descriptor
          (subseq name (1+ (position #\) name)))))
    (cdr (assoc (char return-type-descriptor 0)
                '((#\I . :INTEGER)
                  (#\J . :LONG)
                  (#\S . :SHORT)
                  (#\B . :BYTE)
                  (#\C . :CHAR)
                  (#\D . :DOUBLE)
                  (#\F . :FLOAT)
                  (#\L . :REFERENCE)
                  (#\Z . :BOOLEAN)
                  (#\V . :VOID)
                  (#\[ . :ARRAY))))))

(defun ends-in-V (name)
  (let ((len (length name)))
    (and (not (zerop len))
         (char= (char name (1- len))
                #\V))))

(defun gen-parameter-hints (mdescriptor)
  "Parse the Java method descriptor and return a list representing
 parameter types. Longs are #\J. Doubles are #\D. Everything else is T."
  (let ((param-hints nil)
        (index 0)
        (descriptor (subseq mdescriptor
                            (1+ (position #\( mdescriptor))  ; lint:suppress
                            (position #\) mdescriptor))))
    (loop
      while (< index (length descriptor))
      do (let ((ch (char descriptor index)))
           (cond
             ;; Simple types
             ((char= ch #\J) (push #\J param-hints) (incf index))
             ((char= ch #\D) (push #\D param-hints) (incf index))

             ;; Object types
             ((char= ch #\L)
              (let ((obj-end (position #\; descriptor :start index)))
                (if obj-end
                    (progn (push t param-hints)
                           (setf index (1+ obj-end)))
                    (error "Malformed descriptor: Missing ';' in object type."))))

             ;; Array types
             ((char= ch #\[)
              (loop until (not (char= (char descriptor index) #\[)) do (incf index))
              (let ((ch (char descriptor index)))
                (cond
                  ((char= ch #\L)
                   (let ((obj-end (position #\; descriptor :start index)))
                     (if obj-end
                         (progn (push t param-hints)
                                (setf index (1+ obj-end)))
                         (error "Malformed descriptor: Missing ';' in object type."))))
                  (t
                   (push t param-hints)
                   (incf index)))))

             ;; Default case
             (t
              (push t param-hints)
              (incf index)))))
    (nreverse param-hints)))

(defun parse-parameter-types (descriptor)
  "Parse the Java method descriptor and return a list of parameter types
as strings."
  (let ((param-list nil)
        (index 0)
        (descriptor (subseq descriptor (position #\( descriptor) (position #\) descriptor))))  ; lint:suppress
    (loop while (< index (length descriptor))
          do (let ((ch (char descriptor index)))
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
                  (let ((dimensions 0))
                    ;; Count array dimensions
                    (loop while (and (< index (length descriptor))
                                     (char= (char descriptor index) #\[))
                          do (incf dimensions)
                             (incf index))
                    ;; Determine component type
                    (let* ((component-ch (char descriptor index))
                           (component-type
                             (cond
                               ;; Object type
                               ((char= component-ch #\L)
                                (let ((obj-end (position #\; descriptor :start index)))
                                  (prog1
                                      (subseq descriptor (1+ index) obj-end)
                                    (setf index (1+ obj-end)))))
                               ;; Primitive type
                               (t
                                (prog1
                                    (translate-type component-ch)
                                  (incf index))))))
                      ;; Build type string with correct number of [] suffixes
                      (let ((type-str component-type))
                        (dotimes (i dimensions)
                          (setf type-str (concatenate 'string type-str "[]")))
                        (push type-str param-list)))))

                 (t (incf index)))))
    (nreverse param-list))) ; Reverse the list before returning it, since we used push

(defun %bin-type-name-to-class (bin-type-name)
  (let ((ch (char bin-type-name 0)))
    (cond
      ((char= ch #\I) (|java/lang/Class.forName0(Ljava/lang/String;ZLjava/lang/ClassLoader;Ljava/lang/Class;)| (jstring "int") nil nil nil))
      ((char= ch #\J) (|java/lang/Class.forName0(Ljava/lang/String;ZLjava/lang/ClassLoader;Ljava/lang/Class;)| (jstring "long") nil nil nil))
      ((char= ch #\S) (|java/lang/Class.forName0(Ljava/lang/String;ZLjava/lang/ClassLoader;Ljava/lang/Class;)| (jstring "short") nil nil nil))
      ((char= ch #\B) (|java/lang/Class.forName0(Ljava/lang/String;ZLjava/lang/ClassLoader;Ljava/lang/Class;)| (jstring "byte") nil nil nil))
      ((char= ch #\C) (|java/lang/Class.forName0(Ljava/lang/String;ZLjava/lang/ClassLoader;Ljava/lang/Class;)| (jstring "char") nil nil nil))
      ((char= ch #\D) (|java/lang/Class.forName0(Ljava/lang/String;ZLjava/lang/ClassLoader;Ljava/lang/Class;)| (jstring "double") nil nil nil))
      ((char= ch #\F) (|java/lang/Class.forName0(Ljava/lang/String;ZLjava/lang/ClassLoader;Ljava/lang/Class;)| (jstring "float") nil nil nil))
      ((char= ch #\Z) (|java/lang/Class.forName0(Ljava/lang/String;ZLjava/lang/ClassLoader;Ljava/lang/Class;)| (jstring "boolean") nil nil nil))
      ((char= ch #\V) (|java/lang/Class.forName0(Ljava/lang/String;ZLjava/lang/ClassLoader;Ljava/lang/Class;)| (jstring "void") nil nil nil))

      ;; For object types
      ((char= ch #\L)
       (progn
         (let* ((obj-end (position #\; bin-type-name))
                (res (|java/lang/Class.forName0(Ljava/lang/String;ZLjava/lang/ClassLoader;Ljava/lang/Class;)| (jstring (subseq bin-type-name 1 obj-end)) nil nil nil)))
           res)))

      ((char= ch #\[)
       (let ((res (|java/lang/Class.forName0(Ljava/lang/String;ZLjava/lang/ClassLoader;Ljava/lang/Class;)|
                   (jstring bin-type-name) nil nil nil)))
         res)))))

(defun %get-return-type (descriptor)
  (let* ((return-type-descriptor
           (subseq descriptor (1+ (position #\) descriptor)))))
    (%bin-type-name-to-class return-type-descriptor)))

(defun %get-parameter-types (descriptor)
  "Parse the Java method descriptor and return a list of parameter types
as strings."
  (let ((param-list nil)
        (index 0)
        (descriptor (subseq descriptor
                            (position #\( descriptor)  ; lint:suppress
                            (position #\) descriptor))))
    (loop while (< index (length descriptor))
          do (let ((ch (char descriptor index)))
               (cond
                 ;; For simple types
                 ((char= ch #\I)
                  (push (|java/lang/Class.forName0(Ljava/lang/String;ZLjava/lang/ClassLoader;Ljava/lang/Class;)|
                         (jstring "int") nil nil nil)
                        param-list)
                  (incf index))
                 ((char= ch #\J)
                  (push (|java/lang/Class.forName0(Ljava/lang/String;ZLjava/lang/ClassLoader;Ljava/lang/Class;)|
                         (jstring "long") nil nil nil)
                        param-list)
                  (incf index))
                 ((char= ch #\S)
                  (push (|java/lang/Class.forName0(Ljava/lang/String;ZLjava/lang/ClassLoader;Ljava/lang/Class;)|
                         (jstring "short") nil nil nil)
                        param-list)
                  (incf index))
                 ((char= ch #\B)
                  (push (|java/lang/Class.forName0(Ljava/lang/String;ZLjava/lang/ClassLoader;Ljava/lang/Class;)|
                         (jstring "byte") nil nil nil)
                        param-list)
                  (incf index))
                 ((char= ch #\C)
                  (push (|java/lang/Class.forName0(Ljava/lang/String;ZLjava/lang/ClassLoader;Ljava/lang/Class;)|
                         (jstring "char") nil nil nil)
                        param-list)
                  (incf index))
                 ((char= ch #\D)
                  (push (|java/lang/Class.forName0(Ljava/lang/String;ZLjava/lang/ClassLoader;Ljava/lang/Class;)|
                         (jstring "double") nil nil nil)
                        param-list)
                  (incf index))
                 ((char= ch #\F)
                  (push (|java/lang/Class.forName0(Ljava/lang/String;ZLjava/lang/ClassLoader;Ljava/lang/Class;)|
                         (jstring "float") nil nil nil)
                        param-list)
                  (incf index))
                 ((char= ch #\Z)
                  (push (|java/lang/Class.forName0(Ljava/lang/String;ZLjava/lang/ClassLoader;Ljava/lang/Class;)|
                         (jstring "boolean") nil nil nil)
                        param-list)
                  (incf index))

                 ;; For object types
                 ((char= ch #\L)
                  (let ((obj-end (position #\; descriptor :start index)))
                    (push (|java/lang/Class.forName0(Ljava/lang/String;ZLjava/lang/ClassLoader;Ljava/lang/Class;)|
                           (jstring (subseq descriptor (1+ index) obj-end))
                           nil nil nil)
                          param-list)
                    (setf index (1+ obj-end))))

                 ;; For array types
                 ((char= ch #\[)
                  (let ((start index))
                    ;; Skip array dimension markers only
                    (loop while (and (< index (length descriptor))
                                     (char= (char descriptor index) #\[))
                          do (incf index))
                    ;; Now consume exactly one component type
                    (when (< index (length descriptor))
                      (let ((component-ch (char descriptor index)))
                        (cond
                          ;; Primitive component type - consume just one character
                          ((member component-ch '(#\I #\J #\S #\B #\C #\D #\F #\Z))
                           (incf index))
                          ;; Object component type - consume through semicolon
                          ((char= component-ch #\L)
                           (let ((semi (position #\; descriptor :start index)))
                             (when semi
                               (setf index (1+ semi)))))
                          ;; Unknown - skip one character
                          (t (incf index)))))
                    (push (|java/lang/Class.forName0(Ljava/lang/String;ZLjava/lang/ClassLoader;Ljava/lang/Class;)|
                           (jstring (substitute #\. #\/ (subseq descriptor start index))) nil nil nil)
                          param-list)))

                 (t (incf index)))))
    (coerce (nreverse param-list) 'vector))) ; Reverse the list before returning it, since we used push

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
