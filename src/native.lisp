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

(defun |java/lang/Object.registerNatives()V| ()
  ())

(defun |java/lang/ClassLoader.registerNatives()V| ()
  ())

(defun |java/lang/System.registerNatives()V| ()
  ())

(defun |java/lang/Class.registerNatives()V| ()
  ())

(defun |java/lang/Class.desiredAssertionStatus0(Ljava/lang/Class;)Z| (class)
  (print "desiredAssertionStatus0")
  (print class)
  nil)

(defun |java/lang/Class.getSecurityManager()Ljava/lang/SecurityManager;| ()
  (print "java/lang/Class.getSecurityManager()Ljava/lang/SecurityManager;")
  (classload "java/lang/SecurityManager")
  (eval (list 'make-instance (list 'quote '|java/lang/SecurityManager|))))

(defmethod |println(Ljava/lang/String;)V| (stream string)
  (format t "~A~%" (slot-value string '|value|)))

(defmethod |println(I)V| (stream number)
  (format t "~A~%" number))

(defmethod |println(Ljava/lang/Object;)V| (stream object)
  (format t "~A~%" object))

(defmethod |fillInStackTrace(I)Ljava/lang/Throwable;| ((|this| |java/lang/Throwable|) dummy)
  (let ((bt (trivial-backtrace:print-backtrace nil :output nil)))
    (print bt)))

(defmethod |sun/reflect/Reflection.getCallerClass()Ljava/lang/Class;| ()
  (let* ((caller-string (format nil "~A" (third (sb-debug:backtrace-as-list))))
         (cstring (subseq caller-string 1 (position #\. caller-string))))
    (gethash cstring *java-classes*)))

(defmethod |java/lang/Class.forName0(Ljava/lang/String;ZLjava/lang/ClassLoader;Ljava/lang/Class;)Ljava/lang/Class;| (name initialize loader caller)
  (let ((lname (substitute #\/ #\. (slot-value name '|value|))))
    (or (gethash lname *java-classes*)
        (progn (%clinit (classload lname))
               (let ((java-class (make-instance '|java/lang/Class|)))
                 (setf (slot-value java-class '|name|)
                       (let ((s (make-instance '|java/lang/String|)))
                         (setf (slot-value s '|value|) name)))
                 (setf (slot-value java-class '|classLoader|) loader)
                 (setf (gethash lname *java-classes*) java-class)
                 java-class)))))
