;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: OPENLDK; Base: 10 -*-
;;;
;;; Copyright (C) 2023, 2024, 2025  Anthony Green <green@moxielogic.com>
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

(defun |java/lang/Object.registerNatives()| ()
  ())

(defun |java/lang/ClassLoader.registerNatives()| ()
  ())

(defun |java/lang/System.registerNatives()| ()
  ())

(defun |java/lang/Class.registerNatives()| ()
  ())

(defun |java/lang/Class.desiredAssertionStatus0(Ljava/lang/Class;)| (class)
  nil)

(defun |java/lang/Class.getSecurityManager()| ()
  (print "java/lang/Class.getSecurityManager()")
  (classload "java/lang/SecurityManager")
  (eval (list 'make-instance (list 'quote '|java/lang/SecurityManager|))))

(defmethod |println(Ljava/lang/String;)| (stream string)
  (format t "~A~%" (slot-value string '|value|)))

(defmethod |println(I)| (stream number)
  (format t "~A~%" number))

(defmethod |println(Ljava/lang/Object;)| (stream object)
  (format t "~A~%" object))

(defmethod |println(Ljava/lang/Object;)| (stream (object (eql nil)))
  (format t "null~%"))

(defmethod |fillInStackTrace(I)| ((|this| |java/lang/Throwable|) dummy)
  (let ((bt (trivial-backtrace:print-backtrace nil :output nil)))
    (print bt)))

(defmethod |sun/reflect/Reflection.getCallerClass()| ()
  (let* ((caller-string (format nil "~A" (fourth (sb-debug:backtrace-as-list))))
         (cstring (subseq caller-string 1 (position #\. caller-string))))
    (java-class (gethash cstring *classes*))))

(defmethod |getClass()| (object)
  ;;; FIXME - throw nullpointerexception
	(when *debug-trace*
		(format t "tracing: java/lang/Object.getClass(~A): ~A~%" object (java-class (gethash (format nil "~A" (type-of object)) *classes*))))
	(java-class (gethash (format nil "~A" (type-of object)) *classes*)))

(defmethod |java/lang/Class.forName0(Ljava/lang/String;ZLjava/lang/ClassLoader;Ljava/lang/Class;)| (name initialize loader caller)
	(when *debug-trace*
		(format t "tracing: java/lang/Class.forName0(Ljava/lang/String;ZLjava/lang/ClassLoader;Ljava/lang/Class;)~%"))
  (let ((lname (substitute #\/ #\. (slot-value name '|value|))))
    (or (and (gethash lname *classes*)
             (java-class (gethash lname *classes*)))
        (progn (let ((klass (classload lname)))
                 (%clinit klass)
                 (java-class klass))))))

(defmethod |java/lang/System.currentTimeMillis()| ()
	;;; FIXME: this probably isn't right.
  (floor (* (get-internal-real-time) 1000)
				 internal-time-units-per-second))

(defmethod |java/lang/System.arraycopy(Ljava/lang/Object;ILjava/lang/Object;II)| (source_arr sourcePos dest_arr destPos len)
	(dotimes (i len)
		(setf (aref dest_arr (+ destPos i)) (aref source_arr (+ sourcePos i)))))

(defmethod |java/security/AccessController.doPrivileged(Ljava/security/PrivilegedAction;)| (action)
  (|run()| action))

(defmethod |java/lang/Class.getPrimitiveClass(Ljava/lang/String;)| (class-name)
  (let ((name (slot-value class-name '|value|)))
    (cond
      ((string= name "float")
       (let ((jc (java-class (gethash "java/lang/Float" *classes*))))
         jc))
      (t (error (format nil "getPrimitiveClass(~A)" name))))))

(defmethod |java/lang/Float.floatToRawIntBits(F)| (float)
  (float-features:single-float-bits float))

(defmethod |java/lang/Double.doubleToRawLongBits(D)| (double)
  (float-features:double-float-bits double))

(defmethod |java/lang/Double.longBitsToDouble(J)| (long-bits)
  (float-features:bits-double-float long-bits))

(defmethod |java/util/TimeZone.getSystemTimeZoneID(Ljava/lang/String;)| (arg)
  (jstring (local-time:format-timestring nil (local-time:now) :format '(:timezone))))

(defmethod |length()| ((str string))
  (length str))

(defmethod |java/util/TimeZone.getSystemGMTOffsetID()| ()
  (jstring (local-time:format-timestring nil (local-time:now) :format '(:gmt-offset))))


(defun %stringize-array (array)
  "Convert an array of characters and integers (ASCII values) into a string."
  (coerce
   (map 'list
        (lambda (x)
          (if (integerp x)
              (code-char x) ;; Convert integer to character
              x))           ;; Keep character as is
        array)
   'string))

(defmethod print-object ((str |java/lang/String|) out)
  (print-unreadable-object (str out :type t)
    (format out "~S" (%stringize-array (slot-value str '|value|)))))
