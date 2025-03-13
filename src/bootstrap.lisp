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

(defclass/std |java/lang/Object| ()
  ())

(defclass/std |java/lang/Cloneable| ()
  ())

(defmethod |clone()| ((|this| |java/lang/Object|))
  "Default implementation of clone for CLONEABLE objects."
  (assert (typep |this| '|java/lang/Cloneable|))
  (let ((copy (allocate-instance (class-of |this|))))
    (dolist (slot (mapcar #'sb-mop:slot-definition-name
                          (sb-mop:class-slots (class-of |this|))))
      (when (slot-boundp |this| slot)
        (setf (slot-value copy slot) (slot-value |this| slot))))
    copy))

(defclass |java/lang/String| (|java/lang/Object|)
  ((|value| :initform NIL :allocation :instance)
   (|hash| :initform NIL :allocation :instance)
   (|serialVersionUID| :initform NIL :allocation :class)
   (|serialPersistentFields| :initform NIL :allocation :class)
   (CASE_INSENSITIVE_ORDER :initform NIL :allocation :class)))

(defclass |java/lang/Class| (|java/lang/Object|)
  ((ANNOTATION :initform 0 :allocation :class)
   (ENUM :initform 0 :allocation :class)
   (SYNTHETIC :initform 0 :allocation :class)
   (|cachedConstructor| :initform nil :allocation :instance)
   (|newInstanceCallerCache| :initform nil :allocation :instance)
   (|name| :initform nil :allocation :instance)
   (|classLoader| :initform nil :allocation :instance)
   (|allPermDomain| :initform nil :allocation :class)
   (|useCaches| :initform nil :allocation :class)
   (|reflectionData| :initform nil :allocation :instance)
   (|classRedefinedCount| :initform 0 :allocation :instance)
   (|genericInfo| :initform nil :allocation :instance)
   (|serialVersionUID| :initform 0 :allocation :class)
   (|serialPersistentFields| :initform nil :allocation :class)
   (|reflectionFactory| :initform nil :allocation :class)
   (|initted| :initform nil :allocation :class)
   (|enumConstants| :initform nil :allocation :instance)
   (|enumConstantDirectory| :initform nil :allocation :instance)
   (|annotationData| :initform nil :allocation :instance)
   (|annotationType| :initform nil :allocation :instance)
   (|classValueMap| :initform nil :allocation :instance)))

(defmethod print-object ((s |java/lang/String|) out)
  (print-unreadable-object (s out :type t)
    (format out "~S" (slot-value s '|value|))))

(defclass |java/io/UnixFileSystem| (|java/lang/Object|)
  ())

(defclass |java/lang/Runtime| (|java/lang/Object|)
  ())

(defclass |sun/misc/Unsafe| (|java/lang/Object|)
  ())

(defclass |java/lang/ClassLoader| (|java/lang/Object|)
  ())

(defclass |java/io/InputStream| (|java/lang/Object|)
  ())

(defclass |java/io/FileInputStream| (|java/io/InputStream|)
  ())

(defclass |java/io/OutputStream| (|java/lang/Object|)
  ())

(defclass |java/io/FileOutputStream| (|java/io/OutputStream|)
  ())

(defclass |java/io/RandomAccessFile| (|java/lang/Object|)
  ())

(defclass |java/lang/Thread| (|java/lang/Object|)
  ())

(defclass |java/lang/Throwable| (|java/lang/Object|)
  ())

(defclass |sun/reflect/ConstantPool| (|java/lang/Object|)
  ())

(defmethod %lisp-condition ((throwable |java/lang/Throwable|))
  (error (format nil "Missing %lisp-condition implementation for ~A." throwable)))

(define-condition |condition-java/lang/Throwable| (error)
  ((|objref|)))

(define-condition |condition-java/lang/Exception|
    (|condition-java/lang/Throwable|)
  ())

(defclass |java/lang/Throwable| (|java/lang/Object|)
  ())

(defclass |java/lang/Exception| (|java/lang/Throwable|)
  ())

(defclass |java/lang/ArithmeticException| (|java/lang/RuntimeException|)
  ())
