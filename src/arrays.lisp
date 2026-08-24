;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: OPENLDK; Base: 10 -*-
;;;
;;; Copyright (C) 2025  Anthony Green <green@moxielogic.com>
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

(defstruct (java-array (:constructor make-java-array-default))
  (component-class nil :type t)
  (data #() :type vector))

(defun make-java-array (&key component-class (size 0) initial-element initial-contents)
  "Construct a Java-style array wrapper with COMPONENT-CLASS and DATA built from SIZE/INITIAL-ELEMENT or INITIAL-CONTENTS."
  (assert component-class)
  ;; Validate size for negativity (JVM spec: throw NegativeArraySizeException)
  (when (< size 0)
    (let ((exc (%make-java-instance "java/lang/NegativeArraySizeException")))
      (|<init>()| exc)
      (error (%lisp-condition exc))))
  (if initial-contents
      (let* ((contents-length (length initial-contents))
             (data (if (zerop contents-length)
                       (make-array size :initial-element initial-element)
                       (make-array contents-length :initial-contents initial-contents))))
        (make-java-array-default :component-class component-class :data data))
      (make-java-array-default
       :component-class component-class
       :data (make-array size :initial-element initial-element))))

(defun %array-component-class (array)
  "Return ARRAY's component class as a |java/lang/Class| object, healing
legacy values in place: bootstrap byte-array placeholders (created before
the byte class was registered), internal <CLASS> metaobjects, and
descriptor strings."
  (let ((cc (java-array-component-class array)))
    (if (typep cc '|java/lang/Class|)
        cc
        (let ((healed (cond
                        ((member cc '(:early-placeholder :early-byte-placeholder))
                         (%get-java-class-by-bin-name "byte" t))
                        ((typep cc '<class>)
                         (java-class cc))
                        ((stringp cc)
                         (%bin-type-name-to-class cc))
                        (t nil))))
          (cond
            (healed
             (setf (java-array-component-class array) healed)
             healed)
            (t cc))))))

(defun jaref (array index)
  "Java-style array access for ARRAY at INDEX."
  (when (null array)
    (error (%lisp-condition (%make-throwable '|java/lang/NullPointerException|))))
  (let* ((data (java-array-data array))
         (len (length data)))
    (when (or (< index 0) (>= index len))
      ;; Throw Java ArrayIndexOutOfBoundsException
      (let ((exc (%make-java-instance "java/lang/ArrayIndexOutOfBoundsException")))
        (|<init>(Ljava/lang/String;)| exc
         (jstring (format nil "~A" index)))
        (error (%lisp-condition exc))))
    (aref data index)))

(defun (setf jaref) (new-value array index)
  "Setter for JAVA array element at INDEX."
  ;; Null check (JVM spec: throw NullPointerException)
  (when (null array)
    (error (%lisp-condition (%make-throwable '|java/lang/NullPointerException|))))
  (let* ((data (java-array-data array))
         (len (length data)))
    ;; Bounds check
    (when (or (< index 0) (>= index len))
      (let ((exc (%make-java-instance "java/lang/ArrayIndexOutOfBoundsException")))
        (|<init>(Ljava/lang/String;)| exc
         (jstring (format nil "~A" index)))
        (error (%lisp-condition exc))))
    (setf (aref data index) new-value)))

(defun %array-store-compatible-p (component value)
  "Can VALUE legally be stored into a reference array whose component
Class is COMPONENT?  Permissive when the component's CLOS class is not
yet emitted — a missed check is preferable to a spurious
ArrayStoreException during bootstrap."
  (let ((component-name (lstring (slot-value component '|name|))))
    (cond
      ((string= component-name "java.lang.Object") t)
      ;; Array-of-arrays component: require an array value; finer
      ;; component covariance is enforced when the nested array is used.
      ((char= (char component-name 0) #\[)
       (typep value 'java-array))
      (t
       (let* ((bin-name (substitute #\/ #\. component-name))
              (clos-class (find-class (intern bin-name (class-package bin-name)) nil)))
         (or (null clos-class)
             (typep value clos-class)
             (%native-type-castable-p value bin-name)))))))

(defun %aastore (array index value)
  "Reference-array store implementing the aastore bytecode: performs
the JVM's covariance check, throwing ArrayStoreException when VALUE is
not assignable to ARRAY's component type."
  (when (and array value)
    (let ((component (%array-component-class array)))
      (when (and (typep component '|java/lang/Class|)
                 (not (%array-store-compatible-p component value)))
        (error (%lisp-condition (%make-throwable '|java/lang/ArrayStoreException|))))))
  (setf (jaref array index) value))

(defun java-array-length (array)
  "Return the logical length of the Java-style ARRAY."
  ;; Null check (JVM spec: throw NullPointerException)
  (when (null array)
    (error (%lisp-condition (%make-throwable '|java/lang/NullPointerException|))))
  ;; TODO: Add type check (throw IllegalArgumentException if not an array)
  ;; Deferred due to bootstrap complexity with exception classes
  (length (java-array-data array)))

(defun |java/lang/reflect/Array.getLength(Ljava/lang/Object;)| (obj)
  "java.lang.reflect.Array.getLength implementation."
  ;; Null check (JVM spec: throw NullPointerException)
  (when (null obj)
    (error (%lisp-condition (%make-throwable '|java/lang/NullPointerException|))))
  ;; TODO: Add type check (throw IllegalArgumentException if not an array)
  ;; Deferred due to bootstrap complexity with exception classes
  (length (java-array-data obj)))

(defun |java/lang/reflect/Array.get(Ljava/lang/Object;I)| (array index)
  "java.lang.reflect.Array.get implementation."
  (jaref array index))

(defun |java/lang/reflect/Array.set(Ljava/lang/Object;ILjava/lang/Object;)| (array index value)
  "java.lang.reflect.Array.set implementation."
  (setf (jaref array index) value))

(defmethod |length()| ((array java-array))
  (length (java-array-data array)))
