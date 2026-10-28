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

;;; Primitive arrays are backed by specialized Lisp vectors -- int[] by
;;; (SIMPLE-ARRAY (SIGNED-BYTE 32) (*)), char[] by (UNSIGNED-BYTE 16) code
;;; units, and so on -- so JIT'd code can access them with an inline,
;;; unboxed AREF.  byte[] and boolean[] share (SIGNED-BYTE 8) because the
;;; JVM's baload/bastore serve both.  Reference arrays are SIMPLE-VECTORs.

(defstruct (java-array (:constructor make-java-array-default))
  (component-class nil :type t)
  (data #() :type vector)
  ;; What %AASTORE's covariance check needs to know of the component
  ;; class, computed at the first store: NIL until then, T when any value
  ;; may be stored, a CLOS class any instance of which may be, or :SLOW.
  (store-check nil :type t))

(defun %primitive-element-type (name)
  "Lisp element type for an array whose component is the primitive type
NAME (\"int\", \"char\", ...), or T for anything else."
  (cond
    ((string= name "int") '(signed-byte 32))
    ((or (string= name "byte") (string= name "boolean")) '(signed-byte 8))
    ((string= name "char") '(unsigned-byte 16))
    ((string= name "long") '(signed-byte 64))
    ((string= name "double") 'double-float)
    ((string= name "short") '(signed-byte 16))
    ((string= name "float") 'single-float)
    (t t)))

(defun %descriptor-element-type (descriptor)
  "Lisp element type for an array whose component is the field DESCRIPTOR."
  (if (= (length descriptor) 1)
      (case (char descriptor 0)
        (#\I '(signed-byte 32))
        ((#\B #\Z) '(signed-byte 8))
        (#\C '(unsigned-byte 16))
        (#\J '(signed-byte 64))
        (#\D 'double-float)
        (#\S '(signed-byte 16))
        (#\F 'single-float)
        (t t))
      t))

(defvar *component-element-types*
  (make-hash-table :test #'eq :weakness :key :synchronized t)
  "Cache of component Class -> Lisp element type for its arrays.")

(defun %component-element-type (component-class)
  "Lisp element type for arrays whose component is COMPONENT-CLASS, in any
of the forms a java-array's component-class slot can take."
  (cond
    ((member component-class '(:early-placeholder :early-byte-placeholder))
     '(signed-byte 8))
    ((stringp component-class) (%descriptor-element-type component-class))
    ((typep component-class '<class>) (%primitive-element-type (name component-class)))
    ((null component-class) t)
    (t
     (or (gethash component-class *component-element-types*)
         (let ((name (and (slot-exists-p component-class '|name|)
                          (slot-boundp component-class '|name|)
                          (slot-value component-class '|name|))))
           ;; A Class still being bootstrapped has no name yet: don't cache.
           (if name
               (setf (gethash component-class *component-element-types*)
                     (%primitive-element-type (lstring name)))
               t))))))

(defun %array-value (element-type value)
  "Normalize VALUE for storage in an array of ELEMENT-TYPE: characters
become UTF-16 code units, booleans 0/1, and integers wrap to the
element width as Java's narrowing stores do."
  (if (eq element-type t)
      value
      (let ((value (typecase value
                     (character (char-code value))
                     (null 0)
                     ((eql t) 1)
                     (t value))))
        (cond
          ((eq element-type 'single-float) (float value 1f0))
          ((eq element-type 'double-float) (float value 1d0))
          ((equal element-type '(unsigned-byte 16)) (ldb (byte 16 0) value))
          (t (sb-c::mask-signed-field (second element-type) value))))))

(defun %char-data-string (data)
  "Lisp string of the UTF-16 code units (or characters) in char[] DATA."
  (map 'string (lambda (c) (if (characterp c) c (code-char c))) data))

(defun make-java-array (&key component-class (size 0) initial-element initial-contents
                          (element-type (%component-element-type component-class)))
  "Construct a Java-style array wrapper with COMPONENT-CLASS and DATA built
from SIZE/INITIAL-ELEMENT or INITIAL-CONTENTS.  ELEMENT-TYPE, the Lisp
element type of DATA, defaults from COMPONENT-CLASS."
  (assert component-class)
  ;; Validate size for negativity (JVM spec: throw NegativeArraySizeException)
  (when (< size 0)
    (let ((exc (%make-java-instance "java/lang/NegativeArraySizeException")))
      (|<init>()V| exc)
      (error (%lisp-condition exc))))
  (let ((initial-element (if (eq element-type t)
                             initial-element
                             (%array-value element-type (or initial-element 0)))))
    (make-java-array-default
     :component-class component-class
     :data (cond
             ((or (null initial-contents) (zerop (length initial-contents)))
              (make-array size :element-type element-type :initial-element initial-element))
             ((eq element-type t)
              (make-array (length initial-contents) :initial-contents initial-contents))
             ((typep initial-contents `(simple-array ,element-type (*)))
              (copy-seq initial-contents))
             (t
              (map-into (make-array (length initial-contents) :element-type element-type)
                        (lambda (v) (%array-value element-type v))
                        initial-contents))))))

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

(defun %throw-array-index-error (index)
  "Throw ArrayIndexOutOfBoundsException for INDEX."
  (let ((exc (%make-java-instance "java/lang/ArrayIndexOutOfBoundsException")))
    (|<init>(Ljava/lang/String;)V| exc
     (jstring (format nil "~A" index)))
    (error (%lisp-condition exc))))

(defun jaref (array index)
  "Java-style array access for ARRAY at INDEX."
  (when (null array)
    (error (%lisp-condition (%make-throwable '|java/lang/NullPointerException|))))
  (let ((data (java-array-data array)))
    (unless (< -1 index (length data))
      (%throw-array-index-error index))
    (aref data index)))

(defun (setf jaref) (new-value array index)
  "Setter for JAVA array element at INDEX."
  ;; Null check (JVM spec: throw NullPointerException)
  (when (null array)
    (error (%lisp-condition (%make-throwable '|java/lang/NullPointerException|))))
  (let ((data (java-array-data array)))
    (unless (< -1 index (length data))
      (%throw-array-index-error index))
    (setf (aref data index)
          (if (simple-vector-p data)
              new-value
              (%array-value (array-element-type data) new-value)))))

(defun %array-type-name-for-component (component-name)
  "Class.getName()-style name of an array type whose component Class is
named COMPONENT-NAME (dotted form, e.g. \"java.lang.String\", \"int\",
or \"[I\")."
  (cond
    ((char= (char component-name 0) #\[)
     (concatenate 'string "[" component-name))
    ((let ((code (cdr (assoc component-name
                             '(("byte" . "B") ("char" . "C") ("int" . "I")
                               ("short" . "S") ("long" . "J") ("double" . "D")
                               ("float" . "F") ("boolean" . "Z"))
                             :test #'string=))))
       (when code (concatenate 'string "[" code))))
    (t (concatenate 'string "[L" component-name ";"))))

(defun %array-store-compatible-p (component value)
  "Can VALUE legally be stored into a reference array whose component
Class is COMPONENT?  Permissive when the component's CLOS class is not
yet emitted — a missed check is preferable to a spurious
ArrayStoreException during bootstrap."
  (let ((component-name (lstring (slot-value component '|name|))))
    (cond
      ((string= component-name "java.lang.Object") t)
      ;; Array-of-arrays component: the value must be an array whose
      ;; type is assignable to the component array type.
      ((char= (char component-name 0) #\[)
       (and (typep value 'java-array)
            (let ((value-component (%array-component-class value)))
              (or (not (typep value-component '|java/lang/Class|))
                  (%array-type-name-assignable-p
                   component-name
                   (%array-type-name-for-component
                    (lstring (slot-value value-component '|name|))))))))
      (t
       (let* ((bin-name (substitute #\/ #\. component-name))
              (clos-class (find-class (intern bin-name (%java-class-package component)) nil)))
         (or (null clos-class)
             (typep value clos-class)
             (%native-type-castable-p value bin-name)))))))

(defun %array-store-check (component)
  "The STORE-CHECK of an array whose component Class is COMPONENT."
  (if (typep component '|java/lang/Class|)
      (let ((component-name (lstring (slot-value component '|name|))))
        (cond
          ((string= component-name "java.lang.Object") t)
          ((char= (char component-name 0) #\[) :slow)
          (t
           (let* ((bin-name (substitute #\/ #\. component-name))
                  (clos-class (find-class (intern bin-name (%java-class-package component)) nil)))
             ;; A class not yet emitted may be later: check in full each time.
             (or clos-class :slow)))))
      t))

(defun %aastore (array index value)
  "Reference-array store implementing the aastore bytecode: performs
the JVM's covariance check, throwing ArrayStoreException when VALUE is
not assignable to ARRAY's component type."
  (when (and array value)
    (let ((check (or (java-array-store-check array)
                     (setf (java-array-store-check array)
                           (%array-store-check (%array-component-class array))))))
      (unless (or (eq check t)
                  (and (not (eq check :slow)) (typep value check))
                  (%array-store-compatible-p (%array-component-class array) value))
        (error (%lisp-condition (%make-throwable '|java/lang/ArrayStoreException|))))))
  (setf (jaref array index) value))

(defun %array-length-slow (arrayref)
  "arraylength of something other than a java-array: NIL throws
NullPointerException; a Lisp string serves as a char[]."
  (if arrayref
      (length arrayref)
      (error (%lisp-condition (%make-throwable '|java/lang/NullPointerException|)))))

(defun java-array-length (array)
  "Return the logical length of the Java-style ARRAY."
  ;; Null check (JVM spec: throw NullPointerException)
  (when (null array)
    (error (%lisp-condition (%make-throwable '|java/lang/NullPointerException|))))
  ;; TODO: Add type check (throw IllegalArgumentException if not an array)
  ;; Deferred due to bootstrap complexity with exception classes
  (length (java-array-data array)))

(defun %check-reflective-array (array)
  "Validate the Object argument to java.lang.reflect.Array operations."
  (unless array (%throw '|java/lang/NullPointerException|))
  (unless (java-array-p array)
    (%throw '|java/lang/IllegalArgumentException| "Argument is not an array")))

(defun %array-widening-p (source target)
  "Whether reflection permits the primitive conversion SOURCE -> TARGET."
  (or (eql source target)
      (find target
            (case source
              (#\B "SIJFD") (#\S "IJFD") (#\C "IJFD")
              (#\I "JFD") (#\J "FD") (#\F "D")))))

(defun %reflective-array-primitive-type (array)
  (%check-reflective-array array)
  (%field-primitive-char (%array-component-class array)))

(defun %reflective-array-get (array index target)
  "Read a primitive array using reflection's widening conversion rules."
  (let* ((source (%reflective-array-primitive-type array))
         (value (jaref array index)))
    (unless (and source (%array-widening-p source target))
      (%throw '|java/lang/IllegalArgumentException| "Invalid array element conversion"))
    (let ((number (if (characterp value) (char-code value) value)))
      (case target
        (#\F (float number 1f0)) (#\D (float number 1d0))
        (t value)))))

(defun %reflective-array-set (array index value source)
  "Store a primitive value without allowing reflection to narrow its type."
  (let ((target (%reflective-array-primitive-type array)))
    ;; Bounds checks precede the primitive conversion check, as on the JDK.
    (jaref array index)
    (unless (and target (%array-widening-p source target))
      (%throw '|java/lang/IllegalArgumentException| "Invalid array element conversion"))
    (setf (jaref array index) value))
  nil)

(dolist (entry '(("Boolean" #\Z) ("Byte" #\B) ("Char" #\C) ("Short" #\S)
                 ("Int" #\I) ("Long" #\J) ("Float" #\F) ("Double" #\D)))
  (destructuring-bind (name type) entry
    (eval `(defun ,(intern (format nil "java/lang/reflect/Array.get~A(Ljava/lang/Object;I)~A" name type))
               (array index)
             (%reflective-array-get array index ,type)))
    (eval `(defun ,(intern (format nil "java/lang/reflect/Array.set~A(Ljava/lang/Object;I~A)V" name type))
               (array index value)
             (%reflective-array-set array index value ,type)))))

(defun |java/lang/reflect/Array.getLength(Ljava/lang/Object;)I| (obj)
  "java.lang.reflect.Array.getLength implementation."
  (%check-reflective-array obj)
  (length (java-array-data obj)))

(defun |java/lang/reflect/Array.get(Ljava/lang/Object;I)Ljava/lang/Object;| (array index)
  "java.lang.reflect.Array.get implementation."
  (jaref array index))

(defun |java/lang/reflect/Array.set(Ljava/lang/Object;ILjava/lang/Object;)V| (array index value)
  "java.lang.reflect.Array.set implementation."
  (setf (jaref array index) value))

;; Arrays inherit Object.equals: identity (the Kotlin compiler compares
;; arrays so).
(defmethod |equals(Ljava/lang/Object;)Z| ((array java-array) other)
  (if (eq array other) 1 0))

(defmethod |length()| ((array java-array))
  (length (java-array-data array)))
