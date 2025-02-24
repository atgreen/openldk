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

(defclass/std constant-class-reference ()
  ((index)))
(define-print-object/std constant-class-reference)

(defclass/std constant-string-reference ()
  ((index)))

(defclass/std constant-value ()
  ((value)))

(defclass/std constant-double (constant-value) ())
(defclass/std constant-float (constant-value) ())
(defclass/std constant-int (constant-value) ())
(defclass/std constant-long (constant-value) ())

(defclass/std constant-invoke-dynamic ()
  ((bootstrap-method-attr-index)
   (name-and-type-index)))

(define-print-object/std constant-invoke-dynamic)

(defclass/std constant-method-handle ()
  ((reference-kind)
   (reference-index)))

(define-print-object/std constant-method-handle)

(defclass/std constant-method-type ()
  ((descriptor-index)))

(define-print-object/std constant-method-type)

(defclass/std constant-field-reference ()
  ((class-index
		name-and-type-descriptor-index)))

(defclass/std constant-method-reference ()
  ((class-index
		method-descriptor-index)))

(defclass/std constant-interface-method-reference (constant-method-reference)
  ())

(defclass/std constant-name-and-type-descriptor ()
  ((name-index
		type-descriptor-index)))

(defmethod emit ((v constant-string-reference) cp)
  (make-instance 'ir-string-literal
                 :value (format nil "~A" (emit (aref cp (slot-value v 'index)) cp))))

(defmethod get-stack-jtype ((v constant-string-reference))
  :REFERENCE)

(defmethod emit ((v constant-float) cp)
  (make-instance 'ir-float-literal :value (slot-value v 'value)))

(defmethod get-stack-jtype ((v constant-float))
  :FLOAT)

(defmethod emit ((v constant-double) cp)
  (make-instance 'ir-double-literal :value (slot-value v 'value)))

(defmethod get-stack-jtype ((v constant-double))
  :FLOAT)

(defmethod emit ((v constant-int) cp)
  (make-instance 'ir-int-literal :value (slot-value v 'value)))

(defmethod get-stack-jtype ((v constant-int))
  :INTEGER)

(defmethod emit ((v constant-long) cp)
  (make-instance 'ir-long-literal :value (slot-value v 'value)))

(defmethod get-stack-jtype ((v constant-long))
  :LONG)

(defmethod emit ((v constant-field-reference) cp)
  (let ((class (emit (aref cp (slot-value v 'class-index)) cp)))
    (values (emit-name (aref cp (slot-value v 'name-and-type-descriptor-index)) cp) class)))

(defmethod emit-type ((v constant-field-reference) cp)
  (let ((class (emit (aref cp (slot-value v 'class-index)) cp)))
    (values (emit-type (aref cp (slot-value v 'name-and-type-descriptor-index)) cp) class)))

(defmethod emit ((v constant-class-reference) cp)
  (let ((classname (emit (aref cp (slot-value v 'index)) cp)))
    (if (eq (aref classname 0) #\[)
        (make-instance 'ir-class
                       :class (if (eq (aref classname 1) #\L)
                                  (%get-array-lclass-from-name "[Ljava/lang/Object;")
                                  (%get-array-lclass-from-name classname)))
        (make-instance 'ir-class :class (classload classname)))))

(defmethod get-stack-jtype ((v constant-class-reference))
  :REFERENCE)

(defmethod emit-name ((v constant-class-reference) cp)
  (emit (aref cp (slot-value v 'index)) cp))

(defmethod emit ((v constant-name-and-type-descriptor) cp)
  (format nil "~A~A"
          (emit (aref cp (slot-value v 'name-index)) cp)
          (emit (aref cp (slot-value v 'type-descriptor-index)) cp)))

(defmethod emit-name ((v constant-name-and-type-descriptor) cp)
  (format nil "~A" (emit (aref cp (slot-value v 'name-index)) cp)))

(defmethod emit-type ((v constant-name-and-type-descriptor) cp)
  (format nil "~A" (emit (aref cp (slot-value v 'type-descriptor-index)) cp)))

#|
(defmethod emit ((v constant-method-reference) cp)
  (format nil "~A.~A"
          (emit (aref cp (slot-value v 'class-index)) cp)
          (emit (aref cp (slot-value v 'method-descriptor-index)) cp)))
|#
(defmethod emit ((v constant-method-reference) cp)
  (format nil "~A"
          (emit (aref cp (slot-value v 'method-descriptor-index)) cp)))

(defun wrap-fast-read-sequence (vec buf &key (start 0) (end nil))
  "Must wrap this so it has the same signature as clhs' READ-SEQUENCE."
  (fast-io:fast-read-sequence vec buf start end))

(defclass/std <class> ()
  ((initialized-p
		name
		super
    interfaces
		constant-pool
		access-flags
		fields
		methods
    attributes
		java-class)))

(defmethod print-object ((class <class>) out)
  (print-unreadable-object (class out :type t)
    (format out "~A" (slot-value class 'name))))

(defclass/std <field> ()
  ((class :with)
	 (name)
	 (descriptor)
	 (access-flags :std 0)
	 (attributes)))

(defclass/std <method> (<field>)
  ())

(defclass/std <bootstrap-method> ()
  ((method-ref)
   (method-args)))

(define-print-object/std <bootstrap-method>)

(define-print-object/std <method>)

(defun interface-p (class)
  (not (eq 0 (logand #x200 (slot-value class 'access-flags)))))

(defun native-p (method)
  (not (eq 0 (logand #x100 (slot-value method 'access-flags)))))

(defun static-p (method)
  (not (eq 0 (logand #x8 (slot-value method 'access-flags)))))

(defun abstract-p (method)
  (not (eq 0 (logand #x400 (slot-value method 'access-flags)))))

(defun bridge-p (method)
  (not (eq 0 (logand #x40 (access-flags method)))))

(defmethod print-object ((method <method>) out)
  (print-unreadable-object (method out :type t)
    (format out "~A.~A.~A" (slot-value method 'class) (slot-value method 'name) (slot-value method 'descriptor))))

(defclass/std <code> ()
  ((max-stack)
   (max-locals)
   (code)
   (exceptions)
   (attributes)))

(defmacro read-u2 ()
  `(bitio:read-integer bitio :num-bytes 2 :byte-endian :be :unsignedp t))

(defmacro read-u4 ()
  `(bitio:read-integer bitio :num-bytes 4 :byte-endian :be :unsignedp t))

(defmacro read-u8 ()
  `(bitio:read-integer bitio :num-bytes 8 :byte-endian :be :unsignedp t))

(defmacro read-buffer (size)
  `(let ((buf (make-array ,size :element-type '(unsigned-byte 8))))
     (bitio:read-bytes bitio buf :bit-endian :be :bits-per-byte 8)
     buf))

(defclass/std <exception-table-entry> ()
  ((start-pc)
   (end-pc)
   (handler-pc)
   (catch-type)))

(defmethod print-object ((ete <exception-table-entry>) out)
  (print-unreadable-object (ete out :type t)
    (with-slots (start-pc end-pc handler-pc) ete
      (format out "(~A:~A]->~A" start-pc end-pc handler-pc))))

(defun read-exceptions (bitio cp count)
  (if (> count 0)
      (let ((exceptions (make-array count)))
        (dotimes (i count)
          (setf (aref exceptions i)
                (make-instance '<exception-table-entry>
                               :start-pc (read-u2)
                               :end-pc (read-u2)
                               :handler-pc (read-u2)
                               :catch-type (let ((i (read-u2)))
                                             (if (eq i 0)
                                                 nil
                                                 (emit-name (aref cp i) cp))))))
        exceptions)
      nil))

(defun read-attributes (bitio constant-pool class count)
  "Read COUNT attributes from a classfile for CLASS using the BITIO
stream."
  (let ((attributes (make-hash-table :test 'equal)))
    (dotimes (i count)
      (let* ((name-index (read-u2))
             (name (slot-value (aref constant-pool name-index) 'value))
             (attributes-length (read-u4)))
        (alexandria:eswitch (name :test 'string=)
          ("BootstrapMethods"
           (let ((num-bootstrap-methods (read-u2)))
             (setf (gethash "BootstrapMethods" attributes)
                   (loop for i below num-bootstrap-methods
                         collect (let* ((bootstrap-method-ref (read-u2))
                                        (num-bootstrap-arguments (read-u2))
                                        (bootstrap-arguments (loop for i below num-bootstrap-arguments
                                                                   collect (read-u2))))
                                   (make-instance '<bootstrap-method>
                                                  :method-ref bootstrap-method-ref
                                                  :method-args bootstrap-arguments))))))
          ("Code"
           (let* ((max-stack (read-u2))
                  (max-locals (read-u2))
                  (code-length (read-u4))
                  (code (read-buffer code-length))
                  (exception-table-length (read-u2))
                  (exceptions (read-exceptions bitio constant-pool exception-table-length))
                  (attribute-count (read-u2))
                  (code-attributes (read-attributes bitio constant-pool class attribute-count)))
             (setf (gethash "Code" attributes)
                   (make-instance '<code>
                                  :max-stack max-stack
                                  :max-locals max-locals
                                  :code code
                                  :exceptions exceptions
                                  :attributes code-attributes))))
          ("ConstantValue"
           (read-buffer attributes-length))
          ("Deprecated"
           (read-buffer attributes-length))
          ("EnclosingMethod"
           (read-buffer attributes-length))
          ("Exceptions"
           (read-buffer attributes-length))
          ("InnerClasses"
           (read-buffer attributes-length))
          ("LocalVariableTable"
           (read-buffer attributes-length))
          ("LocalVariableTypeTable"
           (read-buffer attributes-length))
          ("LineNumberTable"
           (read-buffer attributes-length))
          ("RuntimeVisibleAnnotations"
           (read-buffer attributes-length))
          ("StackMapTable"
           (read-buffer attributes-length))
          ("SourceFile"
           (read-buffer attributes-length))
          ("Signature"
           (read-buffer attributes-length)))))
    attributes))

(defun modified-utf8-to-utf8 (data)
  "Convert a vector of bytes from Modified UTF-8 to standard UTF-8."
  (let ((result (make-array 0 :element-type 'unsigned-byte :adjustable t :fill-pointer 0)))
    (loop for i from 0 below (length data)
          do (let ((byte (aref data i)))
               (if (and (= byte #xC0)
                        (< (1+ i) (length data))
                        (= (aref data (1+ i)) #x80))
                   (progn
                     ;; Replace the Modified UTF-8 null sequence (0xC0 0x80) with 0x00
                     (vector-push-extend 0 result)
                     (incf i)) ;; Skip the next byte (0x80)
                   ;; Otherwise, copy the byte as is
                   (vector-push-extend byte result))))
    result))

(defun read-classfile (fin)
  (let ((class (make-instance '<class>)))
    (with-slots (methods fields super interfaces) class
      (fast-io:with-fast-input (fin-fast nil fin)
        (let ((bitio (bitio:make-bitio fin-fast #'fast-io:fast-read-byte #'wrap-fast-read-sequence)))
          (flet ((read-u1 () (bitio:read-one-byte bitio)))
            (bitio:read-integer bitio :unsignedp nil :byte-endian :be) ;; magic bytes
            (read-u2) ;; minor-version
            (read-u2) ;; major version
            (let ((constant-pool-count (read-u2)))
              (let ((constant-pool (make-array (1+ constant-pool-count)))
                    (skip nil))
                (setf (slot-value class 'constant-pool) constant-pool)
                (dotimes (i (1- constant-pool-count))
                  (if skip
                      (setf skip nil)
                      (let ((tag (read-u1)))
                        (setf (aref constant-pool (1+ i))
                              ;; https://en.wikipedia.org/wiki/Java_class_file
                              (ccase tag
                                (1
                                 (let* ((size (read-u2))
                                        (octets (read-buffer size)))
                                   (make-instance 'ir-string-literal
                                                  :value (flexi-streams:octets-to-string (modified-utf8-to-utf8 octets)
                                                                                         :external-format :utf-8))))
                                (3
                                 (make-instance 'constant-int
                                                :value (unsigned-to-signed-integer (read-u4))))
                                (4
                                 (make-instance 'constant-float
                                                :value (float-features:bits-single-float (read-u4))))
                                (5
                                 (progn
                                   (setf skip t)
                                   (let ((ul (read-u8)))
                                     (make-instance 'constant-long
                                                    :value (unsigned-to-signed-long ul)))))
                                (6
                                 (progn
                                   (setf skip t)
                                   (make-instance 'constant-double
                                                  :value (float-features:bits-double-float (read-u8)))))
                                (7
                                 (make-instance 'constant-class-reference
                                                :index (read-u2)))
                                (8
                                 (make-instance 'constant-string-reference
                                                :index (read-u2)))
                                (9
                                 (make-instance 'constant-field-reference
                                                :class-index (read-u2)
                                                :name-and-type-descriptor-index (read-u2)))
                                (10
                                 (let ((class-index (read-u2))
                                       (method-descriptor-index (read-u2)))
                                   (make-instance 'constant-method-reference
                                                  :class-index class-index
                                                  :method-descriptor-index method-descriptor-index)))
                                (11
                                 (let ((class-index (read-u2))
                                       (method-descriptor-index (read-u2)))
                                   (make-instance 'constant-interface-method-reference
                                                  :class-index class-index
                                                  :method-descriptor-index method-descriptor-index)))
                                (12
                                 (let ((name-index (read-u2))
                                       (type-descriptor-index (read-u2)))
                                   (make-instance 'constant-name-and-type-descriptor
                                                  :name-index name-index
                                                  :type-descriptor-index type-descriptor-index)))
                                (15
                                 (let ((reference-kind (read-u1))
                                       (reference-index (read-u2)))
                                   (make-instance '|com/moxielogic/OpenLDK/MethodHandle|
                                                  :reference-kind reference-kind
                                                  :reference-index reference-index)))
                                (16
                                 (let ((descriptor-index (read-u2)))
                                   (make-instance 'constant-method-type
                                                  :descriptor-index descriptor-index)))
                                (18
                                 (let ((bootstrap-method-attr-index (read-u2))
                                       (name-and-type-index (read-u2)))
                                   (make-instance 'constant-invoke-dynamic
                                                  :bootstrap-method-attr-index bootstrap-method-attr-index
                                                  :name-and-type-index name-and-type-index)))
                                )))))

                (let* ((access-flags (read-u2))
                       (this-class (read-u2))
                       (super-class (read-u2))
                       (interface-count (read-u2)))
                  (when (> interface-count 0)
                    (setf interfaces (make-array interface-count)))
                  (setf (slot-value class 'name) (slot-value (aref constant-pool (slot-value (aref constant-pool this-class) 'index)) 'value))
                  (setf (slot-value class 'access-flags) access-flags)
                  (if (> super-class 0)
                      (setf super
                            (let ((super (slot-value (aref constant-pool (slot-value (aref constant-pool super-class) 'index)) 'value)))
                              (if (and (not (eq 0 (logand #x200 access-flags)))
                                       (string= super "java/lang/Object"))
                                  nil
                                  super))))

                  (dotimes (i interface-count)
                    (let ((interface (read-u2)))
                      (setf (aref interfaces i) (slot-value (aref constant-pool (slot-value (aref constant-pool interface) 'index)) 'value))))

                  (let ((fields-count (read-u2)))
                    (setf fields (make-array fields-count))
                    (dotimes (i fields-count)
                      (let* ((access-flags (read-u2))
                             (name-index (read-u2))
                             (descriptor-index (read-u2))
                             (attributes-count (read-u2))
                             (field (make-instance '<field>
                                                   :class class
                                                   :name (slot-value (aref constant-pool name-index) 'value)
                                                   :descriptor (slot-value (aref constant-pool descriptor-index) 'value)
                                                   :access-flags access-flags)))
                        (setf (aref fields i) field)
                        (setf (slot-value field 'attributes)
                              (read-attributes bitio constant-pool class attributes-count)))))
                  (let ((methods-count (read-u2)))
                    (setf methods (make-array methods-count))
                    (dotimes (i methods-count)
                      (let* ((access-flags (read-u2))
                             (name-index (read-u2))
                             (descriptor-index (read-u2))
                             (attributes-count (read-u2))
                             (method (make-instance '<method>
                                                    :class class
                                                    :name (slot-value (aref constant-pool name-index) 'value)
                                                    :descriptor (slot-value (aref constant-pool descriptor-index) 'value)
                                                    :access-flags access-flags)))
                        (setf (aref methods i) method)
                        (setf (slot-value method 'attributes)
                              (read-attributes bitio constant-pool class attributes-count)))))
                  (setf (attributes class) (read-attributes bitio constant-pool class (read-u2))))))))))
    class))
