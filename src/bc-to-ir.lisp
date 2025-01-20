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

(defun pop-args (num-args)
  (loop repeat num-args
        collect (make-instance 'ir-pop)))

(defun :AALOAD (context code)
  (declare (ignore code))
  (with-slots (pc) context
    (let ((pc-start pc))
      (incf pc)
      (list (make-instance 'ir-aaload
                           :address pc-start)))))

(defun :AASTORE (context code)
  (declare (ignore code))
  (with-slots (pc) context
    (let ((pc-start pc))
      (incf pc)
      (list (make-instance 'ir-aastore
                           :address pc-start)))))

(defun :ACONST_NULL (context code)
  (declare (ignore code))
  (with-slots (pc) context
    (let ((pc-start pc))
      (incf pc)
      (list (make-instance 'ir-push
                           :address pc-start
                           :value (make-instance 'ir-null-literal
                                                 :address pc-start))))))

(defun :ALOAD (context code)
  (declare (ignore code))
  (with-slots (pc) context
    (let ((pc-start pc)
          (index (aref code (incf pc))))
      (incf pc)
      (list (make-instance 'ir-push
                           :address pc-start
                           :value (make-instance 'ir-local-variable
                                                 :address pc-start
                                                 :index index))))))

(defun :ALOAD_0 (context code)
  (declare (ignore code))
  (with-slots (pc) context
    (let ((pc-start pc))
      (incf pc)
      (list (make-instance 'ir-push
                           :address pc-start
                           :value (make-instance 'ir-local-variable
                                                 :address pc-start
                                                 :index 0))))))

(defun :ALOAD_1 (context code)
  (declare (ignore code))
  (with-slots (pc) context
    (let ((pc-start pc))
      (incf pc)
      (list (make-instance 'ir-push
                           :address pc-start
                           :value (make-instance 'ir-local-variable
                                                 :address pc-start
                                                 :index 1))))))

(defun :ALOAD_2 (context code)
  (declare (ignore code))
  (with-slots (pc) context
    (let ((pc-start pc))
      (incf pc)
      (list (make-instance 'ir-push
                           :address pc-start
                           :value (make-instance 'ir-local-variable
                                                 :address pc-start
                                                 :index 2))))))

(defun :ALOAD_3 (context code)
  (declare (ignore code))
  (with-slots (pc) context
    (let ((pc-start pc))
      (incf pc)
      (list (make-instance 'ir-push
                           :address pc-start
                           :value (make-instance 'ir-local-variable
                                                 :address pc-start
                                                 :index 3))))))

(defun :ARRAYLENGTH (context code)
  (with-slots (pc) context
    (let ((pc-start pc))
      (incf pc)
      (list (make-instance 'ir-array-length
                           :address pc-start)))))


(defun :ASTORE (context code)
  (with-slots (pc) context
    (let ((pc-start pc)
          (index (aref code (incf pc))))
      (incf pc)
      (list (make-instance 'ir-store
                           :address pc-start
                           :target (make-instance 'ir-local-variable
                                                  :address pc-start
                                                  :index index))))))

(defun :ASTORE_0 (context code)
  (declare (ignore code))
  (with-slots (pc) context
    (let ((pc-start pc))
      (incf pc)
      (list (make-instance 'ir-store
                           :address pc-start
                           :target (make-instance 'ir-local-variable
                                                  :address pc-start
                                                  :index 0))))))

(defun :ASTORE_1 (context code)
  (declare (ignore code))
  (with-slots (pc) context
    (let ((pc-start pc))
      (incf pc)
      (list (make-instance 'ir-store
                           :address pc-start
                           :target (make-instance 'ir-local-variable
                                                  :address pc-start
                                                  :index 1))))))

(defun :ASTORE_2 (context code)
  (declare (ignore code))
  (with-slots (pc) context
    (let ((pc-start pc))
      (incf pc)
      (list (make-instance 'ir-store
                           :address pc-start
                           :target (make-instance 'ir-local-variable
                                                  :address pc-start
                                                  :index 2))))))

(defun :IALOAD (context code)
  (declare (ignore code))
  (with-slots (pc) context
    (let ((pc-start pc))
      (incf pc)
      (list (make-instance 'ir-iaload :address pc-start)))))

(defun :IASTORE (context code)
  (declare (ignore code))
  (with-slots (pc) context
    (let ((pc-start pc))
      (incf pc)
      (list (make-instance 'ir-iastore
                           :address pc-start)))))

(defun %unsigned-to-signed-byte (value)
  "Convert an unsigned byte (0-255) to a signed byte (-128 to 127)."
  (if (> value 127)
      (- value 256)
      value))

(defun :IINC (context code)
  (with-slots (pc) context
    (let ((pc-start pc))
      (with-slots (constant-pool) class
        (let* ((index (aref code (incf pc)))
               (const (%unsigned-to-signed-byte (aref code (incf pc)))))
          (incf pc)
          (list (make-instance 'ir-iinc
                               :address pc-start
                               :index index
                               :const const)))))))

(defun :ISTORE (context code)
  (with-slots (pc) context
    (let ((pc-start pc)
          (index (aref code (incf pc))))
      (incf pc)
      (list (make-instance 'ir-store
                           :address pc-start
                           :target (make-instance 'ir-local-variable
                                                  :address pc-start
                                                  :index index))))))

(defun :ISTORE_0 (context code)
  (declare (ignore code))
  (with-slots (pc) context
    (let ((pc-start pc))
      (incf pc)
      (list (make-instance 'ir-store
                           :address pc-start
                           :target (make-instance 'ir-local-variable
                                                  :address pc-start
                                                  :index 0))))))

(defun :ISTORE_1 (context code)
  (declare (ignore code))
  (with-slots (pc) context
    (let ((pc-start pc))
      (incf pc)
      (list (make-instance 'ir-store
                           :address pc-start
                           :target (make-instance 'ir-local-variable
                                                  :address pc-start
                                                  :index 1))))))

(defun :ISTORE_2 (context code)
  (declare (ignore code))
  (with-slots (pc) context
    (let ((pc-start pc))
      (incf pc)
      (list (make-instance 'ir-store
                           :address pc-start
                           :target (make-instance 'ir-local-variable
                                                  :address pc-start
                                                  :index 2))))))

(defun :ISTORE_3 (context code)
  (declare (ignore code))
  (with-slots (pc) context
    (let ((pc-start pc))
      (incf pc)
      (list (make-instance 'ir-store
                           :address pc-start
                           :target (make-instance 'ir-local-variable
                                                  :address pc-start
                                                  :index 3))))))

(defun :ASTORE_3 (context code)
  (declare (ignore code))
  (with-slots (pc) context
    (let ((pc-start pc))
      (incf pc)
      (list (make-instance 'ir-store
                           :address pc-start
                           :target (make-instance 'ir-local-variable
                                                  :address pc-start
                                                  :index 3))))))

(defun :CALOAD (context code)
  (declare (ignore code))
  (with-slots (pc) context
    (let ((pc-start pc))
      (incf pc)
      (list (make-instance 'ir-caload :address pc-start)))))

(defun :CASTORE (context code)
  (declare (ignore code))
  (with-slots (pc) context
    (let ((pc-start pc))
      (incf pc)
      (list (make-instance 'ir-castore :address pc-start)))))

(defun :ATHROW (context code)
  (declare (ignore code))
  (with-slots (pc) context
    (let ((pc-start pc))
      (incf pc)
      (list (make-instance 'ir-throw :address pc-start)))))

(defun :CHECKCAST (context code)
  (with-slots (pc class) context
    (let ((pc-start pc))
      (with-slots (constant-pool) class
        (let* ((index (+ (* (aref code (incf pc)) 256)
                         (aref code (incf pc))))
               (class (aref constant-pool index)))
          (incf pc)
          (list (make-instance 'ir-checkcast
                               :address pc-start
                               :class (emit class constant-pool))))))))

(defun :IADD (context code)
  (declare (ignore code))
  (with-slots (pc) context
    (let ((pc-start pc))
      (incf pc)
      (list (make-instance 'ir-iadd
                           :address pc-start)))))

(defun :FADD (context code)
  (declare (ignore code))
  (with-slots (pc) context
    (let ((pc-start pc))
      (incf pc)
      (list (make-instance 'ir-fadd
                           :address pc-start)))))

(defun :DADD (context code)
  (:FADD context code))

(defun :IAND (context code)
  (declare (ignore code))
  (with-slots (pc) context
    (let ((pc-start pc))
      (incf pc)
      (list (make-instance 'ir-iand
                           :address pc-start)))))

(defun :IOR (context code)
  (declare (ignore code))
  (with-slots (pc) context
    (let ((pc-start pc))
      (incf pc)
      (list (make-instance 'ir-ior
                           :address pc-start)))))

(defun :IXOR (context code)
  (declare (ignore code))
  (with-slots (pc) context
    (let ((pc-start pc))
      (incf pc)
      (list (make-instance 'ir-ixor
                           :address pc-start)))))

(defun :ISUB (context code)
  (declare (ignore code))
  (with-slots (pc) context
    (let ((pc-start pc))
      (incf pc)
      (list (make-instance 'ir-sub
                           :address pc-start)))))

(defun :BIPUSH (context code)
  (with-slots (pc) context
    (let ((pc-start pc))
      (let* ((byte (aref code (incf pc))))
        (incf pc)
        (list (make-instance 'ir-push
                             :address pc-start
                             :value (make-instance 'ir-int-literal :address pc-start :value byte)))))))

(defun :DCONST_0 (context code)
  (declare (ignore code))
  (with-slots (pc) context
    (let ((pc-start pc))
      (incf pc)
      (list (make-instance 'ir-push
                           :address pc-start
                           :value (make-instance 'ir-double-literal
                                                 :address pc-start
                                                 :value 0.0))))))

(defun :DCONST_1 (context code)
  (declare (ignore code))
  (with-slots (pc) context
    (let ((pc-start pc))
      (incf pc)
      (list (make-instance 'ir-push
                           :address pc-start
                           :value (make-instance 'ir-double-literal
                                                 :address pc-start
                                                 :value 1.0))))))

(defun :DDIV (context code)
  (declare (ignore code))
  (with-slots (pc) context
    (let ((pc-start pc))
      (incf pc)
      (list (make-instance 'ir-div
                           :address pc-start)))))

(defun :DLOAD (context code)
  (with-slots (pc) context
    (let ((pc-start pc)
          (index (aref code (incf pc))))
      (incf pc)
      (list (make-instance 'ir-push
                           :address pc-start
                           :value (make-instance 'ir-local-variable
                                                 :address pc-start
                                                 :index index))))))

(defun :DLOAD_2 (context code)
  (declare (ignore code))
  (with-slots (pc) context
    (let ((pc-start pc))
      (incf pc)
      (list (make-instance 'ir-push
                           :address pc-start
                           :value (make-instance 'ir-local-variable
                                                 :address pc-start
                                                 :index 2))))))

(defun :DMUL (context code)
  (declare (ignore code))
  (with-slots (pc) context
    (let ((pc-start pc))
      (incf pc)
      (list (make-instance 'ir-mul
                           :address pc-start)))))

(defun :LSTORE (context code)
  (with-slots (pc) context
    (let ((pc-start pc)
          (index (aref code (incf pc))))
      (incf pc)
      (list (make-instance 'ir-store
                           :address pc-start
                           :target (make-instance 'ir-local-variable
                                                  :address pc-start
                                                  :index index))
            (make-instance 'ir-store
                           :address pc-start
                           :target (make-instance 'ir-local-variable
                                                  :address pc-start
                                                  :index (1+ index)))))))

(defun :DSTORE (context code)
  (:LSTORE context code))

(defun :FSTORE (context code)
  (:DSTORE context code))

(defun :DSTORE_2 (context code)
  (declare (ignore code))
  (with-slots (pc) context
    (let ((pc-start pc))
      (incf pc)
      (list (make-instance 'ir-store
                           :address pc-start
                           :target (make-instance 'ir-local-variable
                                                  :address pc-start
                                                  :index 2))))))

(defun :DSUB (context code)
  (declare (ignore code))
  (with-slots (pc) context
    (let ((pc-start pc))
      (incf pc)
      (list (make-instance 'ir-sub
                           :address pc-start)))))

(defun :DUP (context code)
  (declare (ignore code))
  (with-slots (pc) context
    (let ((pc-start pc))
      (incf pc)
      (list (make-instance 'ir-dup
                           :address pc-start)))))

(defun :DUP2 (context code)
  (declare (ignore code))
  (with-slots (pc) context
    (let ((pc-start pc))
      (incf pc)
      (list (make-instance 'ir-dup  ;; FIXME HACK!
                           :address pc-start)))))

(defun :DUP_X1 (context code)
  (declare (ignore code))
  (with-slots (pc) context
    (let ((pc-start pc))
      (incf pc)
      (list (make-instance 'ir-dup-x1
                           :address pc-start)))))

(defun :GETSTATIC (context code)
  (with-slots (pc class is-clinit-p) context
    (let ((context-class class)
          (pc-start pc))
      (with-slots (constant-pool) context-class
        (let* ((index (+ (* (aref code (incf pc)) 256)
                         (aref code (incf pc)))))
          (multiple-value-bind (fieldname class)
              (emit (aref constant-pool index) constant-pool)
            (format t "BMG: ~A~%"
                    (aref constant-pool (slot-value (aref constant-pool (slot-value (aref constant-pool index) 'type-descriptor-index)) 'type-descriptor-index)))
            (incf pc)
            (let ((code (list (make-instance 'ir-push
                                             :address (if (and is-clinit-p (equal (ir-class-class class) context-class)) pc-start (+ pc-start 0.1))
                                             :value (make-instance 'ir-static-member
                                                                   :address pc-start
                                                                   :class class
                                                                   :member-name fieldname)))))
              (if (and is-clinit-p (equal (ir-class-class class) context-class))
                  code
                  (cons (make-instance 'ir-clinit
                                       :address pc-start
                                       :class class)
                        code)))))))))

(defun unsigned-to-signed (unsigned-value)
  (if (>= unsigned-value 32768)
      (- unsigned-value 65536)
      unsigned-value))

(defun :GOTO (context code)
  (with-slots (pc class) context
    (let ((pc-start pc))
      (with-slots (constant-pool) class
        (let* ((offset (unsigned-to-signed (+ (* (aref code (incf pc)) 256)
                                              (aref code (incf pc))))))
          (incf pc)
          (list (make-instance 'ir-goto
                               :address pc-start
                               :offset (+ pc-start offset))))))))

(defun :FCMPG (context code)
  (declare (ignore code))
  (with-slots (pc) context
    (let ((pc-start pc))
      (incf pc)
      (list (make-instance 'ir-fcmpg
                           :address pc-start)))))

(defun :FCMPL (context code)
  (declare (ignore code))
  (with-slots (pc) context
    (let ((pc-start pc))
      (incf pc)
      (list (make-instance 'ir-fcmpl
                           :address pc-start)))))

(defun :FCONST_0 (context code)
  (declare (ignore code))
  (with-slots (pc) context
    (let ((pc-start pc))
      (incf pc)
      (list (make-instance 'ir-push
                           :address pc-start
                           :value (make-instance 'ir-float-literal
                                                 :address pc-start
                                                 :value 0.0))))))

(defun :FCONST_1 (context code)
  (declare (ignore code))
  (with-slots (pc) context
    (let ((pc-start pc))
      (incf pc)
      (list (make-instance 'ir-push
                           :address pc-start
                           :value (make-instance 'ir-float-literal
                                                 :address pc-start
                                                 :value 1.0))))))

(defun :FLOAD_0 (context code)
  (declare (ignore code))
  (with-slots (pc) context
    (let ((pc-start pc))
      (incf pc)
      (list (make-instance 'ir-push
                           :address pc-start
                           :value (make-instance 'ir-local-variable
                                                 :address pc-start
                                                 :index 0))))))

(defun :FLOAD_1 (context code)
  (declare (ignore code))
  (with-slots (pc) context
    (let ((pc-start pc))
      (incf pc)
      (list (make-instance 'ir-push
                           :address pc-start
                           :value (make-instance 'ir-local-variable
                                                 :address pc-start
                                                 :index 1))))))

(defun :FLOAD_2 (context code)
  (declare (ignore code))
  (with-slots (pc) context
    (let ((pc-start pc))
      (incf pc)
      (list (make-instance 'ir-push
                           :address pc-start
                           :value (make-instance 'ir-local-variable
                                                 :address pc-start
                                                 :index 2))))))

(defun :L2I (context code)
  (declare (ignore code))
  (with-slots (pc) context
    (let ((pc-start pc))
      (incf pc)
      ;; Just pop the high-order bits off
      (list (make-instance 'ir-pop :address pc-start)))))

(defun :L2F (context code)
  (declare (ignore code))
  (with-slots (pc) context
    (let ((pc-start pc))
      (incf pc)
      (list (make-instance 'ir-l2f :address pc-start)))))

(defun :F2I (context code)
  (declare (ignore code))
  (with-slots (pc) context
    (let ((pc-start pc))
      (incf pc)
      (list (make-instance 'ir-f2i :address pc-start)))))

(defun :D2L (context code)
  (declare (ignore code))
  (with-slots (pc) context
    (let ((pc-start pc))
      (incf pc)
      (list (make-instance 'ir-d2l :address pc-start)))))

(defun :F2D (context code)
  (declare (ignore code))
  (with-slots (pc) context
    (let ((pc-start pc))
      (incf pc)
      (list (make-instance 'ir-nop)))))

(defun :FDIV (context code)
  (declare (ignore code))
  (with-slots (pc) context
    (let ((pc-start pc))
      (incf pc)
      (list (make-instance 'ir-fdiv
                           :address pc-start)))))

(defun :FMUL (context code)
  (declare (ignore code))
  (with-slots (pc) context
    (let ((pc-start pc))
      (incf pc)
      (list (make-instance 'ir-mul
                           :address pc-start)))))

(defun :INEG (context code)
  (declare (ignore code))
  (with-slots (pc) context
    (let ((pc-start pc))
      (incf pc)
      (list (make-instance 'ir-ineg :address pc-start)))))

(defun :I2C (context code)
  (declare (ignore code))
  (with-slots (pc) context
    (let ((pc-start pc))
      (incf pc)
      (list (make-instance 'ir-i2c :address pc-start)))))

(defun :I2F (context code)
  (declare (ignore code))
  (with-slots (pc) context
    (let ((pc-start pc))
      (incf pc)
      (list (make-instance 'ir-i2f :address pc-start)))))

(defun :I2L (context code)
  (declare (ignore code))
  (with-slots (pc) context
    (let ((pc-start pc))
      (incf pc)
      (list (make-instance 'ir-push
                           :address pc-start
                           :value (make-instance 'ir-pop :address pc-start))))))

(defun :ICONST_M1 (context code)
  (declare (ignore code))
  (with-slots (pc) context
    (let ((pc-start pc))
      (incf pc)
      (list (make-instance 'ir-push
                           :address pc-start
                           :value (make-instance 'ir-int-literal
                                                 :address pc-start
                                                 :value -1))))))

(defun :ICONST_0 (context code)
  (declare (ignore code))
  (with-slots (pc) context
    (let ((pc-start pc))
      (incf pc)
      (list (make-instance 'ir-push
                           :address pc-start
                           :value (make-instance 'ir-int-literal
                                                 :address pc-start
                                                 :value 0))))))

(defun :ICONST_1 (context code)
  (declare (ignore code))
  (with-slots (pc) context
    (let ((pc-start pc))
      (incf pc)
      (list (make-instance 'ir-push
                           :address pc-start
                           :value (make-instance 'ir-int-literal
                                                 :address pc-start
                                                 :value 1))))))

(defun :ICONST_2 (context code)
  (declare (ignore code))
  (with-slots (pc) context
    (let ((pc-start pc))
      (incf pc)
      (list (make-instance 'ir-push
                           :address pc-start
                           :value (make-instance 'ir-int-literal
                                                 :address pc-start
                                                 :value 2))))))

(defun :ICONST_3 (context code)
  (declare (ignore code))
  (with-slots (pc) context
    (let ((pc-start pc))
      (incf pc)
      (list (make-instance 'ir-push
                           :address pc-start
                           :value (make-instance 'ir-int-literal
                                                 :address pc-start
                                                 :value 3))))))

(defun :ICONST_4 (context code)
  (declare (ignore code))
  (with-slots (pc) context
    (let ((pc-start pc))
      (incf pc)
      (list (make-instance 'ir-push
                           :address pc-start
                           :value (make-instance 'ir-int-literal
                                                 :address pc-start
                                                 :value 4))))))

(defun :ICONST_5 (context code)
  (declare (ignore code))
  (with-slots (pc) context
    (let ((pc-start pc))
      (incf pc)
      (list (make-instance 'ir-push
                           :address pc-start
                           :value (make-instance 'ir-int-literal
                                                 :address pc-start
                                                 :value 5))))))

(defun :IF_ACMPEQ (context code)
  (with-slots (pc class) context
    (let ((pc-start pc))
      (with-slots (constant-pool) class
        (let* ((offset (unsigned-to-signed (+ (* (aref code (incf pc)) 256)
                                              (aref code (incf pc))))))
          (incf pc)
          (list (make-instance 'ir-if-acmpeq
                               :address pc-start
                               :offset (+ pc-start offset))))))))

(defun :IF_ACMPNE (context code)
  (with-slots (pc class) context
    (let ((pc-start pc))
      (with-slots (constant-pool) class
        (let* ((offset (unsigned-to-signed (+ (* (aref code (incf pc)) 256)
                                              (aref code (incf pc))))))
          (incf pc)
          (list (make-instance 'ir-if-acmpne
                               :address pc-start
                               :offset (+ pc-start offset))))))))

(defun :IF_ICMPEQ (context code)
  (with-slots (pc class) context
    (let ((pc-start pc))
      (with-slots (constant-pool) class
        (let* ((offset (unsigned-to-signed (+ (* (aref code (incf pc)) 256)
                                              (aref code (incf pc))))))
          (incf pc)
          (list (make-instance 'ir-if-icmpeq
                               :address pc-start
                               :offset (+ pc-start offset))))))))

(defun :IF_ICMPGE (context code)
  (with-slots (pc class) context
    (let ((pc-start pc))
      (with-slots (constant-pool) class
        (let* ((offset (unsigned-to-signed (+ (* (aref code (incf pc)) 256)
                                              (aref code (incf pc))))))
          (incf pc)
          (list (make-instance 'ir-if-icmpge
                               :address pc-start
                               :offset (+ pc-start offset))))))))

(defun :IF_ICMPLE (context code)
  (with-slots (pc class) context
    (let ((pc-start pc))
      (with-slots (constant-pool) class
        (let* ((offset (unsigned-to-signed (+ (* (aref code (incf pc)) 256)
                                              (aref code (incf pc))))))
          (incf pc)
          (list (make-instance 'ir-if-icmple
                               :address pc-start
                               :offset (+ pc-start offset))))))))

(defun :IF_ICMPGT (context code)
  (with-slots (pc class) context
    (let ((pc-start pc))
      (with-slots (constant-pool) class
        (let* ((offset (unsigned-to-signed (+ (* (aref code (incf pc)) 256)
                                              (aref code (incf pc))))))
          (incf pc)
          (list (make-instance 'ir-if-icmpgt
                               :address pc-start
                               :offset (+ pc-start offset))))))))

(defun :IF_ICMPLT (context code)
  (with-slots (pc class) context
    (let ((pc-start pc))
      (with-slots (constant-pool) class
        (let* ((offset (unsigned-to-signed (+ (* (aref code (incf pc)) 256)
                                              (aref code (incf pc))))))
          (incf pc)
          (list (make-instance 'ir-if-icmplt
                               :address pc-start
                               :offset (+ pc-start offset))))))))

(defun :IF_ICMPNE (context code)
  (with-slots (pc class) context
    (let ((pc-start pc))
      (with-slots (constant-pool) class
        (let* ((offset (unsigned-to-signed (+ (* (aref code (incf pc)) 256)
                                              (aref code (incf pc))))))
          (incf pc)
          (list (make-instance 'ir-if-icmpne
                               :address pc-start
                               :offset (+ pc-start offset))))))))

(defun :IMUL (context code)
  (declare (ignore code))
  (with-slots (pc) context
    (let ((pc-start pc))
      (incf pc)
      (list (make-instance 'ir-imul
                           :address pc-start)))))

(defun :IDIV (context code)
  (declare (ignore code))
  (with-slots (pc) context
    (let ((pc-start pc))
      (incf pc)
      (list (make-instance 'ir-idiv
                           :address pc-start)))))

(defun :LDIV (context code)
  (declare (ignore code))
  (with-slots (pc) context
    (let ((pc-start pc))
      (incf pc)
      (list (make-instance 'ir-ldiv
                           :address pc-start)))))

(defun :IREM (context code)
  (declare (ignore code))
  (with-slots (pc) context
    (let ((pc-start pc))
      (incf pc)
      (list (make-instance 'ir-irem
                           :address pc-start)))))

(defun :IFEQ (context code)
  (with-slots (pc class) context
    (let ((pc-start pc))
      (with-slots (constant-pool) class
        (let* ((offset (unsigned-to-signed (+ (* (aref code (incf pc)) 256)
                                              (aref code (incf pc))))))
          (incf pc)
          (list (make-instance 'ir-ifeq
                               :address pc-start
                               :offset (+ pc-start offset))))))))

(defun :IFGE (context code)
  (with-slots (pc class) context
    (let ((pc-start pc))
      (with-slots (constant-pool) class
        (let* ((offset (unsigned-to-signed (+ (* (aref code (incf pc)) 256)
                                              (aref code (incf pc))))))
          (incf pc)
          (list (make-instance 'ir-ifge
                               :address pc-start
                               :offset (+ pc-start offset))))))))

(defun :IFLE (context code)
  (with-slots (pc class) context
    (let ((pc-start pc))
      (with-slots (constant-pool) class
        (let* ((offset (unsigned-to-signed (+ (* (aref code (incf pc)) 256)
                                              (aref code (incf pc))))))
          (incf pc)
          (list (make-instance 'ir-ifle
                               :address pc-start
                               :offset (+ pc-start offset))))))))

(defun :IFLT (context code)
  (with-slots (pc class) context
    (let ((pc-start pc))
      (with-slots (constant-pool) class
        (let* ((offset (unsigned-to-signed (+ (* (aref code (incf pc)) 256)
                                              (aref code (incf pc))))))
          (incf pc)
          (list (make-instance 'ir-iflt
                               :address pc-start
                               :offset (+ pc-start offset))))))))

(defun :IFGT (context code)
  (with-slots (pc class) context
    (let ((pc-start pc))
      (with-slots (constant-pool) class
        (let* ((offset (unsigned-to-signed (+ (* (aref code (incf pc)) 256)
                                              (aref code (incf pc))))))
          (incf pc)
          (list (make-instance 'ir-ifgt
                               :address pc-start
                               :offset (+ pc-start offset))))))))

(defun :IFNE (context code)
  (with-slots (pc class) context
    (let ((pc-start pc))
      (with-slots (constant-pool) class
        (let* ((offset (unsigned-to-signed (+ (* (aref code (incf pc)) 256)
                                              (aref code (incf pc))))))
          (incf pc)
          (list (make-instance 'ir-ifne
                               :address pc-start
                               :offset (+ pc-start offset))))))))

(defun :IFNONNULL (context code)
  (with-slots (pc class) context
    (let ((pc-start pc))
      (with-slots (constant-pool) class
        (let* ((offset (unsigned-to-signed (+ (* (aref code (incf pc)) 256)
                                              (aref code (incf pc))))))
          (incf pc)
          (list (make-instance 'ir-ifnonnull
                               :address pc-start
                               :offset (+ pc-start offset))))))))

(defun :IFNULL (context code)
  (with-slots (pc class) context
    (let ((pc-start pc))
      (with-slots (constant-pool) class
        (let* ((offset (unsigned-to-signed (+ (* (aref code (incf pc)) 256)
                                              (aref code (incf pc))))))
          (incf pc)
          (list (make-instance 'ir-ifnull
                               :address pc-start
                               :offset (+ pc-start offset))))))))

(defun :ILOAD (context code)
  (with-slots (pc) context
    (let ((pc-start pc)
          (index (aref code (incf pc))))
      (incf pc)
      (list (make-instance 'ir-push
                           :address pc-start
                           :value (make-instance 'ir-local-variable
                                                 :address pc-start
                                                 :index index))))))

(defun :LLOAD (context code)
  (with-slots (pc) context
    (let ((pc-start pc)
          (index (aref code (incf pc))))
      (incf pc)
      (list (make-instance 'ir-push
                           :address pc-start
                           :value (make-instance 'ir-local-variable
                                                 :address pc-start
                                                 :index index))))))

(defun :FLOAD (context code)
  (:ILOAD context code))

(defun :ILOAD_0 (context code)
  (declare (ignore code))
  (with-slots (pc) context
    (let ((pc-start pc))
      (incf pc)
      (list (make-instance 'ir-push
                           :address pc-start
                           :value (make-instance 'ir-local-variable
                                                 :address pc-start
                                                 :index 0))))))

(defun :ILOAD_1 (context code)
  (declare (ignore code))
  (with-slots (pc) context
    (let ((pc-start pc))
      (incf pc)
      (list (make-instance 'ir-push
                           :address pc-start
                           :value (make-instance 'ir-local-variable
                                                 :address pc-start
                                                 :index 1))))))

(defun :ILOAD_2 (context code)
  (declare (ignore code))
  (with-slots (pc) context
    (let ((pc-start pc))
      (incf pc)
      (list (make-instance 'ir-push
                           :address pc-start
                           :value (make-instance 'ir-local-variable
                                                 :address pc-start
                                                 :index 2))))))

(defun :ILOAD_3 (context code)
  (declare (ignore code))
  (with-slots (pc) context
    (let ((pc-start pc))
      (incf pc)
      (list (make-instance 'ir-push
                           :address pc-start
                           :value (make-instance 'ir-local-variable
                                                 :address pc-start
                                                 :index 3))))))

(defun :INSTANCEOF (context code)
  (with-slots (pc class) context
    (let ((pc-start pc))
      (with-slots (constant-pool) class
        (let* ((index (+ (* (aref code (incf pc)) 256)
                         (aref code (incf pc))))
               (class (aref constant-pool index)))
          (incf pc)
          (list (make-instance 'ir-instanceof
                               :address pc-start
                               :class (emit class constant-pool))))))))

(defun :INVOKEINTERFACE (context code)
  (with-slots (pc class) context
    (let ((pc-start pc))
      (with-slots (constant-pool) class
        (let* ((index (+ (* (aref code (incf pc)) 256)
                         (aref code (incf pc))))
               (method-reference (aref constant-pool index)))
          (incf pc)
          (incf pc)
          (incf pc)
          (let* ((descriptor
                   (slot-value (aref constant-pool
                                     (slot-value
                                      (aref constant-pool
                                            (slot-value method-reference
                                                        'method-descriptor-index))
                                      'type-descriptor-index))
                               'value))
                 (parameter-count (1+ (count-parameters descriptor))))
            (list (make-instance 'ir-call-virtual-method
                                 :address pc-start
                                 :void-return-p (ends-in-V (emit method-reference constant-pool))
                                 :method-name (lispize-method-name (emit method-reference constant-pool))
                                 :args (pop-args parameter-count)))))))))

(defun :INVOKEVIRTUAL (context code)
  (with-slots (pc class) context
    (let ((pc-start pc))
      (with-slots (constant-pool) class
        (let* ((index (+ (* (aref code (incf pc)) 256)
                         (aref code (incf pc))))
               (method-reference (aref constant-pool index)))
          (incf pc)
          (let* ((descriptor
                   (slot-value (aref constant-pool
                                     (slot-value
                                      (aref constant-pool
                                            (slot-value method-reference
                                                        'method-descriptor-index))
                                      'type-descriptor-index))
                               'value))
                 (parameter-count (1+ (count-parameters descriptor))))
            (list (make-instance 'ir-call-virtual-method
                                 :address pc-start
                                 :void-return-p (ends-in-V (emit method-reference constant-pool))
                                 :method-name (lispize-method-name (emit method-reference constant-pool))
                                 :args (pop-args parameter-count)))))))))

(defun :INVOKESPECIAL (context code)
  (with-slots (pc class) context
    (let ((pc-start pc))
      (with-slots (constant-pool) class
        (let* ((index (+ (* (aref code (incf pc)) 256)
                         (aref code (incf pc))))
               (method-reference (aref constant-pool index)))
          (incf pc)
          (let* ((class
                   (slot-value (emit (aref constant-pool
                                           (slot-value method-reference
                                                       'class-index))
                                     constant-pool)
                               'class))
                 (descriptor
                   (slot-value (aref constant-pool
                                     (slot-value
                                      (aref constant-pool
                                            (slot-value method-reference
                                                        'method-descriptor-index))
                                      'type-descriptor-index))
                               'value))
                 (parameter-count (1+ (count-parameters descriptor))))
            (list (make-instance 'ir-call-special-method
                                 :address pc-start
                                 :class class
                                 :void-return-p (ends-in-V (emit method-reference constant-pool))
                                 :method-name (lispize-method-name (emit method-reference constant-pool))
                                 :args (pop-args parameter-count)))))))))

(defun :INVOKESTATIC (context code)
  (with-slots (pc class is-clinit-p) context
    (let ((context-class class)
          (pc-start pc))
      (with-slots (constant-pool) class
        (let* ((index (+ (* (aref code (incf pc)) 256)
                         (aref code (incf pc))))
               (method-reference (aref constant-pool index)))
          (incf pc)
          (let* ((descriptor
                   (slot-value (aref constant-pool
                                     (slot-value
                                      (aref constant-pool
                                            (slot-value method-reference
                                                        'method-descriptor-index))
                                      'type-descriptor-index))
                               'value))
                 (callee-class
                   (slot-value (aref constant-pool
                                     (slot-value
                                      (aref constant-pool
                                            (slot-value method-reference
                                                        'class-index))
                                      'index))
                               'value))
                 (parameter-count (count-parameters descriptor)))
            (classload callee-class)
            (let ((code (list (make-instance 'ir-call-static-method
                                             :address (if (and is-clinit-p (equal callee-class (name context-class))) pc-start (+ pc-start 0.1))
                                             :class callee-class
                                             :void-return-p (ends-in-V (emit method-reference constant-pool))
                                             :return-type (get-return-type (emit method-reference constant-pool))
                                             :method-name (lispize-method-name (emit method-reference constant-pool))
                                             :args (pop-args parameter-count)))))
              (if (and is-clinit-p (equal callee-class (name context-class)))
                  code
                  (cons (make-instance 'ir-clinit
                                       :address pc-start
                                       :class (make-instance 'ir-class :class (classload callee-class)))
                        code)))))))))

(defun :IRETURN (context code)
  (declare (ignore code))
  (with-slots (pc) context
    (let ((pc-start pc))
      (incf pc)
      (list (make-instance 'ir-return-value
                           :fn-name (slot-value context 'fn-name)
                           :address pc-start)))))

(defun :LRETURN (context code)
  (:IRETURN context code))

(defun :ISHL (context code)
  (declare (ignore code))
  (with-slots (pc) context
    (let ((pc-start pc))
      (incf pc)
      (list (make-instance 'ir-ishl
                           :address pc-start)))))

(defun :ISHR (context code)
  (declare (ignore code))
  (with-slots (pc) context
    (let ((pc-start pc))
      (incf pc)
      (list (make-instance 'ir-ishr
                           :address pc-start)))))

(defun :IUSHR (context code)
  (declare (ignore code))
  (with-slots (pc) context
    (let ((pc-start pc))
      (incf pc)
      (list (make-instance 'ir-iushr
                           :address pc-start)))))

(defun :LADD (context code)
  (declare (ignore code))
  (with-slots (pc) context
    (let ((pc-start pc))
      (incf pc)
      (list (make-instance 'ir-ladd
                           :address pc-start)))))

(defun :LAND (context code)
  (declare (ignore code))
  (with-slots (pc) context
    (let ((pc-start pc))
      (incf pc)
      (list (make-instance 'ir-land
                           :address pc-start)))))

(defun :LOR (context code)
  (declare (ignore code))
  (with-slots (pc) context
    (let ((pc-start pc))
      (incf pc)
      (list (make-instance 'ir-lor
                           :address pc-start)))))

(defun :LSHL (context code)
  (declare (ignore code))
  (with-slots (pc) context
    (let ((pc-start pc))
      (incf pc)
      (list (make-instance 'ir-lshl
                           :address pc-start)))))

(defun :LSHR (context code)
  (declare (ignore code))
  (with-slots (pc) context
    (let ((pc-start pc))
      (incf pc)
      (list (make-instance 'ir-lshr
                           :address pc-start)))))

(defun :ARETURN (context code)
  (:IRETURN context code))

(defun :DRETURN (context code)
  (:IRETURN context code))

(defun :FRETURN (context code)
  (:IRETURN context code))

(defun :LCMP (context code)
  (declare (ignore code))
  (with-slots (pc) context
    (let ((pc-start pc))
      (incf pc)
      (list (make-instance 'ir-lcmp
                           :address pc-start)))))

(defun :LDC (context code)
  (with-slots (pc class) context
    (let ((pc-start pc))
      (with-slots (constant-pool) class
        (let ((index (aref code (incf pc))))
          (incf pc)
          (list (make-instance 'ir-push :address pc-start
                                         :value (emit (aref constant-pool index) constant-pool))))))))

(defun :LDC_W (context code)
  (with-slots (pc class) context
    (let ((pc-start pc))
      (with-slots (constant-pool) class
				(let ((index (+ (* (aref code (incf pc)) 256)
												(aref code (incf pc)))))
          (incf pc)
          (list (make-instance 'ir-push :address pc-start
                                        :value (emit (aref constant-pool index) constant-pool))))))))

(defun :LDC2_W (context code)
  (:LDC_W context code))

(defun :LCONST_0 (context code)
  (declare (ignore code))
  (with-slots (pc) context
    (let ((pc-start pc))
      (incf pc)
      (list (make-instance 'ir-push
                           :address pc-start
                           :value (make-instance 'ir-long-literal
                                                 :address pc-start
                                                 :value 0))
            (make-instance 'ir-push
                           :address pc-start
                           :value (make-instance 'ir-long-literal
                                                 :address pc-start
                                                 :value 0))))))

(defun :LCONST_1 (context code)
  (declare (ignore code))
  (with-slots (pc) context
    (let ((pc-start pc))
      (incf pc)
      (list (make-instance 'ir-push
                           :address pc-start
                           :value (make-instance 'ir-long-literal
                                                 :address pc-start
                                                 :value 1))
            (make-instance 'ir-push
                           :address pc-start
                           :value (make-instance 'ir-long-literal
                                                 :address pc-start
                                                 :value 0))))))

(defun :LLOAD_0 (context code)
  (declare (ignore code))
  (with-slots (pc) context
    (let ((pc-start pc))
      (incf pc)
      (list (make-instance 'ir-push
                           :address pc-start
                           :value (make-instance 'ir-long-local-variable
                                                 :address pc-start
                                                 :index 0))))))

(defun :LLOAD_1 (context code)
  (declare (ignore code))
  (with-slots (pc) context
    (let ((pc-start pc))
      (incf pc)
      (list (make-instance 'ir-push
                           :address pc-start
                           :value (make-instance 'ir-long-local-variable
                                                 :address pc-start
                                                 :index 1))))))

(defun :LSUB (context code)
  (declare (ignore code))
  (with-slots (pc) context
    (let ((pc-start pc))
      (incf pc)
      (list (make-instance 'ir-lsub
                           :address pc-start)))))

(defun :LUSHR (context code)
  (declare (ignore code))
  (with-slots (pc) context
    (let ((pc-start pc))
      (incf pc)
      (list (make-instance 'ir-lushr
                           :address pc-start)))))

(defun :MONITORENTER (context code)
  (declare (ignore code))
  (with-slots (pc) context
    (let ((pc-start pc))
      (incf pc)
      (list (make-instance 'ir-monitorenter :address pc-start)))))

(defun :MONITOREXIT (context code)
  (declare (ignore code))
  (with-slots (pc) context
    (let ((pc-start pc))
      (incf pc)
      (list (make-instance 'ir-monitorexit :address pc-start)))))

(defun :NEW (context code)
  (with-slots (pc class) context
    (let ((pc-start pc))
      (with-slots (constant-pool) class
        (let* ((index (+ (* (aref code (incf pc)) 256)
                         (aref code (incf pc))))
               (class (emit (aref constant-pool index) constant-pool)))
          (incf pc)
          (list (make-instance 'ir-push
                               :address pc-start
                               :value (make-instance 'ir-new :address pc-start :class class))))))))

(defun :NOP (context code)
  (with-slots (pc class) context
    (let ((pc-start pc))
      (incf pc)
      (list (make-instance 'ir-nop :address pc-start)))))

(defun :ANEWARRAY (context code)
  (with-slots (pc class) context
    (let ((pc-start pc))
      (with-slots (constant-pool) class
        (let* ((index (+ (* (aref code (incf pc)) 256)
                         (aref code (incf pc))))
               (class (emit (aref constant-pool index) constant-pool)))
          (incf pc)
          (list (make-instance 'ir-push
                               :address pc-start
                               :value (make-instance 'ir-new-array :address pc-start :class class))))))))

(defun :NEWARRAY (context code)
  (with-slots (pc class) context
    (let ((pc-start pc))
      (let ((atype (aref code (incf pc))))
        (incf pc)
        ;; FIXME: just throw away the type?
        (list (make-instance 'ir-push
                             :address pc-start
                             :value (make-instance 'ir-new-array :address pc-start :class nil)))))))

(defun :POP (context code)
  (declare (ignore code))
  (with-slots (pc) context
    (let ((pc-start pc))
      (incf pc)
      (list (make-instance 'ir-pop :address pc-start)))))

(defun :GETFIELD (context code)
  (with-slots (pc class) context
    (let ((pc-start pc))
      (with-slots (constant-pool) class
        (let* ((index (+ (* (aref code (incf pc)) 256)
                         (aref code (incf pc)))))
          (multiple-value-bind (fieldname)
              (emit (aref constant-pool index) constant-pool)
            (incf pc)
            (list (make-instance 'ir-push
                                 :address pc-start
                                 :value (make-instance 'ir-member
                                                       :address pc-start
                                                       :member-name fieldname)))))))))

(defun :PUTFIELD (context code)
  (with-slots (pc class) context
    (let ((pc-start pc))
      (with-slots (constant-pool) class
        (let* ((index (+ (* (aref code (incf pc)) 256)
                         (aref code (incf pc)))))
          (multiple-value-bind (fieldname)
              (emit (aref constant-pool index) constant-pool)
            (incf pc)
            (list (make-instance 'ir-assign
                                 :address pc-start
                                 :source (make-instance 'ir-pop :address pc-start)
                                 :target (make-instance 'ir-member
                                                        :address pc-start
                                                        :member-name fieldname)))))))))

(defun :PUTSTATIC (context code)
  (with-slots (pc class is-clinit-p) context
    (let ((context-class class)
          (pc-start pc))
      (with-slots (constant-pool) context-class
        (let* ((index (+ (* (aref code (incf pc)) 256)
                         (aref code (incf pc)))))
          (multiple-value-bind (fieldname class)
              (emit (aref constant-pool index) constant-pool)
            (incf pc)
            (let ((code (list (make-instance 'ir-assign
                                             :address (if (and is-clinit-p (equal (ir-class-class class) context-class)) pc-start (+ pc-start 0.1))
                                             :source (make-instance 'ir-pop :address pc-start)
                                             :target (make-instance 'ir-static-member
                                                                    :address pc-start
                                                                    :class class
                                                                    :member-name fieldname)))))
              (if (and is-clinit-p (equal (ir-class-class class) context-class))
                  code
                  (cons (make-instance 'ir-clinit
                                       :address pc-start
                                       :class class)
                        code)))))))))

(defun :RETURN (context code)
  (declare (ignore code))
  (with-slots (pc) context
    (let ((pc-start pc))
      (incf pc)
      (list (make-instance 'ir-return
                           :address pc-start)))))

(defun :SIPUSH (context code)
  (with-slots (pc) context
    (let ((pc-start pc))
      (let* ((short (unsigned-to-signed (+ (* (aref code (incf pc)) 256)
                                           (* (aref code (incf pc)))))))
        (incf pc)
        (list (make-instance 'ir-push
                             :address pc-start
                             :value (make-instance 'ir-int-literal :address pc-start :value short)))))))
