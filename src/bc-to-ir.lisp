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

(defmacro define-bytecode-transpiler-TODO (name args &body body)
  (declare (ignore args))
  (declare (ignore body))
  `(defun ,name ()
     (format t "TODO transpiling ~A~%" ,name)
     (error "TODO")))

(defmacro define-bytecode-transpiler (name args &body body)
  (let ((start-pc (gensym))
        (has-declare (eq (car (car body)) 'declare)))
    `(defun ,name ,args
       ,(when has-declare (car body))
       (let* ((,start-pc (pc context))
              (code (progn ,@(if has-declare (cdr body) body))))
         ;; Save the size of this instruction in the INSN-SIZE array.
         (setf (aref (insn-size context) ,start-pc) (- (pc context) ,start-pc))
         code))))

(defmacro declare-IGNORE (x)
  (declare (ignore x)))

(defun pop-args (num-args context)
  (loop for i below num-args
        collect (pop (stack context))))

(defun %transpile-xastore (context ir-class)
  (with-slots (pc) context
    (let ((pc-start pc))
      (incf pc)
      (push pc (aref (next-insn-list context) pc-start))
      (list (make-instance ir-class
                           :address pc-start
                           :value (pop (stack context))
                           :index (pop (stack context))
                           :arrayref (pop (stack context)))))))

(define-bytecode-transpiler :AASTORE (context code)
  (declare (ignore code))
  (%transpile-xastore context 'ir-aastore))

(define-bytecode-transpiler :CASTORE (context code)
  (declare (ignore code))
  (%transpile-xastore context 'ir-castore))

(define-bytecode-transpiler :IASTORE (context code)
  (declare (ignore code))
  (%transpile-xastore context 'ir-iastore))

(define-bytecode-transpiler :ACONST_NULL (context code)
  (declare (ignore code))
  (with-slots (pc) context
    (let* ((pc-start pc)
           (var (make-stack-variable context pc-start :REFERENCE)))
      (incf pc)
      (push pc (aref (next-insn-list context) pc-start))
      (push var (stack context))
      (list (make-instance 'ir-assign
                           :address pc-start
                           :lvalue var
                           :rvalue (make-instance 'ir-null-literal
                                                  :address pc-start))))))

(defun %transpile-aload-x (context index)
  (with-slots (pc) context
    (let* ((pc-start pc)
           (var (make-stack-variable context pc-start :REFERENCE)))
      (incf pc)
      (push pc (aref (next-insn-list context) pc-start))
      (push var (stack context))
      (list (make-instance 'ir-assign
                           :address pc-start
                           :lvalue var
                           :rvalue (make-instance 'ir-local-variable
                                                  :address pc-start
                                                  :index index))))))

(define-bytecode-transpiler :ALOAD_0 (context code)
  (declare (ignore code))
  (%transpile-aload-x context 0))

(define-bytecode-transpiler :ALOAD_1 (context code)
  (declare (ignore code))
  (%transpile-aload-x context 1))

(define-bytecode-transpiler :ALOAD_2 (context code)
  (declare (ignore code))
  (%transpile-aload-x context 2))

(define-bytecode-transpiler :ALOAD_3 (context code)
  (declare (ignore code))
  (%transpile-aload-x context 3))

(define-bytecode-transpiler :ARRAYLENGTH (context code)
  (with-slots (pc) context
    (let* ((pc-start pc)
           (var (make-stack-variable context pc-start :INTEGER)))
      (incf pc)
      (push pc (aref (next-insn-list context) pc-start))
      (let ((code (list (make-instance 'ir-assign
                                       :address pc-start
                                       :lvalue var
                                       :rvalue (make-instance 'ir-array-length
                                                              :address pc-start
                                                              :arrayref (pop (stack context)))))))
        (push var (stack context))
        code))))

(define-bytecode-transpiler :ASTORE (context code)
  (with-slots (pc) context
    (let ((pc-start pc)
          (index (aref code (incf pc))))
      (incf pc)
      (push pc (aref (next-insn-list context) pc-start))
      (list (make-instance 'ir-assign
                           :address pc-start
                           :lvalue (make-instance 'ir-local-variable
                                                  :address pc-start
                                                  :index index
                                                  :jtype :REFERENCE)
                           :rvalue (pop (stack context)))))))

(defun %transpile-astore-x (context index)
  (with-slots (pc) context
    (let* ((pc-start pc))
      (incf pc)
      (push pc (aref (next-insn-list context) pc-start))
      (list (make-instance 'ir-assign
                           :address pc-start
                           :lvalue (make-instance 'ir-local-variable
                                                  :address pc-start
                                                  :index index
                                                  :jtype :REFERENCE)
                           :rvalue (pop (stack context)))))))

(define-bytecode-transpiler :ASTORE_0 (context code)
  (declare (ignore code))
  (%transpile-astore-x context 0))

(define-bytecode-transpiler :ASTORE_1 (context code)
  (declare (ignore code))
  (%transpile-astore-x context 1))

(define-bytecode-transpiler :ASTORE_2 (context code)
  (declare (ignore code))
  (%transpile-astore-x context 2))

(define-bytecode-transpiler :ASTORE_3 (context code)
  (declare (ignore code))
  (%transpile-astore-x context 3))

(defun %unsigned-to-signed-byte (value)
  "Convert an unsigned byte (0-255) to a signed byte (-128 to 127)."
  (if (> value 127)
      (- value 256)
      value))

(define-bytecode-transpiler :IINC (context code)
  (with-slots (pc) context
    (let ((pc-start pc))
      (with-slots (constant-pool) class
        (let* ((index (aref code (incf pc)))
               (const (%unsigned-to-signed-byte (aref code (incf pc)))))
          (incf pc)
          (push pc (aref (next-insn-list context) pc-start))
          (list (make-instance 'ir-iinc
                               :address pc-start
                               :index index
                               :const const)))))))

(defun %transpile-xstore (context code jtype)
  (with-slots (pc) context
    (let ((pc-start pc)
          (index (aref code (incf pc)))
          (var (pop (stack context))))
      (incf pc)
      (push pc (aref (next-insn-list context) pc-start))
      (list (make-instance 'ir-assign
                           :address pc-start
                           :lvalue (make-instance 'ir-local-variable
                                                  :address pc-start
                                                  :index index
                                                  :jtype jtype)
                           :rvalue var)))))

(define-bytecode-transpiler :ISTORE (context code)
  (%transpile-xstore context code :INTEGER))

(define-bytecode-transpiler :FSTORE (context code)
  (%transpile-xstore context code :FLOAT))

(define-bytecode-transpiler :LSTORE (context code)
  (%transpile-xstore context code :LONG))

(define-bytecode-transpiler :DSTORE (context code)
  (%transpile-xstore context code :DOUBLE))

(defun %transpile-istore-x (context index)
  (with-slots (pc) context
    (let* ((pc-start pc)
           (var (pop (stack context))))
      (incf pc)
      (push pc (aref (next-insn-list context) pc-start))
      (list (make-instance 'ir-assign
                           :address pc-start
                           :lvalue (make-instance 'ir-local-variable
                                                  :address pc-start
                                                  :index index)
                           :rvalue var)))))

(define-bytecode-transpiler :ISTORE_0 (context code)
  (declare (ignore code))
  (%transpile-istore-x context 0))

(define-bytecode-transpiler :ISTORE_1 (context code)
  (declare (ignore code))
  (%transpile-istore-x context 1))

(define-bytecode-transpiler :ISTORE_2 (context code)
  (declare (ignore code))
  (%transpile-istore-x context 2))

(define-bytecode-transpiler :ISTORE_3 (context code)
  (declare (ignore code))
  (%transpile-istore-x context 3))

(define-bytecode-transpiler :CALOAD (context code)
  (declare (ignore code))
  (with-slots (pc) context
    (let* ((pc-start pc)
           (var (make-stack-variable context pc-start :CHAR)))
      (incf pc)
      (push pc (aref (next-insn-list context) pc-start))
      (let ((code
             (list (make-instance 'ir-assign
                                  :address pc-start
                                  :lvalue var
                                  :rvalue (make-instance 'ir-caload
                                                         :address pc-start
                                                         :index (pop (stack context))
                                                         :arrayref (pop (stack context)))))))
        (push var (stack context))
        code))))

(define-bytecode-transpiler :IALOAD (context code)
  (declare (ignore code))
  (with-slots (pc) context
    (let* ((pc-start pc)
           (var (make-stack-variable context pc-start :CHAR)))
      (incf pc)
      (push pc (aref (next-insn-list context) pc-start))
      (let ((code
             (list (make-instance 'ir-assign
                                  :address pc-start
                                  :lvalue var
                                  :rvalue (make-instance 'ir-iaload
                                                         :address pc-start
                                                         :index (pop (stack context))
                                                         :arrayref (pop (stack context)))))))
        (push var (stack context))
        code))))

(define-bytecode-transpiler :AALOAD (context code)
  (declare (ignore code))
  (with-slots (pc) context
    (let* ((pc-start pc)
           (var (make-stack-variable context pc-start :REFERENCE)))
      (incf pc)
      (push pc (aref (next-insn-list context) pc-start))
      (let ((code
             (list (make-instance 'ir-assign
                                  :address pc-start
                                  :lvalue var
                                  :rvalue (make-instance 'ir-aaload
                                                         :address pc-start
                                                         :index (pop (stack context))
                                                         :arrayref (pop (stack context)))))))
        (push var (stack context))
        code))))

(define-bytecode-transpiler :ATHROW (context code)
  (declare (ignore code))
  (with-slots (pc) context
    (let ((pc-start pc))
      (incf pc)
      (list (make-instance 'ir-throw :address pc-start :objref (pop (stack context)))))))

(define-bytecode-transpiler :CHECKCAST (context code)
  (with-slots (pc class) context
    (let ((pc-start pc))
      (with-slots (constant-pool) class
        (let* ((index (+ (* (aref code (incf pc)) 256)
                         (aref code (incf pc))))
               (class (aref constant-pool index)))
          (incf pc)
          (push pc (aref (next-insn-list context) pc-start))
          (list (make-instance 'ir-checkcast
                               :address pc-start
                               :class (emit class constant-pool)
                               :objref (car (stack context)))))))))

(defun %transpile-unop (context ir-class op-type)
  (with-slots (pc) context
    (let* ((pc-start pc)
           (var (make-stack-variable context pc-start op-type))
           (value (pop (stack context))))
      (incf pc)
      (push pc (aref (next-insn-list context) pc-start))
      (push var (stack context))
      (list (make-instance 'ir-assign
                           :address pc-start
                           :lvalue var
                           :rvalue (make-instance ir-class
                                                  :value value
                                                  :address pc-start))))))

(defmacro %define-unop-transpilers (&rest opcodes)
  `(progn
     ,@(mapcar (lambda (opcode)
                 (let ((name (car opcode))
                       (ir-class (cadr opcode))
                       (jtype (caddr opcode)))
                   `(define-bytecode-transpiler ,name (context code)
                      (declare (ignore code))
                      (%transpile-unop context ,ir-class ,jtype))))
               opcodes)))

(%define-unop-transpilers
 (:D2L 'ir-d2l :LONG)
 (:F2D 'ir-f2d :DOUBLE)
 (:F2I 'ir-f2i :INTEGER)
 (:I2F 'ir-i2f :FLOAT)
 (:I2C 'ir-i2c :CHAR)
 (:I2L 'ir-i2l :LONG)
 (:L2F 'ir-l2f :FLOAT)
 (:L2I 'ir-l2i :INTEGER))

 (defun %transpile-binop (context ir-class op-type)
  (with-slots (pc) context
    (let* ((pc-start pc)
           (var (make-stack-variable context pc-start op-type))
           (value2 (pop (stack context)))
           (value1 (pop (stack context))))
      (incf pc)
      (push pc (aref (next-insn-list context) pc-start))
      (push var (stack context))
      (list (make-instance 'ir-assign
                           :address pc-start
                           :lvalue var
                           :rvalue (make-instance ir-class
                                                  :value1 value1
                                                  :value2 value2
                                                  :address pc-start))))))

(defmacro %define-binop-transpilers (&rest opcodes)
  `(progn
     ,@(mapcar (lambda (opcode)
                 (let ((name (car opcode))
                       (ir-class (cadr opcode))
                       (jtype (caddr opcode)))
                   `(define-bytecode-transpiler ,name (context code)
                      (declare (ignore code))
                      (%transpile-binop context ,ir-class ,jtype))))
               opcodes)))

(%define-binop-transpilers
  (:DMUL 'ir-dmul :DOUBLE)
  (:DADD 'ir-dadd :DOUBLE)
  (:FADD 'ir-fadd :FLOAT)
  (:FDIV 'ir-fdiv :FLOAT)
  (:FMUL 'ir-fmul :FLOAT)
  (:FSUB 'ir-fsub :FLOAT)
  (:IADD 'ir-iadd :INTEGER)
  (:IAND 'ir-iand :INTEGER)
  (:IDIV 'ir-idiv :INTEGER)
  (:IMUL 'ir-imul :INTEGER)
  (:IOR 'ir-ior :INTEGER)
  (:IREM 'ir-irem :INTEGER)
  (:ISHL 'ir-ishl :INTEGER)
  (:ISHR 'ir-ishr :INTEGER)
  (:ISUB 'ir-isub :INTEGER)
  (:IUSHR 'ir-iushr :INTEGER)
  (:IXOR 'ir-ixor :INTEGER)
  (:LADD 'ir-ladd :LONG)
  (:LAND 'ir-land :LONG)
  (:LCMP 'ir-lcmp :INTEGER)
  (:LDIV 'ir-idiv :LONG)
  (:LMUL 'ir-lmul :LONG)
  (:LSHL 'ir-ishl :INTEGER)
  (:LSHR 'ir-ishr :INTEGER)
  (:LSUB 'ir-lsub :LONG))

(define-bytecode-transpiler :BIPUSH (context code)
  (with-slots (pc) context
    (let* ((pc-start pc)
           (var (make-stack-variable context pc-start :INTEGER))
           (byte (aref code (incf pc))))
      (incf pc)
      (push pc (aref (next-insn-list context) pc-start))
      (push var (stack context))
      (list (make-instance 'ir-assign
                           :address pc-start
                           :lvalue var
                           :rvalue (make-instance 'ir-int-literal :address pc-start :value byte))))))

(define-bytecode-transpiler-TODO :DDIV (context code)
  (declare-IGNORE (ignore code))
  (with-slots (pc) context
    (let ((pc-start pc))
      (incf pc)
      (push pc (aref (next-insn-list context) pc-start))
      (list (make-instance 'ir-div
                           :address pc-start)))))

(define-bytecode-transpiler-TODO :DLOAD_2 (context code)
  (declare-IGNORE (ignore code))
  (with-slots (pc) context
    (let ((pc-start pc))
      (incf pc)
      (push pc (aref (next-insn-list context) pc-start))
      (list (make-instance 'ir-push
                           :address pc-start
                           :value (make-instance 'ir-local-variable
                                                 :address pc-start
                                                 :index 2))))))

(define-bytecode-transpiler-TODO :DSTORE_2 (context code)
  (declare-IGNORE (ignore code))
  (with-slots (pc) context
    (let ((pc-start pc))
      (incf pc)
      (push pc (aref (next-insn-list context) pc-start))
      (list (make-instance 'ir-store
                           :address pc-start
                           :target (make-instance 'ir-local-variable
                                                  :address pc-start
                                                  :index 2))))))

(define-bytecode-transpiler-TODO :DSUB (context code)
  (declare-IGNORE (ignore code))
  (with-slots (pc) context
    (let ((pc-start pc))
      (incf pc)
      (push pc (aref (next-insn-list context) pc-start))
      (list (make-instance 'ir-dsub
                           :address pc-start)))))

(define-bytecode-transpiler :DUP (context code)
  (declare (ignore code))
  (with-slots (pc) context
    (let ((pc-start pc))
      (incf pc)
      (push pc (aref (next-insn-list context) pc-start))
      (let* ((value (pop (stack context))))
        (push value (stack context))
        (push value (stack context)))
      (list (make-instance 'ir-nop :address pc-start)))))

(define-bytecode-transpiler :DUP2 (context code)
  (declare (ignore code))
  (if (find (var-type (car (stack context))) '(:DOUBLE :LONG))
      (:DUP context code)
      (with-slots (pc) context
        (let ((pc-start pc))
          (incf pc)
          (push pc (aref (next-insn-list context) pc-start))
          (let* ((value1 (pop (stack context)))
                 (value2 (pop (stack context))))
            (push value2 (stack context))
            (push value1 (stack context))
            (push value2 (stack context))
            (push value1 (stack context)))
          (list (make-instance 'ir-nop :address pc-start))))))

(define-bytecode-transpiler :DUP_X1 (context code)
  (declare (ignore code))
  (with-slots (pc) context
    (let ((pc-start pc))
      (incf pc)
      (push pc (aref (next-insn-list context) pc-start))
      (let* ((value1 (pop (stack context)))
             (value2 (pop (stack context))))
        (push value1 (stack context))
        (push value2 (stack context))
        (push value1 (stack context)))
      (list (make-instance 'ir-nop :address pc-start)))))

(define-bytecode-transpiler :GETSTATIC (context code)
  (with-slots (pc class is-clinit-p) context
    (let ((context-class class)
          (pc-start pc))
      (with-slots (constant-pool) context-class
        (let* ((index (+ (* (aref code (incf pc)) 256)
                         (aref code (incf pc)))))
          (multiple-value-bind (fieldname class)
              (emit (aref constant-pool index) constant-pool)
            (incf pc)
            (push pc (aref (next-insn-list context) pc-start))
            (let* ((var (make-stack-variable context pc-start (get-stack-type-from-descriptor (emit-type (aref constant-pool index) constant-pool))))
                   (code (list (make-instance 'ir-assign
                                              :address (if (and is-clinit-p (equal (ir-class-class class) context-class)) pc-start (+ pc-start 0.1))
                                              :lvalue var
                                              :rvalue (make-instance 'ir-static-member
                                                                     :address pc-start
                                                                     :class class
                                                                     :member-name fieldname)))))
              (push var (stack context))
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

(define-bytecode-transpiler :GOTO (context code)
  (with-slots (pc class) context
    (let ((pc-start pc))
      (with-slots (constant-pool) class
        (let* ((offset (unsigned-to-signed (+ (* (aref code (incf pc)) 256)
                                              (aref code (incf pc))))))
          (incf pc)
          (push (+ pc-start offset) (aref (next-insn-list context) pc-start))
          (let ((code (list (make-instance 'ir-goto
                                           :address pc-start
                                           :offset (+ pc-start offset)))))
            (%record-stack-state (+ pc-start offset) context)
            (setf (stack context) (list (make-instance '<stack-bottom-marker>)))
            code))))))

(define-bytecode-transpiler :FCMPG (context code)
  (declare (ignore code))
  (with-slots (pc) context
    (let* ((pc-start pc)
           (var (make-stack-variable context pc-start :INTEGER)))
      (incf pc)
      (push pc (aref (next-insn-list context) pc-start))
      (let ((code (list (make-instance 'ir-assign
                                       :address pc-start
                                       :lvalue var
                                       :rvalue (make-instance 'ir-fcmpg
                                                              :value2 (pop (stack context))
                                                              :value1 (pop (stack context))
                                                              :address pc-start)))))
        (push var (stack context))
        code))))

(define-bytecode-transpiler :FCMPL (context code)
  (declare (ignore code))
  (with-slots (pc) context
    (let* ((pc-start pc)
           (var (make-stack-variable context pc-start :INTEGER)))
      (incf pc)
      (push pc (aref (next-insn-list context) pc-start))
      (let ((code (list (make-instance 'ir-assign
                                       :address pc-start
                                       :lvalue var
                                       :rvalue (make-instance 'ir-fcmpl
                                                              :value2 (pop (stack context))
                                                              :value1 (pop (stack context))
                                                              :address pc-start)))))
        (push var (stack context))
        code))))

(defun %transpile-fconst-x (context value)
  (with-slots (pc stack) context
    (let* ((pc-start pc)
           (var (make-stack-variable context pc-start :FLOAT)))
      (incf pc)
      (push pc (aref (next-insn-list context) pc-start))
      (push var (stack context))
      (list (make-instance 'ir-assign
                           :address pc-start
                           :lvalue var
                           :rvalue (make-instance 'ir-float-literal
                                                  :address pc-start
                                                  :value value))))))

(define-bytecode-transpiler :FCONST_0 (context code)
  (declare (ignore code))
  (%transpile-fconst-x context 0.0))

(define-bytecode-transpiler :FCONST_1 (context code)
  (declare (ignore code))
  (%transpile-fconst-x context 1.0))

(defun %transpile-dconst-x (context value)
  (with-slots (pc stack) context
    (let* ((pc-start pc)
           (var (make-stack-variable context pc-start :DOUBLE)))
      (incf pc)
      (push pc (aref (next-insn-list context) pc-start))
      (push var (stack context))
      (list (make-instance 'ir-assign
                           :address pc-start
                           :lvalue var
                           :rvalue (make-instance 'ir-double-literal
                                                  :address pc-start
                                                  :value value))))))

(define-bytecode-transpiler :DCONST_0 (context code)
  (declare (ignore code))
  (%transpile-fconst-x context 0.0))

(define-bytecode-transpiler :DCONST_1 (context code)
  (declare (ignore code))
  (%transpile-fconst-x context 1.0))

(define-bytecode-transpiler-TODO :INEG (context code)
  (declare-IGNORE (ignore code))
  (with-slots (pc) context
    (let ((pc-start pc))
      (incf pc)
      (push pc (aref (next-insn-list context) pc-start))
      (list (make-instance 'ir-ineg :address pc-start)))))

(defclass/std <stack-variable> (ir-node)
  ((var-numbers)
   (var-type)))

(defclass/std <stack-bottom-marker> (<stack-variable>)
  ())

(defmethod initialize-instance ((marker <stack-bottom-marker>) &key)
  (setf (slot-value marker 'var-numbers) (list +stack-bottom-address+))
  (setf (slot-value marker 'var-type) :VOID)
  (setf (slot-value marker 'address) +stack-bottom-address+))

(defmethod print-object ((sv <stack-bottom-marker>) out)
  (format out "<<"))

(defmethod print-object ((sv <stack-variable>) out)
  (print-unreadable-object (sv out :type t)
    (format out "s{~{~A~^,~}}[~A]" (sort (slot-value sv 'var-numbers) #'<) (slot-value sv 'var-type))))

(defun make-stack-variable (context pc-start type)
  (let ((var (make-instance '<stack-variable>
                            :address pc-start
                            :var-numbers (list (incf (svcount context)))
                            :var-type type)))
    (push var (stack-variables context))
    var))

(defun %transpile-iconst-x (context value)
  (with-slots (pc stack) context
    (let* ((pc-start pc)
           (var (make-stack-variable context pc-start :INTEGER)))
      (incf pc)
      (push pc (aref (next-insn-list context) pc-start))
      (push var (stack context))
      (list (make-instance 'ir-assign
                           :address pc-start
                           :lvalue var
                           :rvalue (make-instance 'ir-int-literal
                                                  :address pc-start
                                                  :value value))))))

(define-bytecode-transpiler :ICONST_M1 (context code)
  (declare (ignore code))
  (%transpile-iconst-x context -1))

(define-bytecode-transpiler :ICONST_0 (context code)
  (declare (ignore code))
  (%transpile-iconst-x context 0))

(define-bytecode-transpiler :ICONST_1 (context code)
  (declare (ignore code))
  (%transpile-iconst-x context 1))

(define-bytecode-transpiler :ICONST_2 (context code)
  (declare (ignore code))
  (%transpile-iconst-x context 2))

(define-bytecode-transpiler :ICONST_3 (context code)
  (declare (ignore code))
  (%transpile-iconst-x context 3))

(define-bytecode-transpiler :ICONST_4 (context code)
  (declare (ignore code))
  (%transpile-iconst-x context 4))

(define-bytecode-transpiler :ICONST_5 (context code)
  (declare (ignore code))
  (%transpile-iconst-x context 5))

(defun %transpile-compare-branch (context code ir-class)
  (with-slots (pc class) context
    (let ((pc-start pc))
      (with-slots (constant-pool) class
        (let* ((offset (unsigned-to-signed (+ (* (aref code (incf pc)) 256)
                                              (aref code (incf pc)))))
               (value2 (pop (stack context)))
               (value1 (pop (stack context))))
          (incf pc)
          (push pc (aref (next-insn-list context) pc-start))
          (push (+ pc-start offset) (aref (next-insn-list context) pc-start))
          (let ((code (list (make-instance ir-class
                                           :address pc-start
                                           :value1 value1
                                           :value2 value2
                                           :offset (+ pc-start offset)))))
            (%record-stack-state (+ pc-start offset) context)
            code))))))

(define-bytecode-transpiler :IF_ACMPNE (context code)
  (%transpile-compare-branch context code 'ir-if-acmpne))

(define-bytecode-transpiler :IF_ACMPEQ (context code)
  (%transpile-compare-branch context code 'ir-if-acmpeq))

(define-bytecode-transpiler :IF_ICMPEQ (context code)
  (%transpile-compare-branch context code 'ir-if-icmpeq))

(define-bytecode-transpiler :IF_ICMPNE (context code)
  (%transpile-compare-branch context code 'ir-if-icmpne))

(define-bytecode-transpiler :IF_ICMPLT (context code)
  (%transpile-compare-branch context code 'ir-if-icmplt))

(define-bytecode-transpiler :IF_ICMPGT (context code)
  (%transpile-compare-branch context code 'ir-if-icmpgt))

(define-bytecode-transpiler :IF_ICMPGE (context code)
  (%transpile-compare-branch context code 'ir-if-icmpge))

(define-bytecode-transpiler :IF_ICMPLE (context code)
  (%transpile-compare-branch context code 'ir-if-icmple))

(defun %record-stack-state (pc context)
  (push (stack context) (gethash pc (stack-state-table context))))

(defmacro %define-if<cond>-transpilers (&rest opcodes)
  `(progn
     ,@(mapcar (lambda (opcode)
                 (let ((name (car opcode))
                       (ir-class (cadr opcode)))
                   `(define-bytecode-transpiler ,name (context code)
                      (with-slots (pc class) context
                        (let ((pc-start pc))
                          (with-slots (constant-pool) class
                            (let* ((offset (unsigned-to-signed
                                             (+ (* (aref code (incf pc)) 256)
                                                (aref code (incf pc))))))
                              (incf pc)
                              (push pc (aref (next-insn-list context) pc-start))
                              (push (+ pc-start offset) (aref (next-insn-list context) pc-start))
                              (let ((code (list (make-instance ',ir-class
                                                               :address pc-start
                                                               :offset (+ pc-start offset)
                                                               :value (pop (stack context))))))
                                (%record-stack-state (+ pc-start offset) context)
                                code))))))))
               opcodes)))

(%define-if<cond>-transpilers
 (:IFEQ ir-ifeq)
 (:IFNE ir-ifne)
 (:IFGE ir-ifge)
 (:IFGT ir-ifgt)
 (:IFLE ir-ifle)
 (:IFLT ir-iflt)
 (:IFNULL ir-ifnull)
 (:IFNONNULL ir-ifnonnull))

(defun %transpile-load (jtype context code)
  (with-slots (pc) context
    (let* ((pc-start pc)
           (index (aref code (incf pc)))
           (var (make-stack-variable context pc-start jtype)))
      (incf pc)
      (push pc (aref (next-insn-list context) pc-start))
      (push var (stack context))
      (list (make-instance 'ir-assign
                           :address pc-start
                           :lvalue var
                           :rvalue (make-instance 'ir-local-variable
                                                  :address pc-start
                                                  :index index))))))

(defmacro %define-load-transpilers (&rest opcodes)
  `(progn
     ,@(mapcar (lambda (opcode)
                 (let ((name (car opcode))
                       (jtype (cadr opcode)))
                   `(define-bytecode-transpiler ,name (context code)
                      (%transpile-load ,jtype context code))))
               opcodes)))

(%define-load-transpilers
 (:ALOAD :REFERENCE)
 (:ILOAD :INTEGER)
 (:LLOAD :LONG)
 (:FLOAD :FLOAT)
 (:DLOAD :DOUBLE))

(defun %transpile-iload-x (context index)
  (with-slots (pc) context
    (let* ((pc-start pc)
           (var (make-stack-variable context pc-start :INTEGER)))
      (incf pc)
      (push pc (aref (next-insn-list context) pc-start))
      (push var (stack context))
      (list (make-instance 'ir-assign
                           :address pc-start
                           :lvalue var
                           :rvalue (make-instance 'ir-local-variable
                                                  :address pc-start
                                                  :index index))))))


(define-bytecode-transpiler :ILOAD_0 (context code)
  (declare (ignore code))
  (%transpile-iload-x context 0))

(define-bytecode-transpiler :ILOAD_1 (context code)
  (declare (ignore code))
  (%transpile-iload-x context 1))

(define-bytecode-transpiler :ILOAD_2 (context code)
  (declare (ignore code))
  (%transpile-iload-x context 2))

(define-bytecode-transpiler :ILOAD_3 (context code)
  (declare (ignore code))
  (%transpile-iload-x context 3))

(defun %transpile-lload-x (context index)
  (with-slots (pc) context
    (let* ((pc-start pc)
           (var (make-stack-variable context pc-start :LONG)))
      (incf pc)
      (push pc (aref (next-insn-list context) pc-start))
      (push var (stack context))
      (list (make-instance 'ir-assign
                           :address pc-start
                           :lvalue var
                           :rvalue (make-instance 'ir-local-variable
                                                  :address pc-start
                                                  :index index))))))


(define-bytecode-transpiler :LLOAD_0 (context code)
  (declare (ignore code))
  (%transpile-lload-x context 0))

(define-bytecode-transpiler :LLOAD_1 (context code)
  (declare (ignore code))
  (%transpile-lload-x context 1))

(define-bytecode-transpiler :LLOAD_2 (context code)
  (declare (ignore code))
  (%transpile-lload-x context 2))

(define-bytecode-transpiler :LLOAD_3 (context code)
  (declare (ignore code))
  (%transpile-lload-x context 3))

(defun %transpile-fload-x (context index)
  (with-slots (pc) context
    (let* ((pc-start pc)
           (var (make-stack-variable context pc-start :FLOAT)))
      (incf pc)
      (push pc (aref (next-insn-list context) pc-start))
      (push var (stack context))
      (list (make-instance 'ir-assign
                           :address pc-start
                           :lvalue var
                           :rvalue (make-instance 'ir-local-variable
                                                  :address pc-start
                                                  :index index))))))


(define-bytecode-transpiler :FLOAD_0 (context code)
  (declare (ignore code))
  (%transpile-fload-x context 0))

(define-bytecode-transpiler :FLOAD_1 (context code)
  (declare (ignore code))
  (%transpile-fload-x context 1))

(define-bytecode-transpiler :FLOAD_2 (context code)
  (declare (ignore code))
  (%transpile-fload-x context 2))

(define-bytecode-transpiler :FLOAD_3 (context code)
  (declare (ignore code))
  (%transpile-fload-x context 3))

(define-bytecode-transpiler :INSTANCEOF (context code)
  (with-slots (pc class) context
    (let ((pc-start pc))
      (with-slots (constant-pool) class
        (let* ((index (+ (* (aref code (incf pc)) 256)
                         (aref code (incf pc))))
               (class (aref constant-pool index))
               (var (make-stack-variable context pc-start :INTEGER)))
          (incf pc)
          (push pc (aref (next-insn-list context) pc-start))
          (let ((code (list (make-instance 'ir-assign
                                           :address pc-start
                                           :lvalue var
                                           :rvalue (make-instance 'ir-instanceof
                                                                  :address pc-start
                                                                  :objref (pop (stack context))
                                                                  :class (emit class constant-pool))))))
            (push var (stack context))
            code))))))

(defun %transpile-virtual-call (context code &optional (is-interface-call? nil))
  (with-slots (pc class) context
    (let ((pc-start pc))
      (with-slots (constant-pool) class
        (let* ((index (+ (* (aref code (incf pc)) 256)
                         (aref code (incf pc))))
               (method-reference (aref constant-pool index)))
          (incf pc)
          (when is-interface-call?
            (incf pc)
            (incf pc))
          (push pc (aref (next-insn-list context) pc-start))
          (let* ((descriptor
                   (slot-value (aref constant-pool
                                     (slot-value
                                      (aref constant-pool
                                            (slot-value method-reference
                                                        'method-descriptor-index))
                                      'type-descriptor-index))
                               'value))
                 (parameter-count (1+ (count-parameters descriptor))))
            (list (let* ((return-type (get-return-type (emit method-reference constant-pool)))
                         (call (make-instance 'ir-call-virtual-method
                                              :address pc-start
                                              :return-type (get-return-type (emit method-reference constant-pool))
                                              :method-name (lispize-method-name (emit method-reference constant-pool))
                                              :args (pop-args parameter-count context))))
                    (if (eq return-type :VOID)
                        call
                        (let ((var (make-stack-variable context pc-start return-type)))
                          (push var (stack context))
                          (make-instance 'ir-assign
                                         :address pc-start
                                         :lvalue var
                                         :rvalue call)))))))))))

(define-bytecode-transpiler :INVOKEINTERFACE (context code)
  (%transpile-virtual-call context code t))

(define-bytecode-transpiler :INVOKEVIRTUAL (context code)
  (%transpile-virtual-call context code))

(define-bytecode-transpiler :INVOKESPECIAL (context code)
  (with-slots (pc class) context
    (let ((pc-start pc))
      (with-slots (constant-pool) class
        (let* ((index (+ (* (aref code (incf pc)) 256)
                         (aref code (incf pc))))
               (method-reference (aref constant-pool index)))
          (incf pc)
          (push pc (aref (next-insn-list context) pc-start))
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
            (list (let* ((return-type (get-return-type (emit method-reference constant-pool)))
                         (call (make-instance 'ir-call-special-method
                                              :address pc-start
                                              :class class
                                              :return-type return-type
                                              :method-name (lispize-method-name (emit method-reference constant-pool))
                                              :args (pop-args parameter-count context))))
                    (if (eq return-type :VOID)
                        call
                        (let ((var (make-stack-variable context pc-start return-type)))
                          (push var (stack context))
                          (make-instance 'ir-assign
                                         :address pc-start
                                         :lvalue var
                                         :rvalue call)))))))))))

(define-bytecode-transpiler :INVOKESTATIC (context code)
  (with-slots (pc class is-clinit-p) context
    (let ((context-class class)
          (pc-start pc))
      (with-slots (constant-pool) class
        (let* ((index (+ (* (aref code (incf pc)) 256)
                         (aref code (incf pc))))
               (method-reference (aref constant-pool index)))
          (incf pc)
          (push pc (aref (next-insn-list context) pc-start))
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
            (let* ((return-type (get-return-type (emit method-reference constant-pool)))
                   (call (make-instance 'ir-call-static-method
                                        :address (if (and is-clinit-p (equal callee-class (name context-class))) pc-start (+ pc-start 0.1))
                                        :class callee-class
                                        :return-type return-type
                                        :method-name (lispize-method-name (emit method-reference constant-pool))
                                        :args (pop-args parameter-count context))))
              (if (not (eq return-type :VOID))
                  (let ((var (make-stack-variable context pc-start return-type)))
                    (push var (stack context))
                    (setf call (make-instance 'ir-assign
                                              :address (if (and is-clinit-p (equal callee-class (name context-class))) pc-start (+ pc-start 0.1))
                                              :lvalue var
                                              :rvalue call))))
              (if (and is-clinit-p (equal callee-class (name context-class)))
                  (list call)
                  (list (make-instance 'ir-clinit
                                       :address pc-start
                                       :class (make-instance 'ir-class :class (classload callee-class)))
                        call)))))))))

(define-bytecode-transpiler :IRETURN (context code)
  (declare (ignore code))
  (with-slots (pc) context
    (let ((pc-start pc))
      (incf pc)
      (list (make-instance 'ir-return-value
                           :fn-name (slot-value context 'fn-name)
                           :address pc-start
                           :value (pop (stack context)))))))

(define-bytecode-transpiler :LRETURN (context code)
  (:IRETURN context code))

(define-bytecode-transpiler-TODO :LOR (context code)
  (declare-IGNORE (ignore code))
  (with-slots (pc) context
    (let ((pc-start pc))
      (incf pc)
      (push pc (aref (next-insn-list context) pc-start))
      (list (make-instance 'ir-lor
                           :address pc-start)))))

(define-bytecode-transpiler :ARETURN (context code)
  (:IRETURN context code))

(define-bytecode-transpiler :DRETURN (context code)
  (:IRETURN context code))

(define-bytecode-transpiler :FRETURN (context code)
  (:IRETURN context code))

(define-bytecode-transpiler :LDC (context code)
  (with-slots (pc class stack) context
    (let* ((pc-start pc))
      (with-slots (constant-pool) class
        (let* ((index (aref code (incf pc)))
               (var (make-stack-variable context pc-start (get-stack-jtype (aref constant-pool index)))))
          (incf pc)
          (push pc (aref (next-insn-list context) pc-start))
          (push var (stack context))
          (list (make-instance 'ir-assign
                               :address pc-start
                               :lvalue var
                               :rvalue (emit (aref constant-pool index) constant-pool))))))))

(define-bytecode-transpiler :LDC_W (context code)
  (with-slots (pc class) context
    (let* ((pc-start pc))
      (with-slots (constant-pool) class
				(let* ((index (+ (* (aref code (incf pc)) 256)
                         (aref code (incf pc))))
               (var (make-stack-variable context pc-start (get-stack-jtype (aref constant-pool index)))))
          (incf pc)
          (push pc (aref (next-insn-list context) pc-start))
          (let ((code (list (make-instance 'ir-assign
                                           :address pc-start
                                           :lvalue var
                                           :rvalue (emit (aref constant-pool index) constant-pool)))))
            (push var (stack context))
            code))))))

(define-bytecode-transpiler :LDC2_W (context code)
  (:LDC_W context code))

(defun %transpile-lconst-x (context value)
  (with-slots (pc stack) context
    (let* ((pc-start pc)
           (var (make-stack-variable context pc-start :LONG)))
      (incf pc)
      (push pc (aref (next-insn-list context) pc-start))
      (push var (stack context))
      (list (make-instance 'ir-assign
                           :address pc-start
                           :lvalue var
                           :rvalue (make-instance 'ir-long-literal
                                                  :address pc-start
                                                  :value value))))))

(define-bytecode-transpiler :LCONST_0 (context code)
  (declare (ignore code))
  (%transpile-iconst-x context 0))

(define-bytecode-transpiler :LCONST_1 (context code)
  (declare (ignore code))
  (%transpile-iconst-x context 1))

(define-bytecode-transpiler-TODO :LUSHR (context code)
  (declare-IGNORE (ignore code))
  (with-slots (pc) context
    (let ((pc-start pc))
      (incf pc)
      (push pc (aref (next-insn-list context) pc-start))
      (list (make-instance 'ir-lushr
                           :address pc-start)))))

(define-bytecode-transpiler :MONITORENTER (context code)
  (declare (ignore code))
  (with-slots (pc) context
    (let ((pc-start pc))
      (incf pc)
      (push pc (aref (next-insn-list context) pc-start))
      (list (make-instance 'ir-monitorenter :address pc-start :objref (pop (stack context)))))))

(define-bytecode-transpiler :MONITOREXIT (context code)
  (declare (ignore code))
  (with-slots (pc) context
    (let ((pc-start pc))
      (incf pc)
      (push pc (aref (next-insn-list context) pc-start))
      (list (make-instance 'ir-monitorexit :address pc-start :objref (pop (stack context)))))))

(define-bytecode-transpiler :NEW (context code)
  (with-slots (pc class) context
    (let ((pc-start pc))
      (with-slots (constant-pool) class
        (let* ((index (+ (* (aref code (incf pc)) 256)
                         (aref code (incf pc))))
               (class (emit (aref constant-pool index) constant-pool))
               (var (make-stack-variable context pc-start :REFERENCE)))
          (incf pc)
          (push pc (aref (next-insn-list context) pc-start))
          (push var (stack context))
          (list (make-instance 'ir-assign
                               :address pc-start
                               :lvalue var
                               :rvalue (make-instance 'ir-new :address pc-start :class class))))))))

(define-bytecode-transpiler-TODO :NOP (context code)
  (with-slots (pc class) context
    (let ((pc-start pc))
      (incf pc)
      (push pc (aref (next-insn-list context) pc-start))
      (list (make-instance 'ir-nop :address pc-start)))))

(define-bytecode-transpiler :ANEWARRAY (context code)
  (with-slots (pc class) context
    (let ((pc-start pc))
      (with-slots (constant-pool) class
        (let* ((index (+ (* (aref code (incf pc)) 256)
                         (aref code (incf pc))))
               (class (emit (aref constant-pool index) constant-pool))
               (var (make-stack-variable context pc-start :ARRAY))
               (size (pop (stack context))))
          (incf pc)
          (push pc (aref (next-insn-list context) pc-start))
          (push var (stack context))
          (list (make-instance 'ir-assign
                               :address pc-start
                               :lvalue var
                               :rvalue (make-instance 'ir-new-array :address pc-start :class class :size size))))))))

(define-bytecode-transpiler :NEWARRAY (context code)
  (with-slots (pc class) context
    (let* ((pc-start pc)
           (var (make-stack-variable context pc-start :ARRAY))
           (size (pop (stack context))))
      (let ((atype (aref code (incf pc))))
        (incf pc)
        (push pc (aref (next-insn-list context) pc-start))
        ;; FIXME: just throw away the type?
        atype
        (push var (stack context))
        (list (make-instance 'ir-assign
                             :address pc-start
                             :lvalue var
                             :rvalue (make-instance 'ir-new-array :address pc-start :class nil :size size)))))))

(define-bytecode-transpiler :POP (context code)
  (declare (ignore code))
  (with-slots (pc) context
    (incf pc)
    (push pc (aref (next-insn-list context) (1- pc)))
    (pop (stack context))
    (list (make-instance 'ir-nop :address (1- pc)))))

(define-bytecode-transpiler :GETFIELD (context code)
  (with-slots (pc class) context
    (let ((pc-start pc))
      (with-slots (constant-pool) class
        (let* ((index (+ (* (aref code (incf pc)) 256)
                         (aref code (incf pc))))
               (var (make-stack-variable context pc-start (get-stack-type-from-descriptor (emit-type (aref constant-pool index) constant-pool)))))
          (multiple-value-bind (fieldname fieldclass)
              (emit (aref constant-pool index) constant-pool)
            (incf pc)
            (push pc (aref (next-insn-list context) pc-start))
            (let ((code (list (make-instance 'ir-assign
                                             :address pc-start
                                             :lvalue var
                                             :rvalue (make-instance 'ir-member
                                                                    :address pc-start
                                                                    :objref (pop (stack context))
                                                                    :member-name fieldname)))))
              (push var (stack context))
              code)))))))

(define-bytecode-transpiler :PUTFIELD (context code)
  (with-slots (pc class) context
    (let ((pc-start pc))
      (with-slots (constant-pool) class
        (let* ((index (+ (* (aref code (incf pc)) 256)
                         (aref code (incf pc)))))
          (multiple-value-bind (fieldname)
              (emit (aref constant-pool index) constant-pool)
            (incf pc)
            (push pc (aref (next-insn-list context) pc-start))
            (list (make-instance 'ir-assign
                                 :address pc-start
                                 :rvalue (pop (stack context))
                                 :lvalue (make-instance 'ir-member
                                                        :address pc-start
                                                        :objref (pop (stack context))
                                                        :member-name fieldname)))))))))

(define-bytecode-transpiler :PUTSTATIC (context code)
  (with-slots (pc class is-clinit-p) context
    (let ((context-class class)
          (pc-start pc))
      (with-slots (constant-pool) context-class
        (let* ((index (+ (* (aref code (incf pc)) 256)
                         (aref code (incf pc)))))
          (multiple-value-bind (fieldname class)
              (emit (aref constant-pool index) constant-pool)
            (incf pc)
            (push pc (aref (next-insn-list context) pc-start))
            (let ((code (list (make-instance 'ir-assign
                                             :address (if (and is-clinit-p (equal (ir-class-class class) context-class)) pc-start (+ pc-start 0.1))
                                             :rvalue (pop (stack context))
                                             :lvalue (make-instance 'ir-static-member
                                                                    :address pc-start
                                                                    :class class
                                                                    :member-name fieldname)))))
              (if (and is-clinit-p (equal (ir-class-class class) context-class))
                  code
                  (cons (make-instance 'ir-clinit
                                       :address pc-start
                                       :class class)
                        code)))))))))

(define-bytecode-transpiler :RETURN (context code)
  (declare (ignore code))
  (with-slots (pc) context
    (let ((pc-start pc))
      (incf pc)
      (list (make-instance 'ir-return
                           :address pc-start)))))

(define-bytecode-transpiler :SIPUSH (context code)
  (with-slots (pc) context
    (let* ((pc-start pc)
           (var (make-stack-variable context pc-start :INTEGER))
           (short (unsigned-to-signed (+ (* (aref code (incf pc)) 256)
                                         (* (aref code (incf pc)))))))
      (incf pc)
      (push pc (aref (next-insn-list context) pc-start))
      (push var (stack context))
      (list (make-instance 'ir-assign
                           :address pc-start
                           :lvalue var
                           :rvalue (make-instance 'ir-int-literal :address pc-start :value short))))))

(define-bytecode-transpiler :TABLESWITCH (context code)
  (with-slots (pc class) context
    (let ((pc-start pc))
      (with-slots (constant-pool) class
        ;; Align PC to the next 4-byte boundary
        (let* ((padding (mod pc 4)))
          (incf pc (1- (- 4 padding))))

        ;; Read default offset, low value, and high value
        (let* ((default-offset (unsigned-to-signed (+ (* (aref code (incf pc)) 16777216)
                                                      (* (aref code (incf pc)) 65536)
                                                      (* (aref code (incf pc)) 256)
                                                      (aref code (incf pc)))))
               (low (unsigned-to-signed (+ (* (aref code (incf pc)) 16777216)
                                           (* (aref code (incf pc)) 65536)
                                           (* (aref code (incf pc)) 256)
                                           (aref code (incf pc)))))
               (high (unsigned-to-signed (+ (* (aref code (incf pc)) 16777216)
                                            (* (aref code (incf pc)) 65536)
                                            (* (aref code (incf pc)) 256)
                                            (aref code (incf pc)))))
               (num-cases (1+ (- high low)))
               (jump-offsets (loop repeat num-cases
                                   collect (unsigned-to-signed (+ (* (aref code (incf pc)) 16777216)
                                                                  (* (aref code (incf pc)) 65536)
                                                                  (* (aref code (incf pc)) 256)
                                                                  (aref code (incf pc)))))))
          ;; Generate intermediate representation
          (let ((jump-offsets (mapcar (lambda (offset)
                                         (+ pc-start offset))
                                       jump-offsets)))

            (let ((code (list (make-instance 'ir-tableswitch
                                             :index (pop (stack context))
                                             :address pc-start
                                             :default-offset (+ pc-start default-offset)
                                             :low low
                                             :high high
                                             :jump-offsets jump-offsets))))

              (dolist (offset jump-offsets)
                (push offset (aref (next-insn-list context) pc-start)))
              (push (+ pc-start default-offset) (aref (next-insn-list context) pc-start))
              ;; FIXME: why is this required??????????
              (incf pc)
              ; (format t "~&TABLESWITCH TARGETS = ~A~%" (aref (next-insn-list context) pc-start))

              ;; Record stack state for each jump destination
              (dolist (offset (cons (+ pc-start default-offset) jump-offsets))
                (%record-stack-state offset context))
              ;; Reset the stack for the next instruction
              (setf (stack context) (list (make-instance '<stack-bottom-marker>)))
              code)))))))

(defun merge-stack-variables (stack-var1 stack-var2)
  "Merge the var-numbers of two stack-variable objects."
  (let ((unums (union (slot-value stack-var1 'var-numbers)
                      (slot-value stack-var2 'var-numbers) :test #'eql)))
    (setf (slot-value stack-var1 'var-numbers) unums)
    (setf (slot-value stack-var2 'var-numbers) unums)
    stack-var1))

(defun merge-stacks (list1 list2)
  "Merge two lists of stack-variable objects into one."
  (loop for sv1 in list1
        for sv2 in list2
        collect (merge-stack-variables sv1 sv2)))
