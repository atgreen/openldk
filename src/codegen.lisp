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

(defun gen-push-item (item)
  (if *debug-stack*
      (list 'let (list (list 'item item))
            (list 'format t "; --- push ~A to ~A~%" 'item 'stack)
            (list 'push 'item 'stack))
      (list 'push item 'stack)))

(defun gen-pop-item ()
  (if *debug-stack*
      (list 'progn
            (list 'format t "; -- pop from ~A~%" 'stack)
            (list 'pop 'stack))
      (list 'pop 'stack)))

(defun trace-insn (insn code)
  (if *debug-trace*
      (list 'progn
            (list 'format t (format nil "~&; [~A]~%" (address insn)))
            code)
      code))

(defun gen-peek-item ()
  (list 'car 'stack))

(defmethod codegen ((insn ssa-literal) &optional (stop-block nil))
  (declare (ignore stop-block))
  (slot-value insn 'value))

(defmethod codegen ((insn ssa-string-literal) &optional (stop-block nil))
  (declare (ignore stop-block))
  (let ((s (make-instance '|java/lang/String|)))
    (setf (slot-value s '|value|) (slot-value insn 'value))
    s))

(defmethod codegen ((insn ssa-aaload) &optional (stop-block nil))
  (declare (ignore stop-block))
  (flag-stack-usage *context*)
  (trace-insn
   insn
   (list 'let (list (list 'index (gen-pop-item))
                    (list 'arrayref (gen-pop-item)))
         (gen-push-item (list 'aref 'arrayref 'index)))))

(defmethod codegen ((insn ssa-aastore) &optional (stop-block nil))
  (declare (ignore stop-block))
  (flag-stack-usage *context*)
  (trace-insn
   insn
   (list 'let (list (list 'value (gen-pop-item))
                    (list 'index (gen-pop-item))
                    (list 'arrayref (gen-pop-item)))
         (list 'setf (list 'aref 'arrayref 'index) 'value))))

(defmethod codegen ((insn ssa-add) &optional (stop-block nil))
  ;; FIXME -- need iadd to mask lower 32-bits
  (declare (ignore stop-block))
  (flag-stack-usage *context*)
  (trace-insn
   insn
   (gen-push-item (list '+ (gen-pop-item) (gen-pop-item)))))

(defmethod codegen ((insn ssa-fadd) &optional (stop-block nil))
  ;; FIXME -- handle NaN cases
  (declare (ignore stop-block))
  (flag-stack-usage *context*)
  (trace-insn
   insn
   (gen-push-item (list '+ (gen-pop-item) (gen-pop-item)))))

(defmethod codegen ((insn ssa-imul) &optional (stop-block nil))
  (declare (ignore stop-block))
  (trace-insn
   insn
   (gen-push-item (list 'logand (list '* (gen-pop-item) (gen-pop-item)) #xFFFFFFFF))))

(defmethod codegen ((insn ssa-iand) &optional (stop-block nil))
  (declare (ignore stop-block))
  (gen-push-item (list 'logand (gen-pop-item) (gen-pop-item))))

(defmethod codegen ((insn ssa-land) &optional (stop-block nil))
  (declare (ignore stop-block))
  (gen-push-item (list 'logand (gen-pop-item) (gen-pop-item))))

(defmethod codegen ((insn ssa-ior) &optional (stop-block nil))
  (declare (ignore stop-block))
  (gen-push-item (list 'logior (gen-pop-item) (gen-pop-item))))

(defmethod codegen ((insn ssa-ixor) &optional (stop-block nil))
  (declare (ignore stop-block))
  (gen-push-item (list 'logxor (gen-pop-item) (gen-pop-item))))

(defmethod codegen ((insn ssa-lor) &optional (stop-block nil))
  (declare (ignore stop-block))
  (gen-push-item (list 'logior (gen-pop-item) (gen-pop-item))))

(defmethod codegen ((insn ssa-array-length) &optional (stop-block nil))
  (declare (ignore stop-block))
  (gen-push-item (list 'length (gen-pop-item))))

(defmethod codegen ((insn ssa-assign) &optional (stop-block nil))
  (declare (ignore stop-block))
  (with-slots (source target) insn
    (list 'let (list (list 'value (codegen source)))
          (list 'setf (codegen target) 'value))))

(defmethod codegen ((insn ssa-call-static-method) &optional (stop-block nil))
  (declare (ignore stop-block))
  (trace-insn
   insn
   (with-slots (class method-name args) insn
     (let* ((nargs (length args))
            (call (cond
                    ((eq nargs 0)
                     (list (intern (format nil "~A.~A" class method-name) :openldk)))
                    ((eq nargs 1)
                     (list (intern (format nil "~A.~A" class method-name) :openldk) (codegen (car args))))
                    (t
                     (list 'apply
                           (list 'function (intern (format nil "~A.~A"
                                                           class
                                                           method-name)
                                                   :openldk))
                           (list 'reverse (cons 'list (mapcar (lambda (a) (codegen a)) args))))))))
       (if (void-return-p insn)
           call
           (gen-push-item call))))))

(defmethod codegen ((insn ssa-caload) &optional (stop-block nil))
  (declare (ignore stop-block))
  ;;; FIXME: throw nullpointerexception and invalid array index exception if needed
  (list 'let (list (list 'index (gen-pop-item))
                   (list 'arrayref (gen-pop-item)))
        (gen-push-item (list 'char-code (list 'aref 'arrayref 'index)))))

(defmethod codegen ((insn ssa-iaload) &optional (stop-block nil))
  (declare (ignore stop-block))
  ;;; FIXME: throw nullpointerexception and invalid array index exception if needed
  (list 'let (list (list 'index (gen-pop-item))
                   (list 'arrayref (gen-pop-item)))
        (gen-push-item (list 'aref 'arrayref 'index))))

(defmethod codegen ((insn ssa-castore) &optional (stop-block nil))
  (declare (ignore stop-block))
  ;;; FIXME: throw nullpointerexception and invalid array index exception if needed
  (list 'let (list (list 'value (gen-pop-item))
                   (list 'index (gen-pop-item))
                   (list 'arrayref (gen-pop-item)))
        (list 'setf (list 'aref 'arrayref 'index) (list 'code-char 'value))))

(defmethod codegen ((insn ssa-checkcast) &optional (stop-block nil))
  (declare (ignore stop-block))
  ;; FIXME: the array test can be done at compiletime
  (with-slots (class) insn
    (list 'progn
          ;; (list 'format t "CHECKCAST: ~A ~A~%" (gen-peek-item) (list 'quote (intern (name (slot-value (slot-value insn 'class) 'class)) :openldk)))
          (list 'when (gen-peek-item)
                (list 'unless (list 'or
                                    (list 'typep (gen-peek-item)
                                          (list 'quote (intern (name (slot-value (slot-value insn 'class) 'class)) :openldk)))
                                    (list 'and
                                          (list 'arrayp (gen-peek-item))
                                          (list 'eq (list 'quote '|java/util/Arrays|) (list 'quote (intern (name (slot-value (slot-value insn 'class) 'class)) :openldk)))))


                      (gen-push-item (list 'make-instance (list 'quote '|java/lang/ClassCastException|)))
                      (list 'error (list 'lisp-condition (gen-peek-item))))))))

(defmethod codegen ((insn ssa-class) &optional (stop-block nil))
  (declare (ignore stop-block))
  (let* ((classname (slot-value (slot-value insn 'class) 'name)))
    (java-class (gethash classname *classes*))))

(defmethod codegen ((insn ssa-branch-target) &optional (stop-block nil))
  (declare (ignore stop-block))
  (intern (format nil "branch-target-~A" (slot-value insn 'index))))

(defmethod codegen ((insn ssa-irem) &optional (stop-block nil))
  (declare (ignore stop-block))
  (flag-stack-usage *context*)
  (list 'handler-case
        (list 'let (list (list 'value2 (gen-pop-item))
                         (list 'value1 (gen-pop-item)))
              (gen-push-item (list 'rem 'value1 'value2)))
        (list 'division-by-zero (list 'e)
              (gen-push-item (list 'make-instance (list 'quote '|java/lang/ArithmeticException|)))
              (list 'error (list 'lisp-condition (gen-peek-item))))))

(defmethod codegen ((insn ssa-fdiv) &optional (stop-block nil))
  ;; FIXME - handle all weird conditions
  (declare (ignore stop-block))
  (flag-stack-usage *context*)
  (list 'handler-case
        (list 'let (list (list 'value2 (gen-pop-item))
                         (list 'value1 (gen-pop-item)))
              (gen-push-item (list '/ 'value1 'value2)))
        (list 'division-by-zero (list 'e)
              (gen-push-item (list 'make-instance (list 'quote '|java/lang/ArithmeticException|)))
              (list 'error (list 'lisp-condition (gen-peek-item))))))

(defmethod codegen ((insn ssa-idiv) &optional (stop-block nil))
  ;; FIXME - handle all weird conditions
  (declare (ignore stop-block))
  (flag-stack-usage *context*)
  (list 'handler-case
        (list 'let (list (list 'value2 (gen-pop-item))
                         (list 'value1 (gen-pop-item)))
              (gen-push-item (list 'floor (list '/ 'value1 'value2))))
        (list 'division-by-zero (list 'e)
              (gen-push-item (list 'make-instance (list 'quote '|java/lang/ArithmeticException|)))
              (list 'error (list 'lisp-condition (gen-peek-item))))))

(defmethod codegen ((insn ssa-ldiv) &optional (stop-block nil))
  ;; FIXME - handle all weird conditions
  (declare (ignore stop-block))
  (flag-stack-usage *context*)
  (list 'handler-case
        (list 'let (list (list 'value2 (gen-pop-item))
                         (list 'value1 (gen-pop-item)))
              (gen-push-item (list 'floor (list '/ 'value1 'value2))))
        (list 'division-by-zero (list 'e)
              (gen-push-item (list 'make-instance (list 'quote '|java/lang/ArithmeticException|)))
              (list 'error (list 'lisp-condition (gen-peek-item))))))

(defmethod codegen ((insn ssa-dup) &optional (stop-block nil))
  (declare (ignore stop-block))
  (flag-stack-usage *context*)
  (trace-insn
   insn
   (gen-push-item (gen-peek-item))))

(defmethod codegen ((insn ssa-dup-x1) &optional (stop-block nil))
  (declare (ignore stop-block))
  (flag-stack-usage *context*)
  (list 'let (list (list 'value1 (gen-pop-item))
                   (list 'value2 (gen-pop-item)))
        (gen-push-item 'value1)
        (gen-push-item 'value2)
        (gen-push-item 'value1)))

(defmethod codegen ((insn ssa-dup2) &optional (stop-block nil))
  (declare (ignore stop-block))
  (flag-stack-usage *context*)
  (trace-insn
   insn
   (list 'let (list (list 'value1 (gen-pop-item))
                    (list 'value2 (gen-pop-item)))
         (gen-push-item 'value2)
         (gen-push-item 'value1)
         (gen-push-item 'value2)
         (gen-push-item 'value1))))

(defmethod codegen ((insn ssa-fcmpg) &optional (stop-block nil))
  (declare (ignore stop-block))
  (flag-stack-usage *context*)
  (list 'let (list (list 'value2 (gen-pop-item))
                   (list 'value1 (gen-pop-item)))
        (list 'if (list 'or (list 'float-features:float-nan-p 'value1) (list 'float-features:float-nan-p 'value2))
              (gen-push-item 1)
              (list 'if (list '> 'value1 'value2)
                    (gen-push-item 1)
                    (list 'if (list '< 'value1 'value2)
                          (gen-push-item -1)
                          (gen-push-item 0))))))

(defmethod codegen ((insn ssa-fcmpl) &optional (stop-block nil))
  (declare (ignore stop-block))
  (flag-stack-usage *context*)
  (list 'let (list (list 'value2 (gen-pop-item))
                   (list 'value1 (gen-pop-item)))
        (list 'if (list 'or (list 'float-features:float-nan-p 'value1) (list 'float-features:float-nan-p 'value2))
              (gen-push-item -1)
              (list 'if (list '> 'value1 'value2)
                    (gen-push-item 1)
                    (list 'if (list '< 'value1 'value2)
                          (gen-push-item -1)
                          (gen-push-item 0))))))

(defmethod codegen ((insn ssa-iastore) &optional (stop-block nil))
  (declare (ignore stop-block))
  (flag-stack-usage *context*)
  (list 'let (list (list 'value (gen-pop-item))
                   (list 'index (gen-pop-item))
                   (list 'arrayref (gen-pop-item)))
        (list 'setf (list 'aref 'arrayref 'index) 'value)))

(defmethod codegen ((insn ssa-ineg) &optional (stop-block nil))
  (declare (ignore stop-block))
  (with-slots (index const) insn
    (gen-push-item (list '- (gen-pop-item)))))

(defmethod codegen ((insn ssa-i2c) &optional (stop-block nil))
  (declare (ignore stop-block))
  (with-slots (index const) insn
    (gen-push-item (list 'code-char (gen-pop-item)))))

(defmethod codegen ((insn ssa-l2f) &optional (stop-block nil))
  (declare (ignore stop-block))
  (with-slots (index const) insn
    (gen-push-item (list 'float (gen-pop-item)))))

(defmethod codegen ((insn ssa-l2i) &optional (stop-block nil))
  (declare (ignore stop-block))
  (with-slots (index const) insn
    (gen-push-item (list 'logand #xffffffff (gen-pop-item)))))

(defmethod codegen ((insn ssa-f2i) &optional (stop-block nil))
  (declare (ignore stop-block))
  (with-slots (index const) insn
    (gen-push-item (list 'floor (gen-pop-item)))))

(defmethod codegen ((insn ssa-d2l) &optional (stop-block nil))
  (declare (ignore stop-block))
  (with-slots (index const) insn
    (gen-push-item (list 'floor (gen-pop-item)))))

(defmethod codegen ((insn ssa-i2f) &optional (stop-block nil))
  (declare (ignore stop-block))
  (with-slots (index const) insn
    (gen-push-item (list 'float (gen-pop-item)))))

(defmethod codegen ((insn ssa-iinc) &optional (stop-block nil))
  (declare (ignore stop-block))
  (trace-insn
   insn
   (with-slots (index const) insn
     (list 'incf (intern (format nil "local-~A" index) :openldk) const))))

(defmethod codegen ((insn ssa-if-acmpeq) &optional (stop-block nil))
  (declare (ignore stop-block))
  (trace-insn
   insn
   (with-slots (offset) insn
     (list 'let (list (list 'o1 (list 'sxhash (gen-pop-item)))
                      (list 'o2 (list 'sxhash (gen-pop-item))))
           ;; (list 'format t "acmpeq ~A ~A~%" 'o1 'o2)
           (list 'if (list 'eq 'o1 'o2)
                 (list 'go (intern (format nil "branch-target-~A" offset))))))))

(defmethod codegen ((insn ssa-if-acmpne) &optional (stop-block nil))
  (declare (ignore stop-block))
  (trace-insn
   insn
   (with-slots (offset) insn
     (list 'let (list (list 'o1 (gen-pop-item))
                      (list 'o2 (gen-pop-item)))
           ;; (list 'format t "acmpne ~A ~A ~A ~A~%" 'o1 'o2 (list 'sxhash 'o1) (list 'sxhash 'o2))
           (list 'if (list 'not (list 'eq (list 'sxhash 'o1) (list 'sxhash 'o2)))
                 (list 'go (intern (format nil "branch-target-~A" offset))))))))

(defmethod codegen ((insn ssa-if-icmpeq) &optional (stop-block nil))
  (declare (ignore stop-block))
  (flag-stack-usage *context*)
  (with-slots (offset) insn
    (list 'if (list 'equal (gen-pop-item) (gen-pop-item))
          (list 'go (intern (format nil "branch-target-~A" offset))))))

(defmethod codegen ((insn ssa-if-icmple) &optional (stop-block nil))
  (declare (ignore stop-block))
  (flag-stack-usage *context*)
  (with-slots (offset) insn
    (list 'if (list '>= (gen-pop-item) (gen-pop-item))
          (list 'go (intern (format nil "branch-target-~A" offset))))))

(defmethod codegen ((insn ssa-if-icmpge) &optional (stop-block nil))
  (declare (ignore stop-block))
  (flag-stack-usage *context*)
  (with-slots (offset) insn
    (list 'if (list '<= (gen-pop-item) (gen-pop-item))
          (list 'go (intern (format nil "branch-target-~A" offset))))))

(defmethod codegen ((insn ssa-if-icmplt) &optional (stop-block nil))
  (declare (ignore stop-block))
  (flag-stack-usage *context*)
  (with-slots (offset) insn
    (list 'if (list '> (gen-pop-item) (gen-pop-item))
          (list 'go (intern (format nil "branch-target-~A" offset))))))

(defmethod codegen ((insn ssa-if-icmpgt) &optional (stop-block nil))
  (declare (ignore stop-block))
  (flag-stack-usage *context*)
  (with-slots (offset) insn
    (list 'if (list '< (gen-pop-item) (gen-pop-item))
          (list 'go (intern (format nil "branch-target-~A" offset))))))

(defmethod codegen ((insn ssa-if-icmpne) &optional (stop-block nil))
  (declare (ignore stop-block))
  (flag-stack-usage *context*)
  (with-slots (offset) insn
    (list 'if (list 'not (list 'eq (gen-pop-item) (gen-pop-item)))
          (list 'go (intern (format nil "branch-target-~A" offset))))))

(defmethod codegen ((insn ssa-ifeq) &optional (stop-block nil))
  (declare (ignore stop-block))
  (flag-stack-usage *context*)
  (with-slots (offset) insn
    (list 'if (list 'eq (gen-pop-item) 0)
          (list 'go (intern (format nil "branch-target-~A" offset))))))

(defmethod codegen ((insn ssa-ifge) &optional (stop-block nil))
  (declare (ignore stop-block))
  (flag-stack-usage *context*)
  (with-slots (offset) insn
    (list 'progn
          (list 'if (list '>= (gen-pop-item) 0)
                (list 'go (intern (format nil "branch-target-~A" offset)))))))

(defmethod codegen ((insn ssa-ifle) &optional (stop-block nil))
  (declare (ignore stop-block))
  (flag-stack-usage *context*)
  (with-slots (offset) insn
    (list 'progn
          (list 'if (list '<= (gen-pop-item) 0)
                (list 'go (intern (format nil "branch-target-~A" offset)))))))

(defmethod codegen ((insn ssa-iflt) &optional (stop-block nil))
  (declare (ignore stop-block))
  (flag-stack-usage *context*)
  (with-slots (offset) insn
    (list 'progn
          (list 'if (list '< (gen-pop-item) 0)
                (list 'go (intern (format nil "branch-target-~A" offset)))))))

(defmethod codegen ((insn ssa-ifgt) &optional (stop-block nil))
  (declare (ignore stop-block))
  (flag-stack-usage *context*)
  (with-slots (offset) insn
    (list 'progn
          (list 'if (list '> (gen-pop-item) 0)
                (list 'go (intern (format nil "branch-target-~A" offset)))))))

(defmethod codegen ((insn ssa-ifne) &optional (stop-block nil))
  (declare (ignore stop-block))
  (flag-stack-usage *context*)
  (trace-insn
   insn (with-slots (offset) insn
          (list 'if (list 'not (list 'eq (gen-pop-item) '0))
                (list 'go (intern (format nil "branch-target-~A" offset)))))))

(defmethod codegen ((insn ssa-ifnonnull) &optional (stop-block nil))
  (declare (ignore stop-block))
  (flag-stack-usage *context*)
  (with-slots (offset) insn
    (list 'if (list 'not (list 'null (gen-pop-item)))
          (list 'go (intern (format nil "branch-target-~A" offset))))))

(defmethod codegen ((insn ssa-ifnull) &optional (stop-block nil))
  (declare (ignore stop-block))
  (flag-stack-usage *context*)
  (with-slots (offset) insn
    (list 'if (list 'null (gen-pop-item))
          (list 'go (intern (format nil "branch-target-~A" offset))))))

(defmethod codegen ((insn ssa-instanceof) &optional (stop-block nil))
  (declare (ignore stop-block))
  (with-slots (class) insn
    (gen-push-item
     (list 'if (list 'typep (gen-pop-item)
                     (list 'quote (intern (name (slot-value (slot-value insn 'class) 'class)) :openldk))) 1 0))))

(defun logical-shift-right-32 (integer shift)
  (logand
   (ash (logand integer #xffffffff) (- shift))
   #xffffffff))

(defun shl (x width bits)
  "Compute bitwise left shift of x by 'bits' bits, represented on 'width' bits"
  (logand (ash x bits)
          (1- (ash 1 width))))

(defun shr (x width bits)
  "Compute bitwise right shift of x by 'bits' bits, represented on 'width' bits"
  (logand (ash x (- bits))
          (1- (ash 1 width))))

(defmethod codegen ((insn ssa-ishl) &optional (stop-block nil))
  (declare (ignore stop-block))
  ;; FIXME: this is wrong.
  (list 'let (list (list 'value2 (gen-pop-item))
                   (list 'value1 (gen-pop-item)))
        (list 'progn
              ;; (list 'format 't "ISHL ~A ~A~%" 'value1 'value2)
              (gen-push-item (list 'ash 'value1 'value2)))))

(defmethod codegen ((insn ssa-lshl) &optional (stop-block nil))
  (declare (ignore stop-block))
  ;; FIXME: this is wrong.
  (list 'let (list (list 'value2 (gen-pop-item))
                   (list 'value1 (gen-pop-item)))
        (list 'progn
              ;; (list 'format 't "LSHL ~A ~A~%" 'value1 'value2)
              (gen-push-item (list 'shl 'value1 'value2 32)))))

(defmethod codegen ((insn ssa-iushr) &optional (stop-block nil))
  (declare (ignore stop-block))
  ;; FIXME: this is wrong.
  (list 'let (list (list 'value2 (gen-pop-item))
                   (list 'value1 (gen-pop-item)))
        (list 'progn
              (gen-push-item (list 'shr 'value1 'value2 32)))))

(defmethod codegen ((insn ssa-lshr) &optional (stop-block nil))
  (declare (ignore stop-block))
  ;; FIXME: this is wrong.
  (list 'let (list (list 'value2 (gen-pop-item))
                   (list 'value1 (gen-pop-item)))
        (list 'progn
              (list 'format 't "LSHR ~A ~A~%" 'value1 'value2)
              (gen-push-item (list 'shr 'value1 'value2 32)))))

(defmethod codegen ((insn ssa-ishr) &optional (stop-block nil))
  (declare (ignore stop-block))
  ;; FIXME: this is wrong.
  (list 'let (list (list 'value2 (gen-pop-item))
                   (list 'value1 (gen-pop-item)))
        (list 'progn
              ;; (list 'format 't "ISHR ~A ~A~%" 'value1 'value2)
              (gen-push-item (list 'ash 'value1 (list '- 0 'value2))))))

(defmethod codegen ((insn ssa-lcmp) &optional (stop-block nil))
  (declare (ignore stop-block))
  (flag-stack-usage *context*)
  (list 'let (list (list 'value2 (gen-pop-item))
                   (list 'value1 (gen-pop-item)))
        (list 'cond
              (list (list 'eq 'value1 'value2)
                    (gen-push-item 0))
              (list (list '> 'value1 'value2)
                    (gen-push-item 1))
              (list 't
                    (gen-push-item -1)))))

(defmethod codegen ((insn ssa-lushr) &optional (stop-block nil))
  (declare (ignore stop-block))
  (flag-stack-usage *context*)
  (list 'let (list (list 'value2 (gen-pop-item))
                   (list 'value1 (gen-pop-item)))
        (list 'progn
              ;; (list 'format 't "LUSHR ~A ~A~%" 'value1 'value2)
              (gen-push-item (list 'ash 'value1 (list \- 'value2))))))

(defmethod codegen ((insn ssa-goto) &optional (stop-block nil))
  (declare (ignore stop-block))
  (trace-insn
   insn
   (with-slots (offset) insn
     (list 'go (intern (format nil "branch-target-~A" offset))))))

(defmethod codegen ((insn ssa-call-virtual-method) &optional (stop-block nil))
  (declare (ignore stop-block))
  (trace-insn
   insn
   (with-slots (method-name args) insn
     (let* ((nargs (length args))
            (call (cond
                    ((eq nargs 0)
                     (error "internal error"))
                    ((eq nargs 1)
                     (list (intern (format nil "~A" method-name) :openldk) (codegen (car args))))
                    (t
                     (list 'apply
                           (list 'function (intern (format nil "~A"
                                                           method-name) :openldk))
                           (list 'reverse (cons 'list (mapcar (lambda (a) (codegen a)) args))))))))
       (if (void-return-p insn)
           call
           (gen-push-item call))))))

(defmethod codegen ((insn ssa-clinit) &optional (stop-block nil))
  (declare (ignore stop-block))
  (with-slots (class) insn
    (let* ((class (ssa-class-class class)))
      (list 'unless (list 'initialized-p class)
            (list (intern (format nil "%clinit-~A" (slot-value class 'name)) :openldk))))))

(defmethod codegen ((insn ssa-local-variable) &optional (stop-block nil))
  (declare (ignore stop-block))
  (with-slots (index) insn
    (intern (format nil "local-~A" index) :openldk)))

(defmethod codegen ((insn ssa-monitorenter) &optional (stop-block nil))
  (declare (ignore stop-block))
  (flag-stack-usage *context*)
  (list 'monitor-enter (gen-pop-item)))

(defmethod codegen ((insn ssa-monitorexit) &optional (stop-block nil))
  (declare (ignore stop-block))
  (flag-stack-usage *context*)
  (list 'monitor-exit (gen-pop-item)))

(defmethod codegen ((insn ssa-mul) &optional (stop-block nil))
  (declare (ignore stop-block))
  (flag-stack-usage *context*)
  (gen-push-item (list '* (gen-pop-item) (gen-pop-item))))

(defmethod codegen ((insn ssa-new) &optional (stop-block nil))
  (declare (ignore stop-block))
  (with-slots (class) insn
    (with-slots (class) class
      (list 'make-instance (list 'quote (intern (slot-value class 'name) :openldk))))))

(defmethod codegen ((insn ssa-new-array) &optional (stop-block nil))
  (declare (ignore stop-block))
  (flag-stack-usage *context*)
  (trace-insn
   insn
   (list 'make-array (gen-pop-item) :initial-element nil)))

(defmethod codegen ((insn ssa-nop) &optional (stop-block nil))
  (declare (ignore stop-block))
  (gensym "NOP-"))

(defmethod codegen ((insn ssa-pop) &optional (stop-block nil))
  (declare (ignore stop-block))
  (flag-stack-usage *context*)
  (trace-insn
   insn
   (gen-pop-item)))

(defmethod codegen ((insn ssa-push) &optional (stop-block nil))
  (declare (ignore stop-block))
  (flag-stack-usage *context*)
  (trace-insn
   insn
   (with-slots (value) insn
     (gen-push-item (codegen value)))))

(defmethod codegen ((insn ssa-sub) &optional (stop-block nil))
  (declare (ignore stop-block))
  (flag-stack-usage *context*)
  (list 'let (list (list 'value2 (gen-pop-item))
                   (list 'value1 (gen-pop-item)))
        (gen-push-item (list '- 'value1 'value2))))

(defmethod codegen ((insn ssa-call-special-method) &optional (stop-block nil))
  (declare (ignore stop-block))
  (trace-insn
   insn
   (with-slots (class method-name args) insn
     (let ((call (list 'destructuring-bind (cons 'method 'next)
                       (list 'closer-mop:compute-applicable-methods-using-classes
                             (list 'function (intern (format nil "~A" method-name) :openldk))
                             ;; FIXME: This should be based on the args list
                             (cons 'list
                                   (cons (find-class (intern (slot-value class 'name) :openldk))
                                         (loop for a in args
                                               collect t))))
                       (list 'let (list (list 'fn (list 'closer-mop:method-function 'method)))
                             (list 'apply 'fn
                                   (list 'list (list 'reverse (cons 'list (mapcar (lambda (a) (codegen a)) args))) 'next))))))
       (if (void-return-p insn)
           call
           (gen-push-item call))))))

(defmethod codegen ((insn ssa-member) &optional (stop-block nil))
  (declare (ignore stop-block))
  (flag-stack-usage *context*)
  (with-slots (member-name) insn
    (list 'slot-value
          (list 'let (list (list 'objref (gen-pop-item)))
                (list 'when (list 'null 'objref) (list 'error
                                                       (format nil "Null Pointer Exception ~A" (slot-value insn 'address))))
                'objref)
          (list 'quote (intern member-name :openldk)))))

(defmethod codegen ((insn ssa-static-member) &optional (stop-block nil))
  (declare (ignore stop-block))
  (with-slots (class member-name) insn
    (list 'slot-value
          (intern (format nil "+static-~A+" (slot-value (slot-value class 'class) 'name)) :openldk)
          (list 'quote (intern member-name :openldk)))))

(defmethod codegen ((insn ssa-store) &optional (stop-block nil))
  (declare (ignore stop-block))
  (flag-stack-usage *context*)
  (trace-insn
   insn
   (with-slots (target) insn
     (list 'setf (codegen target) (gen-pop-item)))))

(define-condition java-lang-throwable (error)
  ((throwable :initarg :throwable :reader throwable)))

(defun make-java-condition (e)
  (make-condition (gethash (class-of e) *condition-table*) :objref e))

(defmethod codegen ((insn ssa-throw) &optional (stop-block nil))
  (declare (ignore stop-block))
  (flag-stack-usage *context*)
  (trace-insn
   insn
   (list 'let (list (list 'c (list 'lisp-condition (gen-peek-item))))
         (list 'error 'c))))

(defmethod codegen ((insn ssa-return) &optional (stop-block nil))
  (declare (ignore stop-block))
  (trace-insn
   insn
   (list 'return)))

(defmethod codegen ((insn ssa-return-value) &optional (stop-block nil))
  (declare (ignore stop-block))
  (flag-stack-usage *context*)
  (list 'return-from
        (intern (slot-value insn 'fn-name) :openldk)
        (gen-pop-item)))

(defmethod codegen ((insn ssa-variable) &optional (stop-block nil))
  (declare (ignore stop-block))
  (slot-value insn 'name))

(defmethod codegen ((basic-block <basic-block>) &optional (stop-block nil))
  (unless (equal basic-block stop-block)
    (if (not (slot-value basic-block 'code-emitted-p))
        (progn
          (push basic-block (slot-value *context* 'blocks))
          (let ((lisp-code
                  (cons (intern (format nil "branch-target-~A" (address (car (slot-value basic-block 'code)))))
                        (loop for insn in (slot-value basic-block 'code)
                              collect (codegen insn)))))
            (setf (slot-value basic-block 'code-emitted-p) t)
            (pop (slot-value *context* 'blocks))
            ;; sort by address
            (let ((successor-list (sort (fset:convert 'list (successors basic-block)) (lambda (a b) (< (address a) (address b))))))
              (if (eq 1 (length successor-list))
                  (if (slot-value (car successor-list) 'code-emitted-p)
                      (when (and (<= (address (car successor-list)) (+ (address (car (last (code basic-block)))) 4)))
                        (setf lisp-code (append lisp-code (list (list 'go (intern (format nil "branch-target-~A" (address (car successor-list)))))))))))
              (dolist (successor successor-list)
                (when successor
                  (setf lisp-code (append lisp-code (codegen successor (or stop-block (try-exit-block basic-block))))))))

            ;; Emit handlers for finally handlers. FIXME: in build-basic-blocks, sort try-catch list by end of range
            (when (find-if (lambda (p) (null (car p))) (try-catch basic-block))
              ;; This is a TRY-CATCH block.  Wrap this in HANDLER-CASE.
              (loop for tc in (reverse (try-catch basic-block))
                    unless (car tc)
                      do (setf lisp-code (append (list (list 'handler-case
                                                             (cons 'tagbody lisp-code)
                                                             (list 'condition (list (intern "condition" :openldk))
                                                                   (cons 'tagbody
                                                                         (codegen (cdr tc) (try-exit-block basic-block))))))
                                                 (when (try-exit-block basic-block)
                                                   (codegen (try-exit-block basic-block)))))))

            ;; Emit handler if there's a non-finally try-catch associated with this block.
            (when (find-if (lambda (p) (car p)) (try-catch basic-block))
              ;; This is a TRY-CATCH block.  Wrap this in HANDLER-CASE.
              (setf lisp-code (append (list (append (list 'handler-case)
                                                    (list (append (list 'tagbody) lisp-code))
                                                    (loop for tc in (try-catch basic-block)
                                                          when (car tc)
                                                            collect (append (list (intern (format nil "condition-~A" (car tc)) :openldk)
                                                                                  (list (intern "condition" :openldk)))
                                                                            (list (cons 'tagbody
                                                                                        (codegen (cdr tc) (try-exit-block basic-block))))))))
                                      (when (try-exit-block basic-block)
                                        (codegen (try-exit-block basic-block))))))

            lisp-code))
        nil)))
