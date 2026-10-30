;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: OPENLDK; Base: 10 -*-
;;;
;;; Copyright (C) 2026  Anthony Green <green@moxielogic.com>
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

(defvar *enable-primitive-result-arithmetic* t)
(defvar *method-primitive-return-type* nil)
(defvar *primitive-result-mutable-variables* nil)

(defun %primitive-written-variables (form)
  "Conservatively find writes, including captured writes, before inferring types."
  (let ((variables nil))
    (labels ((remember (place)
               (when (symbolp place) (pushnew place variables)))
             (visit (x)
               (when (and (consp x) (not (eql (first x) 'quote)))
                 (case (first x)
                   ((setf psetf setq psetq)
                    (loop for (place value) on (rest x) by #'cddr
                          do (remember place) (visit value)))
                   ((incf decf pop) (remember (second x)))
                   ((push pushnew) (remember (third x)))
                   ((rotatef shiftf) (mapc #'remember (rest x)))
                   (multiple-value-setq (mapc #'remember (second x))))
                 (mapc #'visit x))))
      (visit form))
    variables))

(defun %primitive-form-type (form environment)
  "Infer only representation facts, never a native call's nominal descriptor."
  (cond ((numberp form) (type-of form))
        ((symbolp form) (cdr (assoc form environment)))
        ((atom form) nil)
        ((eql (first form) 'the) (second form))
        ((member (first form) '(let let*))
         (let ((local environment) (declared (%primitive-declaration-types (cddr form))))
           (dolist (binding (second form))
             (let* ((name (if (symbolp binding) binding (first binding)))
                    (type (or (cdr (assoc name declared))
                              (unless (member name *primitive-result-mutable-variables*)
                                (%primitive-form-type (and (consp binding) (second binding))
                                                      (if (eql (first form) 'let*) local environment))))))
               (push (cons name type) local)))
           (%primitive-form-type (car (last form)) local)))
        ((member (first form) '(+ - *))
         (let ((types (mapcar (lambda (arg) (%primitive-form-type arg environment)) (rest form))))
           (cond ((and types (every (lambda (type) (eql type 'double-float)) types)) 'double-float)
                 ((and types (every (lambda (type) (eql type 'single-float)) types)) 'single-float))))
        ((eql (first form) 'locally) (%primitive-form-type (car (last form)) environment))
        ((eql (first form) 'sb-c::mask-signed-field) `(signed-byte ,(second form)))
        ((member (first form) '(%as-int %as-long %as-float %as-double %long-word %signed-long))
         (case (first form)
           (%as-int '(signed-byte 32))
           ((%as-long %signed-long) '(signed-byte 64))
           (%as-float 'single-float) (%as-double 'double-float)
           (%long-word '(unsigned-byte 64))))
        ((and (eql (first form) 'aref) (consp (second form))
              (eql (first (second form)) 'the))
         (let ((type (second (second form))))
           (and (consp type) (eql (first type) 'simple-array) (second type))))))

(defun %primitive-test-types (test environment)
  "Refine the true branch of canonical primitive guards."
  (cond ((atom test) environment)
        ((eql (first test) 'and)
         (reduce (lambda (env clause) (%primitive-test-types clause env)) (rest test) :initial-value environment))
        ((and (eql (first test) 'typep) (symbolp (second test))
              (not (member (second test) *primitive-result-mutable-variables*))
              (consp (third test)) (eql (first (third test)) 'quote)
              (member (second (third test)) '((signed-byte 32) (signed-byte 64) single-float double-float) :test #'equal))
         (acons (second test) (second (third test)) environment))
        (t environment)))

(defun %guard-primitive-math (form environment hint)
  "Specialize arithmetic on already-evaluated operands, with the exact old fallback."
  (let* ((signed (and (consp form) (eql (first form) 'sb-c::mask-signed-field)
                      (member (second form) '(32 64))))
         (word (and (consp form) (eql (first form) '%long-word)))
         (operation (cond (signed (third form)) (word (second form)) (t form)))
         (type (cond (signed `(signed-byte ,(second form))) (word '(signed-byte 64)) (t hint))))
    (unless (and (member type '((signed-byte 32) (signed-byte 64) single-float double-float) :test #'equal)
                 (consp operation) (<= 2 (length operation) 3)
                 (member (first operation) (if (consp type) '(+ - * logand logior logxor lognot) '(+ - *)))
                 (every (lambda (arg) (or (numberp arg) (and (symbolp arg) (not (member arg '(t nil)))))) (rest operation))
                 (every (lambda (arg) (or (symbolp arg) (typep arg type))) (rest operation)))
      (return-from %guard-primitive-math form))
    (let* ((arguments (remove-duplicates (rest operation)))
           (unknown (remove-if (lambda (argument)
                                 (let ((known (%primitive-form-type argument environment)))
                                   (and known (subtypep known type)))) arguments)))
      (unless (or unknown (and (equal type '(signed-byte 64)) (or signed word)))
        (return-from %guard-primitive-math form))
      (let ((fast
              (if (and (equal type '(signed-byte 64)) (or signed word))
                  (let* ((words (mapcar (lambda (a) (cons a (gensym "WORD"))) arguments))
                         (value `(ldb (byte 64 0)
                                      (,(first operation) ,@(mapcar (lambda (a) (cdr (assoc a words))) (rest operation))))))
                    `(let ,(mapcar (lambda (entry) `(,(cdr entry) (%long-word (the (signed-byte 64) ,(car entry))))) words)
                       (declare (type (unsigned-byte 64) ,@(mapcar #'cdr words)))
                       ,(if signed `(%signed-long ,value) value)))
                  `(locally (declare (type ,type ,@(remove-if-not #'symbolp arguments))) ,form))))
        (if unknown
            `(if (and ,@(mapcar (lambda (argument) `(typep ,argument ',type)) unknown)) ,fast ,form)
            fast)))))

(defun %primitive-declaration-types (body)
  "The lexical type declarations at the start of BODY."
  (loop for statement in body while (and (consp statement) (eql (first statement) 'declare))
        append (loop for declaration in (rest statement)
                     when (and (consp declaration) (eql (first declaration) 'type))
                       append (loop for variable in (cddr declaration) collect (cons variable (second declaration))))))

(defun %specialize-primitive-result-arithmetic (form types)
  "Restore primitive arithmetic after opaque calls without declaring their results.
Only actual canonical values take typed paths; all other Lisp values keep the
old evaluation, coercion and error behavior. Existing proven types skip guards."
  (unless *enable-primitive-result-arithmetic*
    (return-from %specialize-primitive-result-arithmetic form))
  (let ((*primitive-result-mutable-variables* (%primitive-written-variables form)))
   (labels ((walk-sequence (forms env hint)
             (loop for tail on forms collect (walk (first tail) env (and (null (rest tail)) hint))))
           (walk (x env hint)
             (cond ((atom x) x)
                   ((member (first x) '(quote function declare lambda labels flet macrolet symbol-macrolet
                                       progv prog prog* multiple-value-bind destructuring-bind
                                       do do* dolist dotimes loop)) x)
                   ((member (first x) '(let let*))
                    (let ((local env) (bindings nil)
                          (declared (%primitive-declaration-types (cddr x))))
                      (dolist (binding (second x))
                        (let* ((name (if (symbolp binding) binding (first binding)))
                               (init (and (consp binding) (second binding)))
                               (value (walk init (if (eql (first x) 'let*) local env) (cdr (assoc name declared))))
                               (type (or (cdr (assoc name declared))
                                         (unless (member name *primitive-result-mutable-variables*)
                                           (%primitive-form-type value (if (eql (first x) 'let*) local env))))))
                          (push (list name value) bindings)
                          (push (cons name type) local)))
                      `(,(first x) ,(nreverse bindings) ,@(walk-sequence (cddr x) local hint))))
                   ((eql (first x) 'if)
                    `(if ,(walk (second x) env nil)
                         ,(walk (third x) (%primitive-test-types (second x) env) hint)
                         ,(walk (fourth x) env hint)))
                   ((eql (first x) 'handler-case)
                    `(handler-case ,(walk (second x) env hint)
                       ,@(loop for (condition variables . body) in (cddr x)
                               collect `(,condition ,variables
                                         ,@(walk-sequence body
                                                         (append (mapcar (lambda (var) (cons var nil)) variables) env)
                                                         hint)))))
                   ((eql (first x) 'setf)
                    `(setf ,@(loop for (place value) on (rest x) by #'cddr
                                   append (list place (walk value env (and (symbolp place) (cdr (assoc place env))))))))
                   ((eql (first x) 'return-from)
                    `(return-from ,(second x) ,(walk (third x) env *method-primitive-return-type*)))
                   ((member (first x) '(block progn locally))
                    (if (eql (first x) 'block)
                        `(block ,(second x) ,@(walk-sequence (cddr x) env hint))
                        `(,(first x) ,@(walk-sequence (rest x) (append (%primitive-declaration-types (rest x)) env) hint))))
                   ((member (first x) '(%as-int %as-long %as-float %as-double))
                    `(,(first x) ,(walk (second x) env (%primitive-form-type x env))))
                   ((eql (first x) 'sb-c::mask-signed-field)
                    (%guard-primitive-math `(,(first x) ,(second x) ,(walk (third x) env nil)) env hint))
                   ((eql (first x) '%long-word)
                    (%guard-primitive-math `(%long-word ,(walk (second x) env nil)) env hint))
                   (t (%guard-primitive-math
                       (cons (if (consp (first x)) (walk (first x) env nil) (first x))
                             (mapcar (lambda (arg) (walk arg env nil)) (rest x))) env hint)))))
    (walk form types nil))))
