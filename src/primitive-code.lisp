;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: OPENLDK; Base: 10 -*-
;;;
;;; Copyright (C) 2024, 2025, 2026  Anthony Green <green@moxielogic.com>
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

;;; Long values are signed at Java/Lisp boundaries, but modular arithmetic
;;; runs on unsigned words so SBCL can use registers rather than bignums.
(declaim (inline %long-word %signed-long))
(defun %long-word (value) (ldb (byte 64 0) value))
(defun %signed-long (bits) (sb-c::mask-signed-field 64 bits))

(defvar *method-primitive-parameters* nil)
(defvar *enable-primitive-entries* t)
(defvar *enable-word-longs* t)
(defvar *enable-safe-leaves* t)

(defun %safe-integer-leaf-form-p (form variables)
  "Prove a straight-line integer leaf cannot raise a translated Lisp error.
Unknown calls, division, shifts, arrays, fields and exception regions fail
closed. VARIABLES contains only locals whose complete provenance is integer."
  (labels ((safe (x locals)
             (cond ((integerp x) t)
                   ((symbolp x) (member x locals))
                   ((atom x) nil)
                   ((member (car x) '(+ - * logand logior logxor lognot))
                    (every (lambda (a) (safe a locals)) (cdr x)))
                   ((member (car x) '(%as-int %as-long))
                    (and (= (length x) 2) (safe (second x) locals)))
                   ((eq (car x) 'sb-c::mask-signed-field)
                    (and (= (length x) 3) (member (second x) '(32 64))
                         (safe (third x) locals)))
                   ((eq (car x) 'let)
                    (and (every (lambda (b) (and (consp b) (= (length b) 2)
                                                (safe (second b) locals))) (second x))
                         (every (lambda (b) (or (and (consp b) (eq (car b) 'declare))
                                               (safe b (append (mapcar #'first (second x)) locals))))
                                (cddr x))))
                   ((eq (car x) 'block)
                    (every (lambda (b) (safe b locals)) (cddr x)))
                   ((member (car x) '(progn tagbody))
                    (every (lambda (b) (safe b locals)) (cdr x)))
                   ((eq (car x) 'return-from)
                    (and (= (length x) 3) (safe (third x) locals))))))
    (safe form variables)))

(defun %omit-safe-leaf-handlers (body types)
  "Remove only method-level handlers around proven integer leaves."
  (let ((integers (loop for (variable . type) in types
                        when (member type '((signed-byte 32) (signed-byte 64)) :test #'equal)
                          collect variable)))
    (mapcar (lambda (form)
              (if (and *enable-safe-leaves* (consp form)
                       (eq (car form) 'with-java-conditions)
                       (null (getf (second form) :arrays))
                       (= (length form) 3)
                       (%safe-integer-leaf-form-p (third form) integers))
                  (third form)
                  form)) body)))

(defun %primitive-parameter-types (method)
  "Local slots with canonical primitive arguments at a guarded entry."
  (loop with slot = (if (static-p method) 0 1)
        for type in (parse-parameter-types (descriptor method))
        for category = (cond ((member type '("int" "short" "byte" "char" "boolean") :test #'string=) :int)
                             ((string= type "long") :long)
                             ((string= type "float") :float)
                             ((string= type "double") :double))
        when category collect (cons (intern (format nil "local-~D" slot) :openldk) category)
        do (incf slot (if (member type '("long" "double") :test #'string=) 2 1))))

(defun %primitive-lisp-type (category)
  (ecase category (:int '(signed-byte 32)) (:long '(signed-byte 64))
                  (:float 'single-float) (:double 'double-float)))

(defun %long-mask-p (form)
  (and (consp form) (eq (car form) 'sb-c::mask-signed-field)
       (eql (second form) 64) (= (length form) 3)))

(defun %modular-long-let-p (form)
  (and (consp form) (eq (car form) 'let) (= (length form) 3)
       (%long-mask-p (third form))
       (consp (third (third form)))
       (member (car (third (third form))) '(+ - * logand logior logxor lognot))))

(defun %lower-word-longs (form variables)
  "Lower typed long storage and modular expressions; decode signed uses.
Only modular operations consume words. Comparisons, division, right shifts,
conversions, fields, calls and returns see the original signed values."
  (labels
      ((safe (x words)
         (or (integerp x) (and (symbolp x) (member x words))
             (and (consp x)
                  (cond ((%long-mask-p x) (safe (third x) words))
                        ((eq (car x) '%as-long) (safe (second x) words))
                        ((%modular-long-let-p x)
                         (and (every (lambda (b) (safe (second b) words)) (second x))
                              (safe (third x) (append (mapcar #'first (second x)) words))))
                        ((member (car x) '(+ - * logand logior logxor lognot))
                         (every (lambda (a) (safe a words)) (cdr x)))))))
       (word (x words)
         (cond ((and (symbolp x) (member x words)) x)
               ((integerp x) (%long-word x))
               ((atom x) `(%long-word ,x))
               ((%long-mask-p x) (word (third x) words))
               ((eq (car x) '%as-long)
                (if (safe (second x) words)
                    (word (second x) words)
                    `(%long-word (%as-long ,(normal (second x) words)))))
               ((%modular-long-let-p x)
                (if (safe x words)
                    (let ((locals (mapcar #'first (second x))))
                      `(let ,(mapcar (lambda (binding) (list (first binding) (word (second binding) words))) (second x))
                         (declare ,@(mapcar (lambda (v) `(type (unsigned-byte 64) ,v)) locals))
                         ,(word (third x) (append locals words))))
                    `(let ,(mapcar (lambda (binding) (list (first binding) (normal (second binding) words))) (second x))
                       ,(word (third x) words))))
               ((member (car x) '(+ - * logand logior logxor lognot))
                (if (every (lambda (arg) (safe arg words)) (cdr x))
                    `(ldb (byte 64 0) (,(car x) ,@(mapcar (lambda (arg) (word arg words)) (cdr x))))
                    `(%long-word (,(car x) ,@(mapcar (lambda (arg) (normal arg words)) (cdr x))))))
               ((and (eq (car x) 'ash) (safe (second x) words)
                     (or (and (integerp (third x)) (not (minusp (third x))))
                         (and (consp (third x)) (eq (car (third x)) 'logand))))
                `(ldb (byte 64 0) (ash ,(word (second x) words) ,(normal (third x) words))))
               (t `(%long-word ,(normal x words)))))
       (normal (x words)
         (cond ((symbolp x) (if (member x words) `(%signed-long ,x) x))
               ((atom x) x)
               ((member (car x) '(quote function)) x)
               ((or (%long-mask-p x) (%modular-long-let-p x)) `(%signed-long ,(word x words)))
               ((eq (car x) 'declare)
                `(declare ,@(mapcar (lambda (decl)
                                      (if (and (eq (car decl) 'type) (equal (second decl) '(signed-byte 64))
                                               (every (lambda (v) (member v words)) (cddr decl)))
                                          `(type (unsigned-byte 64) ,@(cddr decl)) decl)) (cdr x))))
               ((and (eq (car x) 'setf) (= (length x) 3) (member (second x) words))
                `(setf ,(second x) ,(word (third x) words)))
               ((member (car x) '(let let*))
                `(,(car x) ,(mapcar (lambda (binding)
                                     (if (symbolp binding) binding
                                         (list (first binding)
                                               (if (member (first binding) words)
                                                   (word (second binding) words)
                                                   (normal (second binding) words))))) (second x))
                   ,@(mapcar (lambda (body) (normal body words)) (cddr x))))
               ;; A SETF place is a location, not a signed-value expression.
               ((eq (car x) 'setf)
                `(setf ,@(loop for (place value) on (cdr x) by #'cddr
                              append (list (if (symbolp place) place (normal place words)) (normal value words)))))
               (t (cons (if (consp (car x)) (normal (car x) words) (car x))
                        (mapcar (lambda (arg) (normal arg words)) (cdr x)))))))
    (normal form variables)))
