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

(defvar *enable-fast-array-loops* t)

(defun %loop-array-length (form)
  "Recognize codegen's array length expression, returning its source local."
  (when (and (consp form) (eql (car form) '%as-int))
    (setf form (second form)))
  (when (and (consp form) (eql (car form) 'let) (= (length form) 3)
             (= (length (second form)) 1)
             (consp (first (second form))) (= (length (first (second form))) 2))
    (destructuring-bind (alias source) (first (second form))
      (when (and (symbolp source)
                 (equal (third form) `(if (java-array-p ,alias)
                                         (length (java-array-data ,alias))
                                         (%array-length-slow ,alias))))
        source))))

(defun %loop-array-load (form)
  "Recognize a primitive array load: source local, index local and vector type."
  (when (and (consp form) (eql (car form) 'let) (= (length form) 3)
             (= (length (second form)) 2)
             (every (lambda (b) (and (consp b) (= (length b) 2))) (second form)))
    (let* ((bindings (second form)) (index (first (first bindings)))
           (array (first (second bindings))) (body (third form))
           (type (and (consp body) (consp (second body)) (second (second body)))))
      (when (and (symbolp (second (first bindings))) (symbolp (second (second bindings)))
                 (member type '((simple-array (signed-byte 8) (*))
                                (simple-array (unsigned-byte 16) (*))
                                (simple-array (signed-byte 16) (*))
                                (simple-array (signed-byte 32) (*))
                                (simple-array (signed-byte 64) (*))
                                (simple-array single-float (*))
                                (simple-array double-float (*))) :test #'equal)
                 (or (equal body `(aref (the ,type (java-array-data ,array)) ,index))
                     (equal body `(aref (the ,type (java-array-data ,array))
                                        (if (minusp ,index) (%throw-array-index-error ,index) ,index)))))
        (values (second (second bindings)) (second (first bindings)) type)))))

(defun %version-array-tagbody (form)
  "Version a closed zero-based unit-stride primitive reduction loop.
The fast branch has nonthrowing shape/range guards. The entire original
TAGBODY remains the fallback, including initialization and exception order."
  (block reject
    (let* ((forms (cdr form))
           (labels (remove-if #'consp forms)))
      (unless (= (length labels) 2) (return-from reject form))
      (let* ((start (first labels)) (exit (second labels))
             (begin (position start forms)) (end (position exit forms))
             (prefix (subseq forms 0 begin))
             (test (nth (1+ begin) forms))
             (condition (if (and (consp test) (eql (car test) 'progn) (= (length test) 2)) (second test) test))
             (comparison (and (consp condition) (second condition)))
             (index (and (consp comparison) (second comparison)))
             (bound-array (and (consp comparison) (%loop-array-length (third comparison))))
             (body (and (> end (+ begin 3)) (subseq forms (+ begin 2) (- end 2))))
             (increment (and (> end (+ begin 3)) (nth (- end 2) forms)))
             (arrays (and bound-array (list (cons bound-array '(simple-array * (*))))))
             (assigned nil))
        (unless (and (> end (+ begin 3)) (symbolp index) bound-array
                     (equal condition `(when ,comparison (go ,exit)))
                     (eql (car comparison) '>=) (= (length comparison) 3)
                     (equal (nth (1- end) forms) `(go ,start))
                     (equal increment `(setf ,index (%as-int (sb-c::mask-signed-field 32 (+ ,index 1)))))
                     (= 1 (count `(setf ,index (%as-int 0)) prefix :test #'equal)))
          (return-from reject form))
        (labels ((array (source type)
                   (let ((entry (assoc source arrays)))
                     (cond ((null entry) (push (cons source type) arrays))
                           ((equal (cdr entry) '(simple-array * (*))) (setf (cdr entry) type))
                           ((not (equal (cdr entry) type)) (return-from reject form)))))
                 (has-go (x)
                   (and (consp x) (not (member (car x) '(quote function)))
                        (or (eql (car x) 'go) (some #'has-go x))))
                 (pure (x)
                   (cond ((atom x) t)
                         ((%loop-array-load x)
                          (multiple-value-bind (source subscript type) (%loop-array-load x)
                            (unless (eql subscript index) (return-from reject form))
                            (array source type)) t)
                         ((eql (car x) 'quote) t)
                         ((member (car x) '(+ - * logand logior logxor lognot
                                           %as-int %as-long %as-float %as-double %long-word %signed-long byte ldb))
                          (every #'pure (cdr x)))
                         ((eql (car x) 'sb-c::mask-signed-field)
                          (and (member (second x) '(8 16 32 64)) (pure (third x))))
                         ((eql (car x) 'the) (pure (third x)))
                         ((eql (car x) 'declare) t)
                         ((eql (car x) 'let)
                          (and (every (lambda (b) (and (consp b) (= (length b) 2) (pure (second b)))) (second x))
                               (every #'pure (cddr x))))
                         ((and (eql (car x) 'setf) (= (length x) 3) (symbolp (second x)))
                          (push (second x) assigned)
                          (pure (third x))))))
          (unless (and (every #'pure prefix) (every #'pure body)
                       (= 1 (count index assigned))
                       (notany (lambda (entry) (member (car entry) assigned)) arrays)
                       (notany #'has-go (subseq forms (1+ end))))
            (return-from reject form)))
        (let* ((vectors (mapcar (lambda (entry) (cons (car entry) (gensym "DATA"))) arrays))
               (length (gensym "LENGTH"))
               (bound-vector (cdr (assoc bound-array vectors))))
          (labels ((rewrite (x)
                     (cond ((atom x) x)
                           ((eql (%loop-array-length x) bound-array) length)
                           ((%loop-array-load x)
                            (multiple-value-bind (source subscript type) (%loop-array-load x)
                              `(locally (declare (optimize (safety 0)))
                                 (aref (the ,type ,(cdr (assoc source vectors)))
                                       (the (integer 0 2147483647) ,subscript)))))
                           ((member (car x) '(quote declare)) x)
                           (t (mapcar #'rewrite x)))))
            (values
             `(let* (,@(mapcar (lambda (entry)
                                `(,(cdr entry) (and (java-array-p ,(car entry)) (java-array-data ,(car entry))))) vectors)
                     (,length (if (vectorp ,bound-vector) (length ,bound-vector) 0)))
                (if (and ,@(mapcar (lambda (entry) `(typep ,(cdr (assoc (car entry) vectors)) ',(cdr entry))) arrays)
                         (<= ,length 2147483647)
                         ,@(mapcar (lambda (entry) `(>= (length ,(cdr entry)) ,length)) vectors))
                    (tagbody ,@prefix ,start ,(rewrite test)
                       ,@(mapcar #'rewrite body) ,increment (go ,start)
                       ,exit ,@(subseq forms (1+ end)))
                    ,form))
             t)))))))

(defun %specialize-array-loops (form)
  "Find closed reduction loops; keep all unrecognized control flow unchanged."
  (cond ((or (not *enable-fast-array-loops*) (atom form)) form)
        ((member (car form) '(quote function declare)) form)
        ((eql (car form) 'tagbody) (%version-array-tagbody form))
        (t (cons (%specialize-array-loops (car form))
                 (%specialize-array-loops (cdr form))))))
