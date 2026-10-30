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

(defvar *enable-small-method-inlining* t)
(defvar *java-inline-functions* (make-hash-table :test 'eq :weakness :key :synchronized t)
  "Original Java entry functions -> weak method identities, never native replacements.")
(defstruct java-inline-site name class method function private-entry)
(defstruct java-private-inline-entry gf version valid)

(defun %note-java-inline-function (class method)
  "Record an original Java stub or successful compilation, using weak identities."
  ;; A lazy stub may install a registered Lisp body on its first invocation.
  ;; Its original bytecode is therefore not a valid inline implementation.
  (when (gethash (format nil "~A.~A~A" (name class) (name method) (descriptor method))
                 *native-overrides*)
    (return-from %note-java-inline-function nil))
  (when-let ((function
          (if (static-p method)
              (let ((symbol (static-method-symbol (format nil "~A.~A~A" (name class) (name method) (descriptor method))
                                                  (ldk-class-package class))))
                (and (fboundp symbol) (fdefinition symbol)))
              (let* ((symbol (intern (format nil "~A~A" (name method) (descriptor method)) :openldk))
                     (gf (and (fboundp symbol) (fdefinition symbol)))
                     (owner (find-class (intern (name class) (ldk-class-package class)) nil))
                     (entry (and gf owner
                                 (find-method gf nil (cons owner (make-list (count-parameters (descriptor method))
                                                                           :initial-element (find-class t))) nil))))
                (and entry (gethash entry *method-bodies*))))))
    (setf (gethash function *java-inline-functions*) (sb-ext:make-weak-pointer method))))

(declaim (inline %inline-static-target-p))
(defun %inline-static-target-p (site &optional (current (fdefinition (java-inline-site-name site))))
  "A Java recompilation may refresh the guard; Lisp replacement takes the fallback.
The caller owns this site and already retains its statically resolved class.
Keeping the expected function here adds no process-owned loader root."
  (when (eq current (java-inline-site-function site))
    (return-from %inline-static-target-p t))
  (let ((approved (gethash current *java-inline-functions*)))
    (when (and approved
               (eq (sb-ext:weak-pointer-value approved) (java-inline-site-method site)))
      (setf (java-inline-site-function site) current)
      t)))

(defun %small-integer-method-expression (bytes parameters)
  "Recognize a pure, straight-line integer expression. Return expression and success.
No calls, fields, branches, division, monitors or exception handling are admitted."
  (block reject
    (let ((pc 0) (stack nil))
      (labels ((next-byte ()
                 (when (>= pc (length bytes)) (return-from reject nil))
                 (prog1 (aref bytes pc) (incf pc)))
               (operand () (unless stack (return-from reject nil)) (pop stack))
               (local (index)
                 (unless (and (< index (length parameters)) (nth index parameters)) (return-from reject nil))
                 (nth index parameters))
               (binary (operator)
                 (let ((right (operand)) (left (operand)))
                   (push `(sb-c::mask-signed-field 32 (,operator ,left ,right)) stack))))
        (when (> (length bytes) 24) (return-from reject nil))
        (loop while (< pc (length bytes)) for opcode = (next-byte)
              do (case opcode
                   ((2 3 4 5 6 7 8) (push (- opcode 3) stack))
                   (16 (push (sb-c::mask-signed-field 8 (next-byte)) stack))
                   (17 (push (sb-c::mask-signed-field 16 (+ (ash (next-byte) 8) (next-byte))) stack))
                   (21 (push (local (next-byte)) stack))
                   ((26 27 28 29) (push (local (- opcode 26)) stack))
                   (96 (binary '+)) (100 (binary '-)) (104 (binary '*))
                   (126 (binary 'logand)) (128 (binary 'logior)) (130 (binary 'logxor))
                   (116 (push `(sb-c::mask-signed-field 32 (- ,(operand))) stack))
                   ((145 147) (push `(sb-c::mask-signed-field ,(if (= opcode 145) 8 16) ,(operand)) stack))
                   (146 (push `(ldb (byte 16 0) ,(operand)) stack))
                   (172 (unless (and (= pc (length bytes)) (= (length stack) 1)) (return-from reject nil))
                        (return-from reject (values (first stack) t)))
                   (otherwise (return-from reject nil))))))))

(defun %maybe-inline-static-form (call class method-name)
  "Inline a small Java integer leaf, retaining the full generic call as fallback."
  (let* ((method (and class (gethash method-name (%class-method-name-table class))))
         (attribute (and method (gethash "Code" (attributes method))))
         (types (and method (parse-parameter-types (descriptor method)))))
    (unless (and *enable-small-method-inlining* method attribute (static-p method)
                 (serapeum:nor (native-p method) (native-override-p method) (synchronized-p method))
                 (alexandria:emptyp (slot-value attribute 'exceptions))
                 (every (lambda (type) (member type '("int" "byte" "short" "char" "boolean") :test #'string=)) types)
                 (string= (drop (1+ (position #\) (descriptor method))) (descriptor method)) "I")
                 (= (length types) (length (rest call))))
      (return-from %maybe-inline-static-form call))
    (let ((parameters (loop repeat (length types) collect (gensym "ARG"))))
      (multiple-value-bind (expression success)
          (%small-integer-method-expression (slot-value attribute 'code) parameters)
        (if success
            `(let ,(mapcar #'list parameters (rest call))
               (if (and ,@(mapcar (lambda (p) `(typep ,p '(signed-byte 32))) parameters)
                        (%inline-static-target-p
                         (load-time-value
                          (make-java-inline-site :name ',(first call)
                                                 :class ,class
                                                 :method (gethash ,method-name (%class-method-name-table ,class))))
                         (fdefinition ',(first call))))
                   ,expression
                   (,(first call) ,@parameters)))
            call)))))

(defun %private-inline-miss (site gf)
  "Refresh private selection under the same method-change lock as dispatch."
  (unless (typep gf 'java-generic-function)
    (return-from %private-inline-miss nil))
  (bordeaux-threads:with-recursive-lock-held ((java-gf-method-lock gf))
    (when (eql :complex (bordeaux-threads:with-lock-held ((java-gf-cache-lock gf))
                         (%java-gf-method-index gf)))
      (return-from %private-inline-miss nil))
    (let* ((metadata (java-inline-site-method site))
           (owner (java-inline-site-class site))
           (class (find-class (intern (name owner) (ldk-class-package owner))))
           (method (find-method gf nil
                                (cons class (make-list (count-parameters (descriptor metadata))
                                                       :initial-element (find-class t))) nil))
           (body (and method (gethash method *method-bodies*)))
           (approved (and body (gethash body *java-inline-functions*)))
           (valid (and approved (eq metadata (sb-ext:weak-pointer-value approved))
                       (notany #'method-qualifiers
                               (%java-applicable-methods
                                gf class (cons class (make-list (count-parameters (descriptor metadata))
                                                               :initial-element (find-class t)))))))
           (snapshot (make-java-private-inline-entry
                      :gf (sb-ext:make-weak-pointer gf) :version (java-gf-dispatch-version gf) :valid valid)))
      (sb-thread:barrier (:write))
      (setf (java-inline-site-private-entry site) snapshot)
      valid)))

(declaim (inline %inline-private-target-p))
(defun %inline-private-target-p (site gf)
  "Private selection ignores receiver subclasses and observes Lisp method replacement."
  (let ((entry (java-inline-site-private-entry site)))
    (sb-thread:barrier (:read))
    (if (and entry (typep gf 'java-generic-function)
             (eq gf (sb-ext:weak-pointer-value (java-private-inline-entry-gf entry)))
             (eql (java-gf-dispatch-version gf) (java-private-inline-entry-version entry)))
        (java-private-inline-entry-valid entry)
        (%private-inline-miss site gf))))

(defun %maybe-inline-instance-form (call reference method-name &key private)
  "Inline pure final/private instance integer leaves after dispatch guards."
  (let* ((owner (and reference (%find-declaring-class reference method-name)))
         (method (and owner (gethash method-name (%class-method-name-table owner))))
         (attribute (and method (gethash "Code" (attributes method))))
         (types (and method (parse-parameter-types (descriptor method))))
         (arguments (if private (rest (third call)) (cddr call))))
    (unless (and *enable-small-method-inlining* method attribute (not (static-p method))
                 (or (logbitp 1 (access-flags method)) (logbitp 4 (access-flags method))
                     (logbitp 4 (access-flags owner)))
                 (serapeum:nor (native-p method) (native-override-p method) (synchronized-p method))
                 (alexandria:emptyp (slot-value attribute 'exceptions))
                 (every (lambda (type) (member type '("int" "byte" "short" "char" "boolean") :test #'string=)) types)
                 (string= (drop (1+ (position #\) (descriptor method))) (descriptor method)) "I")
                 (= (length arguments) (1+ (length types))))
      (return-from %maybe-inline-instance-form call))
    (let ((receiver (gensym "RECEIVER"))
          (parameters (loop repeat (length types) collect (gensym "ARG")))
          (target (gensym "TARGET"))
          (symbol (intern method-name :openldk)))
      (multiple-value-bind (expression success)
          (%small-integer-method-expression (slot-value attribute 'code) (cons nil parameters))
        (unless success (return-from %maybe-inline-instance-form call))
        (let* ((site `(load-time-value
                      (make-java-inline-site :name ',symbol :class ,owner
                                             :method (gethash ,method-name (%class-method-name-table ,owner)))))
               (actuals arguments)
               (guard (if private `(%inline-private-target-p ,site (fdefinition ',symbol))
                          `(%inline-static-target-p ,site ,target)))
               (fallback (if private `(,(first call) ,(second call) (list ,receiver ,@parameters))
                             `(funcall ,target ,receiver ,@parameters)))
               (body `(if (and ,@(mapcar (lambda (p) `(typep ,p '(signed-byte 32))) parameters)
                               ,@(when private `((not (null ,receiver)))) ,guard)
                          ,expression ,fallback)))
          `(let ,(mapcar #'list (cons receiver parameters) actuals)
             ,(if private body
                  `(let ((,target (%virtual-call-function ,(second call) ,receiver))) ,body))))))))
