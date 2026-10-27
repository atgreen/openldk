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

;;; The Java referent slot must not keep a weak/soft/phantom referent alive.
;;; Weak-key registry entries hold only SBCL weak pointers, never the Reference
;;; itself. Soft references currently use the permitted eager-clearing policy.
;;; FinalReference remains strong: Java finalization/resurrection needs a
;;; separate GC protocol, and must not be approximated by weak references.
(defvar *java-references* (make-hash-table :test 'eq :weakness :key))
(defvar *java-reference-lock* (bordeaux-threads:make-lock "Java references"))
(defvar *java-reference-processor* nil)
(defvar *java-reference-gc-pending* t)

(defun %note-java-reference-gc ()
  "Signal delivery work without allocating, taking locks, or entering Java in GC."
  (setf *java-reference-gc-pending* t))

(pushnew '%note-java-reference-gc sb-ext:*after-gc-hooks*)

(defun %java-reference-value (reference)
  "Read the weak referent without making the registry a strong GC root."
  (bordeaux-threads:with-lock-held (*java-reference-lock*)
    (if-let ((pointer (gethash reference *java-references*)))
      (sb-ext:weak-pointer-value pointer)
      (slot-value reference '|referent|))))

(defun %clear-java-reference (reference)
  "Clear REFERENCE without scheduling it for GC-driven enqueueing."
  (bordeaux-threads:with-lock-held (*java-reference-lock*)
    ;; Removing the registration also prevents GC from enqueueing an explicitly
    ;; cleared reference. Reference.enqueue() still works through Java bytecode.
    (remhash reference *java-references*)
    (setf (slot-value reference '|referent|) nil)))

(defun %collect-java-references ()
  "Claim dead registrations once, without calling Java while holding the lock."
  (bordeaux-threads:with-lock-held (*java-reference-lock*)
    (let ((pending nil))
      (setf *java-reference-gc-pending* nil)
      (maphash (lambda (reference pointer)
                 (unless (sb-ext:weak-pointer-value pointer)
                   (push reference pending)))
               *java-references*)
      (dolist (reference pending)
        (remhash reference *java-references*))
      pending)))

(defun %java-reference-kind-p (reference binary-name)
  "Recognize a JDK reference subtype without requiring it during bootstrap."
  (find binary-name (closer-mop:class-precedence-list (class-of reference))
        :key (lambda (class) (symbol-name (class-name class))) :test #'string=))

(defun %process-java-references ()
  "Deliver collected references outside the GC and registry lock; return the count."
  (let ((pending (%collect-java-references)))
    (dolist (reference pending)
      ;; Use the internal queue operation, not overridable Reference.enqueue().
      ;; The legacy Cleaner has no queue: ReferenceHandler invokes clean directly.
      (if (%java-reference-kind-p reference "jdk/internal/ref/Cleaner")
          (|clean()V| reference)
          (|enqueue(Ljava/lang/ref/Reference;)Z| (slot-value reference '|queue|) reference)))
    (length pending)))

(defun %run-java-reference-processor ()
  "Give the delivery service its own Java identity and Scoped Value state."
  (let ((*scoped-value-cache* nil)
        (thread (%make-java-instance "java/lang/Thread"))
        (current (bordeaux-threads:current-thread)))
    (|<init>(Ljava/lang/String;)V| thread (jstring "Reference Handler"))
    (|setDaemon(Z)V| thread 1)
    (setf (slot-value thread '|eetop|) 1)
    (%set-thread-status thread 4)
    (setf (gethash current *lisp-to-java-threads*) thread)
    (unwind-protect
         (loop
           ;; The GC hook only sets a flag. Java execution, compilation, locks,
           ;; and ReferenceQueue notifications belong on this ordinary thread.
           (when *java-reference-gc-pending*
             (handler-case (%process-java-references)
               (error (condition)
                 (format *error-output* "~&Reference processing failed: ~A~%" condition))))
           (sleep 0.05))
      (remhash current *lisp-to-java-threads*))))

(defun %ensure-java-reference-processor ()
  "Start or restart the reference delivery service, including after image restore."
  (bordeaux-threads:with-lock-held (*java-reference-lock*)
    (unless (and *java-reference-processor*
                 (bordeaux-threads:thread-alive-p *java-reference-processor*))
      (setf *java-reference-gc-pending* t
            *java-reference-processor*
            (bordeaux-threads:make-thread #'%run-java-reference-processor
                                         :name "OpenLDK reference processor")))))

(defun %initialize-java-reference (reference referent queue)
  "Initialize Java queue state and register a non-owning referent."
  (%clinit (classload "java/lang/ref/ReferenceQueue"))
  (let* ((pkg (class-package "java/lang/ref/ReferenceQueue"))
         (storage (symbol-value (intern "+static-java/lang/ref/ReferenceQueue+" pkg))))
    (setf (slot-value reference '|queue|)
          (or queue (slot-value storage '|NULL_QUEUE|))))
  (cond
    ((%java-reference-kind-p reference "java/lang/ref/FinalReference")
     (setf (slot-value reference '|referent|) referent))
    (t
     (setf (slot-value reference '|referent|) nil)
     (when referent
       (bordeaux-threads:with-lock-held (*java-reference-lock*)
         (setf (gethash reference *java-references*) (sb-ext:make-weak-pointer referent)))
       (%ensure-java-reference-processor))))
  nil)

(setf (gethash "java/lang/ref/Reference.<init>(Ljava/lang/Object;Ljava/lang/ref/ReferenceQueue;)V"
               *native-overrides*) #'%initialize-java-reference)
(setf (gethash "java/lang/ref/Reference.<init>(Ljava/lang/Object;)V" *native-overrides*)
      (lambda (reference referent) (%initialize-java-reference reference referent nil)))
(setf (gethash "java/lang/ref/Reference.get()Ljava/lang/Object;" *native-overrides*)
      #'%java-reference-value)

(defmethod |clear0()V| ((reference |java/lang/ref/Reference|))
  (%clear-java-reference reference))

(defmethod |refersTo0(Ljava/lang/Object;)Z| ((reference |java/lang/ref/Reference|) object)
  (if (eq (%java-reference-value reference) object) 1 0))

(defmethod |waitForReferenceProcessing()Z| ((this %java-lang-ref-access-stub))
  (if (plusp (%process-java-references)) 1 0))
