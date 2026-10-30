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

(defclass/std <java-monitor> ()
  ((mutex :std (bordeaux-threads:make-lock))
   (condition-variable :std (bordeaux-threads:make-condition-variable))
   (owner)
   (recursion-count :std 0)
   (wait-set :std nil)
   ;; Threads asleep on CONDITION-VARIABLE, entering or in wait().  A
   ;; broadcast is a futex syscall even with no one to wake, so releases
   ;; skip it while this is zero.
   (sleepers :std 0)))

(defun %monitor-sleep (monitor &optional timeout)
  "Wait on MONITOR's condition variable, its mutex held, for at most
TIMEOUT seconds when given.  Returns NIL on a timeout."
  (incf (sleepers monitor))
  (unwind-protect
       ;; Java permits Long.MAX_VALUE milliseconds. SBCL cannot represent
       ;; that deadline, so wake periodically and let the caller recompute it.
       (let ((interval (and timeout (min timeout 86400))))
         (or (if interval
                 (bordeaux-threads:condition-wait (condition-variable monitor) (mutex monitor)
                                                  :timeout interval)
                 (bordeaux-threads:condition-wait (condition-variable monitor) (mutex monitor)))
             (and timeout (> timeout interval))))
    (let ((*java-thread-sleeping* nil))
      (sb-sys:without-interrupts
        ;; CONDITION-WAIT may unwind before restoring its mutex.
        (unless (sb-thread:holding-mutex-p (mutex monitor))
          (bordeaux-threads:acquire-lock (mutex monitor)))
        (decf (sleepers monitor))))))

(defun %monitor-wake (monitor)
  "Wake every thread asleep on MONITOR, its mutex held."
  (when (plusp (sleepers monitor))
    (sb-thread:condition-broadcast (condition-variable monitor))))

(defvar *monitors* (make-hash-table :weakness :key :synchronized t))

(defvar *last-monitor* (list nil)
  "Weak (object . monitor) shortcut, replaced as one pair for concurrent readers.")

(defun %get-monitor (object)
  "Return (and cache) the monitor object associated with OBJECT.
Monitors for unreferenced objects are automatically reclaimed by the GC
via the weak hash table."
  (let* ((last *last-monitor*)
         (cached (and (rest last) (sb-ext:weak-pointer-value (rest last)))))
    (if (and cached (first last) (eq (sb-ext:weak-pointer-value (first last)) object))
        cached
        (let ((monitor
                (or (gethash object *monitors*)
                    ;; Double-checked locking: re-check under the hash table
                    ;; lock to avoid creating duplicate monitors for the
                    ;; same object.
                    (sb-ext:with-locked-hash-table (*monitors*)
                      (or (gethash object *monitors*)
                          (let ((monitor (make-instance '<java-monitor>)))
                            (setf (gethash object *monitors*) monitor)
                            monitor))))))
          (setf *last-monitor* (cons (sb-ext:make-weak-pointer object) (sb-ext:make-weak-pointer monitor)))
          monitor))))

(defun monitor-enter (object)
  "Enter the monitor for OBJECT, acquiring its mutex.
SBCL's grab-mutex and condition-wait auto-dispatch to fiber-aware paths
when running in a fiber, so no separate fiber code path is needed."
  (when object
    (let* ((monitor (%get-monitor object))
           (mutex (mutex monitor))
           (current-thread (current-thread-identity)))
      (bordeaux-threads:with-lock-held (mutex)
        (cond
          ((eq (owner monitor) current-thread)
           (incf (recursion-count monitor)))
          (t
           (when (owner monitor)
             (let ((*java-thread-blocking-state* (list #x401 object monitor)))
               (loop while (owner monitor)
                     do (%monitor-sleep monitor))))
           (setf (owner monitor) current-thread
                 (recursion-count monitor) 1)))))))

(defun monitor-exit (object)
  "Exit the monitor for OBJECT, releasing its mutex."
  (when object
    (let* ((monitor (%get-monitor object))
           (mutex (mutex monitor))
           (current-thread (current-thread-identity)))
      (bordeaux-threads:with-lock-held (mutex)
        (unless (eq (owner monitor) current-thread)
          ;; JVM spec: monitorexit by a non-owner throws IllegalMonitorStateException.
          (error (%lisp-condition (%make-throwable '|java/lang/IllegalMonitorStateException|))))
        (decf (recursion-count monitor))
        (when (zerop (recursion-count monitor))
          (setf (owner monitor) nil)
          ;; Broadcast: threads entering share this condition variable with
          ;; threads in Object.wait(), and a single wakeup can land on a
          ;; wait(), leaving a thread asleep at the door of a free monitor.
          ;; notify() and notifyAll() already broadcast here.
          (%monitor-wake monitor))))))
