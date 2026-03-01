;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: OPENLDK; Base: 10 -*-
;;;
;;; Copyright (C) 2025, 2026  Anthony Green <green@moxielogic.com>
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

;;; ============================================================================
;;; Virtual Thread Support via SBCL Fibers
;;;
;;; When SBCL is built with :sb-fiber, we use lightweight cooperative fibers
;;; for Java virtual threads instead of OS threads.  All fiber-specific code
;;; is gated on #+sb-fiber / #-sb-fiber reader conditionals so the system
;;; works identically on fiber-less SBCL builds.
;;;
;;; SBCL's grab-mutex, condition-wait, and condition-notify automatically
;;; dispatch to fiber-aware paths when *current-fiber* is bound, so monitors,
;;; wait/notify, and synchronized blocks work transparently without separate
;;; fiber code paths.  The fiber-specific code here handles: thread identity
;;; (fibers need distinct identity from their carrier), lifecycle (start0,
;;; isAlive, join), blocking primitives that bypass monitors (sleep, yield,
;;; LockSupport park/unpark), and the fiber scheduler.
;;; ============================================================================

;;; ---------------------------------------------------------------------------
;;; Thread identity abstraction
;;; ---------------------------------------------------------------------------

(defun current-thread-identity ()
  "Return the current thread identity for monitor ownership and mappings.
When executing in a fiber context, returns the fiber; otherwise returns the
OS thread via bordeaux-threads:current-thread."
  #+sb-fiber
  (let ((fiber (sb-thread:current-fiber)))
    (or fiber (bordeaux-threads:current-thread)))
  #-sb-fiber
  (bordeaux-threads:current-thread))

(defun in-fiber-p ()
  "Return T when the current code is executing inside a fiber."
  #+sb-fiber
  (not (null (sb-thread:current-fiber)))
  #-sb-fiber
  nil)

;;; ---------------------------------------------------------------------------
;;; Fiber-to-Java thread mappings
;;; ---------------------------------------------------------------------------

#+sb-fiber
(defvar *fiber-to-java-threads* (make-hash-table :test #'eq :synchronized t)
  "Map from SBCL fibers to Java Thread objects (for currentThread() in fiber context).")

#+sb-fiber
(defvar *java-to-fibers* (make-hash-table :test #'eq :synchronized t)
  "Map from Java Thread objects to SBCL fibers (for join(), isAlive(), unpark()).")

;;; ---------------------------------------------------------------------------
;;; Fiber park/unpark state
;;; ---------------------------------------------------------------------------

#+sb-fiber
(defvar *fiber-park-flags* (make-hash-table :test #'eq :synchronized t)
  "Map from fiber to permit flag (boolean) for LockSupport park/unpark semantics.")

;;; ---------------------------------------------------------------------------
;;; Fiber scheduler infrastructure
;;;
;;; Uses the multi-carrier start-fibers / submit-fiber / finish-fibers API.
;;; A fiber-scheduler-group is lazily created and fibers are submitted to it.
;;; ---------------------------------------------------------------------------

#+sb-fiber
(defvar *fiber-scheduler-group* nil
  "The fiber-scheduler-group handle for virtual thread execution.")

#+sb-fiber
(defvar *fiber-scheduler-lock* (bordeaux-threads:make-lock "fiber-scheduler-lock")
  "Lock protecting scheduler startup.")

#+sb-fiber
(defun ensure-fiber-scheduler ()
  "Lazily start the fiber scheduler group if not already running."
  (bordeaux-threads:with-lock-held (*fiber-scheduler-lock*)
    (when (null *fiber-scheduler-group*)
      (setf *fiber-scheduler-group*
            (sb-thread:start-fibers
             (list (sb-thread:make-fiber (lambda ()
                                           (loop (sb-thread:fiber-sleep 86400)))
                                         :name "keepalive")))))))

#+sb-fiber
(defun submit-virtual-thread-fiber (fiber)
  "Submit FIBER for execution on the fiber scheduler group."
  (ensure-fiber-scheduler)
  (sb-thread:submit-fiber *fiber-scheduler-group* fiber))
