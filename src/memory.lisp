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

(defvar *java-memory-lock* (bordeaux-threads:make-recursive-lock "Java ordered memory"))

(defmacro with-java-memory-ordering (&body body)
  "Conservatively serialize ordered Java accesses and fence ordinary memory.
Acquire/release and opaque accesses may legally provide stronger ordering."
  `(bordeaux-threads:with-recursive-lock-held (*java-memory-lock*)
     (sb-thread:barrier (:memory))
     (unwind-protect (progn ,@body)
       (sb-thread:barrier (:memory)))))

(defun %shared-memory-session-p (session)
  "True for a shared FFM session whose close can race a memory access."
  (and session
       (find '|jdk/internal/foreign/SharedSession|
             (closer-mop:class-precedence-list (class-of session))
             :key #'class-name)))

(defmacro with-scoped-memory-access ((&rest sessions) &body body)
  "Exclude shared-session teardown until the checked memory access finishes."
  (let ((access (gensym "ACCESS")))
    `(flet ((,access () ,@body))
       (if ,(cons 'or (loop for session in sessions collect `(%shared-memory-session-p ,session)))
           (with-java-memory-ordering (,access))
           (,access)))))
