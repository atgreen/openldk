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

;;;
;;;
(in-package :openldk)

(defvar *cli-enable-native-access* nil)
(defvar *cli-illegal-native-access* "warn")

(defun %configure-native-access ()
  "Apply process permissions to the image's JDK module graph and policy enum."
  (let* ((bootstrap (symbol-value (intern "+static-jdk/internal/module/ModuleBootstrap+" :openldk)))
         (enum-class "jdk/internal/module/ModuleBootstrap$IllegalNativeAccess"))
    (%clinit (classload enum-class))
    (setf (slot-value bootstrap '|ILLEGAL_NATIVE_ACCESS|)
          (slot-value (symbol-value (intern (format nil "+static-~A+" enum-class) :openldk))
                      (intern (string-upcase *cli-illegal-native-access*) :openldk))))
  (let ((all-unnamed (slot-value (symbol-value (intern "+static-java/lang/Module+" :openldk)) '|ALL_UNNAMED_MODULE|)))
    ;; A warning in an image-building run must not grant the launched
    ;; application's unnamed modules permanent native permissions.
    (setf (slot-value all-unnamed '|enableNativeAccess|) 0)
    (dolist (name (remove-duplicates
                   (loop for names in *cli-enable-native-access*
                         append (split-sequence:split-sequence #\, names)) :test #'equal))
      (cond
        ((string= name "ALL-UNNAMED") (setf (slot-value all-unnamed '|enableNativeAccess|) 1))
        ((gethash name *boot-modules*)
         (|implAddEnableNativeAccess()Ljava/lang/Module;| (gethash name *boot-modules*)))
        ((plusp (length name))
         (format *error-output* "WARNING: Unknown module: ~A specified to --enable-native-access~%" name)))))
  nil)

(defun %jni-ensure-native-access (caller owner method &optional jni)
  "Use the JDK's shared JNI/FFM policy, warning deduplication and module flags."
  ;; ClassLoader.findNative checks automatically linked non-bootstrap
  ;; methods; native RegisterNatives bindings bypass that lookup entirely.
  (when (and jni caller (null (slot-value caller '|classLoader|)))
    (return-from %jni-ensure-native-access nil))
  (|jdk/internal/reflect/Reflection.ensureNativeAccess(Ljava/lang/Class;Ljava/lang/Class;Ljava/lang/String;Z)V|
   caller owner (jstring method) (if jni 1 0)))
