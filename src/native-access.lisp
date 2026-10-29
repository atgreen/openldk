;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: OPENLDK; Base: 10 -*-
;;;
;;; Copyright (C) 2026 Anthony Green <green@moxielogic.com>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later WITH Classpath-exception-2.0

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
