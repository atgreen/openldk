;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: OPENLDK; Base: 10 -*-
;;;
;;; Copyright (C) 2026  OpenLDK contributors
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
(in-package :openldk)

;;; Use the JDK's Java fallback linker with our own libffi JNI implementation,
;;; rather than the VM-generated assembly stubs or HotSpot's native libraries.
(defun |jdk/internal/vm/ForeignLinkerSupport.isSupported0()Z| ()
  "Use the libffi fallback instead of VM-generated native stubs."
  0)

(defun %ensure-foreign-bridge ()
  "Register OpenLDK's own fallback-linker JNI functions with the boot loader."
  (%jni-load-library (or (uiop:getenv "OPENLDK_JNI_BRIDGE") *jni-default-shim*) *boot-ldk-class-loader*))

(setf (gethash "jdk/internal/foreign/abi/fallback/LibFallback.tryLoadLibrary()Z" *native-overrides*)
      (lambda ()
        (%ensure-foreign-bridge)
        1))

(defun |jdk/internal/loader/RawNativeLibraries.load0(Ljdk/internal/loader/RawNativeLibraries$RawNativeLibraryImpl;Ljava/lang/String;)Z|
    (library name)
  "Open an ordinary C library without running JNI initialization hooks."
  (%ensure-foreign-bridge)
  (let ((handle (%jni-address (%jni-c "ldk_jni_open" sb-sys:system-area-pointer
                                    (sb-alien:c-string (lstring name))))))
    (setf (slot-value library '|handle|) handle)
    (if (zerop handle) 0 1)))

(defun |jdk/internal/loader/RawNativeLibraries.unload0(Ljava/lang/String;J)V| (name handle)
  "Release an arena-owned C library handle."
  (declare (ignore name))
  (%jni-c "ldk_jni_close" sb-alien:void
          (sb-sys:system-area-pointer (%jni-pointer handle))))

(defun |jdk/internal/loader/NativeLibrary.findEntry0(JLjava/lang/String;)J| (handle name)
  "Find a symbol in an ordinary C library."
  (%jni-address (%jni-c "ldk_jni_symbol" sb-sys:system-area-pointer
                        (sb-sys:system-area-pointer (%jni-pointer handle))
                        (sb-alien:c-string (lstring name)))))

(setf (gethash "jdk/internal/loader/NativeLibraries.find(Ljava/lang/String;)J" *native-overrides*)
      (lambda (this name)
        (let ((loader (get-ldk-loader-for-java-loader (slot-value this '|loader|)))
              (text (lstring name)))
          (bordeaux-threads:with-recursive-lock-held (*jni-lock*)
            (dolist (library *jni-libraries* 0)
              (when (and (member (jni-library-state library) '(:loading :loaded))
                         (eql (if loader (id loader) 0) (jni-library-loader-id library)))
                (let ((address (%jni-address
                                (%jni-c "ldk_jni_symbol" sb-sys:system-area-pointer
                                        (sb-sys:system-area-pointer (%jni-pointer (jni-library-address library)))
                                        (sb-alien:c-string text)))))
                  (unless (zerop address) (return address)))))))))

(setf (gethash "jdk/internal/foreign/SystemLookup.makeSystemLookup()Ljava/lang/foreign/SymbolLookup;"
               *native-overrides*)
      (lambda ()
        (slot-value (symbol-value '|+static-jdk/internal/foreign/SystemLookup+|) 'INSTANCE)))

(setf (gethash "jdk/internal/foreign/SystemLookup.find(Ljava/lang/String;)Ljava/util/Optional;"
               *native-overrides*)
      (lambda (this name)
        (declare (ignore this))
        (unless name (%throw '|java/lang/NullPointerException|))
        (%ensure-jni)
        (let* ((text (lstring name))
               (address (unless (find #\Null text)
                          (%jni-address (%jni-c "ldk_ffi_default_symbol" sb-sys:system-area-pointer
                                                (sb-alien:c-string text))))))
          (%clinit (classload "java/util/Optional"))
          (cond
            ((or (null address) (zerop address))
             (|java/util/Optional.empty()Ljava/util/Optional;|))
            (t
                (%clinit (classload "java/lang/foreign/MemorySegment"))
                (|java/util/Optional.of(Ljava/lang/Object;)Ljava/util/Optional;|
                 (|java/lang/foreign/MemorySegment.ofAddress(J)Ljava/lang/foreign/MemorySegment;| address)))))))
