;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: OPENLDK; Base: 10 -*-
;;;
;;; Copyright (C) 2025  Anthony Green <green@moxielogic.com>
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

;;; jrt: filesystem support.
;;;
;;; OpenLDK's own class loading reads lib/modules directly (see
;;; jimage-classpath-entry in classpath.lisp).  But Java-side tools --
;;; javac in particular -- discover platform classes through the jrt: NIO
;;; filesystem (jdk.internal.jrtfs), which is pure Java layered on
;;; jdk.internal.jimage.ImageReader, itself pure Java once it can map the
;;; jimage file into a ByteBuffer.  Two native bridges make that whole
;;; stack work:
;;;
;;; 1. FileSystemProvider.installedProviders() normally discovers providers
;;;    through module-declared services, which OpenLDK does not support, so
;;;    the "jrt" scheme is never found.  Override it to return the default
;;;    provider plus a JrtFileSystemProvider (whose getFileSystem lazily
;;;    creates the filesystem, so no newFileSystem call is needed).
;;;
;;; 2. NativeImageBuffer.getNativeMap(path) is the native mmap that
;;;    libjimage provides in HotSpot (used when jdk.image.use.jvm.map is
;;;    true, the default).  mmap the file and hand back a DirectByteBuffer.

(defvar *installed-file-system-providers* nil
  "Memoized java.util.List returned by FileSystemProvider.installedProviders().
Reset before dumping an image: it captures a JrtFileSystemProvider whose
cached filesystem holds process-local state (mmapped buffers, channels).")

(setf (gethash "java/nio/file/spi/FileSystemProvider.installedProviders()Ljava/util/List;"
               *native-overrides*)
      (lambda ()
        (or *installed-file-system-providers*
            (setf *installed-file-system-providers*
                  (let ((list (%make-java-instance "java/util/ArrayList")))
                    (|<init>()| list)
                    ;; The platform default (file:) provider, when available.
                    (handler-case
                        (let* ((pkg (loader-package *boot-ldk-class-loader*))
                               (get-default (static-method-symbol
                                             "java/nio/file/FileSystems.getDefault()" pkg)))
                          (classload "java/nio/file/FileSystems")
                          (when (fboundp get-default)
                            (|add(Ljava/lang/Object;)| list
                             (|provider()| (funcall get-default)))))
                      (condition (c)
                        (format *error-output*
                                "~&; Warning: default FileSystemProvider unavailable: ~A~%" c)))
                    ;; The jrt: provider.
                    (handler-case
                        (progn
                          (classload "jdk/internal/jrtfs/JrtFileSystemProvider")
                          (let ((jrt (%make-java-instance "jdk/internal/jrtfs/JrtFileSystemProvider")))
                            (|<init>()| jrt)
                            (|add(Ljava/lang/Object;)| list jrt)))
                      (condition (c)
                        (format *error-output*
                                "~&; Warning: jrt FileSystemProvider unavailable: ~A~%" c)))
                    list)))))

(defvar *jimage-native-maps* (make-hash-table :test #'equal)
  "Path -> (sap . size) for mmapped jimage files.  Entries live for the
process; cleared (without munmap -- the process is about to dump or exit)
before image save.")

(defun |jdk/internal/jimage/NativeImageBuffer.getNativeMap(Ljava/lang/String;)| (path-jstring)
  "mmap the jimage file at PATH and return a DirectByteBuffer over it --
the job libjimage's JNI implementation does in HotSpot."
  (let* ((path (lstring path-jstring))
         (existing (gethash path *jimage-native-maps*))
         (sap-and-size
           (or existing
               (let* ((size (sb-posix:stat-size (sb-posix:stat path)))
                      (fd (sb-posix:open path sb-posix:o-rdonly))
                      (sap (unwind-protect
                                (sb-posix:mmap nil size sb-posix:prot-read
                                               sb-posix:map-private fd 0)
                             (sb-posix:close fd))))
                 (setf (gethash path *jimage-native-maps*) (cons sap size))))))
    (classload "java/nio/DirectByteBuffer")
    (let ((dbb (%make-java-instance "java/nio/DirectByteBuffer")))
      ;; JDK 25's JNI-NewDirectByteBuffer constructor is (long, long).
      (|<init>(JJ)| dbb
       (sb-sys:sap-int (car sap-and-size))
       (cdr sap-and-size))
      dbb)))
