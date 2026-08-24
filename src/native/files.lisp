;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: OPENLDK; Base: 10 -*-
;;;
;;; Copyright (C) 2024, 2025  Anthony Green <green@moxielogic.com>
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

;;; File, file-dispatcher, and channel natives.

(in-package :openldk)

;;; Shutdown.logRuntimeExit — JDK 21 tries to log via System.Logger which
;;; triggers AccessController.doPrivileged and other heavy infrastructure.
;;; Stub it out to avoid pulling in the entire security/logging stack.
(setf (gethash "java/lang/Shutdown.logRuntimeExit(I)V" *native-overrides*)
      (lambda (status) (declare (ignore status)) nil))

;;; AccessController.getProtectionDomain — native method used by the
;;; deprecated security manager.  Return nil (no protection domain).
(defun |java/security/AccessController.getProtectionDomain(Ljava/lang/Class;)| (class)
  (declare (ignore class))
  nil)

(defun |java/lang/Shutdown.halt0(I)| (status)
  (unless *ignore-quit*
    (uiop:quit status t)))

(defmethod |getRawAnnotations()| ((class |java/lang/Class|))
  (let ((lclass (get-ldk-class-for-java-class class)))
    (when (and lclass (attributes lclass))
      (gethash "RuntimeVisibleAnnotations" (attributes lclass)))))

(defun |java/awt/image/ColorModel.initIDs()| ()
  nil)

(defun |java/awt/Toolkit.initIDs()| ()
  "No-op HotSpot field-ID cache initialization for the Lisp object model."
  nil)

(defun |java/net/InetAddressImplFactory.isIPv6Supported()| ()
  0)

(defun |java/awt/image/IndexColorModel.initIDs()| ()
  nil)

(defun |java/awt/image/Raster.initIDs()| ()
  nil)

(defun |java/awt/image/SampleModel.initIDs()| ()
  nil)

(defun |sun/awt/image/ByteComponentRaster.initIDs()| ()
  nil)

(defun |sun/awt/image/BytePackedRaster.initIDs()| ()
  nil)

(defun |sun/awt/image/IntegerComponentRaster.initIDs()| ()
  nil)

(defun |java/util/zip/Deflater.initIDs()| ()
  nil)

(defun |java/util/zip/Inflater.initIDs()| ()
  nil)

(defun |java/security/SystemConfigurator.getSystemFIPSEnabled()| ()
  0)

(defun |java/lang/Package.getSystemPackage0(Ljava/lang/String;)| (name)
  (gethash (lstring name) *packages*))

(defun |java/lang/Thread.holdsLock(Ljava/lang/Object;)| (objref)
  (let ((monitor (%get-monitor objref))
        (current-thread (current-thread-identity)))
    (if (eq (owner monitor) current-thread)
        1 0)))

(defun |sun/nio/ch/IOUtil.iovMax()| ()
  0)

(defun |sun/nio/ch/IOUtil.writevMax()| ()
  "Maximum number of bytes writev(2) can transfer in one call.  We gather
in Lisp, so any generous cap works; Linux IOV_MAX * page-ish value."
  (* 1024 4096))

(defun |sun/nio/ch/FileChannelImpl.initIDs()| ()
  nil)

(defun |sun/nio/ch/NativeThread.init()| ()
  nil)

(defun |sun/nio/ch/NativeThread.current()| ()
  -1)

(defun |sun/nio/ch/NativeThread.current0()| ()
  "JDK 21+ native behind NativeThread.current().  Self-contained: the Java
current() wrapper calls current0(), so delegating back would recurse once
the wrapper is JIT-compiled over the Lisp current() stub."
  -1)

(defun |sun/nio/ch/NativeThread.signal(J)| (thread-id)
  "Signal a native thread blocked in an I/O operation. No-op in single-threaded OpenLDK."
  (declare (ignore thread-id))
  nil)

(defun |sun/nio/ch/NativeThread.signal0(J)| (thread-id)
  "JDK 21+ native behind NativeThread.signal(J).  Self-contained no-op (see
current0 for why it must not delegate to the wrapper name)."
  (declare (ignore thread-id))
  nil)

(defun |sun/nio/ch/NativeThread.supportPendingSignals0()| ()
  "Whether pending-signal delivery to blocked I/O threads is supported: no."
  0)

(defun |java/nio/Bits.pageSize()| ()
  4096)

(defmethod |pageSize()| ((unsafe |sun/misc/Unsafe|))
  4096)

(defmethod |storeFence()| ((unsafe |sun/misc/Unsafe|))
  "Ensures that stores before the fence will not be reordered with stores after the fence."
  (declare (ignore unsafe))
  (sb-thread:barrier (:memory)))

(defmethod |loadFence()| ((unsafe |sun/misc/Unsafe|))
  "Ensures that loads before the fence will not be reordered with loads after the fence."
  (declare (ignore unsafe))
  (sb-thread:barrier (:memory)))

(defmethod |fullFence()| ((unsafe |sun/misc/Unsafe|))
  "Ensures that loads and stores before the fence will not be reordered with those after the fence."
  (declare (ignore unsafe))
  (sb-thread:barrier (:memory)))

(defun |sun/nio/ch/FileDispatcherImpl.init()| ()
  nil)

(defun |sun/nio/ch/FileDispatcherImpl.size0(Ljava/io/FileDescriptor;)| (fd)
  "Return the size of the file associated with FD."
  (let ((real-fd (if (and (slot-exists-p fd '|fd|) (slot-boundp fd '|fd|))
                     (slot-value fd '|fd|)
                     fd)))
    (handler-case
        (let ((stat (sb-posix:fstat real-fd)))
          (sb-posix:stat-size stat))
      (sb-posix:syscall-error ()
        (error (%lisp-condition (%make-throwable '|java/io/IOException|)))))))

(defun |sun/nio/ch/FileDispatcherImpl.close0(Ljava/io/FileDescriptor;)| (fd)
  "Close the file descriptor."
  (let ((real-fd (if (and (slot-exists-p fd '|fd|) (slot-boundp fd '|fd|))
                     (slot-value fd '|fd|)
                     fd)))
    (handler-case
        (sb-posix:close real-fd)
      (sb-posix:syscall-error ()
        nil))))

(defun |sun/nio/ch/FileDispatcherImpl.preClose0(Ljava/io/FileDescriptor;)| (fd)
  "Pre-close — no-op in our implementation."
  (declare (ignore fd))
  nil)

(defun |sun/nio/ch/FileDispatcherImpl.closeIntFD(I)| (fd)
  "Close a raw int file descriptor."
  (handler-case (sb-posix:close fd)
    (sb-posix:syscall-error () nil)))

(defun |sun/nio/ch/FileDispatcherImpl.canTransferToFromOverlappedMap0()| ()
  0)

(defun |sun/nio/ch/FileChannelImpl.maxDirectTransferSize0()| ()
  ;; Linux default: 2GB
  (ash 1 31))

(defun |sun/nio/ch/FileDispatcherImpl.seek0(Ljava/io/FileDescriptor;J)| (fd offset)
  "lseek(2) — return current position or seek to OFFSET."
  (let ((real-fd (if (and (slot-exists-p fd '|fd|) (slot-boundp fd '|fd|))
                     (slot-value fd '|fd|)
                     fd)))
    (handler-case
        (if (= offset -1)
            ;; -1 means query current position (SEEK_CUR with offset 0)
            (sb-posix:lseek real-fd 0 sb-posix:seek-cur)
            (sb-posix:lseek real-fd offset sb-posix:seek-set))
      (sb-posix:syscall-error ()
        (error (%lisp-condition (%make-throwable '|java/io/IOException|)))))))

(defun |sun/nio/ch/FileDispatcherImpl.force0(Ljava/io/FileDescriptor;Z)| (fd metadata)
  "fsync/fdatasync the file descriptor."
  (declare (ignore metadata))
  (let ((real-fd (if (and (slot-exists-p fd '|fd|) (slot-boundp fd '|fd|))
                     (slot-value fd '|fd|)
                     fd)))
    (handler-case
        (sb-posix:fsync real-fd)
      (sb-posix:syscall-error ()
        (error (%lisp-condition (%make-throwable '|java/io/IOException|)))))))

(defun |sun/nio/ch/FileDispatcherImpl.write0(Ljava/io/FileDescriptor;JI)| (fd ptr length)
  "Write LENGTH bytes from native buffer at PTR to file descriptor FD."
  (let ((real-fd (if (and (slot-exists-p fd '|fd|) (slot-boundp fd '|fd|))
                     (slot-value fd '|fd|)
                     fd))
        (sap (sb-sys:int-sap ptr)))
    (sb-unix:unix-write real-fd sap 0 length)))

(defun |sun/nio/ch/FileDispatcherImpl.truncate0(Ljava/io/FileDescriptor;J)| (fd size)
  "Truncate the file to SIZE bytes."
  (let ((real-fd (if (and (slot-exists-p fd '|fd|) (slot-boundp fd '|fd|))
                     (slot-value fd '|fd|)
                     fd)))
    (handler-case
        (progn (sb-posix:ftruncate real-fd size) 0)
      (sb-posix:syscall-error ()
        (error (%lisp-condition (%make-throwable '|java/io/IOException|)))))))

(defun |sun/nio/ch/FileDispatcherImpl.read0(Ljava/io/FileDescriptor;JI)| (fd ptr length)
  "read(2) — read up to LENGTH bytes from FD into native buffer at PTR."
  (let ((real-fd (if (and (slot-exists-p fd '|fd|) (slot-boundp fd '|fd|))
                     (slot-value fd '|fd|)
                     fd))
        (sap (sb-sys:int-sap ptr)))
    (sb-unix:unix-read real-fd sap length)))

;;; JDK 25 moved the per-fd dispatcher natives from FileDispatcherImpl to
;;; UnixFileDispatcherImpl (FileDispatcherImpl keeps only init0/transfer*).
;;; Delegate to the existing implementations and add the iovec/positioned
;;; variants the old family lacked.

(defun |sun/nio/ch/FileDispatcherImpl.init0()| ()
  nil)

(defun %raw-fd (fd)
  "Unwrap a java/io/FileDescriptor object (or pass through a raw int fd)."
  (if (and (slot-exists-p fd '|fd|) (slot-boundp fd '|fd|))
      (slot-value fd '|fd|)
      fd))

(defun |sun/nio/ch/UnixFileDispatcherImpl.read0(Ljava/io/FileDescriptor;JI)| (fd ptr length)
  (|sun/nio/ch/FileDispatcherImpl.read0(Ljava/io/FileDescriptor;JI)| fd ptr length))

(defun |sun/nio/ch/UnixFileDispatcherImpl.write0(Ljava/io/FileDescriptor;JI)| (fd ptr length)
  (|sun/nio/ch/FileDispatcherImpl.write0(Ljava/io/FileDescriptor;JI)| fd ptr length))

(defun |sun/nio/ch/UnixFileDispatcherImpl.seek0(Ljava/io/FileDescriptor;J)| (fd offset)
  (|sun/nio/ch/FileDispatcherImpl.seek0(Ljava/io/FileDescriptor;J)| fd offset))

(defun |sun/nio/ch/UnixFileDispatcherImpl.size0(Ljava/io/FileDescriptor;)| (fd)
  (|sun/nio/ch/FileDispatcherImpl.size0(Ljava/io/FileDescriptor;)| fd))

(defun |sun/nio/ch/UnixFileDispatcherImpl.force0(Ljava/io/FileDescriptor;Z)| (fd metadata)
  (|sun/nio/ch/FileDispatcherImpl.force0(Ljava/io/FileDescriptor;Z)| fd metadata))

(defun |sun/nio/ch/UnixFileDispatcherImpl.truncate0(Ljava/io/FileDescriptor;J)| (fd size)
  (|sun/nio/ch/FileDispatcherImpl.truncate0(Ljava/io/FileDescriptor;J)| fd size))

(defun |sun/nio/ch/UnixFileDispatcherImpl.close0(Ljava/io/FileDescriptor;)| (fd)
  (|sun/nio/ch/FileDispatcherImpl.close0(Ljava/io/FileDescriptor;)| fd))

(defun |sun/nio/ch/UnixFileDispatcherImpl.preClose0(Ljava/io/FileDescriptor;)| (fd)
  (|sun/nio/ch/FileDispatcherImpl.preClose0(Ljava/io/FileDescriptor;)| fd))

(defun |sun/nio/ch/UnixFileDispatcherImpl.closeIntFD(I)| (fd)
  (|sun/nio/ch/FileDispatcherImpl.closeIntFD(I)| fd))

(defun |sun/nio/ch/UnixFileDispatcherImpl.pread0(Ljava/io/FileDescriptor;JIJ)| (fd ptr length offset)
  "pread(2) — positioned read into native buffer at PTR."
  (let ((real-fd (%raw-fd fd))
        (sap (sb-sys:int-sap ptr)))
    (sb-posix:lseek real-fd offset sb-posix:seek-set)
    (sb-unix:unix-read real-fd sap length)))

(defun |sun/nio/ch/UnixFileDispatcherImpl.pwrite0(Ljava/io/FileDescriptor;JIJ)| (fd ptr length offset)
  "pwrite(2) — positioned write from native buffer at PTR."
  (let ((real-fd (%raw-fd fd))
        (sap (sb-sys:int-sap ptr)))
    (sb-posix:lseek real-fd offset sb-posix:seek-set)
    (sb-unix:unix-write real-fd sap 0 length)))

(defun %iovec-base-and-len (iovec-address index)
  "Read struct iovec[INDEX] {iov_base, iov_len} at IOVEC-ADDRESS (LP64)."
  (let ((sap (sb-sys:int-sap (+ iovec-address (* index 16)))))
    (values (sb-sys:sap-ref-64 sap 0)
            (sb-sys:sap-ref-64 sap 8))))

(defun |sun/nio/ch/UnixFileDispatcherImpl.readv0(Ljava/io/FileDescriptor;JI)| (fd iovec-address count)
  "readv(2) — scatter read into COUNT iovec buffers."
  (let ((real-fd (%raw-fd fd))
        (total 0))
    (dotimes (i count total)
      (multiple-value-bind (base len) (%iovec-base-and-len iovec-address i)
        (let ((n (sb-unix:unix-read real-fd (sb-sys:int-sap base) len)))
          (when (or (null n) (minusp n))
            (return (if (zerop total)
                        (or n -1)
                        total)))
          (incf total n)
          (when (< n len)
            (return total)))))))

(defun |sun/nio/ch/UnixFileDispatcherImpl.writev0(Ljava/io/FileDescriptor;JI)| (fd iovec-address count)
  "writev(2) — gather write from COUNT iovec buffers."
  (let ((real-fd (%raw-fd fd))
        (total 0))
    (dotimes (i count total)
      (multiple-value-bind (base len) (%iovec-base-and-len iovec-address i)
        (let ((n (sb-unix:unix-write real-fd (sb-sys:int-sap base) 0 len)))
          (when (or (null n) (minusp n))
            (return (if (zerop total)
                        (or n -1)
                        total)))
          (incf total n)
          (when (< n len)
            (return total)))))))

(defun |sun/nio/ch/UnixFileDispatcherImpl.available0(Ljava/io/FileDescriptor;)| (fd)
  "Bytes available to read: size minus current position, or 0."
  (let ((real-fd (%raw-fd fd)))
    (handler-case
        (let ((size (sb-posix:stat-size (sb-posix:fstat real-fd)))
              (pos (sb-posix:lseek real-fd 0 sb-posix:seek-cur)))
          (max 0 (- size pos)))
      (sb-posix:syscall-error () 0))))

(defun |sun/nio/ch/UnixFileDispatcherImpl.isOther0(Ljava/io/FileDescriptor;)| (fd)
  "Whether FD refers to something other than a regular file/dir/link."
  (let ((real-fd (%raw-fd fd)))
    (handler-case
        (let ((mode (sb-posix:stat-mode (sb-posix:fstat real-fd))))
          (if (or (sb-posix:s-isreg mode)
                  (sb-posix:s-isdir mode)
                  (sb-posix:s-islnk mode))
              0
              1))
      (sb-posix:syscall-error () 0))))

(defun |sun/nio/ch/UnixFileDispatcherImpl.lock0(Ljava/io/FileDescriptor;ZJJZ)| (fd blocking pos size shared)
  "File locking — single-process runtime, report success."
  (declare (ignore fd blocking pos size shared))
  0)

(defun |sun/nio/ch/UnixFileDispatcherImpl.release0(Ljava/io/FileDescriptor;JJ)| (fd pos size)
  "Release a file lock — no-op."
  (declare (ignore fd pos size))
  nil)

(defun |sun/nio/ch/UnixFileDispatcherImpl.allocationGranularity0()| ()
  4096)

;;; ChannelInputStream.read([BII) native override — bypass heavy NIO path
;;; The Java NIO read path (FileChannelImpl → IOUtil → DirectByteBuffer
;;; allocation → Bits.tryReserveMemory CAS loops) triggers dozens of
;;; first-time JIT compilations.  Short-circuit with a direct read().
(setf (gethash "sun/nio/ch/ChannelInputStream.read([BII)I" *native-overrides*)
      (lambda (this b off len)
        (if (zerop len)
            0
            (let* ((ch (slot-value this '|ch|))
                   (fd-obj (when (typep ch '|sun/nio/ch/FileChannelImpl|)
                             (slot-value ch '|fd|)))
                   (fd (when fd-obj (slot-value fd-obj '|fd|))))
              (unless fd
                (unimplemented "ChannelInputStream.read: unsupported channel type ~A"
                       (type-of ch)))
              (let* ((mem (sb-alien:make-alien sb-alien:char len))
                     (sap (sb-alien:alien-sap mem))
                     (n (sb-unix:unix-read fd sap len)))
                (cond
                  ((and n (> n 0))
                   (let ((data (java-array-data b)))
                     (loop for i below n
                           do (setf (aref data (+ off i))
                                    (sb-sys:sap-ref-8 sap i))))
                   (sb-alien:free-alien mem)
                   n)
                  (t
                   (sb-alien:free-alien mem)
                   -1)))))))

;;; Channels$1.write([BII) native override — bypass heavy NIO write path
;;; The Java NIO write path (FileChannelImpl → IOUtil → DirectByteBuffer
;;; allocation → copyMemory) triggers complex memory management.
;;; Short-circuit with a direct write().
(setf (gethash "java/nio/channels/Channels$1.write([BII)V" *native-overrides*)
      (lambda (this b off len)
        (when (> len 0)
          (let* ((ch (slot-value this '|val$ch|))
                 (fd-obj (when (typep ch '|sun/nio/ch/FileChannelImpl|)
                           (slot-value ch '|fd|)))
                 (fd (when fd-obj (slot-value fd-obj '|fd|))))
            (unless fd
              (unimplemented "Channels$1.write: unsupported channel type ~A"
                     (type-of ch)))
            (let* ((data (java-array-data b))
                   (mem (sb-alien:make-alien sb-alien:char len))
                   (sap (sb-alien:alien-sap mem)))
              (loop for i below len
                    do (setf (sb-sys:sap-ref-8 sap i)
                             (let ((v (aref data (+ off i))))
                               (if (< v 0) (+ v 256) v))))
              (sb-unix:unix-write fd sap 0 len)
              (sb-alien:free-alien mem))))))

(defun |java/nio/MappedByteBuffer.checkBounds(III)| (off len size)
  (declare (ignore off)
           (ignore len)
           (ignore size))
  nil)


(defun |sun/nio/fs/UnixNativeDispatcher.init()| ()
  "Return capabilities bitmask: openat(2) + futimes(4) + futimens(8)."
  ;; Bit 1 (2)  = SUPPORTS_OPENAT
  ;; Bit 2 (4)  = SUPPORTS_FUTIMES
  ;; Bit 3 (8)  = SUPPORTS_FUTIMENS
  ;; Bit 4 (16) = SUPPORTS_LUTIMES
  ;; Bit 5 (32) = SUPPORTS_XATTR
  ;; Bit 16 (65536) = SUPPORTS_BIRTHTIME
  (logior 2 4 8))

(defun %read-c-string-from-sap (address)
  "Read a null-terminated C string from native memory at ADDRESS."
  (let ((sap (sb-sys:int-sap address)))
    (loop for i from 0
          for byte = (sb-sys:sap-ref-8 sap i)
          until (zerop byte)
          collect (code-char byte) into chars
          finally (return (coerce chars 'string)))))

(defun |sun/nio/fs/UnixNativeDispatcher.exists0(J)| (address)
  "Check if file at native C-string ADDRESS exists. Returns non-zero if so."
  (let ((path (%read-c-string-from-sap address)))
    (if (probe-file path) 1 0)))

(defun %populate-unix-file-attributes (path attrs follow-links)
  "Populate a UnixFileAttributes object from PATH. FOLLOW-LINKS controls symlink behavior."
  (handler-case
      (let ((stat (if follow-links
                      (sb-posix:stat path)
                      (sb-posix:lstat path))))
        (when (slot-exists-p attrs '|st_mode|)
          (setf (slot-value attrs '|st_mode|) (sb-posix:stat-mode stat)))
        (when (slot-exists-p attrs '|st_ino|)
          (setf (slot-value attrs '|st_ino|) (sb-posix:stat-ino stat)))
        (when (slot-exists-p attrs '|st_dev|)
          (setf (slot-value attrs '|st_dev|) (sb-posix:stat-dev stat)))
        (when (slot-exists-p attrs '|st_rdev|)
          (setf (slot-value attrs '|st_rdev|) (sb-posix:stat-rdev stat)))
        (when (slot-exists-p attrs '|st_nlink|)
          (setf (slot-value attrs '|st_nlink|) (sb-posix:stat-nlink stat)))
        (when (slot-exists-p attrs '|st_uid|)
          (setf (slot-value attrs '|st_uid|) (sb-posix:stat-uid stat)))
        (when (slot-exists-p attrs '|st_gid|)
          (setf (slot-value attrs '|st_gid|) (sb-posix:stat-gid stat)))
        (when (slot-exists-p attrs '|st_size|)
          (setf (slot-value attrs '|st_size|) (sb-posix:stat-size stat)))
        (when (slot-exists-p attrs '|st_atime_sec|)
          (setf (slot-value attrs '|st_atime_sec|) (sb-posix:stat-atime stat)))
        (when (slot-exists-p attrs '|st_mtime_sec|)
          (setf (slot-value attrs '|st_mtime_sec|) (sb-posix:stat-mtime stat)))
        (when (slot-exists-p attrs '|st_ctime_sec|)
          (setf (slot-value attrs '|st_ctime_sec|) (sb-posix:stat-ctime stat)))
        0) ; success
    (sb-posix:syscall-error (e)
      (sb-posix:syscall-errno e))))

(defun |sun/nio/fs/UnixNativeDispatcher.stat0(JLsun/nio/fs/UnixFileAttributes;)| (address attrs)
  "stat(2) — populate UnixFileAttributes.  JDK 25 contract: return 0 on
success or the errno value on failure; the Java caller throws the
UnixException itself."
  (%populate-unix-file-attributes (%read-c-string-from-sap address) attrs t))

(defun |sun/nio/fs/UnixNativeDispatcher.stat1(J)| (address)
  "stat(2) — returns st_mode on success, 0 on failure."
  (let ((path (%read-c-string-from-sap address)))
    (handler-case
        (sb-posix:stat-mode (sb-posix:stat path))
      (sb-posix:syscall-error (e)
        (declare (ignore e))
        0))))

(defun |sun/nio/fs/UnixNativeDispatcher.lstat0(JLsun/nio/fs/UnixFileAttributes;)| (address attrs)
  "lstat(2) — like stat0 but does not follow symlinks."
  (let ((path (%read-c-string-from-sap address)))
    (let ((result (%populate-unix-file-attributes path attrs nil)))
      (unless (zerop result)
        (let ((ux (%make-java-instance "sun/nio/fs/UnixException")))
          (when (slot-exists-p ux '|errno|)
            (setf (slot-value ux '|errno|) result))
          (error (%lisp-condition ux)))))))

(defun |sun/nio/fs/UnixNativeDispatcher.access0(JI)| (address mode)
  "access(2) — check file access.  JDK 25 contract: return 0 on success or
the errno value on failure; the Java caller throws the UnixException."
  (handler-case
      (progn
        (sb-posix:access (%read-c-string-from-sap address) mode)
        0)
    (sb-posix:syscall-error (e)
      (sb-posix:syscall-errno e))))

(defun |sun/nio/fs/UnixNativeDispatcher.open0(JII)| (address flags mode)
  "open(2) — open file. Returns file descriptor."
  (let ((path (%read-c-string-from-sap address)))
    (handler-case
        (sb-posix:open path flags mode)
      (sb-posix:syscall-error (e)
        (let ((ux (%make-java-instance "sun/nio/fs/UnixException")))
          (when (slot-exists-p ux '|errno|)
            (setf (slot-value ux '|errno|) (sb-posix:syscall-errno e)))
          (error (%lisp-condition ux)))))))

(defun |sun/nio/fs/UnixNativeDispatcher.close0(I)| (fd)
  "close(2) — close file descriptor."
  (handler-case
      (sb-posix:close fd)
    (sb-posix:syscall-error (e)
      (let ((ux (%make-java-instance "sun/nio/fs/UnixException")))
        (when (slot-exists-p ux '|errno|)
          (setf (slot-value ux '|errno|) (sb-posix:syscall-errno e)))
        (error (%lisp-condition ux))))))

(defun |sun/nio/fs/UnixNativeDispatcher.read0(IJI)| (fd address len)
  "read(2) — read from file descriptor into native buffer."
  (let ((sap (sb-sys:int-sap address)))
    (handler-case
        (let ((bytes-read 0))
          (loop for i below len
                for byte = (sb-posix:read fd (sb-sys:sap+ sap i) 1)
                while (plusp byte)
                do (incf bytes-read))
          bytes-read)
      (sb-posix:syscall-error (e)
        (let ((ux (%make-java-instance "sun/nio/fs/UnixException")))
          (when (slot-exists-p ux '|errno|)
            (setf (slot-value ux '|errno|) (sb-posix:syscall-errno e)))
          (error (%lisp-condition ux)))))))

(defun |sun/nio/fs/UnixNativeDispatcher.fstat0(ILsun/nio/fs/UnixFileAttributes;)| (fd attrs)
  "fstat(2) — stat by file descriptor."
  (handler-case
      (let ((stat (sb-posix:fstat fd)))
        (when (slot-exists-p attrs '|st_mode|)
          (setf (slot-value attrs '|st_mode|) (sb-posix:stat-mode stat)))
        (when (slot-exists-p attrs '|st_ino|)
          (setf (slot-value attrs '|st_ino|) (sb-posix:stat-ino stat)))
        (when (slot-exists-p attrs '|st_dev|)
          (setf (slot-value attrs '|st_dev|) (sb-posix:stat-dev stat)))
        (when (slot-exists-p attrs '|st_rdev|)
          (setf (slot-value attrs '|st_rdev|) (sb-posix:stat-rdev stat)))
        (when (slot-exists-p attrs '|st_nlink|)
          (setf (slot-value attrs '|st_nlink|) (sb-posix:stat-nlink stat)))
        (when (slot-exists-p attrs '|st_uid|)
          (setf (slot-value attrs '|st_uid|) (sb-posix:stat-uid stat)))
        (when (slot-exists-p attrs '|st_gid|)
          (setf (slot-value attrs '|st_gid|) (sb-posix:stat-gid stat)))
        (when (slot-exists-p attrs '|st_size|)
          (setf (slot-value attrs '|st_size|) (sb-posix:stat-size stat)))
        (when (slot-exists-p attrs '|st_atime_sec|)
          (setf (slot-value attrs '|st_atime_sec|) (sb-posix:stat-atime stat)))
        (when (slot-exists-p attrs '|st_mtime_sec|)
          (setf (slot-value attrs '|st_mtime_sec|) (sb-posix:stat-mtime stat)))
        (when (slot-exists-p attrs '|st_ctime_sec|)
          (setf (slot-value attrs '|st_ctime_sec|) (sb-posix:stat-ctime stat))))
    (sb-posix:syscall-error (e)
      (let ((ux (%make-java-instance "sun/nio/fs/UnixException")))
        (when (slot-exists-p ux '|errno|)
          (setf (slot-value ux '|errno|) (sb-posix:syscall-errno e)))
        (error (%lisp-condition ux))))))

(defun |sun/nio/fs/UnixNativeDispatcher.realpath0(J)| (address)
  "realpath(3) — resolve canonical path. Returns byte array."
  (let* ((path (%read-c-string-from-sap address))
         (real (namestring (truename (pathname path))))
         (bytes (flexi-streams:string-to-octets real :external-format :utf-8)))
    (make-java-array
     :component-class (%get-java-class-by-bin-name "byte")
     :initial-contents bytes)))

(defun |sun/nio/fs/UnixNativeDispatcher.getcwd()| ()
  (make-java-array
   :component-class (%get-java-class-by-bin-name "byte")
   :initial-contents (flexi-streams:string-to-octets (namestring (uiop:getcwd)) :external-format :utf-8)))

(defun |sun/nio/fs/UnixNativeDispatcher.dup(I)| (fd)
  "dup(2) — duplicate file descriptor."
  (handler-case
      (sb-posix:dup fd)
    (sb-posix:syscall-error (e)
      (let ((ux (%make-java-instance "sun/nio/fs/UnixException")))
        (when (slot-exists-p ux '|errno|)
          (setf (slot-value ux '|errno|) (sb-posix:syscall-errno e)))
        (error (%lisp-condition ux))))))

(defun |sun/nio/fs/UnixNativeDispatcher.openat0(IJII)| (dfd address flags mode)
  "openat(2) — open file relative to directory fd."
  (let* ((path (%read-c-string-from-sap address))
         (result (sb-alien:alien-funcall
                  (sb-alien:extern-alien "openat"
                                         (function sb-alien:int
                                                   sb-alien:int
                                                   sb-alien:c-string
                                                   sb-alien:int
                                                   sb-alien:int))
                  dfd path flags mode)))
    (when (< result 0)
      (let ((ux (%make-java-instance "sun/nio/fs/UnixException")))
        (when (slot-exists-p ux '|errno|)
          (setf (slot-value ux '|errno|) (sb-alien:get-errno)))
        (error (%lisp-condition ux))))
    result))

(defvar *dir-pointer-table* (make-hash-table)
  "Map from integer DIR* address to SBCL alien for closedir/readdir.")

(defun |sun/nio/fs/UnixNativeDispatcher.fdopendir(I)| (fd)
  "fdopendir(3) — open directory stream from fd. Returns DIR* as long."
  (let ((dirp (sb-alien:alien-funcall
               (sb-alien:extern-alien "fdopendir"
                                      (function (* t) sb-alien:int))
               fd)))
    (when (sb-alien:null-alien dirp)
      (let ((ux (%make-java-instance "sun/nio/fs/UnixException")))
        (when (slot-exists-p ux '|errno|)
          (setf (slot-value ux '|errno|) (sb-alien:get-errno)))
        (error (%lisp-condition ux))))
    (let ((addr (sb-sys:sap-int (sb-alien:alien-sap dirp))))
      (setf (gethash addr *dir-pointer-table*) dirp)
      addr)))

(defun |sun/nio/fs/UnixNativeDispatcher.closedir(J)| (dirp-addr)
  "closedir(3) — close directory stream."
  (let ((dirp (gethash dirp-addr *dir-pointer-table*)))
    (when dirp
      (remhash dirp-addr *dir-pointer-table*)
      (sb-posix:closedir dirp))))

(defun |sun/nio/fs/UnixNativeDispatcher.readdir0(J)| (dirp-addr)
  "readdir(3) — read directory entry. Returns filename as byte[] or nil.
This is the JDK 25 native name; the implementation lives here because a
compiled Java readdir wrapper may overwrite the old readdir(J) symbol."
  (let ((dirp (gethash dirp-addr *dir-pointer-table*)))
    (unless dirp (return-from |sun/nio/fs/UnixNativeDispatcher.readdir0(J)| nil))
    ;; Use the raw readdir(3) FFI to avoid SBCL's sb-posix:readdir trying to
    ;; naturalize d_name from a null dirent pointer at end-of-directory,
    ;; which causes a CORRUPTION WARNING (memory fault at 0x13 = d_name offset).
    (let ((entry (sb-alien:alien-funcall
                  (sb-alien:extern-alien "readdir"
                                         (function (* t) (* t)))
                  dirp)))
      (when (sb-alien:null-alien entry)
        (return-from |sun/nio/fs/UnixNativeDispatcher.readdir0(J)| nil))
      ;; d_name is at offset 19 in struct dirent on Linux x86-64
      (let* ((name (%read-c-string-from-sap (+ (sb-sys:sap-int (sb-alien:alien-sap entry)) 19)))
             (bytes (flexi-streams:string-to-octets name :external-format :utf-8)))
        (make-java-array
         :component-class (%get-java-class-by-bin-name "byte")
         :initial-contents bytes)))))

(defun |sun/nio/fs/UnixNativeDispatcher.readdir(J)| (dirp-addr)
  "Pre-JDK-25 name for readdir0."
  (|sun/nio/fs/UnixNativeDispatcher.readdir0(J)| dirp-addr))

(defun |sun/nio/fs/UnixNativeDispatcher.write(IJI)| (fd address len)
  "write(2) — write to file descriptor from native buffer."
  (let ((sap (sb-sys:int-sap address)))
    (handler-case
        (sb-posix:write fd sap len)
      (sb-posix:syscall-error (e)
        (let ((ux (%make-java-instance "sun/nio/fs/UnixException")))
          (when (slot-exists-p ux '|errno|)
            (setf (slot-value ux '|errno|) (sb-posix:syscall-errno e)))
          (error (%lisp-condition ux)))))))

(defun |sun/nio/fs/UnixNativeDispatcher.strerror(I)| (errno)
  "strerror(3) — return error description as byte[]."
  (let* ((msg (sb-int:strerror errno))
         (bytes (flexi-streams:string-to-octets msg :external-format :utf-8)))
    (make-java-array
     :component-class (%get-java-class-by-bin-name "byte")
     :initial-contents bytes)))

(defun |sun/nio/fs/UnixNativeDispatcher.getpwuid(I)| (uid)
  "getpwuid(3) — return user name as byte[]."
  (handler-case
      (let* ((pw (sb-posix:getpwuid uid))
             (name (sb-posix:passwd-name pw))
             (bytes (flexi-streams:string-to-octets name :external-format :utf-8)))
        (make-java-array
         :component-class (%get-java-class-by-bin-name "byte")
         :initial-contents bytes))
    (sb-posix:syscall-error (e)
      (let ((ux (%make-java-instance "sun/nio/fs/UnixException")))
        (when (slot-exists-p ux '|errno|)
          (setf (slot-value ux '|errno|) (sb-posix:syscall-errno e)))
        (error (%lisp-condition ux))))))

(defun |sun/nio/fs/UnixNativeDispatcher.getgrgid(I)| (gid)
  "getgrgid(3) — return group name as byte[]."
  (handler-case
      (let* ((gr (sb-posix:getgrgid gid))
             (name (sb-posix:group-name gr))
             (bytes (flexi-streams:string-to-octets name :external-format :utf-8)))
        (make-java-array
         :component-class (%get-java-class-by-bin-name "byte")
         :initial-contents bytes))
    (sb-posix:syscall-error (e)
      (let ((ux (%make-java-instance "sun/nio/fs/UnixException")))
        (when (slot-exists-p ux '|errno|)
          (setf (slot-value ux '|errno|) (sb-posix:syscall-errno e)))
        (error (%lisp-condition ux))))))

(defun |sun/nio/fs/UnixNativeDispatcher.fchmod(II)| (fd mode)
  "fchmod(2) — change file mode."
  (handler-case
      (sb-posix:fchmod fd mode)
    (sb-posix:syscall-error (e)
      (let ((ux (%make-java-instance "sun/nio/fs/UnixException")))
        (when (slot-exists-p ux '|errno|)
          (setf (slot-value ux '|errno|) (sb-posix:syscall-errno e)))
        (error (%lisp-condition ux))))))

(defun |sun/nio/fs/UnixNativeDispatcher.fchown(III)| (fd uid gid)
  "fchown(2) — change file owner."
  (handler-case
      (sb-posix:fchown fd uid gid)
    (sb-posix:syscall-error (e)
      (let ((ux (%make-java-instance "sun/nio/fs/UnixException")))
        (when (slot-exists-p ux '|errno|)
          (setf (slot-value ux '|errno|) (sb-posix:syscall-errno e)))
        (error (%lisp-condition ux))))))

(defun %throw-unix-exception (errno)
  "Throw a sun.nio.fs.UnixException carrying ERRNO."
  (let ((ux (%make-java-instance "sun/nio/fs/UnixException")))
    (when (slot-exists-p ux '|errno|)
      (setf (slot-value ux '|errno|) errno))
    (error (%lisp-condition ux))))

(defun |sun/nio/fs/UnixNativeDispatcher.unlink0(J)| (address)
  "unlink(2) — delete a file."
  (let ((path (%read-c-string-from-sap address)))
    (handler-case
        (sb-posix:unlink path)
      (sb-posix:syscall-error (e)
        (%throw-unix-exception (sb-posix:syscall-errno e))))))

(defun |sun/nio/fs/UnixNativeDispatcher.mkdir0(JI)| (address mode)
  "mkdir(2) — create a directory."
  (let ((path (%read-c-string-from-sap address)))
    (handler-case
        (sb-posix:mkdir path mode)
      (sb-posix:syscall-error (e)
        (%throw-unix-exception (sb-posix:syscall-errno e))))))

(defun |sun/nio/fs/UnixNativeDispatcher.rmdir0(J)| (address)
  "rmdir(2) — remove a directory."
  (let ((path (%read-c-string-from-sap address)))
    (handler-case
        (sb-posix:rmdir path)
      (sb-posix:syscall-error (e)
        (%throw-unix-exception (sb-posix:syscall-errno e))))))

(defun |sun/nio/fs/UnixNativeDispatcher.rename0(JJ)| (from-address to-address)
  "rename(2) — rename a file."
  (let ((from (%read-c-string-from-sap from-address))
        (to (%read-c-string-from-sap to-address)))
    (handler-case
        (sb-posix:rename from to)
      (sb-posix:syscall-error (e)
        (%throw-unix-exception (sb-posix:syscall-errno e))))))

(defun |sun/nio/fs/UnixNativeDispatcher.chmod0(JI)| (address mode)
  "chmod(2) — change file permissions."
  (let ((path (%read-c-string-from-sap address)))
    (handler-case
        (sb-posix:chmod path mode)
      (sb-posix:syscall-error (e)
        (%throw-unix-exception (sb-posix:syscall-errno e))))))

(defmethod |getUTF8At0(Ljava/lang/Object;I)| ((this |sun/reflect/ConstantPool|) cp index)
  (let* ((cp (constant-pool (ldk-class cp)))
         (s (format nil "~A" (emit (aref cp index) cp))))
    (jstring s)))

(defmethod |getIntAt0(Ljava/lang/Object;I)| ((this |sun/reflect/ConstantPool|) cp index)
  (let* ((cp (constant-pool (ldk-class cp)))
         (i (slot-value (aref cp index) 'value)))
    i))

