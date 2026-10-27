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

(defun |java/lang/Shutdown.halt0(I)V| (status)
  (unless *ignore-quit*
    (uiop:quit status t)))

(defmethod |getRawAnnotations()[B| ((class |java/lang/Class|))
  (let ((lclass (get-ldk-class-for-java-class class)))
    (when (and lclass (attributes lclass))
      (gethash "RuntimeVisibleAnnotations" (attributes lclass)))))

;;; Type annotations: the RuntimeVisibleTypeAnnotations attribute of a
;;; class, method or field, NIL (none) when it has none.

(defmethod |getRawTypeAnnotations()[B| ((class |java/lang/Class|))
  (let ((lclass (get-ldk-class-for-java-class class)))
    (when (and lclass (attributes lclass))
      (gethash "RuntimeVisibleTypeAnnotations" (attributes lclass)))))

(defun %member-type-annotations (java-class name descriptor-prefix)
  "The RuntimeVisibleTypeAnnotations of the member of JAVA-CLASS named NAME
whose descriptor starts with DESCRIPTOR-PREFIX, or NIL."
  (let* ((lclass (get-ldk-class-for-java-class java-class))
         (member (and lclass
                      (find-if (lambda (m)
                                 (and (string= (name m) name)
                                      (eql 0 (search descriptor-prefix (descriptor m)))))
                               (if (string= descriptor-prefix "(" :end1 (min 1 (length descriptor-prefix)))
                                   (methods lclass)
                                   (fields lclass))))))
    (and member (attributes member)
         (gethash "RuntimeVisibleTypeAnnotations" (attributes member)))))

(defmethod |getTypeAnnotationBytes0()[B| (member)
  "Executable's and Field's native.  Unspecialized: java.lang.reflect's
classes aren't CLOS classes until they load (see Field.get in
reflection.lisp)."
  (if (slot-exists-p member '|parameterTypes|)
      (%member-type-annotations (slot-value member '|clazz|)
                                (if (slot-exists-p member '|returnType|)
                                    (lstring (slot-value member '|name|))
                                    "<init>")
                                (format nil "(~{~A~})"
                                        (map 'list #'%class->descriptor-string
                                             (java-array-data (slot-value member '|parameterTypes|)))))
      (%member-type-annotations (slot-value member '|clazz|)
                                (lstring (slot-value member '|name|))
                                (%class->descriptor-string (slot-value member '|type$|)))))

(defun |java/awt/image/ColorModel.initIDs()V| ()
  nil)

(defun |java/awt/Toolkit.initIDs()V| ()
  "No-op HotSpot field-ID cache initialization for the Lisp object model."
  nil)

(defun |java/net/InetAddressImplFactory.isIPv6Supported()| ()
  0)

;; JDK 25 asks InetAddress itself.  OpenLDK implements Inet4AddressImpl
;; only, so it reports IPv4 and no IPv6, as InetAddressImplFactory does.
(defun |java/net/InetAddress.isIPv4Available()Z| ()
  1)

(defun |java/net/InetAddress.isIPv6Supported()Z| ()
  0)

;; Native field-ID caching, as Inet4Address.init.
(defun |java/net/Inet6Address.init()V| ()
  nil)

(defun |java/awt/image/IndexColorModel.initIDs()V| ()
  nil)

(defun |java/awt/image/Raster.initIDs()V| ()
  nil)

(defun |java/awt/image/SampleModel.initIDs()V| ()
  nil)

(defun |sun/awt/image/ByteComponentRaster.initIDs()V| ()
  nil)

(defun |sun/awt/image/BytePackedRaster.initIDs()V| ()
  nil)

(defun |sun/awt/image/IntegerComponentRaster.initIDs()V| ()
  nil)

(defun |java/util/zip/Deflater.initIDs()| ()
  nil)

(defun |java/util/zip/Inflater.initIDs()V| ()
  nil)

(defun |java/security/SystemConfigurator.getSystemFIPSEnabled()| ()
  0)

(defun |java/lang/Package.getSystemPackage0(Ljava/lang/String;)| (name)
  (gethash (lstring name) *packages*))

(defun |java/lang/Thread.holdsLock(Ljava/lang/Object;)Z| (objref)
  (let ((monitor (%get-monitor objref))
        (current-thread (current-thread-identity)))
    (if (eq (owner monitor) current-thread)
        1 0)))

(defun |sun/nio/ch/IOUtil.iovMax()I| ()
  "Linux's IOV_MAX.  With 0, IOUtil's scattering and gathering transfers
move nothing."
  1024)

(defun |sun/nio/ch/IOUtil.writevMax()J| ()
  "Maximum number of bytes writev(2) can transfer in one call.  We gather
in Lisp, so any generous cap works; Linux IOV_MAX * page-ish value."
  (* 1024 4096))

(defun |sun/nio/ch/FileChannelImpl.initIDs()| ()
  nil)

(defun |sun/nio/ch/NativeThread.init()V| ()
  nil)

(defun |sun/nio/ch/NativeThread.current()J| ()
  0)

(defun |sun/nio/ch/NativeThread.current0()J| ()
  "JDK 21+ native behind NativeThread.current().  Self-contained: the Java
current() wrapper calls current0(), so delegating back would recurse once
the wrapper is JIT-compiled over the Lisp current() stub.  0 means this
platform can't signal native threads; -1 is the JDK's token for a virtual
thread, which would send blocking socket I/O to the virtual-thread poller."
  0)

(defun |sun/nio/ch/NativeThread.signal(J)V| (thread-id)
  "Signal a native thread blocked in an I/O operation. No-op in single-threaded OpenLDK."
  (declare (ignore thread-id))
  nil)

(defun |sun/nio/ch/NativeThread.signal0(J)V| (thread-id)
  "JDK 21+ native behind NativeThread.signal(J).  Self-contained no-op (see
current0 for why it must not delegate to the wrapper name)."
  (declare (ignore thread-id))
  nil)

(defun |sun/nio/ch/NativeThread.supportPendingSignals0()Z| ()
  "Whether pending-signal delivery to blocked I/O threads is supported: no."
  0)

(defun |java/nio/Bits.pageSize()I| ()
  4096)

(defmethod |pageSize()I| ((unsafe |sun/misc/Unsafe|))
  4096)

(defmethod |storeFence()V| ((unsafe |sun/misc/Unsafe|))
  "Ensures that stores before the fence will not be reordered with stores after the fence."
  (declare (ignore unsafe))
  (sb-thread:barrier (:memory)))

(defmethod |loadFence()V| ((unsafe |sun/misc/Unsafe|))
  "Ensures that loads before the fence will not be reordered with loads after the fence."
  (declare (ignore unsafe))
  (sb-thread:barrier (:memory)))

(defmethod |fullFence()V| ((unsafe |sun/misc/Unsafe|))
  "Ensures that loads and stores before the fence will not be reordered with those after the fence."
  (declare (ignore unsafe))
  (sb-thread:barrier (:memory)))

(defun %raw-fd (fd)
  "The OS file descriptor of FD: a java/io/FileDescriptor or a raw int fd.
FileInputStream keeps a Lisp file stream in its FileDescriptor; its
channel (FileInputStream.getChannel) reads that stream's descriptor."
  (let ((value (if (and (typep fd 'standard-object) (slot-exists-p fd '|fd|) (slot-boundp fd '|fd|))
                   (slot-value fd '|fd|)
                   fd)))
    (if (typep value 'sb-sys:fd-stream)
        (sb-sys:fd-stream-fd value)
        value)))

(defun |sun/nio/ch/FileDispatcherImpl.init()| ()
  nil)

(defun %file-input-native-fd (input)
  "INPUT's OS descriptor, rejecting closed or invalid FileInputStreams."
  (let ((handle (%stream-fd input)))
    (cond
      ((typep handle 'sb-sys:fd-stream)
       (unless (open-stream-p handle) (%throw '|java/io/IOException| "Stream Closed"))
       (sb-sys:fd-stream-fd handle))
      ((and (integerp handle) (not (minusp handle))) handle)
      (t (%throw '|java/io/IOException| "Invalid file descriptor")))))

(defmethod |isRegularFile0(Ljava/io/FileDescriptor;)Z| ((input |java/io/FileInputStream|) descriptor)
  "Test the open descriptor's file type for JDK bulk-read optimizations."
  (declare (ignore descriptor))
  (handler-case
      (if (sb-posix:s-isreg (sb-posix:stat-mode (sb-posix:fstat (%file-input-native-fd input)))) 1 0)
    (sb-posix:syscall-error () (%throw '|java/io/IOException| "Could not stat input file"))))

(defmethod |length0()J| ((input |java/io/FileInputStream|))
  "Return the size of INPUT's open file."
  (handler-case
      (sb-posix:stat-size (sb-posix:fstat (%file-input-native-fd input)))
    (sb-posix:syscall-error () (%throw '|java/io/IOException| "Could not determine input length"))))

(defun %file-input-position (input &optional (position nil position-p))
  "Query or set INPUT's logical position, accounting for Lisp read buffering."
  (let ((fd (%file-input-native-fd input))
        (handle (%stream-fd input)))
    (when (and position-p (not (<= 0 position #x7fffffffffffffff)))
      (%throw '|java/io/IOException| "Invalid input position"))
    (handler-case
        (if (typep handle 'sb-sys:fd-stream)
            (progn
              (when (and position-p (not (file-position handle position)))
                (%throw '|java/io/IOException| "Could not seek input file"))
              (or (file-position handle) (%throw '|java/io/IOException| "Input is not seekable")))
            (sb-posix:lseek fd (if position-p position 0)
                            (if position-p sb-posix:seek-set sb-posix:seek-cur)))
      (sb-posix:syscall-error () (%throw '|java/io/IOException| "Could not seek input file"))
      (stream-error () (%throw '|java/io/IOException| "Could not seek input stream")))))

(defmethod |position0()J| ((input |java/io/FileInputStream|))
  "Return INPUT's current logical read position."
  (%file-input-position input))

(defmethod |skip0(J)J| ((input |java/io/FileInputStream|) count)
  "Seek a regular file forward or backward, including beyond EOF."
  (let* ((before (%file-input-position input))
         (after (if (eql count :end) (|length0()J| input) (+ before count))))
    (%file-input-position input after)
    (- after before)))

(defun |sun/nio/ch/FileKey.init(Ljava/io/FileDescriptor;[J)V| (fd info)
  "Identify FD's file by device and inode for NIO's shared lock table."
  (handler-case
      (let ((stat (sb-posix:fstat (%raw-fd fd))))
        (setf (jaref info 0) (sb-posix:stat-dev stat)
              (jaref info 1) (sb-posix:stat-ino stat)))
    (sb-posix:syscall-error ()
      (error (%lisp-condition (%make-throwable '|java/io/IOException|))))))

(defun |sun/nio/ch/FileDispatcherImpl.size0(Ljava/io/FileDescriptor;)J| (fd)
  "Return the size of the file associated with FD."
  (let ((real-fd (%raw-fd fd)))
    (handler-case
        (let ((stat (sb-posix:fstat real-fd)))
          (sb-posix:stat-size stat))
      (sb-posix:syscall-error ()
        (error (%lisp-condition (%make-throwable '|java/io/IOException|)))))))

(defun |sun/nio/ch/FileDispatcherImpl.close0(Ljava/io/FileDescriptor;)| (fd)
  "Close the file descriptor."
  (let ((real-fd (%raw-fd fd)))
    (handler-case
        (sb-posix:close real-fd)
      (sb-posix:syscall-error ()
        nil))))

(defun |sun/nio/ch/FileDispatcherImpl.preClose0(Ljava/io/FileDescriptor;)| (fd)
  "Pre-close — no-op in our implementation."
  (declare (ignore fd))
  nil)

(defun |sun/nio/ch/FileDispatcherImpl.closeIntFD(I)V| (fd)
  "Close a raw int file descriptor."
  (handler-case (sb-posix:close fd)
    (sb-posix:syscall-error () nil)))

(defun |sun/nio/ch/FileDispatcherImpl.canTransferToFromOverlappedMap0()| ()
  0)

(defun |sun/nio/ch/FileChannelImpl.maxDirectTransferSize0()| ()
  ;; Linux default: 2GB
  (ash 1 31))

(defun |sun/nio/ch/FileDispatcherImpl.seek0(Ljava/io/FileDescriptor;J)J| (fd offset)
  "lseek(2) — return current position or seek to OFFSET."
  (let ((real-fd (%raw-fd fd)))
    (handler-case
        (if (= offset -1)
            ;; -1 means query current position (SEEK_CUR with offset 0)
            (sb-posix:lseek real-fd 0 sb-posix:seek-cur)
            (sb-posix:lseek real-fd offset sb-posix:seek-set))
      (sb-posix:syscall-error ()
        (error (%lisp-condition (%make-throwable '|java/io/IOException|)))))))

(defun |sun/nio/ch/FileDispatcherImpl.force0(Ljava/io/FileDescriptor;Z)I| (fd metadata)
  "fsync/fdatasync the file descriptor."
  (declare (ignore metadata))
  (let ((real-fd (%raw-fd fd)))
    (handler-case
        (sb-posix:fsync real-fd)
      (sb-posix:syscall-error ()
        (error (%lisp-condition (%make-throwable '|java/io/IOException|)))))))

(defmethod |sync0()V| ((descriptor |java/io/FileDescriptor|))
  "Flush Lisp output buffers and synchronize the descriptor with stable storage."
  (handler-case
      (let ((stream (slot-value descriptor '|fd|)))
        (when (typep stream 'sb-sys:fd-stream)
          (unless (open-stream-p stream)
            (%throw '|java/io/SyncFailedException| "Closed file descriptor"))
          (when (output-stream-p stream) (finish-output stream)))
        (let ((fd (%raw-fd descriptor)))
          (unless (and (integerp fd) (not (minusp fd)))
            (%throw '|java/io/SyncFailedException| "Invalid file descriptor"))
          (sb-posix:fsync fd)))
    (sb-posix:syscall-error () (%throw '|java/io/SyncFailedException| "Could not synchronize file"))
    (stream-error () (%throw '|java/io/SyncFailedException| "Could not flush file")))
  nil)

(defun |sun/nio/ch/FileDispatcherImpl.write0(Ljava/io/FileDescriptor;JI)I| (fd ptr length)
  "Write LENGTH bytes from native buffer at PTR to file descriptor FD."
  (let ((real-fd (%raw-fd fd))
        (sap (sb-sys:int-sap ptr)))
    (sb-unix:unix-write real-fd sap 0 length)))

(defun |sun/nio/ch/FileDispatcherImpl.truncate0(Ljava/io/FileDescriptor;J)I| (fd size)
  "Truncate the file to SIZE bytes."
  (let ((real-fd (%raw-fd fd)))
    (handler-case
        (progn (sb-posix:ftruncate real-fd size) 0)
      (sb-posix:syscall-error ()
        (error (%lisp-condition (%make-throwable '|java/io/IOException|)))))))

(defun %native-io-result (n errno)
  "The JDK's convention for a read(2)-style result N: -1 (IOStatus.EOF) at
end of file, and an IOException on failure."
  (cond
    ((null n)
     (error (%lisp-condition (%make-throwable '|java/io/IOException| (sb-int:strerror errno)))))
    ((zerop n) -1)
    (t n)))

(defun |sun/nio/ch/FileDispatcherImpl.read0(Ljava/io/FileDescriptor;JI)I| (fd ptr length)
  "read(2) — read up to LENGTH bytes from FD into native buffer at PTR."
  (let ((real-fd (%raw-fd fd))
        (sap (sb-sys:int-sap ptr)))
    (if (zerop length)
        0
        (multiple-value-call #'%native-io-result (sb-unix:unix-read real-fd sap length)))))

;;; JDK 25 moved the per-fd dispatcher natives from FileDispatcherImpl to
;;; UnixFileDispatcherImpl (FileDispatcherImpl keeps only init0/transfer*).
;;; Delegate to the existing implementations and add the iovec/positioned
;;; variants the old family lacked.

(defun |sun/nio/ch/FileDispatcherImpl.init0()V| ()
  nil)


(defun |sun/nio/ch/UnixFileDispatcherImpl.read0(Ljava/io/FileDescriptor;JI)I| (fd ptr length)
  (|sun/nio/ch/FileDispatcherImpl.read0(Ljava/io/FileDescriptor;JI)I| fd ptr length))

(defun |sun/nio/ch/UnixFileDispatcherImpl.write0(Ljava/io/FileDescriptor;JI)I| (fd ptr length)
  (|sun/nio/ch/FileDispatcherImpl.write0(Ljava/io/FileDescriptor;JI)I| fd ptr length))

(defun |sun/nio/ch/UnixFileDispatcherImpl.seek0(Ljava/io/FileDescriptor;J)J| (fd offset)
  (|sun/nio/ch/FileDispatcherImpl.seek0(Ljava/io/FileDescriptor;J)J| fd offset))

(defun |sun/nio/ch/UnixFileDispatcherImpl.size0(Ljava/io/FileDescriptor;)J| (fd)
  (|sun/nio/ch/FileDispatcherImpl.size0(Ljava/io/FileDescriptor;)J| fd))

(defun |sun/nio/ch/UnixFileDispatcherImpl.force0(Ljava/io/FileDescriptor;Z)I| (fd metadata)
  (|sun/nio/ch/FileDispatcherImpl.force0(Ljava/io/FileDescriptor;Z)I| fd metadata))

(defun |sun/nio/ch/UnixFileDispatcherImpl.truncate0(Ljava/io/FileDescriptor;J)I| (fd size)
  (|sun/nio/ch/FileDispatcherImpl.truncate0(Ljava/io/FileDescriptor;J)I| fd size))

(defun |sun/nio/ch/UnixFileDispatcherImpl.close0(Ljava/io/FileDescriptor;)| (fd)
  (|sun/nio/ch/FileDispatcherImpl.close0(Ljava/io/FileDescriptor;)| fd))

(defun |sun/nio/ch/UnixFileDispatcherImpl.preClose0(Ljava/io/FileDescriptor;)| (fd)
  (|sun/nio/ch/FileDispatcherImpl.preClose0(Ljava/io/FileDescriptor;)| fd))

(defun |sun/nio/ch/UnixFileDispatcherImpl.closeIntFD(I)V| (fd)
  (|sun/nio/ch/FileDispatcherImpl.closeIntFD(I)V| fd))

(defvar *file-mappings* (make-hash-table :synchronized t)
  "Address -> (path position length prot) of each live FileChannel.map.")

(defun |sun/nio/ch/UnixFileDispatcherImpl.map0(Ljava/io/FileDescriptor;IJJZ)J| (fd prot position length sync)
  "mmap(2) LENGTH bytes of FD from POSITION (page-aligned by the caller):
PROT is 0 (read-only), 1 (read-write) or 2 (private, copy-on-write).
Native memory in OpenLDK is addressed directly, so the mapping's address
serves the MappedByteBuffer as is."
  (declare (ignore sync))
  (let* ((raw-fd (%raw-fd fd))
         (address (sb-sys:sap-int
                   (sb-posix:mmap nil length
                                  (if (zerop prot) sb-posix:prot-read (logior sb-posix:prot-read sb-posix:prot-write))
                                  (if (= prot 2) sb-posix:map-private sb-posix:map-shared)
                                  raw-fd position))))
    ;; Kept to map the file again at this address in a restarted image.
    (setf (gethash address *file-mappings*)
          (list (ignore-errors (sb-posix:readlink (format nil "/proc/self/fd/~D" raw-fd)))
                position length prot))
    address))

(defun |sun/nio/ch/UnixFileDispatcherImpl.unmap0(JJ)I| (address length)
  (remhash address *file-mappings*)
  (sb-posix:munmap (sb-sys:int-sap address) length)
  0)

(defun |sun/nio/ch/UnixFileDispatcherImpl.pread0(Ljava/io/FileDescriptor;JIJ)I| (fd ptr length offset)
  "pread(2) — positioned read into native buffer at PTR."
  (let ((real-fd (%raw-fd fd))
        (sap (sb-sys:int-sap ptr)))
    (sb-posix:lseek real-fd offset sb-posix:seek-set)
    (if (zerop length)
        0
        (multiple-value-call #'%native-io-result (sb-unix:unix-read real-fd sap length)))))

(defun |sun/nio/ch/UnixFileDispatcherImpl.pwrite0(Ljava/io/FileDescriptor;JIJ)I| (fd ptr length offset)
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

(defun |sun/nio/ch/UnixFileDispatcherImpl.readv0(Ljava/io/FileDescriptor;JI)J| (fd iovec-address count)
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

(defun |sun/nio/ch/UnixFileDispatcherImpl.writev0(Ljava/io/FileDescriptor;JI)J| (fd iovec-address count)
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

(defun |sun/nio/ch/UnixFileDispatcherImpl.available0(Ljava/io/FileDescriptor;)I| (fd)
  "Bytes available to read: size minus current position, or 0."
  (let ((real-fd (%raw-fd fd)))
    (handler-case
        (let ((size (sb-posix:stat-size (sb-posix:fstat real-fd)))
              (pos (sb-posix:lseek real-fd 0 sb-posix:seek-cur)))
          (max 0 (- size pos)))
      (sb-posix:syscall-error () 0))))

(defun |sun/nio/ch/UnixFileDispatcherImpl.isOther0(Ljava/io/FileDescriptor;)Z| (fd)
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

(defun |sun/nio/ch/UnixFileDispatcherImpl.lock0(Ljava/io/FileDescriptor;ZJJZ)I| (fd blocking pos size shared)
  "Acquire a POSIX byte-range lock, preserving NIO's result codes."
  (let ((lock (make-instance 'sb-posix:flock
                            :type (if (zerop shared) sb-posix:f-wrlck sb-posix:f-rdlck)
                            :whence sb-posix:seek-set :start pos :len size)))
    (handler-case
        (progn
          (sb-posix:fcntl (%raw-fd fd)
                          (if (zerop blocking) sb-posix:f-setlk sb-posix:f-setlkw)
                          lock)
          0)
      (sb-posix:syscall-error (condition)
        (let ((errno (sb-posix:syscall-errno condition)))
          (cond
            ((or (= errno sb-posix:eagain) (= errno sb-posix:eacces)) -1)
            ((= errno sb-posix:eintr) 2)
            (t (error (%lisp-condition (%make-throwable '|java/io/IOException|))))))))))

(defun |sun/nio/ch/UnixFileDispatcherImpl.release0(Ljava/io/FileDescriptor;JJ)V| (fd pos size)
  "Release the POSIX byte-range lock held on FD."
  (handler-case
      (sb-posix:fcntl (%raw-fd fd) sb-posix:f-setlk
                      (make-instance 'sb-posix:flock :type sb-posix:f-unlck
                                     :whence sb-posix:seek-set :start pos :len size))
    (sb-posix:syscall-error ()
      (error (%lisp-condition (%make-throwable '|java/io/IOException|))))))

(defun |sun/nio/ch/UnixFileDispatcherImpl.allocationGranularity0()J| ()
  4096)

;;; ChannelInputStream.read([BII) native override — bypass heavy NIO path
;;; The Java NIO read path (FileChannelImpl → IOUtil → DirectByteBuffer
;;; allocation → Bits.tryReserveMemory CAS loops) triggers dozens of
;;; first-time JIT compilations.  Short-circuit with a direct read().
(setf (gethash "sun/nio/ch/ChannelInputStream.read([BII)I" *native-overrides*)
      (lambda (this b off len) (%channel-input-stream-read this b off len)))

(defun %channel-input-stream-read (this b off len)
        (if (zerop len)
            0
            (let* ((ch (slot-value this '|ch|))
                   (fd-obj (when (typep ch '|sun/nio/ch/FileChannelImpl|)
                             (slot-value ch '|fd|)))
                   (fd (when fd-obj (slot-value fd-obj '|fd|))))
              (unless fd
                ;; Another kind of channel (the jrt: file system's): read
                ;; through it, as ChannelInputStream does.
                (return-from %channel-input-stream-read
                  (|read(Ljava/nio/ByteBuffer;)|
                   ch (|java/nio/ByteBuffer.wrap([BII)Ljava/nio/ByteBuffer;| b off len))))
              (let* ((mem (sb-alien:make-alien sb-alien:char len))
                     (sap (sb-alien:alien-sap mem))
                     (n (sb-unix:unix-read fd sap len)))
                (cond
                  ((and n (> n 0))
                   (let ((data (java-array-data b)))
                     ;; Java bytes are signed.
                     (loop for i below n
                           do (setf (aref data (+ off i))
                                    (sb-sys:signed-sap-ref-8 sap i))))
                   (sb-alien:free-alien mem)
                   n)
                  (t
                   (sb-alien:free-alien mem)
                   -1))))))

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
            (if (null fd)
                ;; Another kind of channel: write through it, as Channels does.
                (let ((bb (|java/nio/ByteBuffer.wrap([BII)Ljava/nio/ByteBuffer;| b off len)))
                  (loop while (plusp (|remaining()| bb))
                        do (|write(Ljava/nio/ByteBuffer;)| ch bb)))
            (let* ((data (java-array-data b))
                   (mem (sb-alien:make-alien sb-alien:char len))
                   (sap (sb-alien:alien-sap mem)))
              (loop for i below len
                    do (setf (sb-sys:sap-ref-8 sap i)
                             (let ((v (aref data (+ off i))))
                               (if (< v 0) (+ v 256) v))))
              (sb-unix:unix-write fd sap 0 len)
              (sb-alien:free-alien mem)))))))

(defun |java/nio/MappedByteBuffer.checkBounds(III)| (off len size)
  (declare (ignore off)
           (ignore len)
           (ignore size))
  nil)


(defun |sun/nio/fs/UnixNativeDispatcher.init()I| ()
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

(defun %stat-nanoseconds (file follow-links)
  "The nanosecond parts of the access, modification and change times of
FILE (a path, or a file descriptor), as three values, which SB-POSIX's
STAT doesn't give; zeros where they aren't known.  Reads struct stat's
st_atim, st_mtim and st_ctim, at the same offsets on x86-64 and AArch64
Linux."
  #+(and linux (or x86-64 arm64))
  (let ((buffer (make-array 256 :element-type '(unsigned-byte 8)))
        (name (if (stringp file)
                  (sb-ext:string-to-octets file :external-format :utf-8 :null-terminate t)
                  (make-array 1 :element-type '(unsigned-byte 8)))))
    (sb-sys:with-pinned-objects (buffer name)
      (if (zerop (handler-case
                     (cond
                       ((not (stringp file))
                        (sb-alien:alien-funcall
                          (sb-alien:extern-alien "fstat" (function sb-alien:int sb-alien:int
                                                                   sb-sys:system-area-pointer))
                          file (sb-sys:vector-sap buffer)))
                       (follow-links
                        (sb-alien:alien-funcall
                         (sb-alien:extern-alien "stat" (function sb-alien:int sb-sys:system-area-pointer
                                                                 sb-sys:system-area-pointer))
                         (sb-sys:vector-sap name) (sb-sys:vector-sap buffer)))
                       (t
                        (sb-alien:alien-funcall
                         (sb-alien:extern-alien "lstat" (function sb-alien:int sb-sys:system-area-pointer
                                                                  sb-sys:system-area-pointer))
                         (sb-sys:vector-sap name) (sb-sys:vector-sap buffer))))
                   (error () -1)))
          (let ((sap (sb-sys:vector-sap buffer)))
            (values (sb-sys:signed-sap-ref-64 sap 80)
                    (sb-sys:signed-sap-ref-64 sap 96)
                    (sb-sys:signed-sap-ref-64 sap 112)))
          (values 0 0 0))))
  #-(and linux (or x86-64 arm64))
  (declare (ignore file follow-links))
  #-(and linux (or x86-64 arm64))
  (values 0 0 0))

(defun %set-time-nanoseconds (file attrs follow-links)
  "Set ATTRS' (a UnixFileAttributes) st_*time_nsec slots from FILE, a path
or a file descriptor."
  (multiple-value-bind (atime mtime ctime) (%stat-nanoseconds file follow-links)
    (loop for (slot value) in `((|st_atime_nsec| ,atime) (|st_mtime_nsec| ,mtime) (|st_ctime_nsec| ,ctime))
          when (slot-exists-p attrs slot)
            do (setf (slot-value attrs slot) value))))

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
        (%set-time-nanoseconds path attrs follow-links)
        0) ; success
    (sb-posix:syscall-error (e)
      (sb-posix:syscall-errno e))))

(defun |sun/nio/fs/UnixNativeDispatcher.stat0(JLsun/nio/fs/UnixFileAttributes;)I| (address attrs)
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

(defun |sun/nio/fs/UnixNativeDispatcher.lstat0(JLsun/nio/fs/UnixFileAttributes;)V| (address attrs)
  "lstat(2) — like stat0 but does not follow symlinks."
  (let ((path (%read-c-string-from-sap address)))
    (let ((result (%populate-unix-file-attributes path attrs nil)))
      (unless (zerop result)
        (let ((ux (%make-java-instance "sun/nio/fs/UnixException")))
          (when (slot-exists-p ux '|errno|)
            (setf (slot-value ux '|errno|) result))
          (error (%lisp-condition ux)))))))

(defun |sun/nio/fs/UnixNativeDispatcher.access0(JI)I| (address mode)
  "access(2) — check file access.  JDK 25 contract: return 0 on success or
the errno value on failure; the Java caller throws the UnixException."
  (handler-case
      (progn
        (sb-posix:access (%read-c-string-from-sap address) mode)
        0)
    (sb-posix:syscall-error (e)
      (sb-posix:syscall-errno e))))

(defun |sun/nio/fs/UnixNativeDispatcher.open0(JII)I| (address flags mode)
  "open(2) — open file. Returns file descriptor."
  (let ((path (%read-c-string-from-sap address)))
    (handler-case
        (sb-posix:open path flags mode)
      (sb-posix:syscall-error (e)
        (let ((ux (%make-java-instance "sun/nio/fs/UnixException")))
          (when (slot-exists-p ux '|errno|)
            (setf (slot-value ux '|errno|) (sb-posix:syscall-errno e)))
          (error (%lisp-condition ux)))))))

(defun |sun/nio/fs/UnixNativeDispatcher.close0(I)V| (fd)
  "close(2) — close file descriptor."
  (handler-case
      (sb-posix:close fd)
    (sb-posix:syscall-error (e)
      (let ((ux (%make-java-instance "sun/nio/fs/UnixException")))
        (when (slot-exists-p ux '|errno|)
          (setf (slot-value ux '|errno|) (sb-posix:syscall-errno e)))
        (error (%lisp-condition ux))))))

(defun |sun/nio/fs/UnixNativeDispatcher.read0(IJI)I| (fd address len)
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

(defun |sun/nio/fs/UnixNativeDispatcher.fstat0(ILsun/nio/fs/UnixFileAttributes;)V| (fd attrs)
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
          (setf (slot-value attrs '|st_ctime_sec|) (sb-posix:stat-ctime stat)))
        (%set-time-nanoseconds fd attrs nil))
    (sb-posix:syscall-error (e)
      (let ((ux (%make-java-instance "sun/nio/fs/UnixException")))
        (when (slot-exists-p ux '|errno|)
          (setf (slot-value ux '|errno|) (sb-posix:syscall-errno e)))
        (error (%lisp-condition ux))))))

(defun %realpath-octets (path-sap)
  "realpath(3) of the C string at PATH-SAP: its octets, or the errno."
  (let ((real (sb-alien:alien-funcall
               (sb-alien:extern-alien "realpath" (function sb-sys:system-area-pointer
                                                           sb-sys:system-area-pointer
                                                           sb-sys:system-area-pointer))
               path-sap (sb-sys:int-sap 0))))
    (if (zerop (sb-sys:sap-int real))
        (values nil (sb-alien:get-errno))
        (unwind-protect
             (let ((length (loop for i from 0 until (zerop (sb-sys:sap-ref-8 real i)) finally (return i))))
               (let ((octets (make-array length :element-type '(unsigned-byte 8))))
                 (dotimes (i length octets)
                   (setf (aref octets i) (sb-sys:sap-ref-8 real i)))))
          (sb-alien:alien-funcall
           (sb-alien:extern-alien "free" (function sb-alien:void sb-sys:system-area-pointer))
           real)))))

(defun %realpath (path)
  "realpath(3) of the string PATH, or NIL if it doesn't resolve.  Unlike
TRUENAME, a directory has no trailing slash and no character is a wildcard."
  (let ((octets (sb-ext:string-to-octets path :external-format :utf-8 :null-terminate t)))
    (sb-sys:with-pinned-objects (octets)
      (let ((real (%realpath-octets (sb-sys:vector-sap octets))))
        (and real (sb-ext:octets-to-string real :external-format :utf-8))))))

(defun |sun/nio/fs/UnixNativeDispatcher.realpath0(J)[B| (address)
  "realpath(3) — resolve canonical path. Returns byte array."
  (multiple-value-bind (bytes errno) (%realpath-octets (sb-sys:int-sap address))
    (unless bytes
      (let ((ux (%make-java-instance "sun/nio/fs/UnixException")))
        (when (slot-exists-p ux '|errno|)
          (setf (slot-value ux '|errno|) errno))
        (error (%lisp-condition ux))))
    (make-java-array
     :component-class (%get-java-class-by-bin-name "byte")
     :initial-contents bytes)))

(defun |sun/nio/fs/UnixNativeDispatcher.getcwd()[B| ()
  (make-java-array
   :component-class (%get-java-class-by-bin-name "byte")
   :initial-contents (flexi-streams:string-to-octets (namestring (uiop:getcwd)) :external-format :utf-8)))

(defun |sun/nio/fs/UnixNativeDispatcher.dup(I)I| (fd)
  "dup(2) — duplicate file descriptor."
  (handler-case
      (sb-posix:dup fd)
    (sb-posix:syscall-error (e)
      (let ((ux (%make-java-instance "sun/nio/fs/UnixException")))
        (when (slot-exists-p ux '|errno|)
          (setf (slot-value ux '|errno|) (sb-posix:syscall-errno e)))
        (error (%lisp-condition ux))))))

(defun |sun/nio/fs/UnixNativeDispatcher.openat0(IJII)I| (dfd address flags mode)
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

(defun |sun/nio/fs/UnixNativeDispatcher.fdopendir(I)J| (fd)
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

(defun |sun/nio/fs/UnixNativeDispatcher.closedir(J)V| (dirp-addr)
  "closedir(3) — close directory stream."
  (let ((dirp (gethash dirp-addr *dir-pointer-table*)))
    (when dirp
      (remhash dirp-addr *dir-pointer-table*)
      (sb-posix:closedir dirp))))

(defun |sun/nio/fs/UnixNativeDispatcher.readdir0(J)[B| (dirp-addr)
  "readdir(3) — read directory entry. Returns filename as byte[] or nil.
This is the JDK 25 native name; the implementation lives here because a
compiled Java readdir wrapper may overwrite the old readdir(J) symbol."
  (let ((dirp (gethash dirp-addr *dir-pointer-table*)))
    (unless dirp (return-from |sun/nio/fs/UnixNativeDispatcher.readdir0(J)[B| nil))
    ;; Use the raw readdir(3) FFI to avoid SBCL's sb-posix:readdir trying to
    ;; naturalize d_name from a null dirent pointer at end-of-directory,
    ;; which causes a CORRUPTION WARNING (memory fault at 0x13 = d_name offset).
    (let ((entry (sb-alien:alien-funcall
                  (sb-alien:extern-alien "readdir"
                                         (function (* t) (* t)))
                  dirp)))
      (when (sb-alien:null-alien entry)
        (return-from |sun/nio/fs/UnixNativeDispatcher.readdir0(J)[B| nil))
      ;; d_name is at offset 19 in struct dirent on Linux x86-64
      (let* ((name (%read-c-string-from-sap (+ (sb-sys:sap-int (sb-alien:alien-sap entry)) 19)))
             (bytes (flexi-streams:string-to-octets name :external-format :utf-8)))
        (make-java-array
         :component-class (%get-java-class-by-bin-name "byte")
         :initial-contents bytes)))))

(defun |sun/nio/fs/UnixNativeDispatcher.readdir(J)[B| (dirp-addr)
  "Pre-JDK-25 name for readdir0."
  (|sun/nio/fs/UnixNativeDispatcher.readdir0(J)[B| dirp-addr))

(defun |sun/nio/fs/UnixNativeDispatcher.write(IJI)I| (fd address len)
  "write(2) — write to file descriptor from native buffer."
  (let ((sap (sb-sys:int-sap address)))
    (handler-case
        (sb-posix:write fd sap len)
      (sb-posix:syscall-error (e)
        (let ((ux (%make-java-instance "sun/nio/fs/UnixException")))
          (when (slot-exists-p ux '|errno|)
            (setf (slot-value ux '|errno|) (sb-posix:syscall-errno e)))
          (error (%lisp-condition ux)))))))

(defun |sun/nio/fs/UnixNativeDispatcher.strerror(I)[B| (errno)
  "strerror(3) — return error description as byte[]."
  (let* ((msg (sb-int:strerror errno))
         (bytes (flexi-streams:string-to-octets msg :external-format :utf-8)))
    (make-java-array
     :component-class (%get-java-class-by-bin-name "byte")
     :initial-contents bytes)))

(defun |sun/nio/fs/UnixNativeDispatcher.getpwuid(I)[B| (uid)
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

(defun |sun/nio/fs/UnixNativeDispatcher.getgrgid(I)[B| (gid)
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

(defun |sun/nio/fs/UnixNativeDispatcher.fchmod(II)V| (fd mode)
  "fchmod(2) — change file mode."
  (handler-case
      (sb-posix:fchmod fd mode)
    (sb-posix:syscall-error (e)
      (let ((ux (%make-java-instance "sun/nio/fs/UnixException")))
        (when (slot-exists-p ux '|errno|)
          (setf (slot-value ux '|errno|) (sb-posix:syscall-errno e)))
        (error (%lisp-condition ux))))))

(defun |sun/nio/fs/UnixNativeDispatcher.fchown(III)V| (fd uid gid)
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

(defun |sun/nio/fs/UnixNativeDispatcher.unlink0(J)V| (address)
  "unlink(2) — delete a file."
  (let ((path (%read-c-string-from-sap address)))
    (handler-case
        (sb-posix:unlink path)
      (sb-posix:syscall-error (e)
        (%throw-unix-exception (sb-posix:syscall-errno e))))))

(defun |sun/nio/fs/UnixNativeDispatcher.mkdir0(JI)V| (address mode)
  "mkdir(2) — create a directory."
  (let ((path (%read-c-string-from-sap address)))
    (handler-case
        (sb-posix:mkdir path mode)
      (sb-posix:syscall-error (e)
        (%throw-unix-exception (sb-posix:syscall-errno e))))))

(defun |sun/nio/fs/UnixNativeDispatcher.rmdir0(J)V| (address)
  "rmdir(2) — remove a directory."
  (let ((path (%read-c-string-from-sap address)))
    (handler-case
        (sb-posix:rmdir path)
      (sb-posix:syscall-error (e)
        (%throw-unix-exception (sb-posix:syscall-errno e))))))

(defun |sun/nio/fs/UnixNativeDispatcher.rename0(JJ)V| (from-address to-address)
  "rename(2) — rename a file."
  (let ((from (%read-c-string-from-sap from-address))
        (to (%read-c-string-from-sap to-address)))
    (handler-case
        (sb-posix:rename from to)
      (sb-posix:syscall-error (e)
        (%throw-unix-exception (sb-posix:syscall-errno e))))))

(defun |sun/nio/fs/UnixNativeDispatcher.chmod0(JI)V| (address mode)
  "chmod(2) — change file permissions."
  (let ((path (%read-c-string-from-sap address)))
    (handler-case
        (sb-posix:chmod path mode)
      (sb-posix:syscall-error (e)
        (%throw-unix-exception (sb-posix:syscall-errno e))))))

(defmethod |getUTF8At0(Ljava/lang/Object;I)Ljava/lang/String;| ((this |sun/reflect/ConstantPool|) cp index)
  (let* ((cp (constant-pool (ldk-class cp)))
         (s (format nil "~A" (emit (aref cp index) cp))))
    (jstring s)))

(defmethod |getIntAt0(Ljava/lang/Object;I)I| ((this |sun/reflect/ConstantPool|) cp index)
  (let* ((cp (constant-pool (ldk-class cp)))
         (i (slot-value (aref cp index) 'value)))
    i))
