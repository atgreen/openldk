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

;;; Linux UDP and native address layouts, using libc rather than JDK libraries.
(in-package :openldk)

;; These describe the same Linux sockaddr ABI used by native/net.lisp.
;; IPv6 layout information is required even when only IPv4 sockets are enabled.
(defun |sun/nio/ch/NativeSocketAddress.AFINET()I| () sb-posix:af-inet)
(defun |sun/nio/ch/NativeSocketAddress.AFINET6()I| () 10)
(defun |sun/nio/ch/NativeSocketAddress.sizeofSockAddr4()I| () 16)
(defun |sun/nio/ch/NativeSocketAddress.sizeofSockAddr6()I| () 28)
(defun |sun/nio/ch/NativeSocketAddress.sizeofFamily()I| () 2)
(defun |sun/nio/ch/NativeSocketAddress.offsetFamily()I| () 0)
(defun |sun/nio/ch/NativeSocketAddress.offsetSin4Port()I| () 2)
(defun |sun/nio/ch/NativeSocketAddress.offsetSin4Addr()I| () 4)
(defun |sun/nio/ch/NativeSocketAddress.offsetSin6Port()I| () 2)
(defun |sun/nio/ch/NativeSocketAddress.offsetSin6Addr()I| () 8)
(defun |sun/nio/ch/NativeSocketAddress.offsetSin6ScopeId()I| () 24)
(defun |sun/nio/ch/NativeSocketAddress.offsetSin6FlowInfo()I| () 4)

(defun %datagram-result (result &key reading socket-error)
  "Preserve IOStatus results and distinguish an unreachable UDP destination."
  (when (minusp result)
    (let ((errno (sb-alien:get-errno)))
      (when (= errno sb-posix:econnrefused)
        (%throw-errno '|java/net/PortUnreachableException| errno))
      (when (and socket-error (not (member errno (list sb-posix:eagain sb-posix:eintr))))
        (%throw-socket-error errno))))
  (%io-result result reading))

(defun |sun/nio/ch/DatagramChannelImpl.receive0(Ljava/io/FileDescriptor;JIJZ)I|
    (fd address length sender connected)
  ;; recvfrom writes the sender's actual sockaddr into Java's native allocation.
  ;; A zero-byte result is a received packet, not stream EOF.
  (sb-alien:with-alien ((size sb-alien:unsigned-int 28))
    (loop
      (let ((result (%libc "recvfrom" sb-alien:long (sb-alien:int (%raw-fd fd))
                      (sb-sys:system-area-pointer (sb-sys:int-sap address))
                      (sb-alien:unsigned-long (min length 65536)) (sb-alien:int 0)
                      (sb-sys:system-area-pointer (sb-sys:int-sap sender))
                      ((* sb-alien:unsigned-int) (sb-alien:addr size)))))
        (unless (and (minusp result) (= (sb-alien:get-errno) sb-posix:econnrefused) (zerop connected))
          (return (%datagram-result result :socket-error t)))))))

(defun |sun/nio/ch/DatagramChannelImpl.send0(Ljava/io/FileDescriptor;JIJI)I|
    (fd address length target target-length)
  (%datagram-result
   (%libc "sendto" sb-alien:long (sb-alien:int (%raw-fd fd))
     (sb-sys:system-area-pointer (sb-sys:int-sap address))
     (sb-alien:unsigned-long (min length 65536)) (sb-alien:int 0)
     (sb-sys:system-area-pointer (sb-sys:int-sap target)) (sb-alien:unsigned-int target-length))
   :socket-error t))

(defun |sun/nio/ch/DatagramChannelImpl.disconnect0(Ljava/io/FileDescriptor;Z)V| (fd ipv6)
  (sb-alien:with-alien ((address (array sb-alien:unsigned-char 28)))
    (dotimes (i 28) (setf (sb-alien:deref address i) 0))
    ;; Linux disconnects a datagram socket with an AF_UNSPEC address.
    (when (minusp (%libc "connect" sb-alien:int (sb-alien:int (%raw-fd fd))
                    ((* (array sb-alien:unsigned-char 28)) (sb-alien:addr address))
                    (sb-alien:unsigned-int (if (zerop ipv6) 16 28))))
      (%throw-socket-error (sb-alien:get-errno))))
  nil)

(defun |sun/nio/ch/DatagramDispatcher.read0(Ljava/io/FileDescriptor;JI)I| (fd address length)
  (%datagram-result
   (%libc "recv" sb-alien:long (sb-alien:int (%raw-fd fd))
     (sb-sys:system-area-pointer (sb-sys:int-sap address)) (sb-alien:unsigned-long length) (sb-alien:int 0))
   :reading t))

(defun |sun/nio/ch/DatagramDispatcher.write0(Ljava/io/FileDescriptor;JI)I| (fd address length)
  (%datagram-result
   (%libc "send" sb-alien:long (sb-alien:int (%raw-fd fd))
     (sb-sys:system-area-pointer (sb-sys:int-sap address)) (sb-alien:unsigned-long length) (sb-alien:int 0))))

(sb-alien:define-alien-type openldk-datagram-message
  (sb-alien:struct openldk-datagram-message
    (name (* t)) (name-length sb-alien:unsigned-int)
    (vectors (* t)) (vector-count sb-alien:unsigned-long)
    (control (* t)) (control-length sb-alien:unsigned-long) (flags sb-alien:int)))

(defun %datagram-vectors (fd address count reading)
  "Transfer one connected datagram across all buffers of a scattering/gathering operation."
  (sb-alien:with-alien ((message openldk-datagram-message))
    (%libc "memset" (* t) ((* openldk-datagram-message) (sb-alien:addr message))
      (sb-alien:int 0) (sb-alien:unsigned-long (sb-alien:alien-size openldk-datagram-message :bytes)))
    (setf (sb-alien:slot message 'vectors) (sb-alien:sap-alien (sb-sys:int-sap address) (* t))
          (sb-alien:slot message 'vector-count) (min count 1024))
    (%datagram-result
     (if reading
         (%libc "recvmsg" sb-alien:long (sb-alien:int (%raw-fd fd))
           ((* openldk-datagram-message) (sb-alien:addr message)) (sb-alien:int 0))
         (%libc "sendmsg" sb-alien:long (sb-alien:int (%raw-fd fd))
           ((* openldk-datagram-message) (sb-alien:addr message)) (sb-alien:int 0)))
     :reading reading)))

(defun |sun/nio/ch/DatagramDispatcher.readv0(Ljava/io/FileDescriptor;JI)J| (fd address count)
  (%datagram-vectors fd address count t))
(defun |sun/nio/ch/DatagramDispatcher.writev0(Ljava/io/FileDescriptor;JI)J| (fd address count)
  (%datagram-vectors fd address count nil))
(defun |sun/nio/ch/DatagramDispatcher.dup0(Ljava/io/FileDescriptor;Ljava/io/FileDescriptor;)V| (source target)
  (%check-io (%libc "dup2" sb-alien:int (sb-alien:int (%raw-fd source)) (sb-alien:int (%raw-fd target))))
  nil)

(defun %put-network-address (memory offset address)
  "Store the network-order bytes of an IPv4 integer in native MEMORY."
  (dotimes (i 4)
    (setf (sb-sys:sap-ref-8 memory (+ offset i)) (ldb (byte 8 (* 8 (- 3 i))) address))))

(defun %ipv4-membership (fd option group interface source)
  "Apply a Linux IPv4 multicast membership or source-filter option."
  (sb-alien:with-alien ((request (array sb-alien:unsigned-char 12)))
    (let ((memory (sb-alien:alien-sap request)))
      (%put-network-address memory 0 group)
      (%put-network-address memory 4 interface)
      (%put-network-address memory 8 source)
      (let ((result (%libc "setsockopt" sb-alien:int (sb-alien:int (%raw-fd fd))
                      (sb-alien:int +ipproto-ip+) (sb-alien:int option)
                      ((* (array sb-alien:unsigned-char 12)) (sb-alien:addr request))
                      (sb-alien:unsigned-int (if (member option '(35 36)) 8 12)))))
        (cond
          ((not (minusp result)) 0)
          ((member (sb-alien:get-errno) (list sb-posix:enoprotoopt sb-posix:eopnotsupp)) +ios-unavailable+)
          (t (%throw-socket-error (sb-alien:get-errno))))))))

(defun |sun/nio/ch/Net.joinOrDrop4(ZLjava/io/FileDescriptor;III)I| (join fd group interface source)
  (%ipv4-membership fd (if (zerop source) (if (zerop join) 36 35) (if (zerop join) 40 39))
                    group interface source))
(defun |sun/nio/ch/Net.blockOrUnblock4(ZLjava/io/FileDescriptor;III)I| (block fd group interface source)
  (%ipv4-membership fd (if (zerop block) 37 38) group interface source))
(defun |sun/nio/ch/Net.setInterface4(Ljava/io/FileDescriptor;I)V| (fd address)
  (sb-alien:with-alien ((value (array sb-alien:unsigned-char 4)))
    (%put-network-address (sb-alien:alien-sap value) 0 address)
    (when (minusp (%libc "setsockopt" sb-alien:int (sb-alien:int (%raw-fd fd))
                    (sb-alien:int +ipproto-ip+) (sb-alien:int 32)
                    ((* (array sb-alien:unsigned-char 4)) (sb-alien:addr value)) (sb-alien:unsigned-int 4)))
      (%throw-socket-error (sb-alien:get-errno))))
  nil)
(defun |sun/nio/ch/Net.getInterface4(Ljava/io/FileDescriptor;)I| (fd)
  (sb-alien:with-alien ((value (array sb-alien:unsigned-char 4)) (size sb-alien:unsigned-int 4))
    (when (minusp (%libc "getsockopt" sb-alien:int (sb-alien:int (%raw-fd fd))
                    (sb-alien:int +ipproto-ip+) (sb-alien:int 32)
                    ((* (array sb-alien:unsigned-char 4)) (sb-alien:addr value))
                    ((* sb-alien:unsigned-int) (sb-alien:addr size))))
      (%throw-socket-error (sb-alien:get-errno)))
    (%as-int (loop for i below 4 sum (ash (sb-alien:deref value i) (* 8 (- 3 i)))))))
