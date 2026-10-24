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

;;; Sockets, selectors and name lookup: the natives of sun.nio.ch.Net,
;;; SocketDispatcher, IOUtil, UnixDispatcher, EPoll and EventFD, and of
;;; Inet4AddressImpl.  java.net's sockets are built on sun.nio.ch in JDK
;;; 25 (NioSocketImpl), so these serve both APIs.  They call libc on real
;;; file descriptors, with buffers in native memory, as the JDK's C does.
;;; IPv4 only, as InetAddress.isIPv6Supported reports; Linux (epoll).

(in-package :openldk)

(defmacro %libc (name return-type &rest arguments)
  "Call the C function NAME, each argument a (TYPE VALUE) pair."
  `(sb-alien:alien-funcall
    (sb-alien:extern-alien ,name (function ,return-type ,@(mapcar #'first arguments)))
    ,@(mapcar #'second arguments)))

;;; Linux's values; the errno and fcntl ones come from sb-posix.
(defconstant +af-unix+ 1)
(defconstant +sock-stream+ 1)
(defconstant +sock-dgram+ 2)
(defconstant +sol-socket+ 1)
(defconstant +so-reuseaddr+ 2)
(defconstant +so-error+ 4)
(defconstant +so-linger+ 13)
(defconstant +ipproto-ip+ 0)
(defconstant +ip-multicast-ttl+ 33)
(defconstant +ip-multicast-loop+ 34)
(defconstant +pollin+ 1)
(defconstant +pollout+ 4)
(defconstant +pollerr+ 8)
(defconstant +pollhup+ 16)
(defconstant +pollnval+ 32)
(defconstant +fionread+ #x541B)
(defconstant +rlimit-nofile+ 7)
(defconstant +msg-oob+ 1)

;;; The JDK's IOStatus codes.
(defconstant +ios-eof+ -1)
(defconstant +ios-unavailable+ -2)
(defconstant +ios-interrupted+ -3)

(defvar *net-process* nil
  "The process the per-process socket state below was set up in: neither
the SIGPIPE disposition nor a file descriptor outlives a saved image.")

(defvar *pre-close-fd* -1
  "One end of a socket pair whose other end is closed.  preClose0 dup2s it
over a channel's descriptor, as the JDK does, so the descriptor number
isn't reused while other threads may still be using it.")

(defun %ensure-net-process ()
  "Set up this process for sockets: ignore SIGPIPE, as a JVM does, so that
writing to a closed connection fails with EPIPE, and make *PRE-CLOSE-FD*."
  (let ((pid (sb-posix:getpid)))
    (unless (eql *net-process* pid)
      (sb-sys:enable-interrupt sb-unix:sigpipe :ignore)
      (sb-alien:with-alien ((pair (array sb-alien:int 2)))
        (when (zerop (%libc "socketpair" sb-alien:int
                            (sb-alien:int +af-unix+) (sb-alien:int +sock-stream+)
                            (sb-alien:int 0) ((* (array sb-alien:int 2)) (sb-alien:addr pair))))
          (%libc "close" sb-alien:int (sb-alien:int (sb-alien:deref pair 1)))
          (setf *pre-close-fd* (sb-alien:deref pair 0))))
      (setf *net-process* pid))))

;;; Errors.

(defun %throw-errno (class errno &optional prefix)
  "Throw a new CLASS whose message is ERRNO's strerror, after PREFIX."
  (%throw class (format nil "~@[~A: ~]~A" prefix (sb-int:strerror errno))))

(defun %throw-socket-error (errno)
  "Throw the exception the JDK's handleSocketError throws for ERRNO."
  (%throw-errno (cond
                  ((eql errno sb-posix:eproto) '|java/net/ProtocolException|)
                  ((member errno (list sb-posix:econnrefused sb-posix:etimedout sb-posix:enotconn))
                   '|java/net/ConnectException|)
                  ((eql errno sb-posix:ehostunreach) '|java/net/NoRouteToHostException|)
                  ((member errno (list sb-posix:eaddrinuse sb-posix:eaddrnotavail sb-posix:eacces))
                   '|java/net/BindException|)
                  (t '|java/net/SocketException|))
                errno))

(defun %io-result (n reading)
  "The JDK's convertReturnVal: N, a read(2) or write(2) result, as an
IOStatus code, or an IOException."
  (cond
    ((plusp n) n)
    ((zerop n) (if reading +ios-eof+ 0))
    (t (let ((errno (sb-alien:get-errno)))
         (cond
           ((eql errno sb-posix:eagain) +ios-unavailable+)
           ((eql errno sb-posix:eintr) +ios-interrupted+)
           (t (%throw-errno '|java/io/IOException| errno)))))))

(defun %check-io (result)
  "RESULT, a system call's return, or an IOException if it failed."
  (when (minusp result)
    (%throw-errno '|java/io/IOException| (sb-alien:get-errno)))
  result)

;;; Addresses.  An IPv4 address is a 32-bit int in host order, as an
;;; InetAddressHolder keeps it.

(defun %inet4-address-int (address)
  "The int address of ADDRESS, an Inet4Address."
  (let ((holder (slot-value address '|holder|)))
    (unless (eql 1 (slot-value holder '|family|))
      (%throw '|java/net/SocketException| "Protocol family unavailable"))
    (logand (slot-value holder '|address|) #xffffffff)))

(defun %fill-sockaddr-in (sap address port)
  "Fill the struct sockaddr_in at SAP with ADDRESS (an int) and PORT."
  (dotimes (i 16) (setf (sb-sys:sap-ref-8 sap i) 0))
  (setf (sb-sys:sap-ref-16 sap 0) sb-posix:af-inet
        (sb-sys:sap-ref-8 sap 2) (ldb (byte 8 8) port)
        (sb-sys:sap-ref-8 sap 3) (ldb (byte 8 0) port))
  (dotimes (i 4)
    (setf (sb-sys:sap-ref-8 sap (+ 4 i)) (ldb (byte 8 (* 8 (- 3 i))) address))))

(defun %sockaddr-in-address (sap)
  "The address (an int) and port of the struct sockaddr_in at SAP."
  (values (logior (ash (sb-sys:sap-ref-8 sap 4) 24) (ash (sb-sys:sap-ref-8 sap 5) 16)
                  (ash (sb-sys:sap-ref-8 sap 6) 8) (sb-sys:sap-ref-8 sap 7))
          (logior (ash (sb-sys:sap-ref-8 sap 2) 8) (sb-sys:sap-ref-8 sap 3))))

(defun %make-inet4-address (address &optional host-name)
  "A new Inet4Address for ADDRESS, an int, named HOST-NAME (a Java string)."
  (%clinit (classload "java/net/Inet4Address"))
  (let ((inet (%make-java-instance "java/net/Inet4Address")))
    (|<init>(Ljava/lang/String;I)V| inet host-name
     (if (logbitp 31 address) (- address (ash 1 32)) address))
    inet))

(defun %make-inet-socket-address (address port)
  "A new InetSocketAddress for ADDRESS, an int, and PORT."
  (%clinit (classload "java/net/InetSocketAddress"))
  (let ((isa (%make-java-instance "java/net/InetSocketAddress")))
    (|<init>(Ljava/net/InetAddress;I)V| isa (%make-inet4-address address) port)
    isa))

(defmacro %with-sockaddr ((sap length-var) &body body)
  "Run BODY with SAP pointing at a sockaddr buffer and LENGTH-VAR at a
socklen_t holding its size."
  (let ((buffer (gensym "BUFFER")))
    `(sb-alien:with-alien ((,buffer (array (sb-alien:unsigned 8) 128))
                           (,length-var sb-alien:unsigned-int 128))
       (let ((,sap (sb-alien:alien-sap ,buffer)))
         ,@body))))

(defun %socket-name (fd &optional peer)
  "The address (an int) and port of FD's local end, or of its PEER's, or
an exception."
  (%with-sockaddr (sap length)
    (when (minusp (if peer
                      (%libc "getpeername" sb-alien:int (sb-alien:int fd) (sb-sys:system-area-pointer sap)
                             ((* sb-alien:unsigned-int) (sb-alien:addr length)))
                      (%libc "getsockname" sb-alien:int (sb-alien:int fd) (sb-sys:system-area-pointer sap)
                             ((* sb-alien:unsigned-int) (sb-alien:addr length)))))
      (%throw-socket-error (sb-alien:get-errno)))
    (if (eql (sb-sys:sap-ref-16 sap 0) sb-posix:af-inet)
        (%sockaddr-in-address sap)
        (values 0 0))))

;;; sun.nio.ch.Net

(defun |sun/nio/ch/Net.initIDs()V| () nil)
(defun |sun/nio/ch/Net.isIPv6Available0()Z| () 0)
(defun |sun/nio/ch/Net.isReusePortAvailable0()Z| () 1)
(defun |sun/nio/ch/Net.isExclusiveBindAvailable()I| () -1)
(defun |sun/nio/ch/Net.shouldShutdownWriteBeforeClose0()Z| () 0)
(defun |sun/nio/ch/Net.shouldSetBothIPv4AndIPv6Options0()Z| () 0)
(defun |sun/nio/ch/Net.canIPv6SocketJoinIPv4Group0()Z| () 0)
(defun |sun/nio/ch/Net.canJoin6WithIPv4Group0()Z| () 0)
(defun |sun/nio/ch/Net.canUseIPv6OptionsWithIPv4LocalAddress0()Z| () 0)
(defun |sun/nio/ch/Net.pollinValue()S| () +pollin+)
(defun |sun/nio/ch/Net.polloutValue()S| () +pollout+)
(defun |sun/nio/ch/Net.pollerrValue()S| () +pollerr+)
(defun |sun/nio/ch/Net.pollhupValue()S| () +pollhup+)
(defun |sun/nio/ch/Net.pollnvalValue()S| () +pollnval+)
(defun |sun/nio/ch/Net.pollconnValue()S| () +pollout+)

(defun %setsockopt-int (fd level option value)
  (sb-alien:with-alien ((v sb-alien:int value))
    (%libc "setsockopt" sb-alien:int (sb-alien:int fd) (sb-alien:int level) (sb-alien:int option)
           ((* sb-alien:int) (sb-alien:addr v)) (sb-alien:unsigned-int 4))))

(defun |sun/nio/ch/Net.socket0(ZZZZ)I| (prefer-ipv6 stream reuse fast-loopback)
  (declare (ignore prefer-ipv6 fast-loopback))
  (%ensure-net-process)
  (let ((fd (%libc "socket" sb-alien:int (sb-alien:int sb-posix:af-inet)
                   (sb-alien:int (if (eql stream 1) +sock-stream+ +sock-dgram+)) (sb-alien:int 0))))
    (when (minusp fd)
      (%throw-socket-error (sb-alien:get-errno)))
    (when (and (eql reuse 1) (minusp (%setsockopt-int fd +sol-socket+ +so-reuseaddr+ 1)))
      (let ((errno (sb-alien:get-errno)))
        (%libc "close" sb-alien:int (sb-alien:int fd))
        (%throw-socket-error errno)))
    fd))

(defun |sun/nio/ch/Net.bind0(Ljava/io/FileDescriptor;ZZLjava/net/InetAddress;I)V| (fd prefer-ipv6 exclusive address port)
  (declare (ignore prefer-ipv6 exclusive))
  (%with-sockaddr (sap length)
    (%fill-sockaddr-in sap (%inet4-address-int address) port)
    (when (minusp (%libc "bind" sb-alien:int (sb-alien:int (%raw-fd fd)) (sb-sys:system-area-pointer sap)
                         (sb-alien:unsigned-int 16)))
      (%throw-socket-error (sb-alien:get-errno)))))

(defun |sun/nio/ch/Net.listen(Ljava/io/FileDescriptor;I)V| (fd backlog)
  (when (minusp (%libc "listen" sb-alien:int (sb-alien:int (%raw-fd fd)) (sb-alien:int backlog)))
    (%throw-socket-error (sb-alien:get-errno))))

(defun |sun/nio/ch/Net.connect0(ZLjava/io/FileDescriptor;Ljava/net/InetAddress;I)I| (prefer-ipv6 fd remote port)
  (declare (ignore prefer-ipv6))
  (%with-sockaddr (sap length)
    (%fill-sockaddr-in sap (%inet4-address-int remote) port)
    (if (zerop (%libc "connect" sb-alien:int (sb-alien:int (%raw-fd fd)) (sb-sys:system-area-pointer sap)
                      (sb-alien:unsigned-int 16)))
        1
        (let ((errno (sb-alien:get-errno)))
          (cond
            ((eql errno sb-posix:einprogress) +ios-unavailable+)
            ((eql errno sb-posix:eintr) +ios-interrupted+)
            (t (%throw-socket-error errno)))))))

(defun |sun/nio/ch/Net.accept(Ljava/io/FileDescriptor;Ljava/io/FileDescriptor;[Ljava/net/InetSocketAddress;)I| (fd new-fd isaa)
  (%with-sockaddr (sap length)
    (let ((accepted (loop
                      (setf length 128)
                      (let ((n (%libc "accept" sb-alien:int (sb-alien:int (%raw-fd fd))
                                      (sb-sys:system-area-pointer sap)
                                      ((* sb-alien:unsigned-int) (sb-alien:addr length)))))
                        (when (or (>= n 0) (/= (sb-alien:get-errno) sb-posix:econnaborted))
                          (return n))))))
      (if (minusp accepted)
          (let ((errno (sb-alien:get-errno)))
            (cond
              ((eql errno sb-posix:eagain) +ios-unavailable+)
              ((eql errno sb-posix:eintr) +ios-interrupted+)
              (t (%throw-errno '|java/io/IOException| errno "Accept failed"))))
          (multiple-value-bind (address port) (%sockaddr-in-address sap)
            (setf (slot-value new-fd '|fd|) accepted)
            (setf (jaref isaa 0) (%make-inet-socket-address address port))
            1)))))

(defun |sun/nio/ch/Net.shutdown(Ljava/io/FileDescriptor;I)V| (fd how)
  (when (and (minusp (%libc "shutdown" sb-alien:int (sb-alien:int (%raw-fd fd)) (sb-alien:int how)))
             (/= (sb-alien:get-errno) sb-posix:enotconn))
    (%throw-socket-error (sb-alien:get-errno))))

(defun |sun/nio/ch/Net.localPort(Ljava/io/FileDescriptor;)I| (fd)
  (nth-value 1 (%socket-name (%raw-fd fd))))

(defun |sun/nio/ch/Net.localInetAddress(Ljava/io/FileDescriptor;)Ljava/net/InetAddress;| (fd)
  (%make-inet4-address (%socket-name (%raw-fd fd))))

(defun |sun/nio/ch/Net.remotePort(Ljava/io/FileDescriptor;)I| (fd)
  (nth-value 1 (%socket-name (%raw-fd fd) t)))

(defun |sun/nio/ch/Net.remoteInetAddress(Ljava/io/FileDescriptor;)Ljava/net/InetAddress;| (fd)
  (%make-inet4-address (%socket-name (%raw-fd fd) t)))

(defun |sun/nio/ch/Net.getIntOption0(Ljava/io/FileDescriptor;ZII)I| (fd may-need-conversion level option)
  (declare (ignore may-need-conversion))
  ;; Room for the largest value: SO_LINGER's struct linger {onoff; linger}.
  (sb-alien:with-alien ((value (array sb-alien:int 2))
                        (length sb-alien:unsigned-int 8))
    (setf (sb-alien:deref value 0) 0 (sb-alien:deref value 1) 0)
    (when (minusp (%libc "getsockopt" sb-alien:int (sb-alien:int (%raw-fd fd)) (sb-alien:int level)
                         (sb-alien:int option) ((* (array sb-alien:int 2)) (sb-alien:addr value))
                         ((* sb-alien:unsigned-int) (sb-alien:addr length))))
      (%throw-socket-error (sb-alien:get-errno)))
    (cond
      ((and (eql level +sol-socket+) (eql option +so-linger+))
       (if (zerop (sb-alien:deref value 0)) -1 (sb-alien:deref value 1)))
      ((and (eql level +ipproto-ip+) (member option (list +ip-multicast-ttl+ +ip-multicast-loop+)))
       (logand (sb-alien:deref value 0) #xff))
      (t (sb-alien:deref value 0)))))

(defun |sun/nio/ch/Net.setIntOption0(Ljava/io/FileDescriptor;ZIIIZ)V| (fd may-need-conversion level option arg ipv6)
  (declare (ignore may-need-conversion ipv6))
  (let ((result
          (if (and (eql level +sol-socket+) (eql option +so-linger+))
              (sb-alien:with-alien ((linger (array sb-alien:int 2)))
                (setf (sb-alien:deref linger 0) (if (minusp arg) 0 1)
                      (sb-alien:deref linger 1) (max arg 0))
                (%libc "setsockopt" sb-alien:int (sb-alien:int (%raw-fd fd)) (sb-alien:int level)
                       (sb-alien:int option) ((* (array sb-alien:int 2)) (sb-alien:addr linger))
                       (sb-alien:unsigned-int 8)))
              (%setsockopt-int (%raw-fd fd) level option arg))))
    (when (minusp result)
      (%throw-socket-error (sb-alien:get-errno)))))

(defun %poll-fd (fd events timeout)
  "poll(2) FD for EVENTS for TIMEOUT milliseconds: its revents, 0 on a
timeout or an interrupted call."
  (sb-alien:with-alien ((pfd (array (sb-alien:unsigned 8) 8)))
    (let ((sap (sb-alien:alien-sap pfd)))
      (setf (sb-sys:signed-sap-ref-32 sap 0) fd
            (sb-sys:sap-ref-16 sap 4) events
            (sb-sys:sap-ref-16 sap 6) 0)
      (let ((n (%libc "poll" sb-alien:int (sb-sys:system-area-pointer sap) (sb-alien:unsigned-long 1)
                      (sb-alien:int (max -1 (min timeout most-positive-fixnum #x7fffffff))))))
        (cond
          ((plusp n) (sb-sys:sap-ref-16 sap 6))
          ((or (zerop n) (eql (sb-alien:get-errno) sb-posix:eintr)) 0)
          (t (%throw-socket-error (sb-alien:get-errno))))))))

(defun |sun/nio/ch/Net.poll(Ljava/io/FileDescriptor;IJ)I| (fd events timeout)
  (%poll-fd (%raw-fd fd) events timeout))

(defun |sun/nio/ch/Net.pollConnect(Ljava/io/FileDescriptor;J)Z| (fd timeout)
  (let ((raw (%raw-fd fd)))
    (if (zerop (%poll-fd raw +pollout+ timeout))
        0
        (sb-alien:with-alien ((err sb-alien:int 0)
                              (length sb-alien:unsigned-int 4))
          (when (minusp (%libc "getsockopt" sb-alien:int (sb-alien:int raw) (sb-alien:int +sol-socket+)
                               (sb-alien:int +so-error+) ((* sb-alien:int) (sb-alien:addr err))
                               ((* sb-alien:unsigned-int) (sb-alien:addr length))))
            (%throw-socket-error (sb-alien:get-errno)))
          (unless (zerop err)
            (%throw-socket-error err))
          1))))

(defun |sun/nio/ch/Net.available(Ljava/io/FileDescriptor;)I| (fd)
  (sb-alien:with-alien ((count sb-alien:int 0))
    (when (minusp (%libc "ioctl" sb-alien:int (sb-alien:int (%raw-fd fd)) (sb-alien:unsigned-long +fionread+)
                         ((* sb-alien:int) (sb-alien:addr count))))
      (%throw-socket-error (sb-alien:get-errno)))
    count))

(defun |sun/nio/ch/Net.sendOOB(Ljava/io/FileDescriptor;B)I| (fd data)
  (sb-alien:with-alien ((byte (sb-alien:unsigned 8) (logand data #xff)))
    (%io-result (%libc "send" sb-alien:long (sb-alien:int (%raw-fd fd))
                       ((* (sb-alien:unsigned 8)) (sb-alien:addr byte)) (sb-alien:unsigned-long 1)
                       (sb-alien:int +msg-oob+))
                nil)))

(defun |sun/nio/ch/Net.discardOOB(Ljava/io/FileDescriptor;)Z| (fd)
  (declare (ignore fd))
  0)

;;; jdk.net.LinuxSocketOptions: the extended options sun.nio.ch.Net asks
;;; about as it initializes.

(defconstant +ipproto-tcp+ 6)
(defconstant +tcp-keepidle+ 4)
(defconstant +tcp-keepintvl+ 5)
(defconstant +tcp-keepcnt+ 6)
(defconstant +tcp-quickack+ 12)
(defconstant +ip-mtu-discover+ 10)
(defconstant +ip-pmtudisc-do+ 2)

(defun %getsockopt-int (fd level option)
  "The int value of socket option OPTION at LEVEL on FD, or an exception."
  (sb-alien:with-alien ((value sb-alien:int 0)
                        (length sb-alien:unsigned-int 4))
    (when (minusp (%libc "getsockopt" sb-alien:int (sb-alien:int fd) (sb-alien:int level)
                         (sb-alien:int option) ((* sb-alien:int) (sb-alien:addr value))
                         ((* sb-alien:unsigned-int) (sb-alien:addr length))))
      (%throw-socket-error (sb-alien:get-errno)))
    value))

(defun %set-int-option (fd level option value)
  (when (minusp (%setsockopt-int fd level option value))
    (%throw-socket-error (sb-alien:get-errno))))

(defun |jdk/net/LinuxSocketOptions.keepAliveOptionsSupported0()Z| () 1)
(defun |jdk/net/LinuxSocketOptions.quickAckSupported0()Z| () 1)
(defun |jdk/net/LinuxSocketOptions.incomingNapiIdSupported0()Z| () 0)

(defun |jdk/net/LinuxSocketOptions.setTcpKeepAliveProbes0(II)V| (fd value)
  (%set-int-option fd +ipproto-tcp+ +tcp-keepcnt+ value))
(defun |jdk/net/LinuxSocketOptions.setTcpKeepAliveTime0(II)V| (fd value)
  (%set-int-option fd +ipproto-tcp+ +tcp-keepidle+ value))
(defun |jdk/net/LinuxSocketOptions.setTcpKeepAliveIntvl0(II)V| (fd value)
  (%set-int-option fd +ipproto-tcp+ +tcp-keepintvl+ value))
(defun |jdk/net/LinuxSocketOptions.getTcpKeepAliveProbes0(I)I| (fd)
  (%getsockopt-int fd +ipproto-tcp+ +tcp-keepcnt+))
(defun |jdk/net/LinuxSocketOptions.getTcpKeepAliveTime0(I)I| (fd)
  (%getsockopt-int fd +ipproto-tcp+ +tcp-keepidle+))
(defun |jdk/net/LinuxSocketOptions.getTcpKeepAliveIntvl0(I)I| (fd)
  (%getsockopt-int fd +ipproto-tcp+ +tcp-keepintvl+))

(defun |jdk/net/LinuxSocketOptions.setQuickAck0(IZ)V| (fd on)
  (%set-int-option fd +ipproto-tcp+ +tcp-quickack+ on))
(defun |jdk/net/LinuxSocketOptions.getQuickAck0(I)Z| (fd)
  (if (zerop (%getsockopt-int fd +ipproto-tcp+ +tcp-quickack+)) 0 1))

(defun |jdk/net/LinuxSocketOptions.setIpDontFragment0(IZZ)V| (fd value ipv6)
  (declare (ignore ipv6))
  (%set-int-option fd +ipproto-ip+ +ip-mtu-discover+ (if (eql value 1) +ip-pmtudisc-do+ 0)))
(defun |jdk/net/LinuxSocketOptions.getIpDontFragment0(IZ)Z| (fd ipv6)
  (declare (ignore ipv6))
  (if (eql (%getsockopt-int fd +ipproto-ip+ +ip-mtu-discover+) +ip-pmtudisc-do+) 1 0))

(defun |jdk/net/LinuxSocketOptions.getSoPeerCred0(I)J| (fd)
  (declare (ignore fd))
  -1)
(defun |jdk/net/LinuxSocketOptions.getIncomingNapiId0(I)I| (fd)
  (declare (ignore fd))
  0)

;;; sun.nio.ch.SocketDispatcher

(defun %read-socket (fd address length)
  (let ((n (%libc "read" sb-alien:long (sb-alien:int (%raw-fd fd))
                  (sb-sys:system-area-pointer (sb-sys:int-sap address)) (sb-alien:unsigned-long length))))
    (if (and (minusp n) (member (sb-alien:get-errno) (list sb-posix:econnreset sb-posix:epipe)))
        (%throw '|sun/net/ConnectionResetException| "Connection reset")
        (%io-result n t))))

(defun |sun/nio/ch/SocketDispatcher.read0(Ljava/io/FileDescriptor;JI)I| (fd address length)
  (%read-socket fd address length))

(defun |sun/nio/ch/SocketDispatcher.readv0(Ljava/io/FileDescriptor;JI)J| (fd address count)
  (let ((n (%libc "readv" sb-alien:long (sb-alien:int (%raw-fd fd))
                  (sb-sys:system-area-pointer (sb-sys:int-sap address)) (sb-alien:int count))))
    (if (and (minusp n) (member (sb-alien:get-errno) (list sb-posix:econnreset sb-posix:epipe)))
        (%throw '|sun/net/ConnectionResetException| "Connection reset")
        (%io-result n t))))

(defun |sun/nio/ch/SocketDispatcher.write0(Ljava/io/FileDescriptor;JI)I| (fd address length)
  (%io-result (%libc "write" sb-alien:long (sb-alien:int (%raw-fd fd))
                     (sb-sys:system-area-pointer (sb-sys:int-sap address)) (sb-alien:unsigned-long length))
              nil))

(defun |sun/nio/ch/SocketDispatcher.writev0(Ljava/io/FileDescriptor;JI)J| (fd address count)
  (%io-result (%libc "writev" sb-alien:long (sb-alien:int (%raw-fd fd))
                     (sb-sys:system-area-pointer (sb-sys:int-sap address)) (sb-alien:int count))
              nil))

;;; sun.nio.ch.UnixDispatcher, which SocketDispatcher extends.

(defun |sun/nio/ch/UnixDispatcher.init()V| () nil)

(defun |sun/nio/ch/UnixDispatcher.close0(Ljava/io/FileDescriptor;)V| (fd)
  (let ((raw (%raw-fd fd)))
    (when (and (integerp raw) (>= raw 0)
               (minusp (%libc "close" sb-alien:int (sb-alien:int raw)))
               (/= (sb-alien:get-errno) sb-posix:eintr))
      (%throw-errno '|java/io/IOException| (sb-alien:get-errno)))))

(defun |sun/nio/ch/UnixDispatcher.preClose0(Ljava/io/FileDescriptor;)V| (fd)
  (%ensure-net-process)
  (let ((raw (%raw-fd fd)))
    (when (and (integerp raw) (>= raw 0) (>= *pre-close-fd* 0))
      (%check-io (%libc "dup2" sb-alien:int (sb-alien:int *pre-close-fd*) (sb-alien:int raw))))))

;;; sun.nio.ch.IOUtil

(defun |sun/nio/ch/IOUtil.fdVal(Ljava/io/FileDescriptor;)I| (fd)
  (%raw-fd fd))

(defun |sun/nio/ch/IOUtil.setfdVal(Ljava/io/FileDescriptor;I)V| (fd value)
  (setf (slot-value fd '|fd|) value))

(defun %set-blocking (fd blocking)
  (let ((flags (%check-io (%libc "fcntl" sb-alien:int (sb-alien:int fd) (sb-alien:int sb-posix:f-getfl)
                                 (sb-alien:int 0)))))
    (%check-io (%libc "fcntl" sb-alien:int (sb-alien:int fd) (sb-alien:int sb-posix:f-setfl)
                      (sb-alien:int (if blocking
                                        (logandc2 flags sb-posix:o-nonblock)
                                        (logior flags sb-posix:o-nonblock)))))))

(defun |sun/nio/ch/IOUtil.configureBlocking(Ljava/io/FileDescriptor;Z)V| (fd blocking)
  (%set-blocking (%raw-fd fd) (eql blocking 1)))

(defun |sun/nio/ch/IOUtil.makePipe(Z)J| (blocking)
  (%ensure-net-process)
  (sb-alien:with-alien ((fds (array sb-alien:int 2)))
    (%check-io (%libc "pipe" sb-alien:int ((* (array sb-alien:int 2)) (sb-alien:addr fds))))
    (let ((read-end (sb-alien:deref fds 0))
          (write-end (sb-alien:deref fds 1)))
      (unless (eql blocking 1)
        (%set-blocking read-end nil)
        (%set-blocking write-end nil))
      (logior (ash read-end 32) write-end))))

(defun |sun/nio/ch/IOUtil.drain(I)Z| (fd)
  "Read FD until it would block: whether anything was read."
  (sb-alien:with-alien ((buffer (array (sb-alien:unsigned 8) 128)))
    (let ((total 0))
      (loop
        (let ((n (%libc "read" sb-alien:long (sb-alien:int fd)
                        (sb-sys:system-area-pointer (sb-alien:alien-sap buffer)) (sb-alien:unsigned-long 128))))
          (cond
            ((plusp n) (incf total n)
             (when (< n 128) (return (if (plusp total) 1 0))))
            ((and (minusp n) (eql (sb-alien:get-errno) sb-posix:eintr)))
            ((and (minusp n) (/= (sb-alien:get-errno) sb-posix:eagain))
             (%throw-errno '|java/io/IOException| (sb-alien:get-errno) "Drain"))
            (t (return (if (plusp total) 1 0)))))))))

(defun |sun/nio/ch/IOUtil.drain1(I)I| (fd)
  (sb-alien:with-alien ((byte (sb-alien:unsigned 8)))
    (let ((n (%libc "read" sb-alien:long (sb-alien:int fd)
                    ((* (sb-alien:unsigned 8)) (sb-alien:addr byte)) (sb-alien:unsigned-long 1))))
      (if (and (minusp n) (eql (sb-alien:get-errno) sb-posix:eagain))
          0
          (%io-result n nil)))))

(defun |sun/nio/ch/IOUtil.write1(IB)I| (fd data)
  (sb-alien:with-alien ((byte (sb-alien:unsigned 8) (logand data #xff)))
    (%io-result (%libc "write" sb-alien:long (sb-alien:int fd)
                       ((* (sb-alien:unsigned 8)) (sb-alien:addr byte)) (sb-alien:unsigned-long 1))
                nil)))

(defun |sun/nio/ch/IOUtil.fdLimit()I| ()
  (sb-alien:with-alien ((limit (array (sb-alien:unsigned 64) 2)))
    (if (minusp (%libc "getrlimit" sb-alien:int (sb-alien:int +rlimit-nofile+)
                       ((* (array (sb-alien:unsigned 64) 2)) (sb-alien:addr limit))))
        1024
        (min (sb-alien:deref limit 0) #x7fffffff))))

(defun |sun/nio/ch/IOUtil.randomBytes([B)Z| (bytes)
  (declare (ignore bytes))
  0)

;;; sun.nio.ch.EPoll: struct epoll_event is packed on x86-64, a 32-bit
;;; events word and then the 64-bit data.

(defun |sun/nio/ch/EPoll.eventSize()I| () 12)
(defun |sun/nio/ch/EPoll.eventsOffset()I| () 0)
(defun |sun/nio/ch/EPoll.dataOffset()I| () 4)

(defun |sun/nio/ch/EPoll.create()I| ()
  (%ensure-net-process)
  (%check-io (%libc "epoll_create" sb-alien:int (sb-alien:int 256))))

(defun |sun/nio/ch/EPoll.ctl(IIII)I| (epfd opcode fd events)
  "epoll_ctl(2): 0, or the errno, as the JDK's returns."
  (sb-alien:with-alien ((event (array (sb-alien:unsigned 8) 12)))
    (let ((sap (sb-alien:alien-sap event)))
      (setf (sb-sys:sap-ref-32 sap 0) (logand events #xffffffff)
            (sb-sys:sap-ref-64 sap 4) fd)
      (if (zerop (%libc "epoll_ctl" sb-alien:int (sb-alien:int epfd) (sb-alien:int opcode) (sb-alien:int fd)
                        (sb-sys:system-area-pointer sap)))
          0
          (sb-alien:get-errno)))))

(defun |sun/nio/ch/EPoll.wait(IJII)I| (epfd address count timeout)
  (let ((n (%libc "epoll_wait" sb-alien:int (sb-alien:int epfd)
                  (sb-sys:system-area-pointer (sb-sys:int-sap address)) (sb-alien:int count)
                  (sb-alien:int timeout))))
    (cond
      ((>= n 0) n)
      ((eql (sb-alien:get-errno) sb-posix:eintr) +ios-interrupted+)
      (t (%throw-errno '|java/io/IOException| (sb-alien:get-errno) "epoll_wait failed")))))

;;; sun.nio.ch.EventFD: a selector's wakeup.

(defun |sun/nio/ch/EventFD.eventfd0()I| ()
  (%ensure-net-process)
  (%check-io (%libc "eventfd" sb-alien:int (sb-alien:unsigned-int 0) (sb-alien:int 0))))

(defun |sun/nio/ch/EventFD.set0(I)I| (efd)
  (sb-alien:with-alien ((one (sb-alien:unsigned 64) 1))
    (%io-result (%libc "write" sb-alien:long (sb-alien:int efd)
                       ((* (sb-alien:unsigned 64)) (sb-alien:addr one)) (sb-alien:unsigned-long 8))
                nil)))

;;; java.net.Inet4AddressImpl: name lookup.

(defconstant +addrinfo-family-offset+ 4)
(defconstant +addrinfo-socktype-offset+ 8)
(defconstant +addrinfo-addr-offset+ 24)
(defconstant +addrinfo-next-offset+ 40)

(defun %lookup-ipv4 (host-name)
  "The IPv4 addresses (ints) getaddrinfo(3) finds for HOST-NAME, a Lisp
string, in order and without duplicates; NIL if there are none."
  (sb-alien:with-alien ((hints (array (sb-alien:unsigned 8) 48))
                        (result sb-sys:system-area-pointer))
    (let ((hints-sap (sb-alien:alien-sap hints)))
      (dotimes (i 48) (setf (sb-sys:sap-ref-8 hints-sap i) 0))
      (setf (sb-sys:signed-sap-ref-32 hints-sap +addrinfo-family-offset+) sb-posix:af-inet
            (sb-sys:signed-sap-ref-32 hints-sap +addrinfo-socktype-offset+) +sock-stream+)
      (when (zerop (%libc "getaddrinfo" sb-alien:int (sb-alien:c-string host-name)
                          (sb-sys:system-area-pointer (sb-sys:int-sap 0))
                          (sb-sys:system-area-pointer hints-sap)
                          ((* sb-sys:system-area-pointer) (sb-alien:addr result))))
        (unwind-protect
             (loop with addresses = nil
                   for info = result then (sb-sys:sap-ref-sap info +addrinfo-next-offset+)
                   until (zerop (sb-sys:sap-int info))
                   do (pushnew (%sockaddr-in-address (sb-sys:sap-ref-sap info +addrinfo-addr-offset+))
                               addresses)
                   finally (return (nreverse addresses)))
          (%libc "freeaddrinfo" sb-alien:void (sb-sys:system-area-pointer result)))))))

(defmethod |getLocalHostName()Ljava/lang/String;| ((impl |java/net/Inet4AddressImpl|))
  (jstring (uiop:hostname)))

(defmethod |lookupAllHostAddr(Ljava/lang/String;)[Ljava/net/InetAddress;| ((impl |java/net/Inet4AddressImpl|) host-name)
  (let ((addresses (%lookup-ipv4 (lstring host-name))))
    (unless addresses
      (%throw '|java/net/UnknownHostException| (lstring host-name)))
    (classload "java/net/InetAddress")
    (make-java-array
     :component-class (%get-java-class-by-bin-name "java/net/InetAddress")
     :initial-contents (mapcar (lambda (address) (%make-inet4-address address host-name)) addresses))))

(defmethod |getHostByAddr([B)Ljava/lang/String;| ((impl |java/net/Inet4AddressImpl|) bytes)
  (%with-sockaddr (sap length)
    (%fill-sockaddr-in sap (reduce (lambda (a b) (logior (ash a 8) (logand b #xff)))
                                   (java-array-data bytes) :initial-value 0)
                       0)
    (sb-alien:with-alien ((host (array sb-alien:char 1025)))
      ;; NI_NAMEREQD: an address with no name is an error, as the JDK's.
      (unless (zerop (%libc "getnameinfo" sb-alien:int (sb-sys:system-area-pointer sap) (sb-alien:unsigned-int 16)
                            (sb-sys:system-area-pointer (sb-alien:alien-sap host)) (sb-alien:unsigned-int 1025)
                            (sb-sys:system-area-pointer (sb-sys:int-sap 0)) (sb-alien:unsigned-int 0)
                            (sb-alien:int 8)))
        (%throw '|java/net/UnknownHostException|))
      (let ((sap (sb-alien:alien-sap host)))
        (jstring (coerce (loop for i from 0
                               for code = (sb-sys:sap-ref-8 sap i)
                               until (zerop code)
                               collect (code-char code))
                         'string))))))

(defmethod |isReachable0([BI[BI)Z| ((impl |java/net/Inet4AddressImpl|) address timeout interface ttl)
  (declare (ignore address timeout interface ttl))
  0)

;;; java.net.NetworkInterface: the interfaces with IPv4 addresses, from
;;; getifaddrs(3), as the JDK lists them without IPv6.  SHA1PRNG's seed
;;; generator reads them all.

(defconstant +ifaddrs-name-offset+ 8)
(defconstant +ifaddrs-flags-offset+ 16)
(defconstant +ifaddrs-addr-offset+ 24)
(defconstant +ifaddrs-netmask-offset+ 32)
(defconstant +ifaddrs-broadaddr-offset+ 40)
(defconstant +iff-up+ #x1)
(defconstant +iff-broadcast+ #x2)
(defconstant +iff-loopback+ #x8)
(defconstant +iff-pointopoint+ #x10)
(defconstant +iff-running+ #x40)
(defconstant +iff-multicast+ #x1000)

(defun %c-string (sap)
  "The NUL-terminated ASCII string at SAP."
  (coerce (loop for i from 0
                for code = (sb-sys:sap-ref-8 sap i)
                until (zerop code)
                collect (code-char code))
          'string))

(defun %interface-addresses ()
  "This host's interfaces, in getifaddrs order: a list of (NAME FLAGS
ADDRESSES), each address (ADDRESS PREFIX-LENGTH BROADCAST), ints or NIL."
  (sb-alien:with-alien ((list sb-sys:system-area-pointer))
    (when (zerop (%libc "getifaddrs" sb-alien:int ((* sb-sys:system-area-pointer) (sb-alien:addr list))))
      (unwind-protect
           (let ((interfaces nil))
             (loop for ifa = list then (sb-sys:sap-ref-sap ifa 0)
                   until (zerop (sb-sys:sap-int ifa))
                   do (let* ((name (%c-string (sb-sys:sap-ref-sap ifa +ifaddrs-name-offset+)))
                             (flags (sb-sys:sap-ref-32 ifa +ifaddrs-flags-offset+))
                             (addr (sb-sys:sap-ref-sap ifa +ifaddrs-addr-offset+))
                             (entry (or (assoc name interfaces :test #'string=)
                                        (car (push (list name flags nil) interfaces)))))
                        (when (and (/= 0 (sb-sys:sap-int addr))
                                   (eql (sb-sys:sap-ref-16 addr 0) sb-posix:af-inet))
                          (let ((netmask (sb-sys:sap-ref-sap ifa +ifaddrs-netmask-offset+))
                                (broadcast (sb-sys:sap-ref-sap ifa +ifaddrs-broadaddr-offset+)))
                            (push (list (%sockaddr-in-address addr)
                                        (if (zerop (sb-sys:sap-int netmask))
                                            0
                                            (logcount (%sockaddr-in-address netmask)))
                                        (and (logtest flags +iff-broadcast+)
                                             (/= 0 (sb-sys:sap-int broadcast))
                                             (%sockaddr-in-address broadcast)))
                                  (third entry))))))
             (loop for (name flags addresses) in (nreverse interfaces)
                   when addresses
                     collect (list name flags (nreverse addresses))))
        (%libc "freeifaddrs" sb-alien:void (sb-sys:system-area-pointer list))))))

(defun %make-network-interface (name addresses)
  "A new NetworkInterface for the interface NAME with ADDRESSES (see
%INTERFACE-ADDRESSES)."
  (%clinit (classload "java/net/NetworkInterface"))
  (classload "java/net/InterfaceAddress")
  (let* ((inets (mapcar (lambda (a) (%make-inet4-address (first a))) addresses))
         (ni (%make-java-instance "java/net/NetworkInterface")))
    (|<init>(Ljava/lang/String;I[Ljava/net/InetAddress;)V|
     ni (jstring name)
     (%libc "if_nametoindex" sb-alien:unsigned-int (sb-alien:c-string name))
     (make-java-array :component-class (%get-java-class-by-bin-name "java/net/InetAddress")
                      :initial-contents inets))
    (setf (slot-value ni '|displayName|) (jstring name)
          (slot-value ni '|bindings|)
          (make-java-array
           :component-class (%get-java-class-by-bin-name "java/net/InterfaceAddress")
           :initial-contents (loop for (nil prefix broadcast) in addresses
                                   for inet in inets
                                   collect (let ((ia (%make-java-instance "java/net/InterfaceAddress")))
                                             (|<init>()V| ia)
                                             (setf (slot-value ia '|address|) inet
                                                   (slot-value ia '|maskLength|) prefix
                                                   (slot-value ia '|broadcast|)
                                                   (and broadcast (%make-inet4-address broadcast)))
                                             ia)))
          (slot-value ni '|childs|)
          (make-java-array :component-class (%get-java-class-by-bin-name "java/net/NetworkInterface")
                           :initial-contents nil))
    ni))

(defun %network-interfaces ()
  (mapcar (lambda (entry) (%make-network-interface (first entry) (third entry)))
          (%interface-addresses)))

(defun %interface-flags (name)
  (second (find (lstring name) (%interface-addresses) :key #'first :test #'string=)))

(defun |java/net/NetworkInterface.init()V| () nil)

(defun |java/net/NetworkInterface.getAll()[Ljava/net/NetworkInterface;| ()
  (let ((all (%network-interfaces)))
    (make-java-array :component-class (%get-java-class-by-bin-name "java/net/NetworkInterface")
                     :initial-contents all)))

(defun |java/net/NetworkInterface.getByName0(Ljava/lang/String;)Ljava/net/NetworkInterface;| (name)
  (find (lstring name) (%network-interfaces) :key (lambda (ni) (lstring (slot-value ni '|name|)))
                                             :test #'string=))

(defun |java/net/NetworkInterface.getByIndex0(I)Ljava/net/NetworkInterface;| (index)
  (find index (%network-interfaces) :key (lambda (ni) (slot-value ni '|index|))))

(defun %interface-for-address (address)
  (let ((target (%inet4-address-int address)))
    (find-if (lambda (entry) (find target (third entry) :key #'first))
             (%interface-addresses))))

(defun |java/net/NetworkInterface.getByInetAddress0(Ljava/net/InetAddress;)Ljava/net/NetworkInterface;| (address)
  (let ((entry (%interface-for-address address)))
    (and entry (%make-network-interface (first entry) (third entry)))))

(defun |java/net/NetworkInterface.boundInetAddress0(Ljava/net/InetAddress;)Z| (address)
  (if (%interface-for-address address) 1 0))

(defun |java/net/NetworkInterface.isUp0(Ljava/lang/String;I)Z| (name index)
  (declare (ignore index))
  (let ((flags (or (%interface-flags name) 0)))
    (if (and (logtest flags +iff-up+) (logtest flags +iff-running+)) 1 0)))

(defun |java/net/NetworkInterface.isLoopback0(Ljava/lang/String;I)Z| (name index)
  (declare (ignore index))
  (if (logtest (or (%interface-flags name) 0) +iff-loopback+) 1 0))

(defun |java/net/NetworkInterface.supportsMulticast0(Ljava/lang/String;I)Z| (name index)
  (declare (ignore index))
  (if (logtest (or (%interface-flags name) 0) +iff-multicast+) 1 0))

(defun |java/net/NetworkInterface.isP2P0(Ljava/lang/String;I)Z| (name index)
  (declare (ignore index))
  (if (logtest (or (%interface-flags name) 0) +iff-pointopoint+) 1 0))

(defun %sys-class-net (name file)
  "The first line of /sys/class/net/NAME/FILE, or NIL."
  (ignore-errors
   (with-open-file (in (format nil "/sys/class/net/~A/~A" name file))
     (read-line in nil nil))))

(defun |java/net/NetworkInterface.getMacAddr0([BLjava/lang/String;I)[B| (address name index)
  "The interface's hardware address, NIL if it has none (all zeros, as
the loopback's), as the JDK's."
  (declare (ignore address index))
  (let* ((text (%sys-class-net (lstring name) "address"))
         (bytes (and text (mapcar (lambda (hex) (parse-integer hex :radix 16 :junk-allowed t))
                                  (uiop:split-string text :separator ":")))))
    (when (and bytes (every #'integerp bytes) (some #'plusp bytes))
      (make-java-array :component-class (%get-java-class-by-bin-name "byte")
                       :initial-contents (mapcar (lambda (b) (if (> b 127) (- b 256) b)) bytes)))))

(defun |java/net/NetworkInterface.getMTU0(Ljava/lang/String;I)I| (name index)
  (declare (ignore index))
  (or (ignore-errors (parse-integer (%sys-class-net (lstring name) "mtu"))) -1))
