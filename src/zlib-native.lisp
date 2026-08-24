;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: OPENLDK; Base: 10 -*-
;;;
;;; Copyright (C) 2026  OpenLDK contributors
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later WITH Classpath-exception-2.0
;;;
;;; java.util.zip's native streaming operations, backed by the system zlib.

(in-package :openldk)

(sb-alien:define-alien-type openldk-z-stream
    (sb-alien:struct openldk-z-stream
      (next-in (* sb-alien:unsigned-char))
      (avail-in sb-alien:unsigned-int)
      (total-in sb-alien:unsigned-long)
      (next-out (* sb-alien:unsigned-char))
      (avail-out sb-alien:unsigned-int)
      (total-out sb-alien:unsigned-long)
      (msg sb-alien:c-string)
      (state (* t))
      (zalloc (* t))
      (zfree (* t))
      (opaque (* t))
      (data-type sb-alien:int)
      (adler sb-alien:unsigned-long)
      (reserved sb-alien:unsigned-long)))

(defvar *zlib-loaded* nil)
(defvar *zlib-streams* (make-hash-table))

(defmacro %zlib-call (name return-type &rest arguments)
  `(sb-alien:alien-funcall
    (sb-alien:extern-alien ,name
                           (function ,return-type
                                     ,@(mapcar #'first arguments)))
    ,@(mapcar #'second arguments)))

(defun %load-zlib ()
  (unless *zlib-loaded*
    (sb-alien:load-shared-object "libz.so.1")
    (setf *zlib-loaded* t)))

(defun %zlib-version ()
  (%zlib-call "zlibVersion" sb-alien:c-string))

(defun %zlib-checksum-sap (function checksum sap length)
  "Update CHECKSUM over LENGTH bytes at SAP using the named zlib FUNCTION."
  (%load-zlib)
  (let ((checksum (logand checksum #xffffffff))
        (pointer (sb-alien:sap-alien sap (* sb-alien:unsigned-char))))
    (unsigned-to-signed-integer
     (logand
      (cond
        ((string= function "crc32")
         (%zlib-call "crc32" sb-alien:unsigned-long
                      (sb-alien:unsigned-long checksum)
                      ((* sb-alien:unsigned-char) pointer)
                      (sb-alien:unsigned-int length)))
        ((string= function "adler32")
         (%zlib-call "adler32" sb-alien:unsigned-long
                      (sb-alien:unsigned-long checksum)
                      ((* sb-alien:unsigned-char) pointer)
                      (sb-alien:unsigned-int length)))
        (t (internal-error "Unknown zlib checksum function ~A" function)))
      #xffffffff))))

(defun %zlib-checksum-byte (function checksum value)
  "Update CHECKSUM with the low byte of VALUE using zlib FUNCTION."
  (let ((memory (sb-alien:make-alien sb-alien:unsigned-char 1)))
    (unwind-protect
         (progn
           (setf (sb-alien:deref memory 0) (logand value #xff))
           (%zlib-checksum-sap function checksum
                               (sb-alien:alien-sap memory) 1))
      (sb-alien:free-alien memory))))

(defun %zlib-checksum-array (function checksum array offset length)
  "Update CHECKSUM from a Java byte ARRAY using zlib FUNCTION."
  (let ((memory (%zlib-array-to-alien array offset length)))
    (unwind-protect
         (%zlib-checksum-sap function checksum
                             (sb-alien:alien-sap memory) length)
      (sb-alien:free-alien memory))))

(defun |java/util/zip/CRC32.update(II)| (checksum value)
  (%zlib-checksum-byte "crc32" checksum value))

(defun |java/util/zip/CRC32.updateBytes0(I[BII)|
    (checksum array offset length)
  (%zlib-checksum-array "crc32" checksum array offset length))

(defun |java/util/zip/CRC32.updateByteBuffer0(IJII)|
    (checksum address offset length)
  (%zlib-checksum-sap "crc32" checksum
                      (sb-sys:sap+ (sb-sys:int-sap address) offset) length))

(defun |java/util/zip/Adler32.update(II)| (checksum value)
  (%zlib-checksum-byte "adler32" checksum value))

(defun |java/util/zip/Adler32.updateBytes(I[BII)|
    (checksum array offset length)
  (%zlib-checksum-array "adler32" checksum array offset length))

(defun |java/util/zip/Adler32.updateByteBuffer(IJII)|
    (checksum address offset length)
  (%zlib-checksum-sap "adler32" checksum
                      (sb-sys:sap+ (sb-sys:int-sap address) offset) length))

(defun %zlib-stream (address)
  (or (gethash address *zlib-streams*)
      (internal-error "Unknown or closed zlib stream at address ~X" address)))

(defun %zero-zlib-stream (stream)
  (%zlib-call "memset" (* t)
               ((* t) (sb-alien:alien-sap stream))
               (sb-alien:int 0)
               (sb-alien:unsigned-long
                (sb-alien:alien-size openldk-z-stream :bytes))))

(defun %register-zlib-stream (stream)
  (let ((address (sb-sys:sap-int (sb-alien:alien-sap stream))))
    (setf (gethash address *zlib-streams*) stream)
    address))

(defun %free-zlib-stream (address)
  (when-let ((stream (gethash address *zlib-streams*)))
    (sb-alien:free-alien stream)
    (remhash address *zlib-streams*)))

(defun %zlib-array-to-alien (array offset length)
  (let ((memory (sb-alien:make-alien sb-alien:unsigned-char (max length 1))))
    (loop for i below length
          do (setf (sb-alien:deref memory i)
                   (logand (jaref array (+ offset i)) #xff)))
    memory))

(defun %zlib-alien-to-array (memory array offset length)
  (loop for i below length
        do (setf (jaref array (+ offset i))
                 (%unsigned-to-signed-byte (sb-alien:deref memory i)))))

(defun %zlib-set-io (stream input-sap input-length output-sap output-length)
  (let ((value (sb-alien:deref stream)))
    (setf (sb-alien:slot value 'next-in)
          (sb-alien:sap-alien input-sap (* sb-alien:unsigned-char))
          (sb-alien:slot value 'avail-in) input-length
          (sb-alien:slot value 'next-out)
          (sb-alien:sap-alien output-sap (* sb-alien:unsigned-char))
          (sb-alien:slot value 'avail-out) output-length)))

(defun %zlib-result (stream input-length output-length finished need-dictionary)
  (let* ((value (sb-alien:deref stream))
         (input-used (- input-length (sb-alien:slot value 'avail-in)))
         (output-used (- output-length (sb-alien:slot value 'avail-out)))
         (packed (logior input-used
                         (ash output-used 31)
                         (if finished (ash 1 62) 0)
                         (if need-dictionary (ash 1 63) 0))))
    (values (unsigned-to-signed-long packed) input-used output-used)))

(defun %throw-data-format-exception (inflater input-used output-used)
  (when (slot-exists-p inflater '|inputConsumed|)
    (setf (slot-value inflater '|inputConsumed|) input-used))
  (when (slot-exists-p inflater '|outputConsumed|)
    (setf (slot-value inflater '|outputConsumed|) output-used))
  (error (%lisp-condition
          (%make-throwable '|java/util/zip/DataFormatException|))))

(defun %throw-zlib-illegal-argument (operation status)
  "The JDK throws IllegalArgumentException when zlib rejects a
dictionary (wrong adler32 or bad stream state)."
  (declare (ignore operation status))
  (error (%lisp-condition
          (%make-throwable '|java/lang/IllegalArgumentException|))))

(defun |java/util/zip/Inflater.init(Z)| (nowrap)
  (%load-zlib)
  (let ((stream (sb-alien:make-alien openldk-z-stream)))
    (%zero-zlib-stream stream)
    (let ((status
            (%zlib-call "inflateInit2_" sb-alien:int
                         ((* openldk-z-stream) stream)
                         (sb-alien:int (if (zerop nowrap) 15 -15))
                         (sb-alien:c-string (%zlib-version))
                         (sb-alien:int
                          (sb-alien:alien-size openldk-z-stream :bytes)))))
      (unless (zerop status)
        (sb-alien:free-alien stream)
        (internal-error "inflateInit2 failed with zlib status ~D" status))
      (%register-zlib-stream stream))))

(defun %inflate-saps (inflater address input-sap input-length output-sap output-length)
  (let ((stream (%zlib-stream address)))
    (%zlib-set-io stream input-sap input-length output-sap output-length)
    (let ((status (%zlib-call "inflate" sb-alien:int
                              ((* openldk-z-stream) stream)
                              (sb-alien:int 1)))) ; Z_PARTIAL_FLUSH
      (multiple-value-bind (packed input-used output-used)
          (%zlib-result stream input-length output-length (= status 1) (= status 2))
        (case status
          ((0 1 2 -5) packed)       ; OK, STREAM_END, NEED_DICT, BUF_ERROR
          (-3 (%throw-data-format-exception inflater input-used output-used))
          (otherwise (internal-error "inflate failed with zlib status ~D" status)))))))

(defmethod |inflateBytesBytes(J[BII[BII)|
    ((inflater t) address input input-offset input-length
     output output-offset output-length)
  (let ((input-memory (%zlib-array-to-alien input input-offset input-length))
        (output-memory (sb-alien:make-alien sb-alien:unsigned-char
                                            (max output-length 1))))
    (unwind-protect
         (let ((result (%inflate-saps inflater address
                                      (sb-alien:alien-sap input-memory) input-length
                                      (sb-alien:alien-sap output-memory) output-length)))
           (let ((written (logand (ash result -31) #x7fffffff)))
             (%zlib-alien-to-array output-memory output output-offset written))
           result)
      (sb-alien:free-alien output-memory)
      (sb-alien:free-alien input-memory))))

(defmethod |inflateBytesBuffer(J[BIIJI)|
    ((inflater t) address input input-offset input-length
     output-address output-length)
  (let ((input-memory (%zlib-array-to-alien input input-offset input-length)))
    (unwind-protect
         (%inflate-saps inflater address
                        (sb-alien:alien-sap input-memory) input-length
                        (sb-sys:int-sap output-address) output-length)
      (sb-alien:free-alien input-memory))))

(defmethod |inflateBufferBytes(JJI[BII)|
    ((inflater t) address input-address input-length
     output output-offset output-length)
  (let ((output-memory (sb-alien:make-alien sb-alien:unsigned-char
                                            (max output-length 1))))
    (unwind-protect
         (let ((result (%inflate-saps inflater address
                                      (sb-sys:int-sap input-address) input-length
                                      (sb-alien:alien-sap output-memory) output-length)))
           (let ((written (logand (ash result -31) #x7fffffff)))
             (%zlib-alien-to-array output-memory output output-offset written))
           result)
      (sb-alien:free-alien output-memory))))

(defmethod |inflateBufferBuffer(JJIJI)|
    ((inflater t) address input-address input-length output-address output-length)
  (%inflate-saps inflater address
                 (sb-sys:int-sap input-address) input-length
                 (sb-sys:int-sap output-address) output-length))

(defun |java/util/zip/Inflater.setDictionary(J[BII)| (address dictionary offset length)
  (let ((memory (%zlib-array-to-alien dictionary offset length)))
    (unwind-protect
         (let ((status
                 (%zlib-call "inflateSetDictionary" sb-alien:int
                              ((* openldk-z-stream) (%zlib-stream address))
                              ((* sb-alien:unsigned-char) memory)
                              (sb-alien:unsigned-int length))))
           (unless (zerop status)
             (%throw-zlib-illegal-argument "inflateSetDictionary" status)))
      (sb-alien:free-alien memory)))
  nil)

(defun |java/util/zip/Inflater.setDictionaryBuffer(JJI)| (address buffer-address length)
  (let ((status
          (%zlib-call "inflateSetDictionary" sb-alien:int
                       ((* openldk-z-stream) (%zlib-stream address))
                       ((* sb-alien:unsigned-char)
                        (sb-alien:sap-alien (sb-sys:int-sap buffer-address)
                                            (* sb-alien:unsigned-char)))
                       (sb-alien:unsigned-int length))))
    (unless (zerop status)
      (%throw-zlib-illegal-argument "inflateSetDictionary" status)))
  nil)

(defun |java/util/zip/Inflater.getAdler(J)| (address)
  (unsigned-to-signed-integer
   (sb-alien:slot (sb-alien:deref (%zlib-stream address)) 'adler)))

(defun |java/util/zip/Inflater.reset(J)| (address)
  (unless (zerop (%zlib-call "inflateReset" sb-alien:int
                             ((* openldk-z-stream) (%zlib-stream address))))
    (internal-error "inflateReset failed"))
  nil)

(defun |java/util/zip/Inflater.end(J)| (address)
  (let ((stream (%zlib-stream address)))
    (unless (zerop (%zlib-call "inflateEnd" sb-alien:int
                               ((* openldk-z-stream) stream)))
      (internal-error "inflateEnd failed"))
    (%free-zlib-stream address))
  nil)

(defun |java/util/zip/Deflater.init(IIZ)| (level strategy nowrap)
  (%load-zlib)
  (let ((stream (sb-alien:make-alien openldk-z-stream)))
    (%zero-zlib-stream stream)
    (let ((status
            (%zlib-call "deflateInit2_" sb-alien:int
                         ((* openldk-z-stream) stream)
                         (sb-alien:int level)
                         (sb-alien:int 8)    ; Z_DEFLATED
                         (sb-alien:int (if (zerop nowrap) 15 -15))
                         (sb-alien:int 8)    ; DEF_MEM_LEVEL
                         (sb-alien:int strategy)
                         (sb-alien:c-string (%zlib-version))
                         (sb-alien:int
                          (sb-alien:alien-size openldk-z-stream :bytes)))))
      (unless (zerop status)
        (sb-alien:free-alien stream)
        (internal-error "deflateInit2 failed with zlib status ~D" status))
      (%register-zlib-stream stream))))

(defun %deflate-saps (address input-sap input-length output-sap output-length
                      flush parameters)
  (let* ((stream (%zlib-stream address))
         (setting-parameters (not (zerop (logand parameters 1)))))
    (%zlib-set-io stream input-sap input-length output-sap output-length)
    (let ((status
            (if setting-parameters
                (%zlib-call "deflateParams" sb-alien:int
                             ((* openldk-z-stream) stream)
                             (sb-alien:int (ash parameters -3))
                             (sb-alien:int (logand (ash parameters -1) 3)))
                (%zlib-call "deflate" sb-alien:int
                             ((* openldk-z-stream) stream)
                             (sb-alien:int flush)))))
      (unless (member status '(0 1 -5))
        (internal-error "deflate failed with zlib status ~D" status))
      (%zlib-result stream input-length output-length (= status 1)
                    (and setting-parameters (/= status 0))))))

(defmethod |deflateBytesBytes(J[BII[BIIII)|
    ((deflater t) address input input-offset input-length
     output output-offset output-length flush parameters)
  (declare (ignore deflater))
  (let ((input-memory (%zlib-array-to-alien input input-offset input-length))
        (output-memory (sb-alien:make-alien sb-alien:unsigned-char
                                            (max output-length 1))))
    (unwind-protect
         (let ((result (%deflate-saps address
                                      (sb-alien:alien-sap input-memory) input-length
                                      (sb-alien:alien-sap output-memory) output-length
                                      flush parameters)))
           (let ((written (logand (ash result -31) #x7fffffff)))
             (%zlib-alien-to-array output-memory output output-offset written))
           result)
      (sb-alien:free-alien output-memory)
      (sb-alien:free-alien input-memory))))

(defmethod |deflateBytesBuffer(J[BIIJIII)|
    ((deflater t) address input input-offset input-length
     output-address output-length flush parameters)
  (declare (ignore deflater))
  (let ((input-memory (%zlib-array-to-alien input input-offset input-length)))
    (unwind-protect
         (%deflate-saps address
                        (sb-alien:alien-sap input-memory) input-length
                        (sb-sys:int-sap output-address) output-length
                        flush parameters)
      (sb-alien:free-alien input-memory))))

(defmethod |deflateBufferBytes(JJI[BIIII)|
    ((deflater t) address input-address input-length
     output output-offset output-length flush parameters)
  (declare (ignore deflater))
  (let ((output-memory (sb-alien:make-alien sb-alien:unsigned-char
                                            (max output-length 1))))
    (unwind-protect
         (let ((result (%deflate-saps address
                                      (sb-sys:int-sap input-address) input-length
                                      (sb-alien:alien-sap output-memory) output-length
                                      flush parameters)))
           (let ((written (logand (ash result -31) #x7fffffff)))
             (%zlib-alien-to-array output-memory output output-offset written))
           result)
      (sb-alien:free-alien output-memory))))

(defmethod |deflateBufferBuffer(JJIJIII)|
    ((deflater t) address input-address input-length
     output-address output-length flush parameters)
  (declare (ignore deflater))
  (%deflate-saps address
                 (sb-sys:int-sap input-address) input-length
                 (sb-sys:int-sap output-address) output-length
                 flush parameters))

(defun |java/util/zip/Deflater.setDictionary(J[BII)| (address dictionary offset length)
  (let ((memory (%zlib-array-to-alien dictionary offset length)))
    (unwind-protect
         (let ((status
                 (%zlib-call "deflateSetDictionary" sb-alien:int
                              ((* openldk-z-stream) (%zlib-stream address))
                              ((* sb-alien:unsigned-char) memory)
                              (sb-alien:unsigned-int length))))
           (unless (zerop status)
             (%throw-zlib-illegal-argument "deflateSetDictionary" status)))
      (sb-alien:free-alien memory)))
  nil)

(defun |java/util/zip/Deflater.setDictionaryBuffer(JJI)| (address buffer-address length)
  (let ((status
          (%zlib-call "deflateSetDictionary" sb-alien:int
                       ((* openldk-z-stream) (%zlib-stream address))
                       ((* sb-alien:unsigned-char)
                        (sb-alien:sap-alien (sb-sys:int-sap buffer-address)
                                            (* sb-alien:unsigned-char)))
                       (sb-alien:unsigned-int length))))
    (unless (zerop status)
      (%throw-zlib-illegal-argument "deflateSetDictionary" status)))
  nil)

(defun |java/util/zip/Deflater.getAdler(J)| (address)
  (unsigned-to-signed-integer
   (sb-alien:slot (sb-alien:deref (%zlib-stream address)) 'adler)))

(defun |java/util/zip/Deflater.reset(J)| (address)
  (unless (zerop (%zlib-call "deflateReset" sb-alien:int
                             ((* openldk-z-stream) (%zlib-stream address))))
    (internal-error "deflateReset failed"))
  nil)

(defun |java/util/zip/Deflater.end(J)| (address)
  (let ((stream (%zlib-stream address)))
    (unless (zerop (%zlib-call "deflateEnd" sb-alien:int
                               ((* openldk-z-stream) stream)))
      (internal-error "deflateEnd failed"))
    (%free-zlib-stream address))
  nil)
