;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: OPENLDK; Base: 10 -*-
;;;
;;; Copyright (C) 2025  Anthony Green <green@moxielogic.com>
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

(defclass/std <java-input-stream> (trivial-gray-streams:fundamental-binary-input-stream)
  ((pos :std 0)
   (buf)
   (java-stream :doc "The underlying Java InputStream.")))

(defmethod trivial-gray-streams:stream-read-byte ((stream <java-input-stream>))
  (unwind-protect
       (progn
         (when *debug-trace*
           (format t "~&~V@A trace: <java-input-stream> stream-read-byte(~A)" (incf *call-nesting-level* 1) "*" stream))
         (let ((java-stream (java-stream stream)))
           (let ((byte (|read()| java-stream)))
             (incf (pos stream))
             (if (eql byte -1)
                 :eof
                 byte))))
    (incf *call-nesting-level* -1)))

(defmethod trivial-gray-streams:stream-read-sequence ((stream <java-input-stream>) sequence start end &key)
  (unwind-protect
       (progn
         (when *debug-trace*
           (format t "~&~V@A trace: <java-input-stream> stream-read-sequence(~A ~A ~A ~A)" (incf *call-nesting-level* 1) "*" stream sequence start end))
         (let ((java-stream (java-stream stream)))
           (let* ((buffer (make-array (- end start) :element-type '(unsigned-byte 8)))
                  (bytes-read (|readBytes([BII)| java-stream buffer 0 (- end start))))
             (if (eql bytes-read -1)
                 start ; Return the start position if no bytes were read (EOF)
                 (progn
                   (incf (pos stream) bytes-read)
                   (replace sequence buffer :start1 start :end1 (+ start bytes-read))
                   (+ start bytes-read))))))
    (incf *call-nesting-level* -1)))

#|
(defmethod trivial-gray-streams:stream-close ((stream <java-input-stream>))
  "Close the underlying Java InputStream and then call the next method."
  (unwind-protect
      (progn
        (when *debug-trace*
          (format t "~&~V@A trace: <java-input-stream> stream-close(~A)"
                  (incf *call-nesting-level* 1) "*" stream))
        ;; Close the underlying Java stream
        (|close(Ljava/io/InputStream;)| (java-stream stream))
        ;; Call-next-method typically sets the stream's state to closed.
        (call-next-method))
    (incf *call-nesting-level* -1)))
|#

(defmethod trivial-gray-streams:stream-listen ((stream <java-input-stream>))
  "Return T if at least one byte is available immediately, else NIL."
  (unwind-protect
      (progn
        (when *debug-trace*
          (format t "~&~V@A trace: <java-input-stream> stream-listen(~A)"
                  (incf *call-nesting-level* 1) "*" stream))
        (let* ((java-stream (java-stream stream))
               (available (|available(Ljava/io/InputStream;)| java-stream)))
          (if (plusp available) t nil)))
    (incf *call-nesting-level* -1)))

(defmethod trivial-gray-streams:stream-clear-input ((stream <java-input-stream>))
  "Attempt to skip any currently available bytes in the buffer."
  (unwind-protect
      (progn
        (when *debug-trace*
          (format t "~&~V@A trace: <java-input-stream> stream-clear-input(~A)"
                  (incf *call-nesting-level* 1) "*" stream))
        (let* ((java-stream (java-stream stream))
               (available (|available(Ljava/io/InputStream;)| java-stream)))
          (when (plusp available)
            (|skip(Ljava/io/InputStream;J)| java-stream (coerce available 'long)))))
    (incf *call-nesting-level* -1)))

;; For a purely input stream, we typically define no-ops for finish-output and force-output
(defmethod trivial-gray-streams:stream-file-position ((stream <java-input-stream>))
  (pos stream))

(defmethod (setf trivial-gray-streams:stream-file-position) (newval (stream <java-input-stream>))
  (let ((file-stream (slot-value (java-stream stream) '|fd|)))
    (file-position file-stream newval)
    (setf (pos stream) (file-position file-stream))))

;; For a purely input stream, we typically define no-ops for finish-output and force-output
(defmethod trivial-gray-streams:stream-finish-output ((stream <java-input-stream>))
  "No-op on an input stream."
  nil)

(defmethod trivial-gray-streams:stream-force-output ((stream <java-input-stream>))
  "No-op on an input stream."
  nil)
