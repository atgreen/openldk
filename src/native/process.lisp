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

;;; Child processes: java.lang.ProcessImpl and ProcessHandleImpl natives,
;;; on SBCL's RUN-PROGRAM.  forkAndExec hands Java the pipes to the child
;;; as integer file descriptors, registered in *FD-STREAMS* so the file
;;; stream natives (system.lisp) read and write them.

(in-package :openldk)

(defvar *child-processes* (make-hash-table :synchronized t)
  "pid -> the SB-EXT:PROCESS started for it by forkAndExec.")

(defun %java-bytes-to-string (bytes &optional (start 0) end)
  "Decode the octets of the Java byte array BYTES from START to END as UTF-8."
  (let ((data (java-array-data bytes)))
    (sb-ext:octets-to-string
     (map '(vector (unsigned-byte 8)) (lambda (b) (logand b #xFF))
          (subseq data start (or end (length data))))
     :external-format :utf-8)))

(defun %java-c-string (bytes)
  "The string in BYTES, a NUL-terminated Java byte array, or NIL for null."
  (when bytes
    (let ((nul (position 0 (java-array-data bytes))))
      (%java-bytes-to-string bytes 0 nul))))

(defun %java-string-block (bytes count)
  "The first COUNT NUL-terminated strings packed into the Java byte array
BYTES (ProcessImpl's argument and environment blocks)."
  (when (and bytes (plusp count))
    (let ((data (java-array-data bytes))
          (strings '())
          (start 0))
      (dotimes (i count)
        (let ((end (or (position 0 data :start start) (length data))))
          (push (%java-bytes-to-string bytes start end) strings)
          (setf start (1+ end))))
      (nreverse strings))))

(defun %process-redirect (fd index)
  "RUN-PROGRAM's :input/:output/:error argument for ProcessImpl's request
FD for standard stream INDEX: -1 asks for a pipe, INDEX itself to inherit
ours, and a redirect to a file arrives as the file's Lisp stream."
  (declare (ignore index))
  (cond
    ((eql fd -1) :stream)
    ((streamp fd) fd)
    ((and (integerp fd) (gethash fd *fd-streams*)))
    (t t)))

(defun %register-pipe (stream)
  "Register STREAM, a pipe to a child, and return its file descriptor."
  (let ((fd (sb-sys:fd-stream-fd stream)))
    (setf (gethash fd *fd-streams*) stream)
    fd))

(defun %signal-io-exception (format-control &rest args)
  (let ((exc (%make-java-instance "java/io/IOException")))
    (|<init>(Ljava/lang/String;)V| exc (jstring (apply #'format nil format-control args)))
    (error (%lisp-condition exc))))

(defmethod |forkAndExec(I[B[B[BI[BI[B[IZ)I|
    (this mode helperpath prog arg-block argc env-block envc dir fds redirect-error-stream)
  "Start the program PROG with the arguments in ARG-BLOCK, the environment in
ENV-BLOCK (NIL to inherit ours) and the working directory DIR, and return
its pid.  FDS holds the requests for stdin, stdout and stderr (see
%PROCESS-REDIRECT); on return it holds our end of each pipe, or -1."
  (declare (ignore this mode helperpath))
  (let* ((program (%java-c-string prog))
         (args (%java-string-block arg-block argc))
         (env (%java-string-block env-block envc))
         (directory (%java-c-string dir))
         (redirect-error (and redirect-error-stream (not (eql redirect-error-stream 0))))
         (process
           (handler-case
               (apply #'sb-ext:run-program program args
                      :search t
                      :wait nil
                      :input (%process-redirect (jaref fds 0) 0)
                      :output (%process-redirect (jaref fds 1) 1)
                      :error (if redirect-error
                                 :output
                                 (%process-redirect (jaref fds 2) 2))
                      (append (when env (list :environment env))
                              (when directory (list :directory directory))))
             (error (e)
               (%signal-io-exception "error=2, ~A" e)))))
    (setf (jaref fds 0) (if (sb-ext:process-input process)
                            (%register-pipe (sb-ext:process-input process))
                            -1))
    (setf (jaref fds 1) (if (sb-ext:process-output process)
                            (%register-pipe (sb-ext:process-output process))
                            -1))
    (setf (jaref fds 2) (if (and (not redirect-error) (sb-ext:process-error process))
                            (%register-pipe (sb-ext:process-error process))
                            -1))
    (setf (gethash (sb-ext:process-pid process) *child-processes*) process)
    (sb-ext:process-pid process)))

(defun |java/lang/ProcessHandleImpl.initNative()V| ()
  nil)

(defun |java/lang/ProcessHandleImpl.getCurrentPid0()J| ()
  (sb-unix:unix-getpid))

(defun |java/lang/ProcessHandleImpl.waitForProcessExit0(JZ)I| (pid reap)
  "Wait for the child PID to exit and return its exit status, or 128 plus
the signal number if a signal killed it, as the JDK does."
  (let ((process (gethash pid *child-processes*)))
    (if (null process)
        0
        (progn
          (sb-ext:process-wait process)
          (when (and reap (not (eql reap 0)))
            (remhash pid *child-processes*))
          (if (eq (sb-ext:process-status process) :signaled)
              (+ 128 (sb-ext:process-exit-code process))
              (sb-ext:process-exit-code process))))))

(defun |java/lang/ProcessHandleImpl.isAlive0(J)J| (pid)
  "0 (alive, start time unknown) if PID is running, -1 if not."
  (let ((process (gethash pid *child-processes*)))
    (cond
      (process (if (sb-ext:process-alive-p process) 0 -1))
      ((eql pid (sb-unix:unix-getpid)) 0)
      (t (if (sb-unix:unix-kill pid 0) 0 -1)))))

(defun |java/lang/ProcessHandleImpl.destroy0(JJZ)Z| (pid start-time force)
  "Send PID SIGKILL if FORCE, else SIGTERM; 1 if the signal was sent."
  (declare (ignore start-time))
  (let ((signal (if (and force (not (eql force 0))) sb-unix:sigkill sb-unix:sigterm)))
    (if (sb-unix:unix-kill pid signal) 1 0)))
