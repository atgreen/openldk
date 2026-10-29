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

;;; java.lang.management: the natives of sun.management.VMManagementImpl and
;;; ThreadImpl, enough for ManagementFactory's MXBeans (the Kotlin compiler
;;; measures its phases with the ThreadMXBean).  Of the optional monitoring,
;;; OpenLDK supports thread CPU time, for the current thread only, from
;;; Linux per-thread resource usage; counters it doesn't keep read 0.

(in-package :openldk)

(defun |sun/management/VMManagementImpl.getVersion0()Ljava/lang/String;| ()
  "Return the management interface version implemented by OpenLDK."
  (jstring "4.0"))

(defun |sun/management/VMManagementImpl.initOptionalSupportFields()V| ()
  "Publish support for current-thread CPU time to the management interface."
  (let ((statics (symbol-value (intern "+static-sun/management/VMManagementImpl+" :openldk))))
    (setf (slot-value statics '|currentThreadCpuTimeSupport|)
          (if (minusp (%current-thread-cpu-time-nanos)) 0 1))))

(defun %current-thread-cpu-time-nanos (&optional user-only)
  "Read Linux RUSAGE_THREAD, never the process-wide Lisp runtime counter."
  #+(and linux 64-bit)
  (sb-alien:with-alien ((usage (array sb-alien:long 18)))
    ;; Linux x86-64 struct rusage is two timevals followed by 14 longs.
    (if (zerop (sb-alien:alien-funcall
                (sb-alien:extern-alien "getrusage" (function sb-alien:int sb-alien:int (* sb-alien:long)))
                1 (sb-alien:cast (sb-alien:addr usage) (* sb-alien:long))))
        (+ (* (sb-alien:deref usage 0) 1000000000)
           (* (sb-alien:deref usage 1) 1000)
           (if user-only 0
               (+ (* (sb-alien:deref usage 2) 1000000000)
                  (* (sb-alien:deref usage 3) 1000))))
        -1))
  #-(and linux 64-bit)
  (locally (declare (ignore user-only)) -1))

(defun %thread-cpu-time (id &optional user-only)
  "CPU time of the calling platform thread, or -1 when unavailable."
  (if (or (zerop id) (eql id (|threadId()| (|java/lang/Thread.currentThread()Ljava/lang/Thread;|))))
      (%current-thread-cpu-time-nanos user-only)
      -1))

(defun |sun/management/ThreadImpl.getThreadTotalCpuTime0(J)J| (id)
  "Return current-thread user plus system CPU time in nanoseconds."
  (%thread-cpu-time id))

(defun |sun/management/ThreadImpl.getThreadUserCpuTime0(J)J| (id)
  "Return current-thread user CPU time in nanoseconds."
  (%thread-cpu-time id t))

(defun |sun/management/ThreadImpl.setThreadCpuTimeEnabled0(Z)V| (enable)
  "Accept the CPU-time monitoring flag; runtime accounting is always available."
  (declare (ignore enable))
  nil)

(defun |sun/management/ThreadImpl.setThreadContentionMonitoringEnabled0(Z)V| (enable)
  "Accept the monitoring flag without enabling unsupported contention accounting."
  (declare (ignore enable))
  nil)

(defun |sun/management/ThreadImpl.setThreadAllocatedMemoryEnabled0(Z)V| (enable)
  "Accept the monitoring flag without enabling unsupported allocation accounting."
  (declare (ignore enable))
  nil)

(defun |sun/management/ThreadImpl.getThreads()[Ljava/lang/Thread;| ()
  "Return the currently registered live Java threads."
  (|java/lang/Thread.getThreads()[Ljava/lang/Thread;|))

(defun %managed-thread-carrier (thread)
  "The live Lisp thread carrying the platform Java THREAD, if any."
  (let ((carrier (or (gethash thread *java-threads*)
                     (loop for carrier being the hash-keys of *lisp-to-java-threads*
                             using (hash-value java-thread)
                           when (eq thread java-thread) return carrier))))
    (when (and carrier (bt:thread-alive-p carrier)) carrier)))

(defun %managed-thread-snapshot (thread max-depth)
  "Capture (state lock owner frames) on THREAD's carrier at an interrupt point.
Only collect Lisp data there: loading classes or constructing Java objects in
the sampled thread could deadlock against the requesting thread's class locks."
  (when-let ((carrier (%managed-thread-carrier thread)))
    (let ((snapshot nil)
          (ready (sb-thread:make-semaphore :count 0)))
      (flet ((capture ()
               (let* ((blocking *java-thread-blocking-state*)
                      (monitor (third blocking))
                      (owner (and monitor (owner monitor)))
                      (frames (unless (zerop max-depth)
                                (serapeum:filter #'%java-frame-p (%safe-list-backtrace)))))
                 (setf snapshot
                       (list (or (first blocking) 4) (second blocking)
                             (and owner (gethash owner *lisp-to-java-threads*))
                             (if (and (plusp max-depth) (< max-depth (length frames)))
                                 (serapeum:take max-depth frames) frames))))
               (sb-thread:signal-semaphore ready)))
        (if (eq carrier (bt:current-thread))
            (capture)
            (handler-case
                (progn
                  (sb-thread:interrupt-thread carrier #'capture)
                  ;; Foreign code can defer interrupts indefinitely. An
                  ;; unavailable stack is allowed; never stall the monitor.
                  (sb-thread:wait-on-semaphore ready :timeout 0.25))
              (sb-thread:interrupt-thread-error () nil))))
      (when (%managed-thread-carrier thread)
        (or snapshot (list 4 nil nil nil))))))

(defun %managed-thread-info (thread max-depth)
  "Build a JDK ThreadInfo from a live platform thread's actual snapshot."
  (when-let ((snapshot (%managed-thread-snapshot thread max-depth)))
    (destructuring-bind (state lock owner frames) snapshot
      (let ((info (%make-java-instance "java/lang/management/ThreadInfo"))
            (trace (make-java-array
                    :component-class (%get-java-class-by-bin-name "java/lang/StackTraceElement")
                    :initial-contents
                    (mapcar (lambda (frame)
                              (%fill-stack-trace-element
                               (%make-java-instance "java/lang/StackTraceElement") frame))
                            frames))))
        (|<init>(Ljava/lang/Thread;ILjava/lang/Object;Ljava/lang/Thread;JJJJ[Ljava/lang/StackTraceElement;)V|
         info thread state lock owner 0 -1 0 -1 trace)
        info))))

(defun %managed-thread-infos (ids max-depth)
  "ThreadInfo array preserving IDS order, with NIL for absent/dead threads."
  (%clinit (classload "java/lang/management/ThreadInfo"))
  (let* ((threads (java-array-data (|java/lang/Thread.getThreads()[Ljava/lang/Thread;|)))
         (by-id (make-hash-table :test #'eql)))
    (map nil (lambda (thread) (setf (gethash (|threadId()J| thread) by-id) thread)) threads)
    (make-java-array
     :component-class (%get-java-class-by-bin-name "java/lang/management/ThreadInfo")
     :initial-contents
     (map 'vector (lambda (id)
                    (when-let ((thread (gethash id by-id)))
                      (%managed-thread-info thread max-depth)))
          (if ids (java-array-data ids)
              (map 'vector (lambda (thread) (|threadId()J| thread)) threads))))))

(defun |sun/management/ThreadImpl.getThreadInfo1([JI[Ljava/lang/management/ThreadInfo;)V|
    (ids max-depth infos)
  "Fill the caller's ThreadInfo array with bounded stack snapshots."
  (replace (java-array-data infos)
           (java-array-data (%managed-thread-infos ids max-depth)))
  nil)

(defun |sun/management/ThreadImpl.dumpThreads0([JZZI)[Ljava/lang/management/ThreadInfo;|
    (ids locked-monitors locked-synchronizers max-depth)
  "Dump selected or all live platform threads, with their Java stack frames."
  ;; The Java wrapper checks the optional monitor/synchronizer support flags.
  (declare (ignore locked-monitors locked-synchronizers))
  (%managed-thread-infos ids max-depth))

(defmacro %define-vm-management-natives (&rest specs)
  "Define VMManagementImpl's instance natives: each spec is (NAME VALUE-FORM)."
  `(progn
     ,@(loop for (name value) in specs
             collect `(defmethod ,(intern name :openldk) (this)
                        (declare (ignore this))
                        ,value))))

(defun %live-java-thread-count ()
  "Count the currently registered live Java threads."
  (java-array-length (|java/lang/Thread.getThreads()[Ljava/lang/Thread;|)))

(%define-vm-management-natives
 ("isThreadContentionMonitoringEnabled()" 0)
 ("isThreadCpuTimeEnabled()" 1)
 ("isThreadAllocatedMemoryEnabled()" 0)
 ("endAOTRecording()" 0)
 ("getTotalClassCount()" (+ (hash-table-count *java-classes-by-bin-name*) *unloaded-java-class-count*))
 ("getUnloadedClassCount()" *unloaded-java-class-count*)
 ("getVerboseClass()" 0)
 ("getVerboseGC()" 0)
 ("getProcessId()" (sb-posix:getpid))
 ("getVmArguments0()" (make-java-array :component-class (%get-java-class-by-bin-name "java/lang/String")
                                       :size 0))
 ("getStartupTime()" (or *vm-start-time-millis* 0))
 ("getUptime0()J" (if *vm-start-time-millis*
                     (- (|java/lang/System.currentTimeMillis()J|) *vm-start-time-millis*)
                     0))
 ("getAvailableProcessors()" (%available-processor-count))
 ("getTotalCompileTime()" 0)
 ("getTotalThreadCount()" (%live-java-thread-count))
 ("getLiveThreadCount()" (%live-java-thread-count))
 ("getPeakThreadCount()" (%live-java-thread-count))
 ("getDaemonThreadCount()" 0)
 ("getSafepointCount()" 0)
 ("getTotalSafepointTime()" 0)
 ("getSafepointSyncTime()" 0)
 ("getTotalApplicationNonStoppedTime()" 0)
 ("getLoadedClassSize()" 0)
 ("getUnloadedClassSize()" 0)
 ("getClassLoadingTime()" 0)
 ("getMethodDataSize()" 0)
 ("getInitializedClassCount()" 0)
 ("getClassInitializationTime()" 0)
 ("getClassVerificationTime()" 0))
