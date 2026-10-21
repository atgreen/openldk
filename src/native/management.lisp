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
;;; SBCL's run time (the process's); counters it doesn't keep read 0.

(in-package :openldk)

(defun |sun/management/VMManagementImpl.getVersion0()| ()
  (jstring "4.0"))

(defun |sun/management/VMManagementImpl.initOptionalSupportFields()| ()
  (let ((statics (symbol-value (intern "+static-sun/management/VMManagementImpl+" :openldk))))
    (setf (slot-value statics '|currentThreadCpuTimeSupport|) 1)))

(defun %current-thread-cpu-time-nanos ()
  (* (get-internal-run-time) (/ 1000000000 internal-time-units-per-second)))

(defun %thread-cpu-time (id)
  "CPU time in nanoseconds of the thread with ID (0 for the current one), or
-1 when it isn't known: only the current thread's is."
  (if (or (zerop id) (eql id (|threadId()| (|java/lang/Thread.currentThread()|))))
      (%current-thread-cpu-time-nanos)
      -1))

(defun |sun/management/ThreadImpl.getThreadTotalCpuTime0(J)| (id)
  (%thread-cpu-time id))

(defun |sun/management/ThreadImpl.getThreadUserCpuTime0(J)| (id)
  (%thread-cpu-time id))

(defun |sun/management/ThreadImpl.setThreadCpuTimeEnabled0(Z)| (enable)
  (declare (ignore enable))
  nil)

(defun |sun/management/ThreadImpl.setThreadContentionMonitoringEnabled0(Z)| (enable)
  (declare (ignore enable))
  nil)

(defun |sun/management/ThreadImpl.setThreadAllocatedMemoryEnabled0(Z)| (enable)
  (declare (ignore enable))
  nil)

(defun |sun/management/ThreadImpl.getThreads()| ()
  (|java/lang/Thread.getThreads()|))

(defmacro %define-vm-management-natives (&rest specs)
  "Define VMManagementImpl's instance natives: each spec is (NAME VALUE-FORM)."
  `(progn
     ,@(loop for (name value) in specs
             collect `(defmethod ,(intern name :openldk) (this)
                        (declare (ignore this))
                        ,value))))

(defun %live-java-thread-count ()
  (java-array-length (|java/lang/Thread.getThreads()|)))

(%define-vm-management-natives
 ("isThreadContentionMonitoringEnabled()" 0)
 ("isThreadCpuTimeEnabled()" 1)
 ("isThreadAllocatedMemoryEnabled()" 0)
 ("endAOTRecording()" 0)
 ("getTotalClassCount()" (hash-table-count *java-classes-by-bin-name*))
 ("getUnloadedClassCount()" 0)
 ("getVerboseClass()" 0)
 ("getVerboseGC()" 0)
 ("getProcessId()" (sb-posix:getpid))
 ("getVmArguments0()" (make-java-array :component-class (%get-java-class-by-bin-name "java/lang/String")
                                       :size 0))
 ("getStartupTime()" (or *vm-start-time-millis* 0))
 ("getUptime0()" (if *vm-start-time-millis*
                     (- (|java/lang/System.currentTimeMillis()|) *vm-start-time-millis*)
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
