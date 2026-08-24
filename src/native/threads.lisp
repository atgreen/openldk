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

;;; Thread, virtual-thread, and fiber native methods.

(in-package :openldk)

;;; The current |java/lang/Thread| object.
(defvar *current-thread* nil)

(defmethod |add(Ljava/lang/Thread;)| (thread-group thread)
  (declare (ignore thread-group))
  (declare (ignore thread))
  (unimplemented "ThreadGroup.add(Thread)"))

(defmethod |java/lang/Thread.currentThread()| ()
  "Return the Java Thread object for the current Lisp thread (or fiber)."
  ;; When in a fiber, check fiber mapping first
  #+sb-fiber
  (when (in-fiber-p)
    (let ((java-thread (gethash (sb-thread:current-fiber) *fiber-to-java-threads*)))
      (when java-thread
        (return-from |java/lang/Thread.currentThread()| java-thread))))
  ;; Check if current Lisp thread has an associated Java Thread
  (let* ((current-lisp-thread (bordeaux-threads:current-thread))
         (java-thread (gethash current-lisp-thread *lisp-to-java-threads*)))
    (or java-thread
        ;; Fallback to main thread (for compatibility).
        ;; Re-register the mapping so the current lisp thread is properly tracked.
        (when *current-thread*
          (setf (gethash current-lisp-thread *lisp-to-java-threads*) *current-thread*)
          *current-thread*)
        ;; Create main thread if it doesn't exist
        (let ((thread (%make-java-instance "java/lang/Thread"))
              (thread-group (%make-java-instance "java/lang/ThreadGroup")))
          (|<init>()| thread-group)
          (setf *current-thread* thread)
          ;; Register main thread in our mappings
          (setf (gethash current-lisp-thread *lisp-to-java-threads*) thread)
          ;; Set priority — JDK 21 moved this to Thread$FieldHolder, but it may
          ;; still be a direct slot on Thread depending on class version.
          (when (slot-exists-p thread '|priority|)
            (setf (slot-value thread '|priority|) 1))
          ;; Initialize the FieldHolder for JDK 21+ Thread structure
          (when (slot-exists-p thread '|holder|)
            (when (classload "java/lang/Thread$FieldHolder")
              (let ((holder (%make-java-instance "java/lang/Thread$FieldHolder")))
                (when (slot-exists-p holder '|group|)
                  (setf (slot-value holder '|group|) thread-group))
                (when (slot-exists-p holder '|priority|)
                  (setf (slot-value holder '|priority|) 5)) ;; NORM_PRIORITY
                (when (slot-exists-p holder '|daemon|)
                  (setf (slot-value holder '|daemon|) 0))
                (setf (slot-value thread '|holder|) holder))))
          (handler-case
              (|<init>(Ljava/lang/ThreadGroup;Ljava/lang/Runnable;Ljava/lang/String;J)|
               thread thread-group nil (jstring "main") 0)
            (condition ()
              ;; If the old constructor doesn't work, set name directly
              (setf (slot-value thread '|name|) (jstring "main"))))
          thread))))

(defmethod |setPriority0(I)| ((thread |java/lang/Thread|) priority)
  "Thread priorities are advisory; SBCL threads have none."
  (declare (ignore thread priority))
  nil)

(defmethod |isAlive()| ((thread |java/lang/Thread|))
  (let ((lisp-thread (gethash thread *java-threads*)))
    (cond
      ;; A started platform thread: alive iff its Lisp thread is.
      (lisp-thread
       (if (bordeaux-threads:thread-alive-p lisp-thread) 1 0))
      #+sb-fiber
      ((gethash thread *java-to-fibers*)
       (if (sb-thread:fiber-alive-p (gethash thread *java-to-fibers*)) 1 0))
      ;; The calling thread's own Java object (e.g. the main thread) is
      ;; only registered in the lisp->java direction.
      ((eq thread (gethash (bordeaux-threads:current-thread)
                           *lisp-to-java-threads*))
       1)
      ;; Never started, or its thread is gone.
      (t 0))))

(defmethod |isInterrupted(Z)| ((thread |java/lang/Thread|) clear-interrupted)
  "Interrupted status lives in *THREAD-INTERRUPTED* (see interrupt0)."
  (let ((interrupted (gethash thread *thread-interrupted*)))
    (when (and interrupted
               clear-interrupted
               (not (eql clear-interrupted 0)))
      (setf (gethash thread *thread-interrupted*) nil))
    (if interrupted 1 0)))

;;; JDK 21 Thread native methods for virtual thread support.
;;; OpenLDK treats all threads as platform (carrier) threads.

(defun |java/lang/Thread.currentCarrierThread()| ()
  "Return the current carrier thread (always the OS thread, never a fiber)."
  (let* ((current-lisp-thread (bordeaux-threads:current-thread))
         (java-thread (gethash current-lisp-thread *lisp-to-java-threads*)))
    (or java-thread
        (|java/lang/Thread.currentThread()|))))

(defun |java/lang/Thread.sleep0(J)| (milliseconds)
  "JDK 21 private native sleep0 — replaces public native sleep(J)."
  (|java/lang/Thread.sleep(J)| milliseconds))

(defun |java/lang/Thread.yield0()| ()
  "JDK 21 private native yield0 — replaces public native yield()."
  #+sb-fiber
  (when (in-fiber-p)
    (sb-thread:fiber-yield)
    (return-from |java/lang/Thread.yield0()|))
  nil)

(defvar *scoped-value-cache* nil
  "Thread-local scoped value cache for JDK 21 ScopedValue support.")

(defun |java/lang/Thread.scopedValueCache()| ()
  "Return the current thread's scoped value cache."
  *scoped-value-cache*)

(defun |java/lang/Thread.setScopedValueCache([Ljava/lang/Object;)| (cache)
  "Set the current thread's scoped value cache."
  (setf *scoped-value-cache* cache))

(defun |java/lang/Thread.findScopedValueBindings()| ()
  "Return scoped value bindings for the current thread."
  nil)

(defun |java/lang/Thread.ensureMaterializedForStackWalk(Ljava/lang/Object;)| (obj)
  "No-op for platform threads."
  (declare (ignore obj))
  nil)

(defvar *next-thread-id* 0
  "Atomic thread ID counter for JDK 21 Thread$ThreadIdentifiers.")

(defun |java/lang/Thread.getNextThreadIdOffset()| ()
  "Return the field offset for the thread ID counter.
   Returns a dummy value — OpenLDK doesn't use Unsafe for thread IDs."
  0)

;; Override Thread$ThreadIdentifiers.next() to use a simple Lisp counter
;; instead of Unsafe atomic operations on a JVM-internal address.
(setf (gethash "java/lang/Thread$ThreadIdentifiers.next()J" *native-overrides*)
      (lambda ()
        (bordeaux-threads:with-lock-held (*identity-hash-counter-lock*)
          (incf *next-thread-id*))))

(defmethod |setCurrentThread(Ljava/lang/Thread;)| ((thread |java/lang/Thread|) new-thread)
  "Set the current thread reference. Used by virtual thread machinery."
  (declare (ignore thread))
  #+sb-fiber
  (when (in-fiber-p)
    (setf (gethash (sb-thread:current-fiber) *fiber-to-java-threads*) new-thread)
    (return-from |setCurrentThread(Ljava/lang/Thread;)|))
  (let ((lisp-thread (bordeaux-threads:current-thread)))
    (setf (gethash lisp-thread *lisp-to-java-threads*) new-thread)))

(defmethod |getStackTrace0()| ((thread |java/lang/Thread|))
  "Return stack trace elements for the thread."
  (declare (ignore thread))
  nil)

;;; JDK 21: Object.wait0(J) — private native wait.
;;; Must contain the actual wait logic (not delegate to wait(J) which calls wait0).
(defmethod |wait0(J)| ((this |java/lang/Object|) timeout)
  (let* ((monitor (%get-monitor this))
         (mutex (mutex monitor))
         (cv (condition-variable monitor))
         (current-thread (current-thread-identity)))
    (bordeaux-threads:with-lock-held (mutex)
      (unless (eq (owner monitor) current-thread)
        (error (%lisp-condition (%make-throwable '|java/lang/IllegalMonitorStateException|))))
      (let ((saved-recursion (recursion-count monitor)))
        (push current-thread (wait-set monitor))
        (setf (owner monitor) nil (recursion-count monitor) 0)
        (bordeaux-threads:condition-notify cv)
        (loop while (member current-thread (wait-set monitor))
              do (if (zerop timeout)
                     (bordeaux-threads:condition-wait cv mutex)
                     (unless (bordeaux-threads:condition-wait cv mutex :timeout (/ timeout 1000.0))
                       (setf (wait-set monitor) (remove current-thread (wait-set monitor)))
                       (return))))
        (loop while (owner monitor)
              do (bordeaux-threads:condition-wait cv mutex))
        (setf (owner monitor) current-thread (recursion-count monitor) saved-recursion)))))

;;; JDK 21 VirtualThread — JVMTI notification stubs.
;;; OpenLDK doesn't support JVMTI, so all are no-ops.

(defun |java/lang/VirtualThread.registerNatives()| ()
  nil)

(setf (gethash "java/lang/VirtualThread.notifyJvmtiStart()V" *native-overrides*)
      (lambda (this) (declare (ignore this)) nil))

(setf (gethash "java/lang/VirtualThread.notifyJvmtiEnd()V" *native-overrides*)
      (lambda (this) (declare (ignore this)) nil))

(setf (gethash "java/lang/VirtualThread.notifyJvmtiMount(Z)V" *native-overrides*)
      (lambda (this first-mount) (declare (ignore this first-mount)) nil))

(setf (gethash "java/lang/VirtualThread.notifyJvmtiUnmount(Z)V" *native-overrides*)
      (lambda (this first-unmount) (declare (ignore this first-unmount)) nil))

(setf (gethash "java/lang/VirtualThread.notifyJvmtiHideFrames(Z)V" *native-overrides*)
      (lambda (this hide) (declare (ignore this hide)) nil))

;;; JDK 21 Continuation — virtual thread continuation support stubs.
;;; OpenLDK doesn't implement continuations; virtual threads run as platform threads.

(defun |jdk/internal/vm/Continuation.registerNatives()| ()
  nil)

(defun |jdk/internal/vm/Continuation.doYield()| ()
  "Stub: yield from continuation. Returns 0 (success)."
  0)

(setf (gethash "jdk/internal/vm/Continuation.enterSpecial(Ljdk/internal/vm/Continuation;ZZ)V" *native-overrides*)
      (lambda (cont is-virtual-thread force-yield)
        (declare (ignore cont is-virtual-thread force-yield))
        nil))

(defun |jdk/internal/vm/Continuation.pin()| ()
  nil)

(defun |jdk/internal/vm/Continuation.unpin()| ()
  nil)

(setf (gethash "jdk/internal/vm/Continuation.isPinned0(Ljdk/internal/vm/ContinuationScope;)I" *native-overrides*)
      (lambda (scope)
        (declare (ignore scope))
        0))

(defun |jdk/internal/vm/ContinuationSupport.isSupported0()| ()
  "Continuations are not supported in OpenLDK. This causes the JDK to create
   BoundVirtualThread (extends BaseVirtualThread) instead of VirtualThread.
   BoundVirtualThread uses start0()/run() like platform threads."
  0)

;;; Virtual thread support: BoundVirtualThread (extends BaseVirtualThread).
;;; When ContinuationSupport.isSupported0() returns 0, the JDK creates
;;; BoundVirtualThread instead of VirtualThread. BoundVirtualThread uses
;;; Thread.start() → start0() like platform threads. The task runs via
;;; BoundVirtualThread.run() → runWith(bindings, task).
;;;
;;; These :around methods on BaseVirtualThread ensure isAlive() and join()
;;; work correctly by checking the actual Lisp thread status. Thread.join()
;;; in bytecode uses `while(isAlive()) { wait(0); }` for non-VirtualThread
;;; instances, so correct isAlive() is critical. We also override join(J)
;;; directly to use bordeaux-threads:join-thread for reliable waiting.

(defmethod |isAlive()| :around ((thread |java/lang/BaseVirtualThread|))
  "Check if the virtual thread's underlying thread (OS or fiber) is still running."
  #+sb-fiber
  (let ((fiber (gethash thread *java-to-fibers*)))
    (when fiber
      (return-from |isAlive()| (if (sb-thread:fiber-alive-p fiber) 1 0))))
  (let ((lisp-thread (gethash thread *java-threads*)))
    (if (and lisp-thread (bordeaux-threads:thread-alive-p lisp-thread))
        1
        0)))

(defmethod |join(J)| :around ((thread |java/lang/BaseVirtualThread|) millis)
  "Wait for the virtual thread to finish (OS thread or fiber)."
  #+sb-fiber
  (let ((fiber (gethash thread *java-to-fibers*)))
    (when fiber
      (cond
        ((not (sb-thread:fiber-alive-p fiber)) (return-from |join(J)|))
        (t
         (if (zerop millis)
             (sb-thread:fiber-join fiber)
             (sb-thread:fiber-join fiber :timeout (/ millis 1000.0d0)))
         (return-from |join(J)|)))))
  ;; Fall through to OS thread join
  (let ((lisp-thread (gethash thread *java-threads*)))
    (cond
      ((null lisp-thread) nil)
      ((not (bordeaux-threads:thread-alive-p lisp-thread)) nil)
      ((zerop millis)
       (bordeaux-threads:join-thread lisp-thread)
       nil)
      (t
       (let ((timeout-sec (/ millis 1000.0d0)))
         ;; bordeaux-threads 0.9.x join-thread has no timeout; use SBCL's, which
         ;; supports :timeout/:default (bt1 threads are sb-thread:thread objects).
         (handler-case
             (sb-thread:join-thread lisp-thread :timeout timeout-sec :default nil)
           (sb-thread:join-thread-error () nil)))))))

;;; Fiber-based start0 for virtual threads (when SBCL has fiber support).
;;; Creates a fiber instead of an OS thread for BaseVirtualThread instances.
#+sb-fiber
(defmethod |start0()| :around ((thread |java/lang/BaseVirtualThread|))
  "Start a virtual thread as a fiber on the fiber scheduler."
  (let ((fiber (sb-thread:make-fiber
                (lambda ()
                  (let ((*scoped-value-cache* nil))
                    (unwind-protect
                         (progn
                           ;; Register fiber-to-Java mapping
                           (setf (gethash (sb-thread:current-fiber) *fiber-to-java-threads*) thread)
                           ;; Call the Thread's run() method
                           (handler-case
                               (|run()| thread)
                             (error (e)
                               (format *error-output* "~&VThread ~A terminated with error: ~A~%" thread e))))
                      ;; Cleanup mappings
                      (remhash (sb-thread:current-fiber) *fiber-to-java-threads*)
                      (remhash thread *java-to-fibers*)
                      (remhash thread *fiber-park-flags*))))
                :name (format nil "Java-VThread-~A"
                              (if (slot-boundp thread '|name|)
                                  (slot-value thread '|name|) "?")))))
    ;; Store the fiber mapping
    (setf (gethash thread *java-to-fibers*) fiber)
    ;; Submit to the fiber scheduler
    (submit-virtual-thread-fiber fiber)))

(defun |sun/misc/Unsafe.registerNatives()| ()
  nil)

(defun |jdk/internal/misc/Unsafe.registerNatives()| ()
  nil)

(defun |jdk/internal/misc/ScopedMemoryAccess.registerNatives()| ()
  nil)

;; Runtime native methods
(defun %available-processor-count ()
  "Number of online CPUs, via sysconf(_SC_NPROCESSORS_ONLN) with a
/proc/cpuinfo fallback."
  (or (ignore-errors
        (let ((n (sb-alien:alien-funcall
                  (sb-alien:extern-alien "sysconf" (function sb-alien:long sb-alien:int))
                  84)))                 ; _SC_NPROCESSORS_ONLN on Linux
          (when (plusp n) n)))
      (ignore-errors
        (with-open-file (in "/proc/cpuinfo")
          (loop for line = (read-line in nil)
                while line
                count (and (>= (length line) 9)
                           (string= "processor" line :end2 9)))))
      1))

(defmethod |availableProcessors()| ((rt |java/lang/Runtime|))
  (%available-processor-count))

(defmethod |freeMemory()| ((rt |java/lang/Runtime|))
  ;; Return SBCL's available dynamic space
  (- (sb-ext:dynamic-space-size) (sb-kernel:dynamic-usage)))

(defmethod |totalMemory()| ((rt |java/lang/Runtime|))
  (sb-kernel:dynamic-usage))

(defmethod |maxMemory()| ((rt |java/lang/Runtime|))
  (sb-ext:dynamic-space-size))

(defmethod |gc()| ((rt |java/lang/Runtime|))
  (sb-ext:gc :full t)
  nil)

;; JDK 9+: Signal native methods — no-op stubs for Terminator.setup()
(defun |jdk/internal/misc/Signal.findSignal0(Ljava/lang/String;)| (name)
  (declare (ignore name))
  ;; Return a dummy signal number
  0)

(defun |jdk/internal/misc/Signal.handle0(IJ)| (sig handler)
  (declare (ignore sig handler))
  ;; Return 0 (success / previous handler was default)
  0)

;; JDK 9+: StringUTF16 native — x86_64 is little-endian
(defun |java/lang/StringUTF16.isBigEndian()| ()
  0)

;; JDK 9+: BootLoader resource loading — delegates to classpath infrastructure.
;; These are bytecoded in JDK but depend on BuiltinClassLoader internals we
;; don't have, so we provide direct implementations.
(setf (gethash "jdk/internal/loader/BootLoader.findResource(Ljava/lang/String;)Ljava/net/URL;" *native-overrides*)
      (lambda (name)
        (let* ((resource-name (lstring name))
               (url-string (get-resource-url-on-classpath resource-name)))
          (when url-string
            (%make-url-from-string url-string)))))

(setf (gethash "jdk/internal/loader/BootLoader.findResourceAsStream(Ljava/lang/String;Ljava/lang/String;)Ljava/io/InputStream;" *native-overrides*)
      (lambda (module-name name)
        (declare (ignore module-name))
        (let* ((resource-name (lstring name))
               (stream (open-resource-on-classpath resource-name)))
          (when stream
            (let ((bytes (flexi-streams:with-output-to-sequence (out)
                           (loop for byte = (read-byte stream nil nil)
                                 while byte do (write-byte byte out)))))
              (close stream)
              (let ((bais (%make-java-instance "java/io/ByteArrayInputStream")))
                (|<init>([B)| bais
                 (make-java-array
                  :component-class (%get-java-class-by-bin-name "byte")
                  :initial-contents (coerce bytes 'vector)))
                bais))))))

;; JDK 9+: NativeLibraries — return nil (no built-in libraries)
(defun |jdk/internal/loader/NativeLibraries.findBuiltinLib(Ljava/lang/String;)| (name)
  (declare (ignore name))
  nil)

