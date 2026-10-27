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

(defmethod |add(Ljava/lang/Thread;)V| (thread-group thread)
  (declare (ignore thread-group))
  (declare (ignore thread))
  (unimplemented "ThreadGroup.add(Thread)"))

(defmethod |java/lang/Thread.currentThread()Ljava/lang/Thread;| ()
  "Return the Java Thread object for the current Lisp thread (or fiber)."
  ;; When in a fiber, check fiber mapping first
  #+sb-fiber
  (when (in-fiber-p)
    (let ((java-thread (gethash (uiop:symbol-call :sb-thread :current-fiber) *fiber-to-java-threads*)))
      (when java-thread
        (return-from |java/lang/Thread.currentThread()Ljava/lang/Thread;| java-thread))))
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
          (|<init>()V| thread-group)
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
              (|<init>(Ljava/lang/ThreadGroup;Ljava/lang/Runnable;Ljava/lang/String;J)V|
               thread thread-group nil (jstring "main") 0)
            (condition ()
              ;; If the old constructor doesn't work, set name directly
              (setf (slot-value thread '|name|) (jstring "main"))))
          thread))))

;; A thread's contextClassLoader defaults to the system class loader (never null
;; for the main thread in a real JVM). OpenLDK never initializes the field, so
;; the JIT-compiled getContextClassLoader() returns null, and Clojure's
;; load-data-readers -> data-reader-urls NPEs calling .getResources on it. Read
;; the field directly and fall back to the system class loader when unset. An
;; :around survives JIT compilation of the bytecode body.
(defmethod |getContextClassLoader()Ljava/lang/ClassLoader;| :around ((thread |java/lang/Thread|))
  (or (and (slot-exists-p thread '|contextClassLoader|)
           (slot-boundp thread '|contextClassLoader|)
           (slot-value thread '|contextClassLoader|))
      *boot-class-loader*))

(defmethod |setPriority0(I)V| ((thread |java/lang/Thread|) priority)
  "Thread priorities are advisory; SBCL threads have none."
  (declare (ignore thread priority))
  nil)

(defmethod |isAlive()Z| ((thread |java/lang/Thread|))
  (let ((lisp-thread (gethash thread *java-threads*)))
    (cond
      ;; A started platform thread: alive iff its Lisp thread is.
      (lisp-thread
       (if (bordeaux-threads:thread-alive-p lisp-thread) 1 0))
      #+sb-fiber
      ((gethash thread *java-to-fibers*)
       (if (uiop:symbol-call :sb-thread :fiber-alive-p (gethash thread *java-to-fibers*)) 1 0))
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
      (%clear-java-thread-interrupt thread))
    (if interrupted 1 0)))

(defun %clear-java-thread-interrupt (thread)
  "Clear both the native interrupt event and the JDK's visible status field."
  (setf (gethash thread *thread-interrupted*) nil)
  (when (slot-exists-p thread '|interrupted|)
    (setf (slot-value thread '|interrupted|) 0)))

(defun |java/lang/Thread.clearInterruptEvent()V| ()
  (%clear-java-thread-interrupt (|java/lang/Thread.currentThread()Ljava/lang/Thread;|)))

;;; JDK 21 Thread native methods for virtual thread support.
;;; OpenLDK treats all threads as platform (carrier) threads.

(defun |java/lang/Thread.currentCarrierThread()Ljava/lang/Thread;| ()
  "Return the current carrier thread (always the OS thread, never a fiber)."
  (let* ((current-lisp-thread (bordeaux-threads:current-thread))
         (java-thread (gethash current-lisp-thread *lisp-to-java-threads*)))
    (or java-thread
        (|java/lang/Thread.currentThread()Ljava/lang/Thread;|))))

(defun |java/lang/Thread.getThreads()[Ljava/lang/Thread;| ()
  "The live platform threads (Thread.activeCount, getAllStackTraces)."
  (let ((threads (list (|java/lang/Thread.currentThread()Ljava/lang/Thread;|))))
    (maphash (lambda (lisp-thread java-thread)
               (when (bordeaux-threads:thread-alive-p lisp-thread)
                 (pushnew java-thread threads)))
             *lisp-to-java-threads*)
    (maphash (lambda (java-thread lisp-thread)
               (when (and (bordeaux-threads:threadp lisp-thread)
                          (bordeaux-threads:thread-alive-p lisp-thread))
                 (pushnew java-thread threads)))
             *java-threads*)
    (make-java-array :component-class (%get-java-class-by-bin-name "java/lang/Thread")
                     :initial-contents (coerce (remove nil threads) 'vector))))

(defun |java/lang/Thread.sleep0(J)| (milliseconds)
  "JDK 21 private native sleep0 — replaces public native sleep(J)."
  (%java-thread-sleep milliseconds))

(defun |java/lang/Thread.sleepNanos0(J)V| (nanos)
  "JDK 25 private native sleepNanos0 — Thread.sleep now passes
nanoseconds and its bytecode wrapper clobbers any sleep(J) defun, so
this must hold the real implementation path."
  (%java-thread-sleep (ceiling nanos 1000000)))

(defun |java/lang/Thread.yield0()V| ()
  "JDK 21 private native yield0 — replaces public native yield()."
  #+sb-fiber
  (when (in-fiber-p)
    (uiop:symbol-call :sb-thread :fiber-yield)
    (return-from |java/lang/Thread.yield0()V|))
  ;; Platform thread: actually yield the OS thread (sched_yield).  A no-op here
  ;; turns java.util.concurrent spin-then-yield-then-park backoff loops
  ;; (SynchronousQueue rendezvous, AQS, the cached thread pool Clojure uses) into
  ;; 100%-CPU busy spins that starve the peer thread they wait on -> intermittent
  ;; hang on Compiler.load-heavy paths (clojure.main startup, specter). (ldk-7eu)
  (sb-thread:thread-yield)
  nil)

(defvar *scoped-value-cache* nil
  "Thread-local scoped value cache for JDK 21 ScopedValue support.")

(defun |java/lang/Thread.scopedValueCache()[Ljava/lang/Object;| ()
  "Return the current thread's scoped value cache."
  *scoped-value-cache*)

(defun |java/lang/Thread.setScopedValueCache([Ljava/lang/Object;)V| (cache)
  "Set the current thread's scoped value cache."
  (setf *scoped-value-cache* cache))

(defun |java/lang/Thread.findScopedValueBindings()Ljava/lang/Object;| ()
  "JDK bytecode maintains the current snapshot explicitly; no stack elision occurs."
  (let ((thread (|java/lang/Thread.currentThread()Ljava/lang/Thread;|)))
    (when (and thread (slot-exists-p thread '|scopedValueBindings|))
      (slot-value thread '|scopedValueBindings|))))

(defun |java/lang/Thread.ensureMaterializedForStackWalk(Ljava/lang/Object;)V| (obj)
  "No-op for platform threads."
  (declare (ignore obj))
  nil)

(defvar *next-thread-id* 0
  "Atomic thread ID counter for JDK 21 Thread$ThreadIdentifiers.")

(defun |java/lang/Thread.getNextThreadIdOffset()J| ()
  "Return the field offset for the thread ID counter.
   Returns a dummy value — OpenLDK doesn't use Unsafe for thread IDs."
  0)

;; Override Thread$ThreadIdentifiers.next() to use a simple Lisp counter
;; instead of Unsafe atomic operations on a JVM-internal address.
(setf (gethash "java/lang/Thread$ThreadIdentifiers.next()J" *native-overrides*)
      (lambda ()
        (bordeaux-threads:with-lock-held (*identity-hash-counter-lock*)
          (incf *next-thread-id*))))

(defmethod |setCurrentThread(Ljava/lang/Thread;)V| ((thread |java/lang/Thread|) new-thread)
  "Set the current thread reference. Used by virtual thread machinery."
  (declare (ignore thread))
  #+sb-fiber
  (when (in-fiber-p)
    (setf (gethash (uiop:symbol-call :sb-thread :current-fiber) *fiber-to-java-threads*) new-thread)
    (return-from |setCurrentThread(Ljava/lang/Thread;)V|))
  (let ((lisp-thread (bordeaux-threads:current-thread)))
    (setf (gethash lisp-thread *lisp-to-java-threads*) new-thread)))

(defmethod |setNativeName(Ljava/lang/String;)V| ((thread |java/lang/Thread|) name)
  "Name the OS thread; JDK 25's setName and InnocuousThread call it.  The
name is recorded in the Java thread already, so there is nothing to do."
  (declare (ignore thread name))
  nil)

(defmethod |getStackTrace0()Ljava/lang/Object;| ((thread |java/lang/Thread|))
  "Return stack trace elements for the thread."
  (declare (ignore thread))
  nil)

;;; JDK 21: Object.wait0(J) — private native wait.
;;; Must contain the actual wait logic (not delegate to wait(J) which calls wait0).
(defun %java-monitor-wait (object timeout)
  "Wait on OBJECT's monitor and always restore ownership before unwinding."
  (let* ((monitor (%get-monitor object))
         (mutex (mutex monitor))
         (identity (current-thread-identity))
         (thread (|java/lang/Thread.currentThread()Ljava/lang/Thread;|))
         (interrupted nil)
         (deadline (when (plusp timeout) (+ (get-internal-real-time)
                                            (* timeout (/ internal-time-units-per-second 1000))))))
    (bt:with-lock-held (mutex)
      (unless (eq (owner monitor) identity) (%throw '|java/lang/IllegalMonitorStateException|))
      (if (gethash thread *thread-interrupted*)
          (setf interrupted t)
          (let ((saved-recursion (recursion-count monitor)))
            (push identity (wait-set monitor))
            (setf (gethash thread *java-thread-waits*) (list monitor identity))
            (setf (owner monitor) nil (recursion-count monitor) 0)
            (%monitor-wake monitor)
            (unwind-protect
                 (handler-case
                     (progn
                       (loop while (and (member identity (wait-set monitor))
                                        (not (gethash thread *thread-interrupted*)))
                           for remaining = (and deadline (/ (- deadline (get-internal-real-time))
                                                            internal-time-units-per-second))
                           do (when (and remaining (<= remaining 0)) (return))
                              (unless (%monitor-sleep monitor remaining) (return)))
                       (when (gethash thread *thread-interrupted*) (setf interrupted t)))
                   (|condition-java/lang/InterruptedException| () (setf interrupted t)))
              (setf (wait-set monitor) (remove identity (wait-set monitor)))
              (remhash thread *java-thread-waits*)
              (loop while (owner monitor)
                    do (handler-case (%monitor-sleep monitor)
                         (|condition-java/lang/InterruptedException| () (setf interrupted t))))
              (setf (owner monitor) identity (recursion-count monitor) saved-recursion)))))
    (when interrupted
      (%clear-java-thread-interrupt thread)
      (%throw '|java/lang/InterruptedException|)))
  nil)

(defmethod |wait0(J)V| ((object |java/lang/Object|) timeout)
  (%java-monitor-wait object timeout))
(defmethod |wait0(J)V| ((object java-array) timeout)
  (%java-monitor-wait object timeout))

(defun %java-array-wait (array timeout &optional (nanos 0))
  "Object.wait's argument validation for arrays, which are Lisp structures."
  (when (or (minusp timeout) (< nanos 0) (> nanos 999999))
    (%throw '|java/lang/IllegalArgumentException|))
  (when (and (plusp nanos) (< timeout #x7fffffffffffffff)) (incf timeout))
  (%java-monitor-wait array timeout))

(defmethod |wait()V| ((array java-array)) (%java-array-wait array 0))
(defmethod |wait(J)V| ((array java-array) timeout) (%java-array-wait array timeout))
(defmethod |wait(JI)V| ((array java-array) timeout nanos) (%java-array-wait array timeout nanos))

;;; JDK 21 VirtualThread — JVMTI notification stubs.
;;; OpenLDK doesn't support JVMTI, so all are no-ops.

(defun |java/lang/VirtualThread.registerNatives()V| ()
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

(defun |jdk/internal/vm/Continuation.registerNatives()V| ()
  nil)

(defun |jdk/internal/vm/Continuation.doYield()I| ()
  "Stub: yield from continuation. Returns 0 (success)."
  0)

(setf (gethash "jdk/internal/vm/Continuation.enterSpecial(Ljdk/internal/vm/Continuation;ZZ)V" *native-overrides*)
      (lambda (cont is-virtual-thread force-yield)
        (declare (ignore cont is-virtual-thread force-yield))
        nil))

(defun |jdk/internal/vm/Continuation.pin()V| ()
  nil)

(defun |jdk/internal/vm/Continuation.unpin()V| ()
  nil)

(setf (gethash "jdk/internal/vm/Continuation.isPinned0(Ljdk/internal/vm/ContinuationScope;)I" *native-overrides*)
      (lambda (scope)
        (declare (ignore scope))
        0))

(defun |jdk/internal/vm/ContinuationSupport.isSupported0()Z| ()
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

(defmethod |isAlive()Z| :around ((thread |java/lang/BaseVirtualThread|))
  "Check if the virtual thread's underlying thread (OS or fiber) is still running."
  #+sb-fiber
  (let ((fiber (gethash thread *java-to-fibers*)))
    (when fiber
      (return-from |isAlive()Z| (if (uiop:symbol-call :sb-thread :fiber-alive-p fiber) 1 0))))
  (let ((lisp-thread (gethash thread *java-threads*)))
    (if (and lisp-thread (bordeaux-threads:thread-alive-p lisp-thread))
        1
        0)))

(defmethod |join(J)V| :around ((thread |java/lang/BaseVirtualThread|) millis)
  "Wait for the virtual thread to finish (OS thread or fiber)."
  #+sb-fiber
  (let ((fiber (gethash thread *java-to-fibers*)))
    (when fiber
      (cond
        ((not (uiop:symbol-call :sb-thread :fiber-alive-p fiber)) (return-from |join(J)V|))
        (t
         (if (zerop millis)
             (uiop:symbol-call :sb-thread :fiber-join fiber)
             (uiop:symbol-call :sb-thread :fiber-join fiber :timeout (/ millis 1000.0d0)))
         (return-from |join(J)V|)))))
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
(defmethod |start0()V| :around ((thread |java/lang/BaseVirtualThread|))
  "Start a virtual thread as a fiber on the fiber scheduler."
  (let ((fiber (uiop:symbol-call :sb-thread :make-fiber
                (lambda ()
                  (let ((*scoped-value-cache* nil))
                    (unwind-protect
                         (progn
                           ;; Register fiber-to-Java mapping
                           (setf (gethash (uiop:symbol-call :sb-thread :current-fiber) *fiber-to-java-threads*) thread)
                           ;; Call the Thread's run() method
                           (handler-case
                               (|run()V| thread)
                             (error (e)
                               (format *error-output* "~&VThread ~A terminated with error: ~A~%" thread e))))
                      ;; Cleanup mappings
                      (remhash (uiop:symbol-call :sb-thread :current-fiber) *fiber-to-java-threads*)
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

(defun |jdk/internal/misc/Unsafe.registerNatives()V| ()
  nil)

(defun |jdk/internal/misc/ScopedMemoryAccess.registerNatives()V| ()
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

(defmethod |availableProcessors()I| ((rt |java/lang/Runtime|))
  (%available-processor-count))

(defmethod |freeMemory()J| ((rt |java/lang/Runtime|))
  ;; Return SBCL's available dynamic space
  (- (sb-ext:dynamic-space-size) (sb-kernel:dynamic-usage)))

(defmethod |totalMemory()J| ((rt |java/lang/Runtime|))
  (sb-kernel:dynamic-usage))

(defmethod |maxMemory()J| ((rt |java/lang/Runtime|))
  (sb-ext:dynamic-space-size))

(defmethod |gc()V| ((rt |java/lang/Runtime|))
  ;; Generation 7 also clears dead core-backed objects. A normal full GC can
  ;; leave warm-image referents alive through otherwise dead core objects.
  (sb-ext:gc :gen 7)
  nil)

;; JDK 9+: Signal native methods — no-op stubs for Terminator.setup()
(defun |jdk/internal/misc/Signal.findSignal0(Ljava/lang/String;)I| (name)
  (declare (ignore name))
  ;; Return a dummy signal number
  0)

(defun |jdk/internal/misc/Signal.handle0(IJ)J| (sig handler)
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

(setf (gethash "jdk/internal/loader/BootLoader.findResources(Ljava/lang/String;)Ljava/util/Enumeration;" *native-overrides*)
      (lambda (name)
        (let ((urls (%make-java-instance "java/util/ArrayList")))
          (|<init>()V| urls)
          (dolist (url-string (get-resource-urls-on-classpath (lstring name)))
            (|add(Ljava/lang/Object;)Z| urls (%make-url-from-string url-string)))
          (|java/util/Collections.enumeration(Ljava/util/Collection;)Ljava/util/Enumeration;| urls))))

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
                (|<init>([B)V| bais
                 (make-java-array
                  :component-class (%get-java-class-by-bin-name "byte")
                  :initial-contents (coerce bytes 'vector)))
                bais))))))

;; JDK 9+: NativeLibraries — return nil (no built-in libraries)
(defun |jdk/internal/loader/NativeLibraries.findBuiltinLib(Ljava/lang/String;)Ljava/lang/String;| (name)
  (declare (ignore name))
  nil)

;; NativeLibraries.load: OpenLDK's natives are Lisp functions, so there is no
;; native library to load.  Report it as not loaded, which the JDK treats as a
;; library that is absent: BootLoader.loadLibrary ignores the result.  This
;; matters on macOS, where ClassLoaderHelper.loadLibraryOnlyIfPresent() is
;; false (dyld's shared cache means a library need not exist as a file), so
;; the JDK calls load without first checking for the file.  Without this,
;; anything reaching BootLoader.loadLibrary("nio"), such as the first use of
;; java.nio.file or MessageDigest.getInstance, signalled UNDEFINED-FUNCTION.
(defun |jdk/internal/loader/NativeLibraries.load(Ljdk/internal/loader/NativeLibraries$NativeLibraryImpl;Ljava/lang/String;ZZ)Z|
    (impl name is-builtin throw-exception-if-fail)
  (declare (ignore impl name is-builtin throw-exception-if-fail))
  0)

