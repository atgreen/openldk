;;;; SPDX-License-Identifier: GPL-3.0-or-later WITH Classpath-exception-2.0
;;;; Run in a disposable SBCL process with a 512 MiB dynamic space.
(require :asdf)
(asdf:load-system :openldk)
(openldk::initialize)

(defvar *retained-arrays* nil)
(defvar *caught-error* nil)
(defvar *host-request-size* (floor (sb-ext:dynamic-space-size) (* 2 sb-vm:n-word-bytes)))

(defun crowd-java-heap ()
  ;; Retain arrays up to the production boundary, including its GC reserve.
  ;; Exhausting that reserve too can make copying GC itself unrecoverable.
  (handler-case
      (loop (push (openldk::make-java-array :component-class "Ljava/lang/Object;"
                                           :size (ash 1 17))
                  *retained-arrays*))
    (openldk::|condition-java/lang/OutOfMemoryError| (condition)
      (assert (eq condition openldk::*array-allocation-error*)))))

(defun exhaust-host-allocator (size)
  ;; Bypass the preflight for this request only. SBCL's real allocation
  ;; failure must travel through the production recovery macro.
  (setf *caught-error* nil)
  (handler-case
      (openldk::with-array-allocation-recovery
        (make-array size :initial-element nil))
    (openldk::|condition-java/lang/OutOfMemoryError| (condition)
      (setf *caught-error* condition)))
  (assert (eq *caught-error* openldk::*array-allocation-error*)))

;; Establish that the later request is legal and fits without retained arrays.
(defun check-host-request-fits ()
  (assert (= *host-request-size* (length (make-array *host-request-size*
                                                    :initial-element nil)))))
(check-host-request-fits)
(openldk::%collect-java-heap)

;; Also exercise a request that cannot fit even an empty dynamic space.
(exhaust-host-allocator (floor (sb-ext:dynamic-space-size) sb-vm:n-word-bytes))

(dotimes (cycle 3)
  (crowd-java-heap)
  (exhaust-host-allocator *host-request-size*)
  (setf *retained-arrays* nil)
  (openldk::%collect-java-heap)
  (check-host-request-fits)
  (dotimes (i 100)
    (let ((array (openldk::make-java-array :component-class "I" :size 1024)))
      (assert (= 1024 (length (openldk::java-array-data array)))))))
(format t "Native exhaustion: Java OutOfMemoryError caught, GC and allocations recovered~%")
