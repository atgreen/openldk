;;; Build an OpenLDK warm image for a startup benchmark.  bench/common.sh
;;; loads this once per image, naming the image and its main class in
;;; BENCH_IMAGE and BENCH_MAIN and the application's classpath in CLASSPATH.
;;;
;;; Images keep the state their warmups leave unless BENCH_RESET is set: a
;;; benchmark's Snapshot main reuses what its first warmup started, while an
;;; application whose static state outlives a run (Quarkus) needs its classes
;;; reset to start again from scratch.

(in-package :openldk)

(dump-warm-image (uiop:getenv "BENCH_IMAGE") (uiop:getenv "BENCH_MAIN")
                 :warmups (list nil nil)
                 :reset-classes (plusp (length (uiop:getenv "BENCH_RESET")))
                 :timeout 1800)
