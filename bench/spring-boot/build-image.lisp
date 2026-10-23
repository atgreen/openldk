;;; Build an OpenLDK warm image of the Spring Boot benchmark.  run.sh loads
;;; this once per image, naming the image and its main class in BOOT_IMAGE
;;; and BOOT_MAIN and the application's classpath in CLASSPATH.
;;;
;;; Both images keep the state their warmups leave (:reset-classes nil).
;;; demo.boot.BootApp starts Boot from scratch on each run anyway;
;;; demo.boot.Snapshot reuses the context its first warmup started.

(in-package :openldk)

(dump-warm-image (uiop:getenv "BOOT_IMAGE") (uiop:getenv "BOOT_MAIN")
                 :warmups (list nil nil)
                 :reset-classes nil
                 :timeout 1800)
