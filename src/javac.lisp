;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: OPENLDK; Base: 10 -*-
;;; SPDX-License-Identifier: GPL-3.0-or-later WITH Classpath-exception-2.0

(in-package :openldk)

(defun javac-main-wrapper ()
  "Entry point for the pre-dumped javac image. Runs com.sun.tools.javac.Main."
  ;; Match Java FP semantics
  (sb-int:set-floating-point-modes :traps nil)
  (handler-case
      (main "com.sun.tools.javac.Main"
            (rest sb-ext:*posix-argv*)
            :classpath (or (uiop:getenv "CLASSPATH") "."))
    (error (condition)
      (cond
        ((typep condition '|condition-java/lang/Throwable|)
         (let ((throwable (and (slot-boundp condition '|objref|)
                               (slot-value condition '|objref|))))
           (if (typep throwable '|java/lang/Throwable|)
               (progn
                 (format *error-output* "~&Unhandled Java exception:~%")
                 (%print-java-stack-trace throwable :stream *error-output*)
                 (finish-output *error-output*))
               (format *error-output* "~&Unhandled Java condition: ~A~%" condition))))
        (t
         (format *error-output* "~&Error: ~A~%" condition)))
      (uiop:quit 1))))

(defun make-javac-image (&optional (output-path "javacl"))
  "Build an executable image that jumps straight into javac."
  (initialize)
  ;; Warm up javac so key classes/methods are already present in the dumped image.
  (ignore-errors
    (main "com.sun.tools.javac.Main"
          '("-help")
          :classpath (or (uiop:getenv "CLASSPATH") ".")))
  ;; Kill helper threads before dumping the image.
  (loop for thread in (bt:all-threads)
        when (and (not (eq thread (bt:current-thread)))
                  (search "Java-Thread" (bt:thread-name thread)))
        do (bt:destroy-thread thread))
  (sb-ext:save-lisp-and-die output-path
                            :executable t
                            :save-runtime-options t
                            :toplevel #'javac-main-wrapper))
