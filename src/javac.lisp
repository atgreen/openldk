;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: OPENLDK; Base: 10 -*-
;;; SPDX-License-Identifier: GPL-3.0-or-later WITH Classpath-exception-2.0

(defpackage #:javacl
  (:use #:cl #:openldk)
  (:export :main
           :make-javac-image))

(in-package :javacl)

(defun main ()
  "Entry point for the pre-dumped javac image. Runs com.sun.tools.javac.Main."
  ;; Match Java FP semantics
  (sb-int:set-floating-point-modes :traps nil)
  (let ((args (uiop:command-line-arguments)))
    (handler-case
        (main "com.sun.tools.javac.Main"
              args
              :classpath (or (uiop:getenv "CLASSPATH") "."))
      (error (condition)
        (cond
          ((typep condition 'openldk::|condition-java/lang/Throwable|)
           (let ((throwable (and (slot-boundp condition 'openldk::|objref|)
                                 (slot-value condition 'openldk::|objref|))))
             (if (typep throwable 'openldk::|java/lang/Throwable|)
                 (progn
                   (format *error-output* "~&Unhandled Java exception:~%")
                   (openldk::%print-java-stack-trace throwable :stream *error-output*)
                   (finish-output *error-output*))
                 (format *error-output* "~&Unhandled Java condition: ~A~%" condition))))
          (t
           (format *error-output* "~&Error: ~A~%" condition)))
        (uiop:quit 1)))))

(defparameter *javac-warmup-classes*
  '("com/sun/tools/javac/Main"
    "com/sun/tools/javac/main/Main"
    "com/sun/tools/javac/main/Option"
    "com/sun/tools/javac/main/JavaCompiler"
    "com/sun/tools/javac/file/JavacFileManager"
    "com/sun/tools/javac/util/Context"
    "com/sun/tools/javac/util/Log"
    "com/sun/tools/javac/util/Options"
    "com/sun/tools/javac/util/List"
    "com/sun/tools/javac/code/Lint"
    "com/sun/tools/javac/resources/compiler")
  "Subset of javac classes to preload into the javacl image.")

(defun %warmup-javac ()
  (let ((openldk::*debug-load* t))
    (dolist (c *javac-warmup-classes*)
      (ignore-errors (openldk::classload c)))))

(defun make-javacl-image (&optional (output-path "javacl"))
  "Build an executable image that jumps straight into javac."
  (openldk::initialize)
  ;; Warm up javac by loading core classes (no compilation run).
  (%warmup-javac)
  ;; Kill helper threads before dumping the image.
  (loop for thread in (bt:all-threads)
        when (and (not (eq thread (bt:current-thread)))
                  (search "Java-Thread" (bt:thread-name thread)))
        do (bt:destroy-thread thread))
  (sb-ext:save-lisp-and-die output-path
                            :executable t
                            :save-runtime-options t
                            :toplevel #'main))
