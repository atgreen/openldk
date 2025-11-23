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
  (let* ((args (uiop:command-line-arguments))
         (cp (default-javac-classpath)))
    (format t "; javacl argv (~D): ~S~%" (length args) args)
    (format t "; javacl default classpath: ~A~%" cp)
    (finish-output)
    (handler-case
        (openldk::main "com.sun.tools.javac.Main"
                       args
                       :classpath cp)
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

(defun default-javac-classpath ()
  "Pick a sensible default classpath for javac: tools.jar if present, else env or \".\""
  (or (uiop:getenv "CLASSPATH")
      (let* ((jh (uiop:getenv "JAVA_HOME"))
             (tools (and jh
                         (merge-pathnames #P"lib/tools.jar"
                                          (uiop:ensure-directory-pathname jh)))))
        (when (and tools (uiop:file-exists-p tools))
          (namestring tools)))
      "."))

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

;; Javac expects a static initializer; provide a no-op to avoid undefined function during warmup.
(defun |%clinit-com/sun/tools/javac/file/JavacFileManager| () nil)

(defun make-javacl-image (&optional (output-path "javacl"))
  "Build an executable image that jumps straight into javac."
  (let ((cp (default-javac-classpath)))
    (openldk::initialize)
    ;; Set classpath inside the image so warmup uses it.
    (when cp
      (setf openldk::*classpath*
            (loop for cpe in (split-sequence:split-sequence (uiop:inter-directory-separator) cp)
                  collect (if (str:ends-with? ".jar" cpe)
                              (make-instance 'openldk::jar-classpath-entry :jarfile cpe)
                              (make-instance 'openldk::dir-classpath-entry :dir cpe))))))
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
