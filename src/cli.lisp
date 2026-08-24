;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: OPENLDK; Base: 10 -*-
;;;
;;; Copyright (C) 2024, 2025, 2026  Anthony Green <green@moxielogic.com>
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

(in-package :openldk)

;;; Command-line entry points and image building: the java-style argument
;;; parser, the toplevel wrappers installed in dumped images, and the
;;; MAKE-IMAGE/DUMP-APP-IMAGE builders.  The run driver itself (MAIN,
;;; INITIALIZE) lives in openldk.lisp.

(defun %print-usage ()
  "Print Java-style usage message."
  (format *error-output* "~%Usage: openldk [options] <mainclass> [args...]~%")
  (format *error-output* "       openldk [options] -jar <jarfile> [args...]~%~%")
  (format *error-output* "where options include:~%")
  (format *error-output* "    -cp <class search path>~%")
  (format *error-output* "    -classpath <class search path>~%")
  (format *error-output* "                  A : separated list of directories, JAR archives,~%")
  (format *error-output* "                  and ZIP archives to search for class files.~%")
  (format *error-output* "    -D<name>=<value>~%")
  (format *error-output* "                  set a system property~%")
  (format *error-output* "    -verbose:[class|gc|jni]~%")
  (format *error-output* "                  enable verbose output~%")
  (format *error-output* "    -version      print product version and exit~%")
  (format *error-output* "    -? -help      print this help message~%")
  (format *error-output* "    --dump-dir <dir>~%")
  (format *error-output* "                  Directory for internal debug info~%")
  (format *error-output* "    --aot <dir>   Ahead-of-time compilation directory~%~%"))

(defun %parse-java-args ()
  "Parse Java-style command line arguments.
   Returns (values mainclass args classpath dump-dir aot).
   Sets *cli-jvm-properties* as a side effect."
  (let ((raw-args (rest sb-ext:*posix-argv*)) ; skip program name
        (classpath nil)
        (dump-dir nil)
        (aot nil)
        (mainclass nil)
        (program-args nil)
        (properties nil)
        (i 0))
    ;; Parse options until we hit mainclass
    (loop while (< i (length raw-args))
          for arg = (nth i raw-args)
          do (cond
               ;; -classpath <path> or -cp <path>
               ((or (string= arg "-classpath") (string= arg "-cp"))
                (incf i)
                (when (< i (length raw-args))
                  (setf classpath (nth i raw-args)))
                (incf i))
               ;; -Dkey=value sets a system property
               ((str:starts-with? "-D" arg)
                (let* ((prop-str (subseq arg 2))
                       (eq-pos (position #\= prop-str)))
                  (if eq-pos
                      (push (cons (subseq prop-str 0 eq-pos)
                                  (subseq prop-str (1+ eq-pos)))
                            properties)
                      ;; -Dkey with no value sets empty string
                      (push (cons prop-str "") properties)))
                (incf i))
               ;; -XX options are consumed and ignored
               ((str:starts-with? "-XX" arg)
                (incf i))
               ;; -X options are consumed and ignored
               ((str:starts-with? "-X" arg)
                (incf i))
               ;; -verbose options
               ((str:starts-with? "-verbose" arg)
                (when (str:contains? "class" arg)
                  (setf *debug-load* t))
                (incf i))
               ;; -version
               ((string= arg "-version")
                (flet ((vm-prop (key)
                         (cdr (assoc key +java-identity-properties+ :test #'string=))))
                  (format t "openldk version ~S~%" (vm-prop "java.version"))
                  (format t "~A (build ~A)~%"
                          (vm-prop "java.runtime.name")
                          (vm-prop "java.runtime.version")))
                (uiop:quit 0))
               ;; -help, -?, -h
               ((or (string= arg "-help") (string= arg "-?") (string= arg "-h")
                    (string= arg "--help"))
                (%print-usage)
                (uiop:quit 0))
               ;; OpenLDK-specific: --dump-dir <dir>
               ((string= arg "--dump-dir")
                (incf i)
                (when (< i (length raw-args))
                  (setf dump-dir (nth i raw-args)))
                (incf i))
               ;; OpenLDK-specific: --aot <dir>
               ((string= arg "--aot")
                (incf i)
                (when (< i (length raw-args))
                  (setf aot (nth i raw-args)))
                (incf i))
               ;; -jar <jarfile>
               ((string= arg "-jar")
                (incf i)
                (when (< i (length raw-args))
                  ;; For -jar, the jarfile IS the mainclass (will be handled specially)
                  (setf mainclass (nth i raw-args)))
                (incf i)
                ;; Everything after -jar <jarfile> is program args
                (setf program-args (subseq raw-args i))
                (return))
               ;; Unknown option starting with -
               ((and (> (length arg) 0) (char= (char arg 0) #\-))
                (format *error-output* "Unrecognized option: ~A~%" arg)
                (%print-usage)
                (uiop:quit 1))
               ;; First non-option is the mainclass
               (t
                (setf mainclass arg)
                (incf i)
                ;; Everything after mainclass is program args
                (setf program-args (subseq raw-args i))
                (return))))
    (setf *cli-jvm-properties* (nreverse properties))
    (values mainclass program-args classpath dump-dir aot)))

(defun main-wrapper ()
  "Main entry point into OpenLDK. Process command line errors here."
  ;; Disable floating-point traps to match Java semantics (NaN/Infinity instead of errors)
  (sb-int:set-floating-point-modes :traps nil)
  ;; Parse Java-style command line arguments
  (multiple-value-bind (mainclass args classpath dump-dir aot)
      (%parse-java-args)
    (unless mainclass
      (%print-usage)
      (uiop:quit 1))
    (handler-bind
        ((error (lambda (condition)
                  (cond
                    ((typep condition '|condition-java/lang/Throwable|)
                     (let ((throwable (and (slot-boundp condition '|objref|)
                                           (slot-value condition '|objref|))))
                       (if (typep throwable '|java/lang/Throwable|)
                           (progn
                             (format *error-output* "~&Unhandled Java exception:~%")
                             (%print-java-stack-trace throwable :stream *error-output*)
                             ;; Print Go-specific fields
                             (when (and (slot-exists-p throwable '|tagbody|)
                                        (slot-boundp throwable '|tagbody|))
                               (format *error-output* "~&Go.tagbody = ~A~%" (slot-value throwable '|tagbody|))
                               (format *error-output* "~&Go.tag = ~A~%" (slot-value throwable '|tag|)))
                             (format *error-output* "~&~%Lisp backtrace at throw site:~%")
                             (trivial-backtrace:print-backtrace condition :output *error-output*)
                             (finish-output *error-output*))
                           (format *error-output* "~&Unhandled Java condition: ~A~%" condition))))
                    (t
                     (format *error-output* "~&Error: ~A~%" condition)))
                  (uiop:quit 1))))
      (main mainclass args :classpath classpath :dump-dir dump-dir :aot aot))
    ;; Force exit — daemon threads may be blocked in wait() and won't terminate
    (sb-ext:exit :code 0 :abort t)))

(defun app-main-wrapper ()
  "Generic entry point for pre-dumped app images.
   Uses *default-mainclass* and *default-classpath* baked at build time.
   CLI -cp overrides the baked classpath; a CLI mainclass overrides the default."
  (sb-int:set-floating-point-modes :traps nil)
  (multiple-value-bind (cli-mainclass args cli-classpath dump-dir aot)
      (%parse-java-args)
    (let ((mainclass (or cli-mainclass *default-mainclass*))
          (classpath (or cli-classpath *default-classpath*)))
      (unless mainclass
        (%print-usage)
        (uiop:quit 1))
      (handler-case
          (main mainclass args :classpath classpath :dump-dir dump-dir :aot aot)
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
          (uiop:quit 1))))))

(defun make-image (&optional (output-path "openldk"))
  (initialize)
  ;; Clear all monitor state to prevent deadlocks in the saved image.
  (clrhash *monitors*)
  ;; Clear stale thread mappings — after image restore the Lisp thread objects are different.
  (clrhash *lisp-to-java-threads*)
  (setf *current-thread* nil)
  ;; Kill all Java threads before saving core (SBCL can't save with threads running)
  (loop for thread in (bt:all-threads)
        when (and (not (eq thread (bt:current-thread)))
                  (search "Java-Thread" (bt:thread-name thread)))
        do (bt:destroy-thread thread))
  (sb-ext:save-lisp-and-die output-path
                            :executable t
                            :save-runtime-options t
                            :toplevel #'main-wrapper))

(defun dump-app-image (output-path default-mainclass &key classpath)
  "Build a generic executable image for a Java application.
   OUTPUT-PATH:       Path for the saved executable.
   DEFAULT-MAINCLASS: The Java class to run when no class is given on the CLI.
   CLASSPATH:         Classpath string to bake in. If nil, reads CLASSPATH env var."
  (initialize)
  (let ((cp (or classpath (uiop:getenv "CLASSPATH") ".")))
    (setf *default-mainclass* default-mainclass)
    (setf *default-classpath* cp)
    (setf *classpath*
          (append
           (loop for cpe in (split-sequence:split-sequence (uiop:inter-directory-separator) cp)
                 collect (if (str:ends-with? ".jar" cpe)
                             (make-instance 'jar-classpath-entry :jarfile cpe)
                             (make-instance 'dir-classpath-entry :dir cpe)))
           (discover-jmod-classpath-entries))))
  ;; Clear all monitor state to prevent deadlocks in the saved image.
  (clrhash *monitors*)
  ;; Clear stale thread mappings — after image restore the Lisp thread objects are different.
  (clrhash *lisp-to-java-threads*)
  (setf *current-thread* nil)
  ;; Kill all Java threads before saving core (SBCL can't save with threads running)
  (loop for thread in (bt:all-threads)
        when (and (not (eq thread (bt:current-thread)))
                  (search "Java-Thread" (bt:thread-name thread)))
        do (bt:destroy-thread thread))
  (sb-ext:save-lisp-and-die output-path
                            :executable t
                            :save-runtime-options t
                            :toplevel #'app-main-wrapper))
