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

(defun %static-storage (bin-name)
  "Return the instance holding the static fields of class BIN-NAME, or NIL
if the class has not been loaded."
  (handler-case
      (let* ((pkg (class-package bin-name))
             (sym (and pkg (find-symbol (format nil "+static-~A+" bin-name) pkg))))
        (when (and sym (boundp sym))
          (symbol-value sym)))
    (condition () nil)))

(defvar *file-streams-to-reopen* nil
  "Files open in Java objects when the image was saved, as
(object path direction position), reopened when the image starts.")

(defun %reopen-saved-file-streams ()
  "SB-EXT:*INIT-HOOKS* function: reopen the files Java objects held open
when the image was saved, at the position they had, and store the new
streams where the old ones were."
  (dolist (entry *file-streams-to-reopen*)
    (destructuring-bind (object path direction position) entry
      (let ((stream (ignore-errors
                     (open path :element-type '(unsigned-byte 8)
                                :direction direction
                                :if-exists (if (eq direction :input) nil :append)))))
        (when stream
          (when position
            (ignore-errors (file-position stream position)))
          (setf (slot-value object '|fd|) stream)))))
  (setf *file-streams-to-reopen* nil))

(defun %record-file-streams-to-reopen ()
  "Find the Java objects (FileDescriptors, file streams, RandomAccessFiles)
holding open Lisp file streams in their fd slot, and arrange for those files
to reopen when the saved image starts: the streams themselves are closed by
SAVE-LISP-AND-DIE."
  (let ((entries '()))
    (sb-vm:map-allocated-objects
     (lambda (obj type size)
       (declare (ignore type size))
       (when (and (typep obj '|java/lang/Object|)
                  (slot-exists-p obj '|fd|)
                  (slot-boundp obj '|fd|))
         (let ((stream (slot-value obj '|fd|)))
           (when (and (typep stream 'sb-sys:fd-stream)
                      (open-stream-p stream))
             (let ((path (ignore-errors (namestring (pathname stream)))))
               (when path
                 (push (list obj path
                             (cond ((and (input-stream-p stream) (output-stream-p stream)) :io)
                                   ((output-stream-p stream) :output)
                                   (t :input))
                             (ignore-errors (file-position stream)))
                       entries)))))))
     :all)
    (setf *file-streams-to-reopen* entries)
    (pushnew '%reopen-saved-file-streams sb-ext:*init-hooks*)
    (length entries)))

(defun %reset-runtime-for-save ()
  "Put the runtime in a state that survives SAVE-LISP-AND-DIE and a restart:
kill other threads, drop process-local handles, and give every lock a fresh
mutex.  Call this last, right before saving, after any build-time runs of
Java code (warmups)."
  ;; Clear unsafe memory table — foreign heap pointers from warmup
  ;; won't survive image save/load.
  (clrhash *unsafe-memory-table*)
  ;; Force ProcessEnvironment to re-initialize at runtime so it reads
  ;; the actual runtime environment, not stale warmup state.
  (let ((pe (%get-ldk-class-by-bin-name "java/lang/ProcessEnvironment" t)))
    (when pe (setf (initialized-p pe) nil)))
  ;; Likewise the NIO default filesystem: its clinit captures user.dir
  ;; (the BUILD directory) into UnixFileSystem.defaultDirectory, sending
  ;; relative-path I/O to the wrong place after restart.
  (dolist (cname '("sun/nio/fs/DefaultFileSystemProvider"
                   "java/nio/file/FileSystems$DefaultFileSystemHolder"))
    (let ((c (%get-ldk-class-by-bin-name cname t)))
      (when c (setf (initialized-p c) nil))))
  ;; Kill ALL helper threads before dumping the image — warmup can spawn
  ;; NIO/cleaner threads with arbitrary names, and save-lisp-and-die
  ;; refuses to run with any other thread alive.
  (loop for thread in (bt:all-threads)
        unless (eq thread (bt:current-thread))
          do (ignore-errors (bt:destroy-thread thread)))
  (loop repeat 50
        while (rest (bt:all-threads))
        do (sleep 0.1))
  ;; Reset ALL lock state.  A lock or monitor saved mid-acquisition (a
  ;; warmup thread killed while holding it, or a warmup that died from
  ;; stack exhaustion inside a synchronized region) blocks the restarted
  ;; image forever on its first acquisition.
  (clrhash *class-load-locks*)
  (setf *class-load-locks-lock* (bt:make-lock "class-load-locks"))
  (clrhash *monitors*)
  (setf *method-compilation-lock* (bt:make-lock "method-compilation-lock"))
  (setf *method-compilation-cv* (bt:make-condition-variable :name "method-compilation-cv"))
  (setf *identity-hash-counter-lock* (bt:make-lock "identity-hash-lock"))
  (setf *cas-lock* (bt:make-recursive-lock "unsafe-cas"))
  ;; Let the restarted image stamp its own start time.
  (setf *vm-start-time-millis* nil)
  ;; Drop stale in-progress compilation claims from dead warmup threads.
  (maphash (lambda (k v)
             (unless (eq v :done)
               (remhash k *methods-being-compiled*)))
           *methods-being-compiled*)
  ;; Classpath entries carry their own locks and open container handles.
  (mapc #'%reset-classpath-entry-runtime-state *classpath*)
  ;; So do java.util.zip.ZipFile objects the program opened itself, and the
  ;; JarFiles JarURLConnection hands out.
  (%zipfile-reset-for-save)
  (clrhash *jarfile-zips*)
  (clrhash *zipentry-times*)
  ;; Files a Java program opened itself (FileInputStream, FileOutputStream,
  ;; RandomAccessFile) reopen when the image starts.
  (%record-file-streams-to-reopen)
  ;; jrt: filesystem state (providers, mmapped jimage buffers) is
  ;; process-local; drop it so the restarted image rebuilds it.
  (setf *installed-file-system-providers* nil)
  (clrhash *jimage-native-maps*)
  ;; Java-side static caches also hold the build-time ImageReader whose
  ;; mmapped buffer dies with this process; using them after restart
  ;; segfaults.  Clear them so the runtime rebuilds the jrt filesystem.
  (let ((sir (%static-storage "jdk/internal/jimage/ImageReader$SharedImageReader")))
    (when (and sir (slot-exists-p sir '|OPEN_FILES|))
      (let ((map (slot-value sir '|OPEN_FILES|)))
        (when map (ignore-errors (|clear()| map))))))
  ;; ResourceBundle negative-caches failed lookups; a NONEXISTENT
  ;; marker cached during warmup would be baked into the image and
  ;; poison the same lookup forever at runtime.
  (let ((rb (%static-storage "java/util/ResourceBundle")))
    (when (and rb (slot-exists-p rb '|cacheList|))
      (let ((m (slot-value rb '|cacheList|)))
        (when m (ignore-errors (|clear()| m))))))
  ;; Synchronized hash tables carry an internal mutex; one saved in the
  ;; locked state (a warmup thread killed mid-access) deadlocks the
  ;; restarted image on its first table access.  Sweep the heap and give
  ;; every synchronized table a fresh lock.
  (let ((tables '())
        (gfs '()))
    (sb-vm:map-allocated-objects
     (lambda (obj type size)
       (declare (ignore type size))
       (cond
         ((and (hash-table-p obj)
               (sb-ext:hash-table-synchronized-p obj))
          (push obj tables))
         ((typep obj 'java-generic-function)
          (push obj gfs))))
     :all)
    (dolist (ht tables)
      (setf (sb-impl::hash-table-%lock ht)
            (sb-thread:make-mutex :name "hash-table lock")))
    ;; java-generic-function dispatch-cache locks are ordinary mutex
    ;; slots with the same saved-locked hazard.
    (dolist (gf gfs)
      (setf (slot-value gf 'cache-lock)
            (bt:make-lock "java-gf-cache")))))

(defun %reset-app-classes ()
  "Return every application class to its uninitialized state: clear its
initialized flag and put its static fields back to their initial values,
so its <clinit> runs again on first use.  Compiled methods are kept.
Returns the number of classes reset."
  (let ((count 0))
    (when *app-ldk-class-loader*
      (maphash (lambda (bin-name class)
                 (declare (ignore bin-name))
                 (setf (initialized-p class) nil
                       (initializing-p class) nil)
                 (let ((clos-class (find-class (intern (name class) (ldk-class-package class)) nil)))
                   (when clos-class
                     (dolist (slot (sb-mop:class-direct-slots clos-class))
                       (when (eq (sb-mop:slot-definition-allocation slot) :class)
                         (let ((init (sb-mop:slot-definition-initfunction slot))
                               (proto (sb-mop:class-prototype clos-class)))
                           (setf (slot-value proto (sb-mop:slot-definition-name slot))
                                 (and init (funcall init))))))))
                 (incf count))
               (slot-value *app-ldk-class-loader* 'ldk-classes-by-bin-name)))
    count))

(defun %run-warmup (mainclass args classpath &key input (timeout 600))
  "Run MAINCLASS with ARGS once at build time, with INPUT (a string, or
NIL for no input) as its standard input.  Errors, timeouts and System.exit
are reported and survived, so one bad warmup doesn't stop the build."
  (let ((input-file (when input
                      (uiop:with-temporary-file (:stream s :pathname p :keep t)
                        (write-string input s)
                        p)))
        (*ignore-quit* t))
    (unwind-protect
         (handler-case
             (sb-ext:with-timeout timeout
               (if input-file
                   (with-open-file (*standard-input* input-file :element-type :default)
                     (main mainclass args :classpath classpath))
                   (main mainclass args :classpath classpath)))
           (sb-ext:timeout ()
             (format *error-output* "~&;; WARMUP ~S timed out after ~Ds~%" args timeout))
           (condition (c)
             (format *error-output* "~&;; WARMUP ~S caught: ~A~%" args c)))
      (when input-file
        (delete-file input-file)))))

(defun dump-warm-image (output-path default-mainclass
                        &key classpath warmups (timeout 600) (reset-classes t))
  "Build an executable image for a Java application whose code is already
compiled: run the application on sample inputs, keep the compiled methods,
reset the application's own classes, and save.
   OUTPUT-PATH:       Path for the saved executable.
   DEFAULT-MAINCLASS: The Java class to run when no class is given on the CLI.
   CLASSPATH:         Classpath string to bake in. If nil, reads CLASSPATH env var.
   WARMUPS:           List of runs, each a plist (:args LIST :input STRING);
                      a run with neither runs main once with no arguments.
   TIMEOUT:           Seconds allowed for each warmup run.
   RESET-CLASSES:     When true (the default), reset the application's
                      classes after warming up, so their <clinit>s run again
                      and main starts from scratch in the saved image.  When
                      NIL, the image keeps the state the warmups left, and
                      main must be written to reuse it.
JDK classes always keep their state."
  (initialize)
  (let ((cp (or classpath (uiop:getenv "CLASSPATH") ".")))
    (setf *default-mainclass* default-mainclass
          *default-classpath* cp)
    (dolist (warmup (or warmups (list nil)))
      (destructuring-bind (&key args input) warmup
        (format *error-output* "~&;; WARMUP ~A ~{~A~^ ~}~%" default-mainclass args)
        (%run-warmup default-mainclass args cp :input input :timeout timeout))))
  (when reset-classes
    (format *error-output* "~&;; Reset ~D application classes~%" (%reset-app-classes)))
  (%reset-runtime-for-save)
  (sb-ext:save-lisp-and-die output-path
                            :executable t
                            :save-runtime-options t
                            :toplevel #'app-main-wrapper))

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
