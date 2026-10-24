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
               ;; -jar <jarfile>: the manifest names the main class, and
               ;; the classpath is the jar and its Class-Path, as on a JVM.
               ((string= arg "-jar")
                (incf i)
                (when (< i (length raw-args))
                  (multiple-value-setq (mainclass classpath)
                    (%jar-launch (nth i raw-args))))
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

(defun %manifest-attributes (jarfile)
  "The main attributes of JARFILE's manifest, an alist of (name . value),
with continuation lines joined."
  (let* ((zip (zip:open-zipfile jarfile))
         (entry (zip:get-zipfile-entry "META-INF/MANIFEST.MF" zip))
         (text (and entry (flexi-streams:octets-to-string (zip:zipfile-entry-contents entry)
                                                          :external-format :utf-8)))
         (lines nil))
    (zip:close-zipfile zip)
    (dolist (line (split-sequence:split-sequence #\Newline (remove #\Return (or text ""))))
      (cond
        ;; A blank line ends the main section.
        ((zerop (length line)) (when lines (return)))
        ((and (char= (char line 0) #\Space) lines)
         (setf (car lines) (concatenate 'string (car lines) (subseq line 1))))
        (t (push line lines))))
    (loop for line in (nreverse lines)
          for colon = (search ": " line)
          when colon
            collect (cons (subseq line 0 colon) (subseq line (+ colon 2))))))

(defun %jar-launch (jarfile)
  "The main class and classpath `java -jar JARFILE` runs with: the
manifest's Main-Class, and JARFILE followed by its Class-Path entries,
which are relative to JARFILE's directory."
  (unless (uiop:file-exists-p jarfile)
    (format *error-output* "Error: Unable to access jarfile ~A~%" jarfile)
    (uiop:quit 1))
  (let* ((attributes (%manifest-attributes jarfile))
         (main-class (cdr (assoc "Main-Class" attributes :test #'string-equal)))
         (class-path (cdr (assoc "Class-Path" attributes :test #'string-equal)))
         (directory (uiop:pathname-directory-pathname (merge-pathnames jarfile))))
    (unless main-class
      (format *error-output* "no main manifest attribute, in ~A~%" jarfile)
      (uiop:quit 1))
    (values (string-trim " " main-class)
            (format nil (concatenate 'string "~A~{" (string (uiop:inter-directory-separator)) "~A~}")
                    jarfile
                    (loop for entry in (split-sequence:split-sequence #\Space (or class-path ""))
                          when (plusp (length entry))
                            collect (namestring (merge-pathnames entry directory)))))))

(defun %use-process-directory ()
  "Resolve relative Lisp pathnames against the process's working directory.
A saved image keeps *DEFAULT-PATHNAME-DEFAULTS* from the build, so natives
that resolve a relative Java path (a classpath entry like target/classes,
say) would look in the directory the image was built in.  java.io.File's
file system, made at build time, keeps its user.dir too: relative Java
files resolve against it."
  (setf *default-pathname-defaults* (uiop:getcwd))
  (let* ((file (%static-storage "java/io/File"))
         (fs (and file (slot-exists-p file '|FS|) (slot-value file '|FS|))))
    (when (and fs (slot-exists-p fs '|userDir|))
      (setf (slot-value fs '|userDir|) (jstring (%current-directory-string))))))

(defun main-wrapper ()
  "Main entry point into OpenLDK. Process command line errors here."
  (%use-process-directory)
  ;; Disable floating-point traps to match Java semantics (NaN/Infinity instead of errors)
  (sb-int:set-floating-point-modes :traps nil)
  (%fasl-cache-start)
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
                     (format *error-output* "~&Error: ~A~%" condition)
                     ;; LDK_DEBUG=e: where the internal error happened.
                     (when *debug-exceptions*
                       (sb-debug:print-backtrace :count 60 :stream *error-output*))))
                  (uiop:quit 1))))
      (main mainclass args :classpath classpath :dump-dir dump-dir :aot aot))
    ;; Force exit — daemon threads may be blocked in wait() and won't terminate.
    ;; An aborting exit runs no exit hooks, so report the cache's counts here.
    (%fasl-cache-report)
    (sb-ext:exit :code 0 :abort t)))

(defun app-main-wrapper ()
  "Generic entry point for pre-dumped app images.
   Uses *default-mainclass* and *default-classpath* baked at build time.
   CLI -cp overrides the baked classpath; a CLI mainclass overrides the default."
  (%use-process-directory)
  (sb-int:set-floating-point-modes :traps nil)
  (%fasl-cache-start)
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

(defun %reinitialize-static-properties ()
  "Make StaticProperty read user.dir, user.home and the like again at
runtime: at build time it captured the build's (java.io.File resolves
relative paths against its user.dir)."
  (let ((class (gethash "jdk/internal/util/StaticProperty" *ldk-classes-by-bin-name*)))
    (when class (setf (initialized-p class) nil))))

(defvar *saved-native-memory* '()
  "(address . contents) of each block of Unsafe memory alive when the
image was saved, for %RESTORE-NATIVE-MEMORY.")

(defun %save-native-memory ()
  (setf *saved-native-memory*
        (loop for address being the hash-keys of *unsafe-memory-table* using (hash-value entry)
              collect (let* ((size (cdr entry))
                             (contents (make-array size :element-type '(unsigned-byte 8)))
                             (sap (sb-sys:int-sap address)))
                        (dotimes (i size) (setf (aref contents i) (sb-sys:sap-ref-8 sap i)))
                        (cons address contents))))
  (clrhash *unsafe-memory-table*)
  (pushnew '%restore-native-memory sb-ext:*init-hooks*))

(defconstant +map-fixed-noreplace+ #x100000
  "Linux's MAP_FIXED_NOREPLACE: map at exactly this address, or fail.")

(defun %restore-native-memory ()
  "SB-EXT:*INIT-HOOKS* function: map each block %SAVE-NATIVE-MEMORY kept
back at its address and fill it in, so the Java objects holding those
addresses work in the restarted image.  A block whose pages the restarted
process already uses stays lost."
  (let ((page-size 4096)
        (mapped (make-hash-table)))
    (loop for (address . contents) in *saved-native-memory*
          for size = (length contents)
          for first-page = (* page-size (floor address page-size))
          for end = (+ address (max size 1))
          for ok = (loop for page from first-page below end by page-size
                         always (or (gethash page mapped)
                                    (let ((result (ignore-errors
                                                   (sb-posix:mmap (sb-sys:int-sap page) page-size
                                                                  (logior sb-posix:prot-read sb-posix:prot-write)
                                                                  (logior sb-posix:map-private sb-posix:map-anon
                                                                          +map-fixed-noreplace+)
                                                                  -1 0))))
                                      (and result (= (sb-sys:sap-int result) page)
                                           (setf (gethash page mapped) t)))))
          if ok
            do (let ((sap (sb-sys:int-sap address)))
                 (dotimes (i size) (setf (sb-sys:sap-ref-8 sap i) (aref contents i)))
                 (setf (gethash address *unsafe-memory-table*) (cons :restored size)))
          else
            do (format *error-output* "~&; OpenLDK: native memory at #x~X (~D bytes) not restored~%"
                       address size))
    (setf *saved-native-memory* '()))
  ;; Files Java code mapped (FileChannel.map): map them again in place.
  (maphash (lambda (address mapping)
             (destructuring-bind (path position length prot) mapping
               (let ((result
                       (and path
                            (ignore-errors
                             (let ((fd (sb-posix:open path (if (zerop prot) sb-posix:o-rdonly sb-posix:o-rdwr))))
                               (unwind-protect
                                    (sb-posix:mmap (sb-sys:int-sap address) length
                                                   (if (zerop prot) sb-posix:prot-read
                                                       (logior sb-posix:prot-read sb-posix:prot-write))
                                                   (logior (if (= prot 2) sb-posix:map-private sb-posix:map-shared)
                                                           +map-fixed-noreplace+)
                                                   fd position)
                                 (sb-posix:close fd)))))))
                 (unless (and result (= (sb-sys:sap-int result) address))
                   (format *error-output* "~&; OpenLDK: mapping of ~A at #x~X not restored~%"
                           path address)))))
           *file-mappings*)
  (maphash (lambda (path sap-and-size)
             (destructuring-bind (sap . size) sap-and-size
               (let ((result (ignore-errors
                              (let ((fd (sb-posix:open path sb-posix:o-rdonly)))
                                (unwind-protect
                                     (sb-posix:mmap sap size sb-posix:prot-read
                                                    (logior sb-posix:map-private +map-fixed-noreplace+)
                                                    fd 0)
                                  (sb-posix:close fd))))))
                 (unless (and result (= (sb-sys:sap-int result) (sb-sys:sap-int sap)))
                   (format *error-output* "~&; OpenLDK: jimage ~A at #x~X not restored~%"
                           path (sb-sys:sap-int sap))
                   (remhash path *jimage-native-maps*)))))
           *jimage-native-maps*))

(defun %reset-runtime-for-save ()
  "Put the runtime in a state that survives SAVE-LISP-AND-DIE and a restart:
kill other threads, drop process-local handles, and give every lock a fresh
mutex.  Call this last, right before saving, after any build-time runs of
Java code (warmups)."
  (%fasl-cache-stop-build)
  ;; Native memory Java code allocated (Unsafe.allocateMemory: direct
  ;; buffers NIO caches, for one) is malloc'd, outside the saved heap, but
  ;; the Java objects holding its addresses are saved: keep the contents,
  ;; to put back at the same addresses on restart.
  (%save-native-memory)
  ;; Force ProcessEnvironment to re-initialize at runtime so it reads
  ;; the actual runtime environment, not stale warmup state.
  (let ((pe (%get-ldk-class-by-bin-name "java/lang/ProcessEnvironment" t)))
    (when pe (setf (initialized-p pe) nil)))
  ;; Likewise the NIO default filesystem: its clinit captures user.dir
  ;; (the BUILD directory) into UnixFileSystem.defaultDirectory, sending
  ;; relative-path I/O to the wrong place after restart.
  (%reinitialize-static-properties)
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
  ;; So do java.util.zip.ZipFile objects, the program's own and the
  ;; JarFiles JarURLConnection hands out.
  (%zipfile-reset-for-save)
  ;; Child processes the warmups started, and their pipes, belong to this
  ;; process.  ProcessHandleImpl's reaper thread pool lost its worker with
  ;; the other threads, yet would hand the next child's reaping to it, so
  ;; Process.waitFor would never return: reset the class, and its <clinit>
  ;; makes a new pool.
  (clrhash *child-processes*)
  (clrhash *fd-streams*)
  (let ((process-handle (%get-ldk-class-by-bin-name "java/lang/ProcessHandleImpl" t)))
    (when (and process-handle (initialized-p process-handle))
      (%reset-class process-handle)))
  ;; Files a Java program opened itself (FileInputStream, FileOutputStream,
  ;; RandomAccessFile) reopen when the image starts.
  (%record-file-streams-to-reopen)
  ;; jrt: filesystem state (providers, mmapped jimage buffers) is
  ;; process-local; drop it so the restarted image rebuilds it.
  (setf *installed-file-system-providers* nil)
  ;; The mmapped jimages are mapped again in place on restart (see
  ;; %RESTORE-NATIVE-MEMORY): Java objects outside these caches (the Scala
  ;; compiler's JDK classpath) hold buffers over them.
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

(defun %reset-class (class)
  "Return CLASS, an LDK class, to its uninitialized state: clear its
initialized flag and put its static fields back to their initial values,
so its <clinit> runs again on first use.  Compiled methods are kept."
  (setf (initialized-p class) nil
        (initializing-p class) nil
        (init-error class) nil)
  (let ((clos-class (find-class (intern (name class) (ldk-class-package class)) nil)))
    (when clos-class
      (dolist (slot (sb-mop:class-direct-slots clos-class))
        (when (eq (sb-mop:slot-definition-allocation slot) :class)
          (let ((init (sb-mop:slot-definition-initfunction slot))
                (proto (sb-mop:class-prototype clos-class)))
            (setf (slot-value proto (sb-mop:slot-definition-name slot))
                  (and init (funcall init)))))))))

(defun %reset-app-classes ()
  "Reset every application class with %RESET-CLASS.  Returns the number of
classes reset."
  (let ((count 0))
    (when *app-ldk-class-loader*
      (maphash (lambda (bin-name class)
                 (declare (ignore bin-name))
                 (%reset-class class)
                 (incf count))
               (slot-value *app-ldk-class-loader* 'ldk-classes-by-bin-name)))
    count))

(defun %save-shutdown-state ()
  "Return a function that puts the JDK's shutdown state back as it is now.
A warmup that calls System.exit runs the shutdown hooks (*IGNORE-QUIT*
only stops the halt): Shutdown records the hook it ran, VM goes to
SYSTEM_SHUTDOWN and ApplicationShutdownHooks drops its table, so the next
run's addShutdownHook throws \"Shutdown in progress\"."
  (let* ((shutdown (%static-storage "java/lang/Shutdown"))
         (vm (%static-storage "jdk/internal/misc/VM"))
         (hooks (and shutdown (copy-seq (java-array-data (slot-value shutdown '|hooks|)))))
         (init-level (and vm (slot-value vm '|initLevel|))))
    (lambda ()
      (when shutdown
        (replace (java-array-data (slot-value shutdown '|hooks|)) hooks)
        (setf (slot-value shutdown '|currentRunningHook|) -1))
      (when vm
        (setf (slot-value vm '|initLevel|) init-level))
      (let ((app-hooks (%static-storage "java/lang/ApplicationShutdownHooks")))
        (when (and app-hooks (null (slot-value app-hooks '|hooks|)))
          (let ((table (%make-java-instance "java/util/IdentityHashMap")))
            (|<init>()V| table)
            (setf (slot-value app-hooks '|hooks|) table)))))))

(defun %run-warmup (mainclass args classpath &key input directory (timeout 600))
  "Run MAINCLASS with ARGS once at build time, with INPUT (a string, or
NIL for no input) as its standard input, and relative file names resolved
against DIRECTORY when given.  Errors, timeouts and System.exit are
reported and survived, so one bad warmup doesn't stop the build, and the
shutdown state System.exit leaves is undone."
  (let ((restore-shutdown (%save-shutdown-state))
        (input-file (when input
                      (uiop:with-temporary-file (:stream s :pathname p :keep t)
                        (write-string input s)
                        p)))
        (*ignore-quit* t)
        (*default-pathname-defaults*
          (if directory
              (merge-pathnames (uiop:ensure-directory-pathname directory))
              *default-pathname-defaults*)))
    (unwind-protect
         (handler-case
             (sb-ext:with-timeout timeout
               (if input-file
                   (with-open-file (*standard-input* input-file :element-type :default)
                     (main mainclass args :classpath classpath))
                   (main mainclass args :classpath classpath)))
           (sb-ext:timeout ()
             (format *error-output* "~&;; WARMUP ~S timed out after ~Ds~%" args timeout))
           ;; Not CONDITION: the warnings compiling Java methods signals
           ;; (redefinitions, as in an image whose languages loaded) would
           ;; end the warmup there.  Java exceptions are ERRORs.
           (error (c)
             (format *error-output* "~&;; WARMUP ~S caught: ~A~%" args c)))
      (funcall restore-shutdown)
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
   WARMUPS:           List of runs, each a plist (:args LIST :input STRING
                      :directory DIR), DIR being where relative file names
                      resolve; a run with none runs main once, no arguments.
   TIMEOUT:           Seconds allowed for each warmup run.
   RESET-CLASSES:     When true (the default), reset the application's
                      classes after each warmup, so their <clinit>s run again
                      and main starts from scratch in the saved image.  When
                      NIL, the image keeps the state the warmups left, and
                      main must be written to reuse it.
JDK classes always keep their state."
  (initialize)
  (%fasl-cache-start-build)
  (let ((cp (or classpath (uiop:getenv "CLASSPATH") ".")))
    (setf *default-mainclass* default-mainclass
          *default-classpath* cp)
    (dolist (warmup (or warmups (list nil)))
      (destructuring-bind (&key args input directory) warmup
        (format *error-output* "~&;; WARMUP ~A ~{~A~^ ~}~%" default-mainclass args)
        (%run-warmup default-mainclass args cp :input input :directory directory
                                               :timeout timeout)
        ;; After each run, so the next warmup starts from scratch too.
        (when reset-classes
          (format *error-output* "~&;; Reset ~D application classes~%" (%reset-app-classes))))))
  (%reset-runtime-for-save)
  (sb-ext:save-lisp-and-die output-path
                            :executable t
                            :save-runtime-options t
                            :toplevel #'app-main-wrapper))

(defun %initialized-classes ()
  "A hash set of the LDK classes initialized so far."
  (let ((set (make-hash-table :test #'eq)))
    (maphash (lambda (bin-name class)
               (declare (ignore bin-name))
               (when (initialized-p class)
                 (setf (gethash class set) t)))
             *ldk-classes-by-bin-name*)
    set))

(defparameter *per-run-state-classes*
  '("java/security/" "sun/security/" "javax/crypto/"
    "java/util/UUID" "java/util/Random" "java/util/SplittableRandom"
    "java/util/concurrent/ThreadLocalRandom"
    "java/io/File$TempDirectory" "java/nio/file/TempFileHelper"
    "java/util/TimeZone" "sun/util/calendar/")
  "Prefixes of the JDK classes whose static state belongs to a run, not
the build: random seeds, security providers, temporary file names and the
default time zone.  %WARM-UP-JDK resets those it initialized.")

(defun %per-run-state-class-p (bin-name)
  (some (lambda (prefix) (eql 0 (search prefix bin-name))) *per-run-state-classes*))

(defun %warm-up-jdk (&optional (run (lambda (directory)
                                      (%run-warmup "openldk.warmup.JdkWarmup" nil
                                                   (namestring directory)))))
  "Compile and run src/JdkWarmup.java, so the JDK code it calls is compiled
into the image, then reset the classes in *PER-RUN-STATE-CLASSES* it
initialized: SecureRandom's seed, the default time zone and the like are
a run's, not the build's.  Other JDK classes keep their state, as in every
image -- resetting them all would rerun one-time initializers
(java.lang.invoke's).  A user.timezone property it set goes too (System's
properties outlive the reset; the runtime refreshes only those it sets).
RUN, a function of the directory the warmup class was compiled into,
runs it: by default with MAIN, as in an image of the openldk command; an
image with a runtime already started (openldk.interop's) runs it without
resetting its classpath.  Skipped, with a note, without a javac."
  (let* ((java-home (lstring (|java/lang/System.getProperty(Ljava/lang/String;)Ljava/lang/String;|
                              (jstring "java.home"))))
         (javac (merge-pathnames "bin/javac" (uiop:ensure-directory-pathname java-home)))
         (source (asdf:system-relative-pathname :openldk "src/JdkWarmup.java")))
    (unless (and (uiop:file-exists-p javac) (uiop:file-exists-p source))
      (format *error-output* "~&;; No javac in ~A: not warming up the JDK~%" java-home)
      (return-from %warm-up-jdk))
    (let ((directory (merge-pathnames (format nil "openldk-warmup-~D/" (sb-posix:getpid))
                                      (uiop:temporary-directory))))
      (ensure-directories-exist directory)
      (unwind-protect
          (progn
            (uiop:run-program (list (namestring javac) "--release" "25"
                                    "-d" (namestring directory) (namestring source))
                              :output *error-output* :error-output *error-output*)
            (let ((before (%initialized-classes))
                  (timezone (|java/lang/System.getProperty(Ljava/lang/String;)Ljava/lang/String;|
                             (jstring "user.timezone")))
                  (count 0))
              (format *error-output* "~&;; WARMUP openldk.warmup.JdkWarmup~%")
              (handler-case (cl:funcall run directory)
                (error (c)
                  (format *error-output* "~&;; WARMUP openldk.warmup.JdkWarmup caught: ~A~%" c)))
              (maphash (lambda (bin-name class)
                         (when (and (initialized-p class) (not (gethash class before))
                                    (%per-run-state-class-p bin-name))
                           (%reset-class class)
                           (format *error-output* "~&;;   reset ~A~%" bin-name)
                           (incf count)))
                       *ldk-classes-by-bin-name*)
              (unless timezone
                (|java/lang/System.clearProperty(Ljava/lang/String;)Ljava/lang/String;|
                 (jstring "user.timezone")))
              (format *error-output* "~&;; Reset ~D JDK classes the warmup initialized~%" count)))
        (uiop:delete-directory-tree directory :validate t :if-does-not-exist :ignore)))))

(defun make-image (&optional (output-path "openldk"))
  (initialize)
  (%fasl-cache-start-build)
  (%warm-up-jdk)
  (%reset-runtime-for-save)
  ;; Clear all monitor state to prevent deadlocks in the saved image.
  (clrhash *monitors*)
  ;; Clear stale thread mappings — after image restore the Lisp thread objects are different.
  (clrhash *lisp-to-java-threads*)
  (setf *current-thread* nil)
  (%reinitialize-static-properties)
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
  (%reinitialize-static-properties)
  ;; Kill all Java threads before saving core (SBCL can't save with threads running)
  (loop for thread in (bt:all-threads)
        when (and (not (eq thread (bt:current-thread)))
                  (search "Java-Thread" (bt:thread-name thread)))
        do (bt:destroy-thread thread))
  (sb-ext:save-lisp-and-die output-path
                            :executable t
                            :save-runtime-options t
                            :toplevel #'app-main-wrapper))
