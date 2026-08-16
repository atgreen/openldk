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

(in-package :openldk)

(defclass classpath-entry ()
  ()
  (:documentation "Abstract base for classpath locations."))

(defclass jar-classpath-entry (classpath-entry)
  ((jarfile :initarg :jarfile)
   (zipfile :initform nil)
   (zipfile-entries)
   (lock :initform (bt:make-lock "jar-classpath-lock")))
  (:documentation "Classpath entry backed by a JAR/ZIP file."))

(defclass jmod-classpath-entry (classpath-entry)
  ((jmodfile :initarg :jmodfile)
   (zipfile :initform nil)
   (zipfile-entries)
   (lock :initform (bt:make-lock "jmod-classpath-lock")))
  (:documentation "Classpath entry backed by a JMOD file. Like JAR but prepends classes/ to lookups."))

(defclass dir-classpath-entry (classpath-entry)
  ((dir :initarg :dir))
  (:documentation "Classpath entry for a directory tree."))

;; Modify the :around method to establish the restart at the right time
(defmethod open-java-classfile :around ((cpe jar-classpath-entry) classname)
  (restart-case
      (handler-case
          (call-next-method)
        (sb-int:closed-saved-stream-error ()
          (invoke-restart 'reopen-zipfile)))
    (reopen-zipfile ()
      :report "Reopen the zipfile and retry."
      (with-slots (zipfile zipfile-entries jarfile lock) cpe
        (bt:with-lock-held (lock)
          (setf zipfile (zip:open-zipfile jarfile))
          (setf zipfile-entries (zip:zipfile-entries zipfile)))
        (open-java-classfile cpe classname)))))

;; And simplify the primary method - no restart needed here
(defmethod open-java-classfile ((cpe jar-classpath-entry) classname)
  "Return an input stream for a java class, CLASSNAME."
  (with-slots (jarfile zipfile zipfile-entries lock) cpe
    (bt:with-lock-held (lock)
      ;; Ensure the zipfile is open
      (unless zipfile
        (setf zipfile (zip:open-zipfile jarfile))
        (setf zipfile-entries (zip:zipfile-entries zipfile)))
      ;; Look up the class file in the zipfile entries (try .class then .cls)
      (when-let (ze (or (gethash (format nil "~A.class" classname) zipfile-entries)
                        (gethash (format nil "~A.cls" classname) zipfile-entries)))
        ;; Create an in-memory input stream for the class file contents
        (let ((result (flexi-streams:make-in-memory-input-stream (zip:zipfile-entry-contents ze))))
          ;; Add the package to the *PACKAGE* hashtable if it doesn't already exist
          (when-let (last-slash-position (position #\/ classname :from-end t))
            (let ((package-name (take (1+ last-slash-position) classname)))
              (unless (gethash package-name *packages*)
                (setf (gethash package-name *packages*) (jstring jarfile)))))
          ;; Return the input stream
          result)))))

;; --- jmod-classpath-entry support ---

;; JMOD files have a 4-byte magic header (JM\1\0) before the ZIP data.
;; The ZIP library reads offsets relative to the start of the file, but
;; those offsets are actually relative to byte 4. This Gray stream wrapper
;; transparently adds the offset to all file-position seeks.

(defclass jmod-offset-stream (sb-gray:fundamental-binary-input-stream)
  ((inner :initarg :inner)
   (offset :initarg :offset :initform 4)))

(defmethod sb-gray:stream-read-byte ((s jmod-offset-stream))
  (read-byte (slot-value s 'inner) nil :eof))

(defmethod sb-gray:stream-read-sequence ((s jmod-offset-stream) seq &optional (start 0) end)
  (read-sequence seq (slot-value s 'inner) :start start :end end))

(defmethod sb-gray:stream-file-position ((s jmod-offset-stream) &optional position)
  (with-slots (inner offset) s
    (cond
      ((null position)
       (let ((pos (file-position inner)))
         (when pos (- pos offset))))
      ((eq position :end)
       (file-position inner :end))
      ((eq position :start)
       (file-position inner offset))
      (t
       (file-position inner (+ position offset))))))

(defmethod close ((s jmod-offset-stream) &key abort)
  (close (slot-value s 'inner) :abort abort))

(defun open-jmod-zipfile (pathname)
  "Open a JMOD file as a zipfile, skipping the 4-byte JMOD magic header."
  (let ((raw (open pathname :element-type '(unsigned-byte 8))))
    ;; Verify JMOD magic: JM\1\0
    (let ((magic (make-array 4 :element-type '(unsigned-byte 8))))
      (read-sequence magic raw)
      (unless (and (= (aref magic 0) #x4A) ; J
                   (= (aref magic 1) #x4D) ; M
                   (= (aref magic 2) #x01)
                   (= (aref magic 3) #x00))
        (close raw)
        (error "Not a valid JMOD file: ~A" pathname)))
    ;; Wrap in offset stream so ZIP library sees adjusted positions
    (let ((wrapper (make-instance 'jmod-offset-stream :inner raw :offset 4)))
      (zip::open-zipfile-from-stream wrapper))))

;; --- jmod-classpath-entry methods ---

(defmethod open-java-classfile :around ((cpe jmod-classpath-entry) classname)
  (restart-case
      (handler-case
          (call-next-method)
        (sb-int:closed-saved-stream-error ()
          (invoke-restart 'reopen-zipfile)))
    (reopen-zipfile ()
      :report "Reopen the zipfile and retry."
      (with-slots (zipfile zipfile-entries jmodfile lock) cpe
        (bt:with-lock-held (lock)
          (setf zipfile (open-jmod-zipfile jmodfile))
          (setf zipfile-entries (zip:zipfile-entries zipfile)))
        (open-java-classfile cpe classname)))))

(defmethod open-java-classfile ((cpe jmod-classpath-entry) classname)
  "Return an input stream for a java class, CLASSNAME, from a JMOD file."
  (with-slots (jmodfile zipfile zipfile-entries lock) cpe
    (bt:with-lock-held (lock)
      (unless zipfile
        (setf zipfile (open-jmod-zipfile jmodfile))
        (setf zipfile-entries (zip:zipfile-entries zipfile)))
      (when-let (ze (or (gethash (format nil "classes/~A.class" classname) zipfile-entries)
                        (gethash (format nil "classes/~A.cls" classname) zipfile-entries)))
        (let ((result (flexi-streams:make-in-memory-input-stream (zip:zipfile-entry-contents ze))))
          (when-let (last-slash-position (position #\/ classname :from-end t))
            (let ((package-name (take (1+ last-slash-position) classname)))
              (unless (gethash package-name *packages*)
                (setf (gethash package-name *packages*) (jstring jmodfile)))))
          result)))))

(defmethod open-resource :around ((cpe jmod-classpath-entry) resource-name)
  "Handle closed stream errors by reopening the zipfile."
  (restart-case
      (handler-case
          (call-next-method)
        (sb-int:closed-saved-stream-error ()
          (invoke-restart 'reopen-zipfile)))
    (reopen-zipfile ()
      :report "Reopen the zipfile and retry."
      (with-slots (zipfile zipfile-entries jmodfile lock) cpe
        (bt:with-lock-held (lock)
          (setf zipfile (open-jmod-zipfile jmodfile))
          (setf zipfile-entries (zip:zipfile-entries zipfile)))
        (open-resource cpe resource-name)))))

(defmethod open-resource ((cpe jmod-classpath-entry) resource-name)
  "Return an input stream for a resource RESOURCE-NAME in this JMOD, or NIL if not found."
  (with-slots (jmodfile zipfile zipfile-entries lock) cpe
    (bt:with-lock-held (lock)
      (unless zipfile
        (setf zipfile (open-jmod-zipfile jmodfile))
        (setf zipfile-entries (zip:zipfile-entries zipfile)))
      (when-let (ze (gethash (format nil "classes/~A" resource-name) zipfile-entries))
        (flexi-streams:make-in-memory-input-stream (zip:zipfile-entry-contents ze))))))

(defmethod get-resource-url ((cpe jmod-classpath-entry) resource-name)
  "Return a jmod: URL string for a resource if it exists in this JMOD, or NIL."
  (with-slots (jmodfile zipfile zipfile-entries lock) cpe
    (bt:with-lock-held (lock)
      (unless zipfile
        (setf zipfile (open-jmod-zipfile jmodfile))
        (setf zipfile-entries (zip:zipfile-entries zipfile)))
      (when (gethash (format nil "classes/~A" resource-name) zipfile-entries)
        (format nil "jmod:file:~A!/classes/~A" jmodfile resource-name)))))

;; --- jimage-classpath-entry support ---

;; Headless JDK builds (e.g. Fedora's java-NN-openjdk-headless) do not ship the
;; jmods/ directory; the runtime classes live only in the jimage container at
;; $JAVA_HOME/lib/modules. The jimage format (magic 0xCAFEDADA) is a container
;; of resources ("/<module>/<path>") indexed by a perfect hash. Rather than
;; reimplement the perfect-hash lookup, we scan every location record once at
;; load time and build a plain path -> (module data-offset compressed-size
;; uncompressed-size) index, which is all class/resource access needs.

(defclass jimage-classpath-entry (classpath-entry)
  ((imagefile :initarg :imagefile)
   (stream :initform nil)               ; open binary stream for resource data reads
   (index :initform nil)                ; hashtable: resource path -> location list
   (data-base :initform 0)              ; file offset where the resource data begins
   (lock :initform (bt:make-lock "jimage-classpath-lock")))
  (:documentation "Classpath entry backed by a JDK jimage file ($JAVA_HOME/lib/modules)."))

(defun %jimage-u4-le (vec i)
  "Read a little-endian unsigned 32-bit integer from byte VECTOR VEC at index I."
  (logior (aref vec i)
          (ash (aref vec (+ i 1)) 8)
          (ash (aref vec (+ i 2)) 16)
          (ash (aref vec (+ i 3)) 24)))

(defun %jimage-string (strings off)
  "Read the NUL-terminated UTF-8 string in the STRINGS byte vector at offset OFF."
  (let ((end (position 0 strings :start off)))
    (sb-ext:octets-to-string strings :start off :end end :external-format :utf-8)))

(defun %jimage-parse-location (locs off)
  "Parse a jimage location record from the LOCS byte vector at offset OFF.
Returns a simple-vector indexed by attribute kind (1=module 2=parent 3=base
4=extension 5=offset 6=compressed-size 7=uncompressed-size)."
  (let ((i off)
        (attrs (make-array 8 :initial-element 0)))
    (loop
      (let ((data (aref locs i)))
        (incf i)
        (when (zerop data) (return))
        (let ((kind (ash data -3))
              (len (1+ (logand data 7)))
              (val 0))
          (dotimes (k len)
            (setf val (logior (ash val 8) (aref locs i)))
            (incf i))
          (setf (aref attrs kind) val))))
    attrs))

(defun %jimage-open-stream (pathname)
  (open pathname :element-type '(unsigned-byte 8)))

(defun load-jimage-index (cpe)
  "Open the jimage file for CPE, verify its header, and build the resource index.
Leaves the data stream open (positioned arbitrarily) for later resource reads."
  (with-slots (imagefile stream index data-base) cpe
    (let ((s (%jimage-open-stream imagefile)))
      (setf stream s)
      (let ((header (make-array 28 :element-type '(unsigned-byte 8))))
        (read-sequence header s)
        (let ((magic (%jimage-u4-le header 0)))
          (unless (= magic #xCAFEDADA)
            (close s)
            (error "Not a valid jimage file (bad magic ~8,'0X): ~A" magic imagefile)))
        (let* ((table-length (%jimage-u4-le header 16))
               (locations-size (%jimage-u4-le header 20))
               (strings-size (%jimage-u4-le header 24))
               (offsets-size (* table-length 4))
               ;; redirect table (table-length u4) is not needed by the scan.
               (offsets (make-array offsets-size :element-type '(unsigned-byte 8)))
               (locations (make-array locations-size :element-type '(unsigned-byte 8)))
               (strings (make-array strings-size :element-type '(unsigned-byte 8)))
               (ht (make-hash-table :test 'equal :size table-length)))
          ;; Skip the redirect table, then read offsets, locations and strings.
          (file-position s (+ 28 offsets-size))
          (read-sequence offsets s)
          (read-sequence locations s)
          (read-sequence strings s)
          ;; The resource data section begins immediately after the strings.
          (setf data-base (file-position s))
          (dotimes (i table-length)
            (let ((loc-off (%jimage-u4-le offsets (* i 4))))
              (unless (zerop loc-off)
                (let* ((attrs (%jimage-parse-location locations loc-off))
                       (module (%jimage-string strings (aref attrs 1)))
                       (parent (%jimage-string strings (aref attrs 2)))
                       (base (%jimage-string strings (aref attrs 3)))
                       (ext (%jimage-string strings (aref attrs 4)))
                       (path (concatenate 'string
                                          (if (plusp (length parent))
                                              (concatenate 'string parent "/") "")
                                          base
                                          (if (plusp (length ext))
                                              (concatenate 'string "." ext) ""))))
                  ;; Only index real module resources; skip meta entries (empty module).
                  (when (plusp (length module))
                    (setf (gethash path ht)
                          (list module
                                (+ data-base (aref attrs 5)) ; absolute file offset
                                (aref attrs 6)               ; compressed size (0 = stored)
                                (aref attrs 7))))))))         ; uncompressed size
          (setf index ht)))))
  cpe)

(defun ensure-jimage-loaded (cpe)
  "Ensure CPE's index is built and its data stream is open (reopening after a
saved image was dumped)."
  (with-slots (stream index) cpe
    (cond
      ((null index) (load-jimage-index cpe))
      ((null stream) (setf stream (%jimage-open-stream (slot-value cpe 'imagefile)))))))

(defun %jimage-read-resource (cpe location)
  "Read the bytes for a resource LOCATION (module offset comp unc) from CPE."
  (destructuring-bind (module offset comp unc) location
    (declare (ignore module))
    (when (plusp comp)
      (error "Compressed jimage resources are not supported: ~A" (slot-value cpe 'imagefile)))
    (let ((bytes (make-array unc :element-type '(unsigned-byte 8))))
      (file-position (slot-value cpe 'stream) offset)
      (read-sequence bytes (slot-value cpe 'stream))
      bytes)))

(defmethod open-java-classfile :around ((cpe jimage-classpath-entry) classname)
  (restart-case
      (handler-case
          (call-next-method)
        (sb-int:closed-saved-stream-error ()
          (invoke-restart 'reopen-jimage)))
    (reopen-jimage ()
      :report "Reopen the jimage stream and retry."
      (with-slots (stream lock) cpe
        (bt:with-lock-held (lock)
          (setf stream (%jimage-open-stream (slot-value cpe 'imagefile))))
        (open-java-classfile cpe classname)))))

(defmethod open-java-classfile ((cpe jimage-classpath-entry) classname)
  "Return an input stream for a java class, CLASSNAME, from a jimage file."
  (with-slots (index lock) cpe
    (bt:with-lock-held (lock)
      (ensure-jimage-loaded cpe)
      (when-let (location (gethash (format nil "~A.class" classname) index))
        (let ((result (flexi-streams:make-in-memory-input-stream
                       (%jimage-read-resource cpe location))))
          (when-let (last-slash-position (position #\/ classname :from-end t))
            (let ((package-name (take (1+ last-slash-position) classname)))
              (unless (gethash package-name *packages*)
                (setf (gethash package-name *packages*) (jstring (slot-value cpe 'imagefile))))))
          result)))))

(defmethod open-resource :around ((cpe jimage-classpath-entry) resource-name)
  (restart-case
      (handler-case
          (call-next-method)
        (sb-int:closed-saved-stream-error ()
          (invoke-restart 'reopen-jimage)))
    (reopen-jimage ()
      :report "Reopen the jimage stream and retry."
      (with-slots (stream lock) cpe
        (bt:with-lock-held (lock)
          (setf stream (%jimage-open-stream (slot-value cpe 'imagefile))))
        (open-resource cpe resource-name)))))

(defmethod open-resource ((cpe jimage-classpath-entry) resource-name)
  "Return an input stream for a resource RESOURCE-NAME in this jimage, or NIL."
  (with-slots (index lock) cpe
    (bt:with-lock-held (lock)
      (ensure-jimage-loaded cpe)
      (when-let (location (gethash resource-name index))
        (flexi-streams:make-in-memory-input-stream
         (%jimage-read-resource cpe location))))))

(defmethod get-resource-url ((cpe jimage-classpath-entry) resource-name)
  "Return a jrt: URL string for a resource if it exists in this jimage, or NIL."
  (with-slots (index lock) cpe
    (bt:with-lock-held (lock)
      (ensure-jimage-loaded cpe)
      (when-let (location (gethash resource-name index))
        (format nil "jrt:/~A/~A" (first location) resource-name)))))

(defun discover-jmod-classpath-entries ()
  "Return classpath entries for the JDK runtime modules.

Prefer $JAVA_HOME/jmods/*.jmod files. When those are absent -- as in headless
JDK builds (e.g. Fedora's java-NN-openjdk-headless) -- fall back to reading the
jimage container at $JAVA_HOME/lib/modules directly."
  (let ((java-home (uiop:getenv "JAVA_HOME")))
    (when java-home
      (let* ((jmods-dir (format nil "~A/jmods/" java-home))
             (jmod-entries
               (when (uiop:directory-exists-p jmods-dir)
                 (loop for jmod in (directory (merge-pathnames "*.jmod" jmods-dir))
                       collect (make-instance 'jmod-classpath-entry
                                              :jmodfile (namestring jmod))))))
        (or jmod-entries
            (let ((modules-file (format nil "~A/lib/modules" java-home)))
              (when (uiop:file-exists-p modules-file)
                (list (make-instance 'jimage-classpath-entry
                                     :imagefile modules-file)))))))))

(defmethod open-java-classfile ((cpe dir-classpath-entry) classname)
  "Return an input stream for a java class, CLASSNAME."
  (with-slots (dir) cpe
    (let ((fqn (format nil "~A~A~A.class" dir (uiop:directory-separator-for-host) classname)))
      (when (uiop:file-exists-p fqn)
        ;; Read into memory and return an in-memory input stream
        (let* ((bytes (read-file-into-byte-vector fqn))
               (result (flexi-streams:make-in-memory-input-stream bytes)))
          ;; Add this package to the *PACKAGE* hashtable.
          (when-let (last-slash-position (position #\/ classname :from-end t))
            (let ((package-name (take (1+ last-slash-position) classname)))
              (unless (gethash package-name *packages*)
                (setf (gethash package-name *packages*) (jstring fqn)))))
          result)))))

;;; Resource loading (for finding arbitrary files in classpath, not just .class files)
;;; See url.lisp for documentation on why this is needed and how it integrates
;;; with Java's ClassLoader.getResource() mechanism.

(defmethod open-resource :around ((cpe jar-classpath-entry) resource-name)
  "Handle closed stream errors by reopening the zipfile."
  (restart-case
      (handler-case
          (call-next-method)
        (sb-int:closed-saved-stream-error ()
          (invoke-restart 'reopen-zipfile)))
    (reopen-zipfile ()
      :report "Reopen the zipfile and retry."
      (with-slots (zipfile zipfile-entries jarfile lock) cpe
        (bt:with-lock-held (lock)
          (setf zipfile (zip:open-zipfile jarfile))
          (setf zipfile-entries (zip:zipfile-entries zipfile)))
        (open-resource cpe resource-name)))))

(defmethod open-resource ((cpe jar-classpath-entry) resource-name)
  "Return an input stream for a resource RESOURCE-NAME in this JAR, or NIL if not found."
  (with-slots (jarfile zipfile zipfile-entries lock) cpe
    (bt:with-lock-held (lock)
      ;; Ensure the zipfile is open
      (unless zipfile
        (setf zipfile (zip:open-zipfile jarfile))
        (setf zipfile-entries (zip:zipfile-entries zipfile)))
      ;; Look up the resource in the zipfile entries
      (when-let (ze (gethash resource-name zipfile-entries))
        (flexi-streams:make-in-memory-input-stream (zip:zipfile-entry-contents ze))))))

(defmethod open-resource ((cpe dir-classpath-entry) resource-name)
  "Return an input stream for a resource RESOURCE-NAME in this directory, or NIL if not found."
  (with-slots (dir) cpe
    (let ((fqn (format nil "~A~A~A" dir (uiop:directory-separator-for-host) resource-name)))
      (when (uiop:file-exists-p fqn)
        (flexi-streams:make-in-memory-input-stream (read-file-into-byte-vector fqn))))))

(defmethod get-resource-url ((cpe jar-classpath-entry) resource-name)
  "Return a jar: URL string for a resource if it exists in this JAR, or NIL."
  (with-slots (jarfile zipfile zipfile-entries lock) cpe
    (bt:with-lock-held (lock)
      ;; Ensure the zipfile is open
      (unless zipfile
        (setf zipfile (zip:open-zipfile jarfile))
        (setf zipfile-entries (zip:zipfile-entries zipfile)))
      ;; Check if the resource exists
      (when (gethash resource-name zipfile-entries)
        (format nil "jar:file:~A!/~A" jarfile resource-name)))))

(defmethod get-resource-url ((cpe dir-classpath-entry) resource-name)
  "Return a file: URL string for a resource if it exists in this directory, or NIL."
  (with-slots (dir) cpe
    (let ((fqn (format nil "~A~A~A" dir (uiop:directory-separator-for-host) resource-name)))
      (when (uiop:file-exists-p fqn)
        (format nil "file:~A" fqn)))))

(defun open-resource-on-classpath (resource-name)
  "Find and open a resource RESOURCE-NAME on the classpath. Returns an input stream or NIL."
  (loop for cpe in *classpath*
        for stream = (open-resource cpe resource-name)
        when stream return stream))

(defun get-resource-url-on-classpath (resource-name)
  "Find a resource RESOURCE-NAME on the classpath and return its URL string, or NIL."
  (loop for cpe in *classpath*
        for url = (get-resource-url cpe resource-name)
        when url return url))

(defun list-jar-classes (jar-entry)
  "Return a list of all class file names in a JAR file."
  (with-slots (jarfile zipfile zipfile-entries lock) jar-entry
    (bt:with-lock-held (lock)
      ;; Ensure the zipfile is open
      (unless zipfile
        (setf zipfile (zip:open-zipfile jarfile))
        (setf zipfile-entries (zip:zipfile-entries zipfile)))
      ;; Collect all .class files
      (let ((classes nil))
        (maphash (lambda (name entry)
                   (declare (ignore entry))
                   (when (str:ends-with? ".class" name)
                     (push name classes)))
                 zipfile-entries)
        classes))))
