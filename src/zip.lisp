;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: OPENLDK; Base: 10 -*-
;;;
;;; Copyright (C) 2025, 2026  Anthony Green <green@moxielogic.com>
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

;; OpenLDK provides its own implementation of the
;; java.util.zip.ZipFile methods in order to use pure lisp code.  The
;; native methods used by java.util.zip.ZipFile are too low level and
;; would require new cffi code, which we want to avoid for now.
;; This should probably be replaced with the OpenJDK implemenation and
;; cffi code for the native layer.  But for now, we do just enough
;; to make things interesting....

(in-package :openldk)

(defvar *ziphandle* 9000)
(defvar *ziphandle-map* (make-hash-table))

;; These two classes get redefined when we read their classfiles.

(defclass |java/util/zip/ZipFile| ()
  ()
  (:documentation "Stub for java.util.zip.ZipFile; populated at runtime."))

(defclass |java/util/jar/JarFile| ()
  ()
  (:documentation "Stub for java.util.jar.JarFile; populated at runtime."))

;; ZipFile and JarFile take their fields from the JDK's class files, and
;; JDK 25's have neither the name nor the jzfile field these methods once
;; kept their state in, so the path and the open zip live in a side table.
(defvar *zipfile-state* (make-hash-table :test #'eq :weakness :key :synchronized t)
  "java.util.zip.ZipFile -> (path . zip:zipfile).")

(defvar *zipfile-entry-metadata* (make-hash-table :test #'eq :weakness :key :synchronized t)
  "Open zip -> entry offset -> (stored CRC . compression method).")

(defvar *zipfile-read-locks* (make-hash-table :test #'eq :weakness :key :synchronized t)
  "Open zip -> lock protecting its shared seekable input stream.")

(defvar *zipfile-input-streams* (make-hash-table :test 'eq :weakness :key :synchronized t)
  "Java ZipFile -> weak set of its open entry streams.")
(defvar *zipfile-object-locks* (make-hash-table :test 'eq :weakness :key :synchronized t))
(defvar *zipfile-registry-lock* (bt:make-lock "Java ZIP registry"))
(defvar *zipfile-cleanup-tokens* (make-hash-table :test 'eq :weakness :key :synchronized t))
(defstruct zipfile-cleanup-token)

(defun %zipfile-lock (zip-file)
  (bt:with-lock-held (*zipfile-registry-lock*)
    (or (gethash zip-file *zipfile-object-locks*)
        (setf (gethash zip-file *zipfile-object-locks*) (bt:make-recursive-lock "Java ZipFile")))))

(defun %cancel-zipfile-cleanup (zip-file)
  (when-let ((token (gethash zip-file *zipfile-cleanup-tokens*)))
    (trivial-garbage:cancel-finalization token)
    (remhash zip-file *zipfile-cleanup-tokens*)))

(defun %finalize-java-zipfile (zip-file zip)
  "Finalize an owned token, without cancelling any user finalizers on ZIP-FILE."
  (%cancel-zipfile-cleanup zip-file)
  (let ((token (make-zipfile-cleanup-token))
        (lock (gethash zip *zipfile-read-locks*)))
    (trivial-garbage:finalize token
      (lambda ()
        (ignore-errors
          (bt:with-lock-held (lock) (zip:close-zipfile zip)))))
    (setf (gethash zip-file *zipfile-cleanup-tokens*) token)))

(defun %java-zip-entry-contents (zip entry)
  "Read ENTRY atomically with respect to other readers of ZIP's file stream."
  (bt:with-lock-held ((gethash zip *zipfile-read-locks*))
    (zip:zipfile-entry-contents entry)))

(defun %open-java-zipfile (path)
  "Open PATH and retain central-directory metadata omitted by the zip library.
Read the stored CRC, rather than computing it from the data: URLClassLoader
uses this value to detect corrupted entries, including streamed ZIP entries
whose local headers contain zero CRCs."
  (let* ((zip (zip:open-zipfile path))
         (stream (zip::zipfile-stream zip))
         (position (file-position stream))
         (metadata (make-hash-table)))
    (handler-case
        (unwind-protect
             (progn
               (zip::seek-to-end-header stream)
               (let ((end (zip::make-end-header stream)))
                 (file-position stream (zip::end/central-directory-offset end))
                 (dotimes (index (zip::end/total-files end))
                   (let ((header (zip::make-directory-entry stream)))
                     (setf (gethash (zip::cd/offset header) metadata)
                           (cons (zip::cd/crc header) (zip::cd/method header)))
                     (file-position stream (+ (file-position stream)
                                              (zip::cd/name-length header)
                                              (zip::cd/extra-length header)
                                              (zip::cd/comment-length header))))))
               (setf (gethash zip *zipfile-entry-metadata*) metadata
                     (gethash zip *zipfile-read-locks*) (bt:make-lock "Java ZIP input stream"))
               zip)
          (file-position stream position))
      (error (condition)
        (zip:close-zipfile zip)
        (error condition)))))

(defun %zipfile-name (zip-file)
  "Return the retained Java pathname of ZIP-FILE."
  (first (gethash zip-file *zipfile-state*)))

(defun %zipfile-zip (zip-file)
  "Return the native ZIP, reopening an image-retained archive once."
  (bt:with-recursive-lock-held ((%zipfile-lock zip-file))
    (when-let ((state (gethash zip-file *zipfile-state*)))
      (or (rest state)
          (let ((zip (%open-java-zipfile (lstring (first state)))))
            (%finalize-java-zipfile zip-file zip)
            (setf (rest state) zip))))))

(defun %zipfile-reset-for-save ()
  "Snapshot live entry streams and close native ZIP handles before image saving."
  (maphash
   (lambda (zip-file state)
     (bt:with-recursive-lock-held ((%zipfile-lock zip-file))
       (when-let ((streams (gethash zip-file *zipfile-input-streams*)))
         (maphash (lambda (stream ignored)
                    (declare (ignore ignored))
                    (%snapshot-zip-input-stream stream)) streams))
       (%cancel-zipfile-cleanup zip-file)
       (when (rest state)
         (bt:with-lock-held ((gethash (rest state) *zipfile-read-locks*))
           (zip:close-zipfile (rest state)))
         (setf (rest state) nil))))
   *zipfile-state*)
  (clrhash *zipfile-object-locks*)
  (setf *zipfile-registry-lock* (bt:make-lock "Java ZIP registry")))


(defun |java/util/zip/ZipFile.<clinit>()V| ()
  "Class initializer for ZipFile.  Of the JDK's, keep only the registration
of the JavaUtilZipFileAccess bridge: JarFile.entries() goes through it."
  (classload "jdk/internal/access/SharedSecrets")
  (classload "java/util/zip/ZipFile$1")
  (let ((access (%make-java-instance "java/util/zip/ZipFile$1")))
    (|<init>()V| access)
    (|jdk/internal/access/SharedSecrets.setJavaUtilZipFileAccess(Ljdk/internal/access/JavaUtilZipFileAccess;)V|
     access)))

(defun |<init>| ()
  "Default constructor stub (no-op).")

(defun %zipfile-open (zip-file name)
  "Open NAME for ZIP-FILE and arrange fallback cleanup for an abandoned archive."
  (%check-not-directory name)
  (bt:with-recursive-lock-held ((%zipfile-lock zip-file))
    (let ((zip (%open-java-zipfile (lstring name))))
      (setf (gethash zip-file *zipfile-state*) (cons name zip))
      (%finalize-java-zipfile zip-file zip))))

(defmethod |<init>(Ljava/io/File;I)V| ((this |java/util/zip/ZipFile|) file mode)
  (assert (= mode 1)) ; only support read for now
  (%zipfile-open this (|getPath()| file)))

(defmethod |<init>(Ljava/io/File;)V| ((this |java/util/zip/ZipFile|) file)
  (|<init>(Ljava/io/File;I)V| this file 1))

(defmethod |<init>(Ljava/lang/String;)V| ((this |java/util/zip/ZipFile|) name)
  (%zipfile-open this name))

;; The Charset overloads (the IntelliJ platform's): entry names are decoded
;; by the zip library, so the charset goes unused.
(defmethod |<init>(Ljava/io/File;Ljava/nio/charset/Charset;)V| ((this |java/util/zip/ZipFile|) file charset)
  (declare (ignore charset))
  (|<init>(Ljava/io/File;I)V| this file 1))

(defmethod |<init>(Ljava/io/File;ILjava/nio/charset/Charset;)V| ((this |java/util/zip/ZipFile|) file mode charset)
  (declare (ignore charset))
  (|<init>(Ljava/io/File;I)V| this file mode))

(defmethod |<init>(Ljava/lang/String;Ljava/nio/charset/Charset;)V| ((this |java/util/zip/ZipFile|) name charset)
  (declare (ignore charset))
  (%zipfile-open this name))

(defun %make-zip-entry (zip-file name ze)
  "A java.util.zip.ZipEntry named NAME (a Java string) for the zip library's
entry ZE in ZIP-FILE."
  (classload "java/util/zip/ZipEntry")
  (let ((entry (%make-java-instance "java/util/zip/ZipEntry")))
    (|<init>(Ljava/lang/String;)V| entry name)
    (|setSize(J)| entry (zip::zipfile-entry-size ze))
    (|setCompressedSize(J)V| entry (zip::zipfile-entry-compressed-size ze))
    (let ((metadata (gethash (zip::zipfile-entry-offset ze)
                             (gethash (%zipfile-zip zip-file) *zipfile-entry-metadata*))))
      (|setCrc(J)V| entry (first metadata))
      (|setMethod(I)V| entry (rest metadata)))
    ;; A JarFile's entries are JarEntries: JarFile.getJarEntry casts
    ;; (Groovy's compiler, finding its global transforms in jars).
    (if (typep zip-file '|java/util/jar/JarFile|)
        (let ((jar-entry (%make-java-instance "java/util/jar/JarEntry")))
          (|<init>(Ljava/util/zip/ZipEntry;)V| jar-entry entry)
          jar-entry)
        entry)))

(defmethod |getEntry(Ljava/lang/String;)Ljava/util/zip/ZipEntry;| ((this |java/util/zip/ZipFile|) name)
  "NAME's entry, or the directory entry NAME/, as the JDK's ZipFile finds."
  (let* ((zip (%zipfile-zip this))
         (lisp-name (lstring name))
         (directory-name (concatenate 'string lisp-name "/")))
    (if-let (ze (zip:get-zipfile-entry lisp-name zip))
      (%make-zip-entry this name ze)
      (when-let (ze (and (not (ends-with? "/" lisp-name))
                         (zip:get-zipfile-entry directory-name zip)))
        (%make-zip-entry this (jstring directory-name) ze)))))

(defun %zipfile-entry-list (zip-file)
  "A java.util.ArrayList of ZIP-FILE's entries."
  (classload "java/util/ArrayList")
  (let ((list (%make-java-instance "java/util/ArrayList")))
    (|<init>()V| list)
    (zip:do-zipfile-entries (name ze (%zipfile-zip zip-file))
      (|add(Ljava/lang/Object;)| list (%make-zip-entry zip-file (jstring name) ze)))
    list))

(defmethod |entries()Ljava/util/Enumeration;| ((this |java/util/zip/ZipFile|))
  "An Enumeration of the entries (Scala's compiler lists jars this way)."
  (|java/util/Collections.enumeration(Ljava/util/Collection;)Ljava/util/Enumeration;|
   (%zipfile-entry-list this)))

(defmethod |stream()Ljava/util/stream/Stream;| ((this |java/util/zip/ZipFile|))
  (|stream()Ljava/util/stream/Stream;| (%zipfile-entry-list this)))

;; JarFile reaches ZipFile's internals through the JavaUtilZipFileAccess
;; bridge (ZipFile$1), whose methods call these private ZipFile methods.
;; The JDK's read state that only its native zip code fills in, so serve
;; them from the zip library (Spring's classpath scanning enumerates jars
;; with JarFile.entries()).  jarEntries and jarStream list the entries
;; directly: JarFile's entries() and stream() call back through the bridge.
(defmethod |jarEntries()Ljava/util/Enumeration;| ((this |java/util/zip/ZipFile|))
  (|java/util/Collections.enumeration(Ljava/util/Collection;)Ljava/util/Enumeration;|
   (%zipfile-entry-list this)))

(defmethod |jarStream()Ljava/util/stream/Stream;| ((this |java/util/zip/ZipFile|))
  (|stream()Ljava/util/stream/Stream;| (%zipfile-entry-list this)))

(defmethod |entryNameStream()Ljava/util/stream/Stream;| ((this |java/util/zip/ZipFile|))
  (classload "java/util/ArrayList")
  (let ((list (%make-java-instance "java/util/ArrayList")))
    (|<init>()V| list)
    (zip:do-zipfile-entries (name ze (%zipfile-zip this))
      (declare (ignore ze))
      (|add(Ljava/lang/Object;)| list (jstring name)))
    (|stream()Ljava/util/stream/Stream;| list)))

(defun %zip-manifest-name (zip-file)
  "The name of ZIP-FILE's META-INF/MANIFEST.MF entry, ignoring case, or NIL."
  (zip:do-zipfile-entries (name ze (%zipfile-zip zip-file))
    (declare (ignore ze))
    (when (string-equal name "META-INF/MANIFEST.MF")
      (return-from %zip-manifest-name name)))
  nil)

(defun %zip-signature-related-names (zip-file)
  "The names of ZIP-FILE's signature-related files, as ZipFile's Source
finds them: META-INF/*.SF, *.RSA, *.DSA, *.EC and SIG-*, not in subdirectories."
  (let ((names nil))
    (zip:do-zipfile-entries (name ze (%zipfile-zip zip-file))
      (declare (ignore ze))
      (when (and (> (length name) 9)
                 (string-equal "META-INF/" name :end2 9)
                 (not (find #\/ name :start 9)))
        (let ((base (drop 9 name)))
          (when (or (some (lambda (ext) (ends-with? ext base :ignore-case t))
                          '(".SF" ".RSA" ".DSA" ".EC"))
                    (starts-with? "SIG-" base :ignore-case t))
            (push name names)))))
    (nreverse names)))

(defmethod |getManifestNum()I| ((this |java/util/zip/ZipFile|))
  (%zip-manifest-count this))

(defmethod |getManifestName(Z)Ljava/lang/String;| ((this |java/util/zip/ZipFile|) only-if-signature-related-files)
  (let ((name (%zip-manifest-name this)))
    (when (and name
               (or (eql only-if-signature-related-files 0)
                   (%zip-signature-related-names this)))
      (jstring name))))

(defmethod |getManifestAndSignatureRelatedFiles()Ljava/util/List;| ((this |java/util/zip/ZipFile|))
  (classload "java/util/ArrayList")
  (let ((list (%make-java-instance "java/util/ArrayList")))
    (|<init>()V| list)
    (dolist (name (append (when-let ((manifest (%zip-manifest-name this))) (list manifest))
                          (%zip-signature-related-names this)))
      (|add(Ljava/lang/Object;)| list (jstring name)))
    list))

(defmethod |getMetaInfVersions(Ljava/lang/String;)Ljava/util/BitSet;| ((this |java/util/zip/ZipFile|) name)
  "The versions N with a META-INF/versions/N/NAME entry."
  (classload "java/util/BitSet")
  (let ((versions (%make-java-instance "java/util/BitSet"))
        (name (lstring name)))
    (|<init>()V| versions)
    (zip:do-zipfile-entries (entry-name ze (%zipfile-zip this))
      (declare (ignore ze))
      (when (and (starts-with? "META-INF/versions/" entry-name)
                 (ends-with? (concatenate 'string "/" name) entry-name))
        (let ((version (parse-integer entry-name :start 18 :junk-allowed t)))
          (when (and version
                     (string= entry-name (format nil "META-INF/versions/~D/~A" version name)))
            (|set(I)V| versions version)))))
    versions))

;; This one reads ZipFile's state itself rather than calling a method.
(setf (gethash "java/util/zip/ZipFile$1.startsWithLocHeader(Ljava/util/zip/ZipFile;)Z" *native-overrides*)
      (lambda (this zip-file)
        (declare (ignore this))
        (if (%zipfile-zip zip-file) 1 0)))

(defmethod |size()I| ((this |java/util/zip/ZipFile|))
  (bt:with-recursive-lock-held ((%zipfile-lock this))
    (let ((zip (%zipfile-zip this))
          (count 0))
      (unless zip (%throw '|java/lang/IllegalStateException| "ZipFile closed"))
      (zip:do-zipfile-entries (name ze zip)
        (declare (ignore name ze))
        (incf count))
      count)))

(defmethod |getMetaInfEntryNames()| ((this |java/util/jar/JarFile|))
  (let ((meta-entries (list)))
    (zip:do-zipfile-entries (name entry (%zipfile-zip this))
      (when (and (< 9 (length name))
                 (string= (take 9 name) "META-INF/"))
        (push (jstring name) meta-entries)))
    (make-java-array
     :component-class (%get-java-class-by-bin-name "java/lang/String")
     :initial-contents meta-entries)))

(defclass/std <zip-entry-source-stream> (|java/io/InputStream|)
  ((zip-file) (compressed-position) (remaining) (closed)))

(defclass/std <zip-input-stream> (|java/io/InputStream|)
  ((zip-file) (delegate) (inflater) (close-owner) (closed) (remaining)))

(defun %zip-entry-data-position (zip entry)
  "Find ENTRY's local payload using its own header, not central-directory lengths."
  (bt:with-lock-held ((gethash zip *zipfile-read-locks*))
    (let ((stream (zip::zipfile-stream zip)))
      (file-position stream (zip::zipfile-entry-offset entry))
      (let ((header (zip::make-local-header stream)))
        (unless (= (zip::file/signature header) #x04034b50)
          (%throw '|java/util/zip/ZipException| "invalid LOC header"))
        (+ (file-position stream) (zip::file/name-length header) (zip::file/extra-length header))))))

(defun %check-zip-read-buffer (array offset length)
  "Check the entire requested range before allocation, I/O, or position changes."
  (unless array (%throw '|java/lang/NullPointerException|))
  (unless (<= 0 offset (+ offset length) (length (java-array-data array)))
    (%throw '|java/lang/IndexOutOfBoundsException|)))

(defun %zip-entry-source-read (source memory length)
  "Read compressed bytes with a bounded pread while excluding archive close."
  (with-slots (zip-file compressed-position remaining closed) source
    (bt:with-recursive-lock-held ((%zipfile-lock zip-file))
      (when closed (%throw '|java/io/IOException| "Stream closed"))
      (when (zerop length) (return-from %zip-entry-source-read 0))
      (when (zerop remaining) (return-from %zip-entry-source-read -1))
      (let ((zip (%zipfile-zip zip-file)))
        (unless zip (%throw '|java/io/IOException| "ZipFile closed"))
        (bt:with-lock-held ((gethash zip *zipfile-read-locks*))
          (let ((fd (sb-sys:fd-stream-fd (zip::zipfile-stream zip))))
            (loop
              (let ((result (%libc "pread" sb-alien:long
                              (sb-alien:int fd) ((* sb-alien:unsigned-char) memory)
                              (sb-alien:unsigned-long (min length remaining))
                              (sb-alien:long compressed-position))))
                (cond
                  ((plusp result)
                   (incf compressed-position result) (decf remaining result)
                   (return result))
                  ((zerop result) (%throw '|java/io/EOFException| "Truncated ZIP entry"))
                  ((/= (sb-alien:get-errno) sb-posix:eintr)
                   (%throw-errno '|java/io/IOException| (sb-alien:get-errno))))))))))))

(defmethod |read()| ((source <zip-entry-source-stream>))
  (let ((memory (sb-alien:make-alien sb-alien:unsigned-char 1)))
    (unwind-protect
         (if (minusp (%zip-entry-source-read source memory 1)) -1 (sb-alien:deref memory 0))
      (sb-alien:free-alien memory))))

(defmethod |read([BII)I| ((source <zip-entry-source-stream>) array offset length)
  (%check-zip-read-buffer array offset length)
  (let ((memory (sb-alien:make-alien sb-alien:unsigned-char (max 1 (min length 65536)))))
    (unwind-protect
         (let ((count (%zip-entry-source-read source memory (min length 65536))))
           (when (plusp count) (%zlib-alien-to-array memory array offset count))
           count)
      (sb-alien:free-alien memory))))

(defmethod |available()I| ((source <zip-entry-source-stream>))
  (if (slot-value source 'closed) 0 (min 2147483647 (slot-value source 'remaining))))
(defmethod |close()| ((source <zip-entry-source-stream>)) (setf (slot-value source 'closed) t))
(defmethod |close()V| ((source <zip-entry-source-stream>)) (|close()| source))

(defun %register-zip-input-stream (zip-file stream)
  "Track an entry weakly; the entry itself retains its owning ZipFile."
  (let ((streams (or (gethash zip-file *zipfile-input-streams*)
                     (setf (gethash zip-file *zipfile-input-streams*)
                           (make-hash-table :test 'eq :weakness :key)))))
    (setf (gethash stream streams) t))
  stream)

(defun %close-zip-input-stream (stream &optional close-owner)
  (with-slots (zip-file delegate inflater closed) stream
    (bt:with-recursive-lock-held ((%zipfile-lock zip-file))
      (unless closed
        (setf closed t)
        (unwind-protect
             (when delegate (|close()| delegate))
          (unwind-protect
               (when inflater (|end()V| inflater))
            (when-let ((streams (gethash zip-file *zipfile-input-streams*)))
              (remhash stream streams))
            (when close-owner (|close()V| zip-file)))))))
  nil)

(defun %zip-stream-own-zipfile (stream)
  (setf (slot-value stream 'close-owner) t)
  stream)

(defun %snapshot-zip-input-stream (stream)
  "Keep the unread bytes of a live entry independently of native inflater state."
  (unless (slot-value stream 'closed)
    (let ((bytes (|readAllBytes()[B| stream)))
      (when-let ((delegate (slot-value stream 'delegate))) (|close()| delegate))
      (when-let ((inflater (slot-value stream 'inflater))) (|end()V| inflater))
      (setf (slot-value stream 'delegate)
            (make-instance '<resource-input-stream> :lisp-stream
              (flexi-streams:make-in-memory-input-stream
                (%convert-to-unsigned-8-bit (java-array-data bytes))))
            (slot-value stream 'inflater) nil
            (slot-value stream 'remaining) (length (java-array-data bytes))))))

(defmethod |getInputStream(Ljava/util/zip/ZipEntry;)Ljava/io/InputStream;|
    ((this |java/util/zip/ZipFile|) zip-entry)
  (unless zip-entry (%throw '|java/lang/NullPointerException|))
  (bt:with-recursive-lock-held ((%zipfile-lock this))
    (let ((zip (%zipfile-zip this)))
      (unless zip (%throw '|java/lang/IllegalStateException| "ZipFile closed"))
      (let ((entry (zip:get-zipfile-entry (lstring (slot-value zip-entry '|name|)) zip)))
        (unless entry (return-from |getInputStream(Ljava/util/zip/ZipEntry;)Ljava/io/InputStream;| nil))
        (let* ((source (make-instance '<zip-entry-source-stream>
                         :zip-file this :compressed-position (%zip-entry-data-position zip entry)
                         :remaining (zip::zipfile-entry-compressed-size entry) :closed nil))
               (method (rest (gethash (zip::zipfile-entry-offset entry)
                                     (gethash zip *zipfile-entry-metadata*))))
               (inflater nil)
               (delegate
                 (case method
                   (0 source)
                   (8 (classload "java/util/zip/Inflater")
                      (classload "java/util/zip/InflaterInputStream")
                      (setf inflater (%make-java-instance "java/util/zip/Inflater"))
                      (|<init>(Z)V| inflater 1)
                      (let ((input (%make-java-instance "java/util/zip/InflaterInputStream")))
                        (|<init>(Ljava/io/InputStream;Ljava/util/zip/Inflater;I)V| input source inflater 8192)
                        input))
                   (t (%throw '|java/util/zip/ZipException| "invalid compression method")))))
          (%register-zip-input-stream this
            (make-instance '<zip-input-stream> :zip-file this :delegate delegate :inflater inflater
                           :close-owner nil :closed nil :remaining (zip::zipfile-entry-size entry))))))))

(defmethod |read()| ((stream <zip-input-stream>))
  (bt:with-recursive-lock-held ((%zipfile-lock (slot-value stream 'zip-file)))
    (when (slot-value stream 'closed) (%throw '|java/io/IOException| "Stream closed"))
    (let ((byte (|read()| (slot-value stream 'delegate))))
      (unless (minusp byte) (decf (slot-value stream 'remaining)))
      byte)))

(defmethod |read([BII)I| ((stream <zip-input-stream>) array offset length)
  (bt:with-recursive-lock-held ((%zipfile-lock (slot-value stream 'zip-file)))
    (when (slot-value stream 'closed) (%throw '|java/io/IOException| "Stream closed"))
    (%check-zip-read-buffer array offset length)
    (let ((count (|read([BII)I| (slot-value stream 'delegate) array offset length)))
      (when (plusp count) (decf (slot-value stream 'remaining) count))
      count)))

(defmethod |available()I| ((stream <zip-input-stream>))
  (if (slot-value stream 'closed) 0 (max 0 (min 2147483647 (slot-value stream 'remaining)))))
(defmethod |close()| ((stream <zip-input-stream>))
  (%close-zip-input-stream stream (slot-value stream 'close-owner)))
(defmethod |close()V| ((stream <zip-input-stream>)) (|close()| stream))

(defmethod |getName()Ljava/lang/String;| ((this |java/util/zip/ZipFile|))
  (%zipfile-name this))

(defun %zip-manifest-count (jar-file)
  "Return the number of META-INF/MANIFEST.MF entries within JAR-FILE."
  (let ((count 0))
    (zip:do-zipfile-entries (entry-name entry (%zipfile-zip jar-file))
      (declare (ignore entry))
      (when (string-equal entry-name "META-INF/MANIFEST.MF")
        (incf count)))
    count))

(defmethod |close()V| ((this |java/util/zip/ZipFile|))
  ;; Never acquire the URL cache lock inside a ZipFile lock: connection
  ;; creation takes them in the opposite order.
  (when (fboundp '%uncache-jar-file) (%uncache-jar-file this))
  (bt:with-recursive-lock-held ((%zipfile-lock this))
    (%cancel-zipfile-cleanup this)
    (unwind-protect
         (when-let ((streams (gethash this *zipfile-input-streams*)))
           (dolist (stream (loop for key being the hash-keys of streams collect key))
             (%close-zip-input-stream stream))
           (remhash this *zipfile-input-streams*))
      (when-let ((state (gethash this *zipfile-state*)))
        (when (rest state)
          (bt:with-lock-held ((gethash (rest state) *zipfile-read-locks*))
            (zip:close-zipfile (rest state))))
        (remhash this *zipfile-state*))))
  nil)

(defvar *crc-table*
  (make-array
   256
   :element-type '(unsigned-byte 32)
   :initial-contents
   '(#x00000000 #x77073096 #xEE0E612C #x990951BA #x076DC419 #x706AF48F
     #xE963A535 #x9E6495A3 #x0EDB8832 #x79DCB8A4 #xE0D5E91E #x97D2D988
     #x09B64C2B #x7EB17CBD #xE7B82D07 #x90BF1D91 #x1DB71064 #x6AB020F2
     #xF3B97148 #x84BE41DE #x1ADAD47D #x6DDDE4EB #xF4D4B551 #x83D385C7
     #x136C9856 #x646BA8C0 #xFD62F97A #x8A65C9EC #x14015C4F #x63066CD9
     #xFA0F3D63 #x8D080DF5 #x3B6E20C8 #x4C69105E #xD56041E4 #xA2677172
     #x3C03E4D1 #x4B04D447 #xD20D85FD #xA50AB56B #x35B5A8FA #x42B2986C
     #xDBBBC9D6 #xACBCF940 #x32D86CE3 #x45DF5C75 #xDCD60DCF #xABD13D59
     #x26D930AC #x51DE003A #xC8D75180 #xBFD06116 #x21B4F4B5 #x56B3C423
     #xCFBA9599 #xB8BDA50F #x2802B89E #x5F058808 #xC60CD9B2 #xB10BE924
     #x2F6F7C87 #x58684C11 #xC1611DAB #xB6662D3D #x76DC4190 #x01DB7106
     #x98D220BC #xEFD5102A #x71B18589 #x06B6B51F #x9FBFE4A5 #xE8B8D433
     #x7807C9A2 #x0F00F934 #x9609A88E #xE10E9818 #x7F6A0DBB #x086D3D2D
     #x91646C97 #xE6635C01 #x6B6B51F4 #x1C6C6162 #x856530D8 #xF262004E
     #x6C0695ED #x1B01A57B #x8208F4C1 #xF50FC457 #x65B0D9C6 #x12B7E950
     #x8BBEB8EA #xFCB9887C #x62DD1DDF #x15DA2D49 #x8CD37CF3 #xFBD44C65
     #x4DB26158 #x3AB551CE #xA3BC0074 #xD4BB30E2 #x4ADFA541 #x3DD895D7
     #xA4D1C46D #xD3D6F4FB #x4369E96A #x346ED9FC #xAD678846 #xDA60B8D0
     #x44042D73 #x33031DE5 #xAA0A4C5F #xDD0D7CC9 #x5005713C #x270241AA
     #xBE0B1010 #xC90C2086 #x5768B525 #x206F85B3 #xB966D409 #xCE61E49F
     #x5EDEF90E #x29D9C998 #xB0D09822 #xC7D7A8B4 #x59B33D17 #x2EB40D81
     #xB7BD5C3B #xC0BA6CAD #xEDB88320 #x9ABFB3B6 #x03B6E20C #x74B1D29A
     #xEAD54739 #x9DD277AF #x04DB2615 #x73DC1683 #xE3630B12 #x94643B84
     #x0D6D6A3E #x7A6A5AA8 #xE40ECF0B #x9309FF9D #x0A00AE27 #x7D079EB1
     #xF00F9344 #x8708A3D2 #x1E01F268 #x6906C2FE #xF762575D #x806567CB
     #x196C3671 #x6E6B06E7 #xFED41B76 #x89D32BE0 #x10DA7A5A #x67DD4ACC
     #xF9B9DF6F #x8EBEEFF9 #x17B7BE43 #x60B08ED5 #xD6D6A3E8 #xA1D1937E
     #x38D8C2C4 #x4FDFF252 #xD1BB67F1 #xA6BC5767 #x3FB506DD #x48B2364B
     #xD80D2BDA #xAF0A1B4C #x36034AF6 #x41047A60 #xDF60EFC3 #xA867DF55
     #x316E8EEF #x4669BE79 #xCB61B38C #xBC66831A #x256FD2A0 #x5268E236
     #xCC0C7795 #xBB0B4703 #x220216B9 #x5505262F #xC5BA3BBE #xB2BD0B28
     #x2BB45A92 #x5CB36A04 #xC2D7FFA7 #xB5D0CF31 #x2CD99E8B #x5BDEAE1D
     #x9B64C2B0 #xEC63F226 #x756AA39C #x026D930A #x9C0906A9 #xEB0E363F
     #x72076785 #x05005713 #x95BF4A82 #xE2B87A14 #x7BB12BAE #x0CB61B38
     #x92D28E9B #xE5D5BE0D #x7CDCEFB7 #x0BDBDF21 #x86D3D2D4 #xF1D4E242
     #x68DDB3F8 #x1FDA836E #x81BE16CD #xF6B9265B #x6FB077E1 #x18B74777
     #x88085AE6 #xFF0F6A70 #x66063BCA #x11010B5C #x8F659EFF #xF862AE69
     #x616BFFD3 #x166CCF45 #xA00AE278 #xD70DD2EE #x4E048354 #x3903B3C2
     #xA7672661 #xD06016F7 #x4969474D #x3E6E77DB #xAED16A4A #xD9D65ADC
     #x40DF0B66 #x37D83BF0 #xA9BCAE53 #xDEBB9EC5 #x47B2CF7F #x30B5FFE9
     #xBDBDF21C #xCABAC28A #x53B39330 #x24B4A3A6 #xBAD03605 #xCDD70693
     #x54DE5729 #x23D967BF #xB3667A2E #xC4614AB8 #x5D681B02 #x2A6F2B94
     #xB40BBE37 #xC30C8EA1 #x5A05DF1B #x2D02EF8D)))

(defun |java/util/zip/CRC32.updateBytes(I[BII)I| (crc array offset length)
  "Update CRC32 value CRC with bytes from ARRAY starting at OFFSET for LENGTH bytes."
  (setf crc (logxor crc #xffffffff))
  (dotimes (n length)
    (let ((i (logand #xff (logxor crc (jaref array (+ offset n))))))
      (setf crc (logxor (aref *crc-table* i) (ash crc -8)))))
  (logxor crc #xfffffff))

;; JarFile.checkForSpecialAttributes reads the manifest through the
;; JavaUtilZipFileAccess bridge ZipFile's class initializer registers, which
;; OpenLDK's ZipFile (initializer stubbed) never does: it threw a
;; NullPointerException where URLClassLoader first opens a jar (the Scala
;; REPL's parent loader).  Scan the manifest here instead.  Multi-release
;; jars are read as plain ones, as OpenLDK's zip methods do.
(defmethod |checkForSpecialAttributes()V| :around ((this |java/util/jar/JarFile|))
  (unless (eql 1 (slot-value this '|hasCheckedSpecialAttributes|))
    (let* ((zip (%zipfile-zip this))
           (entry (and zip (zip:get-zipfile-entry "META-INF/MANIFEST.MF" zip)))
           (text (and entry (flexi-streams:octets-to-string (%java-zip-entry-contents zip entry)
                                                            :external-format :latin-1))))
      ;; Normalize line endings on a copy: the ZIP data may be shared.
      (setf (slot-value this '|hasClassPathAttribute|)
            (if (and text (search (format nil "~%Class-Path:") (substitute #\Newline #\Return text) ; lint:suppress substitute-use
                                  :test #'char-equal))
                1 0))
      (setf (slot-value this '|hasCheckedSpecialAttributes|) 1))))

;; JarFile.getManifest finds the manifest through the same bridge (see
;; checkForSpecialAttributes above); read it from the zip here.
(defmethod |getManifest()Ljava/util/jar/Manifest;| :around ((this |java/util/jar/JarFile|))
  (when-let ((reference (slot-value this '|manRef|)))
    (when-let ((manifest (|get()Ljava/lang/Object;| reference)))
      (return-from |getManifest()Ljava/util/jar/Manifest;| manifest)))
  (let* ((zip (%zipfile-zip this))
         (entry (and zip (zip:get-zipfile-entry "META-INF/MANIFEST.MF" zip))))
    (when entry
      (let ((bytes (map 'vector (lambda (b) (if (> b 127) (- b 256) b))
                        (%java-zip-entry-contents zip entry)))
            (stream (%make-java-instance "java/io/ByteArrayInputStream"))
            (manifest (%make-java-instance "java/util/jar/Manifest")))
        (|<init>([B)V| stream (make-java-array :component-class (%get-java-class-by-bin-name "byte")
                                               :initial-contents bytes))
        (|<init>(Ljava/io/InputStream;)V| manifest stream)
        (let ((reference (%make-java-instance "java/lang/ref/SoftReference")))
          (|<init>(Ljava/lang/Object;)V| reference manifest)
          (setf (slot-value this '|manRef|) reference))
        manifest))))
