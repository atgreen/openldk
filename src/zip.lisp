;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: OPENLDK; Base: 10 -*-
;;;
;;; Copyright (C) 2025  Anthony Green <green@moxielogic.com>
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
  ())

(defclass |java/util/jar/JarFile| ()
  ())

(defun |java/util/zip/ZipFile.<clinit>()| ()
  )

(defun |<init>| ()
  )

(defmethod |<init>(Ljava/io/File;I)| ((this |java/util/zip/ZipFile|) file mode)
  (assert (eq mode 1)) ; only support read for now
  (with-slots (|name| |jzfile|) this
    (setf |name| (|getPath()| file))
    (setf |jzfile| (zip:open-zipfile (lstring |name|)))))

(defmethod |<init>(Ljava/io/File;)| ((this |java/util/zip/ZipFile|) file)
  (|<init>(Ljava/io/File;I)| this file 1))

(defmethod |getEntry(Ljava/lang/String;)| ((this |java/util/zip/ZipFile|) name)
  (let ((ze (zip:get-zipfile-entry (lstring name) (slot-value this '|jzfile|))))
    (when ze
      (let ((entry (make-instance '|java/util/zip/ZipEntry|)))
        (|<init>(Ljava/lang/String;)| entry name)
        entry))))

(defmethod |getMetaInfEntryNames()| ((this |java/util/jar/JarFile|))
  (with-slots (|jzfile|) this
    (let ((meta-entries (list)))
      (zip:do-zipfile-entries (name entry |jzfile|)
        (when (and (< 9 (length name))
                   (string= (subseq name 0 9) "META-INF/"))
          (push (jstring name) meta-entries)))
      (coerce meta-entries 'vector))))

(defclass/std <zip-input-stream> (|java/io/InputStream|)
  ((zip-file)
   (entry)
   (buffer)
   (index)))

(defmethod |getInputStream(Ljava/util/zip/ZipEntry;)| ((this |java/util/zip/ZipFile|) zip-entry)
  (make-instance '<zip-input-stream> :zip-file this :entry zip-entry))

(defmethod |read()| ((this <zip-input-stream>))
  (with-slots (zip-file entry buffer index) this
    (unless buffer
      (setf buffer (zip:zipfile-entry-contents (zip:get-zipfile-entry (lstring (slot-value entry '|name|))
                                                                      (slot-value zip-file '|jzfile|))))
      (setf index -1))
    (if (< index (1- (length buffer)))
        (aref buffer (incf index))
        -1)))

(defmethod |getName()| ((this |java/util/zip/ZipFile|))
  (slot-value this '|name|))

(defmethod |close()| ((this |java/util/zip/ZipFile|))
  (with-slots (|name| |jzfile|) this
    (zip:close-zipfile |jzfile|)))
