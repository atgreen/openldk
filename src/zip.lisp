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

(defclass |java/util/jar/JarInputStream| ()
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
  ; (format t "GET-ENTRY: ~A in ~A~%" (lstring name) (slot-value this '|jzfile|))
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
      (make-java-array
       :component-class (%get-java-class-by-bin-name "java/lang/String")
       :initial-contents meta-entries))))

(defmethod |<init>(Ljava/io/InputStream;Z)| ((this |java/util/jar/JarInputStream|) is verify)
  (with-slots (|in|) this
    (setf |in| (zip::open-zipfile-from-stream (make-instance '<java-input-stream> :java-stream is)))))

(defmethod |close()| ((this |java/util/jar/JarInputStream|))
  (zip:close-zipfile (slot-value this '|in|)))

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

(defclass/std <buffer-input-stream> (|java/io/InputStream|)
  ((buf)
   (pos :std 0)))

(defmethod |read()| ((bis <buffer-input-stream>))
  (with-slots (pos buf) bis
    (if (eq pos (length buf))
        -1
        (let ((byte (aref buf pos)))
          (incf (slot-value bis 'pos))
          byte))))

(defmethod |readBytes([BII)| ((bis <buffer-input-stream>) byte-array offset length)
  (let ((buf (slot-value bis 'buf))
        (bytes-read 0))
    (with-slots (pos) bis
      (loop for i from offset below (+ offset length)
            for byte = (aref buf pos)
            do (progn
                 (incf pos)
                 (incf bytes-read)
                 (setf (jaref byte-array (+ offset pos)) byte)))
      bytes-read)))

(defmethod |getManifest()| ((this |java/util/jar/JarInputStream|))
  (classload "java/util/jar/Manifest")
  (let ((buffer (zip:zipfile-entry-contents (zip:get-zipfile-entry "META-INF/MANIFEST.MF"
                                                                   (slot-value this '|in|))))
        (manifest (make-instance '|java/util/jar/Manifest|)))
    (|<init>()| manifest)
    (setf (slot-value this '|man|) manifest)
    (|read(Ljava/io/InputStream;)| manifest (make-instance '<buffer-input-stream> :buf buffer))))

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

(defun |java/util/zip/CRC32.updateBytes(I[BII)| (crc array offset length)
  (setf crc (logxor crc #xffffffff))
  (dotimes (n length)
    (let ((i (logand #xff (logxor crc (jaref array (+ offset n))))))
      (setf crc (logxor (aref *crc-table* i) (ash crc -8)))))
  (logxor crc #xfffffff))
