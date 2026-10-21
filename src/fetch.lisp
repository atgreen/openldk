;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: OPENLDK; Base: 10 -*-
;;;
;;; Copyright (C) 2026  Anthony Green <green@moxielogic.com>
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

;;; Downloading resources the host doesn't provide.
;;;
;;; When JAVA_HOME is unset, or a language's jars aren't on the classpath,
;;; OpenLDK fetches pinned, checksummed copies into a per-user cache and
;;; uses those.  This happens at run time, not when the system is built:
;;; a saved image may run on another machine, so the cache is located
;;; afresh on every start.
;;;
;;;   LDK_CACHE_DIR  cache location (default $XDG_CACHE_HOME/openldk/)
;;;   LDK_OFFLINE    when set (and not "0"), never download

(define-condition fetch-error (openldk-error)
  ()
  (:documentation "A resource OpenLDK needs could not be downloaded."))

(defun fetch-error (format-control &rest format-arguments)
  (error 'fetch-error :format-control format-control :format-arguments format-arguments))

(defun ldk-cache-dir ()
  "The directory OpenLDK keeps downloaded resources in."
  (uiop:ensure-directory-pathname
   (or (uiop:getenvp "LDK_CACHE_DIR")
       (uiop:xdg-cache-home "openldk/"))))

(defun ldk-offline-p ()
  (let ((offline (uiop:getenvp "LDK_OFFLINE")))
    (and offline (string/= offline "0"))))

(defun %temporary-name (pathname)
  "A sibling of PATHNAME to build it in before renaming it into place."
  (format nil "~A.~36R.tmp"
          (string-right-trim "/" (uiop:native-namestring pathname))
          (random (expt 36 8) (make-random-state t))))

(defun %run-tool (command)
  "Run COMMAND, a list of program and arguments, returning its output.
Failure -- including the program not being installed -- is a FETCH-ERROR."
  (multiple-value-bind (output error-output status)
      (handler-case
          (uiop:run-program command :output :string :error-output :string
                                    :ignore-error-status t)
        (error (e)
          (fetch-error "Could not run `~A' (is it installed?):~%  ~A" (first command) e)))
    (unless (zerop status)
      (fetch-error "`~{~A~^ ~}' failed:~%  ~A" command
                   (string-trim '(#\Newline #\Space) error-output)))
    output))

(defun %file-sha256 (file)
  (let ((output (handler-case
                    (%run-tool (list "sha256sum" (uiop:native-namestring file)))
                  (fetch-error ()
                    (%run-tool (list "shasum" "-a" "256" (uiop:native-namestring file)))))))
    (string-downcase (subseq output 0 (position #\Space output)))))

(defun fetch-url (url destination &key sha256)
  "Download URL to the file DESTINATION, checking its SHA256 if given.
The file only appears at DESTINATION once complete and verified."
  (when (ldk-offline-p)
    (fetch-error "LDK_OFFLINE is set; not downloading ~A" url))
  (ensure-directories-exist destination)
  (let ((temporary (%temporary-name destination)))
    (format *error-output* "~&; OpenLDK: downloading ~A~%;   to ~A~%"
            url (uiop:native-namestring destination))
    (finish-output *error-output*)
    (unwind-protect
         (progn
           (%run-tool (list "curl" "-sSfL" "--retry" "3" "-o" temporary url))
           (when sha256
             (let ((actual (%file-sha256 temporary)))
               (unless (string= actual (string-downcase sha256))
                 (fetch-error "Checksum mismatch for ~A:~%  expected ~A~%  got      ~A"
                              url sha256 actual))))
           (sb-posix:rename temporary (uiop:native-namestring destination)))
      (when (probe-file temporary)
        (delete-file temporary))))
  destination)

(defun fetch-tarball (url directory &key sha256 (strip-components 1) exclude)
  "Download the gzipped tarball at URL and unpack it as DIRECTORY,
dropping members that match the glob patterns in EXCLUDE.  DIRECTORY
only appears once fully unpacked, so a partial one is never used."
  (let* ((target (string-right-trim "/" (uiop:native-namestring directory)))
         (tarball (concatenate 'string (%temporary-name directory) ".tar.gz"))
         (unpacked (%temporary-name directory)))
    (unwind-protect
         (progn
           (fetch-url url tarball :sha256 sha256)
           (ensure-directories-exist (uiop:ensure-directory-pathname unpacked))
           (%run-tool (append (list "tar" "xzf" tarball "-C" unpacked
                                    (format nil "--strip-components=~D" strip-components))
                              (mapcar (lambda (pattern) (format nil "--exclude=~A" pattern))
                                      exclude)))
           ;; Another process may have got there first; theirs is as good.
           (unless (uiop:directory-exists-p directory)
             (sb-posix:rename unpacked target)))
      (when (probe-file tarball)
        (delete-file tarball))
      (when (uiop:directory-exists-p unpacked)
        (uiop:delete-directory-tree (uiop:ensure-directory-pathname unpacked)
                                    :validate t))))
  directory)

;;; The JDK

(defparameter *jdk-release* "jdk-25.0.4.1+1"
  "The Temurin JDK 25 release fetched when JAVA_HOME is unset.")

(defparameter *jdk-downloads*
  '((:linux-x64
     "https://github.com/adoptium/temurin25-binaries/releases/download/jdk-25.0.4.1%2B1/OpenJDK25U-jdk_x64_linux_hotspot_25.0.4.1_1.tar.gz"
     "dbb698396d478e7fa2b1e50f4103324b2a99b90569ee27c33f2261f9215cf41e")
    (:linux-aarch64
     "https://github.com/adoptium/temurin25-binaries/releases/download/jdk-25.0.4.1%2B1/OpenJDK25U-jdk_aarch64_linux_hotspot_25.0.4.1_1.tar.gz"
     "69df11a02cfa3ef7d7ca645e03edce6778ec090e100f6ae2b42097865730ac52"))
  "Per platform: the URL and SHA256 of the *JDK-RELEASE* tarball.")

(defun %jdk-platform ()
  #+(and linux x86-64) :linux-x64
  #+(and linux arm64) :linux-aarch64
  #-(and linux (or x86-64 arm64)) nil)

(defun cached-jdk-home ()
  "Where the downloaded JDK lives (whether or not it's there yet)."
  (merge-pathnames (format nil "jdk/~A/" *jdk-release*) (ldk-cache-dir)))

(defun ensure-cached-jdk ()
  "Return the home directory of the cached JDK, downloading it first if needed."
  (let ((home (cached-jdk-home)))
    (unless (uiop:file-exists-p (merge-pathnames "lib/modules" home))
      (let ((download (assoc (%jdk-platform) *jdk-downloads*)))
        (unless download
          (fetch-error "No JDK download is known for this platform"))
        (destructuring-bind (url sha256) (rest download)
          ;; OpenLDK reads the class library from lib/modules; the jmods
          ;; and sources would only take up space.
          (fetch-tarball url home :sha256 sha256
                                  :exclude '("*/jmods" "*/lib/src.zip" "*/man" "*/demo")))))
    home))
