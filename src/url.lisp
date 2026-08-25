;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: OPENLDK; Base: 10 -*-
;;;
;;; Copyright (C) 2025  Anthony Green <green@moxielogic.com>
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

;;; As a special exception, the copyright holders of the library give
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

;;; OpenLDK provides native implementations for URL-related classes
;;; to handle jar: and file: URLs for resource loading.
;;;
;;; RESOURCE LOADING ARCHITECTURE
;;; =============================
;;;
;;; Problem: Java's ClassLoader.getResource() and getResourceAsStream() weren't
;;; finding resources (like .clj files) in JAR files at runtime. This happened
;;; because:
;;;
;;; 1. The AppClassLoader's URLClassPath is initialized at BUILD TIME when
;;;    sun.misc.Launcher is created during OpenLDK image generation.
;;;
;;; 2. At build time, java.class.path doesn't include runtime classpath entries
;;;    (like user JAR files passed via LDK_CLASSPATH).
;;;
;;; 3. At runtime, even though java.class.path is updated correctly, the
;;;    AppClassLoader still uses the old URLClassPath from build time.
;;;
;;; Solution: We bypass Java's broken URLClassPath by:
;;;
;;; 1. Implementing resource lookup methods in classpath.lisp that search
;;;    OpenLDK's *classpath* variable (which IS correctly set at runtime).
;;;
;;; 2. Using a CLOS :around method on URLClassLoader.findResource to intercept
;;;    ALL resource lookups. The :around method is crucial because:
;;;
;;;    - When Java classes are JIT-compiled, OpenLDK generates defmethod forms
;;;      that define the compiled methods as primary methods.
;;;
;;;    - In CLOS, :around methods ALWAYS wrap primary methods and are called
;;;      first, regardless of what primary methods are defined later.
;;;
;;;    - This means our :around method persists even after URLClassLoader is
;;;      JIT-compiled, ensuring resource lookups always use our native
;;;      implementation.
;;;
;;; 3. Implementing URL.openStream() to handle jar: and file: URLs, allowing
;;;    resources found by our lookup to be read.
;;;
;;; The flow is:
;;;   ClassLoader.getResource(name)
;;;   -> URLClassLoader.findResource(name)  [our :around method intercepts]
;;;   -> get-resource-url-on-classpath(name)  [searches *classpath*]
;;;   -> returns URL or falls through to Java implementation

(in-package :openldk)

;; Stub class for java.net.URL - gets redefined when the classfile is read
(defclass |java/net/URL| ()
  ()
  (:documentation "Stub for java.net.URL; populated at runtime."))

;; Stub class for java.net.URLClassLoader - gets redefined when the classfile is read
(defclass |java/net/URLClassLoader| ()
  ()
  (:documentation "Stub for java.net.URLClassLoader; populated at runtime."))

;; Override URLClassLoader.findResource to use our native classpath
;; This is needed because the AppClassLoader's URLClassPath is initialized
;; at build time with URLs that don't match the runtime classpath.
;; Using :around method to ensure this is always called, even after JIT compilation.
(defmethod |findResource(Ljava/lang/String;)| :around ((loader |java/net/URLClassLoader|) name)
  "Find a resource by NAME using our native classpath implementation."
  (let ((resource-name (lstring name)))
    (or (when-let (url-string (get-resource-url-on-classpath resource-name))
          (%make-url-from-string url-string))
        (call-next-method))))

;; Override URLClassLoader.findClass to use OpenLDK's native class loading.
;; This is needed because URLClassLoader.findClass uses URLClassPath.getResource()
;; internally, which has the same build-time URL issue as findResource.
(defmethod |findClass(Ljava/lang/String;)| :around ((loader |java/net/URLClassLoader|) name)
  "Find and define a class by NAME using OpenLDK's native classpath with this loader."
  (let* ((class-name (lstring name))
         (bin-name (substitute #\/ #\. class-name)))
    ;; Try to load using OpenLDK's native class loading, passing this loader
    (or (handler-case
            (let ((klass (classload bin-name loader)))
              (when klass
                (java-class klass)))
          (condition () nil))
        ;; Fall back to Java implementation
        (call-next-method))))

(defun %open-url-stream-1 (url)
  "Open an InputStream for URL when its protocol is one we serve natively
(jar:, file:, jrt:).  Returns the stream, NIL for a servable protocol whose
target does not exist, or :FALLTHROUGH for other protocols."
  (let ((url-string (or (gethash url *native-url-strings*)
                        (lstring (|toString()| url)))))
    (cond
      ;; Handle jar: URLs
      ((starts-with? "jar:" url-string)
       (when-let (stream (%open-jar-url-stream url-string))
         (make-instance '<resource-input-stream> :lisp-stream stream)))
      ;; Handle file: URLs
      ((starts-with? "file:" url-string)
       (let ((path (subseq url-string 5)))  ; Skip "file:"
         (when (uiop:file-exists-p path)
           (make-instance '<resource-input-stream>
                          :lisp-stream (flexi-streams:make-in-memory-input-stream
                                        (read-file-into-byte-vector path))))))
      ;; Handle jrt: URLs (JDK jimage resources: jrt:/<module>/<resource>)
      ((starts-with? "jrt:" url-string)
       (let* ((rest (subseq url-string 4))            ; drop "jrt:"
              (rest (string-left-trim "/" rest))      ; drop leading slash
              (slash (position #\/ rest)))            ; split off <module>
         (when slash
           (when-let (stream (open-resource-on-classpath (subseq rest (1+ slash))))
             (make-instance '<resource-input-stream> :lisp-stream stream)))))
      (t :fallthrough))))

;; The :around survives JIT compilation of java.net.URL — a compiled Java
;; openStream() installs a primary method on this same GF and would clobber
;; a primary-only Lisp override (it then NPEs in openConnection() on our
;; handler-less synthetic jar:/jrt: URLs).  The fallback PRIMARY below is
;; still required: standard method combination signals NO-PRIMARY-METHOD if
;; only an :around is applicable, which is exactly the situation before
;; java.net.URL has been JIT-compiled.
(defmethod |openStream()| :around ((url |java/net/URL|))
  "Open an InputStream for the URL."
  (let ((stream (%open-url-stream-1 url)))
    (if (eq stream :fallthrough)
        (call-next-method)
        stream)))

(defmethod |openStream()| ((url |java/net/URL|))
  "Fallback primary for URLs of protocols we don't serve, dispatched before
java.net.URL's bytecode is compiled.  The compiled Java method replaces
this; the :around above still intercepts our native protocols."
  (let ((stream (%open-url-stream-1 url)))
    (if (eq stream :fallthrough)
        nil
        stream)))

(defmethod |toString()| :around ((url |java/net/URL|))
  "Return the original spelling of an OpenLDK-constructed resource URL."
  (if-let ((url-string (gethash url *native-url-strings*)))
    (jstring url-string)
    (call-next-method)))

(defmethod |getProtocol()| :around ((url |java/net/URL|))
  "Protocol (scheme) of an OpenLDK-constructed resource URL: the text before
the first ':' (jar, file, jrt)."
  (if-let ((url-string (gethash url *native-url-strings*)))
    (jstring (subseq url-string 0 (position #\: url-string)))
    (call-next-method)))

(defmethod |getProtocol()| ((url |java/net/URL|))
  "Fallback primary (before java.net.URL's bytecode is compiled): read the
protocol field, or parse it out of the URL's string form."
  (if (and (slot-exists-p url '|protocol|)
           (slot-boundp url '|protocol|)
           (slot-value url '|protocol|))
      (slot-value url '|protocol|)
      (let ((s (lstring (|toString()| url))))
        (jstring (subseq s 0 (position #\: s))))))

;;; URL.openConnection() for our synthetic jar:/file:/jrt: resource URLs.
;;; Clojure's clojure.lang.RT.lastModified() calls url.openConnection() (and,
;;; for jar URLs, casts to JarURLConnection and walks getJarFile().getEntry().
;;; getTime()); it must succeed for RT.<clinit> to complete (ldk-uyv).  We
;;; return a synthetic URLConnection (a JarURLConnection for jar: so the cast
;;; holds) and record its URL; the getInputStream()/getLastModified()/
;;; getJarFile() methods below serve it from OpenLDK's zip infrastructure.

;; Stub CLOS classes so the methods below can specialize before the JDK's
;; java.net.URLConnection / java.net.JarURLConnection are JIT-loaded (they are
;; merged with the real classes at load time, as with java/net/URL above).
(defclass |java/net/URLConnection| ()
  ()
  (:documentation "Stub for java.net.URLConnection; populated at runtime."))

(defclass |java/net/JarURLConnection| (|java/net/URLConnection|)
  ()
  (:documentation "Stub for java.net.JarURLConnection; populated at runtime."))

;; url.lisp loads before zip.lisp, so forward-declare the jar/zip stubs the
;; connection methods below specialize on (harmlessly redefined in zip.lisp).
(defclass |java/util/jar/JarFile| ()
  ()
  (:documentation "Stub for java.util.jar.JarFile; populated at runtime."))

(defclass |java/util/zip/ZipEntry| ()
  ()
  (:documentation "Stub for java.util.zip.ZipEntry; populated at runtime."))

(defvar *connection-urls* (make-hash-table :test #'eq :synchronized t)
  "Synthetic URLConnection -> its java.net.URL.")

(defun %url-string-of (url)
  (or (gethash url *native-url-strings*) (lstring (|toString()| url))))

(defun %open-url-connection-1 (url)
  "A synthetic URLConnection for URLs whose protocol we serve, else :FALLTHROUGH."
  (let ((url-string (%url-string-of url)))
    (cond
      ((starts-with? "jar:" url-string)
       (let ((conn (%make-java-instance "java/net/JarURLConnection")))
         (setf (gethash conn *connection-urls*) url)
         conn))
      ((or (starts-with? "file:" url-string) (starts-with? "jrt:" url-string))
       (let ((conn (%make-java-instance "java/net/JarURLConnection")))
         (setf (gethash conn *connection-urls*) url)
         conn))
      (t :fallthrough))))

(defmethod |openConnection()| :around ((url |java/net/URL|))
  (let ((conn (%open-url-connection-1 url)))
    (if (eq conn :fallthrough) (call-next-method) conn)))

(defmethod |openConnection()| ((url |java/net/URL|))
  (let ((conn (%open-url-connection-1 url)))
    (if (eq conn :fallthrough) nil conn)))

;; URLConnection's stream/metadata methods are abstract in java.net, so these
;; primary methods (no JDK bytecode to clobber them) serve our synthetic
;; connections; they no-op to defaults for any connection we didn't create.
;; :around so the compiled java.net.URLConnection.getInputStream() (which
;; throws UnknownServiceException by default) doesn't clobber us; we serve our
;; synthetic connections and defer to that default for any other connection.
(defmethod |getInputStream()| :around ((conn |java/net/URLConnection|))
  (if-let ((url (gethash conn *connection-urls*)))
    (let ((s (%open-url-stream-1 url)))
      (if (eq s :fallthrough) (call-next-method) s))
    (call-next-method)))

(defmethod |getLastModified()| ((conn |java/net/URLConnection|))
  (declare (ignore conn))
  0)

;; The JDK ZipFile/JarFile/ZipEntry stubs carry no CLOS slots (OpenLDK reads
;; jars through its own Lisp zip library, not these classes), so we attach the
;; open zip and per-entry info via side tables instead of object fields.  This
;; supports RT.lastModified()'s getJarFile().getEntry(name).getTime() walk.
(defvar *jarfile-zips* (make-hash-table :test #'eq :synchronized t)
  "java.util.jar.JarFile -> its open zip:zipfile.")
(defvar *zipentry-times* (make-hash-table :test #'eq :synchronized t)
  "java.util.zip.ZipEntry -> its last-modified time (millis).")

(defmethod |getJarFile()| ((conn |java/net/JarURLConnection|))
  (when-let ((url (gethash conn *connection-urls*)))
    (multiple-value-bind (jar-path entry-path) (%parse-jar-url (%url-string-of url))
      (declare (ignore entry-path))
      (when jar-path
        (let ((jf (%make-java-instance "java/util/jar/JarFile")))
          (setf (gethash jf *jarfile-zips*) (zip:open-zipfile jar-path))
          jf)))))

(defmethod |getEntry(Ljava/lang/String;)| ((this |java/util/jar/JarFile|) name)
  (if-let ((zf (gethash this *jarfile-zips*)))
    (when (zip:get-zipfile-entry (lstring name) zf)
      (let ((entry (%make-java-instance "java/util/zip/ZipEntry")))
        ;; 0 = "very old", so callers reload rather than trust a stale class.
        (setf (gethash entry *zipentry-times*) 0)
        entry))
    (call-next-method)))

(defmethod |getTime()| ((this |java/util/zip/ZipEntry|))
  (multiple-value-bind (time present) (gethash this *zipentry-times*)
    (if present time -1)))
