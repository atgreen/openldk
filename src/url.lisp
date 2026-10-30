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
;;; 3. Implementing URL.openStream() for native jar:/jrt: resources, while
;;;    file: URLs use the JDK's Java protocol handler and streaming I/O.
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
(defmethod |findResource(Ljava/lang/String;)Ljava/net/URL;| :around ((loader |java/net/URLClassLoader|) name)
  "Find a resource by NAME using our native classpath implementation."
  (let ((resource-name (lstring name)))
    (or (when (eq loader *boot-class-loader*)
          (when-let (url-string (get-resource-url-on-classpath resource-name))
            (%make-url-from-string url-string)))
        (call-next-method))))

;; URLClassLoader.findClass must use its own URLClassPath. Falling back to
;; the application's classpath here defines classes in the wrong loader and
;; makes unrelated URLClassLoaders share classes. Resource streams and ZIP
;; metadata are supported by the native bridges below and in zip.lisp.

(defun %open-url-stream-1 (url)
  "Open an InputStream for URL when its protocol is one we serve natively
(jar:, jrt:).  Returns the stream, NIL for a servable protocol whose
target does not exist, or :FALLTHROUGH for other protocols."
  (let ((url-string (or (gethash url *native-url-strings*)
                        (lstring (|toString()Ljava/lang/String;| url)))))
    (cond
      ;; Handle jar: URLs
      ((starts-with? "jar:" url-string)
       (%jar-connection-input-stream (%open-url-connection-1 url)))
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
;; a primary-only Lisp override for native JAR caching and JRT resources.
;; File URLs use their actual Java protocol handler. The fallback PRIMARY is
;; still required: standard method combination signals NO-PRIMARY-METHOD if
;; only an :around is applicable, which is exactly the situation before
;; java.net.URL has been JIT-compiled.
(defmethod |openStream()Ljava/io/InputStream;| :around ((url |java/net/URL|))
  "Open an InputStream for the URL."
  (let ((stream (%open-url-stream-1 url)))
    (case stream
      (:fallthrough (call-next-method))
      ((nil) (%throw-file-not-found url))
      (t stream))))

(defmethod |openStream()Ljava/io/InputStream;| ((url |java/net/URL|))
  "Fallback primary for URLs of protocols we don't serve, dispatched before
java.net.URL's bytecode is compiled.  The compiled Java method replaces
this; the :around above still intercepts our native protocols."
  (let ((stream (%open-url-stream-1 url)))
    (if (eq stream :fallthrough)
        nil
        stream)))

(defmethod |toString()Ljava/lang/String;| :around ((url |java/net/URL|))
  "Return the original spelling of an OpenLDK-constructed resource URL."
  (if-let ((url-string (gethash url *native-url-strings*)))
    (jstring url-string)
    (call-next-method)))

(defmethod |getProtocol()Ljava/lang/String;| :around ((url |java/net/URL|))
  "Protocol (scheme) of an OpenLDK-constructed resource URL: the text before
the first ':' (jar, file, jrt)."
  (if-let ((url-string (gethash url *native-url-strings*)))
    (jstring (subseq url-string 0 (position #\: url-string)))
    (call-next-method)))

(defmethod |getProtocol()Ljava/lang/String;| ((url |java/net/URL|))
  "Fallback primary (before java.net.URL's bytecode is compiled): read the
protocol field, or parse it out of the URL's string form."
  (if (and (slot-exists-p url '|protocol|)
           (slot-boundp url '|protocol|)
           (slot-value url '|protocol|))
      (slot-value url '|protocol|)
      (let ((s (lstring (|toString()Ljava/lang/String;| url))))
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

(defvar *connection-urls* (make-hash-table :test #'eq :weakness :key :synchronized t)
  "Synthetic URLConnection -> its java.net.URL.")

(defvar *connection-use-caches* (make-hash-table :test 'eq :weakness :key :synchronized t))
(defvar *connection-jar-files* (make-hash-table :test 'eq :weakness :key :synchronized t))
(defvar *connection-jar-entries* (make-hash-table :test 'eq :weakness :key :synchronized t))
(defvar *jar-file-cache* (make-hash-table :test 'equal :synchronized t))
(defvar *cached-jar-paths* (make-hash-table :test 'eq :weakness :key :synchronized t))
(defvar *jar-file-cache-lock* (bt:make-recursive-lock "JarURLConnection cache"))

(defun %default-url-use-caches (protocol)
  "Read the JDK's actual global/per-protocol defaults, including Java setters."
  (let ((class (classload "java/net/URLConnection")))
    (%clinit class)
    (not (zerop
          (funcall (static-method-symbol
                    "java/net/URLConnection.getDefaultUseCaches(Ljava/lang/String;)Z"
                    (ldk-class-package class)) (jstring protocol))))))

(defun %initialize-synthetic-connection (conn url protocol)
  (let ((use-caches (%default-url-use-caches protocol)))
    (setf (gethash conn *connection-urls*) url
          (gethash conn *connection-use-caches*) use-caches)
    (when (slot-exists-p conn '|url|) (setf (slot-value conn '|url|) url))
    (when (slot-exists-p conn '|useCaches|)
      (setf (slot-value conn '|useCaches|) (if use-caches 1 0))))
  conn)

(defun %connection-connected-p (conn)
  (and (slot-exists-p conn '|connected|) (slot-boundp conn '|connected|)
       (not (zerop (slot-value conn '|connected|)))))

(defun %mark-connection-connected (conn)
  (when (slot-exists-p conn '|connected|) (setf (slot-value conn '|connected|) 1)))

(defun %url-string-of (url)
  (or (gethash url *native-url-strings*) (lstring (|toString()Ljava/lang/String;| url))))

(defun %set-jar-connection-fields (conn url url-string)
  "Fill the fields JarURLConnection's constructor would, which its
getJarFileURL, getEntryName and getJarEntry read (Spring's classpath
scanning walks a jar from its directory's URL this way)."
  (flet ((put (slot value)
           (when (slot-exists-p conn slot)
             (setf (slot-value conn slot) value))))
    (put '|url| url)
    (multiple-value-bind (jar-path entry-path) (%parse-jar-url url-string)
      (when jar-path
        (put '|jarFileURL| (%make-url-from-string
                             (subseq url-string 4 (search "!/" url-string))))
        (put '|entryName| (when (plusp (length entry-path))
                            (jstring entry-path)))))))

(defun %open-url-connection-1 (url)
  "A synthetic URLConnection for URLs whose protocol we serve, else :FALLTHROUGH."
  (let ((url-string (%url-string-of url)))
    (cond
      ((starts-with? "jar:" url-string)
       (let ((conn (%make-java-instance "java/net/JarURLConnection")))
         (%initialize-synthetic-connection conn url "jar")
         (%set-jar-connection-fields conn url url-string)
         conn))
      ((starts-with? "jrt:" url-string)
       (let ((conn (%make-java-instance "java/net/JarURLConnection")))
         (%initialize-synthetic-connection conn url (subseq url-string 0 (position #\: url-string)))
         conn))
      (t :fallthrough))))

(defmethod |openConnection()Ljava/net/URLConnection;| :around ((url |java/net/URL|))
  (let ((conn (%open-url-connection-1 url)))
    (if (eq conn :fallthrough) (call-next-method) conn)))

(defmethod |openConnection()Ljava/net/URLConnection;| ((url |java/net/URL|))
  (let ((conn (%open-url-connection-1 url)))
    (if (eq conn :fallthrough) nil conn)))

;; URLConnection's stream/metadata methods are abstract in java.net, so these
;; primary methods (no JDK bytecode to clobber them) serve our synthetic
;; connections; they no-op to defaults for any connection we didn't create.
;; :around so the compiled java.net.URLConnection.getInputStream() (which
;; throws UnknownServiceException by default) doesn't clobber us; we serve our
;; synthetic connections and defer to that default for any other connection.
(defmethod |getInputStream()Ljava/io/InputStream;| :around ((conn |java/net/URLConnection|))
  (if-let ((url (gethash conn *connection-urls*)))
    (if (starts-with? "jar:" (%url-string-of url))
        (%jar-connection-input-stream conn)
        (let ((stream (%open-url-stream-1 url)))
          (case stream
            (:fallthrough (call-next-method))
            ((nil) (%throw-file-not-found url))
            (t (%mark-connection-connected conn) stream))))
    (call-next-method)))

(defun %throw-file-not-found (url)
  "Throw java.io.FileNotFoundException for URL, whose target doesn't exist,
as a JVM's connection does: URLClassPath takes a null stream for a class
that is there (the Scala REPL's file://_VIRTUAL_/ classpath entry)."
  (let ((exception (%make-java-instance "java/io/FileNotFoundException")))
    (|<init>(Ljava/lang/String;)V| exception (jstring (%url-string-of url)))
    (error (%lisp-condition exception))))

(defmethod |getInputStream()Ljava/io/InputStream;| ((conn |java/net/URLConnection|))
  "Fallback primary, before java.net.URLConnection is loaded: standard method
combination signals NO-PRIMARY-METHOD if only the :around above applies."
  (when-let ((url (gethash conn *connection-urls*)))
    (let ((s (%open-url-stream-1 url)))
      (unless (eq s :fallthrough) s))))

(defmethod |getLastModified()J| ((conn |java/net/URLConnection|))
  (declare (ignore conn))
  0)

;; The rest of what callers configure or ask a connection before reading it
;; (Groovy's class-file reader turns caching off).  The synthetic connection
;; is an instance of the stub class above until java.net.URLConnection is
;; loaded, whose methods then replace these.
(defmethod |setUseCaches(Z)V| :around ((conn |java/net/URLConnection|) use-caches)
  (if (gethash conn *connection-urls*)
      (progn
        (when (%connection-connected-p conn)
          (%throw '|java/lang/IllegalStateException| "Already connected"))
        (setf (gethash conn *connection-use-caches*) (not (zerop use-caches)))
        (when (slot-exists-p conn '|useCaches|) (setf (slot-value conn '|useCaches|) use-caches))
        nil)
      (call-next-method)))

(defmethod |getUseCaches()Z| :around ((conn |java/net/URLConnection|))
  (if (gethash conn *connection-urls*)
      (if (gethash conn *connection-use-caches*) 1 0)
      (call-next-method)))

(defmethod |connect()V| :around ((conn |java/net/URLConnection|))
  (if-let ((url (gethash conn *connection-urls*)))
      (if (starts-with? "jar:" (%url-string-of url))
          (progn (%connect-jar-connection conn) nil)
          (%mark-connection-connected conn))
      (call-next-method)))

(defmethod |setConnectTimeout(I)V| ((conn |java/net/URLConnection|) timeout)
  (declare (ignore conn timeout))
  nil)

(defmethod |setReadTimeout(I)V| ((conn |java/net/URLConnection|) timeout)
  (declare (ignore conn timeout))
  nil)

(defmethod |getURL()Ljava/net/URL;| ((conn |java/net/URLConnection|))
  (gethash conn *connection-urls*))

(defmethod |getContentLength()I| ((conn |java/net/URLConnection|))
  (declare (ignore conn))
  -1)

(defmethod |getContentLengthLong()J| ((conn |java/net/URLConnection|))
  (declare (ignore conn))
  -1)

(defun %new-java-jar-file (path)
  (classload "java/util/jar/JarFile")
  (let ((jar-file (%make-java-instance "java/util/jar/JarFile")))
    (|<init>(Ljava/lang/String;)V| jar-file (jstring path))
    jar-file))

(defun %uncache-jar-file (jar-file)
  "Remove only this cached instance, without holding any ZipFile object lock."
  (when (gethash jar-file *cached-jar-paths*)
    (bt:with-recursive-lock-held (*jar-file-cache-lock*)
      (when-let ((path (gethash jar-file *cached-jar-paths*)))
        (when (eq jar-file (gethash path *jar-file-cache*)) (remhash path *jar-file-cache*))
        (remhash jar-file *cached-jar-paths*)))))

(defun %connect-jar-connection (conn)
  "Publish one validated connection and share its archive only when caching is on."
  (bt:with-recursive-lock-held (*jar-file-cache-lock*)
    (or (gethash conn *connection-jar-files*)
        (when-let ((url (gethash conn *connection-urls*)))
          (multiple-value-bind (jar-path entry-path) (%parse-jar-url (%url-string-of url))
            (unless jar-path (%throw-file-not-found url))
            (let* ((path (handler-case (namestring (truename jar-path))
                           (file-error () (%throw-file-not-found url))))
                   (use-caches (gethash conn *connection-use-caches*))
                   (cached (and use-caches (gethash path *jar-file-cache*)))
                   (jar-file (or cached (%new-java-jar-file path)))
                   (entry (when (plusp (length entry-path))
                            (|getJarEntry(Ljava/lang/String;)Ljava/util/jar/JarEntry;|
                             jar-file (jstring entry-path)))))
              (unless (or entry (zerop (length entry-path)))
                (unless cached (|close()V| jar-file))
                (%throw-file-not-found url))
              (when (and use-caches (null cached))
                (setf (gethash path *jar-file-cache*) jar-file
                      (gethash jar-file *cached-jar-paths*) path))
              (setf (gethash conn *connection-jar-files*) jar-file)
              (when entry (setf (gethash conn *connection-jar-entries*) entry))
              (%mark-connection-connected conn)
              jar-file))))))

(defun %jar-connection-input-stream (conn)
  "Give uncached entry streams ownership of their archive's lifetime."
  (let* ((jar-file (%connect-jar-connection conn))
         (entry (gethash conn *connection-jar-entries*)))
    (unless entry (%throw '|java/io/IOException| "No entry name specified"))
    (handler-case
        (let ((stream (|getInputStream(Ljava/util/zip/ZipEntry;)Ljava/io/InputStream;| jar-file entry)))
          (if (gethash conn *connection-use-caches*) stream (%zip-stream-own-zipfile stream)))
      (error (condition)
        (unless (gethash conn *connection-use-caches*) (|close()V| jar-file))
        (error condition)))))

(defmethod |getJarFile()Ljava/util/jar/JarFile;| ((conn |java/net/JarURLConnection|))
  (%connect-jar-connection conn))

(defmethod |getJarEntry()Ljava/util/jar/JarEntry;| :around ((conn |java/net/JarURLConnection|))
  (if (gethash conn *connection-urls*)
      (progn (%connect-jar-connection conn) (gethash conn *connection-jar-entries*))
      (call-next-method)))
