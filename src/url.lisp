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

(defmethod |openStream()| ((url |java/net/URL|))
  "Open an InputStream for the URL."
  (let ((url-string (lstring (|toString()| url))))
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
      ;; Fall through to Java implementation for other protocols
      (t nil))))
