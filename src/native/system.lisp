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

;;; System properties, System/Runtime/ClassLoader/IO natives.

(in-package :openldk)

;;; Single source of truth for VM/system property values, consumed by
;;; SystemProps$Raw.platformProperties/vmProperties (JDK 9+) and the
;;; legacy System.initProperties.

(defparameter *java-identity-properties*
  '(("java.specification.version" . "25")
    ("java.specification.name" . "Java Platform API Specification")
    ("java.specification.vendor" . "Oracle Corporation")
    ("java.vm.specification.version" . "25")
    ("java.vm.specification.name" . "Java Virtual Machine Specification")
    ("java.vm.specification.vendor" . "Oracle Corporation")
    ("java.vm.name" . "OpenLDK")
    ("java.vm.version" . "1.0")
    ("java.vm.vendor" . "OpenLDK")
    ("java.vm.info" . "interpreted mode")
    ("java.version" . "25")
    ("java.version.date" . "2025-09-16")
    ("java.runtime.version" . "25+36")
    ("java.runtime.name" . "OpenLDK Runtime Environment")
    ("java.vendor" . "OpenLDK")
    ("java.vendor.url" . "https://github.com/atgreen/openldk")
    ("java.vendor.url.bug" . "https://github.com/atgreen/openldk/issues")
    ("java.class.version" . "69.0")
    ("sun.cds.enableSharedLookupCache" . "1")
    ("java.security.debug" . "0")
    ("log4j2.disable.jmx" . "true"))
  "Static java.* identity strings shared by all property providers.")

(defparameter *encoding-properties*
  '(("file.encoding" . "UTF-8")
    ("file.encoding.pkg" . "sun.io")
    ("native.encoding" . "UTF-8")
    ("stdout.encoding" . "UTF-8")
    ("stderr.encoding" . "UTF-8")
    ("sun.stdout.encoding" . "UTF-8")
    ("sun.stderr.encoding" . "UTF-8")
    ("sun.jnu.encoding" . "UTF-8"))
  "Startup encodings; file.encoding may be overridden at launch.")

(defun %configure-runtime-encoding ()
  "Apply the launch-time default charset after restoring a warmed image.
Native pathname and terminal encodings remain UTF-8, independently of file.encoding."
  (let* ((requested (|java/lang/System.getProperty(Ljava/lang/String;)Ljava/lang/String;|
                     (jstring "file.encoding")))
         (name (if (and requested (not (string= (lstring requested) "COMPAT")))
                   requested (jstring "UTF-8"))))
    (%clinit (classload "jdk/internal/util/StaticProperty"))
    (%clinit (classload "java/nio/charset/Charset"))
    (let ((properties (symbol-value (intern "+static-jdk/internal/util/StaticProperty+"
                                           (class-package "jdk/internal/util/StaticProperty"))))
          (charset (symbol-value (intern "+static-java/nio/charset/Charset+"
                                        (class-package "java/nio/charset/Charset")))))
      (setf (slot-value properties '|FILE_ENCODING|) name
            (slot-value charset '|defaultCharset|) nil))
    (|java/lang/System.setProperty(Ljava/lang/String;Ljava/lang/String;)Ljava/lang/String;|
     (jstring "file.encoding") name))
  nil)

(defun %os-name ()
  (cond ((find :LINUX *features*) "Linux")
        ((find :DARWIN *features*) "Mac OS X")
        (t "Unknown")))

(defun %os-version ()
  (cond ((find :LINUX *features*)
         (handler-case
             (with-open-file (stream "/proc/version" :direction :input)
               (let* ((line (read-line stream))
                      (version-start (+ (search "Linux version " line)
                                        (length "Linux version ")))
                      (space-pos (position #\Space line :start version-start)))
                 (subseq line version-start space-pos)))
           (condition () "0.0")))
        ((find :DARWIN *features*)
         (string-trim '(#\Newline) (uiop:run-program "sw_vers --productVersion" :output :string)))
        (t "0.0")))

(defun %os-arch ()
  (cond ((find :X86-64 *features*) "amd64")
        ((find :ARM64 *features*) "aarch64")
        (t "unknown")))

(defun %cpu-endian ()
  (if (find :LITTLE-ENDIAN *features*) "little" "big"))

(defun %user-name ()
  (slot-value (sb-posix:getpwuid (sb-posix:getuid)) 'sb-posix::name))

(defun %vm-dynamic-properties ()
  "Environment-derived properties, computed at call time."
  `(("java.home" . ,(uiop:getenv "JAVA_HOME"))
    ("java.class.path" . ,(or (uiop:getenv "LDK_CLASSPATH")
                              (uiop:getenv "CLASSPATH")
                              "."))
    ("java.library.path" . ,(concatenate 'string (uiop:getenv "JAVA_HOME") "/lib/"))
    ("java.io.tmpdir" . ,(namestring (uiop:temporary-directory)))))

(defun |jdk/internal/util/SystemProps$Raw.platformProperties()[Ljava/lang/String;| ()
  "Return a String[40] of indexed platform properties for JDK 25."
  (let* ((len 40)
         (arr (make-array len :initial-element nil)))
    ;; 0: _display_country_NDX
    (setf (aref arr 0) (jstring "US"))
    ;; 1: _display_language_NDX
    (setf (aref arr 1) (jstring "en"))
    ;; 2: _display_script_NDX (empty)
    (setf (aref arr 2) (jstring ""))
    ;; 3: _display_variant_NDX (empty)
    (setf (aref arr 3) (jstring ""))
    ;; 4: _file_separator_NDX
    (setf (aref arr 4) (jstring "/"))
    ;; 5: _format_country_NDX
    (setf (aref arr 5) (jstring "US"))
    ;; 6: _format_language_NDX
    (setf (aref arr 6) (jstring "en"))
    ;; 7: _format_script_NDX (empty)
    (setf (aref arr 7) (jstring ""))
    ;; 8: _format_variant_NDX (empty)
    (setf (aref arr 8) (jstring ""))
    ;; 9-16: ftp/http/https proxy settings (nil = not set)
    ;; 17: _java_io_tmpdir_NDX
    (setf (aref arr 17) (jstring (namestring (uiop:temporary-directory))))
    ;; 18: _line_separator_NDX
    (setf (aref arr 18) (jstring (format nil "~%")))
    ;; 19: _native_encoding_NDX
    (setf (aref arr 19) (jstring "UTF-8"))
    ;; 20: _os_arch_NDX
    (setf (aref arr 20) (jstring (%os-arch)))
    ;; 21: _os_name_NDX
    (setf (aref arr 21) (jstring (%os-name)))
    ;; 22: _os_version_NDX
    (setf (aref arr 22) (jstring (%os-version)))
    ;; 23: _path_separator_NDX
    (setf (aref arr 23) (jstring ":"))
    ;; 24-26: SOCKS proxy (nil)
    ;; 27: _stderr_encoding_NDX
    (setf (aref arr 27) (jstring "UTF-8"))
    ;; 28: _stdin_encoding_NDX
    (setf (aref arr 28) (jstring "UTF-8"))
    ;; 29: _stdout_encoding_NDX
    (setf (aref arr 29) (jstring "UTF-8"))
    ;; 30: _sun_arch_abi_NDX (empty)
    (setf (aref arr 30) (jstring ""))
    ;; 31: _sun_arch_data_model_NDX
    (setf (aref arr 31) (jstring "64"))
    ;; 32: _sun_cpu_endian_NDX
    (setf (aref arr 32) (jstring (%cpu-endian)))
    ;; 33: _sun_cpu_isalist_NDX (empty)
    (setf (aref arr 33) (jstring ""))
    ;; 34: _sun_io_unicode_encoding_NDX
    (setf (aref arr 34) (jstring (if (find :LITTLE-ENDIAN *features*) "UnicodeLittle" "UnicodeBig")))
    ;; 35: _sun_jnu_encoding_NDX
    (setf (aref arr 35) (jstring "UTF-8"))
    ;; 36: _sun_os_patch_level_NDX (empty)
    (setf (aref arr 36) (jstring ""))
    ;; 37: _user_dir_NDX
    (setf (aref arr 37) (jstring (namestring (uiop:getcwd))))
    ;; 38: _user_home_NDX
    (setf (aref arr 38) (jstring (uiop:getenv "HOME")))
    ;; 39: _user_name_NDX
    (setf (aref arr 39) (jstring (%user-name)))
    ;; Wrap as a java-array
    (make-java-array :component-class (gethash "java/lang/String" *java-classes-by-bin-name*)
                     :initial-contents arr)))

(defun |jdk/internal/util/SystemProps$Raw.vmProperties()[Ljava/lang/String;| ()
  "Return String[] of key-value pairs for JDK 25 VM properties."
  (let* ((pairs (append *java-identity-properties*
                        *encoding-properties*
                        (%vm-dynamic-properties)))
         ;; Flatten to alternating key-value string array
         (flat (loop for (k . v) in pairs
                     when v
                     collect (jstring k)
                     and collect (jstring v)))
         (arr (make-array (length flat) :initial-contents flat)))
    (make-java-array :component-class (gethash "java/lang/String" *java-classes-by-bin-name*)
                     :initial-contents arr)))

(defun |java/lang/System.initProperties(Ljava/util/Properties;)| (props)
  (dolist (prop (append *java-identity-properties*
                        *encoding-properties*
                        (%vm-dynamic-properties)
                        `(("user.home" . ,(uiop:getenv "HOME"))
                          ("user.dir" . ,(%current-directory-string))
                          ("user.name" . ,(%user-name))
                          ("os.name" . ,(%os-name))
                          ("os.version" . ,(%os-version))
                          ("os.arch" . ,(%os-arch))
                          ("sun.cpu.endian" . ,(%cpu-endian))
                          ("file.separator" . "/")
                          ("path.separator" . ":")
                          ("line.separator" . ,(format nil "~%")))))
    (when (cdr prop)
      (|java/lang/System.setProperty(Ljava/lang/String;Ljava/lang/String;)Ljava/lang/String;|
       (ijstring (car prop)) (ijstring (cdr prop)))))
  props)

#|
Need to add:

java.awt.printerjob
sun.arch.data.model
sun.awt.graphicsenv
sun.cpu.isalist
sun.desktop
sun.io.unicode.encoding
sun.java2d.fontpath
sun.jnu.encoding
sun.os.patch.level
sun.stderr.encoding
sun.stdout.encoding
user.country
user.language
user.script
user.timezone
user.variant

|#

(defun |java/io/FileDescriptor.initIDs()V| ()
  "Initialize file descriptor native IDs (no-op)."
  nil)

(defun |java/io/FileDescriptor.getHandle(I)J| (fd)
  "Get OS file handle for fd.  On Unix, return -1 (handles are Windows-only)."
  (declare (ignore fd))
  -1)

(defun |java/io/FileDescriptor.getAppend(I)Z| (fd)
  "Read append mode from an OS descriptor, including redirected stdout/stderr."
  (let ((handle (%resolve-fd fd)))
    (handler-case
        (if (logtest sb-posix:o-append
                     (sb-posix:fcntl (if (typep handle 'sb-sys:fd-stream)
                                         (sb-sys:fd-stream-fd handle) handle)
                                     sb-posix:f-getfl)) 1 0)
      (sb-posix:syscall-error () 0))))

(defmethod |valid()Z| ((this |java/io/FileDescriptor|))
  "Return true (1) if the file descriptor is valid (has an open stream or known fd)."
  (let ((fd (when (slot-exists-p this '|fd|) (slot-value this '|fd|))))
    (cond
      ((and (streamp fd) (open-stream-p fd)) 1)
      ((and (integerp fd) (>= fd 0)) 1)
      (t 0))))

(defun |java/io/FileInputStream.initIDs()V| ()
  "Initialize FileInputStream native IDs (no-op)."
  nil)

(defun |java/io/FileOutputStream.initIDs()V| ()
  "Initialize FileOutputStream native IDs (no-op)."
  nil)

(defun |java/io/RandomAccessFile.initIDs()V| ()
  "Initialize RandomAccessFile native IDs (no-op)."
  nil)

(defun |java/io/Console.istty()| ()
  "Return whether stdin is a tty."
  (not (zerop (sb-alien:alien-funcall
               (sb-alien:extern-alien "isatty" (function sb-alien:int sb-alien:int))
               0))))

(defun |java/lang/System.console()Ljava/io/Console;| ()
  "Return null - console object not yet supported in OpenLDK."
  nil)
(setf (gethash "java/lang/System.console()Ljava/io/Console;" *native-overrides*)
      #'|java/lang/System.console()Ljava/io/Console;|)

;; Override CheckConsole.haveConsole() to always return true.
;; In Kawa's processArgs bytecode, haveConsole()==false skips Shell.run()
;; entirely and only tries startGuiConsole (which fails without a display).
;; Returning true ensures Shell.run() is always called, which correctly
;; handles both TTY input (with prompts) and piped input (no prompts).
(defun |gnu/kawa/io/CheckConsole.haveConsole()Z| ()
  1)
(setf (gethash "gnu/kawa/io/CheckConsole.haveConsole()Z" *native-overrides*)
      #'|gnu/kawa/io/CheckConsole.haveConsole()Z|)

;; Override TtyInPort.make() to directly create a plain TtyInPort.
;; The bytecoded version has a codegen bug: the exception handler for the
;; JLine reflection path doesn't fall through to the fallback TtyInPort
;; creation code (end-of-handler? prevents it). This native override
;; bypasses the buggy compiled code entirely.
(defun |gnu/kawa/io/TtyInPort.make(Ljava/io/InputStream;Lgnu/kawa/io/Path;Lgnu/kawa/io/OutPort;)Lgnu/kawa/io/TtyInPort;| (in-stream path out-port)
  (let ((tty (make-instance (intern "gnu/kawa/io/TtyInPort" (find-package "OPENLDK.APP")))))
    (|<init>(Ljava/io/InputStream;Lgnu/kawa/io/Path;Lgnu/kawa/io/OutPort;)V| tty in-stream path out-port)
    tty))
(setf (gethash "gnu/kawa/io/TtyInPort.make(Ljava/io/InputStream;Lgnu/kawa/io/Path;Lgnu/kawa/io/OutPort;)Lgnu/kawa/io/TtyInPort;" *native-overrides*)
      #'|gnu/kawa/io/TtyInPort.make(Ljava/io/InputStream;Lgnu/kawa/io/Path;Lgnu/kawa/io/OutPort;)Lgnu/kawa/io/TtyInPort;|)

(defun |java/io/Console.encoding()| ()
  "Return the console encoding, or null for default."
  nil)

(defun |java/io/Console.echo(Z)| (on)
  "Set console echo mode. Returns previous value."
  (declare (ignore on))
  t)

(defun |sun/nio/ch/IOUtil.initIDs()V| ()
  "Initialize NIO IOUtil native IDs (no-op)."
  nil)

(defun |java/security/AccessController.doPrivileged(Ljava/security/PrivilegedExceptionAction;)Ljava/lang/Object;| (action)
  (let ((result (|run()Ljava/lang/Object;| action)))
    result))

(defun |java/security/AccessController.doPrivileged(Ljava/security/PrivilegedExceptionAction;Ljava/security/AccessControlContext;)Ljava/lang/Object;| (action context)
  (declare (ignore context))
  (let ((result (|run()Ljava/lang/Object;| action)))
    result))

(defun %array-type-name-assignable-p (this-name other-name)
  "JLS assignability between two array-type names in Class.getName()
form (dotted, e.g. \"[Ljava.lang.String;\").  An array type is
assignable from another when their component types are: recursively
assignable arrays, identical primitives, or reference types where the
target component is assignable from the source component."
  (let ((tc (subseq this-name 1))
        (oc (subseq other-name 1)))
    (cond
      ;; Nested arrays on both sides: recurse on the component types.
      ((and (char= (char tc 0) #\[) (char= (char oc 0) #\[))
       (%array-type-name-assignable-p tc oc))
      ;; Reference component on the target side.
      ((char= (char tc 0) #\L)
       (let ((t-comp (subseq tc 1 (1- (length tc)))))
         (cond
           ;; Object[], Cloneable[], Serializable[] accept any reference
           ;; or array component (all arrays implement both interfaces),
           ;; but never primitive components ([I is not an Object[]).
           ((member t-comp '("java.lang.Object" "java.lang.Cloneable" "java.io.Serializable")
                    :test #'string=)
            (or (char= (char oc 0) #\L) (char= (char oc 0) #\[)))
           ((char= (char oc 0) #\L)
            (let ((t-class (%bin-type-name-to-class (substitute #\/ #\. tc)))
                  (o-class (%bin-type-name-to-class (substitute #\/ #\. oc))))
              (and t-class o-class
                   (eql 1 (|isAssignableFrom(Ljava/lang/Class;)Z| t-class o-class)))))
           (t nil))))
      ;; Primitive components must match exactly.
      (t (string= tc oc)))))

(defmethod |isAssignableFrom(Ljava/lang/Class;)Z| ((this |java/lang/Class|) other)
  (if (equal this other)
      1
      (if (or (eq (|isPrimitive()Z| this) 1) (eq (|isPrimitive()Z| other) 1))
          0
          (let ((this-name (lstring (slot-value this '|name|)))
                (other-name (lstring (slot-value other '|name|))))
            ;; Handle array types specially - they don't have CLOS classes
            (cond
              ;; Both are arrays: JLS covariance on component types
              ((and (char= (char this-name 0) #\[)
                    (char= (char other-name 0) #\[))
               (if (%array-type-name-assignable-p this-name other-name) 1 0))
              ;; Other is array, this is not - array assignable to Object/Cloneable/Serializable
              ((char= (char other-name 0) #\[)
               (if (member this-name '("java.lang.Object" "java.lang.Cloneable" "java.io.Serializable")
                           :test #'string=)
                   1
                   0))
              ;; java.lang.Object is assignable from everything
              ((string= this-name "java.lang.Object") 1)
              ;; Neither is array - use normal class hierarchy
              (t
               (let ((this-ldk-class (get-ldk-class-for-java-class this))
                     (other-ldk-class (get-ldk-class-for-java-class other)))
                 (if (and this-ldk-class
                          other-ldk-class
                          (find-class (intern (name other-ldk-class) (ldk-class-package other-ldk-class)) nil)
                          (find-class (intern (name this-ldk-class) (ldk-class-package this-ldk-class)) nil)
                          (closer-mop:subclassp (find-class (intern (name other-ldk-class) (ldk-class-package other-ldk-class)))
                                                (find-class (intern (name this-ldk-class) (ldk-class-package this-ldk-class)))))
                     1
                     0))))))))

(defun |java/lang/System.setIn0(Ljava/io/InputStream;)V| (in-stream)
  (setf (slot-value |+static-java/lang/System+| '|in|) in-stream))

(defun |java/lang/System.setErr0(Ljava/io/PrintStream;)V| (print-stream)
  (setf (slot-value |+static-java/lang/System+| '|err|) print-stream))

(defun |java/lang/System.setOut0(Ljava/io/PrintStream;)V| (print-stream)
  (setf (slot-value |+static-java/lang/System+| '|out|) print-stream))

(defmethod |getIntVolatile(Ljava/lang/Object;J)I| ((unsafe |sun/misc/Unsafe|) param-object param-long)
  (cond
    ((typep param-object 'java-array)
     (jaref param-object (%unsafe-array-index param-object param-long)))
    ((null param-object)
     ;; Static field access: look up the static singleton
     (slot-value (%unsafe-static-storage (gethash param-long *field-offset-table*))
                 (%unsafe-slot-key param-long)))
    (t
     (slot-value param-object (%unsafe-slot-key param-long)))))

(defmethod |putIntVolatile(Ljava/lang/Object;JI)V| ((unsafe |sun/misc/Unsafe|) obj offset value)
  "Same as putInt for OpenLDK (no volatile semantics needed in Lisp)."
  (declare (ignore unsafe))
  (cond
    ((typep obj 'java-array)
     (setf (jaref obj (%unsafe-array-index obj offset)) value))
    ((null obj)
     (setf (slot-value (%unsafe-static-storage (gethash offset *field-offset-table*))
                       (%unsafe-slot-key offset))
           value))
    (t
     (setf (slot-value obj (%unsafe-slot-key offset)) value))))

(defmethod |getCharVolatile(Ljava/lang/Object;J)C| ((unsafe |sun/misc/Unsafe|) param-object param-long)
  (cond
    ((typep param-object 'java-array)
     (jaref param-object (%unsafe-array-index param-object param-long)))
    ((null param-object)
     ;; Static field access: look up the static singleton
     (slot-value (%unsafe-static-storage (gethash param-long *field-offset-table*))
                 (%unsafe-slot-key param-long)))
    (t
     (slot-value param-object (%unsafe-slot-key param-long)))))

(defmethod |clone()| ((array java-array))
  (make-java-array :component-class (java-array-component-class array)
                   :element-type (array-element-type (java-array-data array))
                   :initial-contents (java-array-data array)))

(defun |sun/reflect/Reflection.getClassAccessFlags(Ljava/lang/Class;)| (class)
  (let ((lclass (get-ldk-class-for-java-class class)))
    (if lclass (access-flags lclass) 0)))

(defun %class-modifiers (class)
  "Source-level modifiers for CLASS, computed from the ldk-class access flags
with ACC_SUPER (0x20) masked out (it is not a source-level modifier)."
  (cond
    ;; A primitive type: public, final and abstract, as on a JVM.
    ((eq (|isPrimitive()Z| class) 1) #x411)
    ;; An array: its component's access, final and abstract, as on a JVM.
    ((eq (|isArray()Z| class) 1)
     (let ((component (|getComponentType()Ljava/lang/Class;| class)))
       (logior (logand (%class-modifiers component) #x7) #x410)))
    (t
     ;; A nested class's come from its InnerClasses entry, which has
     ;; static, private and protected.
     (let* ((lclass (get-ldk-class-for-java-class class))
            (entry (and lclass (%inner-class-entry lclass))))
       (cond (entry (logandc2 (slot-value entry 'inner-class-access-flags) #x20))
             (lclass (logandc2 (access-flags lclass) #x20))
             (t 0))))))

(defmethod |getModifiers()I| ((class |java/lang/Class|))
  (%class-modifiers class))

;; JDK 25's Class.getModifiers() reads the `modifiers` field directly rather
;; than calling a native, and OpenLDK populates that field at class load. The
;; primordial bootstrap classes (java.lang.Object/String) are pre-baked and
;; never pass through that path, so their field stays 0 and they read back as
;; non-public -- which breaks clojure.lang.Reflector ("Can't call public method
;; of non-public class: String.concat"). An :around survives the JIT-compiled
;; field-reading body and always returns the value computed from access flags.
(defmethod |getModifiers()I| :around ((class |java/lang/Class|))
  (%class-modifiers class))

;; JDK 25 formats integers via jdk.internal.util.DecimalDigits, whose packed
;; getChars path computes a wrong buffer size under OpenLDK and throws
;; ArrayIndexOutOfBoundsException -1 (first hit from Clojure str/pr at
;; instant.clj). Bypass Integer/Long.toString with a direct Lisp formatter.
(defun %java-radix-string (n radix)
  "Java Integer/Long.toString(n, radix): base-RADIX with lowercase digits;
radix outside 2..36 falls back to decimal."
  (if (or (< radix 2) (> radix 36))
      (format nil "~D" n)
      (string-downcase (write-to-string n :base radix))))

(setf (gethash "java/lang/Long.toString(J)Ljava/lang/String;" *native-overrides*)
      (lambda (n) (jstring (format nil "~D" n))))
(setf (gethash "java/lang/Integer.toString(I)Ljava/lang/String;" *native-overrides*)
      (lambda (n) (jstring (format nil "~D" n))))
(setf (gethash "java/lang/Long.toString(JI)Ljava/lang/String;" *native-overrides*)
      (lambda (n radix) (jstring (%java-radix-string n radix))))
(setf (gethash "java/lang/Integer.toString(II)Ljava/lang/String;" *native-overrides*)
      (lambda (n radix) (jstring (%java-radix-string n radix))))

(defun %java-simple-name (class)
  "Compute Class.getSimpleName() for CLASS: strip the package and any enclosing
class ($-separated) and compiler-added local/anonymous digits; arrays get []
suffixes.  A nested class's simple name comes from its InnerClasses entry, as
on a JVM; a class without one (generated, or hand-assembled) keeps the whole
name after its package, $ and all."
  (if (eq (|isArray()Z| class) 1)
      (concatenate 'string (%java-simple-name (|getComponentType()| class)) "[]")
      (let* ((lclass (and (eq 0 (|isPrimitive()Z| class)) (get-ldk-class-for-java-class class)))
             (entry (and lclass (%inner-class-entry lclass))))
        (if entry
            ;; A nested class: the simple name its InnerClasses entry gives
            ;; it, or none for an anonymous class.
            (let ((index (slot-value entry 'inner-name-index)))
              (if (zerop index)
                  ""
                  (slot-value (aref (constant-pool lclass) index) 'value)))
            ;; A top-level class: its name without the package.
            (let* ((n (lstring (slot-value class '|name|)))
                   (dot (position #\. n :from-end t)))
              (if dot (subseq n (1+ dot)) n))))))

;; Class.getSimpleName() is bytecoded and depends on getEnclosingClass(), which
;; we don't reconstruct (so it would return e.g. "Outer$Inner"). Override it.
(setf (gethash "java/lang/Class.getSimpleName()Ljava/lang/String;" *native-overrides*)
      (lambda (class) (jstring (%java-simple-name class))))

(defmethod |getSuperclass()Ljava/lang/Class;| ((class |java/lang/Class|))
  (let ((lclass (get-ldk-class-for-java-class class)))
    (when (and lclass (super lclass))
      (or (gethash (super lclass) *java-classes-by-bin-name*)
          (let ((super-lclass (classload (super lclass))))
            (when super-lclass
              (java-class super-lclass)))))))

;; Kawa's ClassType.getSuperclass() has a guard that checks
;; "java.lang.Object".equals(getName()) to avoid calling
;; reflectClass.getSuperclass() on Object (which returns null).
;; However, ClassType objects may store names with slashes
;; ("java/lang/Object"), so the guard fails and Type.make(null)
;; is called, triggering a NullPointerException on null.isArray().
;; This :around method catches that NPE and returns nil, which is
;; the correct result: Object has no superclass.
(defmethod |getSuperclass()Lgnu/bytecode/ClassType;| :around (self)
  (handler-case (call-next-method)
    (|condition-java/lang/NullPointerException| () nil)))

(defmethod |getInterfaces0()[Ljava/lang/Class;| ((class |java/lang/Class|))
  ;; FIXME: do something different for interfaces?
  (let ((lclass (get-ldk-class-for-java-class class)))
    (make-java-array
     :component-class (%get-java-class-by-fq-name "java.lang.Class")
     :initial-contents
     (if lclass
         (coerce (remove nil (mapcar (lambda (iname)
                                        (let ((lc (gethash iname *ldk-classes-by-bin-name*)))
                                          (when lc (java-class lc))))
                                      (coerce (interfaces lclass) 'list)))
                 'vector)
         #()))))

(defun java-class-to-type-descriptor (jclass)
  "Convert a java.lang.Class object to its JVM type descriptor string.
   E.g., int -> \"I\", java.lang.String -> \"Ljava/lang/String;\", int[] -> \"[I\"."
  (let ((name (lstring (slot-value jclass '|name|))))
    (cond
      ((string= name "int") "I")
      ((string= name "long") "J")
      ((string= name "boolean") "Z")
      ((string= name "byte") "B")
      ((string= name "char") "C")
      ((string= name "short") "S")
      ((string= name "float") "F")
      ((string= name "double") "D")
      ((string= name "void") "V")
      ;; Array types: name is like "[I" or "[Ljava.lang.String;" - already in descriptor form
      ;; but with dots instead of slashes for the element type
      ((and (> (length name) 0) (char= (char name 0) #\[))
       (substitute #\/ #\. name))
      ;; Object types
      (t (format nil "L~A;" (substitute #\/ #\. name))))))

(defun build-method-descriptor (parameter-types &optional return-type)
  "Build a JVM method descriptor string from parameterTypes (java-array of Class)
   and optional returnType (Class). If return-type is nil, uses V (void)."
  (format nil "(~{~A~})~A"
          (if parameter-types
              (loop for pclass across (java-array-data parameter-types)
                    collect (java-class-to-type-descriptor pclass))
              nil)
          (if return-type
              (java-class-to-type-descriptor return-type)
              "V")))

(defun |sun/reflect/NativeConstructorAccessorImpl.newInstance0(Ljava/lang/reflect/Constructor;[Ljava/lang/Object;)|
    (constructor params)
  (let* ((java-class (slot-value constructor '|clazz|))
         (bin-class-name (substitute #\/ #\. (lstring (slot-value java-class '|name|)))))
    (|java/lang/Class.forName0(Ljava/lang/String;ZLjava/lang/ClassLoader;Ljava/lang/Class;)Ljava/lang/Class;| (jstring bin-class-name) nil nil nil)
    ;; Get class package from loader - class symbols live in loader's package
    (let* ((pkg (%java-class-package java-class))
           (class-sym (intern bin-class-name pkg))
           (obj (make-instance class-sym))
           (descriptor (build-method-descriptor (slot-value constructor '|parameterTypes|))))
      (if (string= "()V" descriptor)
          (|<init>()V| obj)
          (progn
            (apply (intern
                    (lispize-method-name
                     (format nil "<init>~A" descriptor))
                    :openldk)
                   ;; params can be NIL for zero-arg constructor paths; guard before accessing array-data.
                   (cons obj (if params (coerce (java-array-data params) 'list) nil)))))
      ; (format t "~&NEWINSTANCE0 ~A = ~A~%" constructor obj)
      obj)))

(defmethod |ensureClassInitialized(Ljava/lang/Class;)V| ((unsafe |sun/misc/Unsafe|) class)
  (let ((lclass (get-ldk-class-for-java-class class)))
    (when lclass
      (%clinit lclass))))

;; JDK 17: Unsafe.ensureClassInitialized0 — native variant
(defmethod |ensureClassInitialized0(Ljava/lang/Class;)V| ((unsafe |jdk/internal/misc/Unsafe|) class)
  (let ((lclass (get-ldk-class-for-java-class class)))
    (when lclass
      (%clinit lclass))))

(defmethod |shouldBeInitialized(Ljava/lang/Class;)Z| ((unsafe |sun/misc/Unsafe|) class)
  (let ((lclass (get-ldk-class-for-java-class class)))
    (if (and lclass (initialized-p lclass))
        nil
        t)))

(defmethod |shouldBeInitialized0(Ljava/lang/Class;)Z| ((unsafe |jdk/internal/misc/Unsafe|) class)
  (let ((lclass (get-ldk-class-for-java-class class)))
    (if (and lclass (initialized-p lclass))
        nil
        t)))

(defvar *unsafe-memory-table* (make-hash-table :synchronized t))


(defun |java/lang/System.mapLibraryName(Ljava/lang/String;)Ljava/lang/String;| (library-name)
  (or #+LINUX (jstring (format nil "lib~A.so" (lstring library-name)))
      #+DARWIN (jstring (format nil "lib~A.dylib" (lstring library-name)))
      (unimplemented "System.mapLibraryName on this platform")))

(defun |java/lang/ClassLoader.findBuiltinLib(Ljava/lang/String;)| (library-name)
  library-name)

(defun |java/lang/ClassLoader.findLoadedClass0(Ljava/lang/String;)Ljava/lang/Class;| (loader name)
  "Check if a class has already been loaded by this class loader.
   Returns the java.lang.Class if found, nil otherwise."
  (let* ((class-name (lstring name))
         (bin-name (substitute #\/ #\. class-name))
         (ldk-loader (get-ldk-loader-for-java-loader loader)))
    ;; findLoadedClass includes delegated classes for which this loader was
    ;; recorded as an initiating loader; it must not search arbitrary parents.
    (when-let ((class (%initiated-class bin-name ldk-loader)))
      ;; Descriptor resolution records synthetic primitives internally, but
      ;; Java ClassLoader APIs can only return reference classes.
      (unless (eql 1 (|isPrimitive()Z| (java-class class)))
        (java-class class)))))

(defun %classfile-name (bytes offset length)
  "The binary name the class file in BYTES (from OFFSET, LENGTH long)
declares: defineClass's for a null name, as the JVM takes it (the Scala
REPL defines its classes so)."
  (handler-case
      (name (read-classfile (make-instance 'byte-array-input-stream
                                         :array bytes :start offset :end (+ offset length))))
    (bytecode-verification-error (condition) (%throw-verification-error condition))))

(defun |java/lang/ClassLoader.defineClass1(Ljava/lang/ClassLoader;Ljava/lang/String;[BIILjava/security/ProtectionDomain;Ljava/lang/String;)Ljava/lang/Class;|
    (loader name bytes offset len pd source)
  "Define a class from byte array data using the specified class loader.
   JDK 17 static native — delegates to %classload-from-stream for full
   class setup (module, inner classes, nest host, throwable conditions, etc.)."
  (declare (ignore source))
  (let* ((ldk-loader (get-ldk-loader-for-java-loader loader))
         (class-name (if name
                         (substitute #\/ #\. (lstring name))
                         (%classfile-name bytes offset len)))
         (_ (when (%initiated-class class-name ldk-loader)
              (%throw '|java/lang/LinkageError| (format nil "Duplicate class definition: ~A" class-name))))
         (stream (make-instance 'byte-array-input-stream
                                :array bytes :start offset :end (+ offset len)))
         (result (%classload-from-stream class-name stream loader ldk-loader)))
    (unless result
      (let ((exc (%make-java-instance "java/lang/NoClassDefFoundError")))
        (|<init>(Ljava/lang/String;)V| exc name)
        (error (%lisp-condition exc))))
    (when pd
      (setf (gethash (java-class result) *protection-domains*) pd))
    (java-class result)))

(defun |java/lang/ClassLoader.defineClass2(Ljava/lang/ClassLoader;Ljava/lang/String;Ljava/nio/ByteBuffer;IILjava/security/ProtectionDomain;Ljava/lang/String;)Ljava/lang/Class;|
    (loader name buffer offset length protection-domain source)
  "Define from a direct ByteBuffer without changing its position or limit."
  (unless (and (not (minusp offset)) (not (minusp length))
               (<= (+ offset length) (|limit()I| buffer)))
    (%throw '|java/lang/IndexOutOfBoundsException|))
  (let ((bytes (make-java-array :component-class (%get-java-class-by-bin-name "byte") :size length)))
    (dotimes (index length)
      (setf (jaref bytes index) (|get(I)B| buffer (+ offset index))))
    (|java/lang/ClassLoader.defineClass1(Ljava/lang/ClassLoader;Ljava/lang/String;[BIILjava/security/ProtectionDomain;Ljava/lang/String;)Ljava/lang/Class;|
     loader name bytes 0 length protection-domain source)))

(defvar *hidden-class-data* (make-hash-table :test #'eq :weakness :key)
  "Maps a hidden class's java.lang.Class object to the classData passed to
MethodHandles.Lookup.defineHiddenClassWithClassData; read back by
MethodHandles.classData().")

(defun |java/lang/ClassLoader.defineClass0(Ljava/lang/ClassLoader;Ljava/lang/Class;Ljava/lang/String;[BIILjava/security/ProtectionDomain;ZILjava/lang/Object;)Ljava/lang/Class;|
    (loader lookup-class name bytes offset len pd initialize flags class-data)
  "JDK 25 native backing MethodHandles.Lookup.defineClass / defineHiddenClass.
   Loads a class from BYTES via %classload-from-stream under its own declared
   name. OpenLDK cracks MethodHandle vmentries directly rather than running the
   spun LambdaForm bytecode, so the hidden class only needs to load and run its
   <clinit>; distinct spins that share a name resolve to one CLOS class, which
   is harmless because the bytecode body is never executed. CLASS-DATA is stashed
   for MethodHandles.classData(). When INITIALIZE is true, runs <clinit>."
  (declare (ignore lookup-class pd))
  (let* ((ldk-loader (get-ldk-loader-for-java-loader loader))
         (class-name (if name
                         (substitute #\/ #\. (lstring name))
                         (format nil "hidden/~A" (gensym "class-"))))
         (stream (make-instance 'byte-array-input-stream
                                :array bytes :start offset :end (+ offset len)))
         (result (%classload-from-stream class-name stream loader ldk-loader
                                         (or (null name) (logbitp 1 flags)))))
    (unless result
      (let ((exc (%make-java-instance "java/lang/NoClassDefFoundError")))
        (|<init>(Ljava/lang/String;)V| exc (or name (jstring class-name)))
        (error (%lisp-condition exc))))
    (let ((jc (java-class result)))
      (when class-data
        (setf (gethash jc *hidden-class-data*) class-data))
      (when (and initialize (not (eql initialize 0)))
        (%clinit result))
      jc)))

(defun %define-class-from-bytes (loader class-name-hint class-bytes)
  "Internal function to define a class from raw bytes.
   LOADER is the java.lang.ClassLoader.
   CLASS-NAME-HINT is optional hint (may be nil, we read actual name from bytes).
   CLASS-BYTES is the raw classfile bytes."
  (let* ((ldk-loader (get-ldk-loader-for-java-loader loader)))
    ;; Create an input stream from the bytes
    (flexi-streams:with-input-from-sequence (stream class-bytes)
      (let* ((class (read-classfile stream))
             (classname (slot-value class 'name))
             (fq-classname (substitute #\. #\/ classname)))
        ;; Verify name matches hint if provided
        (when (and class-name-hint
                   (not (string= classname (substitute #\/ #\. class-name-hint))))
          (internal-error "Class name ~A does not match expected ~A" classname class-name-hint))

        ;; Check if class already defined - return existing java.lang.Class
        ;; This mirrors JVM behavior where defineClass on existing class is an error,
        ;; but we return the existing class instead to handle warm-up scenarios
        ;; Check both java.lang.Class and LDK class to prevent double loading
        (let ((existing-java-class (or (gethash classname *java-classes-by-bin-name*)
                                       (when ldk-loader
                                         (gethash classname (slot-value ldk-loader 'java-classes-by-bin-name))))))
          (when existing-java-class
            (return-from %define-class-from-bytes existing-java-class)))
        ;; Also check for existing LDK class (may have been loaded via classload path)
        (let ((existing-ldk-class (or (gethash classname *ldk-classes-by-bin-name*)
                                      (when ldk-loader
                                        (gethash classname (slot-value ldk-loader 'ldk-classes-by-bin-name))))))
          (when existing-ldk-class
            (return-from %define-class-from-bytes (java-class existing-ldk-class))))

        ;; Set the loader on the class
        (setf (slot-value class 'ldk-loader) ldk-loader)
        (%check-sealed-supertypes
         class (mapcar (lambda (parent) (%resolve-class parent ldk-loader))
                       (append (when (super class) (list (super class))) (coerce (interfaces class) 'list))))

        ;; Store in loader's class maps
        (setf (gethash classname (slot-value ldk-loader 'ldk-classes-by-bin-name)) class)
        (setf (gethash fq-classname (slot-value ldk-loader 'ldk-classes-by-fq-name)) class)
        ;; Also store in global tables for backward compatibility
        (setf (gethash classname *ldk-classes-by-bin-name*) class)
        (setf (gethash fq-classname *ldk-classes-by-fq-name*) class)

        ;; Create java.lang.Class object
        (let ((klass (%make-java-instance "java/lang/Class"))
              (cname (jstring fq-classname)))
          (with-slots (|name| |classLoader|) klass
            (setf |name| cname)
            (setf |classLoader| loader))
          (when (and *unnamed-module* (slot-exists-p klass '|module|))
            (setf (slot-value klass '|module|) (%defining-class-module classname ldk-loader loader)))
          (setf (java-class class) klass)
          (setf (gethash classname (slot-value ldk-loader 'java-classes-by-bin-name)) klass)
          (setf (gethash fq-classname (slot-value ldk-loader 'java-classes-by-fq-name)) klass)
          ;; Also store in global tables for backward compatibility
          (setf (gethash classname *java-classes-by-bin-name*) klass)
          (setf (gethash fq-classname *java-classes-by-fq-name*) klass)

          ;; Emit and evaluate the class definition
          (let ((code (emit-<class> class ldk-loader)))
            (%eval code))
          (%install-method-stubs class ldk-loader)

          ;; Load super and interfaces (using this loader for resolution)
          (let ((super (slot-value class 'super))
                (interfaces (slot-value class 'interfaces)))
            (when super (classload super loader))
            (when interfaces
              (dolist (iface (coerce interfaces 'list))
                (classload iface loader))))

          ;; Emit the class initializer - use loader's package for class symbols
          (let* ((pkg (loader-package ldk-loader))
                 (lisp-class (find-class (intern (substitute #\/ #\. classname) pkg))))
            (closer-mop:finalize-inheritance lisp-class)
            (let ((icc (append (list 'defun (intern (format nil "%clinit-~A" (substitute #\/ #\. classname)) pkg) (list))
                               (loop for k in (reverse (closer-mop:class-precedence-list lisp-class))
                                     ;; Get each class's package from its symbol's package
                                     for clinit-pkg = (symbol-package (class-name k))
                                     for clinit-function = (intern (format nil "~a.<clinit>()V" (class-name k)) clinit-pkg)
                                     when (fboundp clinit-function)
                                       collect (let ((ldkclass (%get-ldk-class-by-bin-name (format nil "~A" (class-name k)) t ldk-loader)))
                                                 (when ldkclass
                                                   (list 'unless (list 'initialized-p ldkclass)
                                                         (list 'setf (list 'initialized-p ldkclass) t)
                                                         (list clinit-function))))))))
              (%eval icc)))

          ;; Return the java.lang.Class
          klass)))))

(defmethod |load(Ljava/lang/String;Z)| ((loader t) library-name is-builtin)
  (when *debug-trace*
    (format t "~&; ~A loading native library ~A (no-op)~%" loader library-name))
  (setf (slot-value loader '|loaded|) 1)
  nil)

(defmethod |sun/misc/Signal.findSignal(Ljava/lang/String;)| (signal-name)
  (let ((sname (lstring signal-name)))
    (cond
      ((string= sname "HUP") 1)
      ((string= sname "INT") 2)
      ((string= sname "KILL") 9)
      ((string= sname "TERM") 15)
      (t (unimplemented "Signal.findSignal(~S)" sname)))))

(defun |sun/misc/Signal.handle0(IJ)| (sig native-h)
  (declare (ignore sig)
           (ignore native-h))
  1)

(defun %java-notify-all (objref)
  "Notify all waiters on any Java object representation, including arrays."
  (let* ((monitor (%get-monitor objref))
         (mutex (mutex monitor))
         (cv (condition-variable monitor))
         (current-thread (current-thread-identity)))
    (bordeaux-threads:with-lock-held (mutex)
      (unless (eq (owner monitor) current-thread)
        (error (%lisp-condition (%make-throwable '|java/lang/IllegalMonitorStateException|))))
      (when (wait-set monitor)
        (setf (wait-set monitor) nil)
        (sb-thread:condition-broadcast cv)))))

(defmethod |notifyAll()V| ((object |java/lang/Object|)) (%java-notify-all object))
(defmethod |notifyAll()V| ((object java-array)) (%java-notify-all object))

(defun |sun/misc/URLClassPath.getLookupCacheURLs(Ljava/lang/ClassLoader;)| (class-loader)
  (declare (ignore class-loader))
  nil)

(defun %raf-stream (raf)
  "The Lisp stream backing RANDOMACCESSFILE RAF.  RAF.fd is a
java.io.FileDescriptor (created by the RAF constructor); the stream lives
in the FileDescriptor's own fd slot so that FileDescriptor methods like
valid() work.  Tolerate a raw stream in fd for robustness."
  (let ((fd (slot-value raf '|fd|)))
    (if (typep fd '|java/io/FileDescriptor|)
        (slot-value fd '|fd|)
        fd)))

(defun %throw-file-open-error (filename reason)
  "Throw the FileNotFoundException the JDK's file opens throw: FILENAME (a
Java string) and, in parentheses, REASON."
  (let ((fnf (%make-java-instance "java/io/FileNotFoundException")))
    (|<init>(Ljava/lang/String;)V| fnf (jstring (format nil "~A (~A)" (lstring filename) reason)))
    (error (%lisp-condition fnf))))

(defun %check-not-directory (filename)
  "Throw FileNotFoundException if FILENAME (a Java string) names a
directory: Lisp's OPEN opens one, and the first read fails."
  (when (handler-case (sb-posix:s-isdir (sb-posix:stat-mode (sb-posix:stat (lstring filename))))
          (sb-posix:syscall-error () nil))
    (%throw-file-open-error filename "Is a directory")))

(defun %file-error-message (condition)
  "SBCL's OS message for a file error, when one is available."
  (cond
    ((typep condition 'sb-posix:syscall-error)
     (sb-int:strerror (sb-posix:syscall-errno condition)))
    ((and (typep condition 'sb-int:simple-file-error)
          (slot-boundp condition 'sb-kernel::message))
     (slot-value condition 'sb-kernel::message))))

(defun %file-descriptor-exhausted-p (condition)
  "Whether CONDITION reports process or system descriptor exhaustion."
  (let ((message (%file-error-message condition)))
    (and (stringp message)
         (member message (list (sb-int:strerror sb-posix:emfile)
                               (sb-int:strerror sb-posix:enfile)) :test #'string=))))

(defun %call-with-file-open-errors (filename thunk)
  "Translate file opens, collecting abandoned resources and retrying once on EMFILE."
  (%check-not-directory filename)
  (labels ((attempt (recover-p)
             (handler-case (funcall thunk)
               (sb-ext:file-does-not-exist ()
                 (%throw-file-open-error filename "No such file or directory"))
               ((or file-error sb-posix:syscall-error) (condition)
                 (if (and recover-p (%file-descriptor-exhausted-p condition))
                     (progn
                       (sb-ext:gc :full t)
                       (%process-java-references)
                       (attempt nil))
                     (%throw-file-open-error filename
                       (or (%file-error-message condition) "Permission denied")))))))
    (attempt t)))

(defmacro %with-file-open-errors ((filename) &body body)
  "Open a file with Java errors and one bounded descriptor-recovery attempt."
  `(%call-with-file-open-errors ,filename (lambda () ,@body)))

(defmethod |open0(Ljava/lang/String;I)V| ((fis |java/io/RandomAccessFile|) filename mode)
  (%with-file-open-errors (filename)
    (let ((stream (open (sb-ext:parse-native-namestring (lstring filename))
                        :element-type '(unsigned-byte 8)
                        :direction (if (logtest mode 2) :io :input)
                        :if-exists :overwrite
                        :if-does-not-exist (if (logtest mode 2) :create :error)))
          (fd (slot-value fis '|fd|)))
      ;; Store the stream inside the FileDescriptor (the RAF constructor set
      ;; fis.fd = new FileDescriptor()) so valid()/getFD() see a real fd.
      (if (typep fd '|java/io/FileDescriptor|)
          (setf (slot-value fd '|fd|) stream)
          (let ((newfd (%make-java-instance "java/io/FileDescriptor")))
            (setf (slot-value newfd '|fd|) stream)
            (setf (slot-value fis '|fd|) newfd)))
      stream)))

(defmethod |length0()J| ((raf |java/io/RandomAccessFile|))
  ;; Flush buffered writes so file-length reflects them.
  (let ((s (%raf-stream raf)))
    (force-output s)
    (file-length s)))

(defmethod |setLength0(J)V| ((raf |java/io/RandomAccessFile|) length)
  "Resize RAF and clamp its logical pointer, invalidating Lisp stream buffers."
  (let ((stream (%raf-stream raf)))
    (unless (and (typep stream 'sb-sys:fd-stream) (open-stream-p stream))
      (%throw '|java/io/IOException| "Stream Closed"))
    (when (minusp length)
      (%throw '|java/io/IOException| "Invalid argument"))
    (handler-case
        (let ((position (file-position stream)))
          ;; Synchronize buffered output/read-ahead with the shared OS fd.
          (file-position stream position)
          (sb-posix:ftruncate (sb-sys:fd-stream-fd stream) length)
          (file-position stream (min position length)))
      (sb-posix:syscall-error (condition)
        (%throw '|java/io/IOException| (sb-int:strerror (sb-posix:syscall-errno condition))))
      (stream-error (condition)
        (%throw '|java/io/IOException| (princ-to-string condition)))))
  nil)

(defmethod |getFilePointer()J| ((raf |java/io/RandomAccessFile|))
  (file-position (%raf-stream raf)))

(defmethod |read0()I| ((raf |java/io/RandomAccessFile|))
  (let ((byte (read-byte (%raf-stream raf) nil nil)))
    (if byte byte -1)))

(defmethod |seek0(J)V| ((raf |java/io/RandomAccessFile|) position)
  (file-position (%raf-stream raf) position))

(defmethod |write0(I)V| ((raf |java/io/RandomAccessFile|) byte)
  (write-byte (logand byte #xFF) (%raf-stream raf))
  nil)

(defmethod |writeBytes0([BII)V| ((raf |java/io/RandomAccessFile|) byte-array offset length)
  (write-sequence (%convert-to-unsigned-8-bit (java-array-data byte-array))
                  (%raf-stream raf) :start offset :end (+ offset length))
  nil)

(defmethod |readBytes0([BII)I| ((raf |java/io/RandomAccessFile|) byte-array offset length)
  (let ((in-stream (%raf-stream raf))
        (bytes-read 0))
    (loop for i from offset below (+ offset length)
          for byte = (read-byte in-stream nil nil) ; Read a byte, return NIL on EOF
          while byte
          do (setf (jaref byte-array i) (if (> byte 127) (- byte 256) byte))
             (incf bytes-read))  ; Count bytes read
    (if (and (zerop bytes-read) (plusp length)) -1 bytes-read)))

(defmethod |open0(Ljava/lang/String;)V| ((fis |java/io/FileInputStream|) filename)
  (%with-file-open-errors (filename)
    (let ((stream (open (sb-ext:parse-native-namestring (lstring filename))
                        :element-type '(unsigned-byte 8)
                        :direction :input))
          (fd (slot-value fis '|fd|)))
      ;; Store the Lisp stream inside the FileDescriptor's fd slot
      ;; (like FileOutputStream does), so fd.valid() works in JDK 17.
      (if (and fd (slot-exists-p fd '|fd|))
          (setf (slot-value fd '|fd|) stream)
          (setf (slot-value fis '|fd|) stream)))))

;; File descriptors OpenLDK hands to Java as plain integers -- the pipes to a
;; child process (see process.lisp) -- and their Lisp streams.  Java keeps
;; such an integer in a FileDescriptor; the file stream natives below map it
;; back through %RESOLVE-FD.
(defvar *fd-streams* (make-hash-table :synchronized t)
  "Integer file descriptor -> the Lisp stream for it.")

(defun %resolve-fd (fd)
  "FD as the file stream natives use it: the Lisp stream for an integer
descriptor in *FD-STREAMS*, otherwise FD unchanged (0, 1, 2 or a stream)."
  (or (and (integerp fd) (gethash fd *fd-streams*))
      fd))

(defun %close-file-descriptor (fd)
  "Close FD's Lisp stream or raw OS descriptor and forget its registration."
  (let ((stream (%resolve-fd fd)))
    (unwind-protect
         (cond
           ((and (streamp stream) (open-stream-p stream)) (close stream))
           ((and (integerp stream) (not (minusp stream)))
            (handler-case (sb-posix:close stream)
              (sb-posix:syscall-error (condition)
                ;; Linux closes the descriptor even when close reports EINTR.
                ;; Retrying could close a descriptor just reused by another thread.
                (unless (= (sb-posix:syscall-errno condition) sb-posix:eintr)
                  (%throw '|java/io/IOException|
                          (sb-int:strerror (sb-posix:syscall-errno condition))))))))
      (when (integerp fd)
        ;; Do not discard a different stream registered concurrently.
        (sb-ext:with-locked-hash-table (*fd-streams*)
          (when (eq stream (gethash fd *fd-streams*))
            (remhash fd *fd-streams*))))))
  nil)

(defun |java/io/FileCleanable.cleanupClose0(IJ)V| (fd handle)
  "Native fallback for an abandoned FileDescriptor."
  (declare (ignore handle))
  (%close-file-descriptor fd))

(setf (gethash "java/io/FileCleanable.performCleanup()V" *native-overrides*)
      (lambda (cleanable)
        (|java/io/FileCleanable.cleanupClose0(IJ)V|
         (slot-value cleanable '|fd|) (slot-value cleanable '|handle|))))

(defun %stream-fd (holder)
  "The fd a FileInputStream/FileOutputStream/RandomAccessFile HOLDER uses:
its FileDescriptor's fd (or its own fd slot), resolved by %RESOLVE-FD."
  (let ((file-descriptor (slot-value holder '|fd|)))
    (%resolve-fd (if (and file-descriptor (slot-exists-p file-descriptor '|fd|))
                     (slot-value file-descriptor '|fd|)
                     file-descriptor))))

(defmethod |readBytes([BII)I| ((fis |java/io/FileInputStream|) byte-array offset length)
  (let* ((fd (%stream-fd fis))
         (in-stream (cond ((eql fd 0)
                           ;; Flush stdout before blocking on stdin, like C stdio.
                           (force-output *standard-output*)
                           *standard-input*)
                          ((streamp fd) fd)
                          (t (unimplemented "fd ~A in FileInputStream.readBytes" fd))))
         (bytes-read 0))
    ;; First byte: block waiting for input.
    ;; Subsequent bytes: only read if immediately available (listen).
    ;; This matches OS read() behavior on terminals, which returns
    ;; available data rather than trying to fill the entire buffer.
    (when (plusp length)
      (let ((byte (read-byte in-stream nil nil)))
        (when byte
          (setf (jaref byte-array offset) (if (> byte 127) (- byte 256) byte))
          (incf bytes-read)
          (loop for i from (1+ offset) below (+ offset length)
                while (listen in-stream)
                for b = (read-byte in-stream nil nil)
                while b
                do (setf (jaref byte-array i) (if (> b 127) (- b 256) b))
                   (incf bytes-read)))))
    (if (and (zerop bytes-read) (plusp length)) -1 bytes-read)))

(defmethod |available0()I| ((fis |java/io/FileInputStream|))
  (let ((fd (%stream-fd fis)))
    (cond
      ((eql fd 0) 0)
      ((streamp fd)
       ;; A pipe has no length: report whether a byte is waiting.
       (handler-case (- (file-length fd) (file-position fd))
         (error () (if (listen fd) 1 0))))
      (t 0))))

(defmethod |isInstance(Ljava/lang/Object;)Z| ((this |java/lang/Class|) objref)
  (let* ((class-name (lstring (slot-value this '|name|)))
         (normalized-name (substitute #\/ #\. class-name))
         ;; Get class's loader package for correct type lookup
         (pkg (%java-class-package this))
         (class-symbol (intern normalized-name pkg)))
    ;; Handle native Lisp types (integers, floats, doubles, characters)
    (cond
      ((%native-type-castable-p objref normalized-name)
       1)
      ;; Java arrays (java-array structs) are instances of Object, Cloneable, Serializable,
      ;; and matching array types
      ((java-array-p objref)
       (cond
         ((member class-name '("java.lang.Object" "java.lang.Cloneable" "java.io.Serializable")
                  :test #'string=) 1)
         ;; Check if the target is an array class matching the component type
         ((and (plusp (length class-name)) (char= (char class-name 0) #\[)) 1)
         (t 0)))
      ;; Standard typep check for CLOS objects
      ((typep objref class-symbol) 1)
      (t 0))))

(defmethod |closeAll(Ljava/io/Closeable;)V| ((stream stream) closeable)
  (close stream))

(defun %convert-to-unsigned-8-bit (signed-array)
  "Convert a signed 8-bit array to an unsigned 8-bit array."
  (let* ((dimensions (array-dimensions signed-array))
         (unsigned-array (make-array dimensions :element-type '(unsigned-byte 8))))
    (dotimes (i (array-total-size signed-array))
      (setf (row-major-aref unsigned-array i)
            (logand (row-major-aref signed-array i) #xFF)))
    unsigned-array))

(defun %file-output-target (fos)
  "Resolve FOS to an output stream or raw descriptor, rejecting closed handles."
  (let* ((fd (%stream-fd fos))
         (target (case fd (1 *standard-output*) (2 *error-output*) (otherwise fd))))
    (unless (or (and (integerp target) (not (minusp target)))
                (and (streamp target) (open-stream-p target) (output-stream-p target)))
      (%throw '|java/io/IOException| "Stream Closed"))
    target))

(defun %write-file-output-bytes (fos bytes)
  "Write BYTES completely to FOS without retaining a failed OS write in Lisp."
  (let ((target (%file-output-target fos)))
    (handler-case
        (let ((fd (cond ((integerp target) target)
                        ((typep target 'sb-sys:fd-stream) (sb-sys:fd-stream-fd target)))))
          (when (streamp target)
            (finish-output target)
            ;; A FileOutputStream can share a RandomAccessFile's descriptor.
            ;; Discard read-ahead at its logical position before native output.
            (when (input-stream-p target)
              (when-let ((position (file-position target)))
                (file-position target position))))
          (if fd
              (unwind-protect
                   (sb-sys:with-pinned-objects (bytes)
                     (loop with offset = 0
                           while (< offset (length bytes))
                           do (multiple-value-bind (count errno)
                                  ;; Close invalidates Java's descriptor before
                                  ;; OS reuse. Re-read it after short/EINTR writes.
                                  (sb-unix:unix-write
                                   (let ((current (%file-output-target fos)))
                                     (if (integerp current) current (sb-sys:fd-stream-fd current)))
                                   (sb-sys:vector-sap bytes) offset (- (length bytes) offset))
                                (cond
                                  ((and count (plusp count)) (incf offset count))
                                  ((and (null count) (= errno sb-posix:eintr)))
                                  (t (%throw '|java/io/IOException|
                                             (if count "Write made no progress" (sb-int:strerror errno))))))))
                ;; Direct writes bypass SBCL's cached file-position tracking.
                (when (typep target 'sb-sys:fd-stream)
                  (setf (sb-impl::fd-stream-file-position target) -1)))
              (progn (write-sequence bytes target) (finish-output target))))
      (stream-error (condition)
        (%throw '|java/io/IOException| (princ-to-string condition)))
      (sb-posix:syscall-error (condition)
        (%throw '|java/io/IOException| (sb-int:strerror (sb-posix:syscall-errno condition))))))
  nil)

(defmethod |writeBytes([BIIZ)V| ((fos |java/io/FileOutputStream|) byte-array offset length append?)
  (declare (ignore append?))
  (unless byte-array (%throw '|java/lang/NullPointerException|))
  (unless (<= 0 offset (+ offset length) (length (java-array-data byte-array)))
    (%throw '|java/lang/IndexOutOfBoundsException|))
  ;; Like the JDK's native writeBytes, an empty valid region needs no fd.
  (unless (zerop length)
    (let ((bytes (make-array length :element-type '(unsigned-byte 8))))
      (dotimes (index length)
        (setf (aref bytes index) (logand (jaref byte-array (+ offset index)) #xff)))
      (%write-file-output-bytes fos bytes))))

(defmethod |write(IZ)V| ((fos |java/io/FileOutputStream|) byte append?)
  (declare (ignore append?))
  (%write-file-output-bytes fos (make-array 1 :element-type '(unsigned-byte 8)
                                            :initial-element (logand byte #xff))))

;; Embedding code can supply Lisp streams with their own buffers.  Preserve
;; Java's no-op flush after close while flushing any still-open host stream.
(defmethod |flush()V| ((fos |java/io/FileOutputStream|))
  (let ((fd (%stream-fd fos)))
    (cond
      ((eq fd 1) (force-output *standard-output*))
      ((eq fd 2) (force-output *error-output*))
      ;; FileOutputStream inherits OutputStream's no-op flush, including after
      ;; close.  Only flush our extra Lisp buffer while it is still open.
      ((and (streamp fd) (open-stream-p fd)) (force-output fd)))))

(defun %open-descriptor-stream (path direction flags &optional (buffering :full))
  "Open PATH with OS FLAGS, retaining descriptor ownership only on success."
  (let ((fd (sb-posix:open path flags #o666)))
    (unwind-protect
         (progn
           ;; POSIX FD_CLOEXEC is bit zero; SB-POSIX does not export it.
           (sb-posix:fcntl fd sb-posix:f-setfd 1)
           (prog1 (sb-sys:make-fd-stream fd
                                      :input (not (null (member direction '(:input :io))))
                                      :output (not (null (member direction '(:output :io))))
                                      :element-type '(unsigned-byte 8)
                                      :buffering buffering :auto-close t
                                      :pathname (merge-pathnames (sb-ext:parse-native-namestring path)))
             (setf fd nil)))
      (when fd (ignore-errors (sb-posix:close fd))))))

(defmethod |open0(Ljava/lang/String;Z)V| ((fos |java/io/FileOutputStream|) filename append?)
  (%with-file-open-errors (filename)
    (let* ((flags (logior sb-posix:o-wronly sb-posix:o-creat
                          (if (and append? (not (zerop append?)))
                              sb-posix:o-append sb-posix:o-trunc)))
           (stream (%open-descriptor-stream (lstring filename) :output flags :none))
           (fd (slot-value fos '|fd|)))
      ;; O_TRUNC retains the inode and existing permissions.  Unbuffered
      ;; streams expose errors and written data before the native call returns.
      (if (and fd (slot-exists-p fd '|fd|))
          (progn
            (setf (slot-value fd '|fd|) stream)
            (when (slot-exists-p fd '|append|)
              (setf (slot-value fd '|append|) (if (zerop append?) 0 1))))
          (setf (slot-value fos '|fd|) stream)))))

(defmethod |close0()V| (this)
  "Native close for file streams."
  (when (slot-exists-p this '|fd|)
    (let* ((holder (slot-value this '|fd|))
           (owner (if (and (typep holder 'standard-object) (slot-exists-p holder '|fd|))
                      holder this))
           (fd (slot-value owner '|fd|)))
      ;; Invalidate the shared FileDescriptor before close can release its OS
      ;; number for reuse. Repeated closes must not affect an unrelated file.
      (setf (slot-value owner '|fd|) -1)
      (%close-file-descriptor fd)))
  nil)

(defmethod |getEnclosingMethod0()[Ljava/lang/Object;| ((this |java/lang/Class|))
  "Return Object[3] {enclosing Class, method name, method descriptor} from
the EnclosingMethod attribute, or nil for classes that aren't local or
anonymous.  Name/descriptor are null when enclosed by an initializer."
  (let ((lclass (get-ldk-class-for-java-class this)))
    (when lclass
      (when-let ((em (gethash "EnclosingMethod" (attributes lclass))))
        (let* ((cp (constant-pool lclass))
               (outer-ref (aref cp (car em)))
               (outer-name (slot-value (aref cp (index outer-ref)) 'value))
               (outer-class (%java-class-loading outer-name lclass)))
          (when outer-class
            (multiple-value-bind (mname mdesc)
                (if (zerop (cdr em))
                    (values nil nil)
                    (let ((nat (aref cp (cdr em))))
                      (values (slot-value (aref cp (slot-value nat 'name-index)) 'value)
                              (slot-value (aref cp (slot-value nat 'type-descriptor-index)) 'value))))
              (make-java-array
               :component-class (%get-java-class-by-bin-name "java/lang/Object")
               :initial-contents (vector outer-class
                                         (and mname (jstring mname))
                                         (and mdesc (jstring mdesc)))))))))))

(defmethod |getDeclaringClass0()Ljava/lang/Class;| ((this |java/lang/Class|))
  "Return the class that declares THIS as a member, from the InnerClasses
attribute; nil for top-level, local, and anonymous classes."
  (let ((lclass (get-ldk-class-for-java-class this)))
    (when lclass
      (loop for ic in (gethash "InnerClasses" (attributes lclass))
            when (and (not (zerop (outer-class-info-index ic)))
                      (let* ((inner-ref (aref (constant-pool lclass) (inner-class-info-index ic)))
                             (inner-name (slot-value (aref (constant-pool lclass) (index inner-ref))
                                                     'value)))
                        (string= inner-name (name lclass))))
              return (let* ((outer-ref (aref (constant-pool lclass) (outer-class-info-index ic)))
                            (outer-name (slot-value (aref (constant-pool lclass) (index outer-ref))
                                                    'value)))
                       (%java-class-loading outer-name lclass))))))

(defun %java-class-loading (bin-name from-lclass)
  "The java.lang.Class for BIN-NAME, loading the class (through the loader
of FROM-LCLASS) when it isn't loaded: a nested class can be loaded before
the class declaring it."
  (or (%get-java-class-by-bin-name bin-name t
                                   (and (slot-boundp from-lclass 'ldk-loader)
                                        (ldk-loader from-lclass)))
      (%get-java-class-by-bin-name bin-name t)
      (let ((lclass (ignore-errors (classload bin-name))))
        (and lclass (java-class lclass)))))

(defun %inner-class-entry (lclass)
  "LCLASS's own entry in its InnerClasses attribute, or NIL."
  (loop for ic in (gethash "InnerClasses" (attributes lclass))
        when (let* ((inner-ref (aref (constant-pool lclass) (inner-class-info-index ic)))
                    (inner-name (slot-value (aref (constant-pool lclass) (index inner-ref))
                                            'value)))
               (string= inner-name (name lclass)))
          return ic))

(defmethod |getSimpleBinaryName0()Ljava/lang/String;| ((this |java/lang/Class|))
  "The simple name THIS's InnerClasses entry gives it; nil for a top-level
or anonymous class (Class.isAnonymousClass, getSimpleName)."
  (let* ((lclass (and (eq 0 (|isArray()Z| this))
                      (eq 0 (|isPrimitive()Z| this))
                      (get-ldk-class-for-java-class this)))
         (entry (and lclass (%inner-class-entry lclass))))
    (when entry
      (let ((index (slot-value entry 'inner-name-index)))
        (unless (zerop index)
          (jstring (slot-value (aref (constant-pool lclass) index) 'value)))))))

(defmethod |getPermittedSubclasses0()[Ljava/lang/Class;| ((this |java/lang/Class|))
  "Resolve permitted direct subtypes without initializing them; omit missing types."
  (let ((class (get-ldk-class-for-java-class this)))
    (when (and class (hash-table-p (attributes class)))
      (multiple-value-bind (names sealed-p) (gethash "PermittedSubclasses" (attributes class))
        (when sealed-p
          (make-java-array
           :component-class (%get-java-class-by-bin-name "java/lang/Class")
           :initial-contents
           (loop for name in names
                 for child = (handler-case (%resolve-class name (ldk-loader class))
                               (|condition-java/lang/LinkageError| () nil))
                 when (and child
                           (some (lambda (parent)
                                   (eq class (%resolve-class parent (ldk-loader child))))
                                 (append (when (super child) (list (super child)))
                                         (coerce (interfaces child) 'list))))
                   collect (java-class child))))))))

(defmethod |getBooleanAttributes0(Ljava/io/File;)I| ((this |java/io/UnixFileSystem|) file)
  (handler-case
      (let ((attr (org.shirakumo.file-attributes:decode-attributes
                   (org.shirakumo.file-attributes:attributes (lstring (slot-value file '|path|))))))
        (+ #x01 ;; :EXISTS
           (if (getf attr :NORMAL) #x02 #x00)
           (if (getf attr :DIRECTORY) #x04 #x00)))
    (sb-int:simple-file-error (e)
      (declare (ignore e))
      0)))

(defmethod |getNameMax0(Ljava/lang/String;)J| ((this |java/io/UnixFileSystem|) path)
  "The longest file name allowed in the directory PATH: pathconf's
_PC_NAME_MAX.  File.createTempFile asks for it."
  (declare (ignore path))
  255)

;;; The rest of UnixFileSystem's natives.  Results follow the JDK's: 1 or 0
;;; for success or failure (an IOException only where the JDK throws one).

(defun %file-path (file)
  (lstring (slot-value file '|path|)))

(defmethod |createFileExclusively0(Ljava/lang/String;)Z| ((this |java/io/UnixFileSystem|) path)
  "Create the file PATH unless it exists: 1 if this created it, 0 if it
already existed."
  (handler-case
      (progn
        (sb-posix:close (sb-posix:open (lstring path)
                                       (logior sb-posix:o-wronly sb-posix:o-creat sb-posix:o-excl)
                                       #o666))
        1)
    (sb-posix:syscall-error (e)
      (if (= (sb-posix:syscall-errno e) sb-posix:eexist)
          0
          (error (%lisp-condition
                  (%make-throwable '|java/io/IOException|
                                   (sb-int:strerror (sb-posix:syscall-errno e)))))))))

(defmethod |createDirectory0(Ljava/io/File;)Z| ((this |java/io/UnixFileSystem|) file)
  (handler-case (progn (sb-posix:mkdir (%file-path file) #o777) 1)
    (error () 0)))

(defmethod |rename0(Ljava/io/File;Ljava/io/File;)Z| ((this |java/io/UnixFileSystem|) from to)
  (handler-case (progn (sb-posix:rename (%file-path from) (%file-path to)) 1)
    (error () 0)))

(defmethod |setLastModifiedTime0(Ljava/io/File;J)Z| ((this |java/io/UnixFileSystem|) file millis)
  (handler-case
      (let* ((path (%file-path file))
             (atime (sb-posix:stat-atime (sb-posix:stat path))))
        (sb-posix:utimes path atime (/ millis 1000))
        1)
    (error () 0)))

(defmethod |setPermission0(Ljava/io/File;IZZ)Z| ((this |java/io/UnixFileSystem|) file access enable owner-only)
  "ACCESS is 4 (read), 2 (write) or 1 (execute): set or clear it for the
owner, or for everyone unless OWNER-ONLY."
  (handler-case
      (let* ((path (%file-path file))
             (mode (sb-posix:stat-mode (sb-posix:stat path)))
             (bits (if (eql owner-only 1)
                       (ash access 6)
                       (logior (ash access 6) (ash access 3) access))))
        (sb-posix:chmod path (logand (if (eql enable 1) (logior mode bits) (logandc2 mode bits))
                                     #o7777))
        1)
    (error () 0)))

(defmethod |setReadOnly0(Ljava/io/File;)Z| ((this |java/io/UnixFileSystem|) file)
  (handler-case
      (let* ((path (%file-path file))
             (mode (sb-posix:stat-mode (sb-posix:stat path))))
        (sb-posix:chmod path (logand (logandc2 mode #o222) #o7777))
        1)
    (error () 0)))

(defmethod |getSpace0(Ljava/io/File;I)J| ((this |java/io/UnixFileSystem|) file kind)
  "Total, free or usable space of FILE's file system.  OpenLDK doesn't ask
the file system: 0, what the JDK answers when it can't tell."
  (declare (ignore file kind))
  0)

(defmethod |getLength0(Ljava/io/File;)J| ((this |java/io/UnixFileSystem|) file)
  "Length of FILE in bytes, or 0 if it cannot be read, as the JDK's native does."
  (handler-case
      (with-open-file (stream (lstring (slot-value file '|path|)) :element-type '(unsigned-byte 8))
        (file-length stream))
    (error () 0)))

(defmethod |getLength(Ljava/io/File;)J| ((this |java/io/UnixFileSystem|) file)
  "Legacy name for getLength0 — older JDKs call this directly."
  (|getLength0(Ljava/io/File;)J| this file))

(defmethod |delete0(Ljava/io/File;)Z| ((this |java/io/UnixFileSystem|) file)
  "Delete the file (or empty directory); return 1 on success, 0 on failure."
  (handler-case
      (let ((path (lstring (slot-value file '|path|))))
        (if (sb-posix:s-isdir (sb-posix:stat-mode (sb-posix:lstat path)))
            (sb-posix:rmdir path)
            (sb-posix:unlink path))
        1)
    (error () 0)))

(defmethod |list0(Ljava/io/File;)[Ljava/lang/String;| ((this |java/io/UnixFileSystem|) file)
  "The names of the entries of the directory FILE, subdirectories included
and . and .. not: an empty array for an empty directory, NIL if FILE isn't
a directory that can be read, as the JDK's native answers."
  (let ((dir (ignore-errors (sb-posix:opendir (lstring (slot-value file '|path|))))))
    (when (and dir (not (sb-alien:null-alien dir)))
      (unwind-protect
           (make-java-array
            :component-class (%get-java-class-by-bin-name "java/lang/String")
            :initial-contents
            (coerce (loop for entry = (sb-posix:readdir dir)
                          until (sb-alien:null-alien entry)
                          for name = (sb-posix:dirent-name entry)
                          unless (member name '("." "..") :test #'string=)
                            collect (jstring name))
                    'vector))
        (sb-posix:closedir dir)))))

(defmethod |list(Ljava/io/File;)[Ljava/lang/String;| ((this |java/io/UnixFileSystem|) file)
  "Legacy name for list0 — older JDKs call this directly."
  (|list0(Ljava/io/File;)[Ljava/lang/String;| this file))

(defun |java/security/AccessController.doPrivileged(Ljava/security/PrivilegedAction;Ljava/security/AccessControlContext;)Ljava/lang/Object;| (action context)
  (declare (ignore context))
  (let ((result (|run()Ljava/lang/Object;| action)))
    result))

(defun |java/net/InetAddress.init()V| ()
  nil)

(defun |java/io/ObjectStreamClass.initNative()V| ()
  nil)

(defmethod |java/lang/System.gc()V| ()
  ;; Match Runtime.gc, including references retained across image restore.
  (sb-ext:gc :gen 7))

(defun |java/lang/Thread.sleep(J)V| (milliseconds)
  "Legacy public native sleep(J) — newer JDKs compile this as bytecode
delegating to sleep0/sleepNanos0, clobbering this defun; the real
implementation therefore lives in %java-thread-sleep."
  (%java-thread-sleep milliseconds))

(defun %java-thread-sleep (milliseconds)
  "Sleep for the specified milliseconds, checking for interruption."
  ;; Get current thread
  (let* ((current-java-thread (or #+sb-fiber
                                  (when (in-fiber-p)
                                    (gethash (current-thread-identity) *fiber-to-java-threads*))
                                  (gethash (bordeaux-threads:current-thread) *lisp-to-java-threads*))))
    ;; Check if interrupted before sleeping
    (when (and current-java-thread
               (gethash current-java-thread *thread-interrupted*))
      ;; Clear interrupt flag and throw InterruptedException
      (%clear-java-thread-interrupt current-java-thread)
      (let ((exc (%make-java-instance "java/lang/InterruptedException")))
        (|<init>()V| exc)
        (error (%lisp-condition exc))))
    ;; Only this operation accepts asynchronous InterruptedException delivery.
    (let ((*java-thread-sleeping* t)
          (*java-thread-blocking-state* (list #xe1 nil nil)))
      (when (and current-java-thread (gethash current-java-thread *thread-interrupted*))
        (%clear-java-thread-interrupt current-java-thread)
        (let ((*java-thread-sleeping* nil)) (%throw '|java/lang/InterruptedException|)))
      ;; Sleep — use fiber-sleep when in fiber context
      #+sb-fiber
      (if (in-fiber-p)
          (uiop:symbol-call :sb-thread :fiber-sleep (/ milliseconds 1000.0))
          (sleep (/ milliseconds 1000.0)))
      #-sb-fiber
      (sleep (/ milliseconds 1000.0)))
    ;; Check if interrupted after sleeping
    (when (and current-java-thread
               (gethash current-java-thread *thread-interrupted*))
      ;; Clear interrupt flag and throw InterruptedException
      (%clear-java-thread-interrupt current-java-thread)
      (let ((exc (%make-java-instance "java/lang/InterruptedException")))
        (|<init>()V| exc)
        (error (%lisp-condition exc))))))

(defun %string-to-signed-byte-array (string)
  "Encode STRING as a Java byte[] using the platform encoding.  OpenLDK
pins sun.jnu.encoding/native.encoding to UTF-8 (*encoding-properties*),
so UTF-8 here matches what Java code reads from those properties.
Java byte[] data stores signed bytes, so octets are converted."
  (make-java-array
   :component-class "B"
   :initial-contents (map 'list
                          (lambda (b) (if (> b 127) (- b 256) b))
                          (flexi-streams:string-to-octets string :external-format :utf-8))))

(defun |java/lang/ProcessEnvironment.environ()[[B| ()
  (let ((env (remove-if (lambda (e) (not (find #\= e))) (sb-ext:posix-environ))))
    (let ((jenvs (make-java-array :component-class "[B" :size (* 2 (length env)))))
      (loop for kv in env
            for i from 0 by 2
            for p = (position #\= kv)
            do (progn
                 (setf (jaref jenvs i) (%string-to-signed-byte-array (subseq kv 0 p)))
                 (setf (jaref jenvs (+ i 1)) (%string-to-signed-byte-array (subseq kv (1+ p))))))
      jenvs)))

(defun |java/lang/System.identityHashCode(Ljava/lang/Object;)I| (objref)
  ;; Return a unique, stable identity hash per object instance.
  ;; SBCL's sxhash returns the same value for all instances of the same
  ;; CLOS class, which breaks identity-based hash tables like Kawa's
  ;; AbstractWeakHashTable. Instead, assign a unique counter-based hash
  ;; on first access (mimicking JVM object header identity hash).
  (if (null objref)
      0
      (bordeaux-threads:with-lock-held (*identity-hash-counter-lock*)
        (or (gethash objref *identity-hash-table*)
            (let ((hash (unsigned-to-signed-integer
                         (logand (incf *identity-hash-counter*) #xFFFFFFFF))))
              (setf (gethash objref *identity-hash-table*) hash)
              hash)))))

(defun |java/lang/Thread.yield()V| ()
  #+sb-fiber
  (when (in-fiber-p)
    (uiop:symbol-call :sb-thread :fiber-yield)
    (return-from |java/lang/Thread.yield()V|))
  ;; Platform thread: actually yield the OS thread; a no-op busy-spins j.u.c
  ;; spin-then-yield backoff loops.  See Thread.yield0 in native/threads.lisp.
  (sb-thread:thread-yield)
  nil)

(defun %set-thread-status (thread status)
  "Set THREAD's holder.threadStatus (JVMTI state bits) when the field is
present.  Consumed by Thread.getState()/start()/getThreadGroup()."
  (when (slot-exists-p thread '|holder|)
    (let ((holder (slot-value thread '|holder|)))
      (when (and holder (slot-exists-p holder '|threadStatus|))
        (setf (slot-value holder '|threadStatus|) status)))))

(defmethod |start0()V| ((thread |java/lang/Thread|))
  "Start a new Lisp thread that executes the Thread's run() method."
  ;; JDK 25's Thread.isAlive() is a bytecode method delegating to alive(),
  ;; which returns (eetop != 0).  The JIT compiles those over any Lisp override,
  ;; so we keep eetop itself truthful: non-zero while the thread runs, zero once
  ;; it terminates.  Set it in the parent before the child can be observed.
  (when (slot-exists-p thread '|eetop|)
    (setf (slot-value thread '|eetop|) 1))
  ;; Thread.holder.threadStatus drives getState()/restart: Thread.start()
  ;; throws IllegalThreadStateException unless threadStatus == 0 (NEW), and
  ;; VM.toThreadState maps bit 0x4 -> RUNNABLE, 0x2 -> TERMINATED.  Mark
  ;; RUNNABLE now; the cleanup below marks TERMINATED so a dead thread can't
  ;; be restarted and getThreadGroup() returns null after termination.
  (%set-thread-status thread 4)
  ;; A short-lived child must not exit before its parent publishes the mapping.
  (bt:with-recursive-lock-held (*java-thread-registry-lock*)
   (let* ((debug-codegen *debug-codegen*)
         (lisp-thread
          (bordeaux-threads:make-thread
           (lambda ()
             (let ((*scoped-value-cache* nil))
               ;; Register this Lisp thread with the Java Thread
               (bt:with-recursive-lock-held (*java-thread-registry-lock*)
                 (setf (gethash (bordeaux-threads:current-thread) *lisp-to-java-threads*) thread))
               (unwind-protect
                    ;; Call the Thread's run() method
                    (handler-case
                        (handler-bind
                            ((error
                               (lambda (e)
                                 (when debug-codegen
                                   (format *error-output*
                                           "~&Error signalled in ~A: ~A~%" thread e)
                                   (sb-debug:print-backtrace
                                    :stream *error-output* :count 60)))))
                          (|run()V| thread))
                      (error (e)
                        (%dispatch-java-thread-error thread e)))
                 ;; The VM must run Thread.exit after run(), including for
                 ;; BoundVirtualThread. Its container notification lets a
                 ;; structured task scope observe completion of its children.
                 (%run-java-thread-exit thread)
                 ;; On termination: mark not-alive (eetop=0) and wake any thread
                 ;; blocked in Thread.join(), which does `while (isAlive()) wait(0)`
                 ;; on this Thread object -- the JVM notifies join waiters here.
                 (when (slot-exists-p thread '|eetop|)
                   (setf (slot-value thread '|eetop|) 0))
                 (%set-thread-status thread 2) ; TERMINATED
                 (ignore-errors
                  (monitor-enter thread)
                  (unwind-protect
                       (|notifyAll()V| thread)
                    (monitor-exit thread))))))
           :name (format nil "Java-Thread-~A" (slot-value thread '|name|)))))
    ;; Store the Lisp thread in our mapping
    (setf (gethash thread *java-threads*) lisp-thread))))

(defmethod |interrupt0()V| ((thread |java/lang/Thread|))
  "Interrupt the thread by signaling the underlying Lisp thread (or fiber)."
  ;; The interrupted status is maintained in the *thread-interrupted* hash table.
  (setf (gethash thread *thread-interrupted*) t)
  ;; Remove the waiter synchronously so notify() cannot consume its wakeup.
  (when-let ((waiting (gethash thread *java-thread-waits*)))
    (let ((monitor (first waiting)) (identity (second waiting)))
      (bt:with-lock-held ((mutex monitor))
        (setf (wait-set monitor) (remove identity (wait-set monitor)))
        (%monitor-wake monitor))))
  ;; If the thread has a fiber, set its park permit to wake it.
  #+sb-fiber
  (let ((fiber (gethash thread *java-to-fibers*)))
    (when fiber
      (setf (gethash fiber *fiber-park-flags*) t)
      (return-from |interrupt0()V|)))
  ;; A parked thread just returns from park, as LockSupport specifies; it
  ;; checks the interrupted status itself, so don't throw into it.
  (when (%unpark-java-thread thread)
    (return-from |interrupt0()V|))
  ;; If the thread has an associated Lisp thread, interrupt it.
  (let ((lisp-thread (or (gethash thread *java-threads*)
                        (loop for carrier being the hash-keys of *lisp-to-java-threads*
                                using (hash-value java-thread)
                              when (eq java-thread thread) return carrier))))
    (when (and lisp-thread (bt:thread-alive-p lisp-thread))
      ;; Interrupt the Lisp thread to wake it from blocking operations
      (handler-case (bordeaux-threads:interrupt-thread
       lisp-thread
       (lambda ()
         ;; Check if interrupted flag is set and throw InterruptedException
         (when (and *java-thread-sleeping* (gethash thread *thread-interrupted*))
           (let ((*java-thread-sleeping* nil))
             (%clear-java-thread-interrupt thread)
             (%throw '|java/lang/InterruptedException|)))))
        (sb-thread:interrupt-thread-error () nil)))))

(defun %java-notify (objref)
  "Notify one waiter on any Java object representation, including arrays."
  (let* ((monitor (%get-monitor objref))
         (mutex (mutex monitor))
         (cv (condition-variable monitor))
         (current-thread (current-thread-identity)))
    (bordeaux-threads:with-lock-held (mutex)
      (unless (eq (owner monitor) current-thread)
        (error (%lisp-condition (%make-throwable '|java/lang/IllegalMonitorStateException|))))
      (when (wait-set monitor)
        (pop (wait-set monitor))
        (sb-thread:condition-broadcast cv)))))

(defmethod |notify()V| ((object |java/lang/Object|)) (%java-notify object))
(defmethod |notify()V| ((object java-array)) (%java-notify object))

(defun |java/util/concurrent/atomic/AtomicLong.VMSupportsCS8()| ()
  0)

(defun |sun/reflect/NativeMethodAccessorImpl.invoke0(Ljava/lang/reflect/Method;Ljava/lang/Object;[Ljava/lang/Object;)| (method object args)
  (when *debug-trace*
    (format t "~&~V@A trace: entering sun/reflect/NativeMethodAccessorImpl.invoke0(Ljava/lang/reflect/Method;Ljava/lang/Object;[Ljava/lang/Object;)~A~%"
            (incf *call-nesting-level* 1) "*"
            (list method object args)))
  (unwind-protect
       (progn
         ;; Unbox primitive wrapper types only when the parameter type is primitive
         (when args
           (let ((param-types (slot-value method '|parameterTypes|)))
             (dotimes (i (length (java-array-data args)))
               (let ((arg (jaref args i))
                     (param-type (when param-types (jaref param-types i))))
                 ;; Only unbox if parameter type is primitive (isPrimitive returns 1)
                 (when (and param-type (eq 1 (|isPrimitive()Z| param-type)))
                   (cond
                     ((typep arg '|java/lang/Integer|)
                      (setf (jaref args i) (slot-value arg '|value|)))
                     ((typep arg '|java/lang/Long|)
                      (setf (jaref args i) (slot-value arg '|value|)))
                     ((typep arg '|java/lang/Boolean|)
                      (setf (jaref args i) (slot-value arg '|value|)))
                     ((typep arg '|java/lang/Byte|)
                      (setf (jaref args i) (slot-value arg '|value|)))
                     ((typep arg '|java/lang/Short|)
                      (setf (jaref args i) (slot-value arg '|value|)))
                     ((typep arg '|java/lang/Character|)
                      (setf (jaref args i) (slot-value arg '|value|)))
                     ((typep arg '|java/lang/Float|)
                      (setf (jaref args i) (slot-value arg '|value|)))
                     ((typep arg '|java/lang/Double|)
                      (setf (jaref args i) (slot-value arg '|value|)))))))))
         (let* ((java-class (slot-value method '|clazz|))
                (class-name (substitute #\/ #\. (lstring (slot-value java-class '|name|))))
                (is-static (not (eq 0 (logand #x8 (slot-value method '|modifiers|)))))
                ;; For static methods, use the class's package; for instance methods use :openldk
                (java-loader (slot-value java-class '|classLoader|))
                (ldk-loader (get-ldk-loader-for-java-loader java-loader))
                (descriptor (build-method-descriptor
                             (slot-value method '|parameterTypes|)
                             (slot-value method '|returnType|)))
                (method-name (lispize-method-name
                              (if is-static
                                  (concatenate 'string
                                               class-name
                                               "."
                                               (lstring (slot-value method '|name|))
                                               descriptor)
                                  (concatenate 'string
                                               (lstring (slot-value method '|name|))
                                               descriptor))))
                ;; Static methods are in the class's loader package, instance methods in :openldk
                (pkg (if is-static
                         (class-package class-name ldk-loader)
                         (find-package :openldk)))
                (result (apply (intern method-name pkg)
                               (if is-static
                                   (if args (coerce (java-array-data args) 'list) nil)
                                   (cons object (if args (coerce (java-array-data args) 'list) nil))))))
           (when *debug-trace*
             (format t "~&~V@A trace: result = ~A~%"
                     *call-nesting-level* "*" result))
           result))
    (when *debug-trace*
      (incf *call-nesting-level* -1))))

(defun %reflect-new-array (class dimensions)
  "Allocate reflection arrays using the actual component Class and loader."
  (unless class (%throw '|java/lang/NullPointerException|))
  (let* ((name (lstring (slot-value class '|name|)))
         (rank (+ (length dimensions) (or (position-if-not (lambda (c) (char= c #\[)) name) 0))))
    (when (or (null dimensions) (> rank 255) (string= name "void"))
      (%throw '|java/lang/IllegalArgumentException|))
    ;; Validate all dimensions even when an outer dimension is zero.
    (when (some #'minusp dimensions) (%throw '|java/lang/NegativeArraySizeException|))
    (let ((components (list class)))
      (dotimes (i (1- (length dimensions)))
        (push (|getClass()Ljava/lang/Class;| (make-java-array :component-class (car components)))
              components))
      (labels ((allocate (types sizes)
                 (if (cdr sizes)
                     (let ((array (make-java-array :component-class (car types) :size (car sizes))))
                       (dotimes (index (car sizes) array)
                         (setf (jaref array index) (allocate (cdr types) (cdr sizes)))))
                     (make-java-array :component-class (car types) :size (car sizes)))))
        (allocate components dimensions)))))

(defun |java/lang/reflect/Array.newArray(Ljava/lang/Class;I)Ljava/lang/Object;| (class size)
  (%reflect-new-array class (list size)))

(defun |java/lang/reflect/Array.multiNewArray(Ljava/lang/Class;[I)Ljava/lang/Object;| (class dimensions)
  (unless dimensions (%throw '|java/lang/NullPointerException|))
  (%reflect-new-array class (coerce (java-array-data dimensions) 'list)))

(defmethod |findLoadedClass0(Ljava/lang/String;)Ljava/lang/Class;| ((loader |java/lang/ClassLoader|) name)
  "Return the class NAME if LOADER already loaded it (its own map only --
parent delegation is the Java caller's job)."
  (|java/lang/ClassLoader.findLoadedClass0(Ljava/lang/String;)Ljava/lang/Class;| loader name))

(defun %binary-class-name-p (name)
  "Whether NAME can be searched by the system class loader."
  ;; Empty names must not reach CLASSLOAD's internal assertion.  Slash and
  ;; array descriptors are not binary names.  Other names still go through
  ;; the classpath: a matching file with a different declared name must
  ;; produce NoClassDefFoundError rather than ClassNotFoundException.
  (and (plusp (length name))
       (not (find (char name 0) '(#\[ #\.)))
       (not (find #\Null name))
       (not (find #\/ name))))

(defmethod |findClass(Ljava/lang/String;)Ljava/lang/Class;| :around ((loader |java/lang/ClassLoader|) name)
  "Serve the application classpath only for OpenLDK's system loader."
  (if (eq loader *boot-class-loader*)
      (let* ((text (lstring (%non-null name)))
             (illegal-name (and (plusp (length text))
                                (not (find (char text 0) '(#\. #\/)))
                                (not (find #\Null text))
                                (or (char= (char text 0) #\[) (find #\/ text)))))
        ;; The JDK's classpath search can find bytes for an illegal slash or
        ;; array name.  preDefineClass then throws NoClassDefFoundError.  A
        ;; missing file still produces ClassNotFoundException.  Runtime-image
        ;; resources are not application-classpath candidates here.
        (when (and illegal-name
                   (some (lambda (entry)
                           (and (typep entry '(or dir-classpath-entry jar-classpath-entry))
                                (get-resource-url entry (format nil "~A.class" (substitute #\/ #\. text)))))
                         *classpath*))
          (%throw '|java/lang/NoClassDefFoundError| (format nil "IllegalName: ~A" text)))
        (let ((class (and (%binary-class-name-p text)
                          (%classload-system (substitute #\/ #\. text)))))
          (if (and class (not (eql 1 (|isPrimitive()Z| (java-class class)))))
              (java-class class)
              (%throw '|java/lang/ClassNotFoundException| text))))
      (call-next-method)))

(defun %find-bootstrap-class (name)
  "Find a runtime-image class without consulting the application classpath."
  (let* ((binary-name (substitute #\/ #\. (lstring name)))
         (slash (position #\/ binary-name :from-end t)))
    (when (and slash (jdk-class-p binary-name)
               (%runtime-image-package-p (subseq binary-name 0 slash)))
      (when-let ((class (%classload-system binary-name)))
        (java-class class)))))

(defmethod |findBootstrapClass(Ljava/lang/String;)Ljava/lang/Class;| ((loader |java/lang/ClassLoader|) name)
  (%find-bootstrap-class name))

;; JDK 17: findBootstrapClass is a private static native method.  Only the
;; runtime-image classes: neither application classes nor classes belonging
;; to user loaders are visible through a null parent.
(defun |java/lang/ClassLoader.findBootstrapClass(Ljava/lang/String;)Ljava/lang/Class;| (name)
  (%find-bootstrap-class name))

(defvar *native-url-strings* (make-hash-table :test #'eq :weakness :key :synchronized t)
  "URL strings attached to resource URLs constructed by OpenLDK.")

(defun %make-url-from-string (url-string)
  "Create a URL with the JDK's parser and protocol handler fully initialized."
  (let ((*context* nil)
        (url (%make-java-instance "java/net/URL")))
    (invoke-special '|<init>(Ljava/lang/String;)V| '|java/net/URL|
                    (list url (jstring url-string)))
    (setf (gethash url *native-url-strings*) url-string)
    url))

(defun %current-directory-string ()
  "The current directory, as user.dir has it: without a trailing slash."
  (let ((dir (namestring (uiop:getcwd))))
    (if (and (> (length dir) 1) (char= (char dir (1- (length dir))) #\/))
        (subseq dir 0 (1- (length dir)))
        dir)))

(defmethod |getBootstrapResource(Ljava/lang/String;)| ((loader |java/lang/ClassLoader|) name)
  "Find a resource by NAME on the bootstrap classpath."
  (declare (ignore loader))
  (let ((resource-name (lstring name)))
    (when-let (url-string (get-resource-url-on-classpath resource-name))
      (%make-url-from-string url-string))))

(defmethod |getBootstrapResource(Ljava/lang/String;)| ((loader (eql nil)) name)
  "Find a resource by NAME on the bootstrap classpath (static call)."
  (let ((resource-name (lstring name)))
    (when-let (url-string (get-resource-url-on-classpath resource-name))
      (%make-url-from-string url-string))))

(defun |java/lang/ClassLoader.getBootstrapResource(Ljava/lang/String;)| (name)
  "Static native method to find a resource on the bootstrap classpath."
  (let ((resource-name (lstring name)))
    (when-let (url-string (get-resource-url-on-classpath resource-name))
      (%make-url-from-string url-string))))

(defclass/std <resource-input-stream> (|java/io/InputStream|)
  ((lisp-stream :std nil))
  (:documentation "InputStream wrapping a Lisp flexi-stream for resource reading."))

(defmethod |read()| ((this <resource-input-stream>))
  "Read a single byte from the resource stream."
  (unless (slot-value this 'lisp-stream)
    (%throw '|java/io/IOException| "Stream closed"))
  (let ((byte (read-byte (slot-value this 'lisp-stream) nil nil)))
    (if byte byte -1)))

(defmethod |read([BII)I| ((this <resource-input-stream>) byte-array offset length)
  "Read up to LENGTH bytes into BYTE-ARRAY starting at OFFSET.
Java byte arrays hold SIGNED bytes; read-byte yields 0..255."
  (unless byte-array
    (%throw '|java/lang/NullPointerException|))
  (unless (<= 0 offset (+ offset length) (length (java-array-data byte-array)))
    (%throw '|java/lang/IndexOutOfBoundsException|))
  (unless (slot-value this 'lisp-stream)
    (%throw '|java/io/IOException| "Stream closed"))
  (when (zerop length)
    (return-from |read([BII)I| 0))
  (let ((stream (slot-value this 'lisp-stream))
        (bytes-read 0))
    (loop for i from offset below (+ offset length)
          for byte = (read-byte stream nil nil)
          while byte
          do (progn
               (setf (jaref byte-array i) (if (> byte 127) (- byte 256) byte))
               (incf bytes-read)))
    (if (zerop bytes-read) -1 bytes-read)))

(defmethod |close()| ((this <resource-input-stream>))
  "Close the resource stream."
  (when-let (stream (slot-value this 'lisp-stream))
    (close stream)
    (setf (slot-value this 'lisp-stream) nil)))

(defun |java/lang/ClassLoader.getSystemResourceAsStream(Ljava/lang/String;)Ljava/io/InputStream;| (name)
  "Get a system resource as an InputStream."
  (let ((resource-name (lstring name)))
    (when-let (stream (open-resource-on-classpath resource-name))
      (make-instance '<resource-input-stream> :lisp-stream stream))))

(defun %decode-url-path (path)
  "Use the JDK's UTF-8 percent decoder; '+' is a literal path character."
  (let ((class (classload "sun/net/www/ParseUtil")))
    (lstring (funcall (static-method-symbol
                       "sun/net/www/ParseUtil.decode(Ljava/lang/String;)Ljava/lang/String;"
                       (ldk-class-package class)) (jstring path)))))

(defun %parse-jar-url (url-string)
  "Parse a jar: URL and return (jar-path entry-path) or NIL.
   Format: jar:file:/path/to/file.jar!/path/inside/jar"
  (when (starts-with? "jar:file:" url-string)
    (let ((excl-pos (search "!/" url-string)))
      (when excl-pos
        (let ((jar-path (subseq url-string 9 excl-pos))  ; Skip "jar:file:"
              (entry-path (subseq url-string (+ excl-pos 2))))  ; Skip "!/"
          (values (%decode-url-path jar-path) (%decode-url-path entry-path)))))))

;; Note: |openStream()| for java/net/URL is defined in url.lisp after the class is loaded

(defun |java/io/UnixFileSystem.initIDs()V| ()
  nil)

(defmethod |canonicalize0(Ljava/lang/String;)Ljava/lang/String;| ((ufs |java/io/UnixFileSystem|) filename)
  (declare (ignore ufs))
  (let ((path-str (lstring filename)))
    (when *debug-trace*
      (format t "~&DEBUG: canonicalize0 called with: ~S~%" path-str))
    (handler-case
        (let ((result (jstring (or (%realpath path-str) (error "~A doesn't exist" path-str)))))
          (when *debug-trace*
            (format t "~&DEBUG: canonicalize0 truename succeeded: ~S -> ~S~%"
                    path-str (lstring result)))
          result)
      (error (e)
        (when *debug-trace*
          (format t "~&DEBUG: canonicalize0 truename failed: ~S error: ~A~%"
                  path-str e))
        ;; If file doesn't exist, manually resolve to absolute path
        (let* ((pathname (uiop:parse-unix-namestring path-str))
               (absolute-path (if (uiop:absolute-pathname-p pathname)
                                  pathname
                                  (merge-pathnames pathname (uiop:getcwd))))
               (native (uiop:native-namestring absolute-path))
               ;; As realpath(3) would, without a directory's trailing slash.
               (result (jstring (if (and (> (length native) 1) (char= #\/ (char native (1- (length native)))))
                                    (subseq native 0 (1- (length native)))
                                    native))))
          (when *debug-trace*
            (format t "~&DEBUG: canonicalize0 fallback: ~S -> ~S~%"
                    path-str (lstring result)))
          result)))))

(defmethod |checkAccess0(Ljava/io/File;I)Z| ((ufs |java/io/UnixFileSystem|) file access-mode)
  "JDK 25 private native checkAccess0 — must hold the implementation;
the bytecode checkAccess wrapper delegates here and clobbers any
same-named defmethod."
  ;; ACCESS-MODE is 4 (read), 2 (write) or 1 (execute), as access(2)'s.
  (declare (ignore ufs))
  (handler-case (progn (sb-posix:access (lstring (slot-value file '|path|)) access-mode) 1)
    (error () 0)))

(defmethod |checkAccess(Ljava/io/File;I)Z| ((ufs |java/io/UnixFileSystem|) file access-mode)
  "Legacy public native checkAccess — older JDKs call this directly."
  (|checkAccess0(Ljava/io/File;I)Z| ufs file access-mode))

(defmethod |getLastModifiedTime0(Ljava/io/File;)J| ((ufs |java/io/UnixFileSystem|) file)
  "Modification time of FILE in milliseconds, or 0 if it does not exist."
  (declare (ignore ufs))
  (handler-case
      (let ((path (lstring (slot-value file '|path|))))
        (+ (* 1000 (sb-posix:stat-mtime (sb-posix:stat path)))
           (floor (nth-value 1 (%stat-nanoseconds path t)) 1000000)))
    (error () 0)))

(defmethod |getLastModifiedTime(Ljava/io/File;)J| ((ufs |java/io/UnixFileSystem|) file)
  "Legacy name for getLastModifiedTime0 — older JDKs call this directly."
  (|getLastModifiedTime0(Ljava/io/File;)J| ufs file))

(defun |sun/misc/Perf.registerNatives()| ()
  nil)

;; JDK 9+ moved Perf to jdk.internal.perf; ClassLoader.loadClass uses it to
;; count findClass calls.
(defun |jdk/internal/perf/Perf.registerNatives()V| ()
  nil)

(defmethod |createLong(Ljava/lang/String;IIJ)Ljava/nio/ByteBuffer;| (perf name variability units value)
  (classload "java/nio/DirectByteBuffer")
  (let* ((dbb (%make-java-instance "java/nio/DirectByteBuffer"))
         (ptr (%allocate-unsafe-memory 8)))
    (setf (sb-sys:signed-sap-ref-64 (sb-sys:int-sap ptr) 0) value)
    (|<init>(JJ)V| dbb ptr 8)
    dbb))

(defmethod |getBoolean(Ljava/lang/Object;J)Z|((unsafe |sun/misc/Unsafe|) objref ptr)
  (declare (ignore unsafe))
  (let* ((field (gethash ptr *field-offset-table*))
         (key (%unsafe-slot-key ptr))
         (owner (or objref (%unsafe-static-storage field)))
         (v (slot-value owner key)))
    (if v (if (eql v 0) 0 1) 0)))

(defun %unsafe-byte-array-p (object)
  "Return true when OBJECT is a Java byte array."
  (and (typep object 'java-array)
       (let ((component (%array-component-class object)))
         (and (typep component '|java/lang/Class|)
              (string= (lstring (slot-value component '|name|)) "byte")))))

(defun %unsafe-read-byte-array (array offset byte-count)
  "Read BYTE-COUNT native-order bytes from ARRAY at OFFSET as an integer."
  ;; OpenLDK is currently supported on little-endian SBCL/Linux systems.
  (unless (and (<= 0 offset)
               (<= (+ offset byte-count) (length (java-array-data array))))
    (internal-error "Unsafe byte-array read of ~D bytes at ~D exceeds length ~D"
           byte-count offset (length (java-array-data array))))
  (loop for index below byte-count
        sum (ash (logand (jaref array (+ offset index)) #xff)
                 (* index 8))))

(defmethod |getByte(Ljava/lang/Object;J)B|
    ((unsafe |sun/misc/Unsafe|) objref ptr)
  (declare (ignore unsafe))
  (cond
    ((%unsafe-byte-array-p objref)
     (%unsigned-to-signed-byte (logand (jaref objref ptr) #xff)))
    ((null objref)
     (sb-sys:signed-sap-ref-8 (sb-sys:int-sap ptr) 0))
    (t
     (let* ((field (gethash ptr *field-offset-table*))
            (key (intern (mangle-field-name
                          (lstring (slot-value field '|name|)))
                         :openldk)))
       (slot-value objref key)))))

(defmethod |getShort(Ljava/lang/Object;J)S|
    ((unsafe |sun/misc/Unsafe|) objref ptr)
  (declare (ignore unsafe))
  (cond
    ((%unsafe-byte-array-p objref)
     (unsigned-to-signed-short (%unsafe-read-byte-array objref ptr 2)))
    ((null objref)
     (sb-sys:signed-sap-ref-16 (sb-sys:int-sap ptr) 0))
    (t
     (let* ((field (gethash ptr *field-offset-table*))
            (key (intern (mangle-field-name
                          (lstring (slot-value field '|name|)))
                         :openldk)))
       (slot-value objref key)))))

(defmethod |getInt(Ljava/lang/Object;J)I|((unsafe |sun/misc/Unsafe|) objref ptr)
  (declare (ignore unsafe))
  (if (%unsafe-byte-array-p objref)
      (unsigned-to-signed-integer (%unsafe-read-byte-array objref ptr 4))
      (let ((field (gethash ptr *field-offset-table*))
            (key (%unsafe-slot-key ptr)))
        (if objref
            (slot-value objref key)
            (if field
                ;; Static field access: look up the static singleton.
                (slot-value (%unsafe-static-storage field) key)
                (sb-sys:signed-sap-ref-32 (sb-sys:int-sap ptr) 0))))))

(defmethod |putLong(Ljava/lang/Object;JJ)V|((unsafe |sun/misc/Unsafe|) objref ptr value)
  (declare (ignore unsafe))
  (let ((field (gethash ptr *field-offset-table*))
        (key (%unsafe-slot-key ptr)))
    (if objref
        (setf (slot-value objref key) value)
        (if field
            ;; Static field access: look up the static singleton.
            (setf (slot-value (%unsafe-static-storage field) key) value)
            (setf (sb-sys:signed-sap-ref-64 (sb-sys:int-sap ptr) 0) value)))))

(defmethod |putInt(Ljava/lang/Object;JI)V|((unsafe |sun/misc/Unsafe|) objref ptr value)
  (declare (ignore unsafe))
  (let ((field (gethash ptr *field-offset-table*))
        (key (%unsafe-slot-key ptr)))
    (if objref
        (setf (slot-value objref key) value)
        (if field
            ;; Static field access: look up the static singleton.
            (setf (slot-value (%unsafe-static-storage field) key) value)
            (setf (sb-sys:signed-sap-ref-32 (sb-sys:int-sap ptr) 0) value)))))

(defmethod |getLong(Ljava/lang/Object;J)J|((unsafe |sun/misc/Unsafe|) objref ptr)
  (declare (ignore unsafe))
  (if (%unsafe-byte-array-p objref)
      (unsigned-to-signed-long (%unsafe-read-byte-array objref ptr 8))
      (let* ((field (gethash ptr *field-offset-table*))
             (key (when field
                    (intern (mangle-field-name
                             (lstring (slot-value field '|name|)))
                            :openldk))))
        (if objref
            (slot-value objref key)
            (if field
                ;; Static field access: look up the static singleton.
                (let* ((clazz (slot-value field '|clazz|))
                       (lname (lstring (slot-value clazz '|name|)))
                       (bin-name (substitute #\/ #\. lname))
                       (pkg (%java-class-package clazz)))
                  (slot-value
                   (eval (intern (format nil "+static-~A+" bin-name) pkg)) key))
                (sb-sys:signed-sap-ref-64 (sb-sys:int-sap ptr) 0))))))

(defmethod |getFloat(Ljava/lang/Object;J)F|
    ((unsafe |sun/misc/Unsafe|) objref ptr)
  (declare (ignore unsafe))
  (if objref
      (let* ((field (gethash ptr *field-offset-table*))
             (key (intern (mangle-field-name
                           (lstring (slot-value field '|name|)))
                          :openldk)))
        (slot-value objref key))
      (sb-sys:sap-ref-single (sb-sys:int-sap ptr) 0)))

(defmethod |putFloat(Ljava/lang/Object;JF)V|
    ((unsafe |sun/misc/Unsafe|) objref ptr value)
  (declare (ignore unsafe))
  (if objref
      (let* ((field (gethash ptr *field-offset-table*))
             (key (intern (mangle-field-name
                           (lstring (slot-value field '|name|)))
                          :openldk)))
        (setf (slot-value objref key) value))
      (setf (sb-sys:sap-ref-single (sb-sys:int-sap ptr) 0)
            (coerce value 'single-float))))

(defmethod |getDouble(Ljava/lang/Object;J)D|
    ((unsafe |sun/misc/Unsafe|) objref ptr)
  (declare (ignore unsafe))
  (if objref
      (let* ((field (gethash ptr *field-offset-table*))
             (key (intern (mangle-field-name
                           (lstring (slot-value field '|name|)))
                          :openldk)))
        (slot-value objref key))
      (sb-sys:sap-ref-double (sb-sys:int-sap ptr) 0)))

(defmethod |putDouble(Ljava/lang/Object;JD)V|
    ((unsafe |sun/misc/Unsafe|) objref ptr value)
  (declare (ignore unsafe))
  (if objref
      (let* ((field (gethash ptr *field-offset-table*))
             (key (intern (mangle-field-name
                           (lstring (slot-value field '|name|)))
                          :openldk)))
        (setf (slot-value objref key) value))
      (setf (sb-sys:sap-ref-double (sb-sys:int-sap ptr) 0)
            (coerce value 'double-float))))

(defmethod |getLong(J)J| ((unsafe |sun/misc/Unsafe|) ptr)
  (declare (ignore unsafe))
  ;; Convert the integer pointer back to a system address pointer (SAP)
  (let ((sap (sb-sys:int-sap ptr)))
    ;; Dereference the memory to get the long value
    (sb-alien:with-alien ((mem (* sb-alien:long) sap))
      (sb-alien:deref mem 0))))

(defmethod |getLongVolatile(J)| ((unsafe |sun/misc/Unsafe|) ptr)
  ;; FIXME: how is the volatile version different?
  (declare (ignore unsafe))
  ;; Convert the integer pointer back to a system address pointer (SAP)
  (let ((sap (sb-sys:int-sap ptr)))
    ;; Dereference the memory to get the long value
    (sb-alien:with-alien ((mem (* sb-alien:long) sap))
      (sb-alien:deref mem 0))))

(defmethod |setMemory(Ljava/lang/Object;JJB)V| ((unsafe |sun/misc/Unsafe|) obj ptr size byte)
  (assert (null obj))
  (%unsafe-fill-memory ptr size byte))

(defun |java/lang/Shutdown.beforeHalt()V| ()
  nil)


;;; Vector API operations use the JDK's Java fallback implementations. Report
;;; no VM vector intrinsics rather than advertising the host CPU's SIMD support.
(defun |jdk/internal/vm/vector/VectorSupport.registerNatives()I| ()
  0)

(defun |jdk/internal/vm/vector/VectorSupport.getMaxLaneCount(Ljava/lang/Class;)I| (element-type)
  (declare (ignore element-type))
  -1)

(defun |jdk/internal/vm/vector/VectorSupport.getCPUFeatures()Ljava/lang/String;| ()
  (jstring ""))
