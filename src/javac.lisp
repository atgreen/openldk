;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: OPENLDK; Base: 10 -*-
;;; SPDX-License-Identifier: GPL-3.0-or-later WITH Classpath-exception-2.0

(in-package :openldk)

(defun javac-main ()
  "Entry point for the pre-dumped javac image. Runs com.sun.tools.javac.Main."
  ;; Match Java FP semantics
  (sb-int:set-floating-point-modes :traps nil)
  ;; Refresh process-dependent system properties baked in at image-build
  ;; time; relative paths would otherwise resolve against the BUILD
  ;; directory's cwd.  Must happen before any NIO use (the default
  ;; filesystem captures user.dir in its clinit, which make-javac-image
  ;; arranged to re-run at first access).
  (handler-case
      (|java/lang/System.setProperty(Ljava/lang/String;Ljava/lang/String;)|
       (openldk::ijstring "user.dir")
       (openldk::jstring (namestring (uiop:getcwd))))
    (condition () nil))
  ;; The default NIO filesystem object baked into the image captured the
  ;; BUILD directory as its defaultDirectory; patch it so relative paths
  ;; resolve against the actual current directory.
  (handler-case
      (let ((get-default (static-method-symbol "java/nio/file/FileSystems.getDefault()"
                                               (loader-package *boot-ldk-class-loader*))))
        (when (fboundp get-default)
          (let ((fs (funcall get-default))
                (cwd (let ((d (string-right-trim "/" (namestring (uiop:getcwd)))))
                       (if (string= d "") "/" d))))
            (when (and fs (slot-exists-p fs '|defaultDirectory|))
              (setf (slot-value fs '|defaultDirectory|)
                    (make-java-array
                     :component-class (%get-java-class-by-bin-name "byte")
                     :initial-contents (map 'vector
                                            (lambda (c)
                                              (let ((b (char-code c)))
                                                (if (> b 127) (- b 256) b)))
                                            cwd)))))))
    (condition (c)
      (format *error-output* "~&; Warning: could not refresh default filesystem cwd: ~A~%" c)))
  (let* ((args (uiop:command-line-arguments))
         (cp (default-javac-classpath)))
    (handler-bind
        ((error (lambda (condition)
                  (cond
                    ((typep condition 'openldk::|condition-java/lang/Throwable|)
                     (let ((throwable (and (slot-boundp condition 'openldk::|objref|)
                                           (slot-value condition 'openldk::|objref|))))
                       (if (typep throwable 'openldk::|java/lang/Throwable|)
                           (progn
                             (format *error-output* "~&Unhandled Java exception:~%")
                             (openldk::%print-java-stack-trace throwable :stream *error-output*)
                             (finish-output *error-output*))
                           (format *error-output* "~&Unhandled Java condition: ~A~%" condition))))
                    (t
                     (format *error-output* "~&Error: ~A~%" condition)
                     (sb-debug:print-backtrace :stream *error-output* :count 50)))
                  (finish-output *error-output*)
                  (uiop:quit 1))))
      (openldk::main "com.sun.tools.javac.Main"
                     args
                     :classpath cp))))

(defun default-javac-classpath ()
  "Pick a sensible default classpath for javac: tools.jar if present, else env or \".\""
  (or (uiop:getenv "CLASSPATH")
      (let* ((jh (uiop:getenv "JAVA_HOME"))
             (tools (and jh
                         (merge-pathnames #P"../lib/tools.jar"
                                          (uiop:ensure-directory-pathname jh)))))
        (when (and tools (uiop:file-exists-p tools))
          (namestring tools)))
      "."))

(defparameter *javac-warmup-classes*
  '("javax/tools/JavaFileManager"
    "javax/tools/StandardJavaFileManager"
    "com/sun/tools/javac/Main"
    "com/sun/tools/javac/main/Main"
    "com/sun/tools/javac/main/Main$Result"
    "java/io/PrintWriter"
    "com/sun/tools/javac/main/Main$1"
    "java/util/RegularEnumSet$EnumSetIterator"
    "com/sun/tools/javac/file/JavacFileManager$1"
    "com/sun/tools/javac/util/Context$Factory"
    "java/util/LinkedHashSet"
    "com/sun/tools/javac/util/ListBuffer"
    "java/util/AbstractQueue"
    "java/util/Queue"
    "com/sun/tools/javac/main/CommandLine"
    "com/sun/tools/javac/file/CacheFSInfo"
    "com/sun/tools/javac/file/FSInfo"
    "com/sun/tools/javac/processing/JavacProcessingEnvironment"
    "javax/annotation/processing/ProcessingEnvironment"
    "com/sun/source/util/Plugin"
    "com/sun/tools/javac/util/ServiceLoader"
    "com/sun/source/util/JavacTask"
    "javax/tools/JavaCompiler$CompilationTask"
    "java/util/concurrent/Callable"
    "com/sun/tools/javac/util/Log$PrefixKind"
    "com/sun/tools/javac/api/BasicJavacTask"
    "com/sun/tools/doclint/DocLint"
    "javax/tools/JavaFileObject"
    "javax/tools/FileObject"
    "com/sun/tools/javac/util/PropagatedException"
    "com/sun/tools/javac/util/ClientCodeException"
    "com/sun/tools/javac/processing/AnnotationProcessingError"
    "com/sun/tools/javac/util/FatalError"
    "com/sun/tools/javac/util/Assert"
    "com/sun/tools/javac/util/JCDiagnostic$Factory"
    "javax/tools/DiagnosticListener"
    "com/sun/tools/javac/util/Log$DefaultDiagnosticHandler"
    "com/sun/tools/javac/util/Log$DiagnosticHandler"
    "com/sun/tools/javac/util/JavacMessages"
    "com/sun/tools/javac/api/Messages"
    "com/sun/tools/javac/util/Log$1"
    "com/sun/tools/javac/util/JCDiagnostic$Factory$1"
    "java/util/MissingResourceException"
    "com/sun/tools/javac/util/List$3"
    "java/util/ResourceBundle$RBClassLoader"
    "java/util/ResourceBundle$RBClassLoader$1"
    "java/util/ResourceBundle$Control"
    "java/util/ResourceBundle$Control$CandidateListCache"
    "java/util/Arrays$ArrayList"
    "java/util/ResourceBundle$CacheKey"
    "java/util/ResourceBundle$BundleReference"
    "java/util/ResourceBundle$CacheKeyReference"
    "java/util/ResourceBundle$SingleFormatControl"
    "java/util/ResourceBundle$LoaderReference"
    "java/util/LinkedList"
    "java/util/AbstractSequentialList"
    "java/util/Deque"
    "java/util/LinkedList$Node"
    "java/lang/CloneNotSupportedException"
    "java/util/ResourceBundle$Control$1"
    "java/util/PropertyResourceBundle"
    "sun/misc/PerfCounter"
    "java/net/URLClassLoader$1"
    "java/lang/ClassFormatError"
    "sun/net/www/protocol/file/FileURLConnection"
    "sun/net/www/URLConnection"
    "java/net/URLClassLoader$2"
    "com/sun/tools/javac/util/BasicDiagnosticFormatter"
    "com/sun/tools/javac/util/AbstractDiagnosticFormatter"
    "com/sun/tools/javac/api/DiagnosticFormatter"
    "com/sun/tools/javac/util/JCDiagnostic$DiagnosticFlag"
    "com/sun/tools/javac/util/BasicDiagnosticFormatter$BasicConfiguration"
    "com/sun/tools/javac/util/AbstractDiagnosticFormatter$SimpleConfiguration"
    "com/sun/tools/javac/api/DiagnosticFormatter$Configuration"
    "com/sun/tools/javac/api/DiagnosticFormatter$Configuration$DiagnosticPart"
    "com/sun/tools/javac/api/DiagnosticFormatter$Configuration$MultilineLimit"
    "java/util/EnumMap"
    "com/sun/tools/javac/util/BasicDiagnosticFormatter$BasicConfiguration$BasicFormatKind"
    "java/util/EnumMap$1"
    "com/sun/tools/javac/util/AbstractDiagnosticFormatter$1"
    "com/sun/tools/javac/code/Printer"
    "com/sun/tools/javac/code/Type$Visitor"
    "com/sun/tools/javac/code/Symbol$Visitor"
    "com/sun/tools/javac/util/RawDiagnosticFormatter"
    "com/sun/tools/javac/util/BasicDiagnosticFormatter$BasicConfiguration$SourcePosition"
    "com/sun/tools/javac/resources/javac"
    "java/text/MessageFormat"
    "java/text/Format"
    "java/util/Locale$Category"
    "java/util/Locale$1"
    "java/lang/NoSuchFieldError"
    "java/lang/IncompatibleClassChangeError"
    "java/text/NumberFormat"
    "java/text/DecimalFormat"
    "java/text/DecimalFormatSymbols"
    "java/text/DateFormat"
    "java/text/SimpleDateFormat"
    "java/text/ChoiceFormat"
    "java/text/FieldPosition"
    "java/util/Date"
    "java/text/MessageFormat$Field"
    "java/text/Format$Field"
    "java/text/AttributedCharacterIterator$Attribute"
    "com/sun/tools/javac/util/Log$WriterKind"
    "java/util/Formatter"
    "java/util/regex/Pattern$Bound"
    "java/util/regex/Pattern$Utype"
    "java/util/regex/UnicodeProp"
    "java/util/regex/Pattern$Ctype"
    "java/util/regex/Pattern$LastMatch"
    "java/util/regex/Pattern$HorizWS"
    "java/util/regex/Pattern$LineEnding"
    "java/util/regex/Pattern$VertWS"
    "java/util/regex/Pattern$CIBackRef"
    "java/util/regex/Pattern$BackRef"
    "java/util/regex/Pattern$End"
    "java/text/spi/DecimalFormatSymbolsProvider"
    "java/util/spi/LocaleServiceProvider"
    "sun/util/locale/provider/LocaleProviderAdapter"
    "sun/util/locale/provider/JRELocaleProviderAdapter"
    "sun/util/locale/provider/ResourceBundleBasedAdapter"
    "sun/util/locale/provider/SPILocaleProviderAdapter"
    "sun/util/locale/provider/AuxLocaleProviderAdapter"
    "sun/util/locale/provider/LocaleProviderAdapter$Type"
    "sun/util/locale/provider/LocaleProviderAdapter$1"
    "sun/util/cldr/CLDRLocaleProviderAdapter"
    "sun/util/locale/provider/HostLocaleProviderAdapter"
    "sun/util/locale/provider/LocaleServiceProviderPool"
    "sun/util/locale/provider/FallbackLocaleProviderAdapter"
    "java/util/Collections$UnmodifiableCollection$1"
    "java/util/ArrayList$Itr"
    "java/lang/Class$EnclosingMethodInfo"
    "sun/util/locale/provider/DecimalFormatSymbolsProviderImpl"
    "sun/util/locale/provider/AvailableLanguageTags"
    "sun/util/locale/provider/LocaleDataMetaInfo"
    "sun/util/locale/provider/JRELocaleProviderAdapter$1"
    "sun/util/locale/LanguageTag"
    "sun/util/locale/StringTokenIterator"
    "java/util/Collections$EmptyIterator"
    "java/util/Currency"
    "sun/util/locale/provider/LocaleResources"
    "sun/util/resources/LocaleData"
    "sun/util/locale/provider/LocaleResources$ResourceReference"
    "sun/util/resources/LocaleData$1"
    "sun/util/resources/LocaleData$LocaleDataResourceBundleControl"
    "sun/text/resources/FormatData"
    "sun/util/resources/ParallelListResourceBundle"
    "java/util/concurrent/atomic/AtomicMarkableReference"
    "java/util/concurrent/atomic/AtomicMarkableReference$Pair"
    "sun/text/resources/en/FormatData_en"
    "sun/util/resources/ParallelListResourceBundle$KeySet"
    "java/util/concurrent/ConcurrentHashMap$KeySetView"
    "java/util/concurrent/ConcurrentHashMap$CollectionView"
    "java/util/concurrent/ConcurrentHashMap$ForwardingNode"
    "java/util/concurrent/ConcurrentHashMap$TreeNode"
    "java/util/Currency$1"
    "java/io/DataInputStream"
    "java/io/DataInput"
    "java/util/Currency$SpecialCaseEntry"
    "java/io/UTFDataFormatException"
    "java/util/Currency$OtherCurrencyEntry"
    "java/io/FileInputStream$1"
    "java/util/MissingFormatArgumentException"
    "java/util/IllegalFormatException"
    "java/util/FormatterClosedException"
    "java/util/Formatter$FixedString"
    "java/util/Formatter$FormatString"
    "java/util/Formatter$FormatSpecifier"
    "java/util/UnknownFormatConversionException"
    "java/util/Formatter$Flags"
    "java/util/Formatter$Conversion"
    "java/util/DuplicateFormatFlagsException"
    "java/util/UnknownFormatFlagsException"
    "java/util/IllegalFormatWidthException"
    "java/util/IllegalFormatPrecisionException"
    "java/util/MissingFormatWidthException"
    "java/util/Formattable"
    "com/sun/tools/javac/util/Log$2"
    "com/sun/tools/javac/util/JCDiagnostic$DiagnosticType"
    "java/lang/Shutdown"
    "java/lang/Shutdown$Lock"
    "java/lang/ThreadDeath"
    "com/sun/tools/javac/main/Option"
    "com/sun/tools/javac/main/JavaCompiler"
    "com/sun/tools/javac/file/JavacFileManager"
    "com/sun/tools/javac/util/Context"
    "com/sun/tools/javac/util/Log"
    "com/sun/tools/javac/util/Options"
    "com/sun/tools/javac/util/List"
    "com/sun/tools/javac/code/Lint"
    "com/sun/tools/javac/resources/compiler")
  "Subset of javac classes to preload into the javacl image.")

(defun %warmup-javac ()
  (dolist (c *javac-warmup-classes*)
    (openldk::|java/lang/Class.forName0(Ljava/lang/String;ZLjava/lang/ClassLoader;Ljava/lang/Class;)| (openldk::jstring c) nil openldk::*boot-class-loader* nil)))

(defun make-javac-image (&optional (output-path "javacl"))
  "Build an executable image that jumps straight into javac."
  (let ((cp (default-javac-classpath)))
    (openldk::initialize)
    ;; Set classpath inside the image so warmup uses it.
    (when cp
      (setf openldk::*classpath*
            (loop for cpe in (split-sequence:split-sequence (uiop:inter-directory-separator) cp)
                  collect (if (str:ends-with? ".jar" cpe)
                              (make-instance 'openldk::jar-classpath-entry :jarfile cpe)
                              (make-instance 'openldk::dir-classpath-entry :dir cpe))))))
  ;; Warm up javac by loading core classes (no compilation run).
  (%warmup-javac)

  ;; More warm up...
  (unwind-protect
       (let ((cp (default-javac-classpath)))
	 (let ((openldk::*ignore-quit* t))
	   (flet ((safe-warmup (args desc &key (timeout 420))
		    (handler-case
			(sb-ext:with-timeout timeout
			  (openldk::main "com.sun.tools.javac.Main" args :classpath cp))
		      (sb-ext:timeout ()
			(format *error-output* "~&;; WARMUP (~A) timed out after ~Ds~%" desc timeout))
		      (condition (c)
			(format *error-output* "~&;; WARMUP (~A) caught: ~A~%" desc c)))))
	     (safe-warmup '() "no-args")
	     (safe-warmup '("-version") "-version")
	     (safe-warmup '("-verbose" "Hello.java") "Hello.java")
	     (safe-warmup '("-verbose" "-sourcepath" "testsuite/mauve" "testsuite/mauve/gnu/testlet/TestHarness.java") "TestHarness.java"))))
    (progn
      (setf openldk::*ignore-quit* nil)
      ;; Clear unsafe memory table — foreign heap pointers from warmup
      ;; won't survive image save/load.
      (clrhash openldk::*unsafe-memory-table*)
      ;; Force ProcessEnvironment to re-initialize at runtime so it reads
      ;; the actual runtime environment, not stale warmup state.
      (let ((pe (openldk::%get-ldk-class-by-bin-name "java/lang/ProcessEnvironment" t)))
        (when pe (setf (openldk::initialized-p pe) nil)))
      ;; Likewise the NIO default filesystem: its clinit captures user.dir
      ;; (the BUILD directory) into UnixFileSystem.defaultDirectory, sending
      ;; relative-path I/O to the wrong place after restart.
      (dolist (cname '("sun/nio/fs/DefaultFileSystemProvider"
                       "java/nio/file/FileSystems$DefaultFileSystemHolder"))
        (let ((c (openldk::%get-ldk-class-by-bin-name cname t)))
          (when c (setf (openldk::initialized-p c) nil))))
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
      (clrhash openldk::*class-load-locks*)
      (setf openldk::*class-load-locks-lock* (bt:make-lock "class-load-locks"))
      (clrhash openldk::*monitors*)
      (setf openldk::*method-compilation-lock* (bt:make-lock "method-compilation-lock"))
      (setf openldk::*method-compilation-cv* (bt:make-condition-variable :name "method-compilation-cv"))
      (setf openldk::*identity-hash-counter-lock* (bt:make-lock "identity-hash-lock"))
      ;; Drop stale in-progress compilation claims from dead warmup threads.
      (maphash (lambda (k v)
                 (when (eq v t)
                   (remhash k openldk::*methods-being-compiled*)))
               openldk::*methods-being-compiled*)
      ;; Classpath entries carry their own locks and open container handles.
      (mapc #'openldk::%reset-classpath-entry-runtime-state openldk::*classpath*)
      ;; jrt: filesystem state (providers, mmapped jimage buffers) is
      ;; process-local; drop it so the restarted image rebuilds it.
      (setf openldk::*installed-file-system-providers* nil)
      (clrhash openldk::*jimage-native-maps*)
      ;; Java-side static caches also hold the build-time ImageReader whose
      ;; mmapped buffer dies with this process; using them after restart
      ;; segfaults.  Clear them so the runtime rebuilds the jrt filesystem.
      (flet ((static-storage (bin-name)
               (handler-case
                   (let* ((pkg (openldk::class-package bin-name))
                          (sym (and pkg (find-symbol (format nil "+static-~A+" bin-name) pkg))))
                     (when (and sym (boundp sym))
                       (symbol-value sym)))
                 (condition () nil))))
        (let ((sir (static-storage "jdk/internal/jimage/ImageReader$SharedImageReader")))
          (when (and sir (slot-exists-p sir '|OPEN_FILES|))
            (let ((map (slot-value sir '|OPEN_FILES|)))
              (when map (ignore-errors (openldk::|clear()| map))))))
        (let ((jrti (static-storage "com/sun/tools/javac/file/JRTIndex")))
          (when (and jrti (slot-exists-p jrti '|sharedInstance|))
            (setf (slot-value jrti '|sharedInstance|) nil)))
        ;; ResourceBundle negative-caches failed lookups; a NONEXISTENT
        ;; marker cached during warmup would be baked into the image and
        ;; poison the same lookup forever at runtime.
        (let ((rb (static-storage "java/util/ResourceBundle")))
          (when (and rb (slot-exists-p rb '|cacheList|))
            (let ((m (slot-value rb '|cacheList|)))
              (when m (ignore-errors (openldk::|clear()| m)))))))
      ;; Synchronized hash tables carry an internal mutex; one saved in the
      ;; locked state (a warmup thread killed mid-access) deadlocks the
      ;; restarted image on its first table access.  Sweep the heap and give
      ;; every synchronized table a fresh lock.
      (let ((tables '()))
        (sb-vm:map-allocated-objects
         (lambda (obj type size)
           (declare (ignore type size))
           (when (and (hash-table-p obj)
                      (sb-ext:hash-table-synchronized-p obj))
             (push obj tables)))
         :all)
        (dolist (ht tables)
          (setf (sb-impl::hash-table-%lock ht)
                (sb-thread:make-mutex :name "hash-table lock"))))
      (sb-ext:save-lisp-and-die output-path
				:executable t
				:save-runtime-options t
				:toplevel #'javac-main))))

