;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: OPENLDK; Base: 10 -*-
;;; SPDX-License-Identifier: GPL-3.0-or-later WITH Classpath-exception-2.0

(in-package :openldk)

(defun javac-main ()
  "Entry point for the pre-dumped javac image. Runs com.sun.tools.javac.Main."
  ;; Match Java FP semantics
  (sb-int:set-floating-point-modes :traps nil)
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
	   (flet ((safe-warmup (args desc &key (timeout 120))
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
      ;; Kill helper threads before dumping the image.
      (loop for thread in (bt:all-threads)
            when (and (not (eq thread (bt:current-thread)))
                      (search "Java-Thread" (bt:thread-name thread)))
            do (bt:destroy-thread thread))
      (sb-ext:save-lisp-and-die output-path
				:executable t
				:save-runtime-options t
				:toplevel #'javac-main))))

