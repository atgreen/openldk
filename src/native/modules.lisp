;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: OPENLDK; Base: 10 -*-
;;; SPDX-License-Identifier: GPL-3.0-or-later WITH Classpath-exception-2.0
;;;
;;; Boot module metadata is built by the JDK's Java implementation. Native
;;; registration connects those modules to OpenLDK's existing Class objects.

(in-package :openldk)

(defvar *module-access-ready* nil
  "True after runtime startup has constructed the boot module graph.")
(defvar *defined-package-modules* (make-hash-table :test #'eq :weakness :key :synchronized t)
  "(defining Java loader . slash package name) -> application named module.")

(defvar *boot-module-layer-initializing* nil
  "True while this thread is constructing the boot module layer.")
(defvar *boot-module-lock* (bt:make-recursive-lock "boot-module-layer")
  "Serializes lazy boot module initialization.")
(defvar *boot-modules* (make-hash-table :test #'equal :synchronized t)
  "Boot module name -> java.lang.Module.")
(defvar *runtime-package-modules* (make-hash-table :test #'equal :synchronized t)
  "Runtime package in slash form -> its boot module.")

(defun %runtime-class-module (name loader)
  "Return the named runtime module for a bootstrap class NAME in LOADER."
  (when (or (null loader) (zerop (slot-value loader 'id)))
    (if-let ((slash (position #\/ name :from-end t)))
          (gethash (subseq name 0 slash) *runtime-package-modules*) ; lint:suppress use-serapeum-take
          (when (member name '("boolean" "byte" "char" "short" "int" "long"
                              "float" "double" "void") :test #'string=)
            (gethash "java.base" *boot-modules*)))))

(defun %defining-class-module (name ldk-loader java-loader)
  "A runtime module, or the unnamed module owned by the defining Java loader."
  (or (when-let ((slash (position #\/ name :from-end t)))
        (gethash (subseq name 0 slash) (%owner-cache-table *defined-package-modules* java-loader))) ; lint:suppress use-serapeum-take
      (%runtime-class-module name ldk-loader)
      (and java-loader
           (slot-exists-p java-loader '|unnamedModule|)
           (slot-boundp java-loader '|unnamedModule|)
           (slot-value java-loader '|unnamedModule|))
      *unnamed-module*))

(defun %register-defined-module (module packages)
  "Associate an application's named MODULE with its defining loader and packages."
  (let ((loader (slot-value module '|loader|)))
    (loop for package across (java-array-data packages)
          do (setf (gethash (substitute #\/ #\. (lstring package)) ; lint:suppress substitute-use
                           (%owner-cache-table *defined-package-modules* loader)) module)))
  nil)

(defun %register-boot-module (module packages)
  "Associate a boot MODULE with PACKAGES and already loaded runtime classes."
  (when *boot-module-layer-initializing*
    (setf (gethash (lstring (slot-value module '|name|)) *boot-modules*) module)
    (loop for package across (java-array-data packages)
          do (setf (gethash (substitute #\/ #\. (lstring package)) *runtime-package-modules*) module)) ; lint:suppress substitute-use
    (maphash (lambda (name klass)
               (let ((class (%get-ldk-class-by-bin-name name t)))
                 (when-let ((owner (%runtime-class-module name (and class (ldk-loader class)))))
                   (setf (slot-value klass '|module|) owner))))
             *java-classes-by-bin-name*)
    (maphash (lambda (name klass)
               (when (and (plusp (length name)) (char= (char name 0) #\[))
                 (when-let ((component (slot-value klass '|componentType|)))
                   (setf (slot-value klass '|module|) (slot-value component '|module|)))))
             *java-classes-by-bin-name*))
  nil)

(defun %boot-module-layer ()
  "Return the actual boot layer, constructing it with ModuleBootstrap on demand."
  (let ((system |+static-java/lang/System+|))
    (or (slot-value system '|bootLayer|)
        (cond
          (*boot-module-layer-initializing*
              (%clinit (classload "java/lang/ModuleLayer"))
              (|java/lang/ModuleLayer.empty()Ljava/lang/ModuleLayer;|))
          (t (bt:with-recursive-lock-held (*boot-module-lock*)
              (or (slot-value system '|bootLayer|)
                  (let ((*boot-module-layer-initializing* t)
                        (*context* nil))
                    (classload "jdk/internal/module/ModuleBootstrap")
                    (setf (slot-value system '|bootLayer|)
                          (|jdk/internal/module/ModuleBootstrap.boot()Ljava/lang/ModuleLayer;|))))))))))

(defvar *class-access-cache* (make-hash-table :test #'eq :weakness :key :synchronized t)
  "Weak caller Class -> weak referenced Class -> access resolution outcome.")
(defvar *checking-class-access* nil
  "Access checking and boot graph construction are trusted runtime operations.")

(defun %reset-boot-module-layer ()
  "Discard the image's warmed graph before resolving launch-time module roots."
  (when (slot-value |+static-java/lang/System+| '|bootLayer|)
    (dolist (loader (list (|jdk/internal/loader/ClassLoaders.bootLoader()Ljdk/internal/loader/BuiltinClassLoader;|)
                          (|jdk/internal/loader/ClassLoaders.platformClassLoader()Ljava/lang/ClassLoader;|)
                          (|jdk/internal/loader/ClassLoaders.appClassLoader()Ljava/lang/ClassLoader;|)))
      (|clear()V| (slot-value loader '|nameToModule|))
      (|clear()V| (slot-value loader '|moduleToReader|))
      (setf (slot-value loader '|resourceCache|) nil))
    (|clear()V| (|jdk/internal/loader/BuiltinClassLoader.packageToModule()Ljava/util/Map;|))
    (clrhash *class-access-cache*)
    (clrhash *boot-modules*)
    (clrhash *runtime-package-modules*)
    (setf (slot-value |+static-java/lang/System+| '|bootLayer|) nil)))

(defun %check-bytecode-class-access (caller target)
  "Check runtime package/module class access, retaining failed resolutions.
Graph updates affect fresh references, not a reference which already failed."
  (when (and *module-access-ready* (not *boot-module-layer-initializing*)
             (not *checking-class-access*) (not (eq caller target)))
    (let* ((table (%owner-cache-table *class-access-cache* caller 'eq :key))
           (outcome (or (gethash target table)
                        (let ((*checking-class-access* t))
                          (setf (gethash target table)
                                (if (eql 1 (|sun/invoke/util/VerifyAccess.isClassAccessible(Ljava/lang/Class;Ljava/lang/Class;Ljava/lang/Class;I)Z|
                                            target caller nil #x1f))
                                    :allowed :denied))))))
      (when (eql outcome :denied)
        (%throw '|java/lang/IllegalAccessError|
                (format nil "~A cannot access ~A"
                        (lstring (slot-value caller '|name|)) (lstring (slot-value target '|name|)))))))
  nil)
