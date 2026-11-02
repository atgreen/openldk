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
    (when-let ((loader (slot-value module '|loader|)))
      (%register-defined-module module packages)
      (when (typep loader '|jdk/internal/loader/ClassLoaders$AppClassLoader|)
        (setf (gethash loader *java-to-ldk-loaders*) *app-ldk-class-loader*
              (slot-value *app-ldk-class-loader* 'java-loader) loader)))
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

(defvar *attach-providers* nil
  "Stable platform attach providers for this OpenLDK launch.")

(defun %attach-providers ()
  "Return the JDK platform provider despite OpenLDK's loader identity."
  (or *attach-providers*
      (let ((provider (%make-java-instance "sun/tools/attach/AttachProviderImpl"))
            (providers (%make-java-instance "java/util/ArrayList")))
        (|<init>()V| provider)
        (|<init>()V| providers)
        (|add(Ljava/lang/Object;)Z| providers provider)
        (setf *attach-providers*
              (|java/util/Collections.unmodifiableList(Ljava/util/List;)Ljava/util/List;|
               providers)))))

(setf (gethash "com/sun/tools/attach/spi/AttachProvider.providers()Ljava/util/List;"
               *native-overrides*)
      #'%attach-providers)

(defun %attach-to-openldk (provider vmid)
  "Return an attach handle for OpenLDK's current process."
  (unless (member (lstring vmid) (list "0" (princ-to-string (sb-posix:getpid)))
                  :test #'string=)
    (%throw '|com/sun/tools/attach/AttachNotSupportedException|
            "OpenLDK supports self-attach only"))
  (let ((allow (|jdk/internal/misc/VM.getSavedProperty(Ljava/lang/String;)Ljava/lang/String;|
                (jstring "jdk.attach.allowAttachSelf"))))
    (unless (and allow
                 (or (string= "" (lstring allow))
                     (string-equal "true" (lstring allow))))
      (%throw '|java/io/IOException| "Can not attach to current VM")))
  (classload "sun/tools/attach/VirtualMachineImpl")
  (let ((virtual-machine (%make-java-instance "sun/tools/attach/VirtualMachineImpl")))
    (setf (slot-value virtual-machine '|provider|) provider
          (slot-value virtual-machine '|id|) vmid
          (slot-value virtual-machine '|hash|) 0)
    virtual-machine))

(setf (gethash "sun/tools/attach/AttachProviderImpl.attachVirtualMachine(Ljava/lang/String;)Lcom/sun/tools/attach/VirtualMachine;"
               *native-overrides*)
      #'%attach-to-openldk)

(defun %start-local-management-agent (virtual-machine)
  "Start the management agent in-process without a HotSpot attach listener."
  (declare (ignore virtual-machine))
  (|jdk/internal/module/Modules.loadModule(Ljava/lang/String;)Ljava/lang/Module;|
   (jstring "jdk.management.agent"))
  (classload "jdk/internal/agent/Agent")
  (|jdk/internal/agent/Agent.startLocalManagementAgent()V|)
  (let ((properties (|jdk/internal/vm/VMSupport.getAgentProperties()Ljava/util/Properties;|)))
    (|getProperty(Ljava/lang/String;)Ljava/lang/String;|
     properties (jstring "com.sun.management.jmxremote.localConnectorAddress"))))

(setf (gethash "sun/tools/attach/HotSpotVirtualMachine.startLocalManagementAgent()Ljava/lang/String;"
               *native-overrides*)
      #'%start-local-management-agent)

(setf (gethash "jdk/internal/vm/VMSupport.initAgentProperties(Ljava/util/Properties;)Ljava/util/Properties;"
               *native-overrides*)
      #'identity)

(setf (gethash "jdk/internal/agent/ConnectorAddressLink.export(Ljava/lang/String;)V"
               *native-overrides*)
      (lambda (address) (declare (ignore address)) nil))

(setf (gethash "sun/tools/attach/VirtualMachineImpl.execute(Ljava/lang/String;[Ljava/lang/Object;)Ljava/io/InputStream;"
               *native-overrides*)
      (lambda (virtual-machine command arguments)
        (declare (ignore virtual-machine command arguments))
        (%throw '|java/io/IOException| "OpenLDK attach operation is not supported")))

(defun %observable-runtime-class-p (classname)
  "Whether CLASSNAME belongs to a module visible in the current runtime graph."
  (or (not *module-access-ready*)
      (let* ((image-module (%runtime-image-class-module-name classname))
             (slash (and image-module (position #\/ classname :from-end t)))
             (package (and slash (take slash classname))))
        (or (null image-module)
            (gethash package *runtime-package-modules*)
            (gethash package (%owner-cache-table *defined-package-modules* nil))))))

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
      (remhash loader *defined-package-modules*)
      (|clear()V| (slot-value loader '|nameToModule|))
      (|clear()V| (slot-value loader '|moduleToReader|))
      (setf (slot-value loader '|resourceCache|) nil))
    (|clear()V| (|jdk/internal/loader/BuiltinClassLoader.packageToModule()Ljava/util/Map;|))
    (clrhash *class-access-cache*)
    (clrhash *boot-modules*)
    (clrhash *runtime-package-modules*)
    (setf (slot-value |+static-java/lang/System+| '|bootLayer|) nil)))

(defun %cli-export-allows-access-p (caller target)
  "Whether --add-exports explicitly grants CALLER access to TARGET."
  (let* ((target-module (slot-value target '|module|))
         (caller-module (slot-value caller '|module|))
         (target-module-name (and target-module (slot-value target-module '|name|)))
         (caller-module-name (and caller-module (slot-value caller-module '|name|)))
         (target-name (lstring (slot-value target '|name|)))
         (package-end (position #\. target-name :from-end t))
         (package (and package-end (take package-end target-name))))
    (and target-module-name package
         (loop for specification in *cli-add-exports*
               for equals = (position #\= specification)
               for slash = (and equals (position #\/ specification :end equals))
               when (and slash
                         (string= (lstring target-module-name)
                                  (take slash specification))
                         (string= package (subseq specification (1+ slash) equals)))
                 do (when (some (lambda (recipient)
                                  (or (and (string= recipient "ALL-UNNAMED")
                                           (null caller-module-name))
                                      (and caller-module-name
                                           (string= recipient (lstring caller-module-name)))))
                                (split-sequence:split-sequence
                                 #\, (drop (1+ equals) specification)))
                      (return t))))))

(defun %check-bytecode-class-access (caller target)
  "Check runtime package/module class access, retaining failed resolutions.
Graph updates affect fresh references, not a reference which already failed."
  (when (and *module-access-ready* (not *boot-module-layer-initializing*)
             (not *checking-class-access*) (not (eq caller target)))
    (let* ((table (%owner-cache-table *class-access-cache* caller 'eq :key))
           (outcome (or (gethash target table)
                        (let ((*checking-class-access* t))
                          (setf (gethash target table)
                                (if (or (%cli-export-allows-access-p caller target)
                                        (eql 1 (|sun/invoke/util/VerifyAccess.isClassAccessible(Ljava/lang/Class;Ljava/lang/Class;Ljava/lang/Class;I)Z|
                                                target caller nil #x1f)))
                                    :allowed :denied))))))
      (when (eql outcome :denied)
        (%throw '|java/lang/IllegalAccessError|
                (format nil "~A cannot access ~A"
                        (lstring (slot-value caller '|name|)) (lstring (slot-value target '|name|)))))))
  nil)
