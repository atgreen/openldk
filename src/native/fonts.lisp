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

;;; Fontconfig supplies installed font files and fallbacks to the JDK's Java
;;; font manager. No HotSpot fontmanager library is loaded.

(defvar *fontconfig-loaded* nil)
(defvar *fontconfig-lock* (bordeaux-threads:make-recursive-lock "Fontconfig"))

(sb-alien:define-alien-type openldk-fc-font-set
    (sb-alien:struct openldk-fc-font-set
      (count sb-alien:int)
      (capacity sb-alien:int)
      (fonts (* (* t)))))

(defmacro %font-native-call (name return-type &rest arguments)
  "Call a font library function using explicit native argument types."
  `(sb-alien:alien-funcall
    (sb-alien:extern-alien ,name (function ,return-type ,@(mapcar #'first arguments)))
    ,@(mapcar #'second arguments)))

(defun %load-fontconfig ()
  "Load the system font configuration library and initialize its default config."
  (bordeaux-threads:with-recursive-lock-held (*fontconfig-lock*)
    (unless *fontconfig-loaded*
      (sb-alien:load-shared-object "libfontconfig.so.1")
      (when (zerop (%font-native-call "FcInit" sb-alien:int))
        (%throw '|java/lang/IllegalStateException| "Fontconfig initialization failed"))
      (setf *fontconfig-loaded* t))))

(defun %fontconfig-string (pattern property)
  "Copy the first string PROPERTY from a Fontconfig PATTERN."
  (sb-alien:with-alien ((value sb-alien:c-string))
    (when (zerop (%font-native-call "FcPatternGetString" sb-alien:int
                   ((* t) pattern) (sb-alien:c-string property) (sb-alien:int 0)
                   ((* sb-alien:c-string) (sb-alien:addr value))))
      value)))

(defun %fontconfig-fonts (query &optional (fallbacks t) (trim t))
  "Return matched font metadata for QUERY, in Fontconfig fallback order."
  (%load-fontconfig)
  (bordeaux-threads:with-recursive-lock-held (*fontconfig-lock*)
    (let ((pattern (%font-native-call "FcNameParse" (* t) (sb-alien:c-string query))))
      (when (sb-alien:null-alien pattern)
        (%throw '|java/lang/IllegalArgumentException| "Invalid Fontconfig pattern"))
      (unwind-protect
           (progn
             (%font-native-call "FcConfigSubstitute" sb-alien:int
               ((* t) nil) ((* t) pattern) (sb-alien:int 0))
             (%font-native-call "FcDefaultSubstitute" sb-alien:void ((* t) pattern))
             (sb-alien:with-alien ((result sb-alien:int))
               (let ((fonts (%font-native-call "FcFontSort" (* openldk-fc-font-set)
                              ((* t) nil) ((* t) pattern) (sb-alien:int (if trim 1 0))
                              ((* t) nil) ((* sb-alien:int) (sb-alien:addr result)))))
                 (unless (sb-alien:null-alien fonts)
                   (unwind-protect
                        (loop for i below (if fallbacks (sb-alien:slot fonts 'count)
                                              (min 1 (sb-alien:slot fonts 'count)))
                              for font = (sb-alien:deref (sb-alien:slot fonts 'fonts) i)
                              for file = (%fontconfig-string font "file")
                              when file
                                collect (list (%fontconfig-string font "family")
                                              (%fontconfig-string font "style")
                                              (%fontconfig-string font "fullname") file))
                     (%font-native-call "FcFontSetDestroy" sb-alien:void
                       ((* openldk-fc-font-set) fonts)))))))
        (%font-native-call "FcPatternDestroy" sb-alien:void ((* t) pattern))))))

(defun %fontconfig-cache-directories ()
  "Copy the configured Fontconfig cache directory names."
  (%load-fontconfig)
  (let ((strings (%font-native-call "FcConfigGetCacheDirs" (* t) ((* t) nil))))
    (unless (sb-alien:null-alien strings)
      (unwind-protect
           (loop for value = (%font-native-call "FcStrListNext" sb-alien:c-string ((* t) strings))
                 while value collect value)
        (%font-native-call "FcStrListDone" sb-alien:void ((* t) strings))))))

(%define-graphics-id-initializers "sun/font/SunFontManager.initIDs()V")

(defun |sun/font/FontConfigManager.getFontConfigVersion()I| ()
  "Return the installed Fontconfig library version."
  (%load-fontconfig)
  (%font-native-call "FcGetVersion" sb-alien:int))

(defun %fontconfig-java-font (metadata)
  "Build the JDK's font metadata holder from a Fontconfig match."
  (destructuring-bind (family style full-name file) metadata
    (let ((font (%make-java-instance "sun/font/FontConfigManager$FontConfigFont")))
      (|<init>()V| font)
      (setf (slot-value font '|familyName|) (jstring (or family full-name ""))
            (slot-value font '|styleStr|) (jstring (or style "Regular"))
            (slot-value font '|fullName|) (jstring (or full-name family ""))
            (slot-value font '|fontFile|) (jstring file))
      font)))

(defun |sun/font/FontConfigManager.getFontConfig(Ljava/lang/String;Lsun/font/FontConfigManager$FontConfigInfo;[Lsun/font/FontConfigManager$FcCompFont;Z)V|
    (locale info composites fallbacks)
  "Populate the JDK's physical font and fallback metadata using Fontconfig."
  (classload "sun/font/FontConfigManager$FontConfigFont")
  (setf (slot-value info '|fcVersion|) (|sun/font/FontConfigManager.getFontConfigVersion()I|)
        (slot-value info '|cacheDirs|)
        (make-java-array :component-class (%get-java-class-by-bin-name "java/lang/String")
                         :initial-contents (mapcar #'jstring (%fontconfig-cache-directories))))
  (loop for composite across (java-array-data composites)
        for query = (lstring (slot-value composite '|fcName|))
        for locale-name = (and locale (substitute #\- #\_ (lstring locale))) ; lint:suppress substitute-use
        for matches = (%fontconfig-fonts (if locale-name (format nil "~A:lang=~A" query locale-name) query)
                                         (not (zerop fallbacks)))
        for fonts = (mapcar #'%fontconfig-java-font matches)
        do (setf (slot-value composite '|firstFont|) (first fonts))
           (unless (zerop fallbacks)
             (setf (slot-value composite '|allFonts|)
                   (make-java-array :component-class (%get-java-class-by-bin-name "sun/font/FontConfigManager$FontConfigFont")
                                    :initial-contents fonts)))))

(defun |sun/font/FontConfigManager.getFontConfigAASettings(Ljava/lang/String;Ljava/lang/String;)I|
    (locale family)
  "Request grayscale antialiasing for offscreen Java image surfaces."
  (declare (ignore locale family))
  2)

(defmethod |getFontPathNative(ZZ)Ljava/lang/String;| (this no-type1 x11)
  "Return directories containing the platform's configured fallback fonts."
  (declare (ignore this no-type1 x11))
  (jstring (format nil "~{~A~^:~}"
                   (remove-duplicates
                    (loop for metadata in (%fontconfig-fonts "" t nil)
                          collect (directory-namestring (fourth metadata)))
                    :test #'equal))))

(defvar *font-invisible-glyph* nil)

(defun |sun/font/StrikeCache.getInvisibleGlyphPtr()J| ()
  "Return a zero-sized, managed glyph record in image-stable native memory."
  (bordeaux-threads:with-recursive-lock-held (*fontconfig-lock*)
    (or *font-invisible-glyph*
        (let ((address (%allocate-unsafe-memory 40)))
          ;; The JDK 25 GlyphImageLayout marks this shared singleton managed.
          (setf (sb-sys:sap-ref-8 (sb-sys:int-sap address) 14) 1
                *font-invisible-glyph* address)))))

(defun %reset-fontconfig-for-image ()
  "Reinitialize Fontconfig's process-local default config after image restart."
  (setf *fontconfig-loaded* nil
        *fontconfig-lock* (bordeaux-threads:make-recursive-lock "Fontconfig")))

(pushnew '%reset-fontconfig-for-image sb-ext:*save-hooks*)
