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

;;; Display access belongs to OpenLDK; no libawt or libawt_xawt is loaded.
(defvar *x11-loaded* nil)
(defvar *x11-display* nil)
(defvar *x11-lock* (bt:make-recursive-lock "X11 display"))
(defvar *x11-visuals* (make-hash-table))
(defvar *x11-previous-error-handler* nil)
(defvar *x11-callback-errors* (make-hash-table :test #'eq :weakness :key :synchronized t)
  "Calling Lisp thread -> condition deferred until Xlib returns from its callback.")
(defvar *x11-error-memory* nil)


(sb-alien:define-alien-type openldk-xvisual-info
  (sb-alien:struct openldk-xvisual-info
    (visual (* t)) (visual-id sb-alien:unsigned-long)
    (screen sb-alien:int) (depth sb-alien:int) (visual-class sb-alien:int)
    (red-mask sb-alien:unsigned-long) (green-mask sb-alien:unsigned-long)
    (blue-mask sb-alien:unsigned-long) (colormap-size sb-alien:int) (bits-per-rgb sb-alien:int)))

(defun %x11-check-callback-errors ()
  "Deliver a callback condition after returning through the native C stack."
  (let ((thread (bt:current-thread)))
    (when-let ((condition (gethash thread *x11-callback-errors*)))
      (remhash thread *x11-callback-errors*)
      (error condition))))

(defmacro %define-x11-native (signature parameters &body body)
  "Register a native body without eagerly loading its Java declaring class."
  (multiple-value-bind (forms declarations documentation) (alexandria:parse-body body :documentation t)
    (let ((completed (gensym "COMPLETED")))
      `(setf (gethash ,signature *native-overrides*)
             (lambda ,parameters
               ,@(when documentation (list documentation))
               ,@declarations
               (let ((,completed nil))
                 (unwind-protect
                      (multiple-value-prog1 (progn ,@forms) (setf ,completed t))
                   (if ,completed
                       (%x11-check-callback-errors)
                       (remhash (bt:current-thread) *x11-callback-errors*)))))))))

(defun %x11-address (pointer)
  "Convert an Xlib pointer to its Java long representation."
  (sb-sys:sap-int (sb-alien:alien-sap pointer)))

(defun %x11-pointer (address)
  "Convert a Java long address to an Xlib pointer."
  (sb-alien:sap-alien (sb-sys:int-sap address) (* t)))

(defun %x11-java-static (owner signature &rest arguments)
  "Call a JDK implementation helper on behalf of its native backend."
  (let ((class (classload owner)))
    (%clinit class)
    (apply (static-method-symbol (concatenate 'string owner "." signature)
                                 (ldk-class-package class)) arguments)))

(defun %load-x11 ()
  "Load the ordinary system Xlib library and enable thread support."
  (unless *x11-loaded*
    (sb-alien:load-shared-object "libX11.so.6")
    (when (zerop (%libc "XInitThreads" sb-alien:int))
      (%throw '|java/awt/AWTError| "Xlib thread initialization failed"))
    (setf *x11-loaded* t)))

(defmacro %with-x11-awt-lock (&body body)
  "Coordinate native resources with the Java toolkit event-dispatch lock."
  `(progn
     (%x11-java-static "sun/awt/SunToolkit" "awtLock()V")
     (unwind-protect (progn ,@body)
       (%x11-java-static "sun/awt/SunToolkit" "awtUnlock()V"))))

(defmacro %define-x11-awt-native (signature parameters &body body)
  "Define a native whose JNI implementation acquires the AWT lock itself."
  (multiple-value-bind (forms declarations documentation) (alexandria:parse-body body :documentation t)
    `(%define-x11-native ,signature ,parameters
       ,@(when documentation (list documentation))
       ,@declarations
       (%with-x11-awt-lock ,@forms))))

(defun %x11-display ()
  "Open the configured display once, reporting connection failures as AWTError."
  (bt:with-recursive-lock-held (*x11-lock*)
    (%load-x11)
    (or *x11-display*
        (let ((display (%libc "XOpenDisplay" (* t) (sb-alien:c-string nil))))
          (when (sb-alien:null-alien display)
            (%throw '|java/awt/AWTError|
                    (format nil "Cannot connect to X11 display ~A" (uiop:getenv "DISPLAY"))))
          (setf *x11-display* display)
          (%x11-java-static "sun/awt/X11/XErrorHandlerUtil" "init(J)V" (%x11-address display))
          display))))

(defun %x11-supported-visual-p (visual)
  "Match the visual classes and depths enumerated by the JDK X11 backend."
  (let ((depth (getf visual :depth)) (entries (getf visual :entries)))
    (case (getf visual :class)
      (4 (/= depth 12))
      (3 (or (and (= depth 8) (= entries 256))
             (and (= depth 12) (= entries 4096))))
      (2 (= depth 8))
      ((0 1) (and (= depth 8) (= entries 256)))
      (otherwise nil))))

(defun %x11-screen-visuals (screen)
  "Copy actual X visual descriptors, with the default visual first."
  (bt:with-recursive-lock-held (*x11-lock*)
    (or (gethash screen *x11-visuals*)
        (sb-alien:with-alien ((template openldk-xvisual-info) (count sb-alien:int))
          (setf (sb-alien:slot template 'screen) screen)
          (let ((infos (%libc "XGetVisualInfo" (* openldk-xvisual-info)
                         ((* t) (%x11-display)) (sb-alien:long 2)
                         ((* openldk-xvisual-info) (sb-alien:addr template))
                         ((* sb-alien:int) (sb-alien:addr count)))))
            (when (sb-alien:null-alien infos)
              (%throw '|java/awt/AWTError| "No X11 visual configurations"))
            (unwind-protect
                 (let* ((default (%libc "XVisualIDFromVisual" sb-alien:unsigned-long
                                   ((* t) (%libc "XDefaultVisual" (* t)
                                              ((* t) (%x11-display)) (sb-alien:int screen)))))
                        (visuals
                          (loop for i below count
                                collect (let ((info (sb-alien:deref infos i)))
                                          (list :colormap nil :palette nil :palette-valid nil :palette-cache nil
                                                :visual (sb-alien:slot info 'visual-id)
                                                :pointer (%x11-address (sb-alien:slot info 'visual))
                                                :depth (sb-alien:slot info 'depth)
                                                :class (sb-alien:slot info 'visual-class)
                                                :red (sb-alien:slot info 'red-mask)
                                                :green (sb-alien:slot info 'green-mask)
                                                :blue (sb-alien:slot info 'blue-mask)
                                                :entries (sb-alien:slot info 'colormap-size)
                                                :bits-per-rgb (sb-alien:slot info 'bits-per-rgb)))))
                        (first (find default visuals :key (lambda (v) (getf v :visual)))))
                   (unless first (%throw '|java/awt/AWTError| "Missing default visual"))
                   (setf (gethash screen *x11-visuals*)
                         (coerce (cons first (serapeum:filter #'%x11-supported-visual-p
                                                          (remove first visuals :test #'eq))) 'vector)))
              (%libc "XFree" sb-alien:int ((* t) (sb-alien:cast infos (* t))))))))))

(defun %x11-rectangle (screen)
  "Return the physical bounds reported by Xlib for SCREEN."
  (classload "java/awt/Rectangle")
  (let ((rectangle (%make-java-instance "java/awt/Rectangle")))
    (|<init>(IIII)V| rectangle 0 0
      (%libc "XDisplayWidth" sb-alien:int ((* t) (%x11-display)) (sb-alien:int screen))
      (%libc "XDisplayHeight" sb-alien:int ((* t) (%x11-display)) (sb-alien:int screen)))
    rectangle))

(defun %x11-check-save ()
  "Reject saving live X server connections and the Java peers that refer to them."
  (when *x11-display*
    (error "Cannot save an image with an open X11 display; initialize AWT after image restart")))

(defun %reset-x11-for-image ()
  "Reset inactive display state before saving a Lisp image."
  (%x11-check-save)
  (setf *x11-display* nil *x11-loaded* nil
        *x11-lock* (bt:make-recursive-lock "X11 display"))
  (clrhash *x11-visuals*))
(pushnew '%reset-x11-for-image sb-ext:*save-hooks*)

(%define-x11-native "sun/awt/X11GraphicsEnvironment.initDisplay(Z)V" (glx)
  (declare (ignore glx)) (%x11-display) nil)
(%define-x11-native "sun/awt/X11GraphicsEnvironment.initGLX()Z" () 0)
(%define-x11-native "sun/awt/X11GraphicsEnvironment.initXRender(ZZ)Z" (verbose ignore-version)
  (declare (ignore verbose ignore-version)) 0)
(%define-x11-native "sun/awt/X11GraphicsEnvironment.initNativeData()V" ()
  (%x11-display) nil)
(%define-x11-native "sun/awt/X11GraphicsEnvironment.getNumScreens()I" (this)
  (declare (ignore this)) (%libc "XScreenCount" sb-alien:int ((* t) (%x11-display))))
(%define-x11-native "sun/awt/X11GraphicsEnvironment.getDefaultScreenNum()I" (this)
  (declare (ignore this)) (%libc "XDefaultScreen" sb-alien:int ((* t) (%x11-display))))
(%define-x11-native "sun/awt/X11GraphicsEnvironment.pRunningXinerama()Z" () 0)
(%define-x11-native "sun/awt/X11GraphicsEnvironment.checkShmExt()I" () -1)
(%define-x11-native "sun/awt/X11GraphicsEnvironment.getDisplayString()Ljava/lang/String;" ()
  (jstring (%libc "XDisplayString" sb-alien:c-string ((* t) (%x11-display)))))
(%define-x11-native "sun/awt/X11GraphicsDevice.getDisplay()J" (this)
  (declare (ignore this)) (%x11-address (%x11-display)))
(%define-x11-native "sun/awt/X11GraphicsDevice.getNativeScaleFactor(I)D" (screen)
  (declare (ignore screen)) 1d0)
(%define-x11-native "sun/awt/X11GraphicsDevice.pGetBounds(I)Ljava/awt/Rectangle;" (this screen)
  (declare (ignore this)) (%x11-rectangle screen))
(%define-x11-native "sun/awt/X11GraphicsDevice.getNumConfigs(I)I" (this screen)
  (declare (ignore this)) (length (%x11-screen-visuals screen)))
(%define-x11-native "sun/awt/X11GraphicsDevice.getConfigVisualId(II)I" (this index screen)
  (declare (ignore this)) (getf (aref (%x11-screen-visuals screen) index) :visual))
(%define-x11-native "sun/awt/X11GraphicsDevice.getConfigDepth(II)I" (this index screen)
  (declare (ignore this)) (getf (aref (%x11-screen-visuals screen) index) :depth))
(%define-x11-native "sun/awt/X11GraphicsDevice.isDBESupported()Z" () 0)
(%define-x11-native "sun/awt/X11GraphicsDevice.initXrandrExtension(Z)Z" (old-mode)
  (declare (ignore old-mode)) 0)
(%define-x11-native "sun/awt/X11GraphicsConfig.initIDs()V" () nil)
(%define-x11-native "sun/awt/X11/XToolkit.initIDs()V" () nil)
(%define-x11-native "sun/awt/X11/XWindow.initIDs()V" ()
  ;; JNI field-ID caches are unnecessary for CLOS slot access.
  nil)
(%define-x11-native "sun/awt/X11/XToolkit.getEnv(Ljava/lang/String;)Ljava/lang/String;" (name)
  (when-let ((value (uiop:getenv (lstring name)))) (jstring value)))

;;; XlibWrapper declarations below use actual Xlib ABI types, including
;;; pointer conversion and strings, rather than calling JNI implementations.
(defmacro %define-xlib (java-name descriptor c-name result arguments)
  "Register an explicitly typed XlibWrapper call to a system Xlib function."
  (let ((names (loop for arg in arguments for i from 0 collect (intern (format nil "XARG~D" i)))))
    `(%define-x11-native ,(concatenate 'string "sun/awt/X11/XlibWrapper." java-name descriptor) ,names
       (%load-x11)
       ,(let ((call `(%libc ,c-name ,(case result (:pointer '(* t)) (:boolean 'sb-alien:int) (:void 'sb-alien:void) (:string 'sb-alien:c-string) (otherwise result))
                            ,@(loop for type in arguments for name in names
                                    collect (case type
                                              (:pointer `((* t) (%x11-pointer ,name)))
                                              (:string `(sb-alien:c-string (lstring ,name)))
                                              (sb-alien:int `(,type (%as-int ,name)))
                                              (sb-alien:unsigned-long `(,type (ldb (byte 64 0) ,name)))
                                              (sb-alien:unsigned-int `(,type (ldb (byte 32 0) ,name)))
                                              (sb-alien:unsigned-char `(,type (ldb (byte 8 0) ,name)))
                                              (otherwise `(,type ,name)))))))
          (case result (:pointer `(%x11-address ,call))
                       (:boolean `(if (zerop ,call) 0 1))
                       (:string `(when-let ((value ,call)) (jstring value)))
                       (:void `(progn ,call nil))
                       (otherwise (if (char= (char descriptor (1- (length descriptor))) #\I)
                                      `(%as-int ,call) call)))))))

(%define-xlib "XKeysymToKeycode" "(JJ)I" "XKeysymToKeycode" sb-alien:unsigned-char (:pointer sb-alien:unsigned-long))
(%define-xlib "XKeycodeToKeysym" "(JII)J" "XKeycodeToKeysym" sb-alien:unsigned-long (:pointer sb-alien:unsigned-char sb-alien:int))
(%define-xlib "XGetModifierMapping" "(J)J" "XGetModifierMapping" :pointer (:pointer))
(%define-xlib "XFreeModifiermap" "(J)V" "XFreeModifiermap" :void (:pointer))
(%define-xlib "XFree" "(J)V" "XFree" :void (:pointer))
(%define-xlib "DefaultScreen" "(J)J" "XDefaultScreen" sb-alien:int (:pointer))
(%define-xlib "ScreenCount" "(J)I" "XScreenCount" sb-alien:int (:pointer))
(%define-xlib "ScreenOfDisplay" "(JJ)J" "XScreenOfDisplay" :pointer (:pointer sb-alien:int))
(%define-xlib "DisplayWidth" "(JJ)J" "XDisplayWidth" sb-alien:int (:pointer sb-alien:int))
(%define-xlib "DisplayHeight" "(JJ)J" "XDisplayHeight" sb-alien:int (:pointer sb-alien:int))
(%define-xlib "DisplayWidthMM" "(JJ)J" "XDisplayWidthMM" sb-alien:int (:pointer sb-alien:int))
(%define-xlib "DisplayHeightMM" "(JJ)J" "XDisplayHeightMM" sb-alien:int (:pointer sb-alien:int))
(%define-xlib "RootWindow" "(JJ)J" "XRootWindow" sb-alien:unsigned-long (:pointer sb-alien:int))
(%define-xlib "DoesBackingStore" "(J)I" "XDoesBackingStore" sb-alien:int (:pointer))

(defvar *x11-configurations* (make-hash-table :synchronized t))

(defun %x11-colormap (visual screen)
  "Return a real server colormap appropriate to VISUAL."
  (bt:with-recursive-lock-held (*x11-lock*)
    (or (getf visual :colormap)
        (setf (getf visual :colormap)
              (if (eq visual (aref (%x11-screen-visuals screen) 0))
                  (%libc "XDefaultColormap" sb-alien:unsigned-long
                    ((* t) (%x11-display)) (sb-alien:int screen))
                  (%libc "XCreateColormap" sb-alien:unsigned-long
                    ((* t) (%x11-display))
                    (sb-alien:unsigned-long
                     (%libc "XRootWindow" sb-alien:unsigned-long
                       ((* t) (%x11-display)) (sb-alien:int screen)))
                    ((* t) (%x11-pointer (getf visual :pointer))) (sb-alien:int 0)))))))

(sb-alien:define-alien-type openldk-xpixmap-format
  (sb-alien:struct openldk-xpixmap-format
    (depth sb-alien:int) (bits-per-pixel sb-alien:int) (scanline-pad sb-alien:int)))

(defun %x11-pixmap-format (depth)
  "Return actual server bits per pixel and scanline padding for DEPTH."
  (sb-alien:with-alien ((count sb-alien:int))
    (let ((formats (%libc "XListPixmapFormats" (* openldk-xpixmap-format)
                     ((* t) (%x11-display)) ((* sb-alien:int) (sb-alien:addr count)))))
      (when (sb-alien:null-alien formats)
        (%throw '|java/awt/AWTError| "No X11 pixel formats"))
      (unwind-protect
           (progn
             (dotimes (i count)
               (let ((format (sb-alien:deref formats i)))
                 (when (= depth (sb-alien:slot format 'depth))
                   (return-from %x11-pixmap-format
                     (values (sb-alien:slot format 'bits-per-pixel)
                             (sb-alien:slot format 'scanline-pad))))))
             (%throw '|java/awt/AWTError| "Unknown X11 pixel format"))
        (%libc "XFree" sb-alien:int ((* t) (sb-alien:cast formats (* t))))))))

(defun %x11-bits-per-pixel (depth)
  "Return storage width for a server visual with DEPTH significant bits."
  (%x11-pixmap-format depth))

(%define-x11-native "sun/awt/X11GraphicsDevice.getConfigColormap(II)I" (this index screen)
  (declare (ignore this))
  (%as-int (%x11-colormap (aref (%x11-screen-visuals screen) index) screen)))
(defun %x11-java-instance (receiver signature &rest args)
  "Call a Java layout accessor without duplicating its JDK-specific offsets."
  (apply (symbol-function (intern signature :openldk)) receiver args))
(defun %x11-layout-buffer (class-name)
  "Allocate a zeroed native structure using CLASS-NAME's Java layout accessors."
  (let* ((size (%x11-java-static class-name "getSize()I"))
         (address (%allocate-unsafe-memory size))
         (success nil))
    (unwind-protect
         (let ((wrapper (%make-java-instance class-name)))
           (%libc "memset" (* t) ((* t) (%x11-pointer address))
             (sb-alien:int 0) (sb-alien:unsigned-long size))
           (|<init>(J)V| wrapper address)
           (setf success t)
           (values wrapper address))
      (unless success (%free-unsafe-memory address)))))

(%define-x11-native "sun/awt/X11GraphicsConfig.init(II)V" (this visual-id screen)
  (let ((visual (find visual-id (%x11-screen-visuals screen) :key (lambda (v) (getf v :visual))))
        (allocations nil)
        (success nil))
    (unless visual (%throw '|java/lang/IllegalArgumentException| "Unknown X11 visual"))
    (labels ((layout (name)
               (multiple-value-bind (wrapper address) (%x11-layout-buffer name)
                 (push address allocations)
                 (values wrapper address))))
      (unwind-protect
           (multiple-value-bind (data handle) (layout "sun/awt/X11/AwtGraphicsConfigData")
             (multiple-value-bind (bpp pad) (%x11-pixmap-format (getf visual :depth))
               (%x11-java-instance data "set_awt_depth(I)V" (getf visual :depth))
               (%x11-java-instance data "set_awt_cmap(J)V" (%x11-colormap visual screen))
               (%x11-java-instance data "set_awt_num_colors(I)V" (%x11-color-count visual))
               (%x11-java-instance data "set_pixelStride(I)V" (ceiling bpp 8))
               (let ((info (%x11-java-instance data "get_awt_visInfo()Lsun/awt/X11/XVisualInfo;")))
                 (loop for (signature value) in
                       (list (list "set_visual(J)V" (getf visual :pointer))
                             (list "set_visualid(J)V" visual-id)
                             (list "set_screen(I)V" screen)
                             (list "set_depth(I)V" (getf visual :depth))
                             (list "set_class(I)V" (getf visual :class))
                             (list "set_red_mask(J)V" (getf visual :red))
                             (list "set_green_mask(J)V" (getf visual :green))
                             (list "set_blue_mask(J)V" (getf visual :blue))
                             (list "set_colormap_size(I)V" (getf visual :entries))
                             (list "set_bits_per_rgb(I)V" (getf visual :bits-per-rgb)))
                       do (%x11-java-instance info signature value)))
               (multiple-value-bind (image address) (layout "sun/awt/X11/awtImageData")
                 (%x11-java-instance data "set_awtImage(J)V" address)
                 (%x11-java-instance image "set_Depth(I)V" (getf visual :depth))
                 (let ((format (%x11-java-instance image "get_wsImageFormat()Lsun/awt/X11/XPixmapFormatValues;")))
                   (%x11-java-instance format "set_depth(I)V" (getf visual :depth))
                   (%x11-java-instance format "set_bits_per_pixel(I)V" bpp)
                   (%x11-java-instance format "set_scanline_pad(I)V" pad)))
               ;; XIconWindow's byte-raster path needs ColorData even when
               ;; those bytes encode small TrueColor pixels, not palette IDs.
               (when (or (/= (getf visual :class) 4) (<= (getf visual :depth) 8))
                 (let* ((palette (%x11-color-palette visual screen))
                        (count (length palette))
                        (lookup (%allocate-unsafe-memory (* count 4))))
                   (push lookup allocations)
                   (let ((pixels (%allocate-unsafe-memory count)))
                     (push pixels allocations)
                     (dotimes (i count)
                       (setf (sb-sys:sap-ref-32 (sb-sys:int-sap lookup) (* i 4))
                             (logior #xff000000 (aref palette i))
                             (sb-sys:sap-ref-8 (sb-sys:int-sap pixels) i) (ldb (byte 8 0) i)))
                     (multiple-value-bind (colors address) (layout "sun/awt/X11/ColorData")
                       (%x11-java-instance data "set_color_data(J)V" address)
                       (%x11-java-instance colors "set_awt_numICMcolors(I)V" count)
                       (%x11-java-instance colors "set_awt_icmLUT(J)V" lookup)
                       (%x11-java-instance colors "set_awt_icmLUT2Colors(J)V" pixels)))))
               (setf (gethash handle *x11-configurations*) (list* screen visual-id (remove handle allocations))
                     (slot-value this '|aData|) handle
                     (slot-value this '|bitsPerPixel|) bpp
                     success t)))
        (unless success (mapc #'%free-unsafe-memory allocations))))
    nil))
(%define-x11-native "sun/awt/X11GraphicsConfig.dispose(J)V" (handle)
  (let ((allocations
          (bt:with-recursive-lock-held (*x11-lock*)
            (when-let ((record (gethash handle *x11-configurations*)))
              (remhash handle *x11-configurations*)
              (cons handle (cddr record))))))
    (mapc #'%free-unsafe-memory allocations)) nil)
(%define-x11-native "sun/awt/X11GraphicsConfig.isTranslucencyCapable(J)Z" (this handle)
  (declare (ignore this handle))
  ;; The current Xlib surface path has no composited ARGB window support.
  0)
(%define-x11-native "sun/awt/X11GraphicsConfig.getXResolution(I)D" (this screen)
  (declare (ignore this))
  (/ (* 25.4d0 (%libc "XDisplayWidth" sb-alien:int
                 ((* t) (%x11-display)) (sb-alien:int screen)))
     (%libc "XDisplayWidthMM" sb-alien:int ((* t) (%x11-display)) (sb-alien:int screen))))
(%define-x11-native "sun/awt/X11GraphicsConfig.getYResolution(I)D" (this screen)
  (declare (ignore this))
  (/ (* 25.4d0 (%libc "XDisplayHeight" sb-alien:int
                 ((* t) (%x11-display)) (sb-alien:int screen)))
     (%libc "XDisplayHeightMM" sb-alien:int ((* t) (%x11-display)) (sb-alien:int screen))))
(%define-x11-native "sun/java2d/x11/X11SurfaceData.initIDs(Ljava/lang/Class;)V" (xor-class)
  (declare (ignore xor-class)) nil)
(%define-x11-native "sun/java2d/x11/X11SurfaceData.isShmPMAvailable()Z" ()
  ;; This backend does not allocate shared-memory pixmaps.
  0)
(%define-xlib "XSupportsLocale" "()Z" "XSupportsLocale" :boolean ())
(%define-xlib "XSetLocaleModifiers" "(Ljava/lang/String;)Ljava/lang/String;" "XSetLocaleModifiers" :string (:string))
(%define-xlib "XCreateFontCursor" "(JI)I" "XCreateFontCursor" sb-alien:unsigned-long (:pointer sb-alien:unsigned-int))
(%define-xlib "XSelectInput" "(JJJ)V" "XSelectInput" :void (:pointer sb-alien:unsigned-long sb-alien:long))
(%define-xlib "XQueryExtension" "(JLjava/lang/String;JJJ)Z" "XQueryExtension" :boolean
  (:pointer :string :pointer :pointer :pointer))
(%define-x11-native "sun/awt/X11/XlibWrapper.XkbLibraryVersion(JJ)Z" (major minor)
  ;; XKB 1.0 is the client API requested by the JDK bridge.
  (setf (sb-sys:sap-ref-32 (sb-sys:int-sap major) 0) 1
        (sb-sys:sap-ref-32 (sb-sys:int-sap minor) 0) 0)
  (if (zerop (%libc "XkbLibraryVersion" sb-alien:int
               ((* t) (%x11-pointer major)) ((* t) (%x11-pointer minor)))) 0 1))
(%define-xlib "XkbQueryExtension" "(JJJJJJ)Z" "XkbQueryExtension" :boolean
  (:pointer :pointer :pointer :pointer :pointer :pointer))
(%define-xlib "XkbSelectEvents" "(JJJJ)V" "XkbSelectEvents" :void
  (:pointer sb-alien:unsigned-int sb-alien:unsigned-int sb-alien:unsigned-int))
(%define-xlib "XkbSelectEventDetails" "(JJJJJ)V" "XkbSelectEventDetails" :void
  (:pointer sb-alien:unsigned-int sb-alien:unsigned-int sb-alien:unsigned-long sb-alien:unsigned-long))
(%define-xlib "XkbGetMap" "(JJJ)J" "XkbGetMap" :pointer
  (:pointer sb-alien:unsigned-int sb-alien:unsigned-int))
(%define-xlib "XkbGetUpdatedMap" "(JJJ)J" "XkbGetUpdatedMap" sb-alien:int
  (:pointer sb-alien:unsigned-int :pointer))
(%define-xlib "XkbFreeKeyboard" "(JJZ)V" "XkbFreeKeyboard" :void
  (:pointer sb-alien:unsigned-int sb-alien:int))
(%define-x11-native "sun/awt/X11/XlibWrapper.XkbSetDetectableAutoRepeat(JZ)V" (display detectable)
  (%libc "XkbSetDetectableAutoRepeat" sb-alien:int
    ((* t) (%x11-pointer display)) (sb-alien:int detectable) ((* t) nil)) nil)

(%define-x11-native "sun/awt/X11/XlibWrapper.XInternAtoms(J[Ljava/lang/String;ZJ)I" (display names only-if-exists atoms)
  (let* ((count (length (java-array-data names)))
         (strings nil)
         (pointers (sb-alien:make-alien (* sb-alien:char) (max count 1))))
    (unwind-protect
         (progn
           (dotimes (i count)
             (let ((string (sb-alien:make-alien-string (lstring (jaref names i)) :external-format :utf-8)))
               (push string strings)
               (setf (sb-alien:deref pointers i) string)))
           (%libc "XInternAtoms" sb-alien:int
             ((* t) (%x11-pointer display))
             ((* (* sb-alien:char)) pointers) (sb-alien:int count)
             (sb-alien:int only-if-exists) ((* t) (%x11-pointer atoms))))
      (mapc #'sb-alien:free-alien strings)
      (sb-alien:free-alien pointers))))
(%define-xlib "InternAtom" "(JLjava/lang/String;I)J" "XInternAtom" sb-alien:unsigned-long
  (:pointer :string sb-alien:int))
(%define-xlib "XGetWindowAttributes" "(JJJ)I" "XGetWindowAttributes" sb-alien:int
  (:pointer sb-alien:unsigned-long :pointer))
(%define-xlib "XGetWindowProperty" "(JJJJJJJJJJJJ)I" "XGetWindowProperty" sb-alien:int
  (:pointer sb-alien:unsigned-long sb-alien:unsigned-long sb-alien:long sb-alien:long
   sb-alien:int sb-alien:unsigned-long :pointer :pointer :pointer :pointer :pointer))
(%define-xlib "XSync" "(JI)V" "XSync" :void (:pointer sb-alien:int))
(%define-xlib "XFlush" "(J)V" "XFlush" :void (:pointer))

(sb-alien:define-alien-type openldk-xerror-event
  (sb-alien:struct openldk-xerror-event
    (event-type sb-alien:int) (display (* t)) (resource-id sb-alien:unsigned-long)
    (serial sb-alien:unsigned-long) (error-code sb-alien:unsigned-char)
    (request-code sb-alien:unsigned-char) (minor-code sb-alien:unsigned-char)))

(sb-alien:define-alien-callable %x11-error-handler sb-alien:int
    ((display (* t)) (event (* t)))
  ;; A Lisp condition must never unwind through Xlib's C stack.
  (handler-case
      (if (and *x11-display* (= (%x11-address display) (%x11-address *x11-display*)))
          (let ((size (sb-alien:alien-size openldk-xerror-event :bytes)))
            (unless *x11-error-memory* (setf *x11-error-memory* (%allocate-unsafe-memory size)))
            (%libc "memcpy" (* t) ((* t) (%x11-pointer *x11-error-memory*))
              ((* t) event) (sb-alien:unsigned-long size))
            (%x11-java-static "sun/awt/X11/XErrorHandlerUtil" "globalErrorHandler(JJ)I"
                              (%x11-address display) *x11-error-memory*))
          (if *x11-previous-error-handler*
              (sb-alien:alien-funcall
               (sb-alien:sap-alien (sb-sys:int-sap *x11-previous-error-handler*)
                                  (function sb-alien:int (* t) (* t))) display event)
              0))
    (error (condition)
      (setf (gethash (bt:current-thread) *x11-callback-errors*) condition)
      0)))

(%define-x11-native "sun/awt/X11/XlibWrapper.SetToolkitErrorHandler()J" ()
  (%load-x11)
  (let* ((callback (sb-alien:alien-callable-function '%x11-error-handler))
         (previous (%libc "XSetErrorHandler" (* t)
                     ((* t) (sb-alien:cast callback (* t)))))
         (address (%x11-address previous)))
    (unless (= address (%x11-address callback))
      (setf *x11-previous-error-handler* (unless (zerop address) address)))
    address))
(%define-xlib "XSetErrorHandler" "(J)V" "XSetErrorHandler" :void (:pointer))
(%define-xlib "ServerVendor" "(J)Ljava/lang/String;" "XServerVendor" :string (:pointer))
(%define-xlib "XGetSelectionOwner" "(JJ)J" "XGetSelectionOwner" sb-alien:unsigned-long
  (:pointer sb-alien:unsigned-long))
(%define-xlib "XSetSelectionOwner" "(JJJJ)V" "XSetSelectionOwner" :void
  (:pointer sb-alien:unsigned-long sb-alien:unsigned-long sb-alien:unsigned-long))
(%define-xlib "XCreateWindow" "(JJIIIIIIJJJJ)J" "XCreateWindow" sb-alien:unsigned-long
  (:pointer sb-alien:unsigned-long sb-alien:int sb-alien:int sb-alien:unsigned-int
   sb-alien:unsigned-int sb-alien:unsigned-int sb-alien:int sb-alien:unsigned-int
   :pointer sb-alien:unsigned-long :pointer))
(%define-xlib "XDestroyWindow" "(JJ)V" "XDestroyWindow" :void
  (:pointer sb-alien:unsigned-long))
(%define-xlib "XMapWindow" "(JJ)V" "XMapWindow" :void
  (:pointer sb-alien:unsigned-long))
(%define-xlib "XMapRaised" "(JJ)V" "XMapRaised" :void
  (:pointer sb-alien:unsigned-long))
(%define-xlib "XRaiseWindow" "(JJ)V" "XRaiseWindow" :void
  (:pointer sb-alien:unsigned-long))
(%define-xlib "XLowerWindow" "(JJ)V" "XLowerWindow" :void
  (:pointer sb-alien:unsigned-long))
(%define-xlib "XUnmapWindow" "(JJ)V" "XUnmapWindow" :void
  (:pointer sb-alien:unsigned-long))
(%define-xlib "XQueryTree" "(JJJJJJ)I" "XQueryTree" sb-alien:int
  (:pointer sb-alien:unsigned-long :pointer :pointer :pointer :pointer))
(%define-xlib "XReparentWindow" "(JJJII)V" "XReparentWindow" :void
  (:pointer sb-alien:unsigned-long sb-alien:unsigned-long sb-alien:int sb-alien:int))
(%define-xlib "XGetWMNormalHints" "(JJJJ)I" "XGetWMNormalHints" sb-alien:int
  (:pointer sb-alien:unsigned-long :pointer :pointer))
(%define-xlib "XSetWMNormalHints" "(JJJ)V" "XSetWMNormalHints" :void
  (:pointer sb-alien:unsigned-long :pointer))
(%define-xlib "XChangePropertyImpl" "(JJJJIIJI)V" "XChangeProperty" :void
  (:pointer sb-alien:unsigned-long sb-alien:unsigned-long sb-alien:unsigned-long
   sb-alien:int sb-alien:int :pointer sb-alien:int))
(%define-xlib "XDeleteProperty" "(JJJ)V" "XDeleteProperty" :void
  (:pointer sb-alien:unsigned-long sb-alien:unsigned-long))
(%define-x11-native "sun/awt/X11/XlibWrapper.XGetAtomName(JJ)Ljava/lang/String;" (display atom)
  (%load-x11)
  (let ((name (%libc "XGetAtomName" (* sb-alien:char)
                ((* t) (%x11-pointer display)) (sb-alien:unsigned-long atom))))
    (when (sb-alien:null-alien name)
      (%throw '|java/lang/NullPointerException| "Failed to retrieve atom name."))
    (unwind-protect
         (jstring (sb-alien:cast name sb-alien:c-string))
      (%libc "XFree" sb-alien:int ((* sb-alien:char) name)))))
(%define-xlib "XAllocWMHints" "()J" "XAllocWMHints" :pointer
  ())
(%define-xlib "XSetWMHints" "(JJJ)V" "XSetWMHints" :void
  (:pointer sb-alien:unsigned-long :pointer))
(%define-xlib "XAllocSizeHints" "()J" "XAllocSizeHints" :pointer
  ())
(%define-xlib "XChangeWindowAttributes" "(JJJJ)V" "XChangeWindowAttributes" :void
  (:pointer sb-alien:unsigned-long sb-alien:unsigned-long :pointer))

(defvar *x11-event-pipe* nil)

(defun %x11-event-pipe ()
  "Create a close-on-exec nonblocking wakeup pipe for the toolkit event loop."
  (bt:with-recursive-lock-held (*x11-lock*)
    (or *x11-event-pipe*
        (multiple-value-bind (reader writer) (sb-posix:pipe)
          (dolist (fd (list reader writer))
            (sb-posix:fcntl fd sb-posix:f-setfl
                            (logior (sb-posix:fcntl fd sb-posix:f-getfl) sb-posix:o-nonblock))
            ;; POSIX FD_CLOEXEC is bit zero; SB-POSIX does not export it.
            (sb-posix:fcntl fd sb-posix:f-setfd
                            (logior (sb-posix:fcntl fd sb-posix:f-getfd) 1)))
          (setf *x11-event-pipe* (list reader writer))))))

(sb-alien:define-alien-type openldk-xpollfd
  (sb-alien:struct openldk-xpollfd
    (fd sb-alien:int) (events sb-alien:short) (revents sb-alien:short)))

(defun %x11-wait-for-events (deadline)
  "Poll real X input and toolkit wakeups, releasing the Java AWT lock while waiting."
  (let* ((pipe (%x11-event-pipe))
         (timeout (if (zerop deadline) 500
                      (max 0 (min 500 (- deadline (|java/lang/System.currentTimeMillis()J|))))))
         (fds (sb-alien:make-alien openldk-xpollfd 2)))
    (unwind-protect
         (progn
           (setf (sb-alien:slot (sb-alien:deref fds 0) 'fd)
                 (%libc "XConnectionNumber" sb-alien:int ((* t) (%x11-display)))
                 (sb-alien:slot (sb-alien:deref fds 1) 'fd) (first pipe))
           (dotimes (i 2)
             (setf (sb-alien:slot (sb-alien:deref fds i) 'events) 1
                   (sb-alien:slot (sb-alien:deref fds i) 'revents) 0))
           (let ((unlocked nil))
             (unwind-protect
                  (progn
                    (sb-sys:without-interrupts
                      (%x11-java-static "sun/awt/SunToolkit" "awtUnlock()V")
                      (setf unlocked t))
                    (let ((result (%libc "poll" sb-alien:int
                                    ((* openldk-xpollfd) fds) (sb-alien:unsigned-long 2)
                                    (sb-alien:int timeout))))
                      (when (and (minusp result) (/= (sb-alien:get-errno) sb-posix:eintr))
                        (%throw '|java/awt/AWTError| "X11 event polling failed"))))
               (when unlocked (%x11-java-static "sun/awt/SunToolkit" "awtLock()V"))))
           (unless (zerop (logand (sb-alien:slot (sb-alien:deref fds 0) 'revents) #x38))
             (%throw '|java/awt/AWTError| "X11 connection closed"))
           (unless (zerop (sb-alien:slot (sb-alien:deref fds 1) 'revents))
             (sb-alien:with-alien ((buffer (array sb-alien:unsigned-char 256)))
               (loop while (plusp (%libc "read" sb-alien:long
                                   (sb-alien:int (first pipe))
                                   ((* t) (sb-alien:cast (sb-alien:addr buffer) (* t)))
                                   (sb-alien:unsigned-long 256)))))))
      (sb-alien:free-alien fds)))
  nil)

(%define-x11-native "sun/awt/X11/XToolkit.awt_toolkit_init()V" ()
  (%x11-event-pipe) nil)
(%define-x11-native "sun/awt/X11/XToolkit.waitForEvents(J)V" (deadline)
  (%x11-wait-for-events deadline))
(%define-x11-native "sun/awt/X11/XToolkit.wakeup_poll()V" ()
  (sb-alien:with-alien ((byte sb-alien:unsigned-char 1))
    (%libc "write" sb-alien:long (sb-alien:int (second (%x11-event-pipe)))
      ((* sb-alien:unsigned-char) (sb-alien:addr byte)) (sb-alien:unsigned-long 1))) nil)
(%define-x11-native "sun/awt/X11/XToolkit.awt_output_flush()V" ()
  (%libc "XFlush" sb-alien:int ((* t) (%x11-display))) nil)
(%define-x11-native "sun/awt/X11/XToolkit.getTrayIconDisplayTimeout()J" () 2000)
(%define-x11-native "sun/awt/X11/XToolkit.getNumberOfButtonsImpl()I" (this)
  (declare (ignore this))
  (%libc "XGetPointerMapping" sb-alien:int ((* t) (%x11-display)) ((* t) nil) (sb-alien:int 0)))
(%define-x11-native "sun/awt/X11/XToolkit.getDefaultXColormap()J" ()
  (%libc "XDefaultColormap" sb-alien:unsigned-long ((* t) (%x11-display))
    (sb-alien:int (%libc "XDefaultScreen" sb-alien:int ((* t) (%x11-display))))))
(%define-x11-native "sun/awt/X11/XToolkit.nativeLoadSystemColors([I)V" (this colors)
  (declare (ignore this colors))
  ;; The JDK native ensures colormap storage; Java provides fallback theme colors.
  (%x11-colormap (aref (%x11-screen-visuals 0) 0) 0) nil)
(%define-xlib "XEventsQueued" "(JI)I" "XEventsQueued" sb-alien:int (:pointer sb-alien:int))
(%define-xlib "XNextEvent" "(JJ)V" "XNextEvent" :void (:pointer :pointer))
(%define-xlib "XFilterEvent" "(JJ)Z" "XFilterEvent" :boolean (:pointer sb-alien:unsigned-long))
(%define-xlib "XRefreshKeyboardMapping" "(J)V" "XRefreshKeyboardMapping" :void (:pointer))
(%define-xlib "XPeekEvent" "(JJ)V" "XPeekEvent" :void (:pointer :pointer))

(defun %x11-configuration-visual (configuration)
  "Resolve CONFIGURATION's owned native allocation to its server visual."
  (destructuring-bind (screen visual-id &rest allocations)
      (or (gethash (slot-value configuration '|aData|) *x11-configurations*)
          (%throw '|java/awt/AWTError| "Disposed X11 graphics configuration"))
    (declare (ignore allocations))
    (values (or (find visual-id (%x11-screen-visuals screen)
                      :key (lambda (visual) (getf visual :visual)))
                (%throw '|java/awt/AWTError| "Unknown X11 graphics configuration"))
            screen)))
(%define-x11-native "sun/awt/X11GraphicsConfig.getNumColors()I" (this)
  (getf (%x11-configuration-visual this) :entries))

(sb-alien:define-alien-type openldk-xtext-property
  (sb-alien:struct openldk-xtext-property
    (value (* sb-alien:unsigned-char)) (encoding sb-alien:unsigned-long)
    (format sb-alien:int) (items sb-alien:unsigned-long)))
(%define-x11-native "sun/awt/X11/XlibWrapper.SetProperty(JJJLjava/lang/String;)V" (display window atom text)
  (let ((string (sb-alien:make-alien-string (if text (lstring text) "") :external-format :utf-8)))
    (unwind-protect
         (sb-alien:with-alien ((strings (* sb-alien:char) string) (property openldk-xtext-property))
           (setf (sb-alien:slot property 'value) nil)
           (unwind-protect
                (let ((status (%libc "Xutf8TextListToTextProperty" sb-alien:int
                                ((* t) (%x11-pointer display))
                                ((* (* sb-alien:char)) (sb-alien:addr strings))
                                (sb-alien:int 1) (sb-alien:int 3)
                                ((* openldk-xtext-property) (sb-alien:addr property)))))
                  (when (minusp status)
                    (%throw '|java/awt/AWTError| "Cannot encode X11 text property"))
                  (%libc "XChangeProperty" sb-alien:int
                    ((* t) (%x11-pointer display)) (sb-alien:unsigned-long window)
                    (sb-alien:unsigned-long atom) (sb-alien:unsigned-long (sb-alien:slot property 'encoding))
                    (sb-alien:int (sb-alien:slot property 'format)) (sb-alien:int 0)
                    ((* sb-alien:unsigned-char) (sb-alien:slot property 'value))
                    (sb-alien:int (sb-alien:slot property 'items))))
             (unless (sb-alien:null-alien (sb-alien:slot property 'value))
               (%libc "XFree" sb-alien:int ((* sb-alien:unsigned-char) (sb-alien:slot property 'value))))))
      (sb-alien:free-alien string))))
(%define-x11-native "sun/awt/SunToolkit.closeSplashScreen()V" ()
  ;; The OpenLDK launcher does not open a splash window, so there is none to close.
  nil)
(%define-x11-native "sun/awt/X11GraphicsConfig.isTranslucencyCapable(J)Z" (this handle)
  (declare (ignore this handle))
  ;; The current Xlib surface path has no composited ARGB window support.
  0)

(%define-xlib "XSetWindowBackground" "(JJJ)V" "XSetWindowBackground" :void
  (:pointer sb-alien:unsigned-long sb-alien:unsigned-long))
(%define-xlib "XSetWindowBackgroundPixmap" "(JJJ)V" "XSetWindowBackgroundPixmap" :void
  (:pointer sb-alien:unsigned-long sb-alien:unsigned-long))
(%define-xlib "XMoveWindow" "(JJII)V" "XMoveWindow" :void
  (:pointer sb-alien:unsigned-long sb-alien:int sb-alien:int))
(%define-xlib "XResizeWindow" "(JJII)V" "XResizeWindow" :void
  (:pointer sb-alien:unsigned-long sb-alien:unsigned-int sb-alien:unsigned-int))
(%define-xlib "XMoveResizeWindow" "(JJIIII)V" "XMoveResizeWindow" :void
  (:pointer sb-alien:unsigned-long sb-alien:int sb-alien:int sb-alien:unsigned-int sb-alien:unsigned-int))
(%define-xlib "XClearWindow" "(JJ)V" "XClearWindow" :void
  (:pointer sb-alien:unsigned-long))
(%define-xlib "XSendEvent" "(JJZJJ)I" "XSendEvent" sb-alien:int
  (:pointer sb-alien:unsigned-long sb-alien:int sb-alien:long :pointer))
(%define-xlib "XConfigureWindow" "(JJJJ)V" "XConfigureWindow" :void
  (:pointer sb-alien:unsigned-long sb-alien:unsigned-int :pointer))
(%define-xlib "XIconifyWindow" "(JJJ)I" "XIconifyWindow" sb-alien:int
  (:pointer sb-alien:unsigned-long sb-alien:int))
(%define-x11-native "sun/awt/X11/XlibWrapper.XSetInputFocus(JJ)V" (display window)
  (%libc "XSetInputFocus" sb-alien:int ((* t) (%x11-pointer display))
    (sb-alien:unsigned-long window) (sb-alien:int 1) (sb-alien:unsigned-long 0)) nil)
(%define-x11-native "sun/awt/X11/XlibWrapper.XSetInputFocus2(JJJ)V" (display window time)
  (%libc "XSetInputFocus" sb-alien:int ((* t) (%x11-pointer display))
    (sb-alien:unsigned-long window) (sb-alien:int 1) (sb-alien:unsigned-long time)) nil)

(%define-x11-native "sun/awt/X11/XWindowPeer.getLocalHostname()Ljava/lang/String;" ()
  (sb-alien:with-alien ((name (array sb-alien:char 1024)))
    (setf (sb-alien:deref name 1023) 0)
    (if (zerop (%libc "gethostname" sb-alien:int
                 ((* sb-alien:char) (sb-alien:cast (sb-alien:addr name) (* sb-alien:char)))
                 (sb-alien:unsigned-long 1023)))
        (jstring (sb-alien:cast (sb-alien:addr name) sb-alien:c-string))
        (jstring "localhost"))))

(%define-x11-native "sun/awt/X11/XWindowPeer.getJvmPID()I" ()
  (sb-posix:getpid))

(sb-alien:define-alien-type openldk-ximage-prefix
  (sb-alien:struct openldk-ximage-prefix
    (width sb-alien:int) (height sb-alien:int) (xoffset sb-alien:int) (format sb-alien:int)
    (data (* t)) (byte-order sb-alien:int) (bitmap-unit sb-alien:int)
    (bitmap-bit-order sb-alien:int) (bitmap-pad sb-alien:int) (depth sb-alien:int)
    (bytes-per-line sb-alien:int) (bits-per-pixel sb-alien:int)))

(%define-xlib "XGetIconSizes" "(JJJJ)I" "XGetIconSizes" sb-alien:int
  (:pointer sb-alien:unsigned-long :pointer :pointer))
(%define-xlib "XCreatePixmap" "(JJIII)J" "XCreatePixmap" sb-alien:unsigned-long
  (:pointer sb-alien:unsigned-long sb-alien:unsigned-int sb-alien:unsigned-int sb-alien:unsigned-int))
(%define-xlib "XFreePixmap" "(JJ)V" "XFreePixmap" :void
  (:pointer sb-alien:unsigned-long))
(%define-xlib "XCreateBitmapFromData" "(JJJII)J" "XCreateBitmapFromData" sb-alien:unsigned-long
  (:pointer sb-alien:unsigned-long :pointer sb-alien:unsigned-int sb-alien:unsigned-int))
(%define-xlib "XCreateGC" "(JJJJ)J" "XCreateGC" :pointer
  (:pointer sb-alien:unsigned-long sb-alien:unsigned-long :pointer))
(%define-xlib "XFreeGC" "(JJ)V" "XFreeGC" :void (:pointer :pointer))
(%define-xlib "XCreateImage" "(JJIIIJIIII)J" "XCreateImage" :pointer
  (:pointer :pointer sb-alien:unsigned-int sb-alien:int sb-alien:int :pointer
   sb-alien:unsigned-int sb-alien:unsigned-int sb-alien:int sb-alien:int))
(%define-xlib "XPutImage" "(JJJJIIIIII)V" "XPutImage" :void
  (:pointer sb-alien:unsigned-long :pointer :pointer sb-alien:int sb-alien:int
   sb-alien:int sb-alien:int sb-alien:unsigned-int sb-alien:unsigned-int))
(%define-x11-native "sun/awt/X11/XlibWrapper.XDestroyImage(J)V" (address)
  ;; Java owns the Unsafe-allocated pixel buffer and frees it separately.
  (let ((image (sb-alien:sap-alien (sb-sys:int-sap address) (* openldk-ximage-prefix))))
    (setf (sb-alien:slot (sb-alien:deref image) 'data) nil)
    (%libc "XDestroyImage" sb-alien:int ((* openldk-ximage-prefix) image))) nil)
(%define-x11-native "sun/awt/X11/XlibWrapper.XGetWMHints(JJJ)V" (display window destination)
  (let ((hints (%libc "XGetWMHints" (* t) ((* t) (%x11-pointer display))
                 (sb-alien:unsigned-long window)))
        (size (%x11-java-static "sun/awt/X11/XWMHints" "getSize()I")))
    (if (sb-alien:null-alien hints)
        (%libc "memset" (* t) ((* t) (%x11-pointer destination)) (sb-alien:int 0) (sb-alien:unsigned-long size))
        (unwind-protect
             (%libc "memcpy" (* t) ((* t) (%x11-pointer destination)) ((* t) hints) (sb-alien:unsigned-long size))
          (%libc "XFree" sb-alien:int ((* t) hints))))) nil)

(%define-x11-native "sun/awt/X11/XlibWrapper.XGetInputFocus(J)J" (display)
  (sb-alien:with-alien ((window sb-alien:unsigned-long) (revert-to sb-alien:int))
    (%libc "XGetInputFocus" sb-alien:int ((* t) (%x11-pointer display))
      ((* sb-alien:unsigned-long) (sb-alien:addr window)) ((* sb-alien:int) (sb-alien:addr revert-to)))
    window))
(%define-xlib "XTranslateCoordinates" "(JJJJJJJJ)I" "XTranslateCoordinates" sb-alien:int
  (:pointer sb-alien:unsigned-long sb-alien:unsigned-long sb-alien:int sb-alien:int :pointer :pointer :pointer))
(%define-xlib "XQueryPointer" "(JJJJJJJJJ)Z" "XQueryPointer" :boolean
  (:pointer sb-alien:unsigned-long :pointer :pointer :pointer :pointer :pointer :pointer :pointer))
(%define-xlib "XGetGeometry" "(JJJJJJJJJ)I" "XGetGeometry" sb-alien:int
  (:pointer sb-alien:unsigned-long :pointer :pointer :pointer :pointer :pointer :pointer :pointer))
(%define-xlib "XGetPointerMapping" "(JJI)I" "XGetPointerMapping" sb-alien:int
  (:pointer :pointer sb-alien:int))
(%define-xlib "XBell" "(JI)V" "XBell" :void (:pointer sb-alien:int))
(%define-xlib "XGetDefault" "(JLjava/lang/String;Ljava/lang/String;)Ljava/lang/String;" "XGetDefault" :string
  (:pointer :string :string))

(defvar *x11-input-method* nil)

(%define-x11-native "java/awt/AWTEvent.nativeSetSource(Ljava/awt/peer/ComponentPeer;)V" (this peer)
  ;; The Unix JDK implementation is empty: Java already retargets the event.
  (declare (ignore this peer)) nil)

(%define-x11-native "sun/awt/X11InputMethodBase.initIDs()V" ()
  ;; CL implementations use Java slots rather than native cached field IDs.
  nil)
(%define-x11-awt-native "sun/awt/X11/XInputMethod.openXIMNative(J)Z" (this display)
  (declare (ignore this))
  (bt:with-recursive-lock-held (*x11-lock*)
    (unless *x11-input-method*
      (let ((method (%libc "XOpenIM" (* t) ((* t) (%x11-pointer display))
                      ((* t) nil) ((* t) nil) ((* t) nil))))
        (unless (sb-alien:null-alien method)
          (setf *x11-input-method* method))))
    (if *x11-input-method* 1 0)))

(%define-x11-awt-native "sun/awt/X11InputMethodBase.isCompositionEnabledNative()Z" (this)
  (let ((context (slot-value this '|pData|)))
    (if (zerop context) 0
        (sb-alien:with-alien ((state sb-alien:unsigned-long 0))
          (let ((attributes (%libc "XVaCreateNestedList" (* t)
                              (sb-alien:int 0) (sb-alien:c-string "preeditState")
                              ((* sb-alien:unsigned-long) (sb-alien:addr state)) ((* t) nil))))
            (when (sb-alien:null-alien attributes)
              (%throw '|java/lang/OutOfMemoryError| "Cannot allocate XIM attributes"))
            (unwind-protect
                 (let ((error (%libc "XGetICValues" (* t) ((* t) (%x11-pointer context))
                                (sb-alien:c-string "preeditAttributes") ((* t) attributes) ((* t) nil))))
                   (unless (sb-alien:null-alien error)
                     (%throw '|java/lang/UnsupportedOperationException| "XIM composition state is unavailable"))
                   (if (= state 1) 1 0))
              (%libc "XFree" sb-alien:int ((* t) attributes))))))))
(%define-x11-awt-native "sun/awt/X11InputMethodBase.setCompositionEnabledNative(Z)Z" (this enable)
  (let ((context (slot-value this '|pData|)))
    (if (zerop context) 0
        (let ((attributes (%libc "XVaCreateNestedList" (* t)
                            (sb-alien:int 0) (sb-alien:c-string "preeditState")
                            (sb-alien:unsigned-long (if (zerop enable) 2 1)) ((* t) nil))))
          (when (sb-alien:null-alien attributes)
            (%throw '|java/lang/OutOfMemoryError| "Cannot allocate XIM attributes"))
          (unwind-protect
               (let ((error (%libc "XSetICValues" (* t) ((* t) (%x11-pointer context))
                              (sb-alien:c-string "preeditAttributes") ((* t) attributes) ((* t) nil))))
                 (unless (sb-alien:null-alien error)
                   (%throw '|java/lang/UnsupportedOperationException| "XIM composition state is unavailable"))
                 1)
            (%libc "XFree" sb-alien:int ((* t) attributes)))))))
(%define-x11-awt-native "sun/awt/X11InputMethodBase.disposeXIC()V" (this)
  (let ((context (slot-value this '|pData|)))
    (unless (zerop context)
      (%libc "XDestroyIC" sb-alien:void ((* t) (%x11-pointer context)))
      (setf (slot-value this '|pData|) 0))) nil)
(%define-x11-awt-native "sun/awt/X11InputMethodBase.resetXIC()Ljava/lang/String;" (this)
  (let ((context (slot-value this '|pData|)))
    (unless (zerop context)
      (let ((text (%libc "Xutf8ResetIC" (* sb-alien:char) ((* t) (%x11-pointer context)))))
        (unless (sb-alien:null-alien text)
          (unwind-protect (jstring (sb-alien:cast text sb-alien:c-string))
            (%libc "XFree" sb-alien:int ((* sb-alien:char) text))))))))

(%define-x11-native "sun/awt/UNIXToolkit.nativeSync()V" (this)
  (declare (ignore this))
  (%libc "XSync" sb-alien:int ((* t) (%x11-display)) (sb-alien:int 0)) nil)

(%define-xlib "XGrabServer" "(J)V" "XGrabServer" :void (:pointer))
(%define-x11-native "sun/awt/X11/XlibWrapper.XUngrabServer(J)V" (display)
  (%libc "XUngrabServer" sb-alien:int ((* t) (%x11-pointer display)))
  ;; Match the JDK bridge's flush: selection-owner changes must be visible
  ;; before another client starts querying XSETTINGS or window-manager state.
  (%libc "XSync" sb-alien:int ((* t) (%x11-pointer display)) (sb-alien:int 0)) nil)

(sb-alien:define-alien-type openldk-xwindow-changes
  (sb-alien:struct openldk-xwindow-changes
    (x sb-alien:int) (y sb-alien:int) (width sb-alien:int) (height sb-alien:int)
    (border-width sb-alien:int) (sibling sb-alien:unsigned-long) (stack-mode sb-alien:int)))

(%define-x11-native "sun/awt/X11/XlibWrapper.SetZOrder(JJJ)V" (display window above)
  (sb-alien:with-alien ((changes openldk-xwindow-changes))
    (setf (sb-alien:slot changes 'sibling) above
          (sb-alien:slot changes 'stack-mode) (if (zerop above) 0 1))
    (%libc "XConfigureWindow" sb-alien:int ((* t) (%x11-pointer display))
      (sb-alien:unsigned-long window) (sb-alien:unsigned-int (if (zerop above) 64 96))
      ((* openldk-xwindow-changes) (sb-alien:addr changes)))) nil)
