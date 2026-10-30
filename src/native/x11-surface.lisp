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
(defvar *x11-software-surfaces* (make-hash-table :test #'eq :weakness :key :synchronized t))
(defstruct x11-software-surface image window visual screen display gc invalid configuration)
(defun %x11-drawable-size (window)
  "Read a drawable's physical size, including any Java UI scaling."
  (sb-alien:with-alien ((root sb-alien:unsigned-long) (x sb-alien:int) (y sb-alien:int)
                       (width sb-alien:unsigned-int) (height sb-alien:unsigned-int)
                       (border sb-alien:unsigned-int) (depth sb-alien:unsigned-int))
    (when (zerop (%libc "XGetGeometry" sb-alien:int ((* t) (%x11-display))
                   (sb-alien:unsigned-long window) ((* sb-alien:unsigned-long) (sb-alien:addr root))
                   ((* sb-alien:int) (sb-alien:addr x)) ((* sb-alien:int) (sb-alien:addr y))
                   ((* sb-alien:unsigned-int) (sb-alien:addr width))
                   ((* sb-alien:unsigned-int) (sb-alien:addr height))
                   ((* sb-alien:unsigned-int) (sb-alien:addr border))
                   ((* sb-alien:unsigned-int) (sb-alien:addr depth))))
      (%throw '|java/awt/AWTError| "Cannot query X11 drawable geometry"))
    (values width height)))

(defun %close-x11-software-surface (state)
  "Release an owned X graphics context once, including for an abandoned surface."
  (bt:with-recursive-lock-held (*x11-lock*)
    (when (x11-software-surface-gc state)
      (when (eq (x11-software-surface-display state) *x11-display*)
        (%libc "XFreeGC" sb-alien:int ((* t) *x11-display*) ((* t) (x11-software-surface-gc state))))
      (setf (x11-software-surface-gc state) nil))
    (setf (x11-software-surface-invalid state) t))
  nil)

(%define-x11-native "sun/java2d/x11/XSurfaceData.initOps(Lsun/awt/X11ComponentPeer;Lsun/awt/X11GraphicsConfig;I)V"
    (this peer configuration depth)
  (declare (ignore depth))
  (unless peer (%throw '|java/lang/UnsupportedOperationException| "Accelerated X11 pixmaps are unavailable"))
  (multiple-value-bind (visual screen) (%x11-configuration-visual configuration)
    (let ((window (%x11-java-instance peer "getContentWindow()J")))
      (multiple-value-bind (width height) (%x11-drawable-size window)
        ;; The Java renderer's pixel writer uses the surface color model.
        ;; Its backing raster must use that model too, including indexed and
        ;; 16-bit visuals, rather than an unrelated integer RGB layout.
        (let ((image (%x11-java-instance configuration
                         "createCompatibleImage(II)Ljava/awt/image/BufferedImage;" width height)))
          (let* ((display (%x11-display))
                 (gc (%libc "XCreateGC" (* t) ((* t) display)
                       (sb-alien:unsigned-long window) (sb-alien:unsigned-long 0) ((* t) nil)))
                 (success nil))
            (when (sb-alien:null-alien gc)
              (%throw '|java/lang/OutOfMemoryError| "Cannot allocate X11 graphics context"))
            (unwind-protect
                 (let ((state (make-x11-software-surface :image image :window window :visual visual
                                                         :screen screen :display display :gc gc
                                                         :configuration configuration)))
                   (trivial-garbage:finalize this (lambda () (%close-x11-software-surface state)))
                   (when (and (zerop (getf visual :class)) (= (%x11-color-count visual) 256))
                     (setf (gethash image *graphics-physical-gray-images*) t))
                   (setf (gethash this *x11-software-surfaces*) state
                         (gethash this *graphics-surface-images*) image
                         (gethash this *graphics-surface-image-refreshers*) #'%x11-refresh-surface-image
                         success t))
              (unless success (%libc "XFreeGC" sb-alien:int ((* t) display) ((* t) gc)))))))))
  nil)
(defun %x11-refresh-surface-image (surface)
  "Keep the backing raster consistent with the peer's scaled drawable bounds."
  (bt:with-recursive-lock-held (*x11-lock*)
    (let* ((state (gethash surface *x11-software-surfaces*))
           (image (and state (x11-software-surface-image state))))
      (unless (and state (not (x11-software-surface-invalid state)))
        (%throw '|sun/java2d/InvalidPipeException| "Invalid X11 software surface"))
      ;; getBounds includes the configuration's device scale. Peer dimensions
      ;; can change before the server delivers its ConfigureNotify event.
      (let* ((bounds (%x11-java-instance surface "getBounds()Ljava/awt/Rectangle;"))
             (width (max 1 (slot-value bounds '|width|)))
             (height (max 1 (slot-value bounds '|height|))))
        (unless (and (= width (|getWidth()I| image)) (= height (|getHeight()I| image)))
          (let ((replacement (%x11-java-instance (x11-software-surface-configuration state)
                                "createCompatibleImage(II)Ljava/awt/image/BufferedImage;" width height)))
            ;; Copy samples, preserving both indexed pixels and physical gray
            ;; intensities instead of introducing a color-space round trip.
            (let* ((source (|getRaster()Ljava/awt/image/WritableRaster;| image))
                   (destination (|getRaster()Ljava/awt/image/WritableRaster;| replacement))
                   (copy-width (min width (|getWidth()I| image)))
                   (copy-height (min height (|getHeight()I| image)))
                   (data (|getDataElements(IIIILjava/lang/Object;)Ljava/lang/Object;|
                          source 0 0 copy-width copy-height nil)))
              (|setDataElements(IIIILjava/lang/Object;)V|
               destination 0 0 copy-width copy-height data))
            (when (gethash image *graphics-physical-gray-images*)
              (setf (gethash replacement *graphics-physical-gray-images*) t))
            (setf (x11-software-surface-image state) replacement
                  (gethash surface *graphics-surface-images*) replacement
                  image replacement)))
        image))))

(%define-x11-native
    "sun/java2d/loops/GeneralRenderer.createSolidPixelWriter(Lsun/java2d/SunGraphics2D;Lsun/java2d/SurfaceData;)Lsun/java2d/loops/PixelWriter;" ; lint:suppress max-line-length
    (graphics surface)
  ;; Native ByteGray solid primitives use the surface PixelConverter, whereas
  ;; the generic Java fallback calls the ICC ColorModel. Preserve that native
  ;; intensity convention for physical gray surfaces; other models retain the
  ;; ordinary GeneralRenderer behavior.
  (let* ((model (%x11-java-instance surface "getColorModel()Ljava/awt/image/ColorModel;"))
         (image (gethash surface *graphics-surface-images*))
         (rgb (slot-value graphics '|eargb|))
         (pixel (if (and image (gethash image *graphics-physical-gray-images*))
                    (let ((gray (floor (+ (* .299d0 (ldb (byte 8 16) rgb))
                                          (* .587d0 (ldb (byte 8 8) rgb))
                                          (* .114d0 (ldb (byte 8 0) rgb)) .5d0))))
                      (make-java-array :component-class (%get-java-class-by-bin-name "byte")
                                       :initial-contents (list (if (> gray 127) (- gray 256) gray))))
                    (%x11-java-instance model "getDataElements(ILjava/lang/Object;)Ljava/lang/Object;" rgb nil))))
    (classload "sun/java2d/loops/SolidPixelWriter")
    (let ((writer (%make-java-instance "sun/java2d/loops/SolidPixelWriter")))
      (%x11-java-instance writer "<init>(Ljava/lang/Object;)V" pixel)
      writer)))

(%define-x11-native "sun/java2d/x11/X11SurfaceData.isAccelerationEnabled()Z" ()
  ;; Java's volatile-image manager uses its buffered-image backups. Rendering
  ;; uses the Java software pipes; this backend has no X11Renderer acceleration.
  0)
(%define-x11-native "sun/java2d/x11/XSurfaceData.isDrawableValid()Z" (this)
  (let ((state (gethash this *x11-software-surfaces*)))
    (if (and state (not (x11-software-surface-invalid state))) 1 0)))
(%define-x11-native "sun/java2d/x11/XSurfaceData.setInvalid()V" (this)
  (when-let ((state (gethash this *x11-software-surfaces*)))
    (setf (x11-software-surface-invalid state) t)) nil)
(%define-x11-native "sun/java2d/x11/X11SurfaceData.makePipes()V" (this)
  ;; The software pipes are selected in validatePipe instead of native X11Renderer.
  (declare (ignore this)) nil)
(%define-x11-native "sun/java2d/x11/X11SurfaceData.validatePipe(Lsun/java2d/SunGraphics2D;)V" (this graphics)
  (invoke-special '|validatePipe(Lsun/java2d/SunGraphics2D;)V| '|sun/java2d/SurfaceData| (list this graphics)))
(%define-x11-native "sun/java2d/x11/X11SurfaceData.getRaster(IIII)Ljava/awt/image/Raster;" (this x y width height)
  (let ((state (gethash this *x11-software-surfaces*)))
    (unless state (%throw '|java/awt/AWTError| "Uninitialized X11 surface"))
    (%x11-java-instance (%x11-java-instance (%x11-refresh-surface-image this) "getRaster()Ljava/awt/image/WritableRaster;")
                       "createChild(IIIIII[I)Ljava/awt/image/Raster;" x y width height x y nil)))
(%define-x11-native "sun/java2d/x11/X11SurfaceData.copyArea(Lsun/java2d/SunGraphics2D;IIIIII)Z"
    (this graphics x y width height dx dy)
  (declare (ignore this graphics x y width height dx dy))
  ;; Ask SunGraphics2D to use the ordinary software Blit fallback.
  0)
(%define-x11-native "sun/java2d/x11/XSurfaceData.flushNativeSurface()V" (this)
  (trivial-garbage:cancel-finalization this)
  (when-let ((state (gethash this *x11-software-surfaces*)))
    (%close-x11-software-surface state)
    (remhash this *x11-software-surfaces*)
    (remhash this *graphics-surface-images*)
    (remhash this *graphics-surface-image-refreshers*)) nil)

(defun %x11-present-surface (surface)
  "Transfer a software surface to its actual X drawable using the server pixel format."
  (when-let ((state (gethash surface *x11-software-surfaces*)))
    (unless (x11-software-surface-invalid state)
      (let* ((buffer (%x11-refresh-surface-image surface))
             (width (|getWidth()I| buffer)) (height (|getHeight()I| buffer))
             (visual (x11-software-surface-visual state))
             (image (%libc "XCreateImage" (* openldk-ximage-prefix)
                      ((* t) (%x11-display)) ((* t) (%x11-pointer (getf visual :pointer)))
                      (sb-alien:unsigned-int (getf visual :depth)) (sb-alien:int 2)
                      (sb-alien:int 0) ((* t) nil) (sb-alien:unsigned-int width)
                      (sb-alien:unsigned-int height) (sb-alien:int 32) (sb-alien:int 0))))
        (when (sb-alien:null-alien image) (%throw '|java/lang/OutOfMemoryError| "Cannot allocate X11 image"))
        (unwind-protect
             (let* ((size (* height (sb-alien:slot (sb-alien:deref image) 'bytes-per-line)))
                    (data (%libc "calloc" (* t) (sb-alien:unsigned-long 1) (sb-alien:unsigned-long size))))
               (when (sb-alien:null-alien data) (%throw '|java/lang/OutOfMemoryError| "Cannot allocate X11 image pixels"))
               (setf (sb-alien:slot (sb-alien:deref image) 'data) data)
               (let ((pixels (%graphics-image-pixels buffer 0 0 width height)))
                 (dotimes (y height)
                   (dotimes (x width)
                     (let* ((rgb (aref pixels (+ x (* y width))))
                            (pixel (%x11-rgb-to-pixel rgb visual (x11-software-surface-screen state))))
                       (%libc "XPutPixel" sb-alien:int ((* openldk-ximage-prefix) image)
                         (sb-alien:int x) (sb-alien:int y) (sb-alien:unsigned-long pixel))))))
               (bt:with-recursive-lock-held (*x11-lock*)
                 (unless (or (x11-software-surface-invalid state) (null (x11-software-surface-gc state)))
                   (%libc "XPutImage" sb-alien:int ((* t) (%x11-display))
                     (sb-alien:unsigned-long (x11-software-surface-window state))
                     ((* t) (x11-software-surface-gc state)) ((* openldk-ximage-prefix) image)
                     (sb-alien:int 0) (sb-alien:int 0) (sb-alien:int 0) (sb-alien:int 0)
                     (sb-alien:unsigned-int width) (sb-alien:unsigned-int height))
                   (%libc "XFlush" sb-alien:int ((* t) (%x11-display)))
                   (%x11-check-callback-errors))))
          (%libc "XDestroyImage" sb-alien:int ((* openldk-ximage-prefix) image))))))
  nil)
(defun %x11-present-graphics (graphics)
  "Present a completed Java Graphics operation when its destination is an X window."
  (when (slot-exists-p graphics '|surfaceData|)
    (%x11-present-surface (slot-value graphics '|surfaceData|))))
(defmacro %define-x11-presenting-calls (&rest signatures)
  "Present Java rendering operations after their software pipeline finishes."
  `(progn
     ,@(loop for signature in signatures
             for count = (count-parameters (drop (position #\( signature) signature))
             for args = (loop repeat count collect (gensym "ARG"))
             collect `(defmethod ,(intern signature :openldk) :after (graphics ,@args)
                        (declare (ignore ,@args)) (%x11-present-graphics graphics)))))
(%define-x11-presenting-calls
 "fillRect(IIII)V" "drawRect(IIII)V" "drawLine(IIII)V" "clearRect(IIII)V"
 "drawOval(IIII)V" "fillOval(IIII)V" "drawArc(IIIIII)V" "fillArc(IIIIII)V"
 "drawRoundRect(IIIIII)V" "fillRoundRect(IIIIII)V" "copyArea(IIIIII)V"
 "draw(Ljava/awt/Shape;)V" "fill(Ljava/awt/Shape;)V"
 "drawString(Ljava/lang/String;II)V" "drawString(Ljava/lang/String;FF)V"
 "drawGlyphVector(Ljava/awt/font/GlyphVector;FF)V"
 "drawPolygon([I[II)V" "fillPolygon([I[II)V" "drawPolyline([I[II)V"
 "drawImage(Ljava/awt/Image;IILjava/awt/image/ImageObserver;)Z"
 "drawImage(Ljava/awt/Image;IIIILjava/awt/image/ImageObserver;)Z"
 "drawImage(Ljava/awt/Image;IILjava/awt/Color;Ljava/awt/image/ImageObserver;)Z"
 "drawImage(Ljava/awt/Image;IIIILjava/awt/Color;Ljava/awt/image/ImageObserver;)Z"
 "drawImage(Ljava/awt/Image;IIIIIIIILjava/awt/image/ImageObserver;)Z"
 "drawImage(Ljava/awt/Image;IIIIIIIILjava/awt/Color;Ljava/awt/image/ImageObserver;)Z"
 "drawImage(Ljava/awt/Image;Ljava/awt/geom/AffineTransform;Ljava/awt/image/ImageObserver;)Z"
 "drawImage(Ljava/awt/image/BufferedImage;Ljava/awt/image/BufferedImageOp;II)V"
 "drawRenderedImage(Ljava/awt/image/RenderedImage;Ljava/awt/geom/AffineTransform;)V")
