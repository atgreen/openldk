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

;;; The JDK's GeneralRenderer and Marlin pipelines run as Java code. Their
;;; surfaces keep Java rasters instead of HotSpot native SurfaceDataOps.
;;; These entry points supply the remaining pixel compositing primitives.

(defmacro %define-graphics-id-initializers (&rest signatures)
  "Define field-ID cache initializers that the CLOS object model does not need."
  `(progn
     ,@(loop for signature in signatures
             collect `(defun ,(intern signature :openldk) (&rest classes)
                        "No native field-ID cache is needed for Java/CLOS storage."
                        (declare (ignore classes))
                        nil))))

(%define-graphics-id-initializers
 "java/awt/AWTEvent.initIDs()V"
 "java/awt/Event.initIDs()V"
 "java/awt/Checkbox.initIDs()V"
 "java/awt/CheckboxMenuItem.initIDs()V"
 "java/awt/Choice.initIDs()V"
 "java/awt/Component.initIDs()V"
 "java/awt/Container.initIDs()V"
 "java/awt/Cursor.initIDs()V"
 "java/awt/Dialog.initIDs()V"
 "java/awt/Dimension.initIDs()V"
 "java/awt/FontMetrics.initIDs()V"
 "java/awt/Frame.initIDs()V"
 "java/awt/Insets.initIDs()V"
 "java/awt/Label.initIDs()V"
 "java/awt/Menu.initIDs()V"
 "java/awt/MenuBar.initIDs()V"
 "java/awt/MenuItem.initIDs()V"
 "java/awt/ScrollPane.initIDs()V"
 "java/awt/ScrollPaneAdjustable.initIDs()V"
 "java/awt/Scrollbar.initIDs()V"
 "java/awt/TextArea.initIDs()V"
 "java/awt/TrayIcon.initIDs()V"
 "java/awt/Window.initIDs()V"
 "java/awt/event/InputEvent.initIDs()V"
 "java/awt/event/KeyEvent.initIDs()V"
 "java/awt/event/MouseEvent.initIDs()V"
 "java/awt/Font.initIDs()V"
 "java/awt/image/Kernel.initIDs()V"
 "sun/awt/image/ShortComponentRaster.initIDs()V"
 "sun/awt/image/ImageRepresentation.initIDs()V"
 "sun/java2d/SurfaceData.initIDs()V"
 "sun/java2d/pipe/Region.initIDs()V"
 "sun/java2d/pipe/ShapeSpanIterator.initIDs()V"
 "sun/java2d/pipe/SpanClipRenderer.initIDs(Ljava/lang/Class;Ljava/lang/Class;)V"
 "sun/awt/image/BufImgSurfaceData.initIDs(Ljava/lang/Class;Ljava/lang/Class;)V"
 "sun/java2d/Disposer.initIDs()V")

(defun |sun/java2d/loops/GraphicsPrimitiveMgr.initIDs(Ljava/lang/Class;Ljava/lang/Class;Ljava/lang/Class;Ljava/lang/Class;Ljava/lang/Class;Ljava/lang/Class;Ljava/lang/Class;Ljava/lang/Class;Ljava/lang/Class;Ljava/lang/Class;Ljava/lang/Class;)V| (&rest classes) ; lint:suppress max-line-length
  "Preserve class initialization performed by JNI field and method ID lookups."
  ;; In particular SunHints initializes the value table used by default font
  ;; contexts. Resolving its Class literal alone does not initialize it.
  (dolist (class classes)
    (%clinit (get-ldk-class-for-java-class class)))
  nil)

(defun |sun/java2d/SurfaceData.isOpaqueGray(Ljava/awt/image/IndexColorModel;)Z| (model)
  "Use the IndexColorModel property retained by Java, as the JDK native does."
  (if model (slot-value model '|allgrayopaque|) 0))

(defmethod |initRaster(Ljava/lang/Object;IIIIIILjava/awt/image/IndexColorModel;)V|
    (this data offset bit-offset width height pixel-stride scanline-stride palette)
  "Retain Java raster storage; CL loops do not allocate native SurfaceDataOps."
  (declare (ignore this data offset bit-offset width height pixel-stride scanline-stride palette))
  nil)

(defun %graphics-static (class-name field)
  "Return an initialized bootstrap graphics class's static FIELD."
  (let ((class (classload class-name)))
    (%clinit class)
    (slot-value (symbol-value (intern (format nil "+static-~A+" class-name)
                                     (ldk-class-package class)))
                field)))

(defun |sun/java2d/loops/GraphicsPrimitiveMgr.registerNativeLoops()V| ()
  "Register CL alpha primitives alongside the JDK's Java software loops."
  (let* ((any (%graphics-static "sun/java2d/loops/SurfaceType" '|Any|))
         (color (%graphics-static "sun/java2d/loops/SurfaceType" '|AnyColor|))
         (alpha (%graphics-static "sun/java2d/loops/CompositeType" '|AnyAlpha|))
         (primitives
           (loop for name in '("Blit" "MaskBlit" "MaskFill")
                 for class-name = (concatenate 'string "sun/java2d/loops/" name)
                 for class = (classload class-name)
                 do (%clinit class)
                 collect (let ((primitive (%make-java-instance class-name)))
                           (|<init>(JLsun/java2d/loops/SurfaceType;Lsun/java2d/loops/CompositeType;Lsun/java2d/loops/SurfaceType;)V|
                            primitive 0 (if (string= name "MaskFill") color any) alpha any)
                           primitive))))
    (|sun/java2d/loops/GraphicsPrimitiveMgr.register([Lsun/java2d/loops/GraphicsPrimitive;)V|
     (make-java-array :component-class (%get-java-class-by-bin-name "sun/java2d/loops/GraphicsPrimitive")
                      :initial-contents primitives))))

(defun %graphics-blend (source destination rule extra-alpha coverage)
  "Composite straight ARGB pixels using an AlphaComposite rule and mask coverage."
  (let* ((sa (* (/ (ldb (byte 8 24) source) 255d0) extra-alpha))
         (da (/ (ldb (byte 8 24) destination) 255d0))
         (mask (/ coverage 255d0)))
    (multiple-value-bind (fs fd)
        (case rule
          (1 (values 0d0 0d0))
          (2 (values 1d0 0d0))
          (3 (values 1d0 (- 1d0 sa)))
          (4 (values (- 1d0 da) 1d0))
          (5 (values da 0d0))
          (6 (values 0d0 sa))
          (7 (values (- 1d0 da) 0d0))
          (8 (values 0d0 (- 1d0 sa)))
          (9 (values 0d0 1d0))
          (10 (values da (- 1d0 sa)))
          (11 (values (- 1d0 da) sa))
          (12 (values (- 1d0 da) (- 1d0 sa)))
          (otherwise (internal-error "Unknown AlphaComposite rule ~A" rule)))
      (let* ((source-weight (* mask fs sa))
             (dest-weight (* da (+ (* mask fd) (- 1d0 mask))))
             (alpha (+ source-weight dest-weight)))
        (if (zerop alpha)
            0
            (let ((pixel (ash (min 255 (floor (+ .5d0 (* 255d0 alpha)))) 24)))
              (dolist (shift '(16 8 0))
                (let ((channel (/ (+ (* source-weight (ldb (byte 8 shift) source))
                                     (* dest-weight (ldb (byte 8 shift) destination)))
                                  alpha)))
                  (setf pixel (logior pixel (ash (min 255 (floor (+ .5d0 channel))) shift)))))
              (if (logbitp 31 pixel) (- pixel #x100000000) pixel)))))))

(defvar *graphics-surface-images* (make-hash-table :test #'eq :weakness :key :synchronized t)
  "Java SurfaceData -> the buffered image retained by a software display backend.")

(defvar *graphics-surface-image-refreshers* (make-hash-table :test #'eq :weakness :key :synchronized t)
  "Optional SURFACE -> function keeping a display backing image current.")

(defvar *graphics-physical-gray-images* (make-hash-table :test #'eq :weakness :key :synchronized t)
  "Byte-gray display rasters whose samples denote physical intensities.
Their advertised CS_GRAY model describes image color conversion, while the
Java2D display primitives operate directly on stored gray intensities.")

(defun %graphics-gray-argb (sample)
  "Expand a stored byte-gray intensity without a color-space conversion."
  (%as-int (logior #xff000000 (* #x010101 sample))))

(defun %graphics-packed-int-pixels (image x y width height)
  "Snapshot standard integer RGB/ARGB storage without per-pixel Java calls."
  (let ((type (|getType()I| image)))
    (when (and (member type '(1 2))
               ;; coerceData can change premultiplication without changing
               ;; BufferedImage.getType; those images need color conversion.
               (zerop (|isAlphaPremultiplied()Z| image))
               (<= 0 x) (<= 0 y) (<= 0 width) (<= 0 height)
               (<= (+ x width) (|getWidth()I| image))
               (<= (+ y height) (|getHeight()I| image)))
      (let ((raster (|getRaster()Ljava/awt/image/WritableRaster;| image))
            (class (find-class '|sun/awt/image/IntegerComponentRaster| nil)))
        (when (and class (typep raster class))
          (let* ((storage (java-array-data (|getDataStorage()[I| raster)))
                 (stride (|getScanlineStride()I| raster))
                 (step (|getPixelStride()I| raster))
                 (start (+ (|getDataOffset(I)I| raster 0) (* y stride) (* x step)))
                 (pixels (make-array (* width height) :element-type '(signed-byte 32))))
            (dotimes (row height)
              (dotimes (column width)
                (let ((pixel (aref storage (+ start (* row stride) (* column step)))))
                  (setf (aref pixels (+ (* row width) column))
                        (if (= type 1) (%as-int (logior #xff000000 pixel)) pixel)))))
            pixels))))))

(defun %graphics-image-pixels (image x y width height)
  "Snapshot rendering pixels, retaining physical gray intensities on displays."
  (if (gethash image *graphics-physical-gray-images*)
      (map 'vector #'%graphics-gray-argb
           (java-array-data
            (|getSamples(IIIII[I)[I| (|getRaster()Ljava/awt/image/WritableRaster;| image)
                                    x y width height 0 nil)))
      (or (%graphics-packed-int-pixels image x y width height)
          (java-array-data (|getRGB(IIII[III)[I| image x y width height nil 0 width)))))

(defun %graphics-byte-gray (argb)
  "Java2D byte-gray transfer quantization (ByteGray.h)."
  (floor (+ (* 77 (ldb (byte 8 16) argb))
            (* 150 (ldb (byte 8 8) argb))
            (* 29 (ldb (byte 8 0) argb)) 128) 256))

(defun %graphics-surface-image (surface)
  "Return the Java buffered image backing a software rendering SURFACE."
  (or (and (slot-exists-p surface '|bufImg|) (slot-value surface '|bufImg|))
      (when-let ((image (gethash surface *graphics-surface-images*)))
        (if-let ((refresh (gethash surface *graphics-surface-image-refreshers*)))
          (funcall refresh surface)
          image))
      (%throw '|java/lang/UnsupportedOperationException| "Only Java buffered-image surfaces are supported")))

(defun %graphics-composite-rectangle (image composite clip x y width height source mask offset stride)
  "Blend SOURCE pixels into IMAGE, honoring the Java clip and optional byte MASK."
  (let ((rule (slot-value composite '|rule|))
        (extra-alpha (coerce (slot-value composite '|extraAlpha|) 'double-float))
        (image-width (|getWidth()I| image))
        (image-height (|getHeight()I| image))
        (mask-data (and mask (java-array-data mask)))
        (gray-raster (and (gethash image *graphics-physical-gray-images*)
                          (|getRaster()Ljava/awt/image/WritableRaster;| image))))
    (loop for dy from (max 0 y) below (min image-height (+ y height))
          do (loop for dx from (max 0 x) below (min image-width (+ x width))
                   when (or (null clip) (not (zerop (|contains(II)Z| clip dx dy))))
                     do (let ((coverage (if mask-data
                                            (logand #xff (aref mask-data
                                                               (+ offset (* (- dy y) stride) (- dx x))))
                                            255)))
                          (unless (zerop coverage)
                            (let* ((src (funcall source (- dx x) (- dy y)))
                                   (destination (if gray-raster
                                                    (%graphics-gray-argb (|getSample(III)I| gray-raster dx dy 0))
                                                    (|getRGB(II)I| image dx dy)))
                                   (pixel (%graphics-blend
                                           (if gray-raster
                                               (%as-int (logior (logand src #xff000000)
                                                                 (* #x010101 (%graphics-byte-gray src))))
                                               src)
                                           destination rule extra-alpha coverage)))
                              (if gray-raster
                                  (|setSample(IIII)V| gray-raster dx dy 0 (ldb (byte 8 0) pixel))
                                  (|setRGB(III)V| image dx dy pixel)))))))))

(defun %graphics-blit (source destination composite clip sx sy dx dy width height mask offset stride)
  "Copy and composite Java image surfaces, snapshotting overlapping source pixels."
  (when (and (plusp width) (plusp height))
    (let* ((src (%graphics-surface-image source))
           (dst (%graphics-surface-image destination))
           (pixels (%graphics-image-pixels src sx sy width height)))
      (%graphics-composite-rectangle
       dst composite clip dx dy width height
       (lambda (x y) (aref pixels (+ x (* y width)))) mask offset stride))))

(defmethod |Blit(Lsun/java2d/SurfaceData;Lsun/java2d/SurfaceData;Ljava/awt/Composite;Lsun/java2d/pipe/Region;IIIIII)V|
    (this source destination composite clip sx sy dx dy width height)
  "Composite a rectangular transfer with the Java AlphaComposite rules."
  (declare (ignore this))
  (%graphics-blit source destination composite clip sx sy dx dy width height nil 0 0))

(defmethod |MaskBlit(Lsun/java2d/SurfaceData;Lsun/java2d/SurfaceData;Ljava/awt/Composite;Lsun/java2d/pipe/Region;IIIIII[BII)V|
    (this source destination composite clip sx sy dx dy width height mask offset stride)
  "Composite a masked rectangular transfer through Java raster accessors."
  (declare (ignore this))
  (%graphics-blit source destination composite clip sx sy dx dy width height mask offset stride))

(defmethod |MaskFill(Lsun/java2d/SunGraphics2D;Lsun/java2d/SurfaceData;Ljava/awt/Composite;IIII[BII)V|
    (this graphics destination composite x y width height mask offset stride)
  "Fill a masked rectangle with the current Java graphics color."
  (declare (ignore this))
  (let ((color (|getRGB()I| (|getColor()Ljava/awt/Color;| graphics))))
    (%graphics-composite-rectangle
     (%graphics-surface-image destination) composite (slot-value graphics '|clipRegion|)
     x y width height (lambda (px py) (declare (ignore px py)) color) mask offset stride)))

(defun %graphics-convolve-raster (source destination kernel edge)
  "Convolve snapshotted raster samples, retaining translations and band precision."
  (let* ((width (|getWidth()I| source))
         (height (|getHeight()I| source))
         (bands (|getNumBands()I| source))
         (sx (|getMinX()I| source)) (sy (|getMinY()I| source))
         (dx (|getMinX()I| destination)) (dy (|getMinY()I| destination))
         (kw (|getWidth()I| kernel)) (kh (|getHeight()I| kernel))
         (ox (|getXOrigin()I| kernel)) (oy (|getYOrigin()I| kernel))
         (weights (java-array-data (|getKernelData([F)[F| kernel nil)))
         (samples (java-array-data (|getPixels(IIII[D)[D| source sx sy width height nil)))
         (output (make-java-array :component-class (%get-java-class-by-bin-name "double")
                                  :size (* width height bands)))
         (values (java-array-data output))
         (model (|getSampleModel()Ljava/awt/image/SampleModel;| destination))
         (data-type (|getDataType()I| model))
         (floating (member data-type '(4 5)))
         (signed-short (member data-type '(1 2))))
    (unless (and (= width (|getWidth()I| destination))
                 (= height (|getHeight()I| destination))
                 (= bands (|getNumBands()I| destination)))
      (%throw '|java/awt/image/ImagingOpException| "Incompatible convolution rasters"))
    ;; JDK 25's software imaging backend filters ushort storage using signed
    ;; 16-bit arithmetic, including signed saturation before storing the bits.
    (when signed-short
      (dotimes (i (length samples))
        (when (>= (aref samples i) 32768d0) (decf (aref samples i) 65536d0))))
    (dotimes (y height)
      (dotimes (x width)
        (let ((inside (and (<= (floor kw 2) x) (< x (- width (floor kw 2)))
                           (<= (floor kh 2) y) (< y (- height (floor kh 2))))))
          (dotimes (band bands)
            (let* ((index (+ band (* bands (+ x (* y width)))))
                   (sample (cond
                             (inside
                              (loop for ky below kh sum
                                    (loop for kx below kw
                                          for weight = (aref weights (+ kx (* ky kw)))
                                          sum (* weight
                                                 (aref samples (+ band (* bands (+ (- (+ x ox) kx)
                                                                                  (* (- (+ y oy) ky) width)))))))))
                             ((= edge 1) (aref samples index))
                             (t 0d0))))
              (setf (aref values index)
                    (coerce (cond
                              (floating sample)
                              (signed-short (logand #xffff (max -32768 (min 32767 (floor sample)))))
                              (t (max 0 (min (1- (ash 1 (|getSampleSize(I)I| model band))) (floor sample)))))
                            'double-float)))))))
    (|setPixels(IIII[D)V| destination dx dy width height output)
    destination))

(defun %graphics-image-copy (source destination)
  "Copy a buffered image with Java color conversion and replacement compositing."
  (let ((graphics (|createGraphics()Ljava/awt/Graphics2D;| destination)))
    (unwind-protect
         (progn
           (|setComposite(Ljava/awt/Composite;)V| graphics
            (%graphics-static "java/awt/AlphaComposite" '|Src|))
           (|drawImage(Ljava/awt/Image;IILjava/awt/image/ImageObserver;)Z| graphics source 0 0 nil))
      (|dispose()V| graphics)))
  destination)

(defun %graphics-convolve-image (operation source destination)
  "Filter stored image channels like the JDK software imaging backend."
  (unless destination
    (setf destination (|createCompatibleDestImage(Ljava/awt/image/BufferedImage;Ljava/awt/image/ColorModel;)Ljava/awt/image/BufferedImage;|
                       operation source nil)))
  (let ((model (|getColorModel()Ljava/awt/image/ColorModel;| source)))
    (when (eq model (|getColorModel()Ljava/awt/image/ColorModel;| destination))
      (%graphics-convolve-raster (|getRaster()Ljava/awt/image/WritableRaster;| source)
                                 (|getRaster()Ljava/awt/image/WritableRaster;| destination)
                                 (|getKernel()Ljava/awt/image/Kernel;| operation)
                                 (|getEdgeCondition()I| operation))
      (return-from %graphics-convolve-image destination)))
  (let ((output (|createCompatibleDestImage(Ljava/awt/image/BufferedImage;Ljava/awt/image/ColorModel;)Ljava/awt/image/BufferedImage;|
                  operation source nil)))
    (%graphics-convolve-raster (|getRaster()Ljava/awt/image/WritableRaster;| source)
                               (|getRaster()Ljava/awt/image/WritableRaster;| output)
                               (|getKernel()Ljava/awt/image/Kernel;| operation)
                               (|getEdgeCondition()I| operation))
    (%graphics-image-copy output destination)))

;; Supply the acceleration hook through CL rather than the JDK's mlib_image.
;; Other image operators keep their ordinary Java fallback when available.
(setf (gethash "sun/awt/image/ImagingLib.filter(Ljava/awt/image/BufferedImageOp;Ljava/awt/image/BufferedImage;Ljava/awt/image/BufferedImage;)Ljava/awt/image/BufferedImage;"
               *native-overrides*) ; lint:suppress max-line-length
      (lambda (operation source destination)
        (when (and (find-class '|java/awt/image/ConvolveOp| nil) (typep operation '|java/awt/image/ConvolveOp|))
          (%graphics-convolve-image operation source destination))))

(setf (gethash "sun/awt/image/ImagingLib.filter(Ljava/awt/image/RasterOp;Ljava/awt/image/Raster;Ljava/awt/image/WritableRaster;)Ljava/awt/image/WritableRaster;"
               *native-overrides*) ; lint:suppress max-line-length
      (lambda (operation source destination)
        (when (and (find-class '|java/awt/image/ConvolveOp| nil) (typep operation '|java/awt/image/ConvolveOp|))
          (%graphics-convolve-raster source
                                     (or destination (|createCompatibleDestRaster(Ljava/awt/image/Raster;)Ljava/awt/image/WritableRaster;|
                                                      operation source))
                                     (|getKernel()Ljava/awt/image/Kernel;| operation)
                                     (|getEdgeCondition()I| operation)))))
