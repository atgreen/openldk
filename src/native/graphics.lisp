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
 "java/awt/Font.initIDs()V"
 "sun/java2d/SurfaceData.initIDs()V"
 "sun/java2d/pipe/Region.initIDs()V"
 "sun/java2d/pipe/ShapeSpanIterator.initIDs()V"
 "sun/java2d/pipe/SpanClipRenderer.initIDs(Ljava/lang/Class;Ljava/lang/Class;)V"
 "sun/awt/image/BufImgSurfaceData.initIDs(Ljava/lang/Class;Ljava/lang/Class;)V"
 "sun/java2d/loops/GraphicsPrimitiveMgr.initIDs(Ljava/lang/Class;Ljava/lang/Class;Ljava/lang/Class;Ljava/lang/Class;Ljava/lang/Class;Ljava/lang/Class;Ljava/lang/Class;Ljava/lang/Class;Ljava/lang/Class;Ljava/lang/Class;Ljava/lang/Class;)V" ; lint:suppress max-line-length
 "sun/java2d/Disposer.initIDs()V")

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

(defun %graphics-surface-image (surface)
  "Return the Java buffered image backing a software rendering SURFACE."
  (or (and (slot-exists-p surface '|bufImg|) (slot-value surface '|bufImg|))
      (%throw '|java/lang/UnsupportedOperationException| "Only Java buffered-image surfaces are supported")))

(defun %graphics-composite-rectangle (image composite clip x y width height source mask offset stride)
  "Blend SOURCE pixels into IMAGE, honoring the Java clip and optional byte MASK."
  (let ((rule (slot-value composite '|rule|))
        (extra-alpha (coerce (slot-value composite '|extraAlpha|) 'double-float))
        (image-width (|getWidth()I| image))
        (image-height (|getHeight()I| image))
        (mask-data (and mask (java-array-data mask))))
    (loop for dy from (max 0 y) below (min image-height (+ y height))
          do (loop for dx from (max 0 x) below (min image-width (+ x width))
                   when (or (null clip) (not (zerop (|contains(II)Z| clip dx dy))))
                     do (let ((coverage (if mask-data
                                            (logand #xff (aref mask-data
                                                               (+ offset (* (- dy y) stride) (- dx x))))
                                            255)))
                          (unless (zerop coverage)
                            (|setRGB(III)V| image dx dy
                             (%graphics-blend (funcall source (- dx x) (- dy y))
                                              (|getRGB(II)I| image dx dy)
                                              rule extra-alpha coverage))))))))

(defun %graphics-blit (source destination composite clip sx sy dx dy width height mask offset stride)
  "Copy and composite Java image surfaces, snapshotting overlapping source pixels."
  (when (and (plusp width) (plusp height))
    (let* ((src (%graphics-surface-image source))
           (dst (%graphics-surface-image destination))
           (pixels (java-array-data (|getRGB(IIII[III)[I| src sx sy width height nil 0 width))))
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
