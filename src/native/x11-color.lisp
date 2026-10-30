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

(sb-alien:define-alien-type openldk-xcolor
  (sb-alien:struct openldk-xcolor
    (pixel sb-alien:unsigned-long)
    (red sb-alien:unsigned-short) (green sb-alien:unsigned-short) (blue sb-alien:unsigned-short)
    (flags sb-alien:unsigned-char) (pad sb-alien:unsigned-char)))

(defun %x11-allocate-palette-color (colormap red green blue)
  "Allocate a read-only color cell, returning NIL when the colormap is full."
  (sb-alien:with-alien ((color openldk-xcolor))
    (setf (sb-alien:slot color 'red) (* red 257)
          (sb-alien:slot color 'green) (* green 257)
          (sb-alien:slot color 'blue) (* blue 257)
          (sb-alien:slot color 'flags) 7)
    (unless (zerop (%libc "XAllocColor" sb-alien:int ((* t) (%x11-display))
                    (sb-alien:unsigned-long colormap) ((* openldk-xcolor) (sb-alien:addr color))))
      (sb-alien:slot color 'pixel))))

(defun %x11-color-count (visual)
  "Pixel-range size for byte TrueColor, or the indexed colormap's entry count."
  (if (and (= (getf visual :class) 4) (<= (getf visual :depth) 8))
      (ash 1 (getf visual :depth))
      (getf visual :entries)))

(defun %x11-color-palette (visual screen)
  "Retain server colors and ownership bits for an indexed visual's colormap."
  (or (getf visual :palette)
      (bt:with-recursive-lock-held (*x11-lock*)
        (or (getf visual :palette)
            (let* ((count (%x11-color-count visual))
                   (colormap (%x11-colormap visual screen))
                   (dynamic (member (getf visual :class) '(1 3)))
                   (valid (if dynamic 0 (1- (ash 1 count))))
                   (colors (sb-alien:make-alien openldk-xcolor count)))
              (unwind-protect
                   (progn
                     (when dynamic
                       (flet ((allocate (red green blue)
                                (when-let ((pixel (%x11-allocate-palette-color colormap red green blue)))
                                  (when (< pixel count) (setf valid (logior valid (ash 1 pixel)))))))
                         ;; A read-only cube leaves room for grayscale and for
                         ;; cells already owned by other clients of the default map.
                         (when (= (getf visual :class) 3)
                           (dotimes (red 6)
                             (dotimes (green 6)
                               (dotimes (blue 6) (allocate (* red 51) (* green 51) (* blue 51))))))
                         (dotimes (i (if (= (getf visual :class) 1) 256 40))
                           (let ((gray (round (* i 255) (if (= (getf visual :class) 1) 255 39))))
                             (allocate gray gray gray)))))
                     (when (zerop valid) (%throw '|java/awt/AWTError| "Cannot allocate X11 palette colors"))
                     (dotimes (i count) (setf (sb-alien:slot (sb-alien:deref colors i) 'pixel) i))
                     (%libc "XQueryColors" sb-alien:int ((* t) (%x11-display))
                       (sb-alien:unsigned-long colormap) ((* openldk-xcolor) colors) (sb-alien:int count))
                     (let ((palette (make-array count :element-type '(unsigned-byte 32))))
                       (dotimes (i count)
                         (let ((color (sb-alien:deref colors i)))
                           (setf (aref palette i)
                                 (logior (ash (round (sb-alien:slot color 'red) 257) 16)
                                         (ash (round (sb-alien:slot color 'green) 257) 8)
                                         (round (sb-alien:slot color 'blue) 257)))))
                       (setf (getf visual :palette-valid) valid
                             (getf visual :palette-cache) (make-hash-table :synchronized t)
                             (getf visual :palette) palette)))
                (sb-alien:free-alien colors)))))))

(defun %x11-pixel-channel (value mask)
  "Quantize an eight-bit component into a contiguous X11 visual MASK."
  (if (zerop mask) 0
      (let ((shift (1- (integer-length (logand mask (- mask))))))
        (ash (round (* value (ash mask (- shift))) 255) shift))))

(defun %x11-pixel-component (pixel mask)
  "Expand a server pixel component to an eight-bit Java RGB channel."
  (if (zerop mask) 0
      (let ((shift (1- (integer-length (logand mask (- mask))))))
        (round (* 255 (ash (logand pixel mask) (- shift))) (ash mask (- shift))))))

(defun %x11-rgb-to-pixel (rgb visual screen)
  "Encode RGB with the visual's masks or an owned cell in its actual palette."
  (if (= (getf visual :class) 4)
      (logior (%x11-pixel-channel (ldb (byte 8 16) rgb) (getf visual :red))
              (%x11-pixel-channel (ldb (byte 8 8) rgb) (getf visual :green))
              (%x11-pixel-channel (ldb (byte 8 0) rgb) (getf visual :blue)))
      (let* ((palette (%x11-color-palette visual screen))
             (cache (getf visual :palette-cache))
             (rgb (ldb (byte 24 0) rgb)))
        (or (gethash rgb cache)
            (let ((best 0) (distance most-positive-fixnum) (valid (getf visual :palette-valid)))
              (dotimes (i (length palette))
                (when (logbitp i valid)
                  (let* ((color (aref palette i))
                         (red (- (ldb (byte 8 16) rgb) (ldb (byte 8 16) color)))
                         (green (- (ldb (byte 8 8) rgb) (ldb (byte 8 8) color)))
                         (blue (- (ldb (byte 8 0) rgb) (ldb (byte 8 0) color)))
                         (error (+ (* red red) (* green green) (* blue blue))))
                    (when (< error distance) (setf distance error best i)))))
              ;; Bound per-display memoization for applications drawing many
              ;; distinct image colors; palette cells themselves remain owned.
              (when (>= (hash-table-count cache) 4096) (clrhash cache))
              (setf (gethash rgb cache) best))))))

(defun %x11-pixel-to-rgb (pixel visual screen)
  "Decode a physical pixel using the server's channel masks or colormap."
  (if (= (getf visual :class) 4)
      (logior (ash (%x11-pixel-component pixel (getf visual :red)) 16)
              (ash (%x11-pixel-component pixel (getf visual :green)) 8)
              (%x11-pixel-component pixel (getf visual :blue)))
      (let ((palette (%x11-color-palette visual screen)))
        (if (< pixel (length palette)) (aref palette pixel) 0))))

(defun %x11-indexed-color-model (visual screen)
  "Describe actual server entries, excluding unowned dynamic color cells."
  (let* ((palette (%x11-color-palette visual screen))
         (valid (getf visual :palette-valid))
         (count (length palette))
         (colors (make-java-array :component-class (%get-java-class-by-bin-name "int")
                   :initial-contents (map 'vector (lambda (rgb) (%as-int (logior #xff000000 rgb))) palette)))
         (bytes (make-java-array :component-class (%get-java-class-by-bin-name "byte")
                  :initial-contents
                  (loop for i downfrom (1- (ceiling count 8)) to 0
                        for value = (ldb (byte 8 (* i 8)) valid)
                        collect (if (> value 127) (- value 256) value)))))
    (classload "java/math/BigInteger")
    (classload "java/awt/image/IndexColorModel")
    (let ((bits (%make-java-instance "java/math/BigInteger"))
          (model (%make-java-instance "java/awt/image/IndexColorModel")))
      (%x11-java-instance bits "<init>(I[B)V" 1 bytes)
      (%x11-java-instance model "<init>(II[IIILjava/math/BigInteger;)V"
                         (getf visual :depth) count colors 0 (if (> (getf visual :depth) 8) 1 0) bits)
      model)))

(%define-x11-native "sun/awt/X11GraphicsConfig.makeColorModel()Ljava/awt/image/ColorModel;" (this)
  (multiple-value-bind (visual screen) (%x11-configuration-visual this)
    (cond
      ;; The JDK exposes byte TrueColor through a complete pixel/RGB lookup,
      ;; not DirectColorModel, including XIconWindow's byte-raster consumers.
      ((and (= (getf visual :class) 4) (<= (getf visual :depth) 8))
       (%x11-indexed-color-model visual screen))
      ((= (getf visual :class) 4)
          (classload "java/awt/image/DirectColorModel")
          (let ((model (%make-java-instance "java/awt/image/DirectColorModel")))
            (%x11-java-instance model "<init>(IIIII)V" (getf visual :depth)
                               (%as-int (getf visual :red)) (%as-int (getf visual :green))
                               (%as-int (getf visual :blue)) 0)
            model))
      ((and (= (getf visual :class) 0) (= (%x11-color-count visual) 256))
       ;; A full StaticGray ramp is a byte component surface, as in the
       ;; JDK's X11 color model. Dynamic GrayScale remains indexed.
       (let ((space (%x11-java-static "java/awt/color/ColorSpace"
                      "getInstance(I)Ljava/awt/color/ColorSpace;" 1003))
             (bits (make-java-array :component-class (%get-java-class-by-bin-name "int")
                     :initial-contents '(8))))
         (classload "java/awt/image/ComponentColorModel")
         (let ((model (%make-java-instance "java/awt/image/ComponentColorModel")))
           (%x11-java-instance model "<init>(Ljava/awt/color/ColorSpace;[IZZII)V"
                              space bits 0 0 1 0)
           model)))
      ((member (getf visual :class) '(0 1 2 3)) (%x11-indexed-color-model visual screen))
      (t (%throw '|java/awt/AWTError| "Unsupported X11 visual class")))))
