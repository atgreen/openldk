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

;;; Capture uses actual window visuals and colormaps. XGetImage(root) alone
;;; cannot identify the color space of pixels belonging to inferiors.

(sb-alien:define-alien-type openldk-capture-visual
  (sb-alien:struct openldk-capture-visual
    (extensions (* t)) (id sb-alien:unsigned-long) (kind sb-alien:int)
    (red sb-alien:unsigned-long) (green sb-alien:unsigned-long) (blue sb-alien:unsigned-long)
    (bits sb-alien:int) (entries sb-alien:int)))

(sb-alien:define-alien-type openldk-capture-attributes
  (sb-alien:struct openldk-capture-attributes
    (x sb-alien:int) (y sb-alien:int) (width sb-alien:int) (height sb-alien:int)
    (border sb-alien:int) (depth sb-alien:int) (visual (* openldk-capture-visual))
    (root sb-alien:unsigned-long) (kind sb-alien:int)
    (bit-gravity sb-alien:int) (win-gravity sb-alien:int) (backing-store sb-alien:int)
    (backing-planes sb-alien:unsigned-long) (backing-pixel sb-alien:unsigned-long)
    (save-under sb-alien:int) (colormap sb-alien:unsigned-long)
    (map-installed sb-alien:int) (map-state sb-alien:int)
    (all-events sb-alien:long) (events sb-alien:long) (no-propagation sb-alien:long)
    (override-redirect sb-alien:int) (screen (* t))))

(sb-alien:define-alien-type openldk-capture-rectangle
  (sb-alien:struct openldk-capture-rectangle
    (x sb-alien:short) (y sb-alien:short)
    (width sb-alien:unsigned-short) (height sb-alien:unsigned-short)))

(defvar *x11-shape-loaded* nil)

(defun %x11-capture-shapes-p ()
  "Whether this server supports shaped windows, through ordinary libXext."
  (unless *x11-shape-loaded*
    (sb-alien:load-shared-object "libXext.so.6")
    (setf *x11-shape-loaded* t))
  (sb-alien:with-alien ((event sb-alien:int) (error sb-alien:int))
    (not (zerop (%libc "XShapeQueryExtension" sb-alien:int ((* t) (%x11-display))
                  ((* sb-alien:int) (sb-alien:addr event)) ((* sb-alien:int) (sb-alien:addr error)))))))

(defun %x11-capture-intersection (first second)
  (destructuring-bind (left top right bottom) first
    (destructuring-bind (x y end-x end-y) second
      (let ((l (max left x)) (t0 (max top y)) (r (min right end-x)) (b (min bottom end-y)))
        (when (and (< l r) (< t0 b)) (list l t0 r b))))))

(defun %x11-capture-clip (region clip)
  "Intersect disjoint rectangular regions in screen coordinates."
  (loop for rectangle in region append
        (loop for boundary in clip
              for intersection = (%x11-capture-intersection rectangle boundary)
              when intersection collect intersection)))

(defun %x11-capture-shape-flags (window)
  "Distinguish explicit shapes from the server's default rectangle replies."
  (sb-alien:with-alien ((bounding sb-alien:int) (clip sb-alien:int)
                       (bx sb-alien:int) (by sb-alien:int) (bw sb-alien:unsigned-int) (bh sb-alien:unsigned-int)
                       (cx sb-alien:int) (cy sb-alien:int) (cw sb-alien:unsigned-int) (ch sb-alien:unsigned-int))
    (when (zerop (%libc "XShapeQueryExtents" sb-alien:int ((* t) (%x11-display))
                   (sb-alien:unsigned-long window)
                   ((* sb-alien:int) (sb-alien:addr bounding))
                   ((* sb-alien:int) (sb-alien:addr bx)) ((* sb-alien:int) (sb-alien:addr by))
                   ((* sb-alien:unsigned-int) (sb-alien:addr bw)) ((* sb-alien:unsigned-int) (sb-alien:addr bh))
                   ((* sb-alien:int) (sb-alien:addr clip))
                   ((* sb-alien:int) (sb-alien:addr cx)) ((* sb-alien:int) (sb-alien:addr cy))
                   ((* sb-alien:unsigned-int) (sb-alien:addr cw)) ((* sb-alien:unsigned-int) (sb-alien:addr ch))))
      (%throw '|java/awt/AWTError| "Cannot inspect capture window shape"))
    (logior (if (zerop bounding) 0 1) (if (zerop clip) 0 2))))

(defun %x11-capture-shape (window kind x y fallback flags)
  "Return the window's bounding/child-clip shape in screen coordinates."
  (unless (logbitp kind flags) (return-from %x11-capture-shape (list fallback)))
  (sb-alien:with-alien ((count sb-alien:int) (ordering sb-alien:int))
    (setf count 0 ordering 0)
    (let ((rectangles (%libc "XShapeGetRectangles" (* openldk-capture-rectangle)
                        ((* t) (%x11-display)) (sb-alien:unsigned-long window) (sb-alien:int kind)
                        ((* sb-alien:int) (sb-alien:addr count)) ((* sb-alien:int) (sb-alien:addr ordering)))))
      (unwind-protect
           (loop for i below count
                 for rectangle = (sb-alien:deref rectangles i)
                 for left = (+ x (sb-alien:slot rectangle 'x))
                 for top = (+ y (sb-alien:slot rectangle 'y))
                 collect (list left top (+ left (sb-alien:slot rectangle 'width))
                               (+ top (sb-alien:slot rectangle 'height))))
        (unless (sb-alien:null-alien rectangles)
          (%libc "XFree" sb-alien:int ((* t) (sb-alien:cast rectangles (* t)))))))))

(defun %x11-capture-children (window)
  "Copy the server's children in bottom-to-top stacking order."
  (sb-alien:with-alien ((root sb-alien:unsigned-long) (parent sb-alien:unsigned-long)
                       (children (* sb-alien:unsigned-long)) (count sb-alien:unsigned-int))
    (setf children (sb-alien:sap-alien (sb-sys:int-sap 0) (* sb-alien:unsigned-long)))
    (unwind-protect
         (unless (zerop (%libc "XQueryTree" sb-alien:int ((* t) (%x11-display))
                          (sb-alien:unsigned-long window)
                          ((* sb-alien:unsigned-long) (sb-alien:addr root))
                          ((* sb-alien:unsigned-long) (sb-alien:addr parent))
                          ((* (* sb-alien:unsigned-long)) (sb-alien:addr children))
                          ((* sb-alien:unsigned-int) (sb-alien:addr count))))
           (loop for i below count collect (sb-alien:deref children i)))
      (unless (sb-alien:null-alien children)
        (%libc "XFree" sb-alien:int ((* t) (sb-alien:cast children (* t))))))))

(defun %x11-capture-colors (colormap pixels)
  "Query colors in bounded requests, without allocating or modifying cells."
  (let ((result (make-array (length pixels) :element-type '(unsigned-byte 32)))
        (colors (sb-alien:make-alien openldk-xcolor 256)))
    (unwind-protect
         (loop for start from 0 below (length pixels) by 256
               for count = (min 256 (- (length pixels) start))
               do (dotimes (i count)
                    (setf (sb-alien:slot (sb-alien:deref colors i) 'pixel) (aref pixels (+ start i))))
                  (%libc "XQueryColors" sb-alien:int ((* t) (%x11-display))
                    (sb-alien:unsigned-long colormap) ((* openldk-xcolor) colors) (sb-alien:int count))
                  (dotimes (i count)
                    (let ((color (sb-alien:deref colors i)))
                      (setf (aref result (+ start i))
                            (logior (ash (ash (sb-alien:slot color 'red) -8) 16)
                                    (ash (ash (sb-alien:slot color 'green) -8) 8)
                                    (ash (sb-alien:slot color 'blue) -8))))))
      (sb-alien:free-alien colors))
    result))

(defun %x11-capture-color-map (visual colormap cache)
  "Snapshot indexed colors or DirectColor channel ramps while the server is held."
  (let* ((kind (getf visual :class)) (key (list colormap (getf visual :visual))))
    (or (gethash key cache)
        (setf (gethash key cache)
              (cond
                ((= kind 4) :true-color)
                ((= kind 5)
                 (loop for mask in (list (getf visual :red) (getf visual :green) (getf visual :blue))
                       for shift = (1- (integer-length (logand mask (- mask))))
                       collect (%x11-capture-colors colormap
                                 (coerce (loop for i below (1+ (ash mask (- shift))) collect (ash i shift)) 'vector))))
                (t (%x11-capture-colors colormap
                     (coerce (loop for i below (getf visual :entries) collect i) 'vector))))))))

(defun %x11-capture-rgb (pixel visual colors)
  (case (getf visual :class)
    (4 (%x11-pixel-to-rgb pixel visual 0))
    (5 (loop for mask in (list (getf visual :red) (getf visual :green) (getf visual :blue))
             for channel-mask in '(#xff0000 #xff00 #xff)
             for ramp in colors
             for shift = (1- (integer-length (logand mask (- mask))))
             sum (logand channel-mask (aref ramp (ash (logand pixel mask) (- shift))))))
    (otherwise (if (< pixel (length colors)) (aref colors pixel) 0))))

(defstruct x11-capture-tile image left top regions visual colors)

(defun %x11-capture-tiles (root region shapes-p tiles cache)
  "Fetch layers in stacking order; TILES is an ownership list, including on error."
  (labels ((walk (window parent-x parent-y clip)
             (sb-alien:with-alien ((attributes openldk-capture-attributes))
               (when (zerop (%libc "XGetWindowAttributes" sb-alien:int ((* t) (%x11-display))
                              (sb-alien:unsigned-long window)
                              ((* openldk-capture-attributes) (sb-alien:addr attributes))))
                 (%throw '|java/awt/AWTError| "Cannot inspect capture window"))
               ;; InputOnly ancestors cannot contain InputOutput descendants.
               (unless (and (= 2 (sb-alien:slot attributes 'map-state))
                            (= 1 (sb-alien:slot attributes 'kind)))
                 (return-from walk nil))
               (let* ((border (sb-alien:slot attributes 'border))
                      (x (+ parent-x (sb-alien:slot attributes 'x) border))
                      (y (+ parent-y (sb-alien:slot attributes 'y) border))
                      (right (+ x (sb-alien:slot attributes 'width)))
                      (bottom (+ y (sb-alien:slot attributes 'height)))
                      (bounds (list (- x border) (- y border) (+ right border) (+ bottom border)))
                      (flags (if shapes-p (%x11-capture-shape-flags window) 0))
                      (visible (%x11-capture-clip (%x11-capture-shape window 0 x y bounds flags) clip)))
                 (when visible
                   (when (= 1 (sb-alien:slot attributes 'kind))
                     (let* ((native (sb-alien:deref (sb-alien:slot attributes 'visual)))
                            (visual (list :class (sb-alien:slot native 'kind) :visual (sb-alien:slot native 'id)
                                          :red (sb-alien:slot native 'red) :green (sb-alien:slot native 'green)
                                          :blue (sb-alien:slot native 'blue) :entries (sb-alien:slot native 'entries)))
                            (left (reduce #'min visible :key #'first))
                            (top (reduce #'min visible :key #'second))
                            (end-x (reduce #'max visible :key #'third))
                            (end-y (reduce #'max visible :key #'fourth))
                            (image (%libc "XGetImage" (* openldk-ximage-prefix) ((* t) (%x11-display))
                                     (sb-alien:unsigned-long window) (sb-alien:int (- left x)) (sb-alien:int (- top y))
                                     (sb-alien:unsigned-int (- end-x left)) (sb-alien:unsigned-int (- end-y top))
                                     (sb-alien:unsigned-long #xffffffffffffffff) (sb-alien:int 2))))
                       (when (sb-alien:null-alien image) (%throw '|java/awt/AWTError| "Cannot capture X window"))
                       (let ((published nil))
                         (unwind-protect
                              (let ((tile (make-x11-capture-tile :image image :left left :top top
                                                               :regions visible :visual visual)))
                                (push tile (car tiles))
                                (setf published t)
                                (setf (x11-capture-tile-colors tile)
                                      (%x11-capture-color-map visual (sb-alien:slot attributes 'colormap) cache)))
                           (unless published
                             (%libc "XDestroyImage" sb-alien:int ((* openldk-ximage-prefix) image)))))))
                   (let ((child-clip (%x11-capture-clip
                                      (%x11-capture-shape window 1 x y (list x y right bottom) flags)
                                      visible)))
                     (when child-clip
                       (dolist (child (%x11-capture-children window)) (walk child x y child-clip)))))))))
    (walk root 0 0 (list region))))

(defun %x11-capture-screen (screen x y width height data)
  "Fetch a stable window-tree snapshot, then convert it without holding the server."
  (let* ((display (%x11-display))
         (root (%libc "XRootWindow" sb-alien:unsigned-long ((* t) display) (sb-alien:int screen)))
         (left (max 0 x)) (top (max 0 y))
         (right (min (+ x width) (%libc "XDisplayWidth" sb-alien:int ((* t) display) (sb-alien:int screen))))
         (bottom (min (+ y height) (%libc "XDisplayHeight" sb-alien:int ((* t) display) (sb-alien:int screen))))
         (shapes-p (%x11-capture-shapes-p)) (tiles (list nil)))
    (when (and (< left right) (< top bottom))
      (unwind-protect
           (progn
             (%libc "XGrabServer" sb-alien:int ((* t) display))
             (unwind-protect
                  (%x11-capture-tiles root (list left top right bottom) shapes-p tiles (make-hash-table :test #'equal))
               (%libc "XUngrabServer" sb-alien:int ((* t) display))
               (%libc "XFlush" sb-alien:int ((* t) display)))
             (dolist (tile (reverse (car tiles)))
               (let ((image (x11-capture-tile-image tile)))
                 (dolist (rectangle (x11-capture-tile-regions tile))
                   (destructuring-bind (l t0 r b) rectangle
                     (loop for row from t0 below b do
                       (loop for column from l below r
                             for pixel = (%libc "XGetPixel" sb-alien:unsigned-long ((* openldk-ximage-prefix) image)
                                          (sb-alien:int (- column (x11-capture-tile-left tile)))
                                          (sb-alien:int (- row (x11-capture-tile-top tile))))
                             for rgb = (%x11-capture-rgb pixel (x11-capture-tile-visual tile) (x11-capture-tile-colors tile))
                             do (setf (aref data (+ (- column x) (* (- row y) width)))
                                      (%as-int (logior #xff000000 rgb))))))))))
        (dolist (tile (car tiles))
          (%libc "XDestroyImage" sb-alien:int ((* openldk-ximage-prefix) (x11-capture-tile-image tile))))))))

(%define-x11-awt-native "sun/awt/X11/XRobotPeer.getRGBPixelsImpl(Lsun/awt/X11GraphicsConfig;IIII[IZ)V"
    (configuration x y width height array gtk)
  (declare (ignore gtk))
  (when (and (plusp width) (plusp height))
    (unless (>= (length (java-array-data array)) (* width height))
      (%throw '|java/lang/ArrayIndexOutOfBoundsException| "Screen capture buffer is too small"))
    (multiple-value-bind (visual screen) (%x11-configuration-visual configuration)
      (declare (ignore visual))
      (bt:with-recursive-lock-held (*x11-lock*)
        (%x11-capture-screen screen x y width height (java-array-data array))))) nil)
