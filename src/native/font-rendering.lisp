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

;;; Logical scaler/context handles survive saved images; all Cairo objects
;;; are scoped to an operation and destroyed before returning to Java.
(defstruct font-scaler-state file index)
(defstruct font-context-state scaler matrix aa fm bold italic)
(defvar *font-scalers* (make-hash-table))
(defvar *font-contexts* (make-hash-table))
(defvar *font-next-handle* 100)
(defvar *font-cairo-loaded* nil)

(sb-alien:define-alien-type font-cairo-matrix
    (sb-alien:struct font-cairo-matrix
      (xx sb-alien:double) (yx sb-alien:double) (xy sb-alien:double)
      (yy sb-alien:double) (x0 sb-alien:double) (y0 sb-alien:double)))
(sb-alien:define-alien-type font-cairo-glyph
    (sb-alien:struct font-cairo-glyph
      (index sb-alien:unsigned-long) (x sb-alien:double) (y sb-alien:double)))
(sb-alien:define-alien-type font-cairo-extents
    (sb-alien:struct font-cairo-extents
      (x sb-alien:double) (y sb-alien:double)
      (width sb-alien:double) (height sb-alien:double)
      (advance-x sb-alien:double) (advance-y sb-alien:double)))
(sb-alien:define-alien-type font-cairo-path
    (sb-alien:struct font-cairo-path
      (status sb-alien:int) (data (* t)) (count sb-alien:int)))

(defun %font-render-error (message)
  "Signal a scaler failure through the JDK's normal recovery boundary."
  (%throw '|sun/font/FontScalerException| message))

(defun %load-font-cairo ()
  "Load Cairo's system FreeType backend, without JDK native libraries."
  (%load-fontconfig)
  (unless *font-cairo-loaded*
    (sb-alien:load-shared-object "libcairo.so.2")
    (setf *font-cairo-loaded* t)))

(defun %font-scaler (handle)
  "Resolve an active logical scaler HANDLE."
  (or (gethash handle *font-scalers*) (%font-render-error "Disposed font scaler")))

(defun %font-with-cairo (scaler-handle context-handle operation)
  "Call OPERATION with a temporary Cairo context configured for this strike."
  (bordeaux-threads:with-recursive-lock-held (*fontconfig-lock*)
    (%load-font-cairo)
    (let* ((scaler (%font-scaler scaler-handle))
           (context (gethash context-handle *font-contexts*))
           (matrix (and context (font-context-state-matrix context)))
           (pattern nil)
           (face nil) (surface nil) (cr nil) (options nil))
      (unless (and context (= scaler-handle (font-context-state-scaler context)))
        (%font-render-error "Disposed or mismatched font context"))
      (unwind-protect
           (progn
             (setf pattern (%font-native-call "FcPatternCreate" (* t)))
             (when (sb-alien:null-alien pattern) (%font-render-error "Cannot allocate font pattern"))
             (%font-native-call "FcPatternAddString" sb-alien:int
               ((* t) pattern) (sb-alien:c-string "file")
               (sb-alien:c-string (font-scaler-state-file scaler)))
             (%font-native-call "FcPatternAddInteger" sb-alien:int
               ((* t) pattern) (sb-alien:c-string "index") (sb-alien:int (font-scaler-state-index scaler)))
             (when (> (font-context-state-bold context) 1)
               (%font-native-call "FcPatternAddBool" sb-alien:int
                 ((* t) pattern) (sb-alien:c-string "embolden") (sb-alien:int 1)))
             (setf face (%font-native-call "cairo_ft_font_face_create_for_pattern" (* t) ((* t) pattern))
                   surface (%font-native-call "cairo_recording_surface_create" (* t)
                             (sb-alien:int #x3000) ((* t) nil))
                   cr (%font-native-call "cairo_create" (* t) ((* t) surface))
                   options (%font-native-call "cairo_font_options_create" (* t)))
             (%font-native-call "cairo_set_font_face" sb-alien:void ((* t) cr) ((* t) face))
             (%font-native-call "cairo_font_options_set_antialias" sb-alien:void
               ((* t) options) (sb-alien:int (if (= (font-context-state-aa context) 1) 1 2)))
             (%font-native-call "cairo_font_options_set_hint_metrics" sb-alien:void
               ((* t) options) (sb-alien:int (if (= (font-context-state-fm context) 2) 1 2)))
             (%font-native-call "cairo_font_options_set_hint_style" sb-alien:void
               ((* t) options) (sb-alien:int 1))
             (%font-native-call "cairo_set_font_options" sb-alien:void ((* t) cr) ((* t) options))
             (sb-alien:with-alien ((transform font-cairo-matrix))
               (setf (sb-alien:slot transform 'xx) (aref matrix 0)
                     (sb-alien:slot transform 'yx) (aref matrix 1)
                     (sb-alien:slot transform 'xy)
                     (- (aref matrix 2) (* (if (zerop (font-context-state-italic context)) 0d0 (/ #x366a 65536d0))
                                           (aref matrix 0)))
                     (sb-alien:slot transform 'yy)
                     (- (aref matrix 3) (* (if (zerop (font-context-state-italic context)) 0d0 (/ #x366a 65536d0))
                                           (aref matrix 1)))
                     (sb-alien:slot transform 'x0) 0d0 (sb-alien:slot transform 'y0) 0d0)
               (%font-native-call "cairo_set_font_matrix" sb-alien:void
                 ((* t) cr) ((* font-cairo-matrix) (sb-alien:addr transform))))
             (unless (zerop (%font-native-call "cairo_status" sb-alien:int ((* t) cr)))
               (%font-render-error "Cannot create Cairo font strike"))
             (funcall operation cr))
        (when options (%font-native-call "cairo_font_options_destroy" sb-alien:void ((* t) options)))
        (when cr (%font-native-call "cairo_destroy" sb-alien:void ((* t) cr)))
        (when surface (%font-native-call "cairo_surface_destroy" sb-alien:void ((* t) surface)))
        (when face (%font-native-call "cairo_font_face_destroy" sb-alien:void ((* t) face)))
        (when pattern (%font-native-call "FcPatternDestroy" sb-alien:void ((* t) pattern)))))))

(defun %font-glyph-extents (cr glyph-code)
  "Return a glyph's bearings, dimensions, and advance in Java device coordinates."
  (sb-alien:with-alien ((glyph font-cairo-glyph) (extents font-cairo-extents))
    (setf (sb-alien:slot glyph 'index) glyph-code
          (sb-alien:slot glyph 'x) 0d0 (sb-alien:slot glyph 'y) 0d0)
    (%font-native-call "cairo_glyph_extents" sb-alien:void
      ((* t) cr) ((* font-cairo-glyph) (sb-alien:addr glyph))
      (sb-alien:int 1) ((* font-cairo-extents) (sb-alien:addr extents)))
    (loop for field in '(x y width height advance-x advance-y)
          collect (sb-alien:slot extents field))))

(defun %font-path-commands (cr glyph-code x y)
  "Copy native glyph outline commands before releasing the Cairo context."
  (sb-alien:with-alien ((glyph font-cairo-glyph))
    (setf (sb-alien:slot glyph 'index) glyph-code
          (sb-alien:slot glyph 'x) (coerce x 'double-float)
          (sb-alien:slot glyph 'y) (coerce y 'double-float))
    (%font-native-call "cairo_glyph_path" sb-alien:void
      ((* t) cr) ((* font-cairo-glyph) (sb-alien:addr glyph)) (sb-alien:int 1))
    (let ((path (%font-native-call "cairo_copy_path" (* font-cairo-path) ((* t) cr))))
      (unwind-protect
           (progn
             (unless (zerop (sb-alien:slot path 'status)) (%font-render-error "Cannot obtain glyph outline"))
             (let ((data (sb-alien:alien-sap (sb-alien:slot path 'data))))
               (loop with i = 0 while (< i (sb-alien:slot path 'count))
                     for type = (sb-sys:sap-ref-32 data (* i 16))
                     for length = (sb-sys:sap-ref-32 data (+ (* i 16) 4))
                     collect (cons type (loop for point from 1 below length
                                              append (list (sb-sys:sap-ref-double data (* (+ i point) 16))
                                                           (sb-sys:sap-ref-double data (+ (* (+ i point) 16) 8)))))
                     do (incf i length))))
        (%font-native-call "cairo_path_destroy" sb-alien:void ((* font-cairo-path) path))))))

(defun %font-java-path (commands)
  "Build a Java GeneralPath after all native resources and locks are released."
  (classload "java/awt/geom/GeneralPath")
  (let ((path (%make-java-instance "java/awt/geom/GeneralPath")))
    (|<init>()V| path)
    (dolist (command commands path)
      (let ((points (mapcar (lambda (v) (coerce v 'single-float)) (rest command))))
        (case (first command)
          (0 (apply #'|moveTo(FF)V| path points))
          (1 (apply #'|lineTo(FF)V| path points))
          (2 (apply #'|curveTo(FFFFFF)V| path points))
          (3 (|closePath()V| path))
          (otherwise (%font-render-error "Invalid native glyph path command")))))))

(%define-graphics-id-initializers "sun/font/FreetypeFontScaler.initIDs(Ljava/lang/Class;)V")

(defmethod |initNativeScaler(Lsun/font/Font2D;IIZI)J| (this font type index cjk size)
  "Create a logical font scaler backed by the actual font file."
  (declare (ignore this type cjk size))
  (let ((file (lstring (slot-value font '|platName|))))
    (unless (and file (uiop:file-exists-p (sb-ext:parse-native-namestring file)))
      (%font-render-error "Font file is unavailable"))
    (bordeaux-threads:with-recursive-lock-held (*fontconfig-lock*)
      (let ((handle (incf *font-next-handle*)))
        (setf (gethash handle *font-scalers*) (make-font-scaler-state :file file :index index))
        handle))))

(defmethod |createScalerContextNative(J[DIIFF)J| (this scaler transform aa fm bold italic)
  "Create a logical strike carrying the Java font transform and rendering hints."
  (declare (ignore this))
  (bordeaux-threads:with-recursive-lock-held (*fontconfig-lock*)
    (%font-scaler scaler)
    (let ((handle (incf *font-next-handle*)))
      (setf (gethash handle *font-contexts*)
            (make-font-context-state :scaler scaler :matrix (copy-seq (java-array-data transform))
                                     :aa aa :fm fm :bold bold :italic italic))
      handle)))

(defun |sun/font/NullFontScaler.getNullScalerContext()J| ()
  "Reserved context for the JDK's explicit null scaler."
  1)

(defmethod |getGlyphAdvanceNative(Lsun/font/Font2D;JJI)F| (this font context scaler glyph)
  "Return the actual glyph's device-space horizontal advance."
  (declare (ignore this font))
  (coerce (fifth (%font-with-cairo scaler context (lambda (cr) (%font-glyph-extents cr glyph)))) 'single-float))

(defmethod |getGlyphMetricsNative(Lsun/font/Font2D;JJILjava/awt/geom/Point2D$Float;)V|
    (this font context scaler glyph point)
  "Populate the actual glyph's two-dimensional advance."
  (declare (ignore this font))
  (let ((extents (%font-with-cairo scaler context (lambda (cr) (%font-glyph-extents cr glyph)))))
    (setf (slot-value point '|x|) (coerce (fifth extents) 'single-float)
          (slot-value point '|y|) (coerce (sixth extents) 'single-float)))
  nil)

(defmethod |getGlyphOutlineNative(Lsun/font/Font2D;JJIFF)Ljava/awt/geom/GeneralPath;|
    (this font context scaler glyph x y)
  "Return a transformed outline of the selected glyph."
  (declare (ignore this font))
  (%font-java-path (%font-with-cairo scaler context (lambda (cr) (%font-path-commands cr glyph x y)))))

(defmethod |getGlyphVectorOutlineNative(Lsun/font/Font2D;JJ[IIFF)Ljava/awt/geom/GeneralPath;|
    (this font context scaler glyphs count x y)
  "Return glyph outlines at their advancing baseline positions."
  (declare (ignore this font))
  (%font-java-path
   (%font-with-cairo scaler context
     (lambda (cr)
       (loop for index below count for glyph = (jaref glyphs index)
             append (%font-path-commands cr glyph x y)
             do (let ((extent (%font-glyph-extents cr glyph)))
                  (incf x (coerce (fifth extent) 'single-float))
                  (incf y (coerce (sixth extent) 'single-float))
                  (%font-native-call "cairo_new_path" sb-alien:void ((* t) cr))))))))

(defmethod |disposeNativeScaler(Lsun/font/Font2D;J)V| (this font scaler)
  "Release logical scaler state and all associated contexts."
  (declare (ignore this font))
  (bordeaux-threads:with-recursive-lock-held (*fontconfig-lock*)
    (remhash scaler *font-scalers*)
    (maphash (lambda (handle context)
               (when (= scaler (font-context-state-scaler context)) (remhash handle *font-contexts*)))
             *font-contexts*))
  nil)

(defvar *font-glyph-records* (make-hash-table))

(defun %font-glyph-bitmap (cr glyph-code)
  "Copy a glyph's alpha bitmap and metrics while the configured font is live."
  (let* ((extents (%font-glyph-extents cr glyph-code))
         (left (floor (first extents))) (top (floor (second extents)))
         (width (max 0 (- (ceiling (+ (first extents) (third extents))) left)))
         (height (max 0 (- (ceiling (+ (second extents) (fourth extents))) top)))
         (surface (progn
                    (when (or (> width 65532) (> height 65535))
                      (%font-render-error "Glyph bitmap exceeds cache dimensions"))
                    (%font-native-call "cairo_image_surface_create" (* t)
                      (sb-alien:int 2) (sb-alien:int width) (sb-alien:int height))))
         (target (%font-native-call "cairo_create" (* t) ((* t) surface)))
         (options (%font-native-call "cairo_font_options_create" (* t))))
    (unwind-protect
         (progn
           (%font-native-call "cairo_set_font_face" sb-alien:void
             ((* t) target) ((* t) (%font-native-call "cairo_get_font_face" (* t) ((* t) cr))))
           (sb-alien:with-alien ((matrix font-cairo-matrix) (glyph font-cairo-glyph))
             (%font-native-call "cairo_get_font_matrix" sb-alien:void
               ((* t) cr) ((* font-cairo-matrix) (sb-alien:addr matrix)))
             (%font-native-call "cairo_set_font_matrix" sb-alien:void
               ((* t) target) ((* font-cairo-matrix) (sb-alien:addr matrix)))
             (%font-native-call "cairo_get_font_options" sb-alien:void ((* t) cr) ((* t) options))
             (%font-native-call "cairo_set_font_options" sb-alien:void ((* t) target) ((* t) options))
             (%font-native-call "cairo_set_source_rgba" sb-alien:void
               ((* t) target) (sb-alien:double 1d0) (sb-alien:double 1d0)
               (sb-alien:double 1d0) (sb-alien:double 1d0))
             (setf (sb-alien:slot glyph 'index) glyph-code
                   (sb-alien:slot glyph 'x) (coerce (- left) 'double-float)
                   (sb-alien:slot glyph 'y) (coerce (- top) 'double-float))
             (%font-native-call "cairo_show_glyphs" sb-alien:void
               ((* t) target) ((* font-cairo-glyph) (sb-alien:addr glyph)) (sb-alien:int 1)))
           (unless (zerop (%font-native-call "cairo_status" sb-alien:int ((* t) target)))
             (%font-render-error "Cannot rasterize glyph"))
           (%font-native-call "cairo_surface_flush" sb-alien:void ((* t) surface))
           (let* ((stride (%font-native-call "cairo_image_surface_get_stride" sb-alien:int ((* t) surface)))
                  (data (%font-native-call "cairo_image_surface_get_data" (* t) ((* t) surface)))
                  (pixels (make-array (* height width) :element-type '(unsigned-byte 8))))
             ;; Cairo aligns A8 scanlines, but DrawGlyphList's Java fallback
             ;; indexes its coverage mask using the glyph width as the stride.
             ;; Publish packed rows in both the pixels and GlyphInfo.rowBytes.
             (dotimes (row height)
               (dotimes (column width)
                 (setf (aref pixels (+ (* row width) column))
                       (sb-sys:sap-ref-8 (sb-alien:alien-sap data) (+ (* row stride) column)))))
             (list extents left top width height width pixels)))
      (%font-native-call "cairo_font_options_destroy" sb-alien:void ((* t) options))
      (%font-native-call "cairo_destroy" sb-alien:void ((* t) target))
      (%font-native-call "cairo_surface_destroy" sb-alien:void ((* t) surface)))))

(defmethod |getGlyphImageNative(Lsun/font/Font2D;JJI)J| (this font context scaler glyph)
  "Rasterize a glyph into an owned JDK 25 glyph record and alpha bitmap."
  (declare (ignore this font))
  (destructuring-bind (extents left top width height stride pixels)
      (%font-with-cairo scaler context (lambda (cr) (%font-glyph-bitmap cr glyph)))
    (when (or (> width 65535) (> height 65535) (> stride 65535))
      (%font-render-error "Glyph bitmap exceeds cache dimensions"))
    (let* ((address (%allocate-unsafe-memory (+ 40 (length pixels))))
           (sap (sb-sys:int-sap address)))
      (setf (sb-sys:sap-ref-single sap 0) (coerce (fifth extents) 'single-float)
            (sb-sys:sap-ref-single sap 4) (coerce (sixth extents) 'single-float)
            (sb-sys:sap-ref-16 sap 8) width (sb-sys:sap-ref-16 sap 10) height
            (sb-sys:sap-ref-16 sap 12) stride
            (sb-sys:sap-ref-single sap 16) (coerce left 'single-float)
            (sb-sys:sap-ref-single sap 20) (coerce top 'single-float)
            (sb-sys:sap-ref-64 sap 32) (if (plusp (length pixels)) (+ address 40) 0))
      (dotimes (i (length pixels)) (setf (sb-sys:sap-ref-8 sap (+ 40 i)) (aref pixels i)))
      (bordeaux-threads:with-recursive-lock-held (*fontconfig-lock*)
        (setf (gethash address *font-glyph-records*) t))
      address)))

(defun %font-free-glyph (address)
  "Free only glyph cache allocations owned by this font backend."
  (bordeaux-threads:with-recursive-lock-held (*fontconfig-lock*)
    (when (remhash address *font-glyph-records*) (%free-unsafe-memory address))))

(defun |sun/font/StrikeCache.freeLongPointer(J)V| (address)
  "Release one owned glyph cache record."
  (%font-free-glyph address))

(defun |sun/font/StrikeCache.freeIntPointer(I)V| (address)
  "Release a glyph pointer represented by an unsigned 32-bit value."
  (%font-free-glyph (logand address #xffffffff)))

(defun |sun/font/StrikeCache.freeLongMemory([JJ)V| (pointers context)
  "Release a disposed strike's glyph images and logical scaler context."
  (when pointers (loop for address across (java-array-data pointers) do (%font-free-glyph address)))
  (bordeaux-threads:with-recursive-lock-held (*fontconfig-lock*) (remhash context *font-contexts*))
  nil)

(defun |sun/font/StrikeCache.freeIntMemory([IJ)V| (pointers context)
  "Release a disposed strike whose glyph pointers use 32-bit storage."
  (when pointers
    (loop for address across (java-array-data pointers) do (%font-free-glyph (logand address #xffffffff))))
  (bordeaux-threads:with-recursive-lock-held (*fontconfig-lock*) (remhash context *font-contexts*))
  nil)

(defmethod |getGlyphImage(JI)J| (this context glyph)
  "Supply the shared empty image for the JDK's explicit null scaler."
  (declare (ignore this context glyph))
  (|sun/font/StrikeCache.getInvisibleGlyphPtr()J|))

(defmethod |getGlyphOutlineBoundsNative(Lsun/font/Font2D;JJI)Ljava/awt/geom/Rectangle2D$Float;|
    (this font context scaler glyph)
  "Return the selected glyph's transformed ink bounds."
  (declare (ignore this font))
  (let ((extents (%font-with-cairo scaler context (lambda (cr) (%font-glyph-extents cr glyph)))))
    (classload "java/awt/geom/Rectangle2D$Float")
    (let ((rectangle (%make-java-instance "java/awt/geom/Rectangle2D$Float")))
      (apply #'|<init>(FFFF)V| rectangle
             (mapcar (lambda (n) (coerce n 'single-float)) (serapeum:take 4 extents)))
      rectangle)))

(defmethod |getFontMetricsNative(Lsun/font/Font2D;JJ)Lsun/font/StrikeMetrics;| (this font context scaler)
  "Return scaled font metrics from the actual font face."
  (declare (ignore this font))
  (let ((metrics (%font-with-cairo scaler context
                   (lambda (cr)
                     (sb-alien:with-alien ((values (sb-alien:array sb-alien:double 5)))
                       (%font-native-call "cairo_font_extents" sb-alien:void
                         ((* t) cr) ((* sb-alien:double) (sb-alien:cast (sb-alien:addr values) (* sb-alien:double))))
                       (loop for i below 5 collect (sb-alien:deref values i)))))))
    (classload "sun/font/StrikeMetrics")
    (let ((result (%make-java-instance "sun/font/StrikeMetrics")))
      (destructuring-bind (ascent descent height advance-x advance-y) metrics
        (apply #'|<init>(FFFFFFFFFF)V| result
               (mapcar (lambda (n) (coerce n 'single-float))
                       (list 0 (- ascent) 0 descent 0 0 0 (max 0 (- height ascent descent)) advance-x advance-y))))
      result)))
