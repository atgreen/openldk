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
(in-package :openldk)

;; Redefined with the Java fields/interfaces when its class file loads.
(defclass |sun/java2d/pipe/ShapeSpanIterator| (|java/lang/Object|) ()
  (:documentation "Shape span iterator stub, populated from its Java class file."))
(defclass |sun/java2d/pipe/SpanClipRenderer| (|java/lang/Object|) ()
  (:documentation "Span clip renderer stub, populated from its Java class file."))

(defstruct shape-span-state
  path normalize (adjust-x 0.0) (adjust-y 0.0)
  (clip (vector #x-80000000 #x-80000000 #x7fffffff #x7fffffff))
  bounds edges prepared next-y pending)

(defvar *shape-span-states* (make-hash-table :test #'eq :weakness :key :synchronized t))

(defun %shape-span-state (iterator)
  "The CL geometry owned by ITERATOR, rejecting use after disposal."
  (or (gethash iterator *shape-span-states*)
      (%throw '|java/lang/NullPointerException| "Disposed shape span iterator")))

(defmethod |setNormalize(Z)V| ((iterator |sun/java2d/pipe/ShapeSpanIterator|) normalize)
  (let ((path (%make-java-instance "java/awt/geom/Path2D$Float")))
    (|<init>()V| path)
    (setf (gethash iterator *shape-span-states*)
          (make-shape-span-state :path path :normalize (not (zerop normalize))))))

(defun %shape-span-adjust (state coordinates)
  "Normalize endpoints to the quarter-pixel grid, moving curve controls with them."
  (let* ((points (coerce coordinates 'vector))
         (end (- (length points) 2)))
    (when (shape-span-state-normalize state)
      (let* ((x (aref points end)) (y (aref points (1+ end)))
             (dx (- (+ (floor (+ x .25)) .25) x))
             (dy (- (+ (floor (+ y .25)) .25) y))
             (old-x (shape-span-state-adjust-x state))
             (old-y (shape-span-state-adjust-y state)))
        (case end
          ;; Match ShapeSpanIterator's quadratic normalization, which uses
          ;; the new Y adjustment for both control-coordinate averages.
          (2 (incf (aref points 0) (/ (+ old-x dy) 2))
             (incf (aref points 1) (/ (+ old-y dy) 2)))
          (4 (incf (aref points 0) old-x) (incf (aref points 1) old-y)
             (incf (aref points 2) dx) (incf (aref points 3) dy))
          (otherwise nil))
        (incf (aref points end) dx) (incf (aref points (1+ end)) dy)
        (setf (shape-span-state-adjust-x state) dx (shape-span-state-adjust-y state) dy)))
    (map 'list (lambda (value) (coerce value 'single-float)) points)))

(defun %shape-span-add (iterator type coordinates)
  "Append a path segment to the iterator's Java geometry."
  (let* ((state (%shape-span-state iterator))
         (path (shape-span-state-path state))
         (points (unless (= type 4) (%shape-span-adjust state coordinates))))
    (ecase type
      (0 (apply #'|moveTo(FF)V| path points))
      (1 (apply #'|lineTo(FF)V| path points))
      (2 (apply #'|quadTo(FFFF)V| path points))
      (3 (apply #'|curveTo(FFFFFF)V| path points))
      (4 (|closePath()V| path)))
    (setf (shape-span-state-prepared state) nil
          (shape-span-state-next-y state) nil (shape-span-state-pending state) nil)))

(defmethod |setRule(I)V| ((iterator |sun/java2d/pipe/ShapeSpanIterator|) rule)
  (|setWindingRule(I)V| (shape-span-state-path (%shape-span-state iterator)) rule))

(defmethod |addSegment(I[F)V| ((iterator |sun/java2d/pipe/ShapeSpanIterator|) type coordinates)
  (%shape-span-add iterator type
                   (loop for i below (ecase type (0 2) (1 2) (2 4) (3 6) (4 0))
                         collect (jaref coordinates i))))

(defmethod |moveTo(FF)V| ((iterator |sun/java2d/pipe/ShapeSpanIterator|) x y)
  (%shape-span-add iterator 0 (list x y)))
(defmethod |lineTo(FF)V| ((iterator |sun/java2d/pipe/ShapeSpanIterator|) x y)
  (%shape-span-add iterator 1 (list x y)))
(defmethod |quadTo(FFFF)V| ((iterator |sun/java2d/pipe/ShapeSpanIterator|) x1 y1 x2 y2)
  (%shape-span-add iterator 2 (list x1 y1 x2 y2)))
(defmethod |curveTo(FFFFFF)V| ((iterator |sun/java2d/pipe/ShapeSpanIterator|) x1 y1 x2 y2 x3 y3)
  (%shape-span-add iterator 3 (list x1 y1 x2 y2 x3 y3)))
(defmethod |closePath()V| ((iterator |sun/java2d/pipe/ShapeSpanIterator|))
  (%shape-span-add iterator 4 nil))

(defun %shape-span-edge (x0 y0 x1 y1)
  "Prepare integer pixel-center crossings with the JDK's 31-bit fractional steps."
  (let ((direction (if (< y0 y1) 1 -1)))
    (when (> y0 y1) (rotatef x0 x1) (rotatef y0 y1))
    (let ((start (ceiling (- y0 .5))) (end (ceiling (- y1 .5))))
      (when (< start end)
        (let* ((dx (- x1 x0)) (dy (- y1 y0))
               (slope (/ dx dy))
               (first (+ x0 (/ (* (- (+ start .5) y0) dx) dy)))
               (x (ceiling (- first .5)))
               (step (floor slope))
               (fraction (truncate (* (coerce (- slope step) 'double-float) #x7fffffff)))
               (error (truncate (* (coerce (- first (- x .5)) 'double-float) #x7fffffff))))
          (vector start end x step fraction error direction))))))

(defun %shape-span-prepare (state)
  "Flatten the Java path once; retain line edges for scanline winding evaluation."
  (unless (shape-span-state-prepared state)
    (let* ((path (shape-span-state-path state))
           (box (|getBounds2D()Ljava/awt/geom/Rectangle2D;| path))
           (iterator (|getPathIterator(Ljava/awt/geom/AffineTransform;D)Ljava/awt/geom/PathIterator;| path nil 1d0))
           (coords (make-java-array :component-class (%get-java-class-by-bin-name "float") :size 6))
           (edges nil) (x 0.0) (y 0.0) (mx 0.0) (my 0.0) (started nil))
      (labels ((line (nx ny)
                 (when-let ((edge (%shape-span-edge x y nx ny))) (push edge edges))
                 (setf x nx y ny))
               (close-contour () (when started (line mx my))))
        (loop while (zerop (|isDone()Z| iterator))
              for type = (|currentSegment([F)I| iterator coords)
              do (ecase type
                   (0 (close-contour)
                      (setf x (jaref coords 0) y (jaref coords 1) mx x my y started t))
                   (1 (line (jaref coords 0) (jaref coords 1)))
                   (4 (close-contour)))
                 (|next()V| iterator))
        (close-contour))
      (setf (shape-span-state-edges state) edges
            (shape-span-state-bounds state)
            (vector (floor (|getMinX()D| box)) (floor (|getMinY()D| box))
                    (ceiling (|getMaxX()D| box)) (ceiling (|getMaxY()D| box)))
            (shape-span-state-prepared state) t)))
  state)

(defmethod |pathDone()V| ((iterator |sun/java2d/pipe/ShapeSpanIterator|))
  (%shape-span-prepare (%shape-span-state iterator))
  nil)

(defmethod |setOutputAreaXYXY(IIII)V| ((iterator |sun/java2d/pipe/ShapeSpanIterator|) lx ly hx hy)
  (setf (shape-span-state-clip (%shape-span-state iterator)) (vector lx ly hx hy)))

(defmethod |getPathBox([I)V| ((iterator |sun/java2d/pipe/ShapeSpanIterator|) box)
  (let ((bounds (shape-span-state-bounds (%shape-span-prepare (%shape-span-state iterator)))))
    (dotimes (i 4) (setf (jaref box i) (aref bounds i)))))

(defmethod |intersectClipBox(IIII)V| ((iterator |sun/java2d/pipe/ShapeSpanIterator|) lx ly hx hy)
  (let* ((state (%shape-span-state iterator)) (clip (shape-span-state-clip state)))
    (setf (aref clip 0) (max lx (aref clip 0)) (aref clip 1) (max ly (aref clip 1))
          (aref clip 2) (min hx (aref clip 2)) (aref clip 3) (min hy (aref clip 3)))
    (setf (shape-span-state-pending state)
          (loop for box in (shape-span-state-pending state)
                for left = (max (aref box 0) (aref clip 0))
                for right = (min (aref box 2) (aref clip 2))
                when (and (< left right) (<= (aref clip 1) (aref box 1)) (< (aref box 1) (aref clip 3)))
                  collect (vector left (aref box 1) right (aref box 3))))))

(defun %shape-span-row (state y lx hx)
  "Return sorted spans for pixel centers on scanline Y using the path's winding rule."
  (let ((events nil) (spans nil) (winding 0) (start nil)
        (even-odd (zerop (|getWindingRule()I| (shape-span-state-path state)))))
    (dolist (edge (shape-span-state-edges state))
      (when (<= (aref edge 0) y (1- (aref edge 1)))
        (let* ((steps (- y (aref edge 0)))
               (x (+ (aref edge 2) (* steps (aref edge 3))
                     (floor (+ (aref edge 5) (* steps (aref edge 4))) #x80000000))))
          (push (cons x (aref edge 6)) events))))
    (setf events (sort events #'< :key #'car))
    (loop while events
          for x = (caar events)
          for before = (if even-odd (oddp winding) (not (zerop winding)))
          do (loop while (and events (= (caar events) x)) do (incf winding (cdar events)) (pop events))
             (let ((after (if even-odd (oddp winding) (not (zerop winding)))))
               (cond
                 ((and (not before) after) (setf start x))
                 ((and before (not after))
                  (let ((left (max lx start)) (right (min hx x)))
                    (when (< left right) (push (vector left y right (1+ y)) spans))))
                 (t nil))))
    (nreverse spans)))

(defmethod |nextSpan([I)Z| ((iterator |sun/java2d/pipe/ShapeSpanIterator|) box)
  (let* ((state (%shape-span-prepare (%shape-span-state iterator)))
         (bounds (shape-span-state-bounds state)) (clip (shape-span-state-clip state))
         (lx (max (aref bounds 0) (aref clip 0))) (ly (max (aref bounds 1) (aref clip 1)))
         (hx (min (aref bounds 2) (aref clip 2))) (hy (min (aref bounds 3) (aref clip 3))))
    (setf (shape-span-state-next-y state) (max ly (or (shape-span-state-next-y state) ly)))
    (loop
      (when-let ((span (pop (shape-span-state-pending state))))
        (dotimes (i 4) (setf (jaref box i) (aref span i)))
        (return 1))
      (when (or (>= lx hx) (>= (shape-span-state-next-y state) hy)) (return 0))
      (setf (shape-span-state-pending state) (%shape-span-row state (shape-span-state-next-y state) lx hx))
      (incf (shape-span-state-next-y state)))))

(defmethod |skipDownTo(I)V| ((iterator |sun/java2d/pipe/ShapeSpanIterator|) y)
  (let ((state (%shape-span-state iterator)))
    (setf (shape-span-state-next-y state) (max y (or (shape-span-state-next-y state) y))
          (shape-span-state-pending state)
          (remove-if (lambda (box) (< (aref box 1) y)) (shape-span-state-pending state)))))

(defmethod |appendPoly([I[IIII)V| ((iterator |sun/java2d/pipe/ShapeSpanIterator|) xs ys count xoff yoff)
  (|setRule(I)V| iterator 0)
  (when (plusp count)
    (dotimes (i count)
      (%shape-span-add iterator (if (zerop i) 0 1)
                       (list (+ xoff (jaref xs i)) (+ yoff (jaref ys i)))))
    (|closePath()V| iterator))
  (|pathDone()V| iterator))

(defmethod |getNativeIterator()J| ((iterator |sun/java2d/pipe/ShapeSpanIterator|))
  (declare (ignore iterator)) 0)
(defmethod |getNativeConsumer()J| ((iterator |sun/java2d/pipe/ShapeSpanIterator|))
  (declare (ignore iterator)) 0)
(defmethod |dispose()V| ((iterator |sun/java2d/pipe/ShapeSpanIterator|))
  (remhash iterator *shape-span-states*) nil)

(defun %shape-clip-tile (iterator alpha offset stride band fill)
  "Intersect an alpha tile with the Java region, preserving existing coverage."
  (let* ((region (slot-value iterator '|region|))
         (lx (jaref band 0)) (ly (jaref band 1)) (hx (jaref band 2)) (hy (jaref band 3))
         (left hx) (top hy) (right lx) (bottom ly))
    (loop for y from ly below hy
          do (loop for x from lx below hx
                   for index = (+ offset (- x lx) (* (- y ly) stride))
                   do (if (zerop (|contains(II)Z| region x y))
                          (setf (jaref alpha index) 0)
                          (progn
                            (when fill (setf (jaref alpha index) -1))
                            (setf left (min left x) top (min top y)
                                  right (max right (1+ x)) bottom (max bottom (1+ y)))))))
    (if (and (< left right) (< top bottom))
        (setf (jaref band 0) left (jaref band 1) top (jaref band 2) right (jaref band 3) bottom)
        (setf (jaref band 2) lx (jaref band 3) ly)))
  nil)

(defmethod |fillTile(Lsun/java2d/pipe/RegionIterator;[BII[I)V|
    ((renderer |sun/java2d/pipe/SpanClipRenderer|) iterator alpha offset stride band)
  (declare (ignore renderer))
  (%shape-clip-tile iterator alpha offset stride band t))

(defmethod |eraseTile(Lsun/java2d/pipe/RegionIterator;[BII[I)V|
    ((renderer |sun/java2d/pipe/SpanClipRenderer|) iterator alpha offset stride band)
  (declare (ignore renderer))
  (%shape-clip-tile iterator alpha offset stride band nil))
