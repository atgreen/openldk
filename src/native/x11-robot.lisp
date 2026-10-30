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

(defvar *x11-test-loaded* nil)
(defvar *x11-robot-button-masks* #())

(defun %load-x11-test ()
  "Load the ordinary system XTest library used for real input injection."
  (%load-x11)
  (unless *x11-test-loaded*
    (handler-case (sb-alien:load-shared-object "libXtst.so.6")
      (error () (%throw '|java/awt/AWTException| "The XTest library is unavailable")))
    (setf *x11-test-loaded* t)))

(defun %x11-robot-sync ()
  "Complete robot requests before returning to the Java caller."
  (%libc "XSync" sb-alien:int ((* t) (%x11-display)) (sb-alien:int 0)))

(%define-x11-awt-native "sun/awt/X11/XRobotPeer.loadNativeLibraries()V" ()
  (%load-x11-test) nil)
(%define-x11-awt-native "sun/awt/X11/XRobotPeer.setup(I[I)V" (count masks)
  (%load-x11-test)
  (bt:with-recursive-lock-held (*x11-lock*)
    (sb-alien:with-alien ((event-base sb-alien:int) (error-base sb-alien:int)
                         (major sb-alien:int) (minor sb-alien:int))
      (unless (and (not (zerop (%libc "XTestQueryExtension" sb-alien:int ((* t) (%x11-display))
                               ((* sb-alien:int) (sb-alien:addr event-base))
                               ((* sb-alien:int) (sb-alien:addr error-base))
                               ((* sb-alien:int) (sb-alien:addr major))
                               ((* sb-alien:int) (sb-alien:addr minor)))))
                   (or (> major 2) (and (= major 2) (>= minor 2))))
        (%throw '|java/awt/AWTException| "Robot requires XTEST version 2.2")))
    (%libc "XTestGrabControl" sb-alien:int ((* t) (%x11-display)) (sb-alien:int 1))
    (setf *x11-robot-button-masks* (take (min count (length (java-array-data masks))) (java-array-data masks)))) nil)

(%define-x11-awt-native "sun/awt/X11/XRobotPeer.<init>(Lsun/awt/X11GraphicsDevice;)V" (this device)
  ;; This peer implements capture and injection directly through Xlib/XTest.
  ;; Its constructor therefore selects that backend without the GTK adapter.
  (setf (slot-value this '|xgc|) (%x11-java-instance device "getDefaultConfiguration()Ljava/awt/GraphicsConfiguration;"))
  (let* ((toolkit (%x11-java-static "java/awt/Toolkit" "getDefaultToolkit()Ljava/awt/Toolkit;"))
         (accessor (%x11-java-static "sun/awt/AWTAccessor" "getInputEventAccessor()Lsun/awt/AWTAccessor$InputEventAccessor;")))
    (%x11-java-static "sun/awt/X11/XRobotPeer" "setup(I[I)V"
                      (%x11-java-instance toolkit "getNumberOfButtons()I")
                      (%x11-java-instance accessor "getButtonDownMasks()[I"))) nil)

(%define-x11-awt-native "sun/awt/X11/XRobotPeer.mouseMoveImpl(Lsun/awt/X11GraphicsConfig;II)V" (configuration x y)
  (multiple-value-bind (visual screen) (%x11-configuration-visual configuration)
    (declare (ignore visual))
    (bt:with-recursive-lock-held (*x11-lock*)
      (%libc "XWarpPointer" sb-alien:int ((* t) (%x11-display)) (sb-alien:unsigned-long 0)
        (sb-alien:unsigned-long (%libc "XRootWindow" sb-alien:unsigned-long
                                  ((* t) (%x11-display)) (sb-alien:int screen)))
        (sb-alien:int 0) (sb-alien:int 0) (sb-alien:unsigned-int 0) (sb-alien:unsigned-int 0)
        (sb-alien:int x) (sb-alien:int y))
      (%x11-robot-sync))) nil)

(defun %x11-robot-button (button press)
  "Queue a real XTest button transition, including wheel buttons four and five."
  (when (zerop (%libc "XTestFakeButtonEvent" sb-alien:int ((* t) (%x11-display))
                 (sb-alien:unsigned-int button) (sb-alien:int press) (sb-alien:unsigned-long 0)))
    (%throw '|java/awt/AWTError| "XTest rejected a mouse button event")))

(defun %x11-robot-buttons (buttons press)
  "Translate legacy and extended Java masks to physical X buttons."
  (bt:with-recursive-lock-held (*x11-lock*)
    (dotimes (i (length *x11-robot-button-masks*))
      (let ((mask (logior (aref *x11-robot-button-masks* i)
                         (case i (0 16) (1 8) (2 4) (otherwise 0)))))
        (unless (zerop (logand buttons mask))
          (%x11-robot-button (if (< i 3) (1+ i) (+ i 3)) press))))
    (%x11-robot-sync)) nil)

(%define-x11-awt-native "sun/awt/X11/XRobotPeer.mousePressImpl(I)V" (buttons) (%x11-robot-buttons buttons 1))
(%define-x11-awt-native "sun/awt/X11/XRobotPeer.mouseReleaseImpl(I)V" (buttons) (%x11-robot-buttons buttons 0))
(%define-x11-awt-native "sun/awt/X11/XRobotPeer.mouseWheelImpl(I)V" (amount)
  (bt:with-recursive-lock-held (*x11-lock*)
    (dotimes (i (abs amount))
      (%x11-robot-button (if (minusp amount) 4 5) 1)
      (%x11-robot-button (if (minusp amount) 4 5) 0))
    (%x11-robot-sync)) nil)

(defvar *x11-java-keysyms* nil)

(defun %x11-keysym-priority (entry)
  "Prefer standard, left, right, then keypad mappings and canonical lowercase letters."
  (let ((symbol (first entry))
        (location (case (rest entry) (1 0) (2 1) (3 2) (4 3) (otherwise 4))))
    (+ (ash location 48) (if (<= 65 symbol 90) (ash 1 40) 0) symbol)))

(defun %x11-keysyms-for-java-code (code)
  "Invert the JDK's complete forward table, retaining alternate keyboard locations."
  (unless *x11-java-keysyms*
    (bt:with-recursive-lock-held (*x11-lock*)
      (unless *x11-java-keysyms*
        (let* ((table (%graphics-static "sun/awt/X11/XKeysym" '|keysym2JavaKeycodeHash|))
               (entries (%x11-java-instance table "entrySet()Ljava/util/Set;"))
               (iterator (%x11-java-instance entries "iterator()Ljava/util/Iterator;"))
               (reverse (make-hash-table)))
          (loop until (zerop (%x11-java-instance iterator "hasNext()Z"))
                do (let* ((entry (%x11-java-instance iterator "next()Ljava/lang/Object;"))
                          (key (%x11-java-instance entry "getKey()Ljava/lang/Object;"))
                          (mapping (%x11-java-instance entry "getValue()Ljava/lang/Object;"))
                          (java-code (%x11-java-instance mapping "getJavaKeycode()I")))
                     (unless (zerop java-code)
                       (push (cons (%x11-java-instance key "longValue()J")
                                   (%x11-java-instance mapping "getKeyLocation()I"))
                             (gethash java-code reverse)))))
          (maphash (lambda (key values)
                     (setf (gethash key reverse) (sort values #'< :key #'%x11-keysym-priority))) reverse)
          (setf *x11-java-keysyms* reverse)))))
  (gethash code *x11-java-keysyms*))

(defun %x11-keysym-for-java-code (code)
  "Choose a keysym available on the real keyboard, falling back to its canonical mapping."
  (let ((entries (%x11-keysyms-for-java-code code)))
    (or (loop for (symbol . location) in entries
              when (not (zerop (%libc "XKeysymToKeycode" sb-alien:unsigned-char
                                 ((* t) (%x11-display)) (sb-alien:unsigned-long symbol))))
                return symbol)
        (caar entries) 0)))

(defun %x11-robot-key (java-code press)
  "Resolve the JDK's keysym table against the server's current keyboard map."
  (let* ((symbol (%x11-keysym-for-java-code java-code))
         (code (%libc "XKeysymToKeycode" sb-alien:unsigned-char
                 ((* t) (%x11-display)) (sb-alien:unsigned-long symbol))))
    (when (or (zerop symbol) (zerop code)) (%throw '|java/lang/IllegalArgumentException| "No X11 mapping for Java keycode"))
    (bt:with-recursive-lock-held (*x11-lock*)
      (%libc "XTestFakeKeyEvent" sb-alien:int ((* t) (%x11-display))
        (sb-alien:unsigned-int code) (sb-alien:int press) (sb-alien:unsigned-long 0))
      (%x11-robot-sync))) nil)

(%define-x11-awt-native "sun/awt/X11/XRobotPeer.keyPressImpl(I)V" (code) (%x11-robot-key code 1))
(%define-x11-awt-native "sun/awt/X11/XRobotPeer.keyReleaseImpl(I)V" (code) (%x11-robot-key code 0))

(%define-x11-awt-native "sun/awt/X11/XWindow.getKeySymForAWTKeyCode(I)I" (code)
  (%as-int (%x11-keysym-for-java-code code)))
(%define-x11-awt-native "sun/awt/X11/XWindow.getAWTKeyCodeForKeySym(I)I" (symbol)
  (if-let ((mapping (%x11-java-static "sun/awt/X11/XKeysym" "getJavaKeycode(J)Lsun/awt/X11/XKeysym$Keysym2JavaKeycode;"
                                     (ldb (byte 32 0) symbol))))
    (%x11-java-instance mapping "getJavaKeycode()I") 0))

(%define-xlib "XConvertCase" "(JJJ)V" "XConvertCase" :void (sb-alien:unsigned-long :pointer :pointer))
(%define-xlib "XkbKeycodeToKeysym" "(JIII)J" "XkbKeycodeToKeysym" sb-alien:unsigned-long
  (:pointer sb-alien:unsigned-char sb-alien:int sb-alien:int))
(%define-xlib "XkbTranslateKeyCode" "(JIJJJ)Z" "XkbTranslateKeyCode" :boolean
  (:pointer sb-alien:unsigned-int sb-alien:unsigned-int :pointer :pointer))
