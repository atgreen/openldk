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

(defstruct x11-input-context pointer display composing)
(defvar *x11-input-contexts* (make-hash-table :test #'eq :weakness :key :synchronized t))
(defvar *x11-active-input-method* nil)

(defun %x11-current-input-method ()
  "Return the live Java input method whose XIC currently owns keyboard focus."
  (when *x11-active-input-method* (sb-ext:weak-pointer-value *x11-active-input-method*)))

(defun %close-x11-input-context (context)
  "Destroy an owned XIC without retaining its Java input-method owner."
  (bt:with-recursive-lock-held (*x11-lock*)
    (when (x11-input-context-pointer context)
      (when (eq (x11-input-context-display context) *x11-display*)
        (%libc "XDestroyIC" sb-alien:void ((* t) (x11-input-context-pointer context))))
      (setf (x11-input-context-pointer context) nil
            (x11-input-context-composing context) nil))) nil)

(%define-x11-awt-native "sun/awt/X11/XInputMethod.createXICNative(J)Z" (this window)
  (bt:with-recursive-lock-held (*x11-lock*)
    (cond
      ((gethash this *x11-input-contexts*) 1)
      ((null *x11-input-method*) 0)
      (t
       (let ((pointer (%libc "XCreateIC" (* t) ((* t) *x11-input-method*)
                        (sb-alien:c-string "inputStyle") (sb-alien:unsigned-long #x408)
                        (sb-alien:c-string "clientWindow") (sb-alien:unsigned-long window)
                        (sb-alien:c-string "focusWindow") (sb-alien:unsigned-long window) ((* t) nil))))
         (if (sb-alien:null-alien pointer) 0
             (let ((context (make-x11-input-context :pointer pointer :display *x11-display*))
                   (success nil))
               ;; PreeditNothing/StatusNothing let the XIM server provide its
               ;; own composition UI; committed Unicode is delivered below.
               (unwind-protect
                    (progn
                      (trivial-garbage:finalize this (lambda () (%close-x11-input-context context)))
                      (setf (gethash this *x11-input-contexts*) context
                            (slot-value this '|pData|) (%x11-address pointer)
                            success t)
                      1)
                 (unless success
                   (remhash this *x11-input-contexts*)
                   (%close-x11-input-context context))))))))))

(%define-x11-awt-native "sun/awt/X11/XInputMethod.setXICFocusNative(JZZ)V" (this window focused active)
  (declare (ignore active))
  (when-let ((context (gethash this *x11-input-contexts*)))
    (bt:with-recursive-lock-held (*x11-lock*)
      (let ((pointer (x11-input-context-pointer context)))
        (when pointer
          (if (zerop focused)
              (progn
                (%libc "XUnsetICFocus" sb-alien:void ((* t) pointer))
                (when (eq this (%x11-current-input-method)) (setf *x11-active-input-method* nil)))
              (let ((error (%libc "XSetICValues" (* t) ((* t) pointer)
                              (sb-alien:c-string "focusWindow") (sb-alien:unsigned-long window) ((* t) nil))))
                (unless (sb-alien:null-alien error)
                  (%throw '|java/awt/AWTError| "Cannot set X input-method focus window"))
                (%libc "XSetICFocus" sb-alien:void ((* t) pointer))
                (setf *x11-active-input-method* (sb-ext:make-weak-pointer this)))))))) nil)

(%define-x11-awt-native "sun/awt/X11InputMethodBase.disposeXIC()V" (this)
  (trivial-garbage:cancel-finalization this)
  (when-let ((context (gethash this *x11-input-contexts*)))
    (%close-x11-input-context context)
    (remhash this *x11-input-contexts*))
  (when (eq this (%x11-current-input-method)) (setf *x11-active-input-method* nil))
  (setf (slot-value this '|pData|) 0) nil)

(%define-x11-awt-native "sun/awt/X11InputMethodBase.turnoffStatusWindow()V" (this)
  ;; This backend creates no private status window; the XIM server owns it.
  ;; Disabling the IC's focus hides the server-managed preedit/status UI.
  (when-let ((context (gethash this *x11-input-contexts*)))
    (when (x11-input-context-pointer context)
      (%libc "XUnsetICFocus" sb-alien:void ((* t) (x11-input-context-pointer context))))) nil)
(%define-x11-awt-native "sun/awt/X11/XInputMethod.adjustStatusWindow(J)V" (this window)
  ;; There is no application-owned status window for this negotiated style.
  (declare (ignore this window)) nil)

(%define-x11-awt-native "sun/awt/X11/XWindow.haveCurrentX11InputMethodInstance()Z" (this)
  (declare (ignore this))
  (if (%x11-current-input-method) 1 0))

(defun %x11-lookup-input-string (context event)
  "Return XIM's UTF-8 result, keysym, and status, retrying buffer overflow."
  (loop with size = 512
        do (let ((buffer (sb-alien:make-alien sb-alien:unsigned-char size)))
             (unwind-protect
                  (sb-alien:with-alien ((symbol sb-alien:unsigned-long 0) (status sb-alien:int 0))
                    (let ((count (%libc "Xutf8LookupString" sb-alien:int
                                   ((* t) (x11-input-context-pointer context)) ((* t) (%x11-pointer event))
                                   ((* sb-alien:unsigned-char) buffer) (sb-alien:int size)
                                   ((* sb-alien:unsigned-long) (sb-alien:addr symbol))
                                   ((* sb-alien:int) (sb-alien:addr status)))))
                      (if (= status -1)
                          (setf size (max (1+ size) count))
                          (let ((bytes (make-array (max 0 count) :element-type '(unsigned-byte 8))))
                            (dotimes (i (length bytes)) (setf (aref bytes i) (sb-alien:deref buffer i)))
                            (return-from %x11-lookup-input-string
                              (values (sb-ext:octets-to-string bytes :external-format :utf-8) symbol status))))))
               (sb-alien:free-alien buffer)))))

(%define-x11-awt-native "sun/awt/X11/XWindow.x11inputMethodLookupString(J[J)Z" (this event symbols)
  (declare (ignore this))
  (let* ((owner (%x11-current-input-method))
         (context (and owner (gethash owner *x11-input-contexts*)))
         (data (java-array-data symbols)))
    (unless (>= (length data) 2) (%throw '|java/lang/ArrayIndexOutOfBoundsException| "Keysym result requires two elements"))
    (setf (aref data 0) 0 (aref data 1) 0)
    (if (null context) 0
        (multiple-value-bind (text symbol status) (%x11-lookup-input-string context event)
          (classload "sun/awt/X11/XKeyEvent")
          (let ((key-event (%make-java-instance "sun/awt/X11/XKeyEvent")))
            (|<init>(J)V| key-event event)
            (cond
              ((and (= status 4) (not (x11-input-context-composing context))
                    (not (zerop (%x11-java-instance key-event "get_keycode()I"))))
               (setf (aref data 0) symbol) 0)
              ((member status '(2 4))
               (setf (x11-input-context-composing context) nil)
               (unless (zerop (length text))
                 (%x11-java-instance owner "dispatchCommittedText(Ljava/lang/String;J)V"
                                     (jstring text) (%x11-java-instance key-event "get_time()J")))
               1)
              ((= status 3)
               (when (= symbol #xff20) (setf (x11-input-context-composing context) t))
               (if (x11-input-context-composing context) 1 (progn (setf (aref data 0) symbol) 0)))
              (t 1)))))))
