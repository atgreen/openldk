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

;;; Build ./abcl, an ABCL image whose Java code OpenLDK has already
;;; compiled.  Run through `make abcl`, which downloads abcl.jar and
;;; compiles ABCLMain.java first.  See docs/IMAGES.md.
(in-package :openldk)

;; ABCL's compiled Lisp classes get their static fields from its fasl
;; loader, not their own <clinit>s, so resetting them would break the image.
;; Keep the booted Lisp instead (:reset-classes nil); ABCLMain reuses the
;; interpreter it finds.  The warmup works in a scratch package it deletes
;; at the end, so its definitions don't end up in the image.
;;
;; One REPL session fed through stdin.  Each form pulls in code the image
;; should not have to compile at runtime: the reader and printer, function
;; definition and calls, CLOS, the condition system, FORMAT, LOOP, hash
;; tables, strings, ABCL's compiler, closures included, and starting a
;; child process (the :ls command, through ProcessBuilder).  Its last form
;; resets the prompt number, which the REPL then advances to 1, so the
;; image's first prompt is CL-USER(1).
(defparameter *abcl-warmup-input* "
(defpackage :warmup (:use :cl))
(in-package :warmup)
(defun fact (n) (if (< n 2) 1 (* n (fact (1- n)))))
(print (fact 30))
(defclass point () ((x :initarg :x :reader px) (y :initarg :y :reader py)))
(defmethod norm2 ((p point)) (+ (* (px p) (px p)) (* (py p) (py p))))
(defmethod print-object ((p point) s) (format s \"#<POINT ~a ~a>\" (px p) (py p)))
(print (list (make-instance 'point :x 3 :y 4) (norm2 (make-instance 'point :x 3 :y 4))))
(print (handler-case (error \"boom ~a\" 42) (error (e) (format nil \"caught: ~a\" e))))
(print (mapcar (lambda (x) (* x x)) '(1 2 3 4)))
(print (format nil \"~{~a~^, ~} ~r ~:d ~,3f\" '(a b c) 42 1234567 3.14159))
(print (loop for i from 1 to 10 when (evenp i) collect i into evens finally (return evens)))
(let ((h (make-hash-table :test 'equal)))
  (setf (gethash \"one\" h) 1 (gethash \"two\" h) 2)
  (print (sort (loop for k being the hash-keys of h collect k) #'string<)))
(print (string-upcase (concatenate 'string \"hello\" \", \" \"world\")))
(print (with-output-to-string (s) (write 1/3 :stream s)))
(compile 'fact)
(print (fact 20))
(defun make-adder (n) (lambda (x) (+ x n)))
(compile 'make-adder)
(print (funcall (make-adder 3) 4))
(print (funcall (compile nil '(lambda (x) (* x 2))) 21))
(print (java:jstatic \"getProperty\" \"java.lang.System\" \"java.version\"))
(in-package :cl-user)
(delete-package :warmup)
:ls
(setf top-level::*cmd-number* 0)
(quit)
")

(dump-warm-image "abcl" "org.armedbear.lisp.ABCLMain"
                 :classpath "lib/abcl/abcl.jar:lib/abcl"
                 :warmups (list (list :input *abcl-warmup-input*))
                 :timeout 1800
                 :reset-classes nil)
