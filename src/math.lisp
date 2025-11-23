;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: OPENLDK; Base: 10 -*-
;;;
;;; Copyright (C) 2025  Anthony Green <green@moxielogic.com>
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

;;; Macro to eliminate code duplication between Math and StrictMath
(defmacro define-math-functions ((name params) docstring &body body)
  "Define both Math and StrictMath versions of a function."
  `(progn
     (defun ,(intern (format nil "java/lang/Math.~A" name) :openldk) ,params
       ,docstring
       ,@body)
     (defun ,(intern (format nil "java/lang/StrictMath.~A" name) :openldk) ,params
       ,docstring
       ,@body)))

(define-math-functions ("abs(I)" (a))
  "Returns the absolute value of an integer."
  (if (eq a -2147483648)
      a
      (abs a)))

(define-math-functions ("abs(J)" (a))
  "Returns the absolute value of a long integer."
  (if (eq a -9223372036854775808)
      a
      (abs a)))

(define-math-functions ("abs(F)" (a))
  "Returns the absolute value of a float."
  (float-features:bits-single-float
   (logand #x7FFFFFFF (float-features:single-float-bits a))))

(define-math-functions ("abs(D)" (a))
  "Returns the absolute value of a double."
  (float-features:bits-double-float
   (logand #x7FFFFFFFFFFFFFFF (float-features:double-float-bits a))))

(define-math-functions ("acos(D)" (a))
  "Returns the arc cosine of a value, in radians."
  (acos a))

(define-math-functions ("asin(D)" (a))
  "Returns the arc sine of a value, in radians."
  (asin a))

(define-math-functions ("atan(D)" (a))
  "Returns the arc tangent of a value, in radians."
  (atan a))

(define-math-functions ("atan2(DD)" (a b))
  "Returns the angle theta from the conversion of rectangular coordinates (a, b) to polar coordinates."
  (atan a b))

(define-math-functions ("cbrt(D)" (a))
  "Returns the cube root of a value."
  (if (< a 0.0d0)
      (- (expt (- a) (/ 1.0d0 3.0d0)))
      (expt a (/ 1.0d0 3.0d0))))

(define-math-functions ("ceil(D)" (a))
  "Returns the smallest integer greater than or equal to the given value."
  (ceiling a))

(define-math-functions ("cos(D)" (a))
  "Returns the cosine of an angle in radians."
  (cos a))

(define-math-functions ("cosh(D)" (a))
  "Returns the hyperbolic cosine of a value."
  (cosh a))

(define-math-functions ("exp(D)" (a))
  "Returns Euler's number e raised to the power of a value."
  (exp a))

(define-math-functions ("expm1(D)" (a))
  "Returns e^a - 1."
  (if (< (abs a) 1.0d-5)
      ;; Use Taylor series for small values to preserve precision
      ;; expm1(x) = x + x²/2! + x³/3! + x⁴/4! + ...
      (let* ((x a)
             (term x)
             (sum x)
             (n 2))
        (loop repeat 15
              do (setf term (* term (/ x n)))
                 (setf sum (+ sum term))
                 (incf n)
              while (> (abs term) (* (abs sum) 1.0d-16)))
        sum)
      ;; Use regular formula for larger values
      (- (exp a) 1.0d0)))

(define-math-functions ("floor(D)" (a))
  "Returns the largest integer less than or equal to the given value."
  (floor a))

(define-math-functions ("hypot(DD)" (a b))
  "Returns sqrt(a^2 + b^2) without intermediate overflow or underflow."
  (let ((abs-a (abs a))
        (abs-b (abs b)))
    (cond
      ((> abs-a abs-b)
       (if (zerop abs-a)
           0.0d0
           (* abs-a (sqrt (+ 1.0d0 (expt (/ abs-b abs-a) 2))))))
      ((zerop abs-b)
       0.0d0)
      (t
       (* abs-b (sqrt (+ 1.0d0 (expt (/ abs-a abs-b) 2))))))))

(define-math-functions ("IEEEremainder(DD)" (a b))
  "Computes the remainder operation as defined by IEEE 754."
  (- a (* b (fround (/ a b)))))

(define-math-functions ("log(D)" (a))
  "Returns the natural logarithm (base e) of a value."
  (log a))

(define-math-functions ("log10(D)" (a))
  "Returns the base 10 logarithm of a value."
  (log a 10))

(define-math-functions ("log1p(D)" (a))
  "Returns the natural logarithm of (1 + a)."
  (if (< (abs a) 0.5d0)
      ;; Use Taylor series for small values to preserve precision
      ;; log1p(x) = x - x²/2 + x³/3 - x⁴/4 + ...
      (let* ((x a)
             (x-power x)
             (sum x)
             (n 2))
        (loop repeat 30
              do (setf x-power (* x-power x))
                 (let ((term (/ x-power n)))
                   (if (evenp n)
                       (setf sum (- sum term))
                       (setf sum (+ sum term)))
                   (incf n)
                   (when (< (abs term) (* (abs sum) 1.0d-16))
                     (return))))
        sum)
      ;; Use regular formula for larger values
      (log (+ a 1.0d0))))

(define-math-functions ("max(II)" (a b))
  "Returns the greater of two integers."
  (max a b))

(define-math-functions ("max(JJ)" (a b))
  "Returns the greater of two long integers."
  (max a b))

(define-math-functions ("max(FF)" (a b))
  "Returns the greater of two floats."
  (max a b))

(define-math-functions ("max(DD)" (a b))
  "Returns the greater of two doubles."
  (max a b))

(define-math-functions ("min(II)" (a b))
  "Returns the smaller of two integers."
  (min a b))

(define-math-functions ("min(JJ)" (a b))
  "Returns the smaller of two long integers."
  (min a b))

(define-math-functions ("min(FF)" (a b))
  "Returns the smaller of two floats."
  (min a b))

(define-math-functions ("min(DD)" (a b))
  "Returns the smaller of two doubles."
  (min a b))

(define-math-functions ("pow(DD)" (a b))
  "Returns a raised to the power of b."
  (expt a b))

(define-math-functions ("random()" ())
  "Returns a pseudo-random number between 0.0 and 1.0."
  (random 1.0d0))

(define-math-functions ("rint(D)" (a))
  "Returns the integer value that is closest to the argument."
  (fround a))

(define-math-functions ("round(F)" (a))
  "Returns the closest integer to the given float."
  (round a))

(define-math-functions ("round(D)" (a))
  "Returns the closest integer to the given double."
  (round a))

(define-math-functions ("signum(F)" (a))
  "Returns the signum function of a float: -1 if negative, 1 if positive, 0 if zero."
  (cond ((float-features:float-nan-p a) a)
        ((> a 0) 1.0)
        ((< a 0) -1.0)
        (t 0.0)))

(define-math-functions ("signum(D)" (a))
  "Returns the signum function of a double: -1 if negative, 1 if positive, 0 if zero."
  (cond ((float-features:float-nan-p a) a)
        ((> a 0) 1.0d0)
        ((< a 0) -1.0d0)
        (t 0.0d0)))

(define-math-functions ("sin(D)" (a))
  "Returns the sine of an angle in radians."
  (sin a))

(define-math-functions ("sinh(D)" (a))
  "Returns the hyperbolic sine of a value."
  (sinh a))

(define-math-functions ("sqrt(D)" (a))
  "Returns the square root of a value."
  (sqrt a))

(define-math-functions ("tan(D)" (a))
  "Returns the tangent of an angle in radians."
  (tan a))

(define-math-functions ("tanh(D)" (a))
  "Returns the hyperbolic tangent of a value."
  (tanh a))

(define-math-functions ("toDegrees(D)" (a))
  "Converts an angle from radians to degrees."
  (* a (/ 180.0d0 pi)))

(define-math-functions ("toRadians(D)" (a))
  "Converts an angle from degrees to radians."
  (* a (/ pi 180.0d0)))

(define-math-functions ("ulp(F)" (a))
  "Returns the unit in the last place (ULP) of a float."
  (float-epsilon a))

(define-math-functions ("ulp(D)" (a))
  "Returns the unit in the last place (ULP) of a double."
  (float-epsilon a))

