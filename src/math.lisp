;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: OPENLDK; Base: 10 -*-
;;;
;;; Copyright (C) 2025  Anthony Green <green@moxielogic.com>
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

(defun |java/lang/Math.abs(I)| (a)
  "Returns the absolute value of an integer."
  (if (eq a -2147483648)
      a
      (abs a)))

(defun |java/lang/Math.abs(J)| (a)
  "Returns the absolute value of a long integer."
  (if (eq a -9223372036854775808)
      a
      (abs a)))

(defun |java/lang/Math.abs(F)| (a)
  "Returns the absolute value of a float."
  (float-features:bits-single-float
   (logand #x7FFFFFFF (float-features:single-float-bits a))))

(defun |java/lang/Math.abs(D)| (a)
  "Returns the absolute value of a double."
  (float-features:bits-double-float
   (logand #x7FFFFFFFFFFFFFFF (float-features:double-float-bits a))))

(defun |java/lang/Math.acos(D)| (a)
  "Returns the arc cosine of a value, in radians."
  (acos a))

(defun |java/lang/Math.asin(D)| (a)
  "Returns the arc sine of a value, in radians."
  (asin a))

(defun |java/lang/Math.atan(D)| (a)
  "Returns the arc tangent of a value, in radians."
  (atan a))

(defun |java/lang/Math.atan2(DD)| (a b)
  "Returns the angle theta from the conversion of rectangular coordinates (a, b) to polar coordinates."
  (atan a b))

(defun |java/lang/Math.cbrt(D)| (a)
  "Returns the cube root of a value."
  (expt a (/ 1 3)))

(defun |java/lang/Math.ceil(D)| (a)
  "Returns the smallest integer greater than or equal to the given value."
  (ceiling a))

(defun |java/lang/Math.cos(D)| (a)
  "Returns the cosine of an angle in radians."
  (cos a))

(defun |java/lang/Math.cosh(D)| (a)
  "Returns the hyperbolic cosine of a value."
  (cosh a))

(defun |java/lang/Math.exp(D)| (a)
  "Returns Euler's number e raised to the power of a value."
  (exp a))

(defun |java/lang/Math.expm1(D)| (a)
  "Returns e^a - 1."
  (- (exp a) 1))

(defun |java/lang/Math.floor(D)| (a)
  "Returns the largest integer less than or equal to the given value."
  (floor a))

(defun |java/lang/Math.hypot(DD)| (a b)
  "Returns sqrt(a^2 + b^2) without intermediate overflow or underflow."
  (sqrt (+ (* a a) (* b b))))

(defun |java/lang/Math.IEEEremainder(DD)| (a b)
  "Computes the remainder operation as defined by IEEE 754."
  (rem a b))

(defun |java/lang/Math.log(D)| (a)
  "Returns the natural logarithm (base e) of a value."
  (log a))

(defun |java/lang/Math.log10(D)| (a)
  "Returns the base 10 logarithm of a value."
  (log a 10))

(defun |java/lang/Math.log1p(D)| (a)
  "Returns the natural logarithm of (1 + a)."
  (log (+ a 1)))

(defun |java/lang/Math.max(II)| (a b)
  "Returns the greater of two integers."
  (max a b))

(defun |java/lang/Math.max(JJ)| (a b)
  "Returns the greater of two long integers."
  (max a b))

(defun |java/lang/Math.max(FF)| (a b)
  "Returns the greater of two floats."
  (max a b))

(defun |java/lang/Math.max(DD)| (a b)
  "Returns the greater of two doubles."
  (max a b))

(defun |java/lang/Math.min(II)| (a b)
  "Returns the smaller of two integers."
  (min a b))

(defun |java/lang/Math.min(JJ)| (a b)
  "Returns the smaller of two long integers."
  (min a b))

(defun |java/lang/Math.min(FF)| (a b)
  "Returns the smaller of two floats."
  (min a b))

(defun |java/lang/Math.min(DD)| (a b)
  "Returns the smaller of two doubles."
  (min a b))

(defun |java/lang/Math.pow(DD)| (a b)
  "Returns a raised to the power of b."
  (expt a b))

(defun |java/lang/Math.random()| ()
  "Returns a pseudo-random number between 0.0 and 1.0."
  (random 1.0))

(defun |java/lang/Math.rint(D)| (a)
  "Returns the integer value that is closest to the argument."
  (fround a))

(defun |java/lang/Math.round(F)| (a)
  "Returns the closest integer to the given float."
  (round a))

(defun |java/lang/Math.round(D)| (a)
  "Returns the closest integer to the given double."
  (round a))

(defun |java/lang/Math.signum(F)| (a)
  "Returns the signum function of a float: -1 if negative, 1 if positive, 0 if zero."
  (cond ((> a 0) 1.0)
        ((< a 0) -1.0)
        (t 0.0)))

(defun |java/lang/Math.signum(D)| (a)
  "Returns the signum function of a double: -1 if negative, 1 if positive, 0 if zero."
  (cond ((> a 0) 1.0)
        ((< a 0) -1.0)
        (t 0.0)))

(defun |java/lang/Math.sin(D)| (a)
  "Returns the sine of an angle in radians."
  (sin a))

(defun |java/lang/Math.sinh(D)| (a)
  "Returns the hyperbolic sine of a value."
  (sinh a))

(defun |java/lang/Math.sqrt(D)| (a)
  "Returns the square root of a value."
  (sqrt a))

(defun |java/lang/Math.tan(D)| (a)
  "Returns the tangent of an angle in radians."
  (tan a))

(defun |java/lang/Math.tanh(D)| (a)
  "Returns the hyperbolic tangent of a value."
  (tanh a))

(defun |java/lang/Math.toDegrees(D)| (a)
  "Converts an angle from radians to degrees."
  (* a (/ 180.0 cl-user::pi)))

(defun |java/lang/Math.toRadians(D)| (a)
  "Converts an angle from degrees to radians."
  (* a (/ cl-user::pi 180.0)))

(defun |java/lang/Math.ulp(F)| (a)
  "Returns the unit in the last place (ULP) of a float."
  (float-epsilon a))

(defun |java/lang/Math.ulp(D)| (a)
  "Returns the unit in the last place (ULP) of a double."
  (float-epsilon a))

(defun |java/lang/StrictMath.abs(I)| (a)
  "Returns the absolute value of an integer."
  (if (eq a -2147483648)
      a
      (abs a)))

(defun |java/lang/StrictMath.abs(J)| (a)
  "Returns the absolute value of a long integer."
  (if (eq a -9223372036854775808)
      a
      (abs a)))

(defun |java/lang/StrictMath.abs(F)| (a)
  "Returns the absolute value of a float."
  (float-features:bits-single-float
   (logand #x7FFFFFFF (float-features:single-float-bits a))))

(defun |java/lang/StrictMath.abs(D)| (a)
  "Returns the absolute value of a double."
  (float-features:bits-double-float
   (logand #x7FFFFFFFFFFFFFFF (float-features:double-float-bits a))))

(defun |java/lang/StrictMath.acos(D)| (a)
  "Returns the arc cosine of a value, in radians."
  (acos a))

(defun |java/lang/StrictMath.asin(D)| (a)
  "Returns the arc sine of a value, in radians."
  (asin a))

(defun |java/lang/StrictMath.atan(D)| (a)
  "Returns the arc tangent of a value, in radians."
  (atan a))

(defun |java/lang/StrictMath.atan2(DD)| (a b)
  "Returns the angle theta from the conversion of rectangular coordinates (a, b) to polar coordinates."
  (atan a b))

(defun |java/lang/StrictMath.cbrt(D)| (a)
  "Returns the cube root of a value."
  (expt a (/ 1 3)))

(defun |java/lang/StrictMath.ceil(D)| (a)
  "Returns the smallest integer greater than or equal to the given value."
  (ceiling a))

(defun |java/lang/StrictMath.cos(D)| (a)
  "Returns the cosine of an angle in radians."
  (cos a))

(defun |java/lang/StrictMath.cosh(D)| (a)
  "Returns the hyperbolic cosine of a value."
  (cosh a))

(defun |java/lang/StrictMath.exp(D)| (a)
  "Returns Euler's number e raised to the power of a value."
  (exp a))

(defun |java/lang/StrictMath.expm1(D)| (a)
  "Returns e^a - 1."
  (- (exp a) 1))

(defun |java/lang/StrictMath.floor(D)| (a)
  "Returns the largest integer less than or equal to the given value."
  (floor a))

(defun |java/lang/StrictMath.hypot(DD)| (a b)
  "Returns sqrt(a^2 + b^2) without intermediate overflow or underflow."
  (sqrt (+ (* a a) (* b b))))

(defun |java/lang/StrictMath.IEEEremainder(DD)| (a b)
  "Computes the remainder operation as defined by IEEE 754."
  (rem a b))

(defun |java/lang/StrictMath.log(D)| (a)
  "Returns the natural logarithm (base e) of a value."
  (log a))

(defun |java/lang/StrictMath.log10(D)| (a)
  "Returns the base 10 logarithm of a value."
  (log a 10))

(defun |java/lang/StrictMath.log1p(D)| (a)
  "Returns the natural logarithm of (1 + a)."
  (log (+ a 1)))

(defun |java/lang/StrictMath.max(II)| (a b)
  "Returns the greater of two integers."
  (max a b))

(defun |java/lang/StrictMath.max(JJ)| (a b)
  "Returns the greater of two long integers."
  (max a b))

(defun |java/lang/StrictMath.max(FF)| (a b)
  "Returns the greater of two floats."
  (max a b))

(defun |java/lang/StrictMath.max(DD)| (a b)
  "Returns the greater of two doubles."
  (max a b))

(defun |java/lang/StrictMath.min(II)| (a b)
  "Returns the smaller of two integers."
  (min a b))

(defun |java/lang/StrictMath.min(JJ)| (a b)
  "Returns the smaller of two long integers."
  (min a b))

(defun |java/lang/StrictMath.min(FF)| (a b)
  "Returns the smaller of two floats."
  (min a b))

(defun |java/lang/StrictMath.min(DD)| (a b)
  "Returns the smaller of two doubles."
  (min a b))

(defun |java/lang/StrictMath.pow(DD)| (a b)
  "Returns a raised to the power of b."
  (expt a b))

(defun |java/lang/StrictMath.random()| ()
  "Returns a pseudo-random number between 0.0 and 1.0."
  (random 1.0))

(defun |java/lang/StrictMath.rint(D)| (a)
  "Returns the integer value that is closest to the argument."
  (fround a))

(defun |java/lang/StrictMath.round(F)| (a)
  "Returns the closest integer to the given float."
  (round a))

(defun |java/lang/StrictMath.round(D)| (a)
  "Returns the closest integer to the given double."
  (round a))

(defun |java/lang/StrictMath.signum(F)| (a)
  "Returns the signum function of a float: -1 if negative, 1 if positive, 0 if zero."
  (cond ((> a 0) 1.0)
        ((< a 0) -1.0)
        (t 0.0)))

(defun |java/lang/StrictMath.signum(D)| (a)
  "Returns the signum function of a double: -1 if negative, 1 if positive, 0 if zero."
  (cond ((> a 0) 1.0)
        ((< a 0) -1.0)
        (t 0.0)))

(defun |java/lang/StrictMath.sin(D)| (a)
  "Returns the sine of an angle in radians."
  (sin a))

(defun |java/lang/StrictMath.sinh(D)| (a)
  "Returns the hyperbolic sine of a value."
  (sinh a))

(defun |java/lang/StrictMath.sqrt(D)| (a)
  "Returns the square root of a value."
  (sqrt a))

(defun |java/lang/StrictMath.tan(D)| (a)
  "Returns the tangent of an angle in radians."
  (tan a))

(defun |java/lang/StrictMath.tanh(D)| (a)
  "Returns the hyperbolic tangent of a value."
  (tanh a))

(defun |java/lang/StrictMath.toDegrees(D)| (a)
  "Converts an angle from radians to degrees."
  (* a (/ 180.0 cl-user::pi)))

(defun |java/lang/StrictMath.toRadians(D)| (a)
  "Converts an angle from degrees to radians."
  (* a (/ cl-user::pi 180.0)))

(defun |java/lang/StrictMath.ulp(F)| (a)
  "Returns the unit in the last place (ULP) of a float."
  (float-epsilon a))

(defun |java/lang/StrictMath.ulp(D)| (a)
  "Returns the unit in the last place (ULP) of a double."
  (float-epsilon a))
