;;; SPDX-License-Identifier: GPL-3.0-or-later WITH Classpath-exception-2.0
;;; Exact, deliberately slow test oracle. Never loaded into production images.
(in-package :openldk)

(defun %remainder-reference (dividend divisor)
  (cond
    ((or (float-features:float-nan-p dividend)
         (float-features:float-nan-p divisor)
         (float-features:float-infinity-p dividend)
         (zerop divisor))
     (if (typep dividend 'single-float)
         float-features:single-float-nan float-features:double-float-nan))
    ((or (float-features:float-infinity-p divisor) (zerop dividend)) dividend)
    (t (let ((result (float (rem (rational dividend) (rational divisor)) dividend)))
         (if (zerop result) (float-sign dividend result) result)))))

(defun %remainder-test-bits (value)
  (if (typep value 'single-float)
      (if (float-features:float-nan-p value) #x7fc00000
          (logand (sb-kernel:single-float-bits value) #xffffffff))
      (if (float-features:float-nan-p value) #x7ff8000000000000
          (float-features:double-float-bits value))))

(defun %check-remainder-functions (&optional (random-count 100000)
                                           (single-function #'%java-frem)
                                           (double-function #'%java-drem))
  "Check deterministic boundary matrices and raw-bit random pairs against exact arithmetic."
  (let ((state #x123456789abcdef)
        (count 0))
    (labels ((next-bits ()
               (setf state (logand #xffffffffffffffff (logxor state (ash state 13))))
               (setf state (logxor state (ash state -7)))
               (setf state (logand #xffffffffffffffff (logxor state (ash state 17)))))
             (check-pair (function x y)
               (let ((actual (funcall function x y))
                     (expected (%remainder-reference x y)))
                 (unless (= (%remainder-test-bits actual) (%remainder-test-bits expected))
                   (error "Remainder ~X % ~X: got ~X, expected ~X"
                          (%remainder-test-bits x) (%remainder-test-bits y)
                          (%remainder-test-bits actual) (%remainder-test-bits expected)))
                 (incf count))))
      (dolist (single '(t nil))
        (let* ((edges (if single
                          '(0 #x80000000 1 2 3 #x7fffff #x800000 #x800001 #x3f000000
                            #x3f800000 #x40000000 #x40e00000 #x7f7fffff #xff7fffff
                            #xbf800000 #xc0800000 #x807fffff #x80800000 #x7f800000
                            #xff800000 #x7fc00000 #x7f800001 #xffc12345)
                          '(0 #x8000000000000000 1 2 3 #xfffffffffffff #x10000000000000
                            #x10000000000001 #x3fe0000000000000 #x3ff0000000000000
                            #x4000000000000000 #x401c000000000000 #x7fefffffffffffff
                            #xffefffffffffffff #xbff0000000000000 #xc010000000000000
                            #x800fffffffffffff #x8010000000000000 #x7ff0000000000000
                            #xfff0000000000000 #x7ff8000000000000 #x7ff0000000000001
                            #xfff8123456789abc)))
               (decode (if single #'float-features:bits-single-float #'float-features:bits-double-float))
               (function (if single single-function double-function)))
          (dolist (x edges)
            (dolist (y edges)
              (check-pair function (funcall decode x) (funcall decode y))))
          (dotimes (i random-count)
            (let ((x (next-bits)) (y (next-bits)))
              (check-pair function
                          (funcall decode (if single (logand x #xffffffff) x))
                          (funcall decode (if single (logand y #xffffffff) y)))))))
      count)))
