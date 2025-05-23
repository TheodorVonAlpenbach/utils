(in-package :numerics-utils)

;;; -*- Mode: LISP; Syntax: Common-lisp; Package: USER -*-

;;;; Gamma Function

;;; Reference
;;;   M. Abramowitz and I. Stegun, eds,
;;;     Handbook of Mathematical Functions,
;;;     National Bureau of Standards, 1964
;;;   William H. Press, Brian P. Flannery, Saul A. Teukolsky, William T. Vetterling
;;;     Numerical Recipes in C, The Art of Scientific Computing
;;;     Cambridge University Press, 1988
;;;     cf 178

;;;  (c) Copyright Gerald Roylance 1982, 1984, 1985, 1989
;;;      All Rights Reserved.
;;;  This file may be distributed noncommercially provided
;;;  that this notice is not removed.

;;; Bugs and fixes
;;;   gamma-incomplete chokes on large x

;;(in-package "CLMATH")


;;;; NBS Approximation to the Gamma Function

;;; NBS 6.1.15 Recurrence Formula
;;;    (gamma 1+z) = z (gamma z)
;;;
;;; NBS 6.1.17 Reflection Formula
;;;    (gamma 1-z) (gamma z) = -z (gamma -z) (gamma z) = pi csc(pi z)
;;;   therefore
;;;    (gamma z) = (pi csc(pi z)) / (-z (gamma -z))

;;; NBS 6.1.35
;;;
(defun gamma-function-nbs (a)
  (let ((x (- a 1.0)))
    (declare (float x))
    (cond ((< x 0.0) (/ (gamma-function-nbs (1+ a)) a))
	  ((> x 1.0) (* x (gamma-function-nbs x)))
	  (t  (poly x
		    1.000000000 -.577191652
		    0.988205891 -.897056937
		    0.918206857 -.756704078
		    0.482199394 -.193527818
		    0.035868343)		;eps < 3e-7
	      ))))

;;; (POLY X
;;;       1.0000000 -.5748646
;;;       0.9512363 -.6998588
;;;       0.4245549 -.1010678)		;eps < 5e-5

#|+ignore
(- (gamma-function-nbs 1.6) 0.8935153493)
|#

;;;; Stirling's Approximation to the Gamma Function

;;; NBS 6.1.37, Stirling's Formula, an approximation to the gamma-function
;;;
;;; complex z, z->inf, |arg z|<pi

(defun gamma-stirling (z)
  (let ((zi (/ z)))
    (declare (float zi))
    (* (exp (- z))
       (expt z (- z 0.5))
       (sqrt (* 2.0 (float pi z)))
       (poly zi
	     1.0
	     (/    1.0      12.0)
	     (/    1.0     288.0)
	     (/ -139.0   51840.0)
	     (/ -571.0 2488320.0)
	     ;; ...
	     ))))


;;;; Lancoz Approximation to the Gamma Function

;;; from Press...

;;; Lancoz approximation for Re[z] > 0
;;; Gamma(z+1) = (z+gamma+0.5)^{z + 0.5} exp{-(z+gamma+0.5)}
;;;              * sqrt{2 \pi}[c_0 + {c_1 \over z+1} + ... + {c_N \over z+N} + eps]
;;;
;;; for gamma=5, N=6, and a certain set of c_i, abs(eps)< 2.0E-10
;;;  (even for complex arguments!)

(defun gammln (xx)
  (if (< (realpart xx) 1.0)
      (let ((z (- 1.0 xx)))
	(- (log (/ (* pi z) (sin (* pi z))))
	   (gammln (+ 1.0 z))))
      (let* ((z   (- xx 1.0))
	     (tmp (+ z 5.0 0.5)))
	(+ (* (log tmp) (+ z 0.5))
	   (- tmp)
	   (log (sqrt (* 2 pi)))
	   (log (+ 1.0
		   (/  76.18009173d0   (+ z 1.0d0))
		   (/ -86.50532033d0   (+ z 2.0d0))
		   (/  24.01409822d0   (+ z 3.0d0))
		   (/  -1.231739516d0  (+ z 4.0d0))
		   (/   0.120858003d-2 (+ z 5.0d0))
		   (/  -0.536382d-5    (+ z 6.0d0))))))))

(defun fgammln (xx)
  (declare (type single-float xx))
  (if (< (realpart xx) 1.0)
      (let ((z (- 1.0 xx)))
	(- (log (/ (* (float pi 1.0) z)
		   (sin (* (float pi 1.0) z))))	; ***
	   (fgammln (+ 1.0 z))))
      (let* ((z   (- xx 1.0))
	     (tmp (+ z 5.0 0.5)))
	(+ (* (log tmp) (+ z 0.5))
	   (- tmp)
	   (float (log (sqrt (* 2 pi))) 1.0)
	   (log (+ 1.0
		   (/  76.18009173e0   (+ z 1.0))
		   (/ -86.50532033e0   (+ z 2.0))
		   (/  24.01409822e0   (+ z 3.0))
		   (/  -1.231739516e0  (+ z 4.0))
		   (/   0.120858003e-2 (+ z 5.0))
		   (/  -0.536382e-5    (+ z 6.0))))))))

;;;; Gamma Function and Log Gamma Function

(defun gamma-function (a)
  (typecase a
    (integer      (exp (fgammln (float a))))
    (single-float (exp (fgammln        a )))
    (otherwise    (exp  (gammln        a )))))

(defun gamma (a) (gamma-function a))

(defun log-gamma-function (a)
  (typecase a
    (integer            (fgammln (float a)))
    (single-float       (fgammln        a ))
    (otherwise           (gammln        a ))))

(proclaim '(ftype (function (float) float) gamma-function-reciprocal))

(defun gamma-function-reciprocal (z)
  (cond ((< z -1.0) (* z (gamma-function-reciprocal (1+ z))))
	((> z  1.0) (/   (gamma-function-reciprocal (1- z)) (1- z)))
	(t
	 (poly z
	       +0.0				; 0 -- Numbers have not been checked
	       +1.0000000000000000
	       +0.5772156649015329
	       -0.6558780715202538
	       -0.0420026350340952
	       +0.1665386113822915		; 5
	       -0.0421977345555443
	       -0.0096219715278770
	       +0.0072189432466630
	       -0.0011651675918591
	       -0.0002152416741149		;10
	       +0.0001280502823882
	       -0.0000201348547807
	       -0.0000012504934821
	       +0.0000011330272320
	       -0.0000002056338417		;15
	       +0.0000000061160950
	       +0.0000000050020075
	       -0.0000000011812746
	       +0.0000000001043427
	       +0.0000000000077823		;20
	       -0.0000000000036968
	       +0.0000000000005100
	       -0.0000000000000206
	       -0.0000000000000054
	       +0.0000000000000014		;25
	       +0.0000000000000001		;26
	       ))))

;; http://rosettacode.org/wiki/Gamma_function
(defun upper-incomplete-gamma (a x)
  "Returns Integral(t**(a-1)*e**-t, t = x..inf)"
  (when (or (> a 171) (< x 0))
    (error "overflow"))
  (let ((xam (if (plusp x) (+ (- x) (* a (log x))) 0)))
    (when (> xam 700)
      (error "overflow"))

    (if (> x (1+ a))
      (loop for k from 60 downto 1
	    for t0 = 0 then (/ (- k a) (1+ (/ k (+ x t0))))
	    finally (return (safe-op #'/ (exp-safe xam) (+ x t0))))
    (let ((ga (gamma a)))
      (if (zerop x)
	ga
	;; else x <= 1+a 
      (loop for i from 1 to 60
	    for s = (/ 1 a) then (+ s r)
	    for r = s then (/ x (+ a i))
	    while (< (abs (/ r s)) 1E-15)
	    finally (return (- ga (* (exp-safe xam) s)))))))))
;;(upper-incomplete-gamma 0.5 5)

(defun lower-incomplete-gamma (a x)
  "Returns Integral(t**(a-1)*e**-t, t = 0..x)"
  (- (gamma a) (upper-incomplete-gamma a x)))
;;(upper-incomplete-gamma 0.5 5)

(defun incomplete-gamma (a x1 x2)
  "Returns Integral(t**(a-1)*e**-t, t = x1..x2)"
  (if (eql x2 :infinity)
    (if (zerop x1)
      (gamma a)
      (upper-incomplete-gamma a x1))
    (if (zerop x1)
      (lower-incomplete-gamma a x2)
      (- (upper-incomplete-gamma a x1)
	 (upper-incomplete-gamma a x2)))))
;;(incomplete-gamma pi 0 :infinity)
