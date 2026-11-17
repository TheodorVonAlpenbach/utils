;;; mb-math-functions.el --- Additional mathematical functions -*- lexical-binding: t; -*-

;;; Commentary:

;; Trigonometric, special-function, series, polynomial, and numerical
;; root-finding helpers.

;;; Code:

(require 'cl-lib)

(defun mb-math--horner (coefficients x)
  "Evaluate COEFFICIENTS at X using Horner's method."
  (cl-loop for coefficient in (reverse coefficients)
           for sum = coefficient then (+ (* sum x) coefficient)
           finally return sum))

(defun sec (x)
  "Return the secant of X radians."
  (/ 1.0 (cos x)))

(defun scs (x)
  "Return the cosecant of X radians."
  (/ 1.0 (sin x)))

(defun asec (x)
  "Return the inverse secant of X in radians."
  (acos (/ 1.0 x)))

(defun ascs (x)
  "Return the inverse cosecant of X in radians."
  (asin (/ 1.0 x)))

(defun sinh (x)
  "Return the hyperbolic sine of X."
  (/ (- (exp x) (exp (- x))) 2.0))

(defun cosh (x)
  "Return the hyperbolic cosine of X."
  (/ (+ (exp x) (exp (- x))) 2.0))

(defun tanh (x)
  "Return the hyperbolic tangent of X."
  (let ((y (exp (* 2 x))))
    (/ (- y 1) (+ y 1))))

(defmacro mb-math--define-degree-trigonometry (fns)
  "Define degree-based variants of trigonometric functions FNS."
  `(progn
     ,@(cl-loop
        for fn in fns
        append
        (let ((name (symbol-name fn)))
          `((defun ,(intern (concat name "d")) (degrees)
              ,(format "Apply `%s' to DEGREES." fn)
              (,fn (* degrees-to-radians degrees)))
            (defun ,(intern (concat "a" name "d")) (ratio)
              ,(format "Apply inverse `%s' to RATIO and return degrees." fn)
              (* radians-to-degrees
                 (,(intern (concat "a" name)) ratio))))))))
(mb-math--define-degree-trigonometry (cos sin tan sec scs))
;;(cl-loop for fn in '(sind cosd tand) collect (funcall fn 45))
;;(list (asind 0.5) (acosd 0.5) (atand 1))

(cl-defun polylogarithm-abs-lt-1 (x &optional (n 2) (num-iterations 50))
  "Approximate the order N polylogarithm of X.
Require the absolute value of X to be less than one and sum terms with
indices below NUM-ITERATIONS."
  (unless (< (abs x) 1)
    (error "Absolute value of X must be less than one: %S" x))
  (unless (and (integerp num-iterations) (> num-iterations 1))
    (error "Iteration count must be an integer greater than one: %S"
           num-iterations))
  (cl-loop for i from 1 below num-iterations
	   sum (/ (expt x i) (expt i n))))

(cl-defun dilog-lt-minus-1 (x &optional (num-iterations 50))
  "Approximate the dilogarithm of X below -1.
Use NUM-ITERATIONS terms in the transformed power series."
  (when (>= x -1)
    (error "X must be less than -1: %S" x))
  (let ((y (/ 1 (- 1.0 x))))
    (- 0
       (polylogarithm-abs-lt-1 (- 1 y) 2 num-iterations)
       (/ (expt (log y) 2) 2))))
;;(dilog-lt-minus-1 -2)

(cl-defun dilog-gt-1 (x &optional (num-iterations 50))
  "Approximate the real dilogarithm of X above one.
Use NUM-ITERATIONS terms in the transformed power series."
  (when (<= x 1)
    (error "X must be greater than one: %S" x))
  (- (dilog 1 num-iterations)
    (* (log x) (log (- x 1)))
     (dilog (- 1 x) num-iterations)))
;;(dilog-gt-1 3 60)

(cl-defun dilog (x &optional (num-iterations 50))
  "Approximate the real dilogarithm of X.
Use NUM-ITERATIONS terms in power-series branches."
  (cond
   ((= x 0) 0)
   ((= x -1) (- (/ (expt float-pi 2) 12)))
   ((= x 1) (/ (expt float-pi 2) 6))
   ((< (abs x) 1) (polylogarithm-abs-lt-1 x 2 num-iterations))
   ((< x -1) (dilog-lt-minus-1 x num-iterations))
   ((> x 1) (dilog-gt-1 x num-iterations))))
;;(dilog 7)

(cl-defun atan2 (x y)
  "Return the quadrant-aware arctangent for coordinates X and Y.
Return `indeterminate' when both coordinates are zero."
  (if (and (zerop x) (zerop y))
      'indeterminate
    (atan y x)))
;;(atan2 -1 0)

;;; erf function
(defconst mb-math--erf-tau-polynomial
  '(-1.26551223 1.00002368 0.37409196 0.09678418
    -0.18628806 0.27886807 -1.13520398 1.48851587
    -0.82215223 0.17087277)
  "Coefficients for the error-function approximation.")

(defun mb-math--erf-tau-1 (x s)
  "Return the error-function approximation term for X and S."
  (* s (exp (- (mb-math--horner mb-math--erf-tau-polynomial s)
               (expt x 2)))))

(defun mb-math--erf-tau (x)
  "Return the error-function approximation term for X."
  (mb-math--erf-tau-1 x (/ 1 (+ 1 (* 0.5 (abs x))))))

(cl-defun erf (x)
  "Calculate the erf function for X.
See https://en.wikipedia.org/wiki/Error_function#Numerical_approximations
for numerical implementation basis."
  (if (< x 0)
    (- (mb-math--erf-tau x) 1)
    (- 1 (mb-math--erf-tau x))))
;;(cl-loop for x from -3 to 3 by 0.5 collect (erf x))
;;(cl-loop for x in '(0.8) collect (erf x))

(cl-defun erfc (x)
  "Calculate the complementary error function for X."
  (- 1 (erf x)))

(cl-defun inverse-erf (x)
  "Approximate the inverse error function of X.
X must be strictly between -1 and 1."
  (unless (and (numberp x) (< -1 x 1))
    (error "X must be a number strictly between -1 and 1: %S" x))
  (let* ((a (/ (* 8 (- float-pi 3))
	       (* 3 float-pi (- 4 float-pi))))
	 (ln1-x2 (log (- 1 (expt x 2))))
	 (subexp (+ (/ 2 (* float-pi a))
		    (/ ln1-x2 2))))
    (* (cl-signum x)
       (sqrt (- (sqrt (- (expt subexp 2) (/ ln1-x2 a)))
		subexp)))))
;;(cl-loop for x in '(0 .1 .2 .5 1 1.2 1.5 1.82 2 3) collect (- x (inverse-erf (erf x))))

(cl-defun normal-distribution-quantile (p &optional (mu 0) (sigma 1))
  "Return the quantile at probability P for a normal distribution.
MU is the mean and SIGMA is the positive standard deviation."
  (unless (and (numberp p) (< 0 p 1))
    (error "Probability must be strictly between zero and one: %S" p))
  (unless (and (numberp mu) (numberp sigma) (> sigma 0))
    (error "MU must be numeric and SIGMA positive: %S, %S" mu sigma))
  (+ mu (* sigma (sqrt 2) (inverse-erf (- (* 2 p) 1)))))
;;(cl-loop for p in (list .95 .9 (/ 5 6.0) .75 .5) collect (normal-distribution-quantile p 100 15))

(cl-defun geometric-series (r &key (start 0) end)
  "Return the geometric sum of powers of R from START below END.
START defaults to zero.  When END is nil, return the infinite tail and
require the absolute value of R to be less than one."
  (unless (numberp r)
    (error "Common ratio must be numeric: %S" r))
  (unless (and (integerp start) (>= start 0))
    (error "START must be a non-negative integer: %S" start))
  (unless (or (null end) (and (integerp end) (>= end 0)))
    (error "END must be nil or a non-negative integer: %S" end))
  (cond
   ((and end (<= end start)) 0)
   ((and end (= r 1)) (- end start))
   (end (/ (- (expt r start) (expt r end))
           (- 1.0 r)))
   ((< (abs r) 1) (/ (expt r start) (- 1.0 r)))
   (t (error "Absolute value of common ratio must be less than one"))))
;;(geometric-series 0.5 :end 1)

(cl-defun quadratic-discriminant (a b c)
  "Return the discriminant of A*x^2 + B*x + C."
  (- (expt b 2) (* 4 a c)))
;;(quadratic-discriminant 1 1 1)

(cl-defun quadratic-root (a b c)
  "Return the real roots of A*x^2 + B*x + C."
  (when (zerop a)
    (error "Quadratic coefficient A must be nonzero"))
  (let ((d (quadratic-discriminant a b c)))
    (when (< d 0)
      (error "The discriminant is negative: %S" d))
    (let* ((2a (* 2.0 a))
	   (b/2a (/ b 2.0 a))
	   (sqrt-expr (/ (sqrt d) 2a)))
      (list (- sqrt-expr b/2a) (- (+ sqrt-expr b/2a))))))
;;(quadratic-root 1 0 -1)

(cl-defun falsi-method (f a b e m)
  "Return the solution to the equation F(x) = 0.
Here, A and B bracket the search interval.  E is half the upper bound for
relative error.  M is the positive maximum number of iterations."
  (unless (and (numberp a) (numberp b) (< a b))
    (error "A and B must be numeric and ordered: %S, %S" a b))
  (unless (and (numberp e) (> e 0))
    (error "Relative error must be positive: %S" e))
  (unless (and (integerp m) (> m 0))
    (error "Iteration count must be a positive integer: %S" m))
  (let ((c 0)
        (fc 0)
        (n 0)
        (side 0)
	;; starting values at endpoints of interval
        (fa (funcall f a))
        (fb (funcall f b)))
    (cond
     ((zerop fa) a)
     ((zerop fb) b)
     ((> (* fa fb) 0)
      (error "F(A) and F(B) must have opposite signs"))
     (t
      (while (< n m)
        (setf c (/ (- (* fa b) (* fb a)) (- fa fb)))
        (if (< (abs (- b a)) (* e (abs (+ b a))))
            (setf n m)
          (setf fc (funcall f c))
          (if (> (* fc fb) 0)
              (progn
                (setf b c fb fc)
                (if (= side -1)
                    (setq fa (/ fa 2.0)))
                (setf side -1))
            (if (> (* fa fc) 0)
                (progn
                  (setf a c fa fc)
                  (if (= side 1)
                      (setq fb (/ fb 2.0)))
                  (setf side 1))
              (setf n m))))
        (cl-incf n))
      c))))

(cl-defun newton-raphson (f df x0 &optional (eps 1E-12) (max-n 20))
  "Approximate a root of F from X0 using derivative DF.
Stop when the correction is smaller than positive EPS or after positive
integer MAX-N iterations.  Return root metadata on convergence and nil
otherwise."
  (unless (and (numberp eps) (> eps 0))
    (error "EPS must be positive: %S" eps))
  (unless (and (integerp max-n) (> max-n 0))
    (error "MAX-N must be a positive integer: %S" max-n))
  (cl-loop for n below max-n
	for xn = x0 then xn+1
	for fn = (funcall f xn)
	for dfn = (funcall df xn)
        do (when (zerop dfn)
             (error "Derivative is zero at %S" xn))
	for dx = (/ fn dfn)
	for xn+1 = (- xn dx)
	collect xn+1 into partial
	if (< (abs dx) eps) return (list xn+1 (/ dx eps) n partial)))
;;(setf qwe (newton-raphson #'(lambda (x) (- (area-secant x) (/ pi 3))) #'(lambda (x) (/ (- 1 (cos x)) 2)) 2.0))

(provide 'mb-math-functions)

;;; mb-math-functions.el ends here
