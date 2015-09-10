(in-package :numerics-utils)

(defparameter *float-epsilon* 1E-16)
(defconstant 1/SQRT-PI (/ 1 (sqrt pi)))
(defconstant 2PI (* 2 pi))
(defconstant SQRT-2 (sqrt 2))

(defun normal-distribution (mu sigma)
  (let ((a (/ 1 (* sigma (sqrt 2PI))))
	(b (- (/ 1 (* 2 (sq sigma))))))
    (lambda (x) (* a (exp (* b (sq (- x mu))))))))
;;(gp::plot `(:d ,(normal-distribution 0 1) :x-values (-4 4)))

(defun normalize-radian (x &optional (min-radian (- pi)))
  (+ (mod (- x min-radian) 2PI) min-radian))
;;(normalize-radian pi)

(defun wrapped-normal-distribution (mu sigma &optional (k-limit 10))
  "Returns WND for MU and SIGMA."
  (declare (float mu sigma h g d theta f) (fixnum k))
  (let* ((h (/ 1 SQRT-2 sigma))
	 (g (* 2PI h))
	 (d (* 1/SQRT-PI h)))
    (if (>= sigma 2PI)
      (constantly (/ 1 2PI))
      (lambda (theta)
	(let* ((f (* (normalize-radian (- theta mu)) h)))
	  (flet ((addend (k) (safe-op #'exp (- (sq (+ f (safe-* g k)))))))
	    (safe-* d (+ (addend 0)
			 (loop for k from -1 above (- k-limit)
			       for x = (addend k)
			       while (not (zerop x)) sum x)
			 (loop for k from 1 below k-limit
			       for x = (addend k)
			       while (not (zerop x)) sum x)))))))))
;;(handles-outflow () (funcall (wrapped-normal-distribution 3.11366738555788 0.136511989704312) -2.8448865))
;;(gp::plot `((:l (:d ,(wrapped-normal-distribution -2.600540585471551 0.7) :x-values ,(list (- pi) pi)))))

(defun wnd (&rest args) (apply #'wrapped-normal-distribution args))
;;(handles-outflow () (funcall (wnd -0.136135681655558 0.349065850398866) 3.0455992))

(defun rayleigh-distribution-function (sigma)
  "http://en.wikipedia.org/wiki/Rayleigh_distribution"
  (let* ((sigma2 (sq sigma))
	 (a (/ 1 sigma2))
	 (b (/ -1 2 sigma2)))
    (lambda (x) (* a x (exp-safe (* b (sq x)))))))
;;(gp:plot (rayleigh-distribution-function 1) :x-values '(0 10))

(defun rayleigh-cumulative-function (sigma)
  "http://en.wikipedia.org/wiki/Rayleigh_distribution"
 (let ((a (/ -1 2 (sq sigma))))
    (lambda (x) (- 1 (exp-safe (* a (sq x)))))))
;;(gp:plot (rayleigh-cumulative-function 4) :x-values '(0 10))

(defun variance-vector (vector)
  "Returns unbiased sample variance of the elements in VECTOR"
  (let ((mean (average vector)))
    (/ (loop for x across vector
	     sum (sq (- x mean)))
       (1- (length vector)))))
;;(stdev-vector #(1 2 3 4))

(defun variance (sequence) (variance-vector (coerce sequence 'vector)))
(defun stdev (sequence) (sqrt (variance sequence)))

(defun random-normal-fn (mu sigma &optional (n 12))
  "Returns a function that generates values that are normal distributed"
  (let ((n/2 (/ n 2)))
    (lambda () (+ mu (* sigma (- (loop repeat 12 sum (random 1.0)) n/2))))))
;;(stdev (loop with fn = (random-normal-fn 0 PI) repeat 100000 collect (funcall fn)))

