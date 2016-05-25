;; This implementation is based on /usr/lib/gcc/i686-pc-cygwin/4.8.3/include/c++/tr1/gamma.tcc
;; From gamma.tcc
;; Written by Edward Smith-Rowland based on:
;;   (1) Handbook of Mathematical Functions,
;;       ed. Milton Abramowitz and Irene A. Stegun,
;;       Dover Publications,
;;       Section 6, pp. 253-266
;;   (2) The Gnu Scientific Library, http:;;www.gnu.org/software/gsl
;;   (3) Numerical Recipes in C, by W. H. Press, S. A. Teukolsky,
;;       W. T. Vetterling, B. P. Flannery, Cambridge University Press (1992),
;;       2nd ed, pp. 213-216
;;   (4) Gamma, Exploring Euler's Constant, Julian Havil,
;;       Princeton, 2003.

(cl-defun bernoulli-number (n &optional (type :second))
  "Returns the Nth Bernoulli number of type TYPE (:FIRST or :SECOND).
http://en.wikipedia.org/wiki/Bernoulli_number."
  (case n
    (0 1)
    (1 (if (eql type :first) -0.5 0.5))
    (t (if (oddp n)
	 0
	 (loop with a = (make-vector (1+ n) 0)
	       for m to n
	       do (setf (aref a m) (/ 1.0 (1+ m)))
	       do (loop for j from m downto 1
			do (setf (aref a (1- j))
				 (* j (- (aref a (1- j))
					 (aref a j)))))
	finally return (aref a 0))))))
;;(bernoulli-number 101)

(defun log-gamma-bernoulli (x)
  "The logarithm of the gamma function by asymptotic expansion with Bernoulli number coefficients.
This is like Sterling's approximation."
  (let ((lg (+ (* 0.5 (log (* 2 pi)))
		(- (* (- x 0.5)
		      (log x))
		   x)))
	(x2 (sq x)))
    (loop for i from 1 below 20
	  for 2i = (* 2 i)
	  for help* = (/ 1.0 x) then help
	  for help = (/ help* (* 2i (- 2i 1) x2))
	  do (incf lg (* (bernoulli-number 2i) help)))
    lg))
;;(log-gamma-bernoulli 1)
;;(log 1)

(defconst +lanczos-cheb-7+
  (vector   0.99999999999980993227684700473478
	  676.520368121885098567009190444019
	-1259.13921672240287047156078755283
	  771.3234287776530788486528258894
         -176.61502916214059906584551354
  	   12.507343278686904814458936853
	   -0.13857109526572011689554707
	    9.984369578019570859563e-6
	    1.50563273514931155834e-7))

(log (sqrt (* 2 pi)))


(defun log-gamma-lanczos (x)
  "Return log(Gamma(x)) by the Lanczos method."
  (let ((xm1 (- x 1.0))
	(log-sqrt-2pi 0.9189385332046727417803297364056176)
	(sum (aref +lanczos-cheb-7+ 0)))
    (loop for k from 1 below 9
	  do (incf sum (/ (aref +lanczos-cheb-7+ k)
			  (+ xm1 k))))
    (+ (* (+ xm1 0.5) (log (/ (+ xm1 7.5) float-e)))
       (- (+ log-sqrt-2pi (log sum)) 7))))
;;(log-gamma-lanczos 4)

(defun log-gamma (x)
  "The logarithm of the gamma function. Also returns the sign of the gamma function at X
as a second value."
  (if (> x 0.5)
    (values (log-gamma-lanczos x) 1)
    (let* ((sin-fact (sin (* pi x)))
	   (sign (if (plusp x)
		   1 (signum sin-fact))))
      (if (zerop sin-fact)
	(error "Argument is nonpositive integer in log-gamma")
	(values (- (log pi) (log (abs sin-fact)) (log-gamma-lanczos (- 1.0 x))) sign)))))
;;(log-gamma -0.5)

(defun gamma (x)
  "Returns the value of the gamma function for the value of x"
  (destructuring-bind (lnx sign) (log-gamma x)
    (* sign (exp lnx))))
;;(gamma -0.5)

(defun test-gamma ()
  "Test the result of `gamma'"
  (let ((a `((1 1)
	     (2 1)
	     (3 2)
	     (4 6)
	     (-1.5 ,(* (/ 4.0 3) (sqrt pi)))
	     (-0.5 ,(* -2 (sqrt pi)))
	     (0.5 ,(sqrt pi))
	     (1.5 ,(* 0.5 (sqrt pi)))
	     (2.5 ,(* (/ 3.0 4) (sqrt pi)))
	     (3.5 ,(* (/ 15.0 8) (sqrt pi))))))
    (loop for (x fasit) in a
	  for res = (gamma x)
	  for diff = (- res fasit)
	  collect (list diff x) into result
	  sum diff into error
 	  finally return (list error result))))
;;(test-gamma)

;;; And now, to something quite different:
(cl-defun wrapped-normal-distribution-pdf (mu sigma &optional (epsilon 0.00001))
  (lexical-let* ((mu mu) (sigma sigma) (epsilon epsilon)
		 (d (/ 1 (* sigma (sqrt 2))))
		 (a (* d (/ 1 (sqrt float-pi))))
		 (c (* d (* 2 float-pi))))
    (cl-function
     (lambda (theta &key (k-limit 500))
       (let ((b (* d (- theta mu))))
	 (* a (loop with sum = (exp (- (sq b))) ; k = 0
		    for k to k-limit
		    for s- = (exp (- (sq (+ b (* c (- k))))))
		    for s+ = (exp (- (sq (+ b (* c (+ k))))))
		    for s = (+ s- s+)
		    do (incf sum s)
		    until (< (abs s) epsilon)
		    finally return sum)))))))
;;(funcall (wrapped-normal-distribution-pdf 0 100) 0)

(defconst infinity+ cl-most-positive-float)
(defconst infinity- cl-most-negative-float)

(require 'mb-gnuplot)
(cl-defun wnd-test (&key (mu 0) (sigma 1))
  "Plots pdf of the wrapped normal distribution with mean MU and
  standard deviation SIGMA. See `wrapped-normal-distribution-pdf'."
  (mb-gnuplot (wrapped-normal-distribution-pdf mu sigma) :x-range (list (- pi) pi) :y-range (list 0 1)))
;;(wnd-test)

(cl-defun wnd-test2 (&optional (mu 0))
  (let* ((xr (list (- pi) pi))
	 (yr (list 0 1))
	 (sigma2s-colors (list (list 0.125     :pink)
			       (list 0.25      :red)
			       (list 0.5       :green)
			       (list 1         :turquiose)
			       (list 2         :blue)
			       (list infinity+ :black)))
	 (plots (loop for (sigma2 color) in sigma2s-colors
		      for fn = (wrapped-normal-distribution-pdf mu (sqrt sigma2))
		      collect (list fn :color color :legend (format "σ² = %s" (gp-float sigma2))))))

    (mb-gnuplot2 plots :x-range xr :y-range yr :title "Wrapped normal distribution"
		 '(:text :above "Probability density function")
		 '(:text :below "The support is chosen to be [-π,π] with μ=0"))
    plots))
;;(setf qwe (wnd-test2))
;;(format "σ² = %f" 1.0)
