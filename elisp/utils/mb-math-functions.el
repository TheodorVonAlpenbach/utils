;;;; Standard math functions not supported by Emacs

(cl-defun polylogarithm-abs-lt-1 (x &optional (n 2) (num-iterations 50))
  "Calculates the Li_n function of X.
See http://mathworld.wolfram.com/Polylogarithm.html for a definition."
  (loop for i from 1 below num-iterations
	sum (/ (expt x i) (expt i n))))

(cl-defun dilog-lt-minus-1 (x &optional (num-iterations 50))
  "http://www.geocities.com/hjsmithh/Numbers/Dilog.html"
  (when (>= x -1) (error "Input"))
  (let ((y (/ 1 (- 1.0 x))))
    (- 0 
       (polylogarithm-abs-lt-1 (- 1 y) 2 num-iterations)
       (/ (sq (log y)) 2))))
;;(dilog-lt-minus-1 -2)

(cl-defun dilog-gt-1 (x &optional (num-iterations 50))
  (when (< x 1) (error "Input"))
  (- (dilog 1)
    (* (log x) (log (- x 1)))
     (dilog (- 1 x))))
;;(dilog-gt-1 3 60)

(cl-defun dilog (x &optional (num-iterations 50))
  "http://mathworld.wolfram.com/Dilogarithm.html. For testing, see
http://functions.wolfram.com/webMathematica/FunctionEvaluation.jsp?name=PolyLog2"
  (cond
   ((= x 0) 0)
   ((= x -1) (- (/ (sq pi) 12)))
   ((= x 1) (/ (sq pi) 6))
   ((< (abs x) 1) (polylogarithm-abs-lt-1 x 2 num-iterations))
   ((< x -1) (dilog-lt-minus-1 x num-iterations))
   ((> x 1) (dilog-gt-1 x num-iterations))))
;;(dilog 7)

(defun atan2 (x y)
  "Arctan with two arguments"
  ;; http://en.wikipedia.org/wiki/Atan2
  (numcond x
      (> (atan (/ (float y) x)))
      (< (numcond y
	   (>= (+ (atan (/ (float y) x)) float-pi))
	   (<  (- (atan (/ (float y) x)) float-pi))))
      (= (numcond y
	   (> (/ float-pi 2))
	   (< (- (/ float-pi 2)))
	   (= 'indeterminate)))))
;;(atan2 -1 0)

;;; erf function
(defconst +erf-tau-polynomial+
  '(-1.26551223 1.00002368 0.37409196 0.09678418
    -0.18628806 0.27886807 -1.13520398 1.48851587
    -0.82215223 0.17087277))

(defun erf-tau-1 (x s)
  (* s (exp (- (horner +erf-tau-polynomial+ s) (sq x)))))

(defun erf-tau (x)
  (erf-tau-1 x (/ 1 (+ 1 (* 0.5 (abs x))))))

(defun erf (x)
  "Calculate the erf function for X.
See https://en.wikipedia.org/wiki/Error_function#Numerical_approximations
for numerical implementation basis."
  (if (minusp x)
    (- (erf-tau x) 1)
    (- 1 (erf-tau x))))
;;(loop for x from -3 to 3 by 0.5 collect (erf x))
;;(loop for x in '(0.8) collect (erf x))

(defun erfc (x)
  "Calculate the complimentary error function for X"
  (- 1 (erf x)))

(defun inverse-erf (x)
  ""
  (let* ((a (/ (* 8 (- pi 3))
	       (* 3 pi (- 4 pi))))
	 (ln1-x2 (log (- 1 (sq x))))
	 (subexp (+ (/ 2 (* pi a))
		    (/ ln1-x2 2))))
    (* (signum x)
       (sqrt (- (sqrt (- (sq subexp) (/ ln1-x2 a)))
		subexp)))))
;;(loop for x in '(0 .1 .2 .5 1 1.2 1.5 1.82 2 3) collect (- x (inverse-erf (erf x))))

(cl-defun normal-distribution-quantile (p &optional (mu 0) (sigma 1))
  (assert (< 0 p 1))
  (+ mu (* sigma (sqrt 2) (inverse-erf (- (* 2 p) 1)))))
;;(loop for p in (list .95 .9 (/ 5 6.0) .75 .5) collect (normal-distribution-quantile p 100 15))

(cl-defun geometric-series (r &key (start 0) end)
  "Return the infinite geometric series 1 + r + r^2 + ..."
  (if (zerop start)
    (if end
      (if (<= end start)
	0
	(if (= (abs r) 1)
	 (- end start)
	 (/ (- 1.0 (expt r end))
	    (- 1 r))))
      (if (< (abs r) 1)
	(/ 1 (- 1.0 r))
	(error "Absolute value of common ratio must be less than one")))
    (- (geometric-series r :start 0 :end end)
       (geometric-series r :start 0 :end start))))
;;(geometric-series 0.5 :end 1)

(provide 'mb-math-functions)
