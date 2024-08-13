;;;; Standard math functions not supported by Emacs
(defun sec (x) (/ 1 (cos x)))
(defun scs (x) (/ 1 (sin x)))
(defun sinh (x) (/ (- (exp x) (exp (- x))) 2))
(defun cosh (x) (/ (+ (exp x) (exp (- x))) 2))
(defun tanh (x) (let ((y (exp (* 2 x)))) (/ (1- y) (1+ y))))
;;(+ (sinh pi) (cosh pi) (- (exp pi)))
;;(- (tanh 1) (/ (sinh 1) (cosh 1)))

(defmacro define-degree-trigonometry (fns)
  `(let ((c ,(/ pi 180.0)))
     ,@(cl-loop for fn in fns
		collect `(defun ,(intern (concat (sstring fn) "d")) (d)
			   (,fn (* degrees-to-radians d)))
		collect `(defun ,(intern (concat "a" (sstring fn) "d")) (r)
			   (* radians-to-degrees
			      (,(intern (concat "a" (sstring fn))) r))))))
(define-degree-trigonometry (cos sin tan sec scs))
;;(cl-loop for fn in '(sind cosd tand) collect (funcall fn 45))
;;(list (asind 0.5) (acosd 0.5) (atand 1))

(cl-defun polylogarithm-abs-lt-1 (x &optional (n 2) (num-iterations 50))
  "Calculates the Li_n function of X.
See http://mathworld.wolfram.com/Polylogarithm.html for a definition."
  (cl-loop for i from 1 below num-iterations
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
;;(cl-loop for x from -3 to 3 by 0.5 collect (erf x))
;;(cl-loop for x in '(0.8) collect (erf x))

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
;;(cl-loop for x in '(0 .1 .2 .5 1 1.2 1.5 1.82 2 3) collect (- x (inverse-erf (erf x))))

(cl-defun normal-distribution-quantile (p &optional (mu 0) (sigma 1))
  (cl-assert (< 0 p 1))
  (+ mu (* sigma (sqrt 2) (inverse-erf (- (* 2 p) 1)))))
;;(cl-loop for p in (list .95 .9 (/ 5 6.0) .75 .5) collect (normal-distribution-quantile p 100 15))

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

(defun quadratic-discriminant (a b c)
  "Return the discriminant of the quadratic polynomial ax2 + bx + c."
  (- (sq b) (* 4 a c)))
;;(quadratic-discriminant 1 1 1)

(defun quadratic-root (a b c)
  "Return the roots of the quadratic polynomial ax2 + bx + c."
  (let ((d (quadratic-discriminant a b c)))
    (cl-assert (not (minusp d)) t "The discriminant is negative.")
    (let* ((2a (* 2.0 a))
	   (b/2a (/ b 2.0 a))
	   (sqrt-expr (/ (sqrt d) 2a)))
      (list (- sqrt-expr b/2a) (- (+ sqrt-expr b/2a))))))
;;(quadratic-root 1 0 -1)

(defun falsi-method (f a b e m)
  "Return the solution to the equation F(x) = 0.
Here, A, B are the endpoints of an interval where we search. E is
half of upper bound for relative error. M is the maximal number
of iterations."
  (let ((c 0)
        (fc 0)
        (n 0)
        (side 0)
	;; starting values at endpoints of interval
        (fa (funcall f a))
        (fb (funcall f b)))
    (while (< n m)
      (setf c (/ (- (* fa b) (* fb a)) (- fa fb)))
      (if (< (abs (- b a)) (* e (abs (+ b a))))
        (setf n m)
        (setf fc (funcall f c))
        (if (> (* fc fb) 0)
	  ;; fc and fb have same sign, copy c to b
          (progn
            (setf b c fb fc)
            (if (= side -1)
              (/2 fa))
            (setf side -1))
          (if (> (* fa fc) 0)
	    ;; fc and fa have same sign, copy c to a
            (progn
              (setf a c fa fc)
              (if (= side 1)
                (/2 fb))
              (setf side 1))
            (setf n m))))
      (incf n))
    c))
;;(falsi-method #'(lambda (x) (- (cos x) (expt x 3))) 0 1 5E-15 100)

(cl-defun newton-raphson (f df x0 &optional (eps 1E-12) (max-n 20))
  (loop for n below max-n
	for xn = x0 then xn+1
	for fn = (funcall f xn)
	for dfn = (funcall df xn)
	for dx = (/ fn dfn)
	for xn+1 = (- xn dx)
	collect xn+1 into partial
	if (< (abs dx) eps) return (list xn+1 (/ dx eps) n partial)))
;;(setf qwe (newton-raphson #'(lambda (x) (- (area-secant x) (/ pi 3))) #'(lambda (x) (/ (- 1 (cos x)) 2)) 2.0))

(provide 'mb-math-functions)
