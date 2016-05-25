;;;; Signal stuff
(require 'mb-polynomials)

;;; elliptic filters, see
;;  http://dsp-book.narod.ru/DFD/DFD5.pdf
(defun selectivity-factor (wp ws)
  "This is k in Rorabaugh"
  (/ (float wp) ws))
;;(selectivity-factor 3000 3200)

(defun modular-constant (k)
  "This returns the entity denoted q in both Rorabaugh (R) and Wikipedia (W, see below).
While Rorabaugh calls this the /modular constant/, W states that
this represent a /nome/, i.e. (exp (* pi i tau)). The entity k
represents R's /selectvity factor/ and /elliptic modulus k(tau)
in W.

W http://en.wikipedia.org/wiki/Jacobi_elliptic_functions#Definition_in_terms_of_theta_functions"
  (let* ((4rk (expt (- 1 (* k k)) 0.25))
	 (u (/ (- 1 4rk) (* 2 (+ 1 4rk)))))
    ;; see W for an even longer, and possibly more accurate, formula
    (+ u (* 2 (expt u 5)) (* 15 (expt u 9)) (* 150 (expt u 13)))))
(defalias 'nome #'modular-constant)
;;(mapcar #'modular-constant (list 1 (sqrt (- 1 (/ 1.0 16))))) ==> '(1/2 1/6)
;;(modular-constant (selectivity-factor 3000 3200))

(defun dB (x)
  (* 10 (log x 10)))

(defun dB-inverse (dB)
  (expt 10 (/ dB 10.0)))
;;(loop for x in (list pi float-e 1 2 3) collect (- x (dB (dB-inverse x))))

(defun* amplitude (x from-scale &optional (to-scale nil))
  (if (eql from-scale to-scale)
    x
    (let ((x* (case from-scale
		(:dB (dB-inverse x))
		(t x))))
      (case to-scale
	(:dB (dB x*))
	(t x*)))))
;;(amplitude 1.05 nil :dB)

(defun* discrimination-factor (Ap As &optional (scale :dB))
  (/ (1- (float (amplitude As scale)))
     (1- (amplitude Ap scale))))
;;(discrimination-factor .1 50)

(defun* elliptic-filter-order (wp ws Ap As &key (amplitude-scale :dB))
  (ceiling (/ (log (* 16.0 (discrimination-factor Ap As amplitude-scale)) 10)
	      (log (/ 1.0 (modular-constant (selectivity-factor wp ws))) 10))))
;;(elliptic-filter-order 1000 1040 1 50)

(defun* elliptic-min-stop-band-loss (order Ap q &key (scale :dB))
  (1+ (/ (1- (amplitude Ap scale))
	 (* 16 (expt q order)))))
;;(dB (elliptic-min-stop-band-loss 9 0.1 0.13))

(defun* DFD-V (n Ap &optional (scale :dB))
  "It is possible that the Rorabaugh 'V' is the same as the Wikipedia 'tau'"
  (let ((Ap-squared (sqrt (amplitude Ap scale))))
    (/ (log (/ (1+ Ap-squared)
	       (1- Ap-squared)))
       (* 2 n))))
;;(DFD-V 9 0.1)

(defsubst sinh (x) (/ (- (exp x) (exp (- x))) 2))
(defsubst cosh (x) (/ (+ (exp x) (exp (- x))) 2))

(defun theta-01 (w q)
  "See http://en.wikipedia.org/wiki/Theta_function.
Note that the 'nome' q must be real in this implementation
Not implemented yet, doesn't fit")

(defun* DFD-p0-upper-sum (q V &key (hyperbolic t) (epsilon 1E-10) (n 1000))
  "This looks a bit like theta-11, but not quite. Weird."
  (loop for m below n
	for x = (* (1+ (* 2 m)) V)
	for term = (* (expt -1 m)
		      (expt q (* m (1+ m)))
		      (if hyperbolic (sinh x) (sin x)))
	while (> (abs term) epsilon)
	sum term))
;;(DFD-p0-upper-sum 0.1290407910348594 (DFD-V 9 0.1))

(defun* DFD-p0-lower-sum (q V &key (hyperbolic t) (epsilon 1E-10) (n 1000))
  "This looks a bit like theta-01, but not quite. Weird."
  (loop for m from 1 below n
	for x = (* 2 m V)
	for term = (* (expt -1 m)
		      (expt q (sq m))
		      (if hyperbolic (cosh x) (cos x)))
	while (> (abs term) epsilon)
	sum term))
;;(DFD-p0-lower-sum 0.1290407910348594 (DFD-V 9 0.1))

(defun* DFD-theta-fraction (q V &optional (hyperbolic t))
  (/ (* (expt q 0.25) (DFD-p0-upper-sum q V :hyperbolic hyperbolic))
     (+ 0.5 (DFD-p0-lower-sum q V :hyperbolic hyperbolic))))
;;(DFD-theta-fraction 0.1290407910348594 (DFD-V 9 0.1))

(defun DFD-p0 (q V)
  (abs (DFD-theta-fraction q V t)))
;;(DFD-p0 0.1290407910348594 (DFD-V 9 0.1))

(defun DFD-W (p0 k)
  (let ((p0-squared (sq p0)))
    (sqrt (* (1+ (/ p0-squared k))
	     (1+ (* p0-squared k))))))
;;(DFD-W 0.47021803511169497 0.9375)

(defun DFD-X (n i q)
  (let ((mu (if (oddp n) i (- i 0.5))))
    (DFD-theta-fraction q (/ (* mu pi) n) nil)))
;;(DFD-X 9 1 0.1290407910348594)

(defun DFD-Y (X k)
  (let ((X-squared (sq X)))
    (sqrt (* (- 1 (/ X-squared k))
	     (- 1 (* X-squared k))))))

(defun DFD-cauer (X Y W p0)
  (let* ((X^2 (sq X))
	 (p0^2 (sq p0))
	 (X^2*p0^2 (* X^2 p0^2))
	 (a (/ 1 X^2))
	 (b (/ (* 2 p0 Y) 
	       (1+ X^2*p0^2)))
	 (c (/ (+ (* p0^2 (sq Y)) (* X^2 (sq W)))
	       (sq (1+ X^2*p0^2)))))
    (list a b c)))

(defun* DFD-H0 (cauers p0 n Ap &optional (scale :dB))
  "Note that r = (floor n) is implicit in the length of cauers"
  (* (if (oddp n) p0 (sqrt (amplitude Ap scale)))
     (/ (reduce #'* (mapcar #'third cauers)) ;the c's
	(reduce #'* (mapcar #'first cauers))))) ;the a's

(defun scale-cauers (cauers alpha)
  (loop for (a b c) in cauers
	for a* = (* (sq alpha) a)
	for b* = (* alpha b)
	for c* = (* (sq alpha) c)
	collect (list a* b* c*)))

(defun* elliptic-parameters (&key (order 9) (Ap 0.1) (wp 3000) (ws 3200) (normalize t))
  "Returns a list of polynomial fractions each corresponding to a
  set of cauer coefficients, together with the head fraction"
  (let* ((k (selectivity-factor wp ws)) 
	 (q (modular-constant k))
	 (As (dB (elliptic-min-stop-band-loss order Ap q)))
	 (V (DFD-V order Ap))
	 (p0 (DFD-p0 q V))
	 (W (DFD-W p0 k))
	 (r (floor order 2))
	 (Xs (loop for i from 1 to r collect (DFD-X order i q)))
	 (Ys (loop for x in Xs collect (DFD-Y x k)))
	 (cauers (loop for x in Xs
		       for y in Ys
		       collect (DFD-cauer x y W p0)))
	 (H0 (DFD-H0 cauers p0 order Ap))
	 (alpha (sqrt (* wp ws))))
    (unless normalize
      (setf cauers (scale-cauers cauers alpha))
      (when (oddp order)
	(setf H0 (* alpha H0))
	(setf p0 (* alpha p0))))
    (list :H0 H0 :cauers cauers :Y Ys :X Xs :r r :k k :q q :As As :V V :p0 p0 :W W)))
;;(elliptic-parameters)

(defun* elliptic-fractions (&key (order 9) (Ap 0.1) (wp 3000) (ws 3200) (normalize t))
  "Returns a list of polynomial fractions each corresponding to a
  set of cauer coefficients, together with the head fraction"
  (let* ((params (elliptic-parameters :order order :Ap Ap :wp wp :ws ws :normalize normalize))
	 (H0-fraction (list (list (getf params :H0)) 
			   (if (oddp order) 
			     (list (getf params :p0) 1)
			     (list 1))))
	 (cauer-fractions (loop for i from 1 to (getf params :r)
			       for (a b c) in (getf params :cauers)
			       for num = (list a 0 1)
			       for den = (list c b 1)
			       collect (list num den))))
    (cons H0-fraction cauer-fractions)))
;;(elliptic-fractions :normalize t)

(defun z-transform-polynomial-1 (p K)
  "Helper for `digitalize-biquad'. K2 is square of K (= T/2)"
  (destructuring-bind (c0 c1) p
    (list (+ c0 (* c1 K)) (- c0 (* c1 K)))))

(defun z-transform-polynomial-2 (p K)
  "Helper for `digitalize-biquad'. K2 is square of K (= T/2)"
  (destructuring-bind (c2 c1 c0) p
    (let* ((K^2 (sq K))
	   (c0K^2 (* c0 K^2))
	   (c1K (* c1 K)))
      (list (+ c0K^2 c1K c2)
	    (* 2 (- c2 c0K^2))
	    (- (+ c0K^2 c2) c1K)))))

(defun digitalize-fp (fp T)
  "Transforms analog biquad filter (expressed in s) to digital
biquad filter (expressed in z^-1).
See http://en.wikipedia.org/wiki/Bilinear_transform"
  (let ((n (fp-order fp)))
    (if (zerop n)
      fp
      (mapcar #'(lambda (x) 
		  (funcall (case n 
			     (1 #'z-transform-polynomial-1)
			     (2 #'z-transform-polynomial-2))
			   x (/ 2 (float T)))) (fp-expand fp n)))))

(defun* elliptic-filter (&key (order 9) (Ap 0.1) (wp 3000) (ws 3200) (T 1.0) (normalize-frequencies t) (normalize-coefficients t))
  (let* ((fps (elliptic-fractions :order order :Ap Ap :wp wp :ws ws :normalize normalize-frequencies))
	 (z-fps (mapcar (bind #'digitalize-fp T) fps))
	 (fp (reduce #'fp* z-fps)))
    (if normalize-coefficients
      (maptree (bind #'/ (first (second fp))) fp)
      fp)))
;;(elliptic-filter-order 3 5 10 10)
;;(elliptic-filter :order 2 :Ap 10 :wp 1000 :ws 1040 :normalize-frequencies t)
;;(elliptic-parameters :order 2 :Ap 10 :wp 1000 :ws 1040 :normalize nil)
;;(getf (elliptic-parameters :order 6 :Ap 5 :wp 0.9 :ws 1) :As)
;;(elliptic-filter-order .9 1 .1 50)
;;(setq mf (parse-matlab-coefficients))
;;(gnuplot (elliptic-filter :order 6 :Ap 5 :wp 0.9 :ws 1))
;;(pfp (elliptic-filter :order 6 :Ap 5 :wp 0.9 :ws 1))

(require 'mb-gnuplot)
(require 'complex)
(defun* test-transfer (&key (order 9) (Ap 1) (wp 3000) (ws 3200) (T 1.0) (normalize t) (scale nil))
  (let* ((fp (reduce #'fp* (elliptic-fractions :order order :Ap Ap :wp wp :ws ws :normalize normalize)))
	 (a 0)
	 (b (* 4 (if normalize 1 (avg wp ws))))
	 (step (* .005 b)))
    (loop for i from (+ a step) to b by step
	  for x = (complex 0 i)
	  collect (list i (amplitude (complex-abs-squared (fp-value fp x)) nil scale)))))
;;(mb-gnuplot (test-transfer :normalize nil :scale :dB) :type :points :x-range '(0 3200) :y-range '(-2 0.02))
;;(mb-gnuplot (test-transfer :normalize nil :scale :dB) :type :points :x-range '(0 12000))
;;(getf (elliptic-parameters :normalize nil) :As)
;;(mb-gnuplot (test-transfer :order 9 :Ap 0.1 :normalize nil :scale :dB) :type :points)

(provide 'elliptic-filter)
;LdAMoMio6Spm
