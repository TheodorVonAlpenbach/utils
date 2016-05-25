(defconst rational-regexp "\\([[:digit:]]+\\)/\\([[:digit:]]+\\)")

(defstruct rational
  numerator
  denominator)

(defun rational-new (a b &optional simplify) 
  (let ((q (make-rational :numerator a :denominator b)))
    (if simplify (rational-simplify q) q)))

(defun rational-a/b (q)
  (list (rational-numerator q) (rational-denominator q)))

(defun rational-from-symbol (x)
  (and (symbolp x)
       (let ((qs (symbol-name x)))
	 (and (string-match-exact rational-regexp qs)
	      (apply #'rational-new (mapcar #'string-to-number (list (match-string 1 qs)
								     (match-string 2 qs))))))))
;;(rational-from-symbol '1/7)

(defun rational-to-string (q)
  (format "%d/%d" (rational-numerator q) (rational-denominator q)))

(defun rational-to-symbol (q)
  (intern (rational-to-string q)))
;;(rational-to-symbol (rational-from-symbol '1/7))

(defun rational-to-float (q)
  (if (rational-p q)
    (/ (float (rational-numerator q)) (rational-denominator q))
    q))
;;(rational-to-float (rational-from-symbol '1/7))

(defun rational-from-integer (n)
  (rational-new n 1))

(defun rational-parse (x)
  (or (rational-from-symbol x)
      (and (rational-p x) x)))

(defun rational-parse-to-number (x)
  (aif (rational-parse x)
    (rational-to-float it)
    x))
;;(rational-parse-to-number 1.2)

(defun rational-invert (q) 
  (if q (rational-new (rational-denominator q) (rational-numerator q))))

(defun rational-simplify (q)
  (let ((a (rational-numerator q))
	(b (rational-denominator q)))
    (when (= b 0) (error "Denominator is zero"))
    (if (= a 0) 
      0
      (let* ((signum (signum* a b))
	     (d (gcd a b))
	     (q (rational-new (* signum (/ (abs a) d)) (/ (abs b) d))))
	(if (= (rational-denominator q) 1)
	  (rational-numerator q) q)))))
;;(rational-simplify (rational-new -6 4))

;;; binary functions
(defun rational-multiply (q1 q2)
  (rational-new 
    (* (rational-numerator q1)
       (rational-numerator q2))
    (* (rational-denominator q1)
       (rational-denominator q2))
    t))

(defun rational-divide (q1 q2)
  (rational-multiply q1 (rational-invert q2)))
;;(rational-divide (rational-new 4 2) (rational-new 2 4) (rational-new 4 2))

(defun rational-add (q1 q2)
  (let* ((a (rational-numerator q1))
	 (b (rational-denominator q1))
	 (c (rational-numerator q2))
	 (d (rational-denominator q2))
	 (e (gcd b d))
	 (b* (/ b e))
	 (d* (/ d e)))
    (rational-new (+ (* a d*)
		      (* b* c))
		   (* b* d* e)
		   t)))

(defun rational-subtract (q1 q2)
  (rational-add q1 (rational-negate q2)))

(defun rational-% (q1 q2) 
  (let ((q (rational-divide q1 q2)))
    (% (rational-numerator q1) (rational-numerator q2))))

(defun rational-mod (q modulo)
  "Returns modulo of a rational number.
The formula is, for a number Q = a/b, let m = b*MODULO, then
Q mod MODULO is defined as (mod a*b m)/m."
  (let* ((a (rational-numerator q))
	 (b (rational-denominator q))
	 (ab (* a b))
	 (bm (* b modulo)))
    (rational-new (mod ab bm) bm t)))
;;(rational-mod (rational-new 11 3) 3)

(defun rational-expt (x y)
  (if (and (rational-p x) (integerp y))
    (rational-new (expt (rational-numerator x) y)
		  (expt (rational-denominator x) y) t)
    (expt x (if (rational-p y) (rational-to-float y) y))))

;;; unary functions
(defun rational-negate (q)
  (rational-new (- (rational-numerator q))
		(rational-denominator q)))
(defun rational-abs (q) 
  (let ((q* (rational-copy q)))
    (absf (rational-numerator q*))
    q*))
(defun rational-1+ (q) (rational-add q (rational-from-integer 1)))
(defun rational-1- (q) (rational-add q (rational-from-integer -1)))

(defun rational-operate-binary (x y rational-binop default-binop)
  (aif (rational-from-symbol x)
    (rational-operate-binary it y rational-binop default-binop)
    (aif (rational-from-symbol y)
      (rational-operate-binary x it rational-binop default-binop)
      (if (rational-p x)
	(if (rational-p y)
	  (funcall rational-binop x y)
	  (case (type-of y)
	    (integer (funcall rational-binop x (rational-from-integer y)))
	    (float (funcall default-binop (rational-to-float x) y))
	    (otherwise (funcall default-binop (rational-to-float x) y))))
	(if (rational-p y)
	  (rational-operate-binary y x rational-binop default-binop)
	  (funcall default-binop x y))))))
;;(rational-add (rational-new 1 4) (rational-new -1 6))

(defun rational-reduce (args rational-binop default-binop)
  (reduce #'(lambda (x y) 
	      (rational-operate-binary x y rational-binop default-binop))
	  args))


;;;; Advising stuff in order to modify all standard numerical functions (+ - * /) and printing
(defconst rational-functions-float '(sin cos tan asin acos atan exp log log10 sqrt truncate floor ceiling round mod = /= < > <= >= equalp zerop))
(defconst rational-functions-equal '(eq eql equal))
(defconst rational-cl-functions '(floor* ceiling* truncate* round* mod* rem*))

(defmacro rational-advise-body (form)
  `(progn
     (ad-deactivate-regexp "rational-advise")
     (setf ad-return-value ,form)
     (ad-activate-regexp "rational-advise")))

;;special advisors for the fundamental operators
(defadvice + (around rational-advise)
  (rational-advise-body (rational-reduce (ad-get-args 0) #'rational-add #'+)))
;;(+ (rational-new 1 2) (rational-new 1 2))
(defadvice - (around rational-advise)
  (rational-advise-body (rational-reduce (ad-get-args 0) #'rational-subtract #'-)))
(defadvice * (around rational-advise)
  (rational-advise-body (rational-reduce (ad-get-args 0) #'rational-multiply #'*)))
(defadvice / (around rational-advise)
  (let ((a (ad-get-arg 0))
	(b (apply #'* (ad-get-args 1))))
    (rational-advise-body (rational-operate-binary a b  #'rational-divide #'/))))
;(= '1/3 '1/4)

;;; unary functions where rational number is interpreted as float, see `rational-unary-functions-float'
(defmacro def-rational-advices-unary-function-float (function-name)
  (let ((fn (eval function-name)))
    `(defadvice ,fn (around rational-advise)
       (rational-advise-body (apply (function ,fn) (mapcar #'rational-parse-to-number (ad-get-args 0)))))))
(loop for f in rational-functions-float do (def-rational-advices-unary-function-float f))

(defadvice prin1 (around rational-advise)
  (rational-advise-body (prin1 (if (rational-p (ad-get-arg 0))
				 (rational-to-symbol (ad-get-arg 0))
				 (ad-get-arg 0))
			       (ad-get-arg 1))))

;;now re-evalue cl defs, since they are compiled with an austere type checking...
(mapcar #'re-evaluate-function rational-cl-functions)

(ad-activate-regexp "rational-advise")
;(= '1/2 .5)
;(mod* '5/3 '1/1)
;(zerop '0/3)
;(rational-mod '5/3 '1/1)
(provide 'mb-rational)

(defun* rational-from-float (x &key (min-denumerator 1) (max-denumerator 100) (epsilon nil))
  (let ((res (loop with min-diff = 1.0e+INF
		   with q = 0
		   for b from min-denumerator to max-denumerator
		   for a = (round (* x b))
		   for diff = (abs (- (/ (float a) b) x))
		   if (< diff min-diff) do (setf min-diff diff
						 q (rational-new a b))
		   if (and epsilon (< diff epsilon)) return q
		   finally return q)))
    (or res (rational-new (round (* x max-denumerator)) max-denumerator t))))
;(rational-from-float pi :min-denumerator 99000 :max-denumerator 100000)

(defun rational-best-approximations (x max-denumerator)
  (loop for d = max-denumerator then (1- (rational-denominator q))
	for q = (rational-from-float x :max-denumerator d)
	while (> (rational-denominator q) 1)
	collect q))
;(rational-best-approximations pi 100000) 
;;(pi 16604)