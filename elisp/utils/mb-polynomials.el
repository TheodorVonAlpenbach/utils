;;;; Simple polynomial representation and manipulation A polynomial is
;;;; represented as a list (a_0 a_1 ... a_N) where a_i, i in [0 N], is
;;;; the i-th coefficient of the polynomial.

;;;; Also, polynomial fractions are represented as a pair of lists
;;;; (numerator denomintator) where both elements are representations
;;;; of polynomials as described above.

(defun polynom-p (p)
  (and (listp p)
       (plusp (length p))
       (numberp (first p))))
;;(mapcar #'polynom-p '(nil () (1) (2 3) (a 1)))

(defun polynom-clean (p)
  (let ((pos (position 0 p :test-not #'= :from-end t)))
    (if pos
      (subseq p 0 (1+ pos))
      '())))
;;(polynom-clean '(0))

(defun polynom-order (p)
  (1- (length (polynom-clean p))))
;;(polynom-order '(1 0 0))

(defun polynom-align (&rest polynomials)
  (pad-lists polynomials))
;;(polynom-align '(1 2 3) '(1))

(defun polynom-addition (&rest polynomials)
  (apply #'mapcar* #'+ (pad-lists polynomials)))
;;(polynom-addition '(1 2 1) '(1))

(defun polynom-scalar-multiplication (polynomial scalar)
  "Multiply all coefficients in POLYNOMIAL with SCALAR"
  (mapcar (bind #'* scalar) polynomial))
;;(polynom-scalar-multiplication '(1 2 3) 123)

(defun polynom-subtraction (&rest polynomials)
  (apply #'polynom-addition
	 (first polynomials)
	 (mapcar (bind #'polynom-scalar-multiplication -1) (rest polynomials))))
;;(polynom-subtraction '(1 2 1) '(1))

(defun polynom-multiplication-1 (p q)
  (destructuring-bind (p q) (polynom-align p q)
    (polynom-clean
     (apply #'polynom-addition
	    (loop for a in p
		  for order from 0
		  collect (append (make-list order 0)
				  (polynom-scalar-multiplication q a)))))))

(defun polynom-multiplication (&rest polynomials)
  (reduce #'polynom-multiplication-1 polynomials :initial-value '(1)))
;;(polynom-multiplication)

(defun polynom-expt (p exponent)
  (assert (not (minusp exponent)))
  (apply #'polynom-multiplication (make-list exponent p)))
;;(polynom-expt '(1 1) 0)

(defun polynom-division-1 (num den)
  (let ((d (- (polynom-order num) (polynom-order den)))
	(num (polynom-clean num))
	(den (polynom-clean den)))
    (unless (minusp d)
      (let* ((q (nconc (make-list d 0) (list (/ (float (last-elt num)) (last-elt den)))))
	     (r (polynom-subtraction num (polynom-multiplication q den))))
	(list q r)))))
;;(polynom-division-1 '(0 -1 0) '(1 1))

(defun polynom-division (num den)
  (loop for num* = num then r
	for (q r) = (polynom-division-1 num* den)
	while q collect q into res
	finally return (mapcar #'polynom-clean (list (apply #'polynom-addition res) num* den))))
;;(polynom-division '(0 0 1) '(1 1))

(defun polynom-derivative (p)
  (loop for a in (rest p)
	for i from 1
	collect (* i a)))
;;(polynom-derivative '(2 2 2))

(cl-defun test-polynom-division (&optional (num '(0 0 1)) (den '(1 1)))
  (destructuring-bind (q rem den) (polynom-division num den)
    (mapcar* #'= num (polynom-addition 
		     (polynom-multiplication q den)
		     rem))))
;;(test-polynom-division)

(defun pole-polynom (poles)
  "Returns the polynomial defined by the POLES a1, a2, ... aN as
\(x - a1)(x - a2)...(x - aN)"
  (reduce #' polynom-multiplication 
	     (mapcar #'(lambda (pole) (list (- pole) 1)) poles)))
;;(pole-polynom '(-1 1))

(cl-defun print-number (x &optional (precision 2))
  "Formats a general number. Integers are formatted as integers,
and floats are formatted with PRECISION"
  (if (floatp x)
    (format "%.2f" x)
    (number-to-string x)))

(cl-defun ppolynom (p &key (variable "x") (multiplication "") (exponent "^"))
  (cl-flet ((pterm (i a symbol)
	   (when (not (zerop a))
	     (let ((sa (if (= a 1) "" (concat (print-number a) multiplication))))
	       (case i
		 (0 (format "%s" (print-number a)))
		 (1 (format "%s%s" sa variable))
		 (t (format "%s%s%s%d" sa variable exponent i)))))))
    (concat* (loop for i from 0 
		   for a in p
		   if (pterm i a variable) collect it)
      :in " + ")))
;;(ppolynom (list 1 1 pi) :multiplication "*" :exponent "**")

(defalias #'p* 'polynom-multiplication)
(defalias #'p+ 'polynom-addition)
(defalias #'p- 'polynom-subtraction)
(defalias #'p^ 'polynom-expt)

;;; Polynomial fractions
(defun fpolynom (numerator denomintator)
  (list numerator denomintator))

(defsubst fp* (qp1 qp2) (mapcar* #'p* qp1 qp2))
;(fp* (qpolynom '(1 1) '(1 1)) (fpolynom '(1 1) '(1 1)))

(defun polynom-expand (p order)
  (let ((n (polynom-order p)))
    (assert (<= n order))
    (append p (make-list (- order n) 0))))
;;(polynom-expand '(1 1) 4)

(defun fp-expand (fp order)
  (mapcar (bind #'polynom-expand order) fp))
;;(fp-expand '((1 2 3 4) (1)) 4)

(defun fp-order (fp)
  (max (polynom-order (first fp)) (polynom-order (second fp))))

(defun fp-p (fp)
  (and (listp fp)
       (= (length fp) 2) 
       (polynom-p (first fp))
       (polynom-p (second fp))))

(cl-defun pfp (fp &rest args)
  "Prints a polynomial fraction. For ARGS, see `ppolynom'."
  (if (fp-p fp)
    (when (fp-p fp)
      (apply #'format "(%s)/(%s)" 
	     (loop for p in fp
		   collect (apply #'ppolynom p args))))
    (apply #'ppolynom fp args)))
;;(mapcar #'pfp '(((1 1) (1 2)) (1 2)))

(defun polynom-value-old (p x)
  (apply #'+
   (loop for coefficient in p
	 for i from 0
	 collect (* coefficient (expt x i)))))

(defun expt-sequence (base order)
  (loop for i to order 
	for term = 1 then (* term base)
	collect term))
;;(expt-sequence 2 10)

(require 'mb-utils-matrix)
(defun polynom-value (p x)
  (scalar-product p (expt-sequence x (polynom-order p))))
;;(with-complex (polynom-value '(1 1) '(0 1)))

(defun fp-value (fp x)
  (if (fp-p fp)
    (/ (polynom-value (first fp) x)
       (polynom-value (second fp) x))
    (polynom-value fp x)))

(defun fp-value (fp x)
  (if (fp-p fp)
    (apply #'/ (mapcar (bind #'scalar-product (expt-sequence x (fp-order fp))) fp))
    (polynom-value fp x)))
;;(fp-value '((1 2 1) (1 -2 1)) 2)

;; (defun ewq (x y) (+ x y))

;; (let ((fn1 'ewq)
;;       (fn2 '-)
;;       a b c)
;;   (fset 'qwe fn1)
;;   (setf a (qwe 2 2))
;;   (fset 'qwe fn2)
;;   (setf b (qwe 2 2))
;;   (fset 'qwe fn1)
;;   (setf c (qwe 2 2))
;;   (list a b c))

(provide 'mb-polynomials)
