;;; mb-combinatorics.el --- Combinatorial utility functions -*- lexical-binding: t; -*-

;;; Commentary:

;; Exact integer sequences, coefficients, divisor sums, and related
;; combinatorial helpers.

;;; Code:

(require 'cl-lib)
(require 'mb-utils-math)

(defun mb-combinatorics--check-nonnegative-integer (value name)
  "Signal an error unless VALUE named NAME is a non-negative integer."
  (unless (and (integerp value) (>= value 0))
    (error "%s must be a non-negative integer: %S" name value)))

(cl-defun factorial (n &optional (m 1))
  "Return the product M through N.
With the default M of 1, return N!.  See also `derangement'."
  (mb-combinatorics--check-nonnegative-integer n "N")
  (mb-combinatorics--check-nonnegative-integer m "M")
  (cond
    ((zerop n) 1)
    (t (product-a-b m n))))
;;(mapcar #'factorial (0-n 10))
;; (* 1.0 (factorial 5910))

(cl-defun derangement (n &optional (initial-zero 1) (initial-one 0))
  "Return the subfactorial !N.
INITIAL-ZERO and INITIAL-ONE override the first two sequence values."
  (mb-combinatorics--check-nonnegative-integer n "N")
  (cl-case n
    (0 initial-zero)
    (1 initial-one)
    (otherwise
     (cl-loop for i from 2 to n
	      for Ln-2 = initial-zero then Ln-1
	      for Ln-1 = initial-one then Ln
	      for Ln = (* (1- i) (+ Ln-1 Ln-2))
	      finally return Ln))))
;;(mapcar #'derangement (0-n 11))

(cl-defun factorials (n &optional (m 1))
  "Return M!, (M + 1)!, ... N!.
By default, M is 1."
  (mb-combinatorics--check-nonnegative-integer n "N")
  (mb-combinatorics--check-nonnegative-integer m "M")
  (cl-loop for i from m to n
	   for p = (factorial m) then (* p i)
	   collect p))
;;(factorials 5 3)

(cl-defun fibonacci-numbers (n &optional (start-values '(0 1)))
  "Return the N first Fibonacci numbers.
START-VALUES must contain exactly the first two numeric values."
  (mb-combinatorics--check-nonnegative-integer n "N")
  (unless (and (proper-list-p start-values)
               (= (length start-values) 2)
               (cl-every #'numberp start-values))
    (error "START-VALUES must contain exactly two numbers: %S" start-values))
  (if (< n 2)
      (cl-subseq start-values 0 n)
    (let ((res (reverse start-values)))
      (cl-loop repeat (- n 2)
	       do (push (+ (cl-first res) (cl-second res)) res))
      (nreverse res))))
;;(fibonacci-numbers 0)

(defun mb-combinatorics--fibonacci-pair (n)
  "Return the cons (F(N) . F(N + 1)) using fast doubling."
  (if (zerop n)
      '(0 . 1)
    (pcase-let* ((`(,a . ,b)
                  (mb-combinatorics--fibonacci-pair (/ n 2)))
                 (c (* a (- (* 2 b) a)))
                 (d (+ (* a a) (* b b))))
      (if (cl-evenp n)
          (cons c d)
        (cons d (+ c d))))))

(defun fibonacci-nth (n)
  "Return the exact Nth Fibonacci number."
  (mb-combinatorics--check-nonnegative-integer n "N")
  (car (mb-combinatorics--fibonacci-pair n)))
;;(time (fibonacci-nth 50000))

(defun simplify-ratio (ratio)
  "Divide the integer elements of RATIO by their greatest common divisor."
  (unless (and (proper-list-p ratio) ratio (cl-every #'integerp ratio))
    (error "RATIO must be a nonempty list of integers: %S" ratio))
  (let ((divisor (apply #'cl-gcd ratio)))
    (when (zerop divisor)
      (error "Cannot simplify an all-zero ratio"))
    (mapcar (lambda (value) (/ value divisor)) ratio)))
;;(simplify-ratio '(6 10))

(defun aliquot-sum (n)
  "Return the sum of the proper positive divisors of N."
  (unless (and (integerp n) (> n 0))
    (error "N must be a positive integer: %S" n))
  (sum (all-factors n t)))
;;(mapcar #'aliquot-sum (1-n 10))

(defun amicable-numbers-p (x y)
  "Return non-nil when distinct positive integers X and Y are amicable."
  (and (/= x y)
       (= (aliquot-sum x) y)
       (= (aliquot-sum y) x)))
;;(amicable-numbers-p 220 284)

(defun abundancy-index (n)
  "Return the abundancy index of positive integer N as a reduced ratio."
  (unless (and (integerp n) (> n 0))
    (error "N must be a positive integer: %S" n))
  (simplify-ratio (list (sum (all-factors n)) n)))
;;(mapcar #'abundancy-index '(220 284 30))

(cl-defun remove-factors (factors divisor)
  "Remove from FACTORS all factors shared with DIVISOR."
  (cl-loop for factor in factors
	for d = divisor then (/ d cl-gcd)
	for cl-gcd = (cl-gcd factor d)
        collect (/ factor cl-gcd)))
;;(remove-factors '(2 3 4 5) 12)

(cl-defun binomial-coefficient (n m &optional (method :auto))
  "Return the binomial coefficient N choose M.
METHOD is passed to `product*'.  Return zero when integer M is outside
the range from zero through N."
  (mb-combinatorics--check-nonnegative-integer n "N")
  (unless (integerp m)
    (error "M must be an integer: %S" m))
  (if (or (< m 0) (> m n))
      0
    (let* ((m* (max m (- n m)))
	   (factors
	    (cl-loop for denominator-factor in (a-b 1 (- n m*))
		     for numerator-factors = (a-b (1+ m*) n)
		     then (remove-factors numerator-factors denominator-factor)
		     finally return numerator-factors)))
      (product* factors :method method))))
;;(time (binomial-coefficient 112 100 :auto))

(cl-defun binomial-coefficients-n (n1 n2 m &optional (method :auto))
  "Return N1 choose M through N2 choose M.
METHOD is passed to `binomial-coefficient'."
  (cl-loop for n from n1 to n2
           collect (binomial-coefficient n m method)))
;;(binomial-coefficients-n 4 6 3)

(cl-defun binomial-coefficients-m (n m1 m2 &optional (method :auto))
  "Return N choose M1 through N choose M2.
METHOD is passed to `binomial-coefficient'."
  (cl-loop for m from m1 to m2
           collect (binomial-coefficient n m method)))
;;(binomial-coefficients-m 6 0 5)
;;(binomial-coefficient 5 2)

(cl-defun catalan-nth (n)
  "Return the Nth Catalan number.
The Catalan numbers form the sequence A000108 in the OEIS."
  (mb-combinatorics--check-nonnegative-integer n "N")
  (/ (binomial-coefficient (2* n) n) (1+ n)))
;;(catalan-nth 3)

(cl-defun stirling-numbers-2 (n k)
  "Return the Stirling number of the second kind S(N, K).
This is the number of partitions of N labeled elements into K
nonempty subsets."
  (mb-combinatorics--check-nonnegative-integer n "N")
  (mb-combinatorics--check-nonnegative-integer k "K")
  (if (> k n)
      0
    (let ((values (make-vector (1+ k) 0)))
      (aset values 0 1)
      (cl-loop for i from 1 to n
               do
               (cl-loop for j from (min i k) downto 1
                        do (aset values j
                                 (+ (* j (aref values j))
                                    (aref values (1- j)))))
               (aset values 0 0))
      (aref values k))))
;;(stirling-numbers-2 0 0)
;;(cl-loop for k to 10 collect (stirling-numbers-2 10 k))

(cl-defun stirling-numbers-1 (n k)
  "Return the unsigned Stirling number of the first kind C(N, K).
This is the number of permutations of N elements with K disjoint cycles."
  (mb-combinatorics--check-nonnegative-integer n "N")
  (mb-combinatorics--check-nonnegative-integer k "K")
  (if (> k n)
      0
    (let ((values (make-vector (1+ k) 0)))
      (aset values 0 1)
      (cl-loop for i from 1 to n
               do
               (cl-loop for j from (min i k) downto 1
                        do (aset values j
                                 (+ (aref values (1- j))
                                    (* (1- i) (aref values j)))))
               (aset values 0 0))
      (aref values k))))
;;(cl-loop for k to 4 collect (stirling-numbers-1 4 k))

(provide 'mb-combinatorics)

;;; mb-combinatorics.el ends here
