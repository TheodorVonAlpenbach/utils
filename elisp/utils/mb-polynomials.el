;;; mb-polynomials.el --- Polynomial arithmetic and formatting -*- lexical-binding: t; -*-

;;; Commentary:

;; Polynomials are represented by coefficient lists in ascending order:
;; (A0 A1 ... AN) represents A0 + A1*x + ... + AN*x^N.
;; Polynomial fractions are represented as (NUMERATOR DENOMINATOR).

;;; Code:

(require 'cl-lib)

(defun polynom-p (polynomial)
  "Return non-nil when POLYNOMIAL is a nonempty list of numbers."
  (and (consp polynomial)
       (proper-list-p polynomial)
       (cl-every #'numberp polynomial)))

(defun mb-polynomials--check (polynomial)
  "Signal an error unless POLYNOMIAL is a valid polynomial."
  (unless (polynom-p polynomial)
    (signal 'wrong-type-argument (list 'polynom-p polynomial))))

(defun polynom-clean (polynomial)
  "Remove trailing zero coefficients from POLYNOMIAL.
Represent the zero polynomial canonically as (0)."
  (mb-polynomials--check polynomial)
  (let ((result (copy-sequence polynomial)))
    (while (and (cdr result) (zerop (car (last result))))
      (setq result (butlast result)))
    result))

(defun polynom-order (polynomial)
  "Return the order of POLYNOMIAL."
  (1- (length (polynom-clean polynomial))))

(defun horner (polynomial x)
  "Evaluate POLYNOMIAL at X using Horner's method."
  (mb-polynomials--check polynomial)
  (cl-loop for coefficient in (reverse polynomial)
           for sum = coefficient then (+ (* sum x) coefficient)
           finally return sum))

(defun polynom-align (&rest polynomials)
  "Return POLYNOMIALS padded with trailing zero coefficients."
  (dolist (polynomial polynomials)
    (mb-polynomials--check polynomial))
  (when polynomials
    (let ((width (apply #'max (mapcar #'length polynomials))))
      (mapcar (lambda (polynomial)
                (append polynomial
                        (make-list (- width (length polynomial)) 0)))
              polynomials))))

(defun polynom-addition (&rest polynomials)
  "Return the sum of POLYNOMIALS."
  (if polynomials
      (polynom-clean
       (apply #'cl-mapcar #'+ (apply #'polynom-align polynomials)))
    '(0)))

(defun polynom-scalar-multiplication (polynomial scalar)
  "Multiply every coefficient in POLYNOMIAL by SCALAR."
  (mb-polynomials--check polynomial)
  (unless (numberp scalar)
    (signal 'wrong-type-argument (list 'numberp scalar)))
  (polynom-clean
   (mapcar (lambda (coefficient) (* coefficient scalar)) polynomial)))

(defun polynom-subtraction (&rest polynomials)
  "Subtract each polynomial after the first from the first.
With one argument, return that polynomial unchanged.  With no
arguments, return the zero polynomial."
  (pcase polynomials
    ('nil '(0))
    (`(,polynomial) (polynom-clean polynomial))
    (`(,first . ,rest)
     (apply #'polynom-addition
            first
            (mapcar (lambda (polynomial)
                      (polynom-scalar-multiplication polynomial -1))
                    rest)))))

(defun polynom-multiplication-1 (p q)
  "Return the product of polynomials P and Q."
  (mb-polynomials--check p)
  (mb-polynomials--check q)
  (let ((result (make-list (1- (+ (length p) (length q))) 0)))
    (cl-loop for a in p
             for i from 0
             do (cl-loop for b in q
                         for j from 0
                         do (cl-incf (nth (+ i j) result) (* a b))))
    (polynom-clean result)))

(defun polynom-multiplication (&rest polynomials)
  "Return the product of POLYNOMIALS."
  (cl-reduce #'polynom-multiplication-1
             polynomials
             :initial-value '(1)))

(defun polynom-expt (polynomial exponent)
  "Raise POLYNOMIAL to the nonnegative integer EXPONENT."
  (unless (and (integerp exponent) (>= exponent 0))
    (error "Polynomial exponent must be a nonnegative integer: %S" exponent))
  (mb-polynomials--check polynomial)
  (cl-loop with result = '(1)
           repeat exponent
           do (setq result (polynom-multiplication-1 result polynomial))
           finally return result))

(defun mb-polynomials--zero-p (polynomial)
  "Return non-nil when POLYNOMIAL is the zero polynomial."
  (equal (polynom-clean polynomial) '(0)))

(defun mb-polynomials--divide-numbers (numerator denominator)
  "Divide NUMERATOR by DENOMINATOR without truncating integer results."
  (when (zerop denominator)
    (error "Division by zero"))
  (if (and (integerp numerator)
           (integerp denominator)
           (zerop (% numerator denominator)))
      (/ numerator denominator)
    (/ numerator (float denominator))))

(defun polynom-division-1 (numerator denominator)
  "Perform one long-division step on NUMERATOR and DENOMINATOR.
Return (QUOTIENT-TERM REMAINDER), or nil when NUMERATOR has lower
order than DENOMINATOR."
  (let ((num (polynom-clean numerator))
        (den (polynom-clean denominator)))
    (when (mb-polynomials--zero-p den)
      (error "Cannot divide by the zero polynomial"))
    (let ((degree (- (polynom-order num) (polynom-order den))))
      (unless (or (mb-polynomials--zero-p num)
                  (< degree 0))
        (let* ((coefficient
                (mb-polynomials--divide-numbers
                 (car (last num))
                 (car (last den))))
               (quotient-term
                (append (make-list degree 0) (list coefficient)))
               (remainder
                (polynom-subtraction
                 num
                 (polynom-multiplication quotient-term den))))
          ;; The leading terms cancel by construction.  Set the result
          ;; explicitly to zero to avoid a floating-point residue.
          (let ((leading-index (+ degree (polynom-order den))))
            (when (< leading-index (length remainder))
              (setf (nth leading-index remainder) 0)))
          (list quotient-term (polynom-clean remainder)))))))

(defun polynom-division (numerator denominator)
  "Divide NUMERATOR by DENOMINATOR.
Return (QUOTIENT REMAINDER DENOMINATOR), with all three
polynomials in canonical form."
  (let ((remainder (polynom-clean numerator))
        (den (polynom-clean denominator))
        (quotient '(0))
        step)
    (when (mb-polynomials--zero-p den)
      (error "Cannot divide by the zero polynomial"))
    (while (setq step (polynom-division-1 remainder den))
      (setq quotient (polynom-addition quotient (cl-first step))
            remainder (cl-second step)))
    (list (polynom-clean quotient) remainder den)))

(defun polynom-derivative (polynomial)
  "Return the derivative of POLYNOMIAL."
  (mb-polynomials--check polynomial)
  (if (cdr polynomial)
      (polynom-clean
       (cl-loop for coefficient in (cdr polynomial)
                for exponent from 1
                collect (* exponent coefficient)))
    '(0)))

(cl-defun test-polynom-division (&optional
                                 (numerator '(0 0 1))
                                 (denominator '(1 1)))
  "Return coefficient comparisons verifying polynomial division."
  (cl-destructuring-bind (quotient remainder divisor)
      (polynom-division numerator denominator)
    (cl-destructuring-bind (expected actual)
        (polynom-align
         numerator
         (polynom-addition
          (polynom-multiplication quotient divisor)
          remainder))
      (cl-mapcar #'= expected actual))))

(defun pole-polynom (poles)
  "Return the monic polynomial whose roots are POLES."
  (cl-reduce #'polynom-multiplication
             (mapcar (lambda (pole) (list (- pole) 1)) poles)
             :initial-value '(1)))

(cl-defun print-number (number &optional (precision 2))
  "Format NUMBER, using PRECISION decimal places for floats."
  (unless (and (integerp precision) (>= precision 0))
    (error "Precision must be a nonnegative integer: %S" precision))
  (if (floatp number)
      (format (format "%%.%df" precision) number)
    (number-to-string number)))

(cl-defun ppolynom (polynomial
                    &key
                    (variable "x")
                    (multiplication "")
                    (exponent "^"))
  "Format POLYNOMIAL as an algebraic expression.
VARIABLE is the indeterminate name.  MULTIPLICATION is inserted
between a displayed coefficient and VARIABLE, while EXPONENT is
inserted before powers greater than one."
  (setq polynomial (polynom-clean polynomial))
  (let (terms)
    (cl-loop for coefficient in polynomial
             for degree from 0
             unless (zerop coefficient)
             do
             (let* ((negative (< coefficient 0))
                    (magnitude (abs coefficient))
                    (body
                     (cond
                      ((zerop degree)
                       (print-number magnitude))
                      ((= magnitude 1)
                       (if (= degree 1)
                           variable
                         (format "%s%s%d" variable exponent degree)))
                      (t
                       (format "%s%s%s%s"
                               (print-number magnitude)
                               multiplication
                               variable
                               (if (= degree 1)
                                   ""
                                 (format "%s%d" exponent degree)))))))
               (push (cons negative body) terms)))
    (setq terms (nreverse terms))
    (if (null terms)
        "0"
      (concat
       (if (caar terms) "-" "")
       (cdar terms)
       (mapconcat (lambda (term)
                    (concat (if (car term) " - " " + ") (cdr term)))
                  (cdr terms)
                  "")))))

(defalias 'p* #'polynom-multiplication)
(defalias 'p+ #'polynom-addition)
(defalias 'p- #'polynom-subtraction)
(defalias 'p^ #'polynom-expt)

(defun fpolynom (numerator denominator)
  "Construct a polynomial fraction from NUMERATOR and DENOMINATOR."
  (list numerator denominator))

(defun fp-p (fraction)
  "Return non-nil when FRACTION is a valid polynomial fraction."
  (and (proper-list-p fraction)
       (= (length fraction) 2)
       (polynom-p (cl-first fraction))
       (polynom-p (cl-second fraction))
       (not (mb-polynomials--zero-p (cl-second fraction)))))

(defun fp* (fraction-1 fraction-2)
  "Multiply polynomial fractions FRACTION-1 and FRACTION-2."
  (unless (and (fp-p fraction-1) (fp-p fraction-2))
    (error "Invalid polynomial fraction"))
  (cl-mapcar #'polynom-multiplication fraction-1 fraction-2))

(defun polynom-expand (polynomial order)
  "Pad POLYNOMIAL with zero coefficients through ORDER."
  (unless (and (integerp order) (>= order 0))
    (error "Polynomial order must be a nonnegative integer: %S" order))
  (let* ((clean (polynom-clean polynomial))
         (current-order (polynom-order clean)))
    (when (> current-order order)
      (error "Cannot expand order %d polynomial to order %d"
             current-order order))
    (append clean (make-list (- order current-order) 0))))

(defun fp-expand (fraction order)
  "Pad both polynomials in FRACTION through ORDER."
  (unless (fp-p fraction)
    (error "Invalid polynomial fraction"))
  (mapcar (lambda (polynomial)
            (polynom-expand polynomial order))
          fraction))

(defun fp-order (fraction)
  "Return the greatest order among the polynomials in FRACTION."
  (unless (fp-p fraction)
    (error "Invalid polynomial fraction"))
  (max (polynom-order (cl-first fraction))
       (polynom-order (cl-second fraction))))

(defun pfp (fraction-or-polynomial &rest arguments)
  "Format FRACTION-OR-POLYNOMIAL.
Pass ARGUMENTS to `ppolynom'."
  (if (fp-p fraction-or-polynomial)
      (format "(%s)/(%s)"
              (apply #'ppolynom (cl-first fraction-or-polynomial) arguments)
              (apply #'ppolynom (cl-second fraction-or-polynomial) arguments))
    (apply #'ppolynom fraction-or-polynomial arguments)))

(defun expt-sequence (base order)
  "Return powers of BASE from zero through ORDER."
  (unless (and (integerp order) (>= order 0))
    (error "Order must be a nonnegative integer: %S" order))
  (cl-loop repeat (1+ order)
           for term = 1 then (* term base)
           collect term))

(defun polynom-value (polynomial x)
  "Evaluate POLYNOMIAL at X."
  (horner polynomial x))

(defun fp-value (fraction-or-polynomial x)
  "Evaluate FRACTION-OR-POLYNOMIAL at X."
  (if (fp-p fraction-or-polynomial)
      (mb-polynomials--divide-numbers
       (polynom-value (cl-first fraction-or-polynomial) x)
       (polynom-value (cl-second fraction-or-polynomial) x))
    (polynom-value fraction-or-polynomial x)))

(provide 'mb-polynomials)

;;; mb-polynomials.el ends here
