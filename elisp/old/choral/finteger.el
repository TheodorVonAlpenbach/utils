;;;; mb-rational.el --- Rational number in prime factor group format.

;;;; Copyright (C) 2012, Mats Bergstrøm

;;;; A rational number is a form q = a/b where a and b are integers
;;;; and b not zero, see http://en.wikipedia.org/wiki/Rational_number

;;;; The internal format is a list of pairs ((p1 nxp1) (p2 nxp2)),
;;;; where p1, p2 ... represent primes and nxp1, nxp2 is the number of
;;;; factors in in q respectively. A positive and negative nxpi means
;;;; that the factor is part of the numerator or denominator
;;;; respectively.

;;;; The approach here is to make the implemenation fast, not the
;;;; calculations. Some of the considerations that should be revised
;;;; to optimize calculation speed are:

;;;; 1. Representation: The number is represented as a list of list
;;;; only. However, since most numbers contain the smallest primes, an
;;;; alternative could be to have a fixed length vector in addition to
;;;; the list. The vector's slot number i then represents the i'th
;;;; prime. Additional factors are handled with pairs as of today.
;;;; For a vector length of, say, 8, a the number 66/23 is thus represented:
;;;; ([1 1 0 0 1 0 0 0] (23 -1)), ie. 2*3*11/23

;;;; 2. Factorization: When a rational number is parsed, it is always
;;;; immediately facorized to the above described format. Another
;;;; approach is to factorize on demand only.

;;;; 3. Number limit control: When adding to rational numbers, the
;;;; formula a/b + c/d = (ad + bc)/bd (and then factorizing) is used.
;;;; Because of possible common factors in the resulting numerator and
;;;; denominator, this could lead to an unnecessary overflow. This
;;;; should be adressed and amended later if needed.

;;;; However, a preprocessed list (in a defconst) is provided,
;;;; containing the first N integers

;;;; RATIONAL := FINTEGER
;;;; FINTEGER := (SIGN (PRIME-FACTOR-GROUP PRIME-FACTOR-GROUP ...))
;;;; SIGN := 1 | -1
;;;; PRIME-FACTOR-GROUP := (P EXPT)
;;;; P := PRIME-NUMBER
;;;; EXPT := INTEGER (/= 0)

(provide 'finteger)

(require 'mb-utils-math)
(require 'mb-utils-div)

(defun finteger-signum (finteger)
  (first finteger))

(defun finteger-prime-factor-groups (finteger)
  (second finteger))

(defun pfg-prime (prime-factor-group)
  (first prime-factor-group))

(defun pfg-exponent (prime-factor-group)
  (second prime-factor-group))

(defun factors-to-finteger (factors &optional signum sorted-p)
  "Factors must be sorted from least to greatest.
Assume that the first factor carry the signum of the product of FACTORS"
  (list (or signum 1)
	(loop for g in (group (if sorted-p factors (sort factors #'<)))
	      collect (list (first g) (length g)))))
;;(factors-to-finteger '(2 3) -1)

(defun integer-to-finteger (integer)
  (factors-to-finteger (nreverse (factorize (abs integer))) (signum integer) t))
;;(integer-to-finteger 24)

(defun finteger-to-integer (finteger)
  (loop with res = 1
	for pfg in (finteger-prime-factor-groups finteger)
	do (setf res (* res (expt (pfg-prime pfg) (pfg-exponent pfg))))
	finally return (* (finteger-signum finteger) res)))
;;(finteger-to-integer (integer-to-finteger -1234))

(defun pfg-to-factors (prime-factor-group)
  (make-list (pfg-exponent prime-factor-group) (pfg-prime prime-factor-group)))

(defun pfgs-to-factors (prime-factor-groups)
  (loop for pfg in prime-factor-groups
	append (pfg-to-factors pfg)))

(defun finteger-to-factors (finteger)
  (let ((factors (pfgs-to-factors (finteger-prime-factor-groups finteger))))
    (when (minusp (finteger-signum finteger))
      (negatef (first factors)))
    factors))
;;(finteger-to-factors (integer-to-finteger -24))

(defun finteger-multiply (finteger1 finteger2)
  (factors-to-finteger (append (finteger-to-factors finteger1)
			       (finteger-to-factors finteger2))))

(defun finteger-multiply (finteger1 finteger2)
  (list (* (finteger-signum finteger1)
	   (finteger-signum finteger2))
	(mapcar #'pfg-sum (group (merge 'list
					(finteger-prime-factor-groups finteger1)
					(finteger-prime-factor-groups finteger2)
					#'< :key #'first)
				 :key #'first))))
;;(finteger-to-integer (finteger-multiply (integer-to-finteger 6) (integer-to-finteger 35)))

(defun pfg-sum (prime-factor-groups)
  "Returns the sums of the PRIME-FACTOR-GROUPS. Assumes that the prime is the same in all groups."
  (list (first (first prime-factor-groups)) 
	(apply #'+ (mapcar #'second prime-factor-groups))))

(mapcar #'pfg-sum (group (merge 'list '((2 3) (3 1)) '((2 3) (5 1)) #'< :key #'first) :key #'first))

(defun finteger-add (finteger1 finteger2)
  "Slow?"
  (integer-to-finteger (+ (finteger-to-integer finteger1)
			  (finteger-to-integer finteger2))))

(defun finteger-negate (finteger)
  (negatef (first finteger)))

(defun finteger-subtract (finteger1 finteger2)
  "Slow?"
  (integer-to-finteger (+ (finteger-to-integer finteger1)
			  (finteger-to-integer finteger2))))

(defun rational-from-fintegers (finteger1 finteger2)
  (rational-divide finteger1 finteger2))

(defun rational-from-integers (a b)
  (rational-from-fintegers (integer-to-finteger a) (integer-to-finteger b)))
;;(rational-from-integers 2 3)

(defun rational-from-symbol (form)
  "Parses a symbol representing a rational number.
It convertes all symbols whose name matches `rational-regexp' to the 
rational number format"
  (and (symbolp form)
       (let ((qs (symbol-name form)))
	 (string-match "\\([[:digit:]]+\\)/\\([[:digit:]]+\\)" qs)
	 (apply #'rational-from-integers (mapcar #'string-to-number (list (match-string 1 qs) (match-string 2 qs)))))))
;;(rational-from-symbol '1/8)

(defun rational-to-string (q)
  (let ((a rational-numerator q)
	(b rational-denominator q))
    (if (= b 1) 
      (number-to-string a)
      (format "%d/%d" a b))))

(defun rational-to-fintegers (q)
  (apply #'rational-from-fintegers (group q :key #'second :test #'minusp)))

(defun rational-to-integers (q)
  (mapcar #'finteger-to-integer (rational-to-fintegers q)))

(defun rational-multiply (q1 q2)
  (finteger-multiply q1 q2))
;;(rational-multiply (integer-to-finteger 6) (integer-to-finteger 3))

(defun nrational-inverse (q)
  "Destructive"
  (loop for pfg in-ref (finteger-prime-factor-groups q)
	do (negatef (second pfg)))
  q)
;;(nrational-inverse (integer-to-finteger 6))

(defun rational-copy (q)
  (copy-tree q))

(defun rational-inverse (q)
  (nrational-inverse (rational-copy q)))
;;(let ((q (integer-to-finteger 6))) (list q (rational-inverse q)))

(defun rational-divide (q1 q2)
  (rational-multiply q1 (rational-inverse q2)))
;;(rational-divide (integer-to-finteger 6) (integer-to-finteger 3))

(defun rational-add (q1 q2)
  (let ((a (first q1)) (b (second q1))
	(c (first q2)) (d (second q2)))
    (rational-from-fintegers (finteger-add (finteger-multiply a d)
					   (finteger-multiply b c))
			     (finteger-multiply b d))))

