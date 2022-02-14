(require 'cl-utils "~/git/utils/elisp/div/euler-project/cl-utils.lisp")
(require 'progress "~/git/utils/elisp/div/euler-project/progress.lisp")
(require '10000-first-primes "~/git/utils/elisp/div/euler-project/10000-first-primes.lisp")
(require 'factor-table "~/git/utils/elisp/div/euler-project/factor-table.lisp")

(defun b-candidates (b-max)
  (loop for p in 10000-first-primes while (< p b-max) collect p))
;;(b-candidates 10)

(defun 027-quadratic (n a b)
  (+ (sq n) (* a n) b))
;;(loop for i below 10 collect (027-quadratic i 1 41))

(defun 027-prime-p (n prime-p)
  (and (> n 1) (= (bit prime-p n) 1)))

(defun 027-length (x)
  (length (first x)))

(defun 027-maximize-b (a b-candidates prime-p)
  (multiple-value-bind (max pos max-value)
      (maximum
       (loop for b-candidate in b-candidates collect
	     (list (remove-duplicates
		       (loop for n from 0
			     for p-candidate = (027-quadratic n a b-candidate)
			     while (027-prime-p p-candidate prime-p)
			     collect p-candidate))
		   a b-candidate))
       :key #'027-length)
    max-value))
;;(027-maximize-b 1 (b-candidates 100) (make-prime-p (027-quadratic 100 1 100)))
;;(maximum (027-maximize-b 1 (b-candidates 100) (make-prime-p (027-quadratic 100 1 100))) :key #'length)

(defun make-prime-p (n)
  (let ((prime-p (make-array n :element-type 'bit :initial-element 0))
	(ft (make-factor-table n)))
    (loop for i below n
	  if (factor-table-row-empty-p ft i)
	  do (setf (bit prime-p i) 1))
    prime-p))
;;(make-prime-p 10)

(defun 027-solution (&optional (a 1000) (b 1000))
  "n^2 + an + b"
  (let* ((b-candidates (b-candidates b))
	 (prime-p (make-prime-p (027-quadratic b a b)))
	 (maxes-per-a (loop for a in (a-b (- 1 a) a)
			    collect (027-maximize-b a b-candidates prime-p))))
    (maximum maxes-per-a :key #'027-length)))
;;(time (027-solution 1000 1000))
;; => 983
