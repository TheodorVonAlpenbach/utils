(require 'cl-utils "~/git/utils/elisp/div/euler-project/cl-utils.lisp")
(require 'factor-table "~/git/utils/elisp/div/euler-project/factor-table.lisp")

(defun abundant-numbers-below (n)
  "Return all abundant numbers below N"
  (init-factor-table n)
  (loop for i below n if (abundant-number-p i t) collect i))
;;(abundant-numbers-below 20)

(defun sums-of-two-abundants (&optional (n 100))
  "Return all numbers below N that is a sum of two abundant numbers."
  (let ((abundants (abundant-numbers-below n))
	(sums (make-array n :element-type 'bit :initial-element 0)))
    (loop for i in abundants append
	  (loop for j in abundants
		for sum = (+ i j)
		while (< sum n) do (setf (bit sums sum) 1)))
    sums))
;;(time (sums-of-two-abundants 30))

(defun 023-solution (&optional (n 28123))
  "In addition to solution, also return the highest number that is not
a sum of two abundants."
  (let ((sums (sums-of-two-abundants n)))
    (values (loop for sum across sums
		  for pos from 0
		  if (zerop sum) sum pos)
	    (position 0 sums :from-end t))))
;;(time (023-solution))
;; => 4179871
