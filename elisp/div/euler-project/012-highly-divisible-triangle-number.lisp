(require 'euler-utils "~/git/utils/elisp/div/euler-project/euler-utils.lisp")
(require 'euler-utils "~/git/utils/elisp/div/euler-project/cl-utils.lisp")
(require 'factor-table "~/git/utils/elisp/div/euler-project/factor-table.lisp")

(defun 012-solution (n &optional (tn-max 1000000))
  "Return lowest triangle number with more than N factors.
Also return the order of that number in the sequence of triangle
numbers, as well as the actual number of factors, which typically will
be more than N."
  (when (or tn-max (not (get-factor-table)))
    (set-factor-table (make-factor-table (1+ tn-max))))
  (loop for i from 1 below tn-max
	for (a b) = (if (evenp i)
		      (list (/ i 2) (1+ i))
		      (list i (/ (1+ i) 2)))
	for l = (length (all-factors-prod a b))
	if (>= l n) return (values (* a b) i l)))
;;(time (012-solution 500))
;; => 76576500 (solution)
;;       12375 (nth triangle number)
;;         576 (number of actual factors)
