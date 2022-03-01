(require 'cl-utils "~/git/utils/elisp/div/euler-project/cl-utils.lisp")

(defun knapsack-solutions (sum quantities)
  "Assume elements in QUANTITIES are in descending order."
  (if (null quantities)
    (when (zerop sum) (list ()))
    (destructuring-bind (q0 . qr) quantities
      (if (< sum q0)
	(knapsack-solutions sum qr)
	(append (knapsack-solutions sum qr)
		(loop for s in (knapsack-solutions (- sum q0) quantities)
		      collect (cons q0 s)))))))
;;(knapsack-solutions 10 '(3 2 1))

(defun 031-solution (&optional (sum 200) (coins '(1 2 5 10 20 50 100 200)))
  (knapsack-solutions sum (sort (copy-list coins) #'>)))
;;(time (length (031-solution)))
;; => 73682
