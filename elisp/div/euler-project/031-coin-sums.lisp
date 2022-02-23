(require 'cl-utils "~/git/utils/elisp/div/euler-project/cl-utils.lisp")

(defun knapsack-solutions (sum quantities)
  "Assume elements in QUANTITIES are in descending order."
  (when (and quantities (plusp sum))
    (print (list sum quantities (rest quantities)))
    (append
     (knapsack-solutions sum (rest quantities))
     (loop with q1 = (first quantities)
	   ;; for rest = sum then (- rest q1) 
	   ;; until (< rest q1)
	   for i below 10
	   until (< sum q1)
	   collect (loop for s in (knapsack-solutions (decf sum q1) quantities)
			 collect (cons q1 s))
	   do (print sum)))))
;;(knapsack-solutions 3 '(2 1))

(defun 031-solution (&optional (sum 200) (coins '(1 2 5 10 20 50 100 200)))
  (knapsack-solutions sum (sort (copy-list coins) #'>)))
;;(time (031-solution 100 5))
