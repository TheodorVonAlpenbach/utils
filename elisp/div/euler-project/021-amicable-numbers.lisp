(require 'cl-utils "~/git/utils/elisp/div/euler-project/cl-utils.lisp")
(require 'factor-table "~/git/utils/elisp/div/euler-project/factor-table.lisp")

(defun sum-proper-divisors (n &optional reset-visited-p)
  (if (< n 2)
    0 (+ 1 (reduce #'+ (proper-divisors n reset-visited-p)))))
;;(sum-proper-divisors 1)

(defun amicable-p (n)
  (= n (sum-proper-divisors (sum-proper-divisors n t) t)))
;;(amicable-p 220)

(defun 021-solution (&optional (n 10000))
  (init-factor-table n)
  (let ((sums (loop for i from 1 below n collect (sum-proper-divisors i t))))
    (init-factor-table (1+ (maximum sums)))
    (let ((amicable-numbers
	   (loop for sum in sums
		 for i from 1
		 if (and (/= i sum)
			 (= i (sum-proper-divisors sum t)))
		 collect i)))
      (values (sum amicable-numbers) amicable-numbers))))
;;(time (021-solution 100))
;; => 31626 
;; (220 284 1184 1210 2620 2924 5020 5564 6232 6368)
