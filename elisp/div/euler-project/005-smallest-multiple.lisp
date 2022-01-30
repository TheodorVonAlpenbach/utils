(require 'factor-table "~/git/utils/elisp/div/euler-project/factor-table.lisp")

(defun 005-solution (&optional (n 20))
  (pt-product (pt-lcm (make-prime-table (1+ n)))))
;;(time (005-solution)) => 232792560
;;(time (005-solution 10000)) ;; < 1s
