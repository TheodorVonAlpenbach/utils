(require 'prime-table "~/git/utils/elisp/div/euler-project/prime-table.lisp")

(defun 005-solution (&optional (n 20))
  (pt-product (pt-lcm (make-prime-table (1+ n)))))
;;(time (005-solution)) => 232792560
;;(time (progn (005-solution 10000) 1)) ;; < 1s
