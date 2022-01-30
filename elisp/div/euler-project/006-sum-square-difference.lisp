(require 'euler-utils "~/git/utils/elisp/div/euler-project/euler-utils.lisp")

(defun 006-solution (&optional (n 100))
  (- (sq (/ (* (1+ n) n) 2))
     (loop for i from 1 to n sum (sq i))))
;;(time (006-solution)) => 25164150
;;(006-solution 10)
