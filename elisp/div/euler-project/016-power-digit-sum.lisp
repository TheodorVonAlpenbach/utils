(require 'euler-utils "~/git/utils/elisp/div/euler-project/euler-utils.lisp")

(defun 016-solution (&optional (n 1000) (a 2))
  (digit-sum (expt a n)))
;;(016-solution)
