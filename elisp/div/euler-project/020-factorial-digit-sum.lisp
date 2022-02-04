(require 'cl-utils "~/git/utils/elisp/div/euler-project/cl-utils.lisp")

(defun 020-solution (&optional (n 100))
  (digit-sum (! n)))
;;(time (020-solution))
;; => 648
