(require 'euler-utils "~/git/utils/elisp/div/euler-project/euler-utils.lisp")

(defun 007-solution (&optional (n 10001))
  (nth-prime (1- n)))
;;(007-solution) => 104743
