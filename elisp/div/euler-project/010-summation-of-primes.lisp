(require 'euler-utils "~/git/utils/elisp/div/euler-project/euler-utils.lisp")

(defun 010-solution (&optional (n 2000000))
  (apply #'+ (primes-below n)))
;;(time (010-solution))
;; => 142913828922
