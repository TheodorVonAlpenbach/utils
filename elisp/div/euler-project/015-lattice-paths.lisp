(require 'cl-utils "~/git/utils/elisp/div/euler-project/cl-utils.lisp")

(defun 015-solution (&optional (n 20) (m 20))
  "It can be shown that the transverse diagonals equals the rows in
the Pascal triangle at level (n + m) . Hence A(n, m) is the binomial
coefficient B(n + m, n) = (n + m)! / (n! * m!)"
  (binomial-coefficient (+ n m) n))
;;(time (015-solution))
;; => 137846528820


