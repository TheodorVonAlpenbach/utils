(require 'euler-utils "~/git/utils/elisp/div/euler-project/euler-utils.lisp")

(defun 009-solution (&optional (n 1000))
  (loop for a from 1
	for a2 = (sq a)
	while (< a2 n) do
	(loop for b from a
	      for b2 = (sq b)
	      for c2 = (+ a2 b2)
	      while (<= c2 n)
	      if (integerp (sqrt c2))
	      collect (list a b (sqrt c2)))))
;;(009-solution)
