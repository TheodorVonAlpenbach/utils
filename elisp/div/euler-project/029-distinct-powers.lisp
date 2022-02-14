(require 'cl-utils "~/git/utils/elisp/div/euler-project/cl-utils.lisp")

(defun 029-solution (&optional (a-max 100) (b-max 100))
  (remove-duplicates
      (sort (loop for a from 2 to a-max append
		  (loop for b from 2 to b-max collect
			(expt a b)))
	#'<)))
;;(time (029-solution 100 5))
