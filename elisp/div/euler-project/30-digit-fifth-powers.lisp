(require 'cl-utils "~/git/utils/elisp/div/euler-project/cl-utils.lisp")
(require 'progress "~/git/utils/elisp/div/euler-project/progress.lisp")

(defun sum-digits-pth-powers (n p)
  (reduce #'+ (number-to-digits n 'vector) :key (lambda (x) (expt x p))))

(defun 030-solution (&optional (p 5))
  "This can obviously be more effective, but..."
  (let* ((ceiling-n (loop for i from 1 below 10
			  if (< (expt 9 p) (/ (expt 10 i) i))
			  return (expt 10 i)))
	 (progress (init-progress 2 ceiling-n)))
    (loop for i from 2 below ceiling-n
	  do (progress-handle progress i)
	  if (= i (sum-digits-pth-powers i p))
	  collect i)))
;;(time (030-solution))
;; => (4150 4151 54748 92727 93084 194979)
