(require 'cl-utils "~/git/utils/elisp/div/euler-project/cl-utils.lisp")
(require 'progress "~/git/utils/elisp/div/euler-project/progress.lisp")

(defun reciprocal-cycle (d)
  (let ((remainders (make-array d :element-type 'fixnum :initial-element -1)))
    (loop for i below d
	  for dividend = 10 then (* 10 remainder)
	  for remainder = (mod dividend d)
	  do (setf (svref remainders i) remainder)
	  until (position remainder remainders :end (min i 10)))
    (subseq remainders 0 (position -1 remainders))))
;;(reciprocal-cycle 7)

(defun 026-solution (&optional (d-max 1000))
  (maximum (cons nil (loop with p = (init-progress 1 d-max)
			   for d from 1 below d-max
			   do (progress-handle p d)
			   collect (reciprocal-cycle d)))
	   :key #'(lambda (x) (1- (length x)))))
;;(time (026-solution))
;; => 983

(defun first-level-reptend-primes (&optional (d-max 1000))
  "While solving this Euler problem I discovered that the maximum
cycles, of course, had a name: OEIS sequence A006883, see
https://en.wikipedia.org/wiki/Full_reptend_prime"
  (loop with p = (init-progress 1 d-max)
	for d from 1 below d-max
	for rc = (reciprocal-cycle d)
	do (progress-handle p d)
	if (= d (length rc)) collect d))
;;(time (026-solution))

