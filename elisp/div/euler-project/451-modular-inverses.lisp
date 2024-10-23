;;; https://projecteuler.net/problem=451
;;; http://www.sbcl.org/manual/#Introspection-and-Tuning

(require 'progress "~/git/utils/elisp/div/euler-project/progress.lisp")
(require 'factor-table "~/git/utils/elisp/div/euler-project/factor-table.lisp")

(defparameter *n* (round 1E7))
(defparameter *ft* (make-factor-table (+ 2 *n*)))

;;; inverses
(defun make-inverses (&optional (n *n*))
  (make-array (1+ (* 2 *n*)) :element-type 'bit :initial-element 1))

(defun min-modular-inverses (&optional (n *n*) (start 3))
  (let ((fs-queue (list (all-factors (1- start)) (all-factors start)))
	(n+1 (1+ n))
	(my-progress (init-progress start (/ n 2)))
	(inverses-sum 0)
	(inverses (make-inverses)))
    (loop for i from start to (/ n 2)
	  for p-max = (min (* i i) (length inverses))
	  for fs- = (pop fs-queue)
	  for fs+ = (all-factors (1+ i)) 
	  do (setf fs-queue (list (car fs-queue) fs+))
	  do (progress-handle my-progress i)
	  do (loop for f- in (sort fs- #'<)
		   do (loop for f+ in (sort (copy-list fs+) #'<)
			    for p = (* f- f+)
			    while (and (< p p-max))
			    if (and (= (bit inverses p) 1)
				    (< (* 2 i) p n+1))
			    do (setf (bit inverses p) 0)
			    and do (incf inverses-sum (- p i)))))
    ;; return the solution to 451
    (+ (reduce #'+ (subseq inverses start (1+ n))) inverses-sum)))
;;(time (min-modular-inverses (round 1E2)))

(defun 451-solution (&optional (n *n*) (start 3))
  "Not so optimalized but more instructive"
  (let ((inverses-sum 0)
	(inverses (make-array (1+ (* 2 n)) :element-type 'bit :initial-element 1)))
    (loop for i from start to (/ n 2)
	  for p-max = (min (* i i) (length inverses))
	  do (loop for f- in (sort (all-factors (1- i))- #'<)
		   do (loop for f+ in (sort (all-factors (1+ i)) #'<)
			    for p = (* f- f+)
			    while (and (< p p-max))
			    if (and (= (bit inverses p) 1)
				    (< (* 2 i) p (1+ n)))
			    do (setf (bit inverses p) 0)
			    and do (incf inverses-sum (- p i)))))
    ;; return the solution to 451
    (+ (reduce #'+ (subseq inverses start (1+ n))) inverses-sum)))
;;(451-solution)

;;; Results
;; n             Sum I(n)
;; 2E02               56
;; 2E03             9050
;; 2E04          1177096
;; 2E05        132762815
;; 2E06      14223825129
;; 2E07    1482595392818
;; 2E08  150844175079947

;;wrong: 5043420735022359

(provide '451-modular-inverses)
