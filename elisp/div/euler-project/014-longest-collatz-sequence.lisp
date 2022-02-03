(require 'cl-utils "~/git/utils/elisp/div/euler-project/cl-utils.lisp")
(require 'progress "~/git/utils/elisp/div/euler-project/progress.lisp")

(defparameter *collatz-length-vector* nil)

(defun next-collatz (n)
  (if (evenp n) (/ n 2) (1+ (* 3 n))))

(defun collatz-sequence (n)
  (if (< n 2)
    (list 1)
    (cons n (collatz-sequence (next-collatz n)))))
;;(collatz-sequence 3)

(defun set-collatz-lengths (n)
  (if (< n (length *collatz-length-vector*))
    (let ((l (svref *collatz-length-vector* n)))
      (if (zerop l)
	(setf (svref *collatz-length-vector* n)
	      (1+ (set-collatz-lengths (next-collatz n))))
	l))
    (1+ (set-collatz-lengths (next-collatz n)))))

(defun make-collatz-length-vector (n)
  (setf *collatz-length-vector*
	(make-array (min (sq n) (round 1E8))
	  :initial-element 0
	  :element-type 'integer))
  (setf (svref *collatz-length-vector* 1) 1)
  (loop with progress = (init-progress 0 n)
	for i from 2 below n
	do (progress-handle progress i)
	do (set-collatz-lengths i))
  *collatz-length-vector*)
  
(defun 014-solution (&optional (n 1000000))
  (maximum (make-collatz-length-vector n)))
;;(time (014-solution))
;;    525 ;
;; => 837799 ;
;;    525


