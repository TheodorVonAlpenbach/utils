(in-package :signal)

(defmethod shift ((x discrete-signal) k)
  "Shifts discrete-signal X K positions to the right.
Formally, if X is x[n], it returns x[n-K]."
  (make-instance 'discrete-signal
    :data (data x) :start (+ (start x) k)))
;;(shift (sig -2 1 2 3) 10)

(defmethod fold ((x discrete-signal) &optional (position 0))
  "Folds discrete-signal X at POSITION. Optional argument not yet supported."
  (declare (ignore position))
  (let ((res (make-instance 'discrete-signal
	       :data (reverse (data x)) :start (1+ (- (end x))))))
    res))
;;(fold (shift x 2))

(defun time-invariant-p (filter &key (from -5) (to 5) (test-filter (delta from to)))
  (loop for k below 10
	always (sequal* (shift (funcall filter test-filter) k)
			(funcall filter (shift test-filter k)))))

(defun additive-p (filter &key (length 10) (test-size 10))
  (loop repeat test-size
	always (loop for i below length
		     always (funcall filter ))))

(defun moving-average-filter (length)
  (lambda (x)
    (apply #'sig (start x)
	   (loop for n from (start x) below (+ (end x) (1- length))
		 collect (/ (loop for k below length
				  sum (sref x (- n k)))
			    length)))))
;;(funcall (moving-average-filter 5) (delta -10 10))
;;(time-invariant-p (moving-average-filter 5))
