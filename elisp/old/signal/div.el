(defun significant-integer (x y)
  (let ((factor (expt 10 (- y (floor (log x 10)) 1))))
    (list (floor (* x factor)) factor)))
;;(significant-integer (/ pi 1000) 3)

(defun float2rational (x y)
  (destructuring-bind (num factor) (significant-integer x y)
    (if (< factor 1)
      (list (/ num factor) 1)
      (mapcar (bind #'/ (gcd num factor)) (list num factor)))))
;;(float2rational 5123123 3)

(defun trunc (x y)
  (destructuring-bind (num f) (significant-integer x y)
      (* num (/ 1.0 f))))
;;(trunc (* pi 1000) 3)
;;(truncate pi .01)

(cl-defun sampling-factor (center-frequency
			   &optional
			   (coefficient 24)
			   (original-sampling-rate 441000))
  (/ (* center-frequency coefficient)
     original-sampling-rate))

(defun upsample (x y &optional interpolate)
  "Assumes X and Y are vectors, X < Y"
  (let ((xn (length x))
	(yn (if (integerp y) y (length y))))
    (assert (< xn yn))
    (let* ((k (/ yn (+ 1.0 (- yn xn))))
	   (limit k)
	   (res (make-vector yn 0))
	   (i 0)
	   (indexes (loop for j below (length x)
			  if (>= i (round limit)) collect i
			  and do (when interpolate (setf (aref res i) (avg (float (aref x j)) (aref x (1+ j)))))
			  and do (incf i)
			  and do (incf limit k)
			  
			  do (setf (aref res i) (aref x j))
			  do (incf i))))
      res)))
;;(upsample #[0 1 2 3 4 5] #[0 1 2 3 4 5 6 7] t)
;;(upsample (coerce (vec-random 46273) 'vector) 46626)
;;(upsample (coerce (vec-random 6) 'vector) 8)
