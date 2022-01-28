(defun 001-solution (&optional (n 1000))
  (let ((multiples (make-array n :element-type 'bit)))
    (flet ((set-multiples (m)
	     (loop for i from 1
		   for mi = (* m i)
		   while (< mi n) do (setf (bit multiples mi) 1))))
      (set-multiples 3)
      (set-multiples 5)
      (loop for i below n if (plusp (bit multiples i)) sum i))))
;;(time (001-solution))
;; => 233168
