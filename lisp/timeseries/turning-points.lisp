(in-package :timeseries)

(defun same-direction-p (a b c)
  (or (<= a b c)
      (>= a b c)))

(defun wrong-direction-p (r ts)
  (same-direction-p r (car ts) (cadr ts)))
;;(wrong-direction-p 0 '(1 2))

(defun turning-points (ts threashold)
  (when (minusp threashold)
    (error "THREASHOLD cannot be negative"))
  (unless (listp ts)
    (error "Timeseries must be a list"))
  (if (null ts)
    (warn "Timeseries is empty")
    (let ((res (list (pop ts)))
	  (state :idle))
      (loop for n = (length ts)
	    for r = (car res)
	    while ts
	    if (= n 1)
	      return (nreverse (push (pop ts) res))
	    else do
	    (case state
	      (:idle
	       (if (wrong-direction-p r ts)
		 (pop ts)
		 (setf state :right-direction)))
	      (:right-direction
	       (if (> (abs (- (first ts) (second ts))) threashold)
		 (progn (push (pop ts) res)
			(setf state :idle))
		 (setf state :below-threshold)))
	      (:below-threshold
	       (if (= n 2)
		 (progn (push (pop ts) res)
			(setf state :idle))
		 (if (apply #'same-direction-p (head ts 3))
		   (setf ts (remove-nth 1 ts)
			 state :right-direction)
		   (destructuring-bind (a b c) (pop-list ts 3)
		      (push (max a c) ts)
		      (setf state :idle))))))))))
;;(turning-points '(0 1 2 3 2 1 2 1) 10)

(defun generate-random-walk (n amplitude &optional (start 0))
  (loop for i below n
	for p2p = (* 2 amplitude)
	for y = start then (+ y (- amplitude (random p2p))) 
	collect y))
;;(generate-random-walk 30 2.0)
;;(gp::plot `(generate-random-walk 20 2.0))

(defun turning-points (ts threashold)
  (when (minusp threashold)
    (error "THREASHOLD cannot be negative"))
  (unless (listp ts)
    (error "Timeseries must be a list"))
  (if (null ts)
    (warn "Timeseries is empty")
    (let ((res (list (pop ts)))
	  (register ()))
      (while (ts)
	(case (length ts)
	  (0 (push (pop ts) register))
	  (1 (if (same-direction-p res register ts)
	       ;; replace single element in register with stronger element
	       (setf (car register) (pop ts))
	       (if (> (abs (- (car register) (car ts)) threshold))
		 ;; move only element in register to res and replace
		 ;; it with top of ts
		 (push (pop register) res)
		 ;; else move top of res to top of register
		 (push (pop ts) register))))
	  (3 (if (same-direction-p res register ts)
	       ;; replace register with top of ts
	       (setf register (list (pop ts)))
	       (if (> (abs (- (cadr register) (car ts))) threshold)
		 (push (pop* register 2) res)
		 (if (between-p register ts)
		   (setf (car register) (pop ts))
		   (pop ts)))))))
      (nreverse res))))
