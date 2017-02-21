(in-package :timeseries)

(defun same-direction-p (a b c)
  (or (<= a b c)
      (>= a b c)))

(defun wrong-direction-p (r ts)
  (same-direction-p r (car ts) (cadr ts)))
;;(wrong-direction-p 0 '(1 2))

(defun turning-points-old (ts threshold)
  (when (minusp threshold)
    (error "THRESHOLD cannot be negative"))
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
	       (if (> (abs (- (first ts) (second ts))) threshold)
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
		   (destructuring-bind (a nil c) (pop-list ts 3)
		      (push (max a c) ts)
		      (setf state :idle))))))))))
;;(turning-points-old '(0 1 2 3 2 1 2 1) 10)

(defun generate-random-walk (n amplitude &optional (start 0))
  (loop for i below n
	for p2p = (* 2 amplitude)
	for y = start then (+ y (- amplitude (random p2p))) 
	collect y))
;;(generate-random-walk 100 2.0)
;;(generate-random-walk 3 2)
;;(gp::plot `(generate-random-walk 20 2.0))
;;(trace generate-random-walk)

(defun timestamps (n &optional delta-t start-t)
  "Return a list of length N with Ith element (+ START-T (* I DELTA-T))"
  (let ((delta-t (or delta-t 1)))
    (loop for i below n
	  for elt = (or start-t 0) then (+ elt delta-t)
	  collect elt)))
;;(timestamps 5)
;;(untrace timestamps)

(defun write-random-walks (n amplitudes filename
			   &optional starts (delta-t 1) (start-t))
  "Generate M timeseries of length N and write to FILENAME.
M is the length of AMPLITUDES."
  (let ((*print-pprint-dispatch* (copy-pprint-dispatch)))
    (set-pprint-dispatch 'float (lambda (s f) (format s "~,2f" f)))
    (write-csv-file
     (transpose-tree
      (cons (timestamps n delta-t start-t)
	    (loop for amplitude in amplitudes
		  for start in (or starts
				   (make-list (length amplitudes)
					      :initial-element 0))
		  collect (generate-random-walk n amplitude start))))
     filename
     :column-separator #\;)))
;;(write-random-walks 3 '(1.0 2.0) "/home/mbe/projects/chess/TPTFilter/test/random-walk.csv" '(.5 .3))
;;(write-random-walks 10 '(1.0 2.0) "~/projects/chess/TPTFilter/test/random-walk2.csv" nil .01 (coerce (get-unix-time) 'double-float))

(defun turning-points (ts threshold)
  (when (minusp threshold)
    (error "THRESHOLD cannot be negative"))
  (unless (listp ts)
    (error "Timeseries must be a list"))
  (if (null ts)
    (warn "Timeseries is empty")
    (let ((res (list (pop ts)))
	  (register ()))
      (loop while ts do
	(case (length ts)
	  (0 (push (pop ts) register))
	  (1 (if (same-direction-p res register ts)
	       ;; replace single element in register with stronger element
	       (setf (car register) (pop ts))
	       (if (> (abs (- (car register) (car ts))) threshold)
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
