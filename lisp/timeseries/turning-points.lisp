(in-package :timeseries)

(defun same-direction-p (a b c key)
  (destructuring-bind (a b c) (mapcar key (list a b c))
    (or (<= a b c) (>= a b c))))
;;(same-direction-p 0 1 0 #'identity)

(defun distance (x y key)
  (abs (- (funcall key x) (funcall key y))))
;; (distance '(0 2) '(0 3/4) #'second)

(defun tp-prologue (ts key)
  "Return first valid RES, A, B, TS constallation"
  (let ((a (pop ts)))
      (print ts)
      (loop for (b c . r) on ts while c
	    if (not (same-direction-p a b c key))
	    return (list (list a) b c r)
	    finally (return (list (list a) b nil ())))))
;;(tp-prologue '(0 -1 0 2 0) #'identity)

(defun tp-epilogue (ts a b key)
  (nreverse (if b
	      (if (same-direction-p (car ts) a b key)
		(cons b ts)
		(cons b (cons a ts)))
	      (cons a ts))))

(defun turning-points (ts threshold &key (key #'identity) debug)
  (when (minusp threshold)
    (error "THRESHOLD cannot be negative"))
  (unless (listp ts)
    (error "Timeseries must be a list"))
  (if (< (length ts) 3)
    ts
    (loop with (res a b ts) = (tp-prologue ts key)
	  while ts
	  do (when debug (print (list :res res :ab (list a b) :ts ts)))
	  do (if (same-direction-p a b (car ts) key)
	       (setf b (pop ts))
	       (if (>= (distance a b key) threshold)
		 (setf res (cons a res)  a b  b (pop ts))
		 (if (same-direction-p a (car ts) b key)
		   (pop ts)
		   ;;else pop ts to we find a TP
		   (loop for r on ts
			 for (c d . e) on ts
			 while (same-direction-p a c d key)
			 finally (setf a c  b d  ts e)))))
	  finally (return (tp-epilogue res a b key)))))
;;(turning-points '(0.3 -1.38 -0.73 0.8 0.84 2.24 0.43 1.55 2.32 2.87 1.14 1.75 1.25 1.07 2.46 1.61) 2)
;;(turning-points '(0 -1 0 2 0 1 2 3 2 2.5) 3/2)
;;(turning-points '((0 0) (0 1) (0 2) (0 3) (0 2) (0 1) (0 2) (0 1)) 3/2 :key #'second)
