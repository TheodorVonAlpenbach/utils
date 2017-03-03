(in-package :timeseries)

;;;; An alternative implementation of TURNING-POINTS using an
;;;; additional third state variable, UP. UP keeps track on the
;;;; orientation of the turning point at A. If it is true, A is a
;;;; maximum, otherwise it is a minimum.

(defun turning-points-dir (ts threshold &key key debug)
  "Tested ok"
  (let (res a b up)
    (flet ((dp (x) (when debug (print x)))
	   (hit-p (va vb) (>= (if up (- vb va) (- va vb)) threshold)))
      (loop for (c . stack) on ts
	    for (va vb vc) = (if key (mapcar key (list a b c)) (list a b c))
	    do (dp `(:res ,res :abc (,va ,vb . ,vc) :up ,up :stack ,stack))
	    do (cond
		 ((not a) (dp :c0)
		  (setf a c)) 
		 ((not b) (dp :c1)
		  (unless (= va vc) (setf b c  up (> vc va))))
		 ((not (if up (> vb vc) (< vb vc))) (dp :c2)
		  (setf b c))
		 ((hit-p va vb) (dp :c3)
		  (push a res) (setf a b  b c  up (not up)))
		 ((if up (< vc va) (< va vc))
		  (dp :c4) (setf a c  b c))
		 (t (dp :c5))))
      (dp `(:res ,res :ab (,a ,b))))
    (nreverse res)))
;;(turning-points-dir (mapcar #'list '(0 2 -2 -1 0 -1 2 0)) 1 :key #'car :debug t)
;;(turning-points-dir '(0 2 -2 -1 0 -1 2 0) 1 :debug t)

(defun turning-points-core1 (ts threshold)
  (let (res a b up)
    (loop for c in ts do
	  (cond
	    ((not a) (setf a c)) 
	    ((not b) (unless (= a c) (setf b c  up (> c a))))
	    ((not (if up (> b c) (< b c))) (setf b c))
	    ((>= (if up (- b a) (- a b)) threshold)
	     (push a res) (setf a b  b c  up (not up)))
	    ((if up (> c b) (< c b)) (setf a c  b nil))))
    (nreverse res)))

(defun turning-points-core2 (ts threshold)
  (let (a b up)
    (loop for c in ts
	  if (not a) do (setf a c)
	  else if (not b) do (unless (= a c) (setf b c  up (> c a)))
	  else if (not (if up (> b c) (< b c))) do (setf b c)
	  else if (>= (if up (- b a) (- a b)) threshold)
	    collect a and do (setf a b  b c  up (not up))
	  else if (if up (> c b) (< c b)) do (setf a c  b nil))))
;;(turning-points-core '(0 2 -2 -1 0 -1 2 0) 1)
