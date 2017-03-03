(in-package :timeseries)

(defun turning-point-p (a b c key)
  (and b (destructuring-bind (a b c) (mapcar key (list a b c))
	   (if (> a c) (or (> b a) (< b c)) (or (> b c) (< b a))))))
;;(nor 1 (turning-point-p 0.5d0 1.99d0 3.24d0 #'identity))

(defun diff (x y key)
  (abs (- (funcall key x) (funcall key y))))
;; (diff '(0 2) '(0 3/4) #'second)

(defun setfsecond (x y) (setf (second x) y))
(defun setfthird (x y) (setf (third x) y))

(defun negate-if (pred number) (if pred (- number) number))

(defun nudge (a b key setkey)
  (destructuring-bind (a* b*) (mapcar key (list a b))
    (let ((b-value (+ a* (negate-if (> a* b*) single-float-epsilon))))
      (if setkey (funcall setkey b b-value) (setf b b-value))
      b)))
;;(nudge '(s 1) '(s 2) #'second #'setfsecond)
;;(nudge 1 2 #'identity nil)


(defun turning-points (ts threshold &key (key #'identity) setkey finalize-p debug)
  (let (res a b)
    (flet ((dp (x) (when debug (print x))))
      (loop for stack on ts
	    for c = (car stack) do (dp `(:res ,res :abc (,a ,b . ,c) :stack ,stack)) do
	    (cond
	      ((not a) (dp :cond0) (setf a c)) 
	      ((not b) (dp :cond1) (setf b c))
	      ((not (turning-point-p a b c key)) (dp :cond2)
	       (setf b c))
	      ((>= (diff a b key) threshold) (dp :cond3)
	       (push a res) (setf a b  b c))
	      ((turning-point-p a c b key) (dp :cond4)
	       (setf a c  b (nudge a b key setkey)))
	      (t (dp :cond5) c)))
      (dp `(:res ,res :ab (,a ,b)))
      (when (and finalize-p (>= (diff a b key) threshold)) (dp :cond6)
	    (push a res))
      (dp `(:res ,res :ab (,a ,b))))
    (nreverse res)))
;;(turning-points '(0 2 -2 -1 0 -1 2 0) 1 :debug t)
