(defpackage :bezier
  (:use :common-lisp :mb-utils :mb-gnuplot)
  (:export :linear-bezier))

(in-package :bezier)

(defun p+ (p q) (mapcar #'+ p q))
(defun p* (k p) (mapcar (bind #'* k) p))

(defun linear-combination (fn1 fn2)
  (lambda (x) (p+ (p* (- 1 x) (funcall fn1 x)) (p* x (funcall fn2 x)))))

(defun B1 (p0 p1) (linear-combination (constantly p0) (constantly p1)))
(defun B2 (p0 p1 p2) (linear-combination (B1 p0 p1) (B1 p1 p2)))
(defun B3 (p0 p1 p2 p3) (linear-combination (B2 p0 p1 p2) (B2 p1 p2 p3)))

(defun bezier (&rest points)
  "Bezier function for an arbitrary number of points."
  (cond ((< (length points) 2) 1 (error "Too few points for calculating a Bezier curve"))
	((= (length points) 2) (apply #'B1 points))
	(t (linear-combination (apply #'bezier (butlast points)) (apply #'bezier (rest points))))))

(defun linear-bezier (p0 p1 &key (n 10)) (a-b 0 1 :length n :key (B1 p0 p1)))
(defun quadratic-bezier (p0 p1 p2 &key (n 10)) (a-b 0 1 :length n :key (B2 p0 p1 p2)))
(defun cubic-bezier (p0 p1 p2 p3 &key (n 10)) (a-b 0 1 :length n :key (B3 p0 p1 p2 p3)))
;;(plot (tree->array (cubic-bezier '(0 1) '(0 2) '(1 0) '(1 1) :n 100)))

(defun plot-beziers (p0 p1 p2 p3 &optional (n 100))
  (plot `(:g ,(mapcar #'tree->array
		(list (linear-bezier p0 p3 :n n)
		      (quadratic-bezier p0 (p* 1/2 (p+ p1 p2)) p3 :n n)
		      (cubic-bezier p0 p1 p2 p3 :n n)))
	     :y-range (0.5 2))))
;;(plot-beziers '(0 1) '(0 2) '(1 2) '(1 1))


