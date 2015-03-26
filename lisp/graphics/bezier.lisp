(defpackage :bezier
  (:use :common-lisp)
  (:export :bezier :bezier-curve))

(in-package :bezier)

(defun p+ (p q) (mapcar #'+ p q))
(defun p* (k p) (mapcar #'(lambda (x) (* k x)) p))

(defun linear-combination (fn1 fn2)
  "Returns the linear combination function of functions FN1 and FN2."
  (lambda (x)
    (p+ (p* (- 1 x) (funcall fn1 x))
	(p* x (funcall fn2 x)))))

(defun bezier (&rest control-points)
  "Returns the Bezier function for CONTROL-POINTS.
Each element in CONTROL-POINTS is a list of coordinates (x y z ...)."
  (case (length control-points)
    ((0 1) (error "Too few points for calculating a Bezier curve"))
    (2     (apply #'linear-combination (mapcar #'constantly control-points)))
    (t     (linear-combination (apply #'bezier (butlast control-points))
			       (apply #'bezier (rest control-points))))))
;;(setf cubic-bezier-fn (bezier '(0 1) '(0 2) '(1 2) '(1 1)))

(defun bezier-curve (n &rest control-points)
  "Returns N points representing the Bezier curve for CONTROL-POINTS."
  (loop with fn = (apply #'bezier control-points)
	for s from 0 to 1 by (/ 1.0 (1- n)) collect (funcall fn s)))
;;(bezier-curve 20 '(0 1) '(0 2) '(1 2) '(1 1))

;;; This demo function is not exported. Note that it requires :mb-gnuplot and :mb-utils.
(defun plot-beziers (p0 p1 p2 p3 &optional (n 100))
  "Plots linear, quadratic, and cubic Bézier curves from P0 to P3."
  (gp::plot `(:g ,(mapcar #'mb-utils::tree->array
			  (list (bezier-curve n p0 p3)
				(bezier-curve n p0 (p* 1/2 (p+ p1 p2)) p3)
				(bezier-curve n p0 p1 p2 p3)))
		 :y-range (0.5 2))))
;;(plot-beziers '(0 1) '(0 2) '(1 2) '(1 1))
