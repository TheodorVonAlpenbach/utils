(in-package :mb-grid)

(defun m- (&rest args) (apply #'matrix-difference args))
(defun m* (&rest args) (apply #'matrix-product args))
(defun m-1 (&rest args) (apply #'matrix-inversion args))
(defun mT (&rest args) (apply #'matrix-transpose args))

(defun independent-p (vectors) (= (rank vectors) (length vectors)))

(defun select-basis (x points values)
  (assert (= (length points) (length values)) () "points and values do not have the same length")
  (let* ((x0 (minimum points :key (bind #'matrix-distance x)))
	 (objs (loop for x in points
		     for y in values
		     collect (list x (matrix-difference x x0) y)))
	 (r (rank (mapcar #'second objs)))
	 (sorted-objs (sort objs #'< :key #'(lambda (obj) (matrix-distance (first obj) x))))
	 (independent-objs (loop with res = ()
				 for obj in sorted-objs
				 for (x v y) = obj
				 while (< (length res) r)
				 if (independent-p (cons v (mapcar #'second res)))
				 do (push obj res)
				 finally (return (reverse res)))))
    (list :x0 x0
	  :y0 (third (first sorted-objs))
	  :x (mapcar #'first independent-objs)
	  :y (mapcar #'third independent-objs)
	  :v (mapcar #'second independent-objs))))

(defun solve-linear-equation (X y)
  "Solves y = Xa + b, and returns the solution as the list (a b).
For some positive integer N, X must be an (N+1)xN matrix and y must be
an (N+1)x1 (column) matrix. The returned solution is the list (a b),
where a is an Nx1 (column) matrix and b is a scalar."
  (let* ((V (map-rows (bind #'m- (matrix-row X 0)) X :start 1))
	 (z (map-rows (bind #'m- (matrix-row y 0)) y :start 1))
	 (a (m* (m-1 V) z)))
    ;; b = y0 = a^T * x0
    (list (matrix-column a 0)
	  (aref (m- (submatrix y :rows 0) (m* (submatrix x :rows 0) a)) 0 0))))
;;(unintern 'solve-linear-equation)

(defun interpolate-scattered-fn (x points values)
  (let* ((o (select-basis x points values))
	 (y (column->matrix (cons (getf o :y0) (getf o :y))))
	 (X (rows->matrix  (cons (getf o :x0) (getf o :x)))))
    (destructuring-bind (a b) (solve-linear-equation X y)
      (lambda (x) (+ (vector-inner-product a x) b)))))

(defun interpolate-scattered (x points values)
  (let ((fn (interpolate-scattered-fn x points values)))
    (values (funcall fn x) fn)))
;;(interpolate-scattered #(0.8 0) '(#(0 0) #(0.5 0) #(0 1) #(1 0) #(1 1)) '(0 0.5 1 2 3))
