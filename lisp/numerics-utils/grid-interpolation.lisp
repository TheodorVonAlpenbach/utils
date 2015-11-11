(in-package :mb-grid)

;;; A tiny implementation of matrix as a list of row vectors
;;; This is hairy. Should use real matrices instead, me thinks (MB).
;;; These methods should by no means be exported.s
(defun vec (n &optional (init 0)) (make-list n :initial-element init))1
(defun vec+ (vec &rest vecs) (apply #'map (type-of vec) #'+ vec vecs))
(defun vec- (vec &rest vecs) (apply #'mapcar #'- vec vecs))
(defun vec-copies (vec n) (loop repeat n collect (copy-list vec)))
(defun dot-product (vec1 vec2) (reduce #'safe-+ (map (type-of vec1) #'safe-* vec1 vec2)))
;;(dot-product '(1 1) '(2 1))

(defun mat (n &optional (m n) (init 0)) (loop for i below n collect (vec m init)))
(defun mat+ (mat &rest mats) (apply #'mapcar #'vec+ mat mats))
(defun diagmat (vec) (let ((res (mat (length vec))))
		       (loop for row in res
			     for x in vec
			     for i from 0
			     do (setf (nth i row) x))
		       res))
;;; The utils actually used are VEC-, VEC-COPIES, DOT-PRODUCT, MAT+, DIAGMAT.

(defun grid-axes-dimensions (axes) (mapcar #'length axes))
(defun grid-dimensions (grid) (grid-axes-dimensions (grid-axes grid)))
;;(grid-dimensions *test-grid*)

(defun generate-index-points (&optional (dimensions '(4 3 2)))
  (labels ((gip (dimensions indexes)
	     (loop for i below (first dimensions)
		   if (rest dimensions) append (gip (rest dimensions) (cons i indexes))
		   else collect (reverse (cons i indexes)))))
    (gip dimensions ())))
;;(generate-index-points '(3 2))

(defun grid-index-points (grid) (generate-index-points (grid-dimensions grid)))
;;(grid-index-points *test-grid*)

(defun index-point->grid-point (index-point grid-axes)
  (mapcar #'aref grid-axes index-point))
;;(index-point->grid-point '(1 1) (list #(0 2 4) (vector pi (exp 1))))

(defun index-point->grid-value (index-point grid-data)
  (apply #'aref grid-data index-point))
;;(index-point->grid-value '(1 0) (grid-data *test-grid*))

;;; this will become obsolete
(defun populate-array (fn grid-axes)
  "GRID-AXES should contain the value axess for each dimension.
Note that the order here is outer dimension first, just like for an
ARRAY object."
  (let ((array (make-array (grid-axes-dimensions grid-axes))))
    (loop for index-point in (generate-index-points (grid-axes-dimensions grid-axes))
	  do (setf (apply #'aref array index-point)
		   (funcall fn (index-point->grid-point index-point grid-axes))))
    array))
;;(populate-array #'(lambda (p) (reduce #'+ p)) (list #(0 2 4) #(1 3)))
;;(generate-array '(2 2)) ==> #2A((0 1) (2 3))

(defun subscripts-rev (dimensions row-major-index)
  "Helper function for SUBSCRIPTS"
  (loop for n = row-major-index then (/ (- n m) d)
	for d in (reverse dimensions)
        for m = (mod n d)
	collect m))

(defun subscripts (dimensions row-major-index)
  "Converts ROW-MAJOR-INDEX corresponding to DIMENSIONS to subscripts."
  (nreverse (subscripts-rev (reverse dimensions) row-major-index)))
;;(mapcar (bind #'subscripts '(10 10 10) 1) (0-n 1000))

(defun array-subscripts (array row-major-index)
  "Converts ROW-MAJOR-INDEX corresponding to ARRAY's dimensions to subscripts.
That is x == (array-row-major-index arr (array-subscripts arr x))."
   (subscripts (array-dimensions array) row-major-index))

(defun make-test-grid ()
  (span-grid (lambda (&rest p) (sqrt (reduce #'+ p))) '(#(0 1 2) #(5 10))))
(defparameter *test-grid* (make-test-grid))
;;(setf *test-grid* (make-test-grid))
;;(list-grid (make-test-grid))

(defun closest-index-edge (x axis)
  "Returns the pair \(N d\), where N is the index of the closest element to X in AXIS,
and d is the offset to the index of the second closest element to X in
AXIS. Hence d is either -1 or 1. The function is of order O(logN), and
assumes that X is a number and that AXIS is a vector of sorted
numbers."
  (destructuring-bind (imin imax) (sequence-index-boundary axis)
    (cond ((< x (aref axis 0))                      ;left of axis axes
	   (list imin 1))      
	  ((> x (aref axis (1- (length axis))))     ;right of axis axes
	   (list imax -1))
	  ;; Else, we search for best index in O(log(N)) time
	  (t (loop for (a b) = (list imin imax) then (if (< (aref axis i) x) (list i b) (list a i))
		   for i = (floor (+ b a) 2)
		   if (= i a) return (if (< (- x (aref axis i))
					    (- (aref axis (1+ i)) x))
				       (list i 1)
				       (list (1+ i) -1)))))))
;;(mapcar (bind #'closest-index-edge #(0 1 2 3)) '(-1 0 0.5 1 1.5 2.5 3 4))

(defun closest-index-edges (point axes)
  "Coordinate order in each egde: innermost dimension first"
  (mapcar #'(lambda (x axis) (closest-index-edge x axis)) point axes))
;;(closest-index-edges '(0 10) (grid-axes *test-grid*))

(defun closest-index-points (index-edges)
  "Coordinate order in each index point: innermost dimension first"
  (let ((index-point (mapcar #'first index-edges))
	(offset-mat (diagmat (mapcar #'second index-edges))))
    (cons index-point
	  (mat+ (vec-copies index-point (length index-point)) offset-mat))))
;;(closest-index-points (closest-index-edges '(0 5) (grid-axes *test-grid*)))

(defvar *grid-verbose* nil)
;;(setf *grid-verbose* nil)

(defun interpolate (point reference-points reference-values)
  "Solves Xa = f, where X = (x1, x2, ..)^T is the result of
`grid-closest-base-vectors', f = (f1 - f0, f2 - f0, ..), where f0 is
F(point), f1 = F(point + x1), f2 = (point + x2), .."
  (let* ((p0 (first reference-points))
	 (p (rest reference-points))
	 (f0 (first reference-values))
	 (f (rest reference-values))
	 (V (mapcar (bind #'vec- p0) p))
;;	 (fd (mapcar (bind #'- f0) f))
	 (fd (mapcar #'(lambda (x) (safe-op #'- x f0)) f))
	 (a (matrix-column (matrix-product (matrix-inversion (tree->array V))
					   (tree->array (mapcar #'list fd)))))
	 (b (safe-op #'- f0 (dot-product a p0)))
	 (F^ (safe-op #'+ (dot-product a point) b)))
    (when *grid-verbose*
      (print  (list :point point :F^ F^ :p0 p0 :p p :V V :f0 f0 :f f :fd fd :a a :b b)))
      F^))
;;(grid-interpolate '(2 2 0) *test-grid*)

(defun grid-interpolate (point grid)
  "Solves Xa = f, where X = (x1, x2, ..)^T is the result of
`grid-closest-base-vectors', f = (f1 - f0, f2 - f0, ..), where f0 is
F(point), f1 = F(point + x1), f2 = (point + x2), .."
  (let* ((index-edges (closest-index-edges point (grid-axes grid)))
	 (index-points (closest-index-points index-edges))
	 (points (mapcar (bind #'index-point->grid-point (grid-axes grid)) index-points))
	 (values (mapcar (bind #'index-point->grid-value (grid-data grid)) index-points)))
    (when *grid-verbose* (print (list point index-edges index-points points values)))
    (interpolate point points values)))
;;(grid-interpolate '(1 10) *test-grid*)
;;(trace grid-interpolate)

(defun test-grid-interpolate (grid)
  (loop for ip in (grid-index-points grid)
	for gp = (index-point->grid-point ip (grid-axes grid))
	for iv = (grid-interpolate gp grid)
	for gv = (index-point->grid-value ip (grid-data grid))
	collect (abs (- iv gv))))
;;(reduce #'+ (test-grid-interpolate *test-grid*))

(defun wrap-grid (grid diameter dimension)
  "Wraps GRID in dimension DIMENSION, so the grid becomes in effect a
cylinder with diameter CYCLE-LENGTH. Currently only DIMENSION 1 is
supported."
  (assert (= dimension 1))
  (copy-object grid
      :data (with-tree (x (grid-data grid)) (expand-tree x dimension))
      :axes (list (first (grid-axes grid))
		  (expand-sequence (second (grid-axes grid)) 1 diameter))))
;;(list-grid (reshape-rao (first *raos*) (new-domain)))
;;(grid-data (first *raos*))
;;(grid-data (reshape-rao (first *raos*) (new-domain)))

(defun reshape-grid (grid new-axes &optional diameter (dimension 1))
  "Returns a new grid of same type as GRID but with grid axes defined
by NEW-AXES. The grid point values are calculated by local
interpolation, see GRID-INTERPOLATE."
  (when *grid-verbose*
    (print (list-grid grid)))
  (if (equal (grid-axes grid) new-axes)
    grid
    (if diameter
      (reshape-grid (wrap-grid grid diameter dimension) new-axes)
      (copy-object grid
	:data (grid-data (span-grid (lambda (&rest p) (grid-interpolate p grid)) new-axes))
	:axes new-axes))))
;;(list-grid (reshape-grid *test-grid* '(#(0 .5 1) #(5 9))))
;;(list-grid *test-grid*)

(defun subdivide-list (list)
  (cons (first list)
	(loop for (x y) in (pairs list)
	      collect (/ (+ x y) 2)
	      collect y)))
;;(subdivide-list '(1 2 3))


(defun subdivide (sequence)
  (with-tree (x sequence) (subdivide-list x)))
;;(subdivide #(1 2 3))

(defun stretch-list (list &optional (factor 2))
  (loop for x in list append (make-list factor :initial-element x)))

(defun stretch (sequence &optional (factor 2))
  (with-tree (x sequence) (stretch-list x factor)))
;;(stretch #(1 2 3) 4) => #(1 1 1 1 2 2 2 2 3 3 3 3)

(defun axis-point (axis ki) (elt axis ki))
;;(axis-point '(1 2 3) 0)

(defun grid-point (grid k)
  (mapcar #'elt (grid-axes grid) k))
;;(grid-point (make-grid #2A((1 2 3) (2 3 4)) '(#(1 2 3) #(1 2))) '(1 0))

(defun grid-value (grid point)
  (apply #'aref (grid-data grid) point))
;;(grid-value (make-grid #2A((1 2 3) (2 3 4)) '(#(1 2 3) #(1 2))) '(1 0))

(defun generate-sigmas (n)
  "Special made for k-pairs. N shoud be even"
  (let* ((sigma 1)
	 (res (list -1)))
    (dotimes (i n (cons 1 res))
      (push (setf sigma (* -1 sigma)) res))))
;;(generate-sigmas 6)

(defun k-pairs (new-axis axis)
  (loop with i = 0
	with sigmas = (generate-sigmas (* 2 (1- (length axis))))
	with sublist = (rcons (subdivide-list (coerce axis 'list)) most-positive-single-float)
	for x in sublist
	for k in (stretch-list (0-n (length axis)) 2)
	for sigma in sigmas
	append (loop while (and (< i (length new-axis))
				(< (axis-point new-axis i) x))
		     collect (list k sigma)
		     do (incf i)
		     do (print (list k sigma)))))
;;(k-pairs #(-1 1 2 3 4 10) #(0 4))

(defun all-k-pairs (new-grid grid)
  (mapcar #'k-pairs (grid-axes new-grid) (grid-axes grid)))
;;(all-k-pairs (make-grid #2A((1 2 3) (2 3 4)) '(#(1 2 3) #(1 2))) (make-grid #2A((1 2 3) (2 3 4)) '(#(1 2 3) #(1 2))))

(defun interpolate-grid-point (new-grid grid k-new-grid &optional (all-k-pairs (all-k-pairs new-grid grid)))
  (let* ((k-pairs (mapcar #'elt all-k-pairs k-new-grid))
	 (k0 (mapcar #'car k-pairs))
	 (x0 (grid-point grid k0))
	 (ki-list (loop for (nil sigma) in k-pairs
			for i from 0
			collect (let ((res (copy-list k0)))
				  (incf (nth i res) sigma)
				  res)))
	 (xi-list (loop for ki in ki-list collect (grid-point grid ki)))
	 (yi-list (loop for ki in ki-list collect (grid-value grid ki)))
	 (x (grid-point new-grid k-new-grid))
	 (y0 (grid-value grid k0)))
    (+ y0 (loop for i from 0
		for x_i in x
		for x0_i in x0
		for xi in xi-list
		for xi_i = (elt xi i)
		for yi in yi-list
		for delta-xi = (- xi_i x0_i)
		sum (/ (* (- x_i x0_i) (- yi y0))
		       delta-xi)))))

(defmethod rank ((x grid)) (rank (grid-data x)))

(defun interpolate-spectrum (new-grid grid)
  "Assume two dimensional grids"
  (assert (= 2 (rank new-grid) (rank grid)))
  (loop with data = (grid-data new-grid)
	with all-k-pairs = (all-k-pairs new-grid grid)
	with (n m) = (dimensions new-grid)
	for i below n do
	(loop for j below m do
	      (setf (aref data i j)
		    (interpolate-grid-point
		     new-grid grid (list i j) all-k-pairs))))
  new-grid)
;;(list-grid (interpolate-spectrum (make-grid #2A((1 2 3) (2 3 4)) '(#(1 1.5) #(1 2 3))) (make-grid #2A((1 2 3) (2 3 4)) '(#(1 2) #(1 2 3)))))
