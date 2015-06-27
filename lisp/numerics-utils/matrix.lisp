;;;; This module is a simple implementation of the ordinary linear
;;;; algebra entity matrix. The matrix is simply a two dimensional
;;;; array. This is way the implementation is somewhat porblematic in
;;;; that almost every method here could be useful as an
;;;; twodimensional array utilty.

;;;; An intermediate solution to this dilemma is as follows: The
;;;; methods that is likely be applied to non-numeric arrays are
;;;; placed in mb-utils. The rest is found here.
(in-package :numerics-utils)

(defun zeros (dimensions &optional (zero 0))
  (make-array dimensions :initial-element zero))
;;(zeros '(3 2))

(defun ones (dimensions) (zeros dimensions 1))
;;(ones '(3 2))

(defun diagonal-matrix (vec)
  (let* ((n (length vec))
	 (res (zeros (list n n))))
    (loop for i below n
	  do (setf (aref res i i) (aref vec i)))
    res))
;;(diagonal-matrix #(1 2 3))

(defun identity-matrix (n &optional (one 1))
  (diagonal-matrix (make-array (list n) :initial-element one)))
;;(identity-matrix 3)
;;(adjust-array (identity-matrix 3) '(6 3))

(defun insert-matrix (a b &optional (row-offset 0) (column-offset 0))
  "Inserts the contents of matrix B in A.
This could perhaps be renamed to INSERT-ARRAY."
  (loop for ai from row-offset
        for bi below (array-dimension b 0)
	do (loop for aj from column-offset
		 for bj below (array-dimension b 1)
		do (setf (aref a ai aj) (aref b bi bj)))))

(defun concatenate-matrix-columns (&rest matrices)
  "Concatenates MATRICES columnwise.
This could perhaps be renamed to CONCATENATE-MATRIX-COLUMNS."
  (let* ((dims (mapcar #'array-dimensions matrices))
	 (res (make-array (list (caar dims) (reduce #'+ (mapcar #'second dims))))))
    (loop for m in matrices
	  for column-offset = 0 then (+ column-offset (array-dimension m 1))
	 do (insert-matrix res m 0 column-offset))
    res))
;;(concatenate-matrix-columns (identity-matrix 3) (identity-matrix 3 2) (identity-matrix 3 3))

(defun column->matrix (column)
  "COLUMN is a sequence"
  (make-array (list (length column) 1) :initial-contents (map 'list #'list column)))
;;(column->matrix #(1 2))

(defun columns->matrix (columns)
  "COLUMNS is a sequence of sequences"
  (apply #'concatenate-matrix-columns (map 'list #'column->matrix columns)))
;;(columns->matrix (list #(1 2) #(1 2)))

(defun rows->matrix (rows) (matrix-transpose (columns->matrix rows)))

(defun get-first-non-zero-row-in-col (a column &optional (start-row 0))
  (let ((n (array-dimension a 0)))
    (do ((i start-row (+ i 1)))
	((or (= i n)
	     (not (= (aref a i column) 0)))
	 (unless (= i n) i)))))
;;(get-first-non-zero-row-in-col (zeros '(3 3)) 2)

(defun divide-row-with-scalar (a i c)
  "Divides each element in row I in matrix A with scalar C. Destructive."
  (loop for j below (array-dimension a 1)
	do (setf (aref a i j) (/ (aref a i j) c)))
  a)
;;(divide-row-with-scalar (ones '(3 3)) 3 2.0)

(defun submatrix (a &key (rows (0-n (array-dimension a 0))) (columns (0-n (array-dimension a 1))))
  (let ((rows (listify rows))
	(columns (listify columns)))
    (let ((res (make-array (list (length rows) (length columns)))))
      (loop for i in (listify rows) for ri from 0
	    do (loop for j in (listify columns) for rj from 0
		     do (setf (aref res ri rj) (aref a i j))))
      res)))
;;(submatrix (identity-matrix 3))

(defun matrix-inversion (a)
  "Inverts square matrix A"
  (let* ((n (array-dimension a 0))
	 (c (concatenate-matrix-columns a (identity-matrix n))))
      (do*
       ((y 0 (+ y 1)))
       ((or (= y n) (not c)))
        (let ((z (get-first-non-zero-row-in-col c y y)))
          (cond
            ((not z) (setf c nil))
            (t
             (if (> z y)
                 (swap-rows c y z))
             (divide-row-with-scalar c y (aref c y y))
             (do*
	      ((i 0 (+ i 1)))
	      ((= i n))
               (unless (= i y)
                 (let ((k (aref c i y)))
                   (dotimes (j (* n 2))
                     (setf (aref c i j) (+ (aref c i j)
					   (* (- k)
					      (aref c y j))))))))))))

      (unless (not c)
        (submatrix c :columns (a-b n (1- (* 2 n)))))))
;;(matrix-inversion #2A((1 2) (3 4)))

(defun matrix-product(a1 a2 &key (inner-fn #'safe-*) (outer-fn #'+))
  "Not exactly correct, but should work for square matrices"
  (destructuring-bind (n1 m1) (array-dimensions a1)
    (destructuring-bind (n2 m2) (array-dimensions a2)
      (assert (= m1 n2))
      (let ((res (make-array (list n1 m2))))
	(loop for r1 below n1 do
	     (loop for c2 below m2 do
		  (setf (aref res r1 c2)
			(reduce outer-fn
				(loop for i below n2
				      collect (funcall inner-fn (aref a1 r1 i) (aref a2 i c2)))))))
	res))))
;;(matrix-product asd (matrix-inversion asd))
;;(setf asd #2A((1 2) (3 4)))

(defun span-matrix (fn row-span column-span)
  (warn "This function is obsolete. Use span-array instead.")
  (span-array fn row-span column-span))
;;(span-matrix #'* #(1 2 3) '(1 2 3))

(defun make-column (sequence)
  (make-array (list (length sequence) 1)
    :initial-contents (map 'list #'list sequence)))
;;(make-column #(1 2 3))

(defun make-row (sequence)
  (make-array (list 1 (length sequence))
    :initial-contents (list sequence)))
;;(make-row #(1 2 3))

;;; these methods should not be exported
(defun map-matrix (fn &rest matrices) (apply #'map-array fn matrices))
(defun matrix-row (matrix i) (array-row matrix i))
(defun matrix-rows (matrix) (array-rows matrix))
(defun matrix-columns (matrix) (matrix-rows (matrix-transpose matrix)))

(defun map-matrix-rows (fn array &optional (type 'vector)) (map-array-rows fn array type))
(defun matrix-reverse-rows (a) (array-reverse-rows a))

(defun matrix-column (matrix &optional (j 0))
  "Returns the Jth column of MATRIX as a VECTOR"
  (make-array (array-dimension matrix 0) :displaced-to (submatrix matrix :columns j)))
;;(matrix-column #2A((0.1962614) (0.16367006)))


(defun matrix-transpose (matrix &optional conjugate)
  (let ((res (make-array (nreverse (array-dimensions matrix)))))
    (loop for i below (array-dimension res 0)
	  do (loop for j below (array-dimension res 1)
		   for value = (aref matrix j i)
		   do (setf (aref res i j) (if conjugate (conjugate value) value))))
    res))

(defun conjugate-transpose (matrix) (matrix-transpose matrix t))

(defun hermitianize (matrix)
  "Returns the Hermitian matrix (A*)A"
  (matrix-product (conjugate-transpose matrix) matrix))
;;(hermitianize mat)

(defun bidiagonalize (matrix)
  "Using Golub-Kahan-Lanczos Bidiagonalization, 
see http://web.eecs.utk.edu/~dongarra/etemplates/node198.html"
  (matrix-product (conjugate-transpose matrix) matrix))
;;(bidiagonalize mat)

(defun singular-value-decompose (matrix)
  (let (sigma u v) (values sigma u v)))
;;(singular-value-decompose matrix)

(defun singular-values (matrix)
  (matrix-diagonal (singular-value-decompose matrix)))
;;(singular-value-decompose matrix)

(defvar *default-norm* '(L 2))
(defun L-norm (matrix &optional (order 2))
  (expt (loop for i below (array-total-size matrix)
	      sum (expt (row-major-aref matrix i) order))
	(/ 1 order)))


(defun scale-matrix (scalar matrix)
  (map-array (bind #'* scalar) matrix))
;;(scale-matrix 17 #(0 1 2))

(defun matrix-norm (matrix &optional (norm *default-norm*))
  (cond ((and (consp norm) (eql (first norm) 'L))
	 (L-norm matrix (second norm)))))
;;(matrix-norm (vector (coerce (sqrt 2) 'double-float) (coerce (sqrt 2) 'double-float)))

(defun normalize-matrix (matrix &optional (norm *default-norm*))
  "Not sure if this is useful for anything else than vectors"
  (scale-matrix (/ 1 (matrix-norm matrix norm)) matrix))
;;(normalize-matrix #(1 1))

(defun matrix-distance (matrix1 matrix2 &optional (norm *default-norm*))
  (matrix-norm (map-array #'- matrix1 matrix2) norm))
;;(matrix-distance (vector (sqrt 2) (sqrt 2)) (vector (sqrt 2) 0))

(defun vector-inner-product (u v)
  (reduce #'+ (map 'vector #'* u v)))
;;(vector-inner-product #(1 2) #(1 2))

(defun project-vector (u v)
  "Projects vector U on vector V"
  (scale-matrix
   (/ (vector-inner-product u v)
      (vector-inner-product v v))
   v))
;;(project-vector (vector (random 1.0) (random 1.0)) #(1 1))

(defun matrix-difference (matrix &rest matrices)
  (apply #'map-array #'- matrix matrices))

(defun m- (&rest args) (apply #'matrix-difference args))
;;(m- #(1) #(1) #(1))

(defun zero-vector-p (vector) (every #'zerop vector))

(defun gram-schmidt (vectors &optional (norm-epsilon 1E-4))
  (loop for a in vectors
	for u = a then (apply #'m- a (mapcar (bind #'project-vector a 1) u-units))
	when (> (matrix-norm u) norm-epsilon)
	collect (normalize-matrix u) into u-units
	finally return u-units))
;;(gram-schmidt (list #(1 0) #(1 0) #(1 1)))

(defun gram-schmidt-test (&optional (vectors (list #(1 2 3) #(2 4 6) #(0 0 1))) (norm-epsilon 1E-4))
  (let ((units (gram-schmidt vectors)))
    (loop for (e1 e2) in (combinations units)
	  for ip = (vector-inner-product e1 e2)
	  for ip* = (if (< (abs ip) norm-epsilon) 0 ip)
	  collect (list e1 e2 ip*))))
;;(gram-schmidt-test)

(defmethod rank ((vectors cons))
  "Returns the rank of MATRIX.
This is a very simple and not so cost-effective version.
TODO: Use SINGULAR-VALUE-DECOMPOSE to calculate rank."
  (length (gram-schmidt vectors)))

(defmethod rank ((matrix array))
  "Returns the rank of MATRIX.
This is a very simple and not so cost-effective version.
TODO: Use SINGULAR-VALUE-DECOMPOSE to calculate rank."
  (rank (if (apply #'< (array-dimensions matrix))
	  (matrix-rows matrix) (matrix-columns matrix))))
;;(rank sing)
;;(setf sing #2A((2 4 6) (1 1 1) (1 2 3)))

(defun map-rows (fn matrix &key (start 0) end)
  (rows->matrix (mapcar fn (subseq (matrix-rows matrix) start end))))
;;(map-rows (bind #'scale-matrix 2 1) sing :start 1)

;;; Simpex stuff. simplex is a list of points The main target for now
;;; is to decide if a point is within a simplex Need to compute
;;; determinant, and then I need some deep linear algebra functions
(defun n-parallelepiped-volume (points)
  (determinant points))

(defun simplex-volume (simplex)
  (/ (n-parallelepiped-volume simplex)
     (faculty (length simplex))))

(defun equal-signum-1 ()
  (mapcar #'signum numbers))

(defun equal-signum (&rest numbers)
  (mapcar #'signum numbers))

(defun within-simplex (x simplex &optional strictly-p)
  (apply #'= (loop for i below (length simplex)
		   for sgn = (signum (simplex-volume (replace-nth i x simplex)))
		   if (not (and strictly-p (zerop sgn)))
		   collect sgn)))
