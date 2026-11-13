;;; mb-utils-matrix.el --- Vector and matrix utility functions -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'mb-utils-math)

;;; Code:

(defconst vec-default-length 3
  "Default vector length and square-matrix dimension.")

(defun mb-matrix--check-size (size)
  "Signal an error unless SIZE is a nonnegative integer."
  (cl-check-type size (integer 0 *)))

(defun mb-matrix--check-operands (operands kind)
  "Signal an error unless OPERANDS contain at least one KIND."
  (unless operands
    (error "At least one %s is required" kind)))

(defun mb-matrix--check-vectors (vectors)
  "Signal an error unless VECTORS all have the same length."
  (when vectors
    (let ((length (length (cl-first vectors))))
      (dolist (vector (cl-rest vectors))
	(unless (= (length vector) length)
	  (error "Vector dimensions do not agree"))))))

(defun mb-matrix--columns (matrix)
  "Return the number of columns in rectangular MATRIX."
  (if (null matrix)
      0
    (let ((columns (length (cl-first matrix))))
      (dolist (row (cl-rest matrix))
	(unless (= (length row) columns)
	  (error "Matrix is not rectangular")))
      columns)))

(defun mb-matrix--check-same-dimension (matrices)
  "Signal an error unless MATRICES have the same dimensions."
  (when matrices
    (let ((dimension (mat-dimension (cl-first matrices))))
      (dolist (matrix (cl-rest matrices))
	(unless (equal (mat-dimension matrix) dimension)
	  (error "Matrix dimensions do not agree"))))))

;;; vec generators
(cl-defun vec (p1 p2)
  "Return the vector from point P1 to point P2."
  (vec-subtract p2 p1))

(cl-defun vec-constant (&optional (n vec-default-length) (c 0))
  "Return a vector with N elements, each equal to C."
  (mb-matrix--check-size n)
  (cl-loop for i below n collect c))

(cl-defun vec-a-b (a b)
  "Return the inclusive sequence A, A + 1, ..., B."
  (cl-loop for i from a to b collect i))

(cl-defun vec-0-n-1 (n)
  "Return the N integers from zero through N minus one."
  (mb-matrix--check-size n)
  (vec-a-b 0 (1- n)))

(cl-defun vec-random (&optional (n vec-default-length) (a 0) (b 1) (integerp nil) seed)
  "Return a vector of N random values between A and B.
When INTEGERP is non-nil, generate integers.  When SEED is non-nil,
randomize the generator before producing the vector."
  (mb-matrix--check-size n)
  (when seed
    (random t))
  (cl-loop for i below n
	   collect (random* a b integerp)))

(cl-defun vec-unit (&optional (i 0) (n vec-default-length) (magnitude 1))
  "Return a vector of N elements with MAGNITUDE at index I."
  (mb-matrix--check-size n)
  (cl-check-type i (integer 0 *))
  (unless (< i n)
    (error "Vector index %d is outside [0, %d)" i n))
  (let ((vec (vec-constant n 0)))
    (setf (nth i vec) magnitude)
    vec))

;;; vec operators
(cl-defun vec-add (&rest vecs)
  "Return the elementwise sum of VECS."
  (mb-matrix--check-operands vecs "vector")
  (mb-matrix--check-vectors vecs)
  (apply #'cl-mapcar #'+ vecs))

(cl-defun vec-subtract (&rest vecs)
  "Subtract the remaining VECS elementwise from the first vector."
  (mb-matrix--check-operands vecs "vector")
  (mb-matrix--check-vectors vecs)
  (apply #'cl-mapcar #'- vecs))

(cl-defun vec-elt-mult (&rest vecs)
  "Return the elementwise product of VECS."
  (mb-matrix--check-operands vecs "vector")
  (mb-matrix--check-vectors vecs)
  (apply #'cl-mapcar #'* vecs))

(cl-defun vec-scalar-mult (v c)
  "Multiply every element of vector V by scalar C."
  (mapcar (lambda (element) (* element c)) v))

(cl-defun vec-sum (v)
  "Return the sum of the elements in vector V."
  (cl-reduce #'+ v :initial-value 0))

(cl-defun scalar-product (&rest vecs)
  "Return the generalized scalar product of VECS."
  (mb-matrix--check-operands vecs "vector")
  (vec-sum (apply #'vec-elt-mult vecs)))

(cl-defun vec-sqr (v)
  "Return the squared Euclidean length of vector V."
  (scalar-product v v))

(cl-defun vec-length (v)
  "Return the Euclidean length of vector V."
  (sqrt (vec-sqr v)))

(cl-defun vec-direction (v)
  "Return the unit vector in the direction of V."
  (let ((length (vec-length v)))
    (when (zerop length)
      (error "Zero vector has no direction"))
    (vec-scalar-mult v (/ 1.0 length))))

(cl-defun vec-dyadic-product (v1 v2)
  "Return the dyadic product of vectors V1 and V2."
  (mapcar (lambda (element) (vec-scalar-mult v2 element)) v1))

(cl-defun vec-distance (v1 v2)
  "Return the Euclidean distance between vectors V1 and V2."
  (vec-length (vec-subtract v1 v2)))

(cl-defun vec-curl (v1 v2)
  "Return the signed two-dimensional determinant of V1 and V2."
  (unless (and (= (length v1) 2) (= (length v2) 2))
    (error "Oriented vector angle requires two-dimensional vectors"))
  (- (* (cl-first v1) (cl-second v2))
     (* (cl-second v1) (cl-first v2))))

(cl-defun vec-angle (v1 v2)
  "Return the counterclockwise angle from 2D vector V1 to V2.
The result is in radians in the half-open interval from zero to 2 pi."
  (let* ((sp (scalar-product (vec-direction v1) (vec-direction v2)))
	 (angle (acos (max -1.0 (min 1.0 sp)))))
    (if (cl-minusp (vec-curl v1 v2))
	(- (* 2 float-pi) angle)
      angle)))
;;; mat queries
(cl-defun mat-rows (m)
  "Return the number of rows in matrix M."
  (length m))

(cl-defun mat-columns (m)
  "Return the number of columns in rectangular matrix M."
  (mb-matrix--columns m))

(cl-defun mat-dimension (m)
  "Return the dimensions of rectangular matrix M as (ROWS COLUMNS)."
  (list (mat-rows m) (mat-columns m)))

;;; vec-mat operators
(cl-defun mat-vec-mult (m v)
  "Return the product of matrix M and column vector V."
  (unless (= (mat-columns m) (length v))
    (error "Matrix and vector dimensions do not agree"))
  (mapcar #'(lambda (row) (scalar-product row v)) m))

(cl-defun vec-mat-mult (v m)
  "Return the product of row vector V and matrix M."
  (unless (= (length v) (mat-rows m))
    (error "Vector and matrix dimensions do not agree"))
  (mapcar (lambda (column) (scalar-product v column)) (transpose m)))

;;; mat unary operators

(cl-defun frobenius-norm (m)
  "Return the Frobenius norm of matrix M."
  (sqrt (vec-sum (mapcar #'vec-sum (hadamard-product m m)))))

(cl-defun vec-pseudo-inverse (v)
  "Return the pseudoinverse of vector V.
This computes V+ = V' / (V' V)."
  (let ((square (vec-sqr v)))
    (when (zerop square)
      (error "Zero vector has no pseudoinverse"))
    (vec-scalar-mult v (/ 1.0 square))))

(cl-defun mat-pseudo-inverse (m)
  "Return the pseudoinverse of one-row or one-column matrix M."
  (cond
   ((= (mat-rows m) 1)
    (transpose (list (vec-pseudo-inverse (car m)))))
   ((= (mat-columns m) 1)
    (list (vec-pseudo-inverse (car (transpose m)))))
   (t
    (error "General matrix pseudoinverse is not implemented"))))

(cl-defun mat-scalar-mult (matrix scalar)
  "Multiply every element of MATRIX by SCALAR."
  (mapcar (lambda (row) (vec-scalar-mult row scalar)) matrix))

;;; mat generators
(cl-defun mat-constant (&optional (n vec-default-length) (m vec-default-length) (c 0))
  "Return a matrix with M rows and N columns, each element equal to C."
  (mb-matrix--check-size n)
  (mb-matrix--check-size m)
  (cl-loop for j below m collect (vec-constant n c)))

(cl-defun mat-diagonal (vec)
  "Return a square matrix with VEC on its diagonal."
  (cl-loop for elt in vec
	   for i from 0
	   collect (vec-unit i (length vec) elt)))

(cl-defun mat-identity (&optional (n vec-default-length))
  "Return the N by N identity matrix."
  (mb-matrix--check-size n)
  (mat-diagonal (vec-constant n 1)))


;;; mat binary operators
(cl-defun mat-mat-mult (m1 m2)
  "Return the matrix product of M1 and M2."
  (unless (= (mat-columns m1) (mat-rows m2))
    (error "Matrix dimensions do not agree"))
  (mapcar (lambda (row) (vec-mat-mult row m2)) m1))

(cl-defun mat-mat-mult* (&rest mats)
  "Return the matrix product of MATS in argument order."
  (mb-matrix--check-operands mats "matrix")
  (if (cl-rest mats)
      (mat-mat-mult (cl-first mats)
		    (apply #'mat-mat-mult* (cl-rest mats)))
    (cl-first mats)))

;;; mat extended binary operators
(cl-defun mat-add  (mat1 mat2)
  "Return the elementwise sum of matrices MAT1 and MAT2."
  (mb-matrix--check-same-dimension (list mat1 mat2))
  (cl-mapcar #'vec-add mat1 mat2))

(cl-defun mat-subtract  (mat1 mat2)
  "Subtract matrix MAT2 elementwise from MAT1."
  (mat-add mat1 (mat-scalar-mult mat2 -1)))

(cl-defun hadamard-product (&rest mats)
  "Return the elementwise product of MATS."
  (mb-matrix--check-operands mats "matrix")
  (mb-matrix--check-same-dimension mats)
  (apply #'cl-mapcar #'vec-elt-mult mats))


;;; Permutation matrices
(cl-defun mat-permutation (vec-permutation)
  "Return the permutation matrix represented by VEC-PERMUTATION."
  (unless (equal (cl-sort (copy-sequence vec-permutation) #'<)
		 (vec-0-n-1 (length vec-permutation)))
    (error "Not a permutation vector: %S" vec-permutation))
  (let ((identity (mat-identity (length vec-permutation))))
    (mapcar (lambda (index) (nth index identity)) vec-permutation)))

(cl-defun mat-permutation-row-switch (n i j)
  "Return the N by N permutation matrix that swaps rows I and J."
  (mb-matrix--check-size n)
  (dolist (index (list i j))
    (cl-check-type index (integer 0 *))
    (unless (< index n)
      (error "Matrix index %d is outside [0, %d)" index n)))
  (let ((permutation (vec-0-n-1 n)))
    (cl-rotatef (nth i permutation) (nth j permutation))
    (mat-permutation permutation)))

(provide 'mb-utils-matrix)

;;; mb-utils-matrix.el ends here
