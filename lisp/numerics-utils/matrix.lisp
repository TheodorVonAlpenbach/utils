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
;;(submatrix (identity-matrix 3) :columns 1)
;;(submatrix #2A((1 0 0 1 0 0) (0 1 0 0 1 0) (0 0 1 0 0 1)) :columns (loop for i from 3 below (* 2 3)))

(defun invert-matrix (a)
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
;;(invert-matrix #2A((1 2) (3 4)))

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
;;(matrix-product asd (invert-matrix asd))
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
(defun map-matrix (fn &rest args) (apply #'map-array fn args))
(defun map-matrix-rows (fn array &optional (type 'vector)) (map-array-rows fn array type))
(defun matrix-reverse-rows (a) (array-reverse-rows a))

(defun matrix-column (matrix &optional (j 0))
  (make-array (array-dimension matrix 1) :displaced-to (submatrix matrix :columns j)))
;;(mapcar (bind #'matrix-column #2A((0.1962614) (0.16367006)) 1) '(0))
