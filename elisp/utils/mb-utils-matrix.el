(defconst vec-default-length 3)

;;; vec generators
(defun vec (p1 p2)
  "Return the vector from point P1 to point P2"
  (vec-subtract p2 p1))
;;(vec '(1 1) '(2 2))

(cl-defun vec-constant (&optional (n vec-default-length) (c 0))
  (cl-loop for i below n collect c))
;;(vec-constant)

(cl-defun vec-a-b (a b)
  (cl-loop for i from a to b collect i))
;;(vec-a-b 0 3) 

(cl-defun vec-0-n-1 (n)
  (vec-a-b 0 (1- n)))
;;(vec-0-n-1 3)

(cl-defun vec-random (&optional (n vec-default-length) (a 0) (b 1) (integerp nil) seed)
  (cl-loop for i below n
	   collect (random* a b integerp seed)))
;;(vec-random 10)

(cl-defun vec-unit (&optional (i 0) (n vec-default-length) (magnitude 1))
  (let ((vec (vec-constant n 0)))
    (setf (nth i vec) magnitude)
    vec))
;;(vec-unit 3 5 'qwe)

;;; vec operators
(defun vec-add (&rest vecs)
  (apply #'mapcar* #'+ vecs))
;;(vec-add '(1 2) '(1 2)'(1 2))

(defun vec-subtract (&rest vecs)
  (apply #'mapcar* #'- vecs))
;;(vec-subtract '(1 2) '(1 2)'(1 2))

(defun vec-elt-mult (&rest vecs)
  (apply #'mapcar* #'* vecs))
;;(vec-elt-mult '(1 2 3) '(1 2 3))

(defun vec-scalar-mult (v c)
  (mapcar (bind #'* c) v))
;;(vec-scalar-mult '(1 2 3) 2)

(defun vec-sum (v) 
  (reduce #'+ v))
;;(vec-sum '(1 2 3))

(defun scalar-product (&rest vecs)
  (vec-sum (apply #'vec-elt-mult vecs)))
;;(scalar-product '(3 4) '(3 4))

(defun vec-sqr (v)
  (scalar-product v v))
;;(vec-sqr '(3 4))

(defun vec-length (v)
  (sqrt (vec-sqr v)))
;;(vec-length '(3 4))

(defun vec-direction (v)
  (vec-scalar-mult v (/ 1.0 (vec-length v))))
;;(vec-direction (vec-direction '(3 4)))

(defun vec-dyadic-product (v1 v2)
  (mapcar (bind #'vec-scalar-mult v2 1) v1))
;;(vec-dyadic-product '(1 2 3) '(1 1 1))

(defun vec-distance (v1 v2)
  (vec-length (vec-subtract v1 v2)))
;;(vec-distance '(0 0) '(1 1))

(defun vec-curl (v1 v2)
  (- (* (first v1) (second v2)) (* (second v1) (first v2))))
;;(vec-curl '(1 0) '(1 0))

(defun vec-angle (v1 v2)
  (let ((sp (scalar-product (vec-direction v1) (vec-direction v2))))
    (if (minusp (vec-curl v1 v2))
      (- (* 2 pi) (acos sp))
      (acos sp))))
;;(vec-angle '(1 0) '(1 -0.01))

;;; mat queries
(defun mat-rows (m) (length m))
(defun mat-columns (m) (length (first m)))
(defun mat-dimension (m) (list (mat-rows m) (mat-columns m)))
;;(mat-dimension (mat-test))

;;; vec-mat operators
(defun mat-vec-mult (m v)
  (mapcar #'(lambda (row) (scalar-product row v)) m))
;;(mat-vec-mult '((1 2) (3 4)) '(1 0))

(defun vec-mat-mult (v m)
  (mapcar #'(lambda (column) (scalar-product v column)) m))
;;(vec-mat-mult '(1 2) '((1 2) (1 2)))

;;; mat unary operators
(defun transpose (m) (apply #'mapcar* #'list m))
;;(transpose '((1 2) (1 2)))

(defun frobenius-norm (m)
  (vec-sum (mapcar #'vec-sum (hadamard-product m m))))
;;(frobenius-norm (mat-subtract (mat-test) (mat-test)))

(defun vec-pseudo-inverse (v)
  "Returns the pseudo inverse of a vector.
v+ = v'/(v'v)"
  (vec-scalar-mult v (/ 1.0 (vec-sqr v))))
;;(vec-pseudo-inverse '(1 2))

(defun mat-pseudo-inverse (m)
  "Returns the pseudo inverse of a matrix.
v+ = v'/(v'v)"
  (if (= (mat-rows m) 1)
    (transpose (list (vec-pseudo-inverse (car m))))
    (if (= (mat-columns m) 1)
      (list (vec-pseudo-inverse (car (transpose m)))))))
;;(mat-pseudo-inverse '((1 2)))
;;(mat-pseudo-inverse '((1) (2)))

(defun mat-scalar-mult (matrix scalar)
  (mapcar (bind #'vec-scalar-mult scalar) matrix))
;;(mat-scalar-mult (mat-test) 123)

;;; mat generators
(cl-defun mat-test ()
  '((1 2) (3 4)))

(cl-defun mat-constant (&optional (n vec-default-length) (m vec-default-length) (c 0))
  (cl-loop for j below m collect (vec-constant n c)))
;;(mat-constant 2 2 1)

(defun mat-diagonal (vec)
  (cl-loop for elt in vec
	   for i from 0
	   collect (vec-unit i (length vec) elt)))
;;(mat-diagonal '(1 2 3 4 5 6 7))

(cl-defun mat-identity (&optional (n vec-default-length))
  (mat-diagonal (vec-constant n 1)))
;;(mat-identity)


;;; mat binary operators
(defun mat-mat-mult (m1 m2)
  (mapcar (bind #'vec-mat-mult (transpose m2)) m1))
;;(mat-mat-mult '((1 2) (1 2)) '((1 2) (1 2)))
;;(mat-mat-mult '((1 0 0) (1 1 0) (1 0 1)) '((1 0 0) (-1 1 0) (-1 0 1)))

(defun mat-mat-mult* (&rest mats)
  (if mats
    (if (rest mats)
      (mat-mat-mult (first mats) (apply #'mat-mat-mult* (rest mats)))
      (first mats))))
;;(mat-mat-mult* '((1 2) (1 2)) '((1 2) (1 2)) '((1 0) (0 1)))

;;; mat extended binary operators
(defun mat-add  (mat1 mat2)
  (mapcar* #'vec-add mat1 mat2))
;;(mat-add (mat-constant 2 2 1) (mat-constant 2 2 1))

(defun mat-subtract  (mat1 mat2)
  (mat-add mat1 (mat-scalar-mult mat2 -1)))
;;(mat-subtract (mat-constant 2 2 1) (mat-constant 2 2 1))

(defun hadamard-product (&rest mats)
  (apply #'mapcar* #'vec-elt-mult mats))
;;(hadamard-product '((1 2) (1 2)) '((1 2) (1 2)))


;;; misc (no idea what the idea is)
(defun mat-permutation (vec-permutation)
  (mapcar (bind #'nth (mat-identity (length vec-permutation))) vec-permutation))
;;(mat-permutation '(0 2 1))

(defun mat-permutation-row-switch (n i j)
  (mat-permutation (rotate-list (vec-0-n-1 n) i j)))
;;(mat-permutation-row-switch 10 0 1)

(provide 'mb-utils-matrix)
