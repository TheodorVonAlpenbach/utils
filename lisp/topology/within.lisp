;;;; This package contains the methods WITHIN and INSIDE.
;;;; (WITHIN X Y) simply means that X is an element or a subset of Y.
;;;; (INSIDE X Y) == (and (WITHIN X Y) (not (WITHIN X (BOUNDARY Y)))),
;;;; i.e. X must be stricly inside Y.
(in-package :topology)

(defmethod cross-product ((x cons) (y cons))
  (determinant (tree->array (list x y))))

(defmethod cross-product ((x point) (y point))
  (cross-product (coordinates x) (coordinates y)))

(defmethod cross-product ((x segment) (y segment))
  (cross-product (segment-vector x) (segment-vector y)))
;;(cross-product (make-segment '(0 0) '(1 0)) (make-segment '(0 0) '(0 1)))

(defmethod cross-product ((x segment) (y point))
  (cross-product (segment-vector x) (p- y (start x))))
;;(cross-product (make-segment '(0 0) '(1 0)) (make-point '(-1 0)))

(defmethod inside ((x point) (y triangle))
  (every #'plusp (boundary y) :key (bind #'cross-product x)))
;;(within (make-point '(1/2 1/2)) (make-triangle '(0 0) '(1 0) '(0 1)))

(defmethod within ((x number) (y cons))
  (destructuring-bind (a b) y (or (<= a x b) (<= b x a))))
;;(within -1 '(2 0))

(defmethod within ((x point) (y triangle))
  (notany #'minusp (mapcar (bind #'cross-product x) (segments (boundary y)))))
;;(within (make-point '(1/2 1/2)) (make-triangle '(0 0) '(1 0) '(0 1)))

(defmethod within ((x geometry) (y geometry))) ;;default nil

(defmethod within ((x point) (y segment))
  (destructuring-bind (xis ais bis)
      (mapcar #'coordinates (list x (start y) (end y)))
    (loop for ai across ais
	  for bi across bis
	  for i from 0
	  when (/= ai bi) return
	  (when (within (aref xis i) (list ai bi))
	    (let ((s (/ (- (aref xis i) bi) (- ai bi))))
	      (every #'(lambda (x a b) (= (- x b) (* s (- a b)))) xis ais bis))))))
;;(within (make-point '(-1 -1)) (make-segment '(0 0) '(1 1)))

(defgeneric gequal (x y))

(defmethod gequal ((x point) (y point))
  (every #'equal (coordinates x) (coordinates y)))
;;(gequal (make-point '(1 2)) (make-point '(1 2)))

(defmethod gequal ((x segment) (y segment))
  (or (and (gequal (start x) (start y))
	   (gequal (end x) (end y)))
      (and (gequal (start x) (end y))
	   (gequal (end x) (start y)))))
;;(gequal (make-segment '(1 0) '(0 0)) (make-segment '(0 0) '(1 0)))

(defmethod inner-product ((x cons) &optional (y x))
  (reduce #'+ (mapcar #'* x y)))
;;(inner-product '(1 2))

(defmethod inner-product ((x point) &optional (y x))
  (inner-product (coordinates x) (coordinates y)))
;;(inner-product (make-point '(0 0)) (make-point '(1 1)))

(defmethod inner-product ((x segment) &optional (y x))
  (inner-product (segment-vector x) (segment-vector y)))
;;(inner-product (make-segment '(1 0) '(0 0)))

(defgeneric distance2 (x y))
(defmethod distance2 ((x point) (y point))
  (inner-product (mapcar #'- (coordinates x) (coordinates y))))
;;(distance2 (make-point '(0 0)) (make-point '(1 1)))

(defmethod projection-parameter-points ((p point) (p1 point) (p2 point))
  "Returns the relative distance from the projection of P on the line segment P2 - P1 to P1 and P2.
If the result is 1 the projection equals P1, if 0 it is equal to P2.
Other values in [0 1] gives the position on the segment accordingly.
If negative, the projection is on the continuation of the segment
closest to P2, if positive closest to P1."
  (let ((p12 (p- p2 p1)))
    (/ (inner-product (p- p p1) p12) (inner-product p12))))
;;(projection-parameter (make-point '(1 1)) (make-point '(0 0)) (make-point '(2 0)))

(defmethod projection-parameter ((p point) (s segment))
  "See point specialization only"
  (projection-parameter-points p (start s) (end s)))
;;(projection-parameter (make-point '(1 1)) (make-segment '(0 0) '(2 0)))

(defmethod distance2 ((x point) (y segment))
  (let ((d2-x-ya (distance2 x (start y)))
	(c (projection-parameter x y)))
    (if (<= 0 c 1)
      (- d2-x-ya (* (sq c) (inner-product y)))
      (min d2-x-ya (distance2 x (end y))))))
;;(distance2 (make-point '(-1 -1)) (make-segment '(0 0) '(1 0)))

(defmethod distance2 ((x segment) (y point)) (distance2 y x))

(defgeneric distance (x y))
(defmethod distance ((x geometry) (y geometry))
  (sqrt (distance2 x y)))
;;(untrace distance)
;;(distance (make-point '(2 0)) (make-segment '(0 0) '(1 0)))
