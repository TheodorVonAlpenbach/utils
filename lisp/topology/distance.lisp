(in-package :topology)

;;; DISTANCE2

(defgeneric distance2 (x y)
  (:documentation
   "Return the square of the distance between geometries X and Y."))

(defmethod distance2 ((x cons) (y (eql nil)))
  "Return distance2 for a coordinate tuple to the origin."
  (reduce #'+ (mapcar #'sq x)))

(defmethod distance2 ((x cons) (y cons))
  (distance2 (g- x y) nil))

;; point
(defmethod distance2 ((x point) (y point))
  (distance2 (coordinates x) (coordinates y)))

;;; p-s
(defmehod projection-parameter-points ((p point) (p1 point) (p2 point))
  "Returns the relative distance from the projection of P on the line segment P2 - P1 to P1 and P2.
If the result is 1 the projection equals P1, if 0 it is equal to P2.
Other values in [0 1] gives the position on the segment accordingly.
If negative, the projection is on the continuation of the segment
closest to P2, if positive closest to P1."
  (let ((dp (g- p2 p1)))
    (/ (inner-product (g- p p1) dp) (norm2 dp))))
;;(projection-parameter-points (mp .1 .5) (mp 1 1) (mp 0 1))

(defmethod projection-parameter ((p point) (s segment))
  "See point specialization only"
  (projection-parameter-points p (start s) (end s)))
;;(projection-parameter (mp 1 0) (ms 0 0 1 1))

(defmethod distance2 ((x point) (y segment))
  (let ((c (projection-parameter x y)))
    (if (<= 0 c 1)
      ;; the projection of X on segment Y is WITHIN Y
      (- (distance2 x (start y))
	 (* (sq c) (diameter2 y)))
      ;; projection of X is outside Y
      (if (minusp c)
	(distance2 x (start y))
	(distance2 x (end y))))))
;;(distance (mp .1 .5) (ms 0 1 1 1))

(case '(1 1)
  ((list 1 1) 'works)
  (t 'not))

;; segment
(defun distance2-1 (s1 s2 t1 t2 p1 p2)
  (if (eql p1 :on)
    (if (eql p2 :on)
      0
      (distance2 s1 (if (eql p2 :behind) (start s2) (end s2))))
    (if (eql p1 :behind)
      (if (eql p2 :behind)
	(if (< (* (sq t1) (diameter2 s1))
	       (* (sq t1) (diameter2 s2)))
	  (distance2 s1 (start s2))
	  (distance2 s2 (start s1)))
	(if (eql p2 :ahead)
	  (distance2 (end s2) (start s1))
	  (distance2-1 s2 s1 t2 t1 p2 p1)))
      (if (eql p1 p2 :ahead)
	(if (< (* (sq (1- t1)) (diameter2 s1))
	       (* (sq (1- t2)) (diameter2 s2)))
	  (distance2 s1 (end s2))
	  (distance2 s2 (end s1)))
	(distance2-1 s2 s1 t2 t1 p2 p1)))))

(defmethod distance2 ((s1 segment) (s2 segment))
  (destructuring-bind (t1 t2) (line-intersection-coeffs s1 s2)
    (flet ((pos (x) (if (minusp x) :behind (if (> x 1) :ahead :on))))
      (distance2-1 s1 s2 t1 t2 (pos t1) (pos t2)))))
;;(distance2 (ms -1/2 0 100 100) (ms 0 0 1 0))

;; composite objects
(defmethod distance2 ((x path) y)
  (loop for s in (segments x)
	minimize (distance2 x y)))
;;(distance (mp -.1 .5) (mpa 0 0 1 0 1 1 0 1))

(defmethod distance2 ((x multi-geometry) y)
  (loop for g in (elements x) minimize (distance2 g y)))


;; mirrors
(defmethod distance2 (x (y point)) (distance2 y x))
(defmethod distance2 (x (y segment)) (distance2 y x))
(defmethod distance2 (x (y path)) (distance2 y x))
(defmethod distance2 (x (y multi-geometry)) (distance2 y x))

;;DISTANCE
;; Should this be a generic function? Is there any case where
;; this could be specialized better without working via distance2?!
(defgeneric distance (x y))
(defmethod distance (x y) (sqrt (distance2 x y)))
