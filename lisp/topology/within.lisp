;;;; This package contains the methods WITHIN and INSIDE.
;;;; (WITHIN X Y) simply means that X is an element or a subset of Y.
;;;; (INSIDE X Y) == (and (WITHIN X Y) (not (WITHIN X (BOUNDARY Y)))),
;;;; i.e. X must be stricly inside Y.
(in-package :topology)

;;; INSIDE
(defmethod inside ((x point) (y triangle))
  (warn "Not checked")
  ;; (every #'plusp (boundary y) (bind #'cross-product x))
  (every #'plusp (mapcar (bind #'cross-product x) (boundary y))))
;;(within (make-point '(1/2 1/2)) (make-triangle '(0 0) '(1 0) '(0 1)))

;;; WITHIN
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

(defun relative-distance-from-ellipse-center2 (p e)
  (norm2 (g/ (g- (rotate-around p (orientation e) (centre e)) (centre e)) (radii e))))

(defun relative-distance-from-ellipse-center (p e)
  (sqrt (relative-distance-from-ellipse-center2 p e)))
;;(relative-distance-from-ellipse-center (make-point '(0.5 0)) (make-ellipse (make-segment '(-1 0) '(1 0)) 1))

(defmethod within ((p point) (e ellipse))
  (<= (relative-distance-from-ellipse-center2 p e) 1))

(defmethod inside ((p point) (e ellipse))
  (< (relative-distance-from-ellipse-center2 p e) 1))
;;(within (make-point '(10 0)) (make-ellipse (make-segment '(-1 0) '(1 0)) 1))
;;(g/ (make-point '(0 0)) '(1 1/2))

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

(defmethod projection-parameter-points ((p point) (p1 point) (p2 point))
  "Returns the relative distance from the projection of P on the line segment P2 - P1 to P1 and P2.
If the result is 1 the projection equals P1, if 0 it is equal to P2.
Other values in [0 1] gives the position on the segment accordingly.
If negative, the projection is on the continuation of the segment
closest to P2, if positive closest to P1."
  (let ((dp (g- p2 p1)))
    (/ (inner-product p dp) (norm2 dp))))
;;(projection-parameter-points (mp 1 0) (mp 0 0) (mp 1 1))

(defmethod projection-parameter ((p point) (s segment))
  "See point specialization only"
  (projection-parameter-points p (start s) (end s)))
;;(projection-parameter (mp 1 0) (ms 0 0 1 1))

;;;DISTANCE2
;; TODO instead of (distance2 x nil), check if diameter2 is the same. In that case this should be used instead, since it is so much clearer
;; Move distance a separate module
(defgeneric distance2 (x y))
(defmethod distance2 ((x cons) (y (eql nil))) (reduce #'+ (mapcar #'sq x)))
(defmethod distance2 ((x cons) (y cons)) (distance2 (g- x y) nil))
(defmethod distance2 ((x point) (y point)) (distance2 (coordinates x) (coordinates y)))

(defmethod distance2 ((x point) (y segment))
  (let ((c (projection-parameter x y)))
    (print c)
    (if (<= 0 c 1)
      ;; the projection of X on segment Y is WITHIN Y
      (- (distance2 x (start y))
	 (* (sq c) (diameter2 y)))
      ;; projection of X is outside Y
      (if (minusp c)
	(distance2 x (start y))
	(distance2 x (end y))))))
;;(distance2 (mp 1 0) (ms 0 1 100.0 100))

(defmethod distance2 ((x segment) (y point)) (distance2 y x))
(defmethod distance2 ((x segment) (y segment))
  (min (distance2 (start x) y)
       (distance2 (end x) y)))
;;(distance2 (ms 0 0 1 0) (ms 0 1 100 100))

;;DISTANCE
(defgeneric distance (x y))
(defmethod distance (x y) (sqrt (distance2 x y)))
;;(distance (make-point '(2 0)) (make-segment '(0 0) '(1 0)))
