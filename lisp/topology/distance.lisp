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

(defmethod distance2 ((x point) (y segment))
  (let ((c (projection-parameter x y)))
    (format t "c: ~a" c)
    (if (<= 0 c 1)
      ;; the projection of X on segment Y is WITHIN Y
      (- (distance2 x (start y))
	 (* (sq c) (diameter2 y)))
      ;; projection of X is outside Y
      (if (minusp c)
	(distance2 x (start y))
	(distance2 x (end y))))))
;;(distance (mp .1 .5) (ms 0 1 1 1))

;; segment
(defmethod distance2 ((x segment) (y segment))
  ;; Caveat! This does not work for overlapping segments!
  ;; Distance between segments is always the distance between its
  ;; boundaries.

  ;; Implementation note. This function should be more
  ;; nicely implemented as
  ;; (distance2 (boundary x) (boundary y))
  ;; but this requires that
  
  ;; 1. (boundary segment) returns a multi-geometry or a derived
  ;; class, say multi-points, none of which has been implemented yet.
  ;; 2. (distance2 ((x multi-geometry) (y multi-geometry)))
  ;; has been implemented

  ;; TOREPLACE
  ;; (if (overlap-p x y) 0 (distance2 (boundary x) (boundary y)))
  
  (min (distance2 (start x) (start y))
       (distance2 (start x) (end y))
       (distance2 (end x) (start y))
       (distance2 (end x) (end y))))
;;(distance2 (ms 0 0 1 0) (ms 0 1 100 100))

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
