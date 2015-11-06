(in-package :topology)

;;; CROSS-PRODUCT
(defmethod cross-product ((x (eql nil)) (y cons))
  "Special specialization, first argument is NIL representing the X-axis."
  (second y))
;;(cross-product nil '(1 1))

(defmethod cross-product ((x cons) (y cons))
  "Since we know this is 2D, we shouldn't invoke the heavy
NUMERICS-UTILS::DETERMINANT."
  (- (* (first x) (second y)) (* (second x) (first y))))
;;(cross-product '(234 456) '(567 34))

(defmethod cross-product ((x point) (y point)) (cross-product (coordinates x) (coordinates y)))
(defmethod cross-product ((x segment) (y segment)) (cross-product (direction x) (direction y)))
(defmethod cross-product ((x segment) (y point)) (cross-product (direction x) (p- y (start x))))

(defmethod cross-product ((x cons) (y (eql nil))) (second x))
(defmethod cross-product ((x point) (y (eql nil))) (second (coordinates x)))
(defmethod cross-product ((x segment) (y (eql nil))) (second (coordinates (direction x))))

;;; ANGLE (no specialization so far)
(defmethod abs-angle (x y)
  "Absolute angle in radians between geometries X and Y.
Absolute here means that the result is always in [0 PI] and that
\(abs-angle X Y) == (abs-angle Y X)."
  (acos (/ (inner-product x y)
	   (norm (centre x)) (norm (centre y)))))

(defmethod abs-angle (x (y (eql nil)))
  (acos (/ (inner-product x y) (norm (centre x)))))

(defmethod angle (x y)
  (if (minusp (cross-product x y))
    (- (abs-angle x y))
    (abs-angle x y)))
;;(angle '(1 0) '(-1 -1))
;;(angle '(-1 0) nil)
;;(angle (make-segment '(0 0) '(-1 0)) nil)

(defmethod orientation (x) (angle x nil))
(defmethod orientation ((x point)) (orientation (coordinates x)))
(defmethod orientation ((x segment)) (orientation (direction x)))
(defmethod orientation ((x ellipse)) (orientation (major-axis x)))
;;(orientation (make-ellipse (make-segment '(0 -2) '(0 3)) 1))
