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
(defmethod cross-product ((x segment) (y point)) (cross-product (direction x) (g- y (start x))))

(defmethod cross-product ((x cons) (y (eql nil))) (second x))
(defmethod cross-product ((x point) (y (eql nil))) (second (coordinates x)))
(defmethod cross-product ((x segment) (y (eql nil))) (second (coordinates (direction x))))

;;; ANGLE (no specialization so far)
(defmethod abs-angle ((x point) (y point))
  "Absolute angle in radians between geometries X and Y.
Absolute here means that the result is always in [0 PI] and that
\(abs-angle X Y) == (abs-angle Y X)."
  (acos (/ (inner-product x y) (norm x) (norm y))))

(defmethod abs-angle ((x point) (y nil))
  (acos (/ (inner-product x nil) (norm x))))

(defmethod abs-angle (x y)
  (abs-angle (centre x) (centre y)))

(defmethod abs-angle (x (y (eql nil)))
    (acos (/ (inner-product x nil) (norm (centre x)))))

(defmethod angle (x y)
  (if (minusp (cross-product x y))
    (- (abs-angle x y))
    (abs-angle x y)))
;;(angle '(1 0) '(-1 -1))
;;(angle '(-1 0) nil)
;;(angle (make-segment '(0 0) '(-1 0)) nil)
;;(abs-angle (make-segment '(0 0) '(-1 0)) nil)
;;(inner-product (make-segment '(0 0) '(-1 0)) nil)
;;(norm (centre (make-segment '(0 0) '(-1 0))))
;;(norm (centre nil))

(defun angle-between-segments (s1 s2)
  (angle (direction s1) (direction s2)))
;;(angle-between-segments (ms 0 0 1 0) (ms 0 0 0 1))

(defmethod orientation (x) (angle x nil))
(defmethod orientation ((x point)) (orientation (coordinates x)))
(defmethod orientation ((x segment)) (orientation (direction x)))
(defmethod orientation ((x ellipse)) (orientation (major-axis x)))
;;(orientation (make-ellipse (make-segment '(0 -2) '(0 3)) 1))
