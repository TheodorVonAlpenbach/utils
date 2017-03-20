(in-package :topology)

;;; ANGLE (no specialization so far)
(defmethod abs-angle ((x point) (y point))
  "Absolute angle in radians between geometries X and Y.
Absolute here means that the result is always in [0 PI] and that
\(abs-angle X Y) == (abs-angle Y X)."
  (acos (/ (inner-product x y) (norm x) (norm y))))

(defmethod abs-angle ((x point) (y (eql nil)))
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
