(in-package :topology)

(defgeneric cross-product (x y)
  (:documentation "Return the cross product between geometries X and Y."))

(defmethod cross-product (x y)
  "Default method. Not implemented."
  (error "Cross-product is not implemented for these types."))

(defmethod cross-product ((x geometry) y)
  "Special generalization"
  (- (cross-product y x)))

(defmethod cross-product ((x (eql nil)) (y cons))
  "Special specialization, first argument is NIL representing the X-axis."
  (second y))
;;(cross-product nil '(1 1))

(defmethod cross-product ((x cons) (y (eql nil)))
  (second x))
(defmethod cross-product ((x cons) (y cons))
  "Since we know this is 2D, we shouldn't invoke the heavy
NUMERICS-UTILS::DETERMINANT."
  (- (* (first x) (second y)) (* (second x) (first y))))
;;(cross-product '(234 456) '(567 34))

(defmethod cross-product ((x point) (y geometry))
  (- (cross-product y x)))
(defmethod cross-product ((x point) (y (eql nil)))
  (second (coordinates x)))
(defmethod cross-product ((x point) (y point))
  (cross-product (coordinates x) (coordinates y)))

(defmethod cross-product ((x segment) (y geometry))
  (- (cross-product y x)))
(defmethod cross-product ((x segment) (y (eql nil)))
  (second (coordinates (direction x))))
(defmethod cross-product ((x segment) (y segment))
  (cross-product (direction x) (direction y)))
(defmethod cross-product ((x segment) (y point))
  (cross-product (direction x) (g- y (start x))))




