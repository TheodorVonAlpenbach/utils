(in-package :topology)

;;;; This module implements the simple operators
;;;; +, -, *, / for GEOMETRY objects.

(defgeneric add-geometry (x y))
(defgeneric negate-geometry (x))
(defgeneric multiply-geometry (x y))
(defgeneric invert-geometry (x))

(defmethod add-geometry ((x cons) (y cons)) (mapcar #'+ x y))
(defmethod add-geometry ((x point) (y cons)) (make-point (add-geometry (coordinates x) y)))
(defmethod add-geometry ((x point) (y point)) (make-point (add-geometry (coordinates x)
									(coordinates y))))
(defmethod add-geometry ((x segment) y) (make-segment (add-geometry (start x) y)
						      (add-geometry (end x) y)))
(defmethod negate-geometry ((x number)) (- x))
(defmethod negate-geometry ((x cons)) (mapcar #'- x))
(defmethod negate-geometry ((x point)) (make-point (negate-geometry (coordinates x))))
(defmethod negate-geometry ((x segment)) (make-segment (end x) (start x)))
(defmethod multiply-geometry ((x cons) (c number)) (mapcar (bind #'* c) x))
(defmethod multiply-geometry ((x cons) (y cons)) (mapcar #'* x y))
(defmethod multiply-geometry ((x cons) (y point)) (multiply-geometry x (coordinates y)))
(defmethod multiply-geometry ((x point) y) (make-point (multiply-geometry (coordinates x) y)))
(defmethod multiply-geometry ((x segment) c) (make-segment (multiply-geometry (start x) c)
							   (multiply-geometry (end x) c)))
(defmethod invert-geometry ((x number)) (/ x))
(defmethod invert-geometry ((x cons)) (mapcar #'/ x))
(defmethod invert-geometry ((x point)) (make-point (invert-geometry (coordinates x))))

;;; short cuts
(defun g+ (&rest addends) (reduce #'add-geometry addends))
;;(g+ (make-point '(0 0)) (make-point '(2 0)))

(defun g- (minuend &rest subtrahends)
  (if subtrahends
    (add-geometry minuend (negate-geometry (reduce #'add-geometry subtrahends)))
    (negate-geometry minuend)))

(defun g* (&rest factors) (reduce #'multiply-geometry factors))

(defun g/ (numerator &rest denominators)
  (if denominators
    (multiply-geometry numerator (invert-geometry (reduce #'multiply-geometry denominators)))
    (invert-geometry numerator)))

(defmethod gaverage (geometries)
  "Average of GEOMETRIES"
  (g/ (reduce #'g+ geometries) (length geometries)))
;;(gaverage (list (make-point '(0 0)) (make-point '(2 0))))

;;; CENTRE
(defgeneric centre (x) (:documentation "The centre of geometry X as a POINT"))
(defmethod centre ((x geometry)) nil)
(defmethod centre ((x cons)) (if (geometry-p (first x)) (gaverage x) x))
(defmethod centre ((x multi-geometry)) (centre (elements x)))
(defmethod centre ((x point)) x)
(defmethod centre ((x segment)) (centre (points x)))
(defmethod centre ((x ellipse)) (centre (major-axis x)))
;;(centre (make-ellipse (make-segment '(0 -2) '(0 3)) 1))

;;; DIRECTION
(defmethod direction (x) x) ;default
(defmethod direction ((x segment)) (g- (end x) (start x)))
;;(direction (make-segment '(0 0) '(1 0)))

;;; INNER-PRODUCT
(defmethod inner-product ((x cons) (y cons)) (reduce #'+ (mapcar #'* x y)))
(defmethod inner-product ((x point) (y point)) (inner-product (coordinates x) (coordinates y)))
(defmethod inner-product ((x segment) (y segment)) (inner-product (direction x) (direction y)))
(defmethod inner-product ((x cons) (y (eql nil))) (declare (ignore y)) (first x))
(defmethod inner-product ((x point) (y (eql nil))) (declare (ignore y)) (first (coordinates x)))
(defmethod inner-product ((x segment) (y (eql nil))) (inner-product (direction x) y)) ;; could make (direction nil) return nil and skip this generic function
;;(inner-product (make-segment '(0 0) '(1 0)) nil)

;;; NORM2, NORM and NORMALIZE
(defmethod norm2 (x) (inner-product x x))
(defmethod norm (x) (sqrt (norm2 x)))
(defmethod normalize (x) (g/ x (norm x)))

;;; DIAMETER2 and DIAMETER
(defmethod diameter2 (x) (norm2 x))
(defmethod diameter2 ((x ellipse)) (diameter2 (major-axis x)))
(defmethod diameter (x) (sqrt (diameter2 x)))
