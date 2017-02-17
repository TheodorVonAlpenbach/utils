(in-package :topology)

(defgeneric diameter2 (x)
  (:documentation "Returns the squared diameter of GEOMETRY. The idea
  of introducing this method is that very often diameter2 is an
  intermediate result in order to calculate diameter. Also diameter2
  is useful in many algorithms linear algebra"))

(defmethod diameter2 ((x segment))
  (norm2 x))
;;(diameter2 (make-segment '(1 1) '(3 2)))

(defun mmg (lpoints)
  "Short cut for make multipoints. Only for testing"
  (make-multi-geometry (mapcar #'make-point lpoints)))
;;(mmg '((1 2)(2 4)))

(defmethod diameter2 ((x geometry))
  "Slow, but safe"
  (loop for (p q) in (relations (points x))
	maximize (diameter2 (make-segment p q))))
;;(diameter2 (mmg '((1 2) (3 4) (0 0))))

(defmethod diameter2 ((x point))
  "Default diameter is 0. It applies automatically to all 1d objects, like
  point."
  0)

(defmethod diameter2 ((x interval))
  (sq (diameter x)))

(defmethod diameter2 ((x box))
  (+ (diameter2 (x-range x))
     (diameter2 (y-range x))))


(defgeneric diameter (geometry)
  (:documentation "Returns the diameter of GEOMETRY"))

(defmethod diameter ((x geometry))
  (sqrt (diameter2 x)))
;;(diameter (mmg '((1 2) (3 4) (0 0))))

(defmethod diameter ((x interval))
  (- (end x) (start x)))

