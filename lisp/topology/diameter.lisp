(in-package :topology)

(defgeneric diameter2 (x)
  (:documentation "Returns the squared diameter of GEOMETRY. The idea
  of introducing this method is that very often diameter2 is an
  intermediate result in order to calculate diameter. Also diameter2
  is useful in many algorithms linear algebra"))

(defmethod diameter2 ((x geometry))
  "Default diameter is 0. It applies automatically to all 1d objects, like
  point."
  0)

(defmethod diameter2 ((x segment))
  (inner-product x))
;;(diameter2 (make-segment '(1 0) '(0 0)))

(defgeneric diameter (geometry)
  (:documentation "Returns the diameter of GEOMETRY"))

(defmethod diameter ((x geometry))
  (sqrt (diameter2 x)))
;;(diameter (make-segment '(0 0) '(1 0)))

