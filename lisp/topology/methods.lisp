(in-package :topology)

(defgeneric length (geometry)
  (:documentation "Returns the area of 1D GEOMETRY."))

(defgeneric area (geometry)
  (:documentation "Returns the area of 2D GEOMETRY."))
