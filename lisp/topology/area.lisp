(in-package :topology)

(defgeneric area (geometry)
  (:documentation "Returns the area of GEOMETRY"))

(defmethod area ((x geometry))
  "Default area is 0. It applies automatically to all 0d objects, like
  point, and to all 1d objects, like segment, polyline etc."
  0)

(defmethod area ((x multi-geometry))
  "Default area is 0. It applies automatically to all 1d objects, like
  point, segment, polyline etc."
  (reduce #'+ (elements x) :key #'area))

(defmethod area ((x polygon))
  "Default area is 0. It applies automatically to all 1d objects, like
  point, segment etc."
  (* 1/2 (abs (loop with cs = (coordinates x)
		    for ((xa ya) (xb yb)) in (pairs (rcons cs (first cs)))
		    sum (* (- xb xa) (+ ya yb))))))
;;(area (make-polygon '((0 0) (1 0) (2 1) (3 0) (3 2) (2 3) (1 2) (0 2))))
;;(area (make-triangle '(0 0) '(1 0) '(2 1)))
;;(plot (make-triangle '(0 0) '(1 0) '(2 1)))
;;(plot (make-polygon '((0 0) (1 0) (2 1) (3 0) (3 2) (2 3) (1 2) (0 2))))


