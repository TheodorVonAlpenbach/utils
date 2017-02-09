(in-package :topology)

(defgeneric bounding-box (geometry)
  (:documentation "Return the cartesian bounding box for GEOMETRY. The
  format is ((xmin xmax) (ymin ymax)). However, this could be changed
  later to (x-interval y-interval)"))

(defmethod bounding-box ((p point))
  (destructuring-bind (x y) (coordinates p)
    (return '((x x) (y y)))))

(defmethod bounding-box ((s segement))
  (destructuring-bind (x y) (coordinates p)
    (return '((x x) (y y)))))
