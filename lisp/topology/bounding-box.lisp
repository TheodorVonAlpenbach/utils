(in-package :topology)

(defmethod bounding-box ((g geometry))
  (loop for (x y) in (coordinates g)
	minimize x into xmin
	maximize x into xmax
	minimize y into ymin
	maximize y into ymax
	finally (return (make-box xmin xmax ymin ymax))))
;;(bounding-box (ms 0 0 1 1))

(defmethod bounding-box ((p point))
    "Return the cartesian bounding box for GEOMETRY. The format
  is ((xmin xmax) (ymin ymax)). However, this could be changed later
  to (x-interval y-interval)"
  (destructuring-bind (x y) (coordinates p)
    (make-box x x y y)))
;;(bounding-box (mp 0 1))
