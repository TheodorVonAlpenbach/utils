(in-package :topology)

;;shortcuts (only for testing)
(defun ms (x1 y1 x2 y2) (make-segment `(,x1 ,y1) `(,x2 ,y2)))
(defun mss (&rest xs-and-ys) (loop for (s e) in (pairs (cut xs-and-ys)) collect (make-segment s e)))
(defun mpl (&rest xs-and-ys) (make-polyline (cut xs-and-ys)))
(defun mpg (&rest xs-and-ys) (make-polygon (cut xs-and-ys)))
;;(mpg 0 0  1 0  1 1)

(defmethod join-convex ((x point) (convex-segments cons))
  (loop for (cs . css) on convex-segments
	for s = (make-segment x (start cs))
	if (plusp (cross-product s cs)) return (cons s (cons cs css))
	finally (return (list (make-segment x (end cs))))))
;;(join-convex (ms 0 2  1 1) (list (ms 1 1  0 0)))
;;(trace join-convex)

(defmethod join-convex ((x segment) (convex-segments cons))
  (join-convex (start x) convex-segments))
;;(join-convex (ms 0 0 1 0) (list (ms 0 0 1 0) (ms 0 0 1 1)))

(defmethod convex-hull ((segments cons))
  (when segments
    (if (atom (rest segments))
      segments
      (join-convex (first segments) (convex-hull (rest segments))))))

(defmethod convex-hull ((x polyline)) (convex-hull (segments x)))
(defmethod convex-hull ((x polygon)) (convex-hull (boundary x)))
;;(convex-hull (mpg 0 0  1 1  2 0  1 1  2 2  1 1  0 2  1 1))
