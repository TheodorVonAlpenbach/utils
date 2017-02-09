(in-package :topology)

;;;; The concept of convexity is always in the anti-clockwise
;;;; direction in this module.

;;shortcuts (only for testing)
(defun ms (x1 y1 x2 y2) (make-segment `(,x1 ,y1) `(,x2 ,y2)))
(defun mss (&rest xs-and-ys)
  (loop for (s e) in (pairs (cut xs-and-ys))
	collect (make-segment s e)))
(defun mpl (&rest xs-and-ys) (make-path (cut xs-and-ys)))
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

(defmethod convex-hull ((x path)) (convex-hull (segments x)))
(defmethod convex-hull ((x polygon)) (convex-hull (boundary x)))
;;(convex-hull (mpg 0 0  1 1  2 0  1 1  2 2  1 1  0 2  1 1))

(defmethod convex3-p (p1 p2 p3)
  "Return T iff the path p1--p2--p3 is convex"
  (plusp (cross-product (g- p2 p1) (g- p3 p1))))
;;(convex3-p '(0 0)'(1 0)'(2 0))

(defmethod merge-convex-paths ((x segment) (y segment) &optional non-contiguous-p)
  (when non-contiguous-p
    (error "MERGE-CONVEX-PATHS not implemented for non-contiguous segments"))
  (if (convex3-p (start x) (end x) (end y))
    (make-instance 'convex-path :segments (list x y))
    (make-convex-path (list (start x) (end y)))))
;;(merge-convex-paths (ms 0 0 1 0) (ms 1 0 1 -1))

(defmethod merge-paths ((x path) (y path))
  (make-instance 'path :segments (append (segments x) (segments y))))
;;(merge-paths (mpl 0 0 1 0) (mpl 1 1 0 1))

(defmethod merge-paths ((x path) (y cons))
  (reduce #'merge-paths y :initial-value x))
;;(merge-paths (mpl 0 0 1 0) (list (mpl 1 1 0 1) (mpl -1 1 -1 0)))

(defmethod merge-convex-paths ((x convex-path) (y convex-path) &optional non-contiguous-p)
  ;;pseudo code
  (merge-convex-paths
   (subsequence x 0 -1)
   (merge-convex-paths (last-elt x) (first y))
   (subsequence y 1)))

;;(unintern 'merge-convex-paths)


