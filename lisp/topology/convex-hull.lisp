(in-package :topology)

;;;; The concept of convexity is always in the anti-clockwise
;;;; direction in this module.

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
  (declare (ignore non-contiguous-p))
  (merge-convex-paths
   (subseq* x 0 -1)
   (merge-convex-paths (last-elt x) (first y))
   (subseq* y 1)))

;;(unintern 'merge-convex-paths)
(defun all-segments-closer-between-p (s1 s2)
  "Return NIL iff a point is closer to S1 than S2 on a convex path." 
  (let ((nu (normalize (g- (start s2) (end s1))))
	(ns1 (normalize (direction s1)))
	(ds2 (direction s2)))
    (plusp (inner-product ds2 (g+ nu ns1)))))
;;(all-segments-closer-between-p (ms -1 0 0 0) (ms 4 3 5 3))

(defun all-segments-closer-between2 (s1 s2)
  "Alternative implementation of all-segments-closer-between-p
using intersection point." 
  (awhen (line-intersect s1 s2)
    (< (distance s1 it)
       (distance s1 s2))))

(defun all-segments-closer-between3 (s1 s2 ns1 ns2)
  "Like all-segments-closer-between-p, but with preprocessed data"
  (let ((nu (normalize (g- (start s2) (end s1)))))
    (plusp (inner-product ns2 (g+ nu ns1)))))
;;(let* ((s1 (ms -1 0 0 0)) (s2 (ms 4 3 5 3)) (ns1 (normalize (direction s1)))(ns2 (normalize (direction s2)))) (time (all-segments-closer-between3 s1 s2 ns1 ns2)))

(defmethod line-intersect-p ((x cons) (y cons))
  "Returns the intersection of lines X and Y.
X are a pairs of points, (X1 X2) and (Y1 Y2), signifying the lines
going through the points X1 and X2, and Y1 and Y2, respectively. The
intersection points is calculating by solving for S in the following
line equations:

T*X2 + (1-T)*X1 = S*Y2 + (1-S)*Y1
"
  (let* ((dx (g- (second x) (first x)))
	 (dy (g- (second y) (first y)))
	 (dz (g- (first y) (second x)))
	 (pdx (coordinates dx))
	 (pdy (coordinates dy))
	 (denominator (- (* (first pdy)
			    (second pdx))
			 (second pdy))))
    (unless (zerop denominator) ;;otherwise no intersection --> NIL
      (let ((s (/ (cross-product dx dz) denominator)))
	(g+ (g* (second y) s)
	    (g* (first y) (- 1 s)))))))
;;(line-intersect-p (ms 0 0 1 0) (ms 4 3 5 3))

(defmethod line-intersect-p ((x segment) (y segment))
  (line-intersect-p (points x) (points y)))
