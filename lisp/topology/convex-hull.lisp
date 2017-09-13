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

(defmethod line-intersection ((s1 segment) (s2 segment))
  "Return the intersection POINT of segments S1 and S2,
or NIL if it does not exist."
  (let* ((u (g- (start s2) (start s1)))
	 (v1 (direction s1))
	 (v2 (direction s2))
	 (t2 (line-intersection-coeff2-1 u v1 v2)))
    (when t2 (g+ (g* v2 t2) (start s2)))))
;;(line-intersection (ms 0 0 2 0) (ms 1 -1 1 1))
;;(trace line-intersection)

(defmethod line-intersection-coeffs ((s1 segment) (s2 segment))
  (flet ((x (p) (first (coordinates p)))
	 (y (p) (second (coordinates p))))
    (let* ((u (g- (start s2) (start s1)))
	   (v1 (direction s1))
	   (v2 (direction s2))
	   (t2 (line-intersection-coeff2-1 u v1 v2))
	   (t1 (awhen t2
		 (if (zerop (x v1))
		   (/ (+ (* it (y v2)) (y u)) (y v1))
		   (/ (+ (* it (x v2)) (x u)) (x v1))))))
      (list t1 t2))))
;;(line-intersection-coeffs (ms 1 2  1 3) (ms 4 5  6 7))
;;(line-intersection-coeffs (ms 1 0 4 0) (ms 0 1 0 3))

(defmethod line-intersection-coeff2 ((x segment) (y segment))
  "Return the second intersection coefficient of X and Y"
  (line-intersection-t (g- (start y) (start x)) (direction x) (direction y)))
;;(line-intersection-coeff2 (ms 0 0 2 0) (ms 1 -1 1 1))

(defmethod line-intersection-coeff2-1 (u v1 v2)
  "Return the T-value of vector V2 at the crossing with V1 + U.
For instance let S1 = (P11 P12) and S2 = (P21 P22) be two line segments.
Let Vi = Pi2 - Pi1, and U = P21 - P11.

A helper for LINE-INTERSECTION-COEFF2"
  (let ((denominator (cross-product v1 v2)))
    (unless (zerop denominator) (/ (cross-product u v1) denominator))))
;;(line-intersection-coeff2-1 '(1 -1) '(2 0) '(0 2))
