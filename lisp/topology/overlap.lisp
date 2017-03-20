(in-package :topology)

(defmethod overlap-p ((p point) (s segment))
  (zerop (distance2 p s)))
;;(overlap-p (ms 0 0 .9 0) (ms 0 0 .9 0))

(defmethod overlap-p ((s1 segment) (s2 segment))
  (destructuring-bind (t1 t2) (line-intersection-coeffs s1 s2)
    (if t1
      (and (<= 0 t1 1) (<= 0 t2 1))
      ;; s1 and s2 are parallel
      (overlap-p (start s1) s2))))
;;(overlap-p (ms 0 0 3 0) (ms 1 0 2 0))

(defmethod line-intersection ((s1 segment) (s2 segment))
  (let* ((u (g- (start s2) (start s1)))
	 (v1 (direction s1))
	 (v2 (direction s2))
	 (t2 (line-intersection-t u v1 v2)))
    (g+ (g* v2 t2) (start s2))))
;;(line-intersection (ms 0 0 2 0) (ms 1 -1 1 1))

