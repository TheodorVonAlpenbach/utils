(in-package :topology)

(defmethod left-of-p ((x geometry) (y geometry))
  "Default implementation. Not particularily slow."
  (minusp (cross-product x y)))
;;(left-of-p (mp 0 1) (ms 0 0  -1 0))
