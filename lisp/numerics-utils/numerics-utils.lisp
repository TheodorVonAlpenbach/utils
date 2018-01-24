(in-package :numerics-utils)

(defun imod (number from to)
  "Return NUMBER modulo the interval [FROM TO[.
Example: Find the radian in [-π, π[
(imod (* 3/2 pi) (- pi) pi)
==> -1.5707963267948966d0"
  (+ from (mod (- number from) (- to from))))
;;(imod (* 3/2 pi) (- pi) pi)
