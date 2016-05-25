(defpackage :geometry
    (:use :common-lisp :mb-utils)
    (:export 
     :equal-geometry
     :interval :make-interval :boundary :interval-length :left-boundary :right-boundary
     :nsplit-interval :split-interval :nshift
     :within))

(in-package :geometry)

(defmethod overlap (x y)
  (noverlap (copy-object x) y))
;;(let ((x (make-interval 0 4)) (y (make-interval 4 6))) (mapcar #'interval-list (list (overlap x y) x y)))

(provide "geometry")
