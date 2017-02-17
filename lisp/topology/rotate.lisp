(in-package :topology)

(defmethod rotate ((p cons) (theta number))
  (let ((c (cos theta)) (s (sin theta)))
    (destructuring-bind (x y) p
      (list (- (* x c) (* y s)) (+ (* x s) (* y c))))))
;;(rotate '(1 0) (/ pi 2))

(defmethod rotate ((p point) (radians number))
  (make-point (rotate (coordinates p) radians)))
;;(rotate (make-point '(1 0)) pi)

(defmethod rotate ((x segment) (y number))
  (make-segment (rotate (start x) y) (rotate (end x) y)))
;;(rotate (make-segment '(-1 0) '(1 0)) (/ pi 2))

(defmethod rotate-around (g theta (around point))
  (g+ (rotate (g- g around) theta) around))
;;(rotate-around (make-segment '(-1 0) '(1 0)) (/ pi 2) (make-point '(-1 0)))

