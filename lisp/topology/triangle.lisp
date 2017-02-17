(in-package :topology)

(defclass triangle (polygon) ())

(defmethod make-triangle (x1 x2 x3)
  (make-instance 'triangle :boundary (make-path (list x1 x2 x3 x1))))
;;(mapcar #'coordinates (mapcar #'start (boundary (make-triangle '(0 0) '(1 0) '(0 1)))))
;;(type-of (boundary (make-triangle '(0 0) '(1 0) '(0 1))))
;;(apply #'make-triangle (mapcar #'make-point '((0 0) (1 0) (0 1))))
;;(coordinates (apply #'make-triangle (mapcar #'make-point '((0 0) (1 0) (0 1)))))
;;(unintern 'make-triangle)
;;(segments (make-triangle '(0 0) '(1 0) '(0 1)))

;; Seems strange to define a copy of superclass method, but ok...
(defmethod print-object ((x triangle) stream)
  (print-unreadable-object (x stream :type t)
    (princ (mapcar #'coordinates (points (boundary x))) stream)))
