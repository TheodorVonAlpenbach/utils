(in-package :topology)

(defclass segment (geometry)
  ((start :initarg :start :accessor start :type point)
   (end :initarg :end :accessor end :type point)))

(defmethod print-object ((x segment) stream)
  (print-unreadable-object (x stream :type t)
    (prin1 (list :start (start x) :end (end x)) stream)))
;;(make-segment '(0 0) '(1 0))

(defmethod make-segment ((start point) (end point))
  (make-instance 'segment :start start :end end))
;;(make-segment (make-point '(0 0)) (make-point '(1 0)))

(defmethod make-segment ((start sequence) (end sequence))
  (make-segment (make-point start) (make-point end)))
;;(make-segment '(0 0) '(1 0))
;;(trace make-segment)
