(in-package :topology)

(defclass path (geometry)
  ((segments :initarg :segments :accessor segments :type list)))

(defmethod make-path (points)
  (make-instance 'path
    :segments (loop for (start end) in (pairs points)
		    collect (make-segment start end))))
;;(trace make-path)

(defmethod head ((x path)) (first (segments x)))
(defmethod tail ((x path)) (last-elt (segments x)))
(defmethod start ((x path)) (start (head x)))
(defmethod end ((x path)) (end (tail x)))

(defmethod print-object ((x path) stream)
  (print-unreadable-object (x stream :type t)
    (princ (mapcar #'coordinates (points x)) stream)))
;;(make-path '((0 0) (1 0) (1 1) (0 1)))
