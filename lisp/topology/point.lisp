(in-package :topology)

(defclass point (geometry)
  ((coordinates :initarg :coordinates :accessor coordinates :type list
		:documentation "An n dimensional point")))

(defmethod print-object ((x point) stream)
  (print-unreadable-object (x stream :type t)
    (princ (coordinates x) stream)))
;;(make-point '(1 1))

(defmethod make-point (sequence)
  (make-instance 'point :coordinates (coerce sequence 'list)))
;;(coordinates (make-point '(1 0)))
