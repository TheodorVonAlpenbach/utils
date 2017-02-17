(in-package :topology)

;;; interval
(defclass interval (geometry)
  ((start :initarg :start
	  :accessor start
	  :type number
	  :documentation "Lower bound of an interval")
   (end :initarg :end
	:accessor end
	:type number
	:documentation "Upper bound of an interval")))

(defmethod print-object ((x interval) stream)
  (print-unreadable-object (x stream :type t)
    (princ (list (start x) (end x)) stream)))

(defun make-interval (min max)
  (make-instance 'interval :start min :end max))
;;(make-interval 0 1)
