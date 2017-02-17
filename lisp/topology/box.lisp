(in-package :topology)

(defclass box (geometry)
  ((x-range :initarg :x-range
	  :accessor x-range
	  :type interval
	  :documentation "The X range of the box")
   (y-range :initarg :y-range
	  :accessor y-range
	  :type interval
	  :documentation "The Y range of the box")))

(defun make-box (xmin xmax ymin ymax)
  (make-instance 'box
    :x-range (make-interval xmin xmax)
    :y-range (make-interval ymin ymax)))
;;(make-box 0 2 1 2)

(defmethod print-object ((x box) stream)
  (print-unreadable-object (x stream :type t)
    (princ (list (x-range x) (y-range x)) stream)))
