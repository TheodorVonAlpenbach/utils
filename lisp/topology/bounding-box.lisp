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

(defun make-interval (min max)
  (make-instance 'interval :start min :end max))
;;(make-interval 0 1)

(defmethod print-object ((x interval) stream)
  (print-unreadable-object (x stream :type t)
    (princ (list (start x) (end x)) stream)))

(defclass bounding-box (geometry)
  ((x-range :initarg :x-range
	  :accessor x-range
	  :type interval
	  :documentation "The X range of the box")
   (y-range :initarg :y-range
	  :accessor y-range
	  :type interval
	  :documentation "The Y range of the box")))

(defun make-bounding-box (xmin xmax ymin ymax)
  (make-instance '
      'interval :start min :end max))
;;(make-interval 0 1)

(defmethod print-object ((x interval) stream)
  (print-unreadable-object (x stream :type t)
    (princ (list (start x) (end x)) stream)))

(defclass bounding-box (geometry)
  ((x-range :initarg :x-range
	  :accessor x-range
	  :type interval
	  :documentation "The X range of the box")
   (y-range :initarg :y-range
	  :accessor y-range
	  :type interval
	  :documentation "The Y range of the box")))

(defun make-bounding-box (xmin xmax ymin ymax)
  (make-instance 'bounding-box
    :x-range (make-interval xmin xmax)
    :y-range (make-interval ymin ymax)))
;;(make-bounding-box 0 2 1 2)

(defun mbb (&rest args) (apply #'make-bounding-box args))
;;(mbb 0 1 0 1)

(defmethod print-object ((x bounding-box) stream)
  (print-unreadable-object (x stream :type t)
    (princ (list (x-range x) (y-range x)) stream)))

(defmethod points ((bb bounding-box))
  (list (make-point (list (start (x-range bb))
			  (start (y-range bb))))
	(make-point (list (end (x-range bb))
			  (start (y-range bb))))
	(make-point (list (end (x-range bb))
			  (end (y-range bb))))
	(make-point (list (start (x-range bb))
			  (end (y-range bb))))))
;;(points (mbb 0 1 2 3))

(defmethod bb ((p point))
    "Return the cartesian bounding box for GEOMETRY. The format
  is ((xmin xmax) (ymin ymax)). However, this could be changed later
  to (x-interval y-interval)"
  (destructuring-bind (x y) (coordinates p)
    (make-bounding-box x x y y)))
;;(bb (mp 0 1))
;;(coordinates (mp 0 1))

(defmethod bb ((g geometry))
  (loop for (x y) in (coordinates g)
	minimize x into xmin
	maximize x into xmax
	minimize y into ymin
	maximize y into ymax
	finally (return (make-bounding-box xmin xmax ymin ymax))))
;;(bb (ms 0 0 1 1))

