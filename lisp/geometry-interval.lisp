;;;; Should be moved into some utility package, for instance :math.
(require "geometry")
(in-package :geometry)

(defclass interval ()
  ((left-boundary
    :accessor left-boundary
    :initarg :left-boundary
    :initform 0
    :documentation "The left boundary of an interval")
   (right-boundary
    :accessor right-boundary
    :initarg :right-boundary
    :initform 0
    :documentation "The right boundary of an interval"))
  (:documentation "A half open interval, including the left boundary"))

(defun make-interval (x y)
  (make-instance 'interval :left-boundary x :right-boundary y))

(defmethod equal-geometry ((x interval) (y interval))
  (and (equal (left-boundary x) (left-boundary y))
       (equal (right-boundary x) (right-boundary y))))
;;(equal-geometry (make-interval 0 1) (make-interval 0 2))

(defmethod boundary ((x interval))
  (and x (list (left-boundary x) (right-boundary x))))

(defun interval-length (x)
  (- (right-boundary x) (left-boundary x)))

(defmethod noverlap ((x interval) (y interval))
  "This method is destructive on X (but X only)."
  (with-accessors ((lx left-boundary) (rx left-boundary)) x
    (with-accessors ((ly left-boundary) (ry left-boundary)) y
      (when (< lx ly) (setf lx ly))
      (when (> rx ry) (setf rx ry))
      (unless (> lx ry) x))))
;;(let ((x (make-interval 0 4)) (y (make-interval 4 6))) (mapcar #'interval-list (list (noverlap x y) x y)))

(defmethod contains-p ((interval interval) (x number))
  "Note that interval class is by default half closed, with an open right boundary."
 (and (<= (left-boundary interval) x)
      (< x (right-boundary interval))))

(defmethod nsplit-interval ((interval interval) (x number))
  "Splits INTERVAL in two values if it contains X. Otherwise return INTERVAL as is."
  (if (contains-p interval x)
    (let ((right-interval (copy-object interval)))
      (setf (right-boundary interval) x)
      (setf (left-boundary right-interval) x)
      (values interval right-interval))
    interval))
;;(let ((iv (make-interval 0 2))) (list iv (multiple-value-list (nsplit-interval iv 100))))

(defmethod split-interval ((interval interval) (x number))
  "Splits INTERVAL in two values if it contains X. Otherwise return INTERVAL as is."
  (nsplit-interval (copy-object interval) x))
;;(let ((iv (make-interval 0 2))) (list iv (multiple-value-list (split-interval iv 100))))
;;(multi-split (make-interval 3/4 9/4) #'split-interval (a-b 1 3))

(defmethod nshift ((x interval) shift)
  "Destructively shifts the start time of duration X"
  (incf (left-boundary x) shift)
  (incf (right-boundary x) shift)
  x)

(defmethod bounding-box (&rest intervals)
  (make-interval (reduce #'min (mapcar #'left-boundary intervals))
		 (reduce #'max (mapcar #'right-boundary intervals))))
;;(bounding-box (make-interval 1 2) (make-interval -1 2) (make-interval 5 6))

(defmethod print-object-simple ((x interval) stream)
  (print-unreadable-object (x stream :type t)
    (format stream "(~d ~d)" (left-boundary x) (right-boundary x))))
;;(make-interval 'a 'b)

(provide "geometry-interval")
