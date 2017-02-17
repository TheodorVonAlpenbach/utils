(in-package :topology)

(defclass ellipse (geometry)
  ((major-axis :initarg :major-axis :accessor major-axis :type segment)
   (minor-diameter :initarg :minor-diameter :accessor minor-diameter :type number)))

(defmethod make-ellipse ((major-axis segment) (minor-diameter number))
  (make-instance 'ellipse :major-axis major-axis :minor-diameter minor-diameter))
;;(make-ellipse (make-segment '(0 -2) '(0 2)) 2)

(defmethod make-ellipse ((major-axis segment) (minor-axis segment))
  (make-instance 'ellipse :major-axis major-axis :minor-diameter (diameter minor-axis)))

(defmethod minor-axis ((x ellipse))
  (scale (rotate-around (major-axis x) (/ pi 2) (centre x))
	 (/ (minor-diameter x) (norm (major-axis x)))
	 (centre x)))
;;(minor-axis (make-ellipse (make-segment '(0 -2) '(0 2)) 2))

(defmethod major-radius ((x ellipse)) (/ (diameter x) 2))
(defmethod minor-radius ((x ellipse)) (/ (minor-diameter x) 2))
;;(minor-radius (make-ellipse (make-segment '(0 -2) '(0 2)) 2))

(defmethod radii ((x ellipse))
  "Return the radii of the ELLIPSE's axes as a pair."
  (list (major-radius x) (minor-radius x)))
;;(radii (make-ellipse (make-segment '(0 -2) '(0 2)) 2))

(defmethod make-ellipse-axis ((radius number) &optional radians center)
  "Return the segement ((-RADIUS 0) (RADIUS 0)),
i.e. a horizontal line of length 2*RADIUS centered at (0 0). If
RADIANS is specified, the result is rotated this amount. If CENTER is
specified is should be a point designator, and the center of the
result is translated to this point."
  (let ((s (make-segment (list (- radius) 0) (list radius 0))))
    (when radians (setf s (rotate s radians)))
    (if center (g+ s center) s)))
;;(make-ellipse-axis 1 (/ pi 2) '(1 1))

(defmethod make-ellipse-origin (major-radius minor-radius &optional radians center)
  "RADIANS is the angle from x-axis to major-axis"
  (make-ellipse (make-ellipse-axis major-radius radians center) (* 2 minor-radius)))
;;(make-ellipse-origin 2 1)
