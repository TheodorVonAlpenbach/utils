(in-package :topology)

(defun geometry-p (x) (typep x 'geometry))
;;(mapcar #'geometry-p (list '(1 2) (make-point '(1 2))))

(defclass geometry () ())

(defclass multi-geometry (geometry)
  ((elements :initarg :elements :accessor elements :type list)))

(defmethod make-multi-geometry (elements)
  (make-instance 'multi-geometry :elements elements))
;;(make-multi-geometry (list (make-point '(1 2)) (make-point '(2 4)))

(defmethod points ((x multi-geometry))
  (mapcan #'points (elements x)))
;;(points (make-multi-geometry (list (make-point '(1 2)) (make-point '(2 4)))))

;;; interval
(defclass interval (geometry)
  ((start :initarg :start :accessor start :type number :documentation "Lower bound of an interval")
   (end :initarg :end :accessor end :type number :documentation "Upper bound of an interval")))

;;; point
(defclass point (geometry)
  ((coordinates :initarg :coordinates :accessor coordinates :type list
		:documentation "An n dimensional point")))

(defmethod points ((x point)) (list x))
(defmethod coordinates ((x geometry)) (mapcar #'coordinates (points x)))

(defmethod print-object ((x point) stream)
  (print-unreadable-object (x stream :type t)
    (princ (coordinates x) stream)))
;;(make-point '(1 1))

(defmethod make-point (sequence)
  (make-instance 'point :coordinates (coerce sequence 'list)))
;;(coordinates (make-point '(1 0)))

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

(defmethod points ((x segment))
  (list (start x) (end x)))
;;(points (make-segment '(1 2) '(2 4)))

(defclass path (geometry)
  ((segments :initarg :segments :accessor segments :type list)))

(defmethod make-path (points)
  (make-instance 'path
    :segments (loop for (start end) in (pairs points)
		    collect (make-segment start end))))
;;(trace make-path)

(defmethod points ((x path))
  (let ((segments (segments x)))
    (cons (start (first segments)) (mapcar #'end segments))))
;;(points (make-path '((0 0) (1 0) (1 1) (0 1))))

(defmethod print-object ((x path) stream)
  (print-unreadable-object (x stream :type t)
    (princ (mapcar #'coordinates (points x)) stream)))
;;(make-path '((0 0) (1 0) (1 1) (0 1)))

(defclass polygon (geometry)
  ((boundary :initarg :boundary :accessor boundary :type path)))

(defmethod make-polygon (points)
  (make-instance 'polygon :boundary (make-path (rcons points (first points)))))
;;(make-polygon '((0 0) (1 0) (0 1)))

(defmethod segments ((x polygon))
  (segments (boundary x)))
;;(segments (make-polygon '((0 0) (1 0) (0 1))))

(defmethod points ((x polygon))
  (butlast (points (boundary x))))
;;(points (make-polygon '((0 0) (1 0) (0 1))))

;; Seems strange to define a copy of superclass method
(defmethod print-object ((x polygon) stream)
  (print-unreadable-object (x stream :type t)
    (princ (mapcar #'coordinates (points (boundary x))) stream)))

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

(defclass ellipse (geometry)
  ((major-axis :initarg :major-axis :accessor major-axis :type segment)
   (minor-diameter :initarg :minor-diameter :accessor minor-diameter :type number)))

(defmethod make-ellipse ((major-axis segment) (minor-diameter number))
  (make-instance 'ellipse :major-axis major-axis :minor-diameter minor-diameter))
;;(make-ellipse (make-segment '(0 -2) '(0 2)) 2)

(defmethod make-ellipse ((major-axis segment) (minor-axis segment))
  (make-instance 'ellipse :major-axis major-axis :minor-diameter (diameter minor-axis)))

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

(defmethod scale (g c &optional at)
  (if at (g+ (g* (g- g at) c) at) (g* g c)))
;;(scale (make-segment '(-1 0) '(1 0)) 2 '(1 0))

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

(defun origin ()
  "Shouldn't this rather be a constant?. Also, it seems to be obsolete."
  (make-point '(0 0)))

(defmethod make-segment-origin ((radius number) &optional radians center)
  "Return the segement ((-RADIUS 0) (RADIUS 0)),
i.e. a horizontal line of length 2*RADIUS centered at (0 0). If
RADIANS is specified, the result is rotated this amount. If CENTER is
specified is should be a point designator, and the center of the
result is translated to this point."
  (let ((s (make-segment (list (- radius) 0) (list radius 0))))
    (when radians (setf s (rotate s radians)))
    (if center (g+ s center) s)))
;;(make-segment-origin 1 (/ pi 2) '(1 1))

(defmethod make-ellipse-origin (major-radius minor-radius &optional radians center)
  "RADIANS is the angle from x-axis to major-axis"
  (make-ellipse (make-segment-origin major-radius radians center) (* 2 minor-radius)))
;;(make-ellipse-origin 2 1)

(defmethod points ((x ellipse))
  (append (points (major-axis x)) (points (minor-axis x))))
