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

(defclass polyline (geometry)
  ((segments :initarg :segments :accessor segments :type list)))

(defmethod make-polyline (points)
  (make-instance 'polyline
    :segments (loop for (start end) in (pairs points)
		    collect (make-segment start end))))
;;(trace make-polyline)

(defmethod points ((x polyline))
  (let ((segments (segments x)))
    (cons (start (first segments)) (mapcar #'end segments))))
;;(points (make-polyline '((0 0) (1 0) (1 1) (0 1))))

(defmethod print-object ((x polyline) stream)
  (print-unreadable-object (x stream :type t)
    (princ (mapcar #'coordinates (points x)) stream)))
;;(make-polyline '((0 0) (1 0) (1 1) (0 1)))

(defclass polygon (geometry)
  ((boundary :initarg :boundary :accessor boundary :type polyline)))

(defmethod make-polygon (points)
  (make-instance 'polygon :boundary (make-polyline (rcons points (first points)))))
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
  (make-instance 'triangle :boundary (make-polyline (list x1 x2 x3 x1))))
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

(defmethod rotate ((x point) (y number))
  (make-point (rotate (coordinates x) y)))
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

(defmethod radii ((x ellipse)) (list (major-radius x) (minor-radius x)))
;;(radii (make-ellipse (make-segment '(0 -2) '(0 2)) 2))

(defun origo () (make-point '(0 0)))

(defmethod make-segment-origo ((r number) &optional orientation center)
  "ORIENTATION is the angle from x-axis to major-axis. Default
ORIENTATION is 0. Default CENTER is (ORIGO)"
  (let ((s (make-segment (list (- r) 0) (list r 0))))
    (when orientation (setf s (rotate s orientation)))
    (when center (g+ s center))
    s))
;;(make-segment-origo 1 (/ pi 2) '(1 1))

(defmethod make-ellipse-origo (major-radius minor-radius &optional orientation center)
  "ORIENTATION is the angle from x-axis to major-axis"
  (make-ellipse (make-segment-origo major-radius orientation center) (* 2 minor-radius)))
;;(make-ellipse-origo 2 1)

(defmethod points ((x ellipse))
  (append (points (major-axis x)) (points (minor-axis x))))