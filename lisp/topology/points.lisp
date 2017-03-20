(in-package :topology)

(defmethod points ((x point))
  (list x))

(defmethod points ((x segment))
  (list (start x) (end x)))
;;(points (make-segment '(1 2) '(2 4)))

(defmethod points ((x path))
  (let ((segments (segments x)))
    (cons (start (elt segments 0))
	  (loop for s across segments collect (end s)))))
;;(points (make-path '((0 0) (1 0) (1 1) (0 1))))

(defmethod points ((x polygon))
  (butlast (points (boundary x))))
;;(points (make-polygon '((0 0) (1 0) (0 1))))
;; This must be generic because of (coordinates point)

(defmethod points ((x ellipse))
  "What's the use of this?"
  (append (points (major-axis x)) (points (minor-axis x))))

(defmethod points ((bb box))
  (list (make-point (list (start (x-range bb))
			  (start (y-range bb))))
	(make-point (list (end (x-range bb))
			  (start (y-range bb))))
	(make-point (list (end (x-range bb))
			  (end (y-range bb))))
	(make-point (list (start (x-range bb))
			  (end (y-range bb))))))
;;(points (mbb 0 1 2 3))

(defmethod points ((x multi-geometry))
  (mapcan #'points (elements x)))

;;(points (make-multi-geometry (list (make-point '(1 2)) (make-point '(2 4)))))
;; Finally, we can easily provide a shortcut for coordinates for every
;; geometry that we have defined POINT for:
(defmethod coordinates ((x geometry))
  (mapcar #'coordinates (points x)))
