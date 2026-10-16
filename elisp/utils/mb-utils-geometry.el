;;; mb-utils-geometry.el --- Small geometry and layout helpers -*- lexical-binding: t; -*-

(require 'cl-lib)

;;; Code:

(cl-defun rest-space (n space &optional (world 1024))
  "Return WORLD minus N objects' N+1 margins of size SPACE."
  (unless (and (integerp n) (>= n 0))
    (error "Object count must be a non-negative integer: %S" n))
  (- world (* (1+ n) space)))
;;(rest-space 3 10)

(cl-defun make-1d (start length)
  "Return a one-dimensional object beginning at START with LENGTH."
  (list start length))

(defsubst 1d-start (object)
  "Return the start coordinate of one-dimensional OBJECT."
  (cl-first object))

(defsubst 1d-length (object)
  "Return the length of one-dimensional OBJECT."
  (cl-second object))

(cl-defun 1d-align (objects &key left-space right-space (world 1024))
  "Distribute OBJECTS across WORLD while preserving their lengths.
When LEFT-SPACE or RIGHT-SPACE is nil, distribute the corresponding
margin together with the unspecified internal spaces."
  (if (null objects)
      nil
    (let* ((count (length objects))
	   (occupied (cl-reduce #'+ objects :key #'1d-length
				:initial-value 0))
	   (remaining (- world occupied))
	   (fixed-left (or left-space 0))
	   (fixed-right (or right-space 0))
	   (flexible-gaps (+ (1- count)
			     (if left-space 0 1)
			     (if right-space 0 1)))
	   (flexible-space (- remaining fixed-left fixed-right)))
      (when (cl-minusp flexible-space)
	(error "Objects and fixed margins exceed world length"))
      (when (and (zerop flexible-gaps) (not (zerop flexible-space)))
	(error "Specified margins do not fill world length"))
      (let ((gap (if (zerop flexible-gaps)
		     0
		   (/ flexible-space (float flexible-gaps))))
	    (start (or left-space nil)))
	(unless start
	  (setq start gap))
	(cl-loop for object in objects
		 for length = (1d-length object)
		 collect (make-1d start length)
		 do (setq start (+ start length gap)))))))
;;(1d-align '((0 10) (0 10)))

(cl-defun 1d-interspaces (n space &optional (world 1024))
  "Return N equal objects separated and surrounded by SPACE in WORLD."
  (unless (and (integerp n) (cl-plusp n))
    (error "Object count must be a positive integer: %S" n))
  (let ((available (rest-space n space world)))
    (when (cl-minusp available)
      (error "Spaces exceed world length"))
    (let ((length (/ available (float n))))
      (cl-loop for index below n
	       for start = space then (+ start space length)
	       collect (make-1d start length)))))

(cl-defun 1d-transform (object offset scale)
  "Scale one-dimensional OBJECT by SCALE, then move it by OFFSET."
  (when (cl-minusp scale)
    (error "Scale must be non-negative: %S" scale))
  (make-1d (+ offset (* scale (1d-start object)))
	   (* scale (1d-length object))))

(cl-defun 1d-move (object offset)
  "Return one-dimensional OBJECT moved by OFFSET."
  (1d-transform object offset 1))

(cl-defun minmax (numbers)
  "Return the minimum and maximum values in NUMBERS."
  (unless numbers
    (error "Cannot find extrema of an empty sequence"))
  (list (cl-reduce #'min numbers)
	(cl-reduce #'max numbers)))
;;(minmax '(1 3 2 4 0))

(cl-defun bounding-box (points)
  "Return axis-aligned bounding intervals for two-dimensional POINTS."
  (unless points
    (error "Cannot bound an empty point set"))
  (dolist (point points)
    (unless (= (length point) 2)
      (error "Not a two-dimensional point: %S" point)))
  (list (minmax (mapcar #'cl-first points))
	(minmax (mapcar #'cl-second points))))
;;(bounding-box '((0 0) (-1 3) (1 2)))

(provide 'mb-utils-geometry)

;;; mb-utils-geometry.el ends here
