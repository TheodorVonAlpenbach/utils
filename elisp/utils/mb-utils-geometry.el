;;; mb-utils-geometry.el --- Small geometry and layout helpers -*- lexical-binding: t; -*-

(require 'cl-lib)

;;; Code:

(defun mb-geometry--check-real (value name)
  "Signal an error unless VALUE is a real number named NAME."
  (unless (or (integerp value) (floatp value))
    (error "%s must be a real number: %S" name value)))

(defun mb-geometry--check-nonnegative (value name)
  "Signal an error unless VALUE is a nonnegative real number named NAME."
  (mb-geometry--check-real value name)
  (when (cl-minusp value)
    (error "%s must be non-negative: %S" name value)))

(defun mb-geometry--check-1d (object)
  "Signal an error unless OBJECT is a valid one-dimensional object."
  (unless (and (consp object) (consp (cdr object)) (null (cddr object)))
    (error "Not a one-dimensional object: %S" object))
  (mb-geometry--check-real (car object) "Object start")
  (mb-geometry--check-nonnegative (cadr object) "Object length")
  object)

(cl-defun rest-space (n space &optional (world 1024))
  "Return WORLD minus N objects' N+1 margins of size SPACE."
  (unless (and (integerp n) (>= n 0))
    (error "Object count must be a non-negative integer: %S" n))
  (mb-geometry--check-nonnegative space "Margin")
  (mb-geometry--check-nonnegative world "World length")
  (- world (* (1+ n) space)))

(cl-defun make-1d (start length)
  "Return a one-dimensional object beginning at START with LENGTH."
  (mb-geometry--check-real start "Object start")
  (mb-geometry--check-nonnegative length "Object length")
  (list start length))

(defsubst 1d-start (object)
  "Return the start coordinate of one-dimensional OBJECT."
  (cl-first (mb-geometry--check-1d object)))

(defsubst 1d-length (object)
  "Return the length of one-dimensional OBJECT."
  (cl-second (mb-geometry--check-1d object)))

(cl-defun 1d-align (objects &key left-space right-space (world 1024))
  "Distribute OBJECTS across WORLD while preserving their lengths.
When LEFT-SPACE or RIGHT-SPACE is nil, distribute the corresponding
margin together with the unspecified internal spaces."
  (mb-geometry--check-nonnegative world "World length")
  (when left-space
    (mb-geometry--check-nonnegative left-space "Left margin"))
  (when right-space
    (mb-geometry--check-nonnegative right-space "Right margin"))
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

(cl-defun 1d-interspaces (n space &optional (world 1024))
  "Return N equal objects separated and surrounded by SPACE in WORLD."
  (unless (and (integerp n) (cl-plusp n))
    (error "Object count must be a positive integer: %S" n))
  (mb-geometry--check-nonnegative space "Margin")
  (mb-geometry--check-nonnegative world "World length")
  (let ((available (rest-space n space world)))
    (when (cl-minusp available)
      (error "Spaces exceed world length"))
    (let ((length (/ available (float n))))
      (cl-loop for index below n
	       for start = space then (+ start space length)
	       collect (make-1d start length)))))

(cl-defun 1d-transform (object offset scale)
  "Scale one-dimensional OBJECT by SCALE, then move it by OFFSET."
  (mb-geometry--check-1d object)
  (mb-geometry--check-real offset "Offset")
  (mb-geometry--check-nonnegative scale "Scale")
  (make-1d (+ offset (* scale (1d-start object)))
	   (* scale (1d-length object))))

(cl-defun 1d-move (object offset)
  "Return one-dimensional OBJECT moved by OFFSET."
  (1d-transform object offset 1))

(cl-defun minmax (numbers)
  "Return the minimum and maximum values in NUMBERS."
  (unless numbers
    (error "Cannot find extrema of an empty sequence"))
  (dolist (number numbers)
    (mb-geometry--check-real number "Sequence element"))
  (list (cl-reduce #'min numbers)
	(cl-reduce #'max numbers)))

(cl-defun bounding-box (points)
  "Return axis-aligned bounding intervals for two-dimensional POINTS."
  (unless points
    (error "Cannot bound an empty point set"))
  (dolist (point points)
    (unless (= (length point) 2)
      (error "Not a two-dimensional point: %S" point))
    (dolist (coordinate point)
      (mb-geometry--check-real coordinate "Point coordinate")))
  (list (minmax (mapcar #'cl-first points))
	(minmax (mapcar #'cl-second points))))

(provide 'mb-utils-geometry)

;;; mb-utils-geometry.el ends here
