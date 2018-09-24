;;;; This modules helps you to calculate the geometry of GUI objects
;;;; The atoms are tagged rectangles, e.g. (:mytag envelope)

(cl-defun gui-align-no-stretch (objects &optional (start 0))
  "Each of OBJECTS is either a :fix or :space.
If length i nil, the :stretch elements are discarded."
  (loop for (tag length) in objects
	for left = start then right
	for right = (+ left length)
	if (eql tag :fix)
	collect (list tag (list left right)) into mid
	finally return (flank (list :start start) mid (list :end right))))
;;(gui-align-no-stretch '((:space 1) (:fix 2) (:space 2) (:fix 1) (:space 1)))

(defun gui-expand-stretch (objects stretch-sum unused-space)
  (loop for o in objects
	for (tag value) = o
	if (neql tag :stretch) collect o
	else collect (list :space (* unused-space (/ value 1.0 stretch-sum)))))
;;(gui-expand-stretch '((:space 1) (:fix 2) (:stretch 2) (:fix 1) (:stretch 1)) 3 6)

(defun gui-align (space objects)
  "Each of OBJECTS is either a :fix, :space, or :stretch.
If length i nil, the :stretch elements are discarded."
  (destructuring-bind (s l)
      (select objects (list (bind #'eql :stretch)) :key #'car)
    (let ((fixed-space (sum l :key #'second)))
      (when (> fixed-space space)
	(error "Not enough space for objects"))
      (let ((stretch-sum (sum s :key #'second)))
	(gui-align-no-stretch
	 (gui-expand-stretch objects stretch-sum (- space fixed-space)))))))
;;(gui-align 10 '((:space 1) (:fix 2) (:stretch 2) (:fix 1) (:stretch 1)))

(cl-defun gui-align-horizontally (objects)
  "Each of OBJECTS is either a :rectangle, :space or :stretch.
If length i nil, the :stretch elements are discarded."
  (destructuring-bind (s l)
      (select objects (list (bind #'eql :stretch)) :key #'car)
    (let ((lsum (sum l :key #'second)))
      (when (> lsum length)
	(error "Given objects are too wide for given length"))
      (if s
	(error "Not implemented")
	(error "Not implemented"))
      (list lsum))))
;;(gui-align-horizontally 10 '((:rectangle 1) (:stretch 1) (:space 1)))

(defun gui-stack-rectangles (orientation rectangles)
  (if (eql orientation :horizontal)
    (list (sum rectangles :key #'first)
	  (min-element (mapcar #'second rectangles) :test #'>))
    (list (min-element (mapcar #'first rectangles) :test #'>)
	  (sum rectangles :key #'second))))
;;(gui-rectangle (os-label-list "qwe" 100))

(cl-defun gui-rectangle (gui &optional (orientation :horizontal))
  (case (car gui)
    (:rectangle (second gui))
    (:space
     (if (eql orientation :horizontal)
       (list (second gui) 0) (list 0 (second gui))))
    (:stretch (list 0 0))
    (:gui-stack
	(destructuring-bind (tag orientation . sub-guis) gui
	  (gui-stack-rectangles orientation
	    (mapcar (bind #'gui-rectangle orientation) sub-guis))))))
;;(gui-rectangle (os-gui))
;;(gui-rectangle (list :space 10) :vertically)
;;(gui-rectangle (os-button "Qwe"))

(cl-defun gui-fill-stretch (gui env)
  "Expand :stretch to :space"
  (case (car gui)
    (:stretch (error ":stretch encountered. Should be handled with :gui-stack"))
    (:gui-stack
	(let* ((stretch-sum (sum (copy :stretch (rest (rest gui)) :key #'car)
				 :key #'second))
	       (orientation (second gui))
	       (gui-env-sans-stretch (gui-rectangle gui))
	       (horizontal-p (eql orientation :horizontal)))
	  (cons :gui-stack
		(cons (second gui)
		      (loop for sub-gui in (rest (rest gui))
			    if (eql (first sub-gui) :stretch)
			    collect (list :space
					  (if horizontal-p
					    (* (- (first env)
						  (first gui-env-sans-stretch))
					       (/ (* 1.0 (second sub-gui))
						  stretch-sum))
					    (* (- (second env)
						  (second gui-env-sans-stretch))
					       (/ (* 1.0 (second sub-gui))
						  stretch-sum))))
			    else collect (gui-fill-stretch
					  sub-gui
					  (if horizontal-p
					    (list (first (gui-rectangle sub-gui))
						  (second env))
					    (list (first env)
						  (second (gui-rectangle sub-gui))))))))))
    (otherwise gui)))
;;(gui-geometry (os-gui-test))

(cl-defun gui-absolute (gui &optional (position '(0 0)) orientation)
  (case (car gui)
    (:space (list nil position (if (eql orientation :horizontal)
				 (list (second gui) 0)
				 (list 0 (second gui)))))
    (:rectangle (list (third gui) position (second gui)))
    (:gui-stack (list :frame position
		      (loop with horizontal-p = (eql (second gui) :horizontal)
			    for sub-gui in (rest (rest gui))
			    for p = (if horizontal-p (first position) (second position))
			    then (+ p (if horizontal-p (first rect) (second rect)))
			    for rect = (gui-rectangle sub-gui (second gui))
			    collect (gui-absolute sub-gui
						  (if horizontal-p
						    (list p (second position))
						    (list (first position) p))
						  (second gui)))
		      (gui-rectangle gui)))
    (otherwise (error "Unknown tag '%S'" (car gui)))))
;;(gui-absolute (gui-fill-stretch (os-gui) (gui-rectangle (os-gui))))

(cl-defun gui-geometry (gui &optional (position '(0 0)))
  (gui-absolute (gui-fill-stretch gui (gui-rectangle gui)) position))
;;(gui-geometry (os-gui))

(defun gui-capitalize-first (string)
  (concat (upcase (substring string 0 1))
	  (substring string 1)))
;;(gui-capitalize-first "lstQwe")

;;;; Scilab stuff
(defun gui-extract-scilab-positions (gui)
  (if (eql (car gui) :frame)
    (loop for x in (third gui)
	  append (gui-extract-scilab-positions x))
    (if (stringp (car gui))
      (destructuring-bind (name (left top) (width height)) gui
	(list (format "%s = [%d, %d, %d, %d];"
		(concat "pos" (gui-capitalize-first name))
		(round left) (round top)
		(round width) (round height)))))))
;;(gui-extract-scilab-positions (gui-geometry (os-gui))) 

(cl-defun gui-scilab-positions (gui &optional
				      (frame-position '(0 0))
				      (frame-name "posFrame"))
  (concat* (append
	    (list (format "%s = [%d, %d, %d, %d];"
		    frame-name
		    (first frame-position) (second frame-position)
		    (first (last-elt gui)) (second (last-elt gui))))
	    (gui-extract-scilab-positions gui))
    :in "\n"))
;;(gui-scilab-positions (gui-geometry (os-gui))) 

(cl-defun gui-export-scilab-positions (gui filename &optional
						      (frame-position '(0 0))
						      (frame-prefix "posFrame"))
  (string-to-file (gui-scilab-positions gui frame-position frame-prefix)
		  filename))
;;(gui-export-scilab-positions (gui-geometry (os-gui)) "~/cvs/sources/SciLab/toolboxes/OsstrupenViewer/macros/OsCtrlPositions.sce" '(100 100)) 

(cl-indent 'gui-geometry 'prog1)
(cl-indent :gui-stack 'prog1)
;;(:frame name objects pos)
;;(type size name pos)
