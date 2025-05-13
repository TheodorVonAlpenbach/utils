;;;; This modules helps you to calculate the geometry of GUI objects
;;;; The atoms are
;;;; · rectangle (:rectangle size name), where size is a rectangle (x y),
;;;;   with x and y being non-negative numbers
;;;; · space (:space length), where length is a non-negative number
;;;; · stretch (:stretch weight), where weight is a non-negative number
;;;;
;;;; The atoms can be stacked with a composite gui object
;;;; · stack (:gui-stack orientation elements), where orientation is
;;;;   either :horizontal or :vertical, and elements are either a gui
;;;;   atom (see above), or another stack object.
;;;;
;;;; The stretch object is a flexible space that consumes so much
;;;; space that the encompassing stack object fills completely its
;;;; designated rectangle. The latter is calculated from the stack
;;;; object's encompassing stack object. For instance when two gui
;;;; objects are stacked horizontally the object with the widest
;;;; rectangle sets the overall rectangle size in the horizontal
;;;; orientation.
;;;;
;;;; Note that these definitions do not directly include any
;;;; positions. The positions of rectangles and spaces are indirectly
;;;; given by their stacking. If you want to put a rectangle inside a
;;;; stack object at a certain position, prepend it with a space
;;;; object.
;;;;
;;;; Example 1: An atom (:rectangle (1 2) "qwe") defines a rectangle
;;;; "qwe" with envelope ((0 0) (1 2)).
;;;;
;;;; Example 2: A simple stack column
;;;; 
;;;; (:gui-stack :vertical
;;;;   ((:space 1)
;;;;    (:rectangle (1 2) "qwe")))
;;;;
;;;; defines the same rectangle as in Example 1, but this time the
;;;; envelope is ((0 1) (1 3)). The space object as "moved" the
;;;; rectangle one unit in the :vertical orientation.
;;;; 
;;;; Example 3: Two rectangles 
;;;;
;;;;(:gui-stack :horizontal
;;;;  ((:gui-stack :vertical
;;;;     ((:rectangle "qwe" (1 2)) (:space 2)))
;;;;   (:gui-stack :vertical
;;;;     ((:stretch) (:rectangle "qwe" (1 2))))))
;;;;
;;;; The first column has a rectangle at ((0 0) (1 2)), but because of
;;;; the subsequent space object the column envelope is ((0 0) (1 4)),
;;;; so the column height is four units. The second column has a
;;;; rectangle of height two units. Since the two columns must align
;;;; in the :vertical orientation, the stretch object with expand to a
;;;; space object of two units, so that the height in this column
;;;; becomes four units as well. Hence, the second rectangle will have
;;;; the envelope ((0 2) (1 4)).
;;;;
;;;; Example 4: Stretch weight. You can provide a stretch object with
;;;; a weight parameter. If there is only one stretch object in a
;;;; stack object, then this weight has no impact, as in the previous
;;;; example. But with two or more stretch objects you can specify the
;;;; weight of each of them with this weight parameter:
;;;;
;;;;(:gui-stack :horizontal
;;;;  ((:gui-stack :vertical
;;;;     ((:rectangle "qwe" (1 2)) (:space 2)))
;;;;   (:gui-stack :vertical
;;;;     ((:stretch 3) (:rectangle "qwe" (1 2)) (:stretch 1)))))
;;;;
;;;; Here the second rectangle lies between two stretch objects, the
;;;; first being three times greater than the second. This means that
;;;; the first stretch object should expand to a space objects that is
;;;; three times longer than the second. As in the previous example,
;;;; the second column needs an additional space of two units to align
;;;; with the first column, so the two stretches must expand to two
;;;; units in total. Denoting the resulting spaces s1 and s2, we get
;;;; the equations
;;;;
;;;; s1 = 3 * s2 
;;;; s1 + s2 = 2
;;;;
;;;; Solving the equations, we get s1 = 1.5, s2 = 0.5, and the second
;;;; column becomes equivalent to
;;;;
;;;; (:gui-stack :vertical
;;;;   ((:space 1.5) (:rectangle "qwe" (1 2)) (:space 0.5)))
;;;;
;;;; So the second rectangle gets the envelope ((0 1.5) (1 3.5)).
      
(cl-defun gui-stack-rectangles (orientation rectangles)
  "Calculate the size of the minimum rectangle that contains RECTANGLES.
The RECTANGLES are stacked one after another along a line with
ORIENTATION, which is either :horizontal or :vertical. For
instance the result of stacking two rectangles (2 3) and (1 1)
is ((+ 2 1) (max 3 1)) = (3 3) along the :horizontal orientation,
and ((max 2 1) (3 + 1)) = (2 4) along the :vertical dimension."
  (if (eql orientation :horizontal)
    (list (sum rectangles :key #'first)
	  (min-element (mapcar #'second rectangles) :test #'>))
    (list (min-element (mapcar #'first rectangles) :test #'>)
	  (sum rectangles :key #'second))))
;;(gui-rectangle (os-label-list "qwe" 100))

(cl-defun gui-rectangle (gui &optional (orientation :horizontal))
  "Calculate the minimum rectangle that contains GUI."
  (cl-case (car gui)
    (:rectangle (second gui))
    (:space
     (if (eql orientation :horizontal)
       (list (second gui) 0) (list 0 (second gui))))
    (:stretch (list 0 0))
    (:gui-stack
	(cl-destructuring-bind (tag orientation . sub-guis) gui
	  (gui-stack-rectangles orientation
	    (mapcar (bind #'gui-rectangle orientation) sub-guis))))))
;;(gui-rectangle (os-gui))
;;(gui-rectangle (list :space 10) :vertically)
;;(gui-rectangle (os-button "Qwe"))

(cl-defun gui-fill-stretch (gui env)
  "Expand :stretch objects to :space objects."
  (cl-case (car gui)
    ;; Should be handled with :gui-stack
    (:stretch (error "Unexpected :stretch object encountered."))
    (:gui-stack
	(let* ((stretch-sum (sum (copy :stretch (rest (rest gui)) :key #'car)
				 :key #'second))
	       (orientation (second gui))
	       (gui-env-sans-stretch (gui-rectangle gui))
	       (horizontal-p (eql orientation :horizontal)))
	  (cons :gui-stack
		(cons (second gui)
		      (cl-loop for sub-gui in (rest (rest gui))
			    if (eql (first sub-gui) :stretch)
			    collect (list :space
					  (if horizontal-p
					    (* (- (first env)
						  (first gui-env-sans-stretch))
					       (/ (float (or (second sub-gui) 1))
						  stretch-sum))
					    (* (- (second env)
						  (second gui-env-sans-stretch))
					       (/ (float (or (second sub-gui) 1))
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
  "Calculate the absolute positions of the objects in GUI.
See `gui-geometry' for a description of the result's format."
  (cl-case (car gui)
    (:space (list nil position (if (eql orientation :horizontal)
				 (list (second gui) 0)
				 (list 0 (second gui)))))
    (:rectangle (list (third gui) position (second gui)))
    (:gui-stack (list :frame position
		      (cl-loop with horizontal-p = (eql (second gui) :horizontal)
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
  "Calculate absolute envelopes in GUI.
The resulting format is the same as in GUI except that

· (:gui-stack ORIENTATION ELEMENTS) objects are converted
  to (:frame ELEMENTS POSITION)

· (:space LENGTH) objects are converted to the format (nil SIZE
  POSITION)

· (:stretch WEIGHT) objects are first converted to (:space
  LENGTH) objects and then converted as described above to the
  format (nil SIZE POSITION)

· (:rectangle size name) objects are converted to the
  format (name SIZE POSITION)

In all cases the POSITION parameter is the calculated
absolute (LEFT BOTTOM) position of the object, and the SIZE
parameter is the size of the object's encompassing rectangle."
  (gui-absolute (gui-fill-stretch gui (gui-rectangle gui)) position))
;;(gui-geometry (os-gui))

;; Emacs indent definitions
(cl-indent 'gui-geometry 'prog1)
(cl-indent :gui-stack 'prog1)

(provide 'gui)
