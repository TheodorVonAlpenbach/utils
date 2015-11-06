(in-package :topology)

(defgeneric format-object (geometry &optional gnuplot-id)
  (:documentation "Formats GEOMETRY for gnuplot scripts. 
Optional GNUPLOT-ID gives the ID tag for 'set object ID ...' statements"))

(defmethod format-object ((x point) &optional (n 1))
  (declare (ignore n))
  (apply #'format nil "set label at ~d,~d \"\" point pointtype 6 pointsize 2" (coordinates x)))

(defmethod format-object ((x multi-geometry) &optional (n 1))
  (concat (loop for o in (elements x)
		for i from n
		collect (format-object o i)) 
	  :in :newline))
;;(format-object (make-multi-geometry (list (make-segment '(1 2) '(1 4)) (make-segment '(2 2) '(2 4)))))

(defmethod format-object ((x segment) &optional (n 1))
  (apply #'format nil "set object ~d polygon from ~d,~d to ~d,~d fs empty border 1"
	 n (flatten* (coordinates x))))

(defun gp-point (coordinates)
  (concat coordinates :in ","))
;;(gp-point '(1 2))

(defmethod format-object ((x polygon) &optional (n 1))
  (concat (rcons (coordinates x) (first (coordinates x)))
	  :pre (format nil "set object ~d polygon from " n)
	  :in " to "
	  :suf " fs empty border 1"
	  :key #'gp-point))
;;(format-object (second egina::+egina-lc-triangles+))

(defun degrees->radians (x) (* 2 pi (/ x 360)))
(defun radians->degrees (x) (* 360 (/ x 2 pi)))
;;(radians->degrees (degrees->radians 45))

(defmethod format-object ((g ellipse) &optional (n 1))
  (destructuring-bind (x y) (coordinates (centre g))
    (format nil "set object ~d ellipse at ~a,~a size ~a,~a angle ~a"
      n x y (minor-diameter g) (diameter g) (radians->degrees (orientation g)))))
;;(format-object (make-ellipse (make-segment '(-2 -2) '(2 2)) 1))
;;(let ((*scale-offset* 0.5)) (plot (make-ellipse (make-segment '(-2 -2) '(2 2)) 3)))
;;(let ((*scale-offset* 0.5)) (plot (make-ellipse (make-segment '(-2 0) '(2 0)) (make-segment '(0 -1) '(0 1)))))
;;(let ((*scale-offset* 0.5)) (plot (make-ellipse-origo 2 1)))

(defun minmax (numbers)
  (list (reduce #'min numbers) (reduce #'max numbers)))

(defmethod bb ((x geometry))
  (mapcar #'minmax (transpose-tree (coordinates x))))

(defmethod bb ((p point))
  (destructuring-bind (x y) (coordinates p)
    (list (list x x) (list y y))))
;;(bb (make-point '(1 2)))

(defmethod bb ((x ellipse)) (bb (major-axis x)))
;;(bb (make-ellipse (make-segment '(0 -2) '(0 2)) 1))
;;(bb (make-ellipse (make-segment '(-2 0) '(2 0)) (make-segment '(0 -1) '(0 1))))

(defparameter *singularity-offset* 1)
(defparameter *scale-offset* 0.1)

(defun gp-minmax (a b &key (singularity-offset *singularity-offset*) (scale-offset *scale-offset*))
  (if (equal a b)
    (list (- a singularity-offset) (+ b singularity-offset))
    (let ((offset (* scale-offset (- b a))))
      (list (- a offset) (+ b offset)))))
;;(gp-minmax 1 10)

(defmethod gp-bb ((x geometry))
  (loop for (a b) in (bb x) collect (gp-minmax a b)))
;;(flatten* (gp-bb (make-point '(1 2))))

(defmethod plot ((x geometry))
  (gp:plot `(:v ,(format-object x)
		,(apply #'format nil "plot [~d:~d] [~d:~d] 0 lt bgnd"
			(flatten* (gp-bb x))))
	   :aspect-ratio :square))
;;(plot (make-multi-geometry (list (make-point '(1 2)) (make-point '(2 4)))))
;;(plot (make-segment '(1 2) '(2 4)))
;;(plot (make-multi-geometry egina::+egina-lc-triangles+))
