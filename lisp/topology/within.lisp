(in-package :topology)
;;;; This package contains the methods WITHIN and INSIDE.
;;;; (WITHIN X Y) simply means that X is an element or a subset of Y.
;;;; (INSIDE X Y) == (and (WITHIN X Y) (not (WITHIN X (BOUNDARY Y)))),
;;;; i.e. X must be stricly inside Y.

;;; INSIDE
(defmethod inside ((x point) (y triangle))
  (warn "Not checked")
  ;; (every #'plusp (boundary y) (bind #'cross-product x))
  (every #'plusp (mapcar (bind #'cross-product x) (boundary y))))
;;(within (make-point '(1/2 1/2)) (make-triangle '(0 0) '(1 0) '(0 1)))

;;; WITHIN
(defmethod within ((x number) (y cons))
  (destructuring-bind (a b) y (or (<= a x b) (<= b x a))))
;;(within -1 '(2 0))

(defmethod within ((x point) (y triangle))
  (notany #'minusp (mapcar (bind #'cross-product x) (segments (boundary y)))))
;;(within (make-point '(1/2 1/2)) (make-triangle '(0 0) '(1 0) '(0 1)))

(defmethod within ((x geometry) (y geometry))) ;;default nil

(defmethod within ((x point) (y segment))
  (destructuring-bind (xis ais bis)
      (mapcar #'coordinates (list x (start y) (end y)))
    (loop for ai across ais
	  for bi across bis
	  for i from 0
	  when (/= ai bi) return
	  (when (within (aref xis i) (list ai bi))
	    (let ((s (/ (- (aref xis i) bi) (- ai bi))))
	      (every #'(lambda (x a b) (= (- x b) (* s (- a b)))) xis ais bis))))))
;;(within (make-point '(-1 -1)) (make-segment '(0 0) '(1 1)))

(defun relative-distance-from-ellipse-center2 (p e)
  (norm2 (g/ (g- (rotate-around p (orientation e) (centre e)) (centre e)) (radii e))))

(defun relative-distance-from-ellipse-center (p e)
  (sqrt (relative-distance-from-ellipse-center2 p e)))
;;(relative-distance-from-ellipse-center (make-point '(0.5 0)) (make-ellipse (make-segment '(-1 0) '(1 0)) 1))

(defmethod within ((p point) (e ellipse))
  (<= (relative-distance-from-ellipse-center2 p e) 1))

(defmethod inside ((p point) (e ellipse))
  (< (relative-distance-from-ellipse-center2 p e) 1))
;;(within (make-point '(10 0)) (make-ellipse (make-segment '(-1 0) '(1 0)) 1))
;;(g/ (make-point '(0 0)) '(1 1/2))

(defgeneric gequal (x y))

(defmethod gequal ((x point) (y point))
  (every #'equal (coordinates x) (coordinates y)))
;;(gequal (make-point '(1 2)) (make-point '(1 2)))

(defmethod gequal ((x segment) (y segment))
  (or (and (gequal (start x) (start y))
	   (gequal (end x) (end y)))
      (and (gequal (start x) (end y))
	   (gequal (end x) (start y)))))
;;(gequal (make-segment '(1 0) '(0 0)) (make-segment '(0 0) '(1 0)))
