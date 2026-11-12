;;; kommuner.el --- Experimental municipality geometry -*- lexical-binding: t; -*-

;;; Commentary:

;; An unfinished exploration of Norwegian municipality boundary geometry.
;; The source data path is machine-specific, and the polygon operations remain
;; unimplemented.  This file is retained as an experiment rather than as part
;; of the maintained quiz application.

;;; Code:

(require 'cl-lib)
(require 'mb-lists)
(require 'mb-utils-geometry)
(require 'mb-utils-io)

(cl-defun nk-parse-text (&optional (filename "~/projects/qp/kommuner/data.txt"))
  (cl-loop for line in (butlast (cdr (file-lines filename)))
	for (sknr navn raw-path _slat _slon) = (split-string line "\t")
	for spath = (split-string raw-path)
	for path = (cl-loop for spoint in spath
			 for xyz = (mapcar #'string-to-number
				   (split-string spoint ","))
			 if (and (cl-first xyz) (cl-second xyz))
			 ;; discard z coordinate
			 collect (butlast xyz))
	if (and sknr navn path)
	collect (list navn (string-to-number sknr) (bounding-box path) path)))
;;(first (nk-parse-text))
;;(last (butlast (file-lines "~/projects/qp/kommuner/data.txt")) 1)

(defvar kommuner--cache nil)

(cl-defun kommuner (&optional refresh)
  (when (or refresh (null kommuner--cache))
    (setf kommuner--cache (nk-parse-text)))
  kommuner--cache)
;;(kommuner t)

(defalias 'nk-bb #'cl-third)
(defalias 'nk-name #'cl-first)

(cl-defun nk-kommune (name)
  (cl-find name (kommuner) :test #'string= :key #'nk-name))
;;(nk-kommune "Oslo")

(cl-defun iv-less-than-p (iv1 iv2)
  "Return non nil iff IV1's upper limit is strictly less than IV2's lower limit."
  (< (cl-second iv1) (cl-first iv2)))
;;(iv-less-than-p '(0 2) '(2 3))

(cl-defun iv-overlap-p (iv1 iv2)
  (not (or (iv-less-than-p iv1 iv2)
           (iv-less-than-p iv2 iv1))))
;;(iv-overlap-p '(0 2) '(1 3))

(cl-defun bb-overlap-p (bb1 bb2)
  (and (iv-overlap-p (cl-first bb1) (cl-first bb2))
       (iv-overlap-p (cl-second bb1) (cl-second bb2))))
;;(bb-overlap-p '((0 2) (0 2)) '((1 3) (1 3)))

(cl-defun nk-overlap-p (k1 k2)
  "Return non nil iff kommunene K1 and K2 overlap.
First naive version. BB criterion"
  (bb-overlap-p (nk-bb k1) (nk-bb k2)))
;;(nk-overlap-p (nk-kommune "Oslo") (nk-kommune "Nesodden"))

(cl-defun nk-overlapping (kommune)
  (cl-loop for k in (kommuner)
	if (nk-overlap-p k kommune)
	collect k))
;;(mapcar #'nk-name (nk-overlapping (nk-kommune "Oslo")))

(cl-defun cpolygon-overlap-p (_cp1 _cp2)
  (error "Complex polygon overlap is not implemented"))

(cl-defun polygon-overlap-p (_cp1 _cp2)
  (error "Polygon overlap is not implemented"))

(cl-defun cpolygon-distance ()
  (error "Complex polygon distance is not implemented"))

(cl-defun polygon-distance ()
  (error "Polygon distance is not implemented"))

(cl-defun ring< (x y &rest numbers)
  (or (null numbers)
      (let* ((list (cons x (cons y numbers)))
	     (pos (cl-loop for i from 0
		   for (a b) in (pairs list)
		   if (not (< a b)) return (1+ i))))
	(and pos
	     (< (nth pos list) x)
	     (< (car (last list)) x)
	     (apply #'< (cl-subseq list pos))))))
;;(ring< 1 2 3 4 5 0 .1)

(cl-defun nk-path-clockwise-p (kommune)
  (cl-destructuring-bind (((_xmin p1) (_xmax p3))
			((_ymin p4) (_ymax p2)))
      (nk-bb kommune)
    (ring< p1 p2 p3 p4)))
;;(mapcar #'nk-path-clockwise-p (cl-subseq kommuner--cache 0 10))

(cl-defun nk-path-orientation (kommune)
  (if (nk-path-clockwise-p kommune)
    :clockwise :anti-clockwise))
;;(mapcar #'nk-path-orientation (cl-subseq kommuner--cache 0 10))

(provide 'kommuner)

;;; kommuner.el ends here
