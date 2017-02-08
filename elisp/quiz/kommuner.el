(cl-defun nk-parse-text (&optional (filename "~/projects/qp/kommuner/data.txt"))
  (loop for line in (butlast (rest (file-lines filename)))
	for (sknr navn spath slat slon) = (split-string line "\t")
	for spath = (split-string spath)
	for path = (loop for spoint in spath
			 for xyz = (mapcar #'string-to-number
				   (split-string spoint ","))
			 if (and (first xyz) (second xyz))
			 ;; discard z coordinate
			 collect (butlast xyz))
	if (and sknr navn path)
	collect (list navn (string-to-number sknr) (bounding-box path) path)))
;;(first (nk-parse-text))
;;(last (butlast (file-lines "~/projects/qp/kommuner/data.txt")) 1)

(defconst +kommuner+ nil)

(defun kommuner (&optional refresh)
  (when (or refresh (null +kommuner+))
    (setf +kommuner+ (nk-parse-text)))
  +kommuner+)
;;(kommuner t)

(defalias 'nk-bb #'third)
(defalias 'nk-name #'first)

(defun nk-kommune (name)
  (find name (kommuner) :test #'string= :key #'nk-name))
;;(nk-kommune "Oslo")

(defun iv-less-than-p (iv1 iv2)
  "Return non nil iff IV1's upper limit is strictly less than IV2's lower limit."
  (< (second iv1) (first iv2)))
;;(iv-less-than-p '(0 2) '(2 3))

(defun iv-overlap-p (iv1 iv2)
  (nor (iv-less-than-p iv1 iv2)
       (iv-less-than-p iv2 iv1)))
;;(iv-overlap-p '(0 2) '(1 3))

(defun bb-overlap-p (bb1 bb2)
  (and (iv-overlap-p (first bb1) (first bb2))
       (iv-overlap-p (second bb1) (second bb2))))
;;(bb-overlap-p '((0 2) (0 2)) '((1 3) (1 3)))

(defun nk-overlap-p (k1 k2)
  "Return non nil iff kommunene K1 and K2 overlap.
First naive version. BB criterion"
  (bb-overlap-p (nk-bb k1) (nk-bb k2)))
;;(nk-overlap-p (nk-kommune "Oslo") (nk-kommune "Nesodden"))

(defun nk-overlapping (kommune)
  (loop for k in (kommuner)
	if (nk-overlap-p k kommune)
	collect k))
;;(mapcar #'nk-name (nk-overlapping (nk-kommune "Oslo")))

(defun cpolygon-overlap-p (cp1 cp2))
(defun polygon-overlap-p (cp1 cp2))
(defun cpolygon-distance)
(defun polygon-distance)

(defun ring< (x y &rest numbers)
  (or (null numbers)
      (let* ((list (cons x (cons y numbers)))
	     (pos (loop for i from 0
		   for (a b) in (pairs list)
		   if (not (< a b)) return (1+ i))))
	(and pos
	     (< (nth pos list) x)
	     (< (last-elt list) x)
	     (apply #'< (subseq list pos)))))))
;;(ring< 1 2 3 4 5 0 .1)

(defun nk-path-clockwise-p (kommune)
  (destructuring-bind (((xmin p1) (xmax p3))
			((ymin p4) (ymax p2)))
      (nk-bb kommune)
    (ring< p1 p2 p3 p4)))
;;(mapcar #'nk-path-clockwise-p (subseq +kommuner+ 0 10))

(defun nk-path-orientation (kommune)
  (if (nk-path-clockwise-p kommune)
    :clockwise :anti-clockwise))
;;(mapcar #'nk-path-orientation (subseq +kommuner+ 0 10))
