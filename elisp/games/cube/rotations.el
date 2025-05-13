;;; Side colors are numbered 0..11 starting from lower left. Faces are
;;; numbered 0..8, also starting from lower left:
;;;
;;;     8  7  6
;;;   9|6‾‾5‾‾4|5
;;;  10|7     3|4
;;;  11|0__1__2|3
;;;     0  1  2

(cl-defun perm-swap (x &rest swaps)
  "Swap the Ith and Jth elements in X, where each SWAP = (I J)" 
  (cl-loop for (i j) in swaps
	do (rotatef (nth i x) (nth j x)))
  x)
;;(perm-swap (0-n 12) '(2 4) '(2 4))

(cl-defun perm-rot3 (x rot3)
  "Rotate the Ith, Jth, Kth elements in X, where rot3 = (I J K)" 
  (cl-destructuring-bind (i j k) rot3
    (rotatef (nth i x) (nth j x) (nth k x))
    x))
;;(perm-rot3 (0-n 12) '(4 7 10))

(cl-defun perm-inverse (x)
  (project x x))
;;(perm-inverse '(0 2 1))

(cl-defun perm-reflect (x)
  (perm-swap x '(0 2) '(3 11) '(4 10) '(5 9) '(6 8)))
;;(perm-reflect (0-n 12))

(defmacro with-reflection (x perm)
  `(perm-reflect (,perm (perm-reflect ,x))))
;;(with-reflection (0-n 12) pll-Nb)

(cl-defun perm-U (x n) (rotate-list x (* 3 n)))
;;(perm-U (0-n 12) 1)

(cl-defun perm-M (x) (perm-swap x '(1 7)))
(cl-defun perm-S (x) (perm-swap x '(4 10)))
(cl-defun perm-F (x) (perm-swap x '(0 3) '(11 2)))
(cl-defun perm-B (x) (perm-swap x '(5 8) '(6 9)))

(cl-defun perm-NE (x) (perm-swap x '(0 6) '(5 11)))
(cl-defun perm-NW (x) (perm-U (perm-NE (perm-U x 1)) -1))
;;(perm-NW (0-n 12))

(cl-defun pll-Aa (&optional (x (0-n 12)))
  (perm-rot3 (perm-rot3 x '(2 5 8)) '(3 6 9)))
(cl-defun pll-Ab (&optional (x (0-n 12))) (perm-inverse (pll-Aa x)))
;;(pll-Ab)

(cl-defun pll-E (&optional (x (0-n 12))) (perm-B (perm-F x)))
(cl-defun pll-F (&optional (x (0-n 12))) (perm-B (perm-S x)))
;;(pll-F)

(cl-defun pll-Nb (&optional (x (0-n 12))) (perm-M (perm-NE x)))
(cl-defun pll-Na (&optional (x (0-n 12))) (with-reflection x pll-Nb))
;;(pll-Nb)

(cl-defun pll-Ub (&optional (x (0-n 12))) (perm-rot3 x '(4 7 10)))
(cl-defun pll-Ua (&optional (x (0-n 12))) (with-reflection x pll-Ub))
;;(pll-Ua)

;; ... and so on

(cl-defun edge2face (edge)
  (aref #[0 1 2 2 3 4 4 5 6 6 7 0] edge))
;;(mapcar #'edge2face (0-n 12))

(cl-defun set-equal-p (numbers1 numbers2)
  "Not in use here anymore."
  (equal (sort* (copy-list numbers1) #'<) (sort* (copy-list numbers2) #'<)))
;;(set-equal-p '(0 2) '(2 0))

(cl-defun perm2face-segments-raw (perm)
  "Return a list consisting of all 2-rotations in PERM."
  (remove-duplicates
      (cl-loop for p in perm for i from 0 if (/= p i)
	    collect (maptree #'edge2face (list i p)))
    :test #'equal))
;;(perm2face-segments-raw (pll-E))

(cl-defun perm2face-segments (perm)
  "Return two lists consisting of ONE-WAY and TWO-WAY segments, respectively."
  (let ((p (partition (perm2face-segments-raw perm)
		      :key #'(lambda (x) (cl-sort (copy-list x) #'<))
		      :test #'equal)))
    (cl-loop for x in p
	  if (= (length x) 1) collect (first x) into one-way
	  if (= (length x) 2) collect (first x) into two-way
	  finally return (list one-way two-way))))
;;(perm2face-segments (pll-E))
;;(perm2face-segments (pll-Aa))

(partition '((2 0) (0 2)) :key #'(lambda (x) (sort* (copy x) #'<)))


(cl-defun tikz-arrow-points (perm)
  )
