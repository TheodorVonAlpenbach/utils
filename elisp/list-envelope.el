;;;; Envelope describes a rectangle as a pair of two list-intervals. 

(require 'list-interval)

(defun e-make-envelope (ix iy)
  (list ix iy))

(defun e-envelope-p (envelope)
  (and envelope
       (consp envelope)
       (= (length envelope) 2)
       (i-interval-p (first envelope))
       (i-interval-p (second envelope))
       envelope))

(defun e-contain-p (envelope x &optional strictly-p) 
  "X may be envelope or 2d point"
  (and (i-contain-p (first envelope) (first x) strictly-p)
       (i-contain-p (second envelope) (second x) strictly-p)))

(defun e-within-p (x envelope &optional strictly-p) 
  (e-contain-p envelope x strictly-p))

(defun e-touch-p (envelope1 envelope2)
  (destructuring-bind ((ix1 iy1) (ix2 iy2)) (list envelope1 envelope2)
    (or (and (i-touch-p ix1 ix2)
	     (i-overlap-p iy1 iy2))
	(and (i-touch-p iy1 iy2)
	     (i-overlap-p ix1 ix2)))))

(defun e-overlap-p (envelope1 envelope2 &optional strictly-p)
  (and (i-overlap-p (first envelope1) (first envelope2))
       (i-overlap-p (second envelope1) (second envelope2))
       (nand strictly-p (e-touch-p envelope1 envelope2))))
;;(e-overlap-p '((0 3) (0 3)) '((2 4) (1 3)) t)
;;(e-touch-p '((0 3) (0 3)) '((2 4) (1 3)) )

(defun e-disjoint-p (envelope1 envelope2 &optional allow-touch-p) 
  (or (not (e-overlap-p envelope1 envelope2))
      (and allow-touch-p
	   (e-touch-p envelope1 envelope2))))

(defun e-intersection (envelope1 envelope2)
  (e-make-envelope
   (i-intersection (first envelope1) (first envelope2))
   (i-intersection (second envelope1) (second envelope2))))

(defun e-union (envelope1 envelope2)
  (and (e-envelope-p envelope1)
       (e-envelope-p envelope2)
       (e-make-envelope
	(i-union (first envelope1) (first envelope2))
	(i-union (second envelope1) (second envelope2)))))

(defun e-stack-envelopes (orientation envelopes)
  envelopes)
;;(e-stack-envelopes)
(cl-indent 'e-stack-envelopes 'prog1)

(provide 'list-envelope)
