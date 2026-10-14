;;; list-envelope.el --- Rectangles made from list intervals -*- lexical-binding: t; -*-

;;;; An envelope describes a rectangle as a pair of list intervals.

(require 'cl-lib)
(require 'list-interval)

(cl-defun e-make-envelope (ix iy)
  (list ix iy))

(cl-defun e-envelope-p (envelope)
  (and envelope
       (consp envelope)
       (= (length envelope) 2)
       (i-interval-p (car envelope))
       (i-interval-p (cadr envelope))
       envelope))

(cl-defun e-contain-p (envelope x &optional strictly-p) 
  "X may be envelope or 2d point"
  (and (i-contain-p (car envelope) (car x) strictly-p)
       (i-contain-p (cadr envelope) (cadr x) strictly-p)))

(cl-defun e-within-p (x envelope &optional strictly-p) 
  (e-contain-p envelope x strictly-p))

(cl-defun e-touch-p (envelope1 envelope2)
  (cl-destructuring-bind ((ix1 iy1) (ix2 iy2)) (list envelope1 envelope2)
    (or (and (i-touch-p ix1 ix2)
	     (i-overlap-p iy1 iy2))
	(and (i-touch-p iy1 iy2)
	     (i-overlap-p ix1 ix2)))))

(cl-defun e-overlap-p (envelope1 envelope2 &optional strictly-p)
  (and (i-overlap-p (car envelope1) (car envelope2))
       (i-overlap-p (cadr envelope1) (cadr envelope2))
       (not (and strictly-p (e-touch-p envelope1 envelope2)))))
;;(e-overlap-p '((0 3) (0 3)) '((2 4) (1 3)) t)
;;(e-touch-p '((0 3) (0 3)) '((2 4) (1 3)) )

(cl-defun e-disjoint-p (envelope1 envelope2 &optional allow-touch-p) 
  (or (not (e-overlap-p envelope1 envelope2))
      (and allow-touch-p
	   (e-touch-p envelope1 envelope2))))

(cl-defun e-intersection (envelope1 envelope2)
  (e-make-envelope
   (i-intersection (car envelope1) (car envelope2))
   (i-intersection (cadr envelope1) (cadr envelope2))))

(cl-defun e-union (envelope1 envelope2)
  (and (e-envelope-p envelope1)
       (e-envelope-p envelope2)
       (e-make-envelope
	(i-union (car envelope1) (car envelope2))
	(i-union (cadr envelope1) (cadr envelope2)))))

(cl-defun e-stack-envelopes (_orientation envelopes)
  envelopes)
;;(e-stack-envelopes)
(put 'e-stack-envelopes 'common-lisp-indent-function 1)

(provide 'list-envelope)

;;; list-envelope.el ends here
