;;;; This module contains shortcuts of functions and classes to make
;;;; the test comments short as possible for readability. They are not
;;;; exported.
(in-package :topology)

(defun mp (x1 y1) (make-point (list x1 y1)))
(defun ms (x1 y1 x2 y2) (make-segment `(,x1 ,y1) `(,x2 ,y2)))
;;(ms 0 0 1 0)

(defun mps (&rest xs-and-ys)
  (mapcar #'make-point (cut xs-and-ys)))
;;(mps 0 0 1 0 0 0 0 1)
(defun mss (&rest xs-and-ys)
  (loop for (s e) in (pairs (cut xs-and-ys))
	collect (make-segment s e)))
;;(mss 0 0 1 0 0 0 0 1)
(defun mpa (&rest xs-and-ys) (make-path (cut xs-and-ys)))
(defun mcpa (&rest xs-and-ys) (make-convex-path (cut xs-and-ys)))
(defun mpl (&rest xs-and-ys) (make-path (cut xs-and-ys)))
(defun mpg (&rest xs-and-ys) (make-polygon (cut xs-and-ys)))
;;(mpg 0 0  1 0  1 1)

(defun mbb (&rest args) (apply #'make-bounding-box args))
;;(mbb 0 1 0 1)

(defun mmg (lpoints)
  "Short cut for make multipoints. Only for testing"
  (make-multi-geometry (mapcar #'make-point lpoints)))
;;(mmg '((1 2)(2 4)))
