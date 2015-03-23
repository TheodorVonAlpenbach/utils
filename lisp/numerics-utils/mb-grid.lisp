(defpackage :mb-grid
  (:use :cl :mb-utils :numerics-utils)
  (:export :grid :make-grid :grid-data :grid-axes :list-grid))

(in-package :mb-grid)

;;;; TODO: make GRID a printable object, and remove LIST-GRID


(defclass grid ()
  ((data :initarg :data :accessor grid-data :type array)
   (axes :initarg :axes :accessor grid-axes :type cons)))

(defun make-grid (data axes)
  (make-instance 'grid :data data :axes axes))

(defun list-grid (grid)
  (list :data (grid-data grid) :axes (grid-axes grid)))
;;(list-grid (make-grid #(1 2 3) #(1 2 3)))

;;; Methods on simple list grid: (data axes)
(defmethod grid-data ((grid cons))
  (first grid))
;;(grid-data (list 'data 'axes))

(defmethod grid-axes ((grid cons))
  (second grid))
;;(grid-axes (list 'data 'axes))

(defmethod span-grid (fn axes)
  (make-grid (apply #'span-array fn axes) axes))
;;(list-grid (span-grid (lambda (&rest p) (sqrt (reduce #'+ p))) '(#(0 1 2) #(5 10))))

(defun map-grid (fn &rest grids)
  "Assumes all grids have the same axes"
  (make-grid (map-array fn (mapcar #'grid-data grids)) (first grids)))
