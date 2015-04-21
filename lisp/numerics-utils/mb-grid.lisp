(defpackage :mb-grid
  (:use :cl :mb-utils :numerics-utils :csv)
  (:export :grid :make-grid :grid-data :grid-axes
	   :list-grid :grid-axes* :span-grid :map-grid
	   :gnuplot-matrix->grid :grid->gnuplot-matrix
	   :export-grid))

(in-package :mb-grid)

;;;; TODO: make GRID a printable object, and remove LIST-GRID
(defclass grid ()
  ((data :initarg :data :accessor grid-data :type array)
   (axes :initarg :axes :accessor grid-axes :type cons)))

(defmethod dimension ((x grid)) (array-rank (grid-data x)))
;;(dimension (make-grid #(1 2 3) #(1 2 3)))

;;(with-slots (data axes) (make-grid #(1 2 3) #(1 2 3)) data)

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
;;(grid-axes (span-grid (lambda (&rest p) (sqrt (reduce #'+ p))) '(#(0 1 2) #(5 10))))

(defun grid-axes* (grid &optional type)
  "Returns the axes of grid as a list. If TYPE is non-nil, it converts
each axis to a sequence of this TYPE"
  (if type
    (loop for x in (grid-axes grid) collect (coerce x type))
    (grid-axes grid)))
;;(grid-axes* (span-grid (lambda (&rest p) (sqrt (reduce #'+ p))) '(#(0 1 2) #(5 10))) 'list)

(defmethod span-grid (fn axes)
  (make-grid (apply #'span-array fn axes) axes))
;;(list-grid (span-grid (lambda (&rest p) (sqrt (reduce #'+ p))) '(#(0 1 2) #(5 10))))

(defun map-grid (fn &rest grids)
  "Assumes all grids have the same axes"
  (make-grid (apply #'map-array fn (mapcar #'grid-data grids)) (grid-axes (first grids))))
;;(setf qwe (span-grid (lambda (&rest p) (sqrt (reduce #'+ p))) '(#(0 1 2) #(5 10))))
;;(list-grid (map-grid (bind #'+ 17) qwe))
;;(list-grid (map-grid #'+ qwe qwe))

(defun grid->gnuplot-matrix (grid)
  "Converts grid to the layout expected by gnuplot for splotting with a nonuniform matrix.
splot 'file' nonuniform matrix. See gnuplot doc for more"
  (assert (= (dimension grid) 2))
  (destructuring-bind (row-axis column-axis) (grid-axes* grid 'list)
    (cons (cons (length column-axis) column-axis)
	  (mapcar #'cons row-axis (array->tree (grid-data grid))))))

(defun gnuplot-matrix->grid (matrix)
  (make-grid (tree->array (mapcar #'rest (rest matrix)))
	     (list (map 'vector #'first (rest matrix))
		   (coerce (rest (first matrix)) 'vector))))
;;(list-grid (gnuplot-matrix->grid '((nil 1 2 3) (0.1 2 3 4) (0.2 3 4 5) (0.3 4 5 6))))

(defun export-grid (grid filename &rest args)
  "Exports GRID to csv format and writes it to FILENAME. See write-csv for ARGS"
  (apply #'write-csv-file (gnuplot-matrix->grid grid) filename args))
;;(export-grid (span-grid #'+ '(#(0 1 2) #(5 10))) "~/tmp/test.csv" :column-separator #\Space)
