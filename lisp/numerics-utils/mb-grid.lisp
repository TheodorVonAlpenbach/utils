(defpackage :mb-grid
  (:use :cl :mb-utils :numerics-utils :csv)
  (:export :grid :make-grid :grid-data :grid-axes
	   :list-grid :grid-axes* :span-grid :map-grid
	   :gnuplot-matrix->grid :grid->gnuplot-matrix
	   :import-grid :export-grid
	   :with-grid-data))

(in-package :mb-grid)

;;;; TODO: make GRID a printable object, and remove LIST-GRID
(defclass grid ()
  ((data :initarg :data :accessor grid-data :type array)
   (axes :initarg :axes :accessor grid-axes :type cons)))

(defun grid-p (x) (typep x 'grid))

(defmethod dimensions ((x grid)) (array-dimensions (grid-data x)))
(defmethod dimension ((x grid)) (array-rank (grid-data x)))
;;(dimension (make-grid #(1 2 3) #(1 2 3)))
;;(grid-p (make-grid #(1 2 3) #(1 2 3)))

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
;;(grid->gnuplot-matrix (gnuplot-matrix->grid '((nil 1 2 3) (0.1 2 3 4) (0.2 3 4 5) (0.3 4 5 6))))
;;(grid-data (egina::sort-grid-columns egina::qwe))

(defun gnuplot-matrix->grid (matrix)
  (make-grid (tree->array (mapcar #'rest (rest matrix)))
	     (list (map 'vector #'first (rest matrix))
		   (coerce (rest (first matrix)) 'vector))))
;;(list-grid (gnuplot-matrix->grid '((nil 1 2 3) (0.1 2 3 4) (0.2 3 4 5) (0.3 4 5 6))))

(defun export-grid (grid filename &rest args)
  "Exports GRID to csv format and writes it to FILENAME. See WRITE-CSV for ARGS"
  (apply #'write-csv-file (grid->gnuplot-matrix grid) filename args))
;;(export-grid (span-grid #'+ '(#(0 1 2) #(5 10))) "~/tmp/test.csv")

(defun import-grid (filename &rest args)
  "Imports and parses a grid object from csv like formatted file FILENAME.
The format may be tuned. See READ-CSV-FILE for ARGS"
  (flet ((parse-number (x)
	   (handler-case (parse-number::parse-number x)
	     (floating-point-underflow () 0))))
    (gnuplot-matrix->grid (maptree #'parse-number (apply #'read-csv-file filename args)))))
;;(list-grid (import-grid "~/tmp/test.csv"))

(defmacro with-grid-data ((var grid &optional (data-type 'array)) &body body)
  "Returns a copy of grid but with data set to the result of BODY.
In BODY the grid data are bound to VAR The grid data is converted to
DATA-TYPE in BODY."
  (with-gensyms (gdata)
    `(let ((,gdata (grid-data ,grid)))
       (copy-object ,grid
	 :data ,(case data-type
		      (array `(let ((,var ,gdata))
				(progn ,@body)))
		      (tree `(with-tree (,var ,gdata)
			       (progn ,@body)))
		      (t (error "WITH-GRID-DATA only supports the types ARRAY and TREE"))))))) 
;;(list-grid (with-grid-data (x (make-grid #2A((1 2 3) (2 3 4)) '(#(1 2 3) #(1 2))) tree) (rest x)))
;;(list-grid (with-grid-data (x (first *raos*) tree) (rest x)))

(defun swap-parts (sequence n)
  (concatenate (type-of sequence) (subseq sequence n) (subseq sequence 0 n)))
;;(swap-parts #(0 1 2 3) 2)

(defmethod rotate ((x cons) &optional (n 1))
  "A single rotation is equal to a transpose."
  (swap-parts x (mod n (length x))))
;;(rotate '(1 2 3 4 5 6) -1)

(defun rotate-sequence (sequence &optional (n 1))
  "A single rotation is equal to a transpose."
  (coerce (rotate (coerce sequence 'list) n) (type-of sequence)))
;;(rotate-sequence #(1 2 3 4 5 6) -1)

(defmethod rotate ((x array) &optional (n 1))
  "A single rotation is equal to a transpose."
  (if (= (array-rank x) 1)
    (coerce (rotate (coerce x 'list) n) 'vector)
    (let ((res (make-array (array-dimensions x))))
      (loop for i below (array-total-size x)
	    do  (setf (apply #'aref res (rotate (listify (mb-utils::row-major-index->index x i)) n))
		      (row-major-aref x i)))
      res)))
;;(rotate #2A((1 2) (3 4)))
;;(rotate #(1 2 3))

(defmethod rotate ((x grid) &optional (n 1))
  "A single rotation is equal to a transpose."
  (make-grid (rotate (grid-data x) n) (rotate (grid-axes x) n)))
;;(list-grid (rotate (make-grid #2A((1 2 3) (2 3 4)) '(#(1 2 3) #(1 2)))))

(defmethod transpose-grid ((x grid))
  (assert (= (dimension x) 2))
  (make-grid (matrix-transpose (grid-data x)) (rotate (grid-axes x))))
;;(list-grid (transpose-grid (make-grid #2A((1 2 3) (2 3 4)) '(#(1 2 3) #(1 2)))))

