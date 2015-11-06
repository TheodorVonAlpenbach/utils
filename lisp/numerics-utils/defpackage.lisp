(defpackage :numerics-utils
  (:use :cl :mb-utils)
  (:export :parse-real
	   :random-interval
	   :normalize-radian
	   :grid-interpolate
	   :reshape-grid
	   :grid-p
	   :gamma :upper-incomplete-gamma :lower-incomplete-gamma
	   :wrapped-normal-distribution :wnd
	   :variance :stddev :random-normal-fn :random-normal
	   :exp-safe :expt-safe
	   :safe-op :safe-+ :safe-*
	   :round-floating-point-underflow-to-zero
	   :handles-outflow
	   :dot-product
	   :save-multiplication-outflow-by-log
	   :matrix-map-rows
	   :integrate-simpson-sequence
	   :matrix-reverse-rows
	   :matrix->tree
	   :tree->matrix
	   :span-matrix
	   :map-matrix
	   :matrix-column
	   :matrix-inversion :matrix-transpose
	   :matrix-product
	   :matrix-distance
	   :matrix-norm
	   :rank
	   :determinant
	   :matrix-difference
	   :column->matrix :columns->matrix :rows->matrix
	   :map-rows
	   :matrix-row
	   :submatrix
	   :vector-inner-product))
