(defpackage :numerics-utils
  (:use :cl :mb-utils)
  (:export :parse-real
	   :random-interval
	   :normalize-radian
	   :grid-interpolate
	   :reshape-grid
	   :gamma :upper-incomplete-gamma :lower-incomplete-gamma
	   :wrapped-normal-distribution :wnd
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
	   :invert-matrix
	   :matrix-product
	   :matrix-distance
	   :rank
	   :matrix-minus
	   :columns->matrix))
