(defpackage :numerics-utils
  (:use :cl :mb-utils)
  (:export :parse-real
	   :normalize-radian
	   :grid-interpolate
	   :reshape-grid
	   :gamma-function
	   :wrapped-normal-distribution :wnd
	   :safe-op
	   :safe-*
	   :round-floating-point-underflow-to-zero
	   :handles-outflow
	   :exp-safe
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
	   :matrix-product))
