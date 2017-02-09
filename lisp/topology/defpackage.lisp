(defpackage :topology
  (:use :cl :mb-utils :numerics-utils)
  (:export :geometry
	   :multi-geometry :make-multi-geometry
	   :interval
	   :point :make-point
	   :segment :make-segment :start :end
	   :path :make-path :segments
	   :polygon
	   :triangle :make-triangle
	   :points :coordinates :boundary
	   :within
	   :gequal
	   :distance
	   :format-object
	   :plot
	   :gp-point))
