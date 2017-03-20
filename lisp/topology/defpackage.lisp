(defpackage :topology
  (:use :cl :mb-utils :numerics-utils)
  (:export :geometry
	   :multi-geometry :make-multi-geometry
	   :interval
	   :point :make-point
	   :segment :make-segment :start :end
	   :path :make-path :segments
	   :convex-path :make-convex-path
	   :polygon :make-polygon
	   :triangle :make-triangle
	   :ellipse :make-ellipse
	   :box :make-box
	   :points :coordinates :boundary
	   :area
	   :within
	   :gequal
	   :distance
	   :format-object
	   :plot
	   :gp-point)
  (:shadow :head
	   :tail))
