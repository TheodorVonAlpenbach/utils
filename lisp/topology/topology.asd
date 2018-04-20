(asdf:defsystem :topology
  :name "topology"
  :author "Mats Bergstrøm <mbe@lightstructures.no>"
  :version "1.0.0"
  :depends-on (:mb-utils :numerics-utils :mb-gnuplot :fifo)
  :components
  ((:cl-source-file "defpackage")
   (:cl-source-file "utils" :depends-on ("defpackage"))
   (:cl-source-file "geometry" :depends-on ("defpackage"))
   (:cl-source-file "interval" :depends-on ("geometry"))
   (:cl-source-file "box" :depends-on ("interval"))

   (:cl-source-file "point" :depends-on ("geometry" "box"))
   (:cl-source-file "segment" :depends-on ("point"))
   (:cl-source-file "path" :depends-on ("segment"))
   (:cl-source-file "convex-path" :depends-on ("algebra"))
   (:cl-source-file "polygon" :depends-on ("path"))
   (:cl-source-file "triangle" :depends-on ("polygon"))
   (:cl-source-file "ellipse" :depends-on ("segment"))
   (:cl-source-file "multi-geometry" :depends-on ("geometry"))

   (:cl-source-file "points" :depends-on ("ellipse" "triangle" "multi-geometry"))
   (:cl-source-file "shortcuts" :depends-on ("points"))
   ;; (:cl-source-file "shapes" :depends-on ("multi-geometry" "shortcuts"))
   (:cl-source-file "algebra" :depends-on ("shortcuts"))
   (:cl-source-file "boundary" :depends-on ("algebra"))
   (:cl-source-file "cross-product" :depends-on ("algebra"))
   (:cl-source-file "left-of-p" :depends-on ("cross-product"))
   (:cl-source-file "distance" :depends-on ("algebra"))
   (:cl-source-file "within" :depends-on ("algebra"))
   (:cl-source-file "diameter" :depends-on ("algebra"))
   (:cl-source-file "area" :depends-on ("points"))
   (:cl-source-file "angle" :depends-on ("algebra"))
   (:cl-source-file "convex-hull" :depends-on ("convex-path"))
   (:cl-source-file "plot" :depends-on ("algebra"))))
