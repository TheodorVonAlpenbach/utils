(asdf:defsystem :topology
  :name "topology"
  :author "Mats Bergstrrøm <mbe@lightstructures.no>"
  :version "1.0.0"
  :depends-on (:mb-utils :numerics-utils :mb-gnuplot)
  :components
  ((:cl-source-file "defpackage")
   (:cl-source-file "shortcuts" :depends-on ("defpackage"))
   (:cl-source-file "shapes" :depends-on ("defpackage" "shortcuts"))
   (:cl-source-file "algebra" :depends-on ("shapes"))
   (:cl-source-file "boundary" :depends-on ("algebra"))
   (:cl-source-file "within" :depends-on ("algebra"))
   (:cl-source-file "diameter" :depends-on ("algebra"))
   (:cl-source-file "angle" :depends-on ("algebra"))
   (:cl-source-file "convex-path" :depends-on ("algebra"))
   (:cl-source-file "convex-hull" :depends-on ("convex-path"))
   (:cl-source-file "plot" :depends-on ("algebra"))))
