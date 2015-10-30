(asdf:defsystem :topology
  :name "topology"
  :author "Mats Bergstrrøm <mbe@lightstructures.no>"
  :version "1.0.0"
  :depends-on (:mb-utils :numerics-utils :mb-gnuplot)
  :components
  ((:cl-source-file "defpackage")
   (:cl-source-file "shapes" :depends-on ("defpackage"))
   (:cl-source-file "boundary" :depends-on ("shapes"))
   (:cl-source-file "within" :depends-on ("shapes"))
   (:cl-source-file "plot" :depends-on ("shapes"))))
