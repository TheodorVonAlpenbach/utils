(asdf:defsystem :timeseries
  :name "timeseries"
  :author "Mats Bergstrrøm <mbe@lightstructures.no>"
  :version "1.0.0"
  :depends-on (:mb-utils :numerics-utils :mb-gnuplot :csv)
  :components
  ((:cl-source-file "defpackage")
   (:cl-source-file "turning-points" :depends-on ("defpackage"))))
