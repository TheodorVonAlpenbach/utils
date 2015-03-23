(asdf:defsystem :mb-grid
  :name "mb-grid"
  :author "Mats Bergstrrøm <mbe@lightstructures.no>"
  :version "0.9.1"
  :licence "BSD"
  :depends-on (:mb-utils :numerics-utils)
  :components
  ((:cl-source-file "mb-grid")
   (:cl-source-file "grid-interpolation" :depends-on ("mb-grid"))))
