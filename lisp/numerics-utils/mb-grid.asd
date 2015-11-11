(asdf:defsystem :mb-grid
  :name "mb-grid"
  :author "Mats Bergstrrøm <mbe@lightstructures.no>"
  :version "0.9.1"
  :licence "BSD"
  :depends-on (:parse-number :mb-utils :numerics-utils :csv)
  :components
  ((:cl-source-file "mb-grid")
   (:cl-source-file "grid-interpolation" :depends-on ("mb-grid"))))
