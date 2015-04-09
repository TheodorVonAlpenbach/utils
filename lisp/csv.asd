(asdf:defsystem :csv
  :name "csv"
  :author "Mats Bergstrrøm <mbe@lightstructures.no>"
  :version "1.0"
  :licence "BSD"
  :depends-on (:mb-utils)
  :components ((:cl-source-file "csv")))
