(asdf:defsystem :timeseries-test
  :name "timeseries-test"
  :author "Mats Bergstrrøm <mbe@lightstructures.no>"
  :version "1.0.0"
  :depends-on (:timeseries :lisp-unit)
  :components
  ((:cl-source-file "timeseries-test")))
