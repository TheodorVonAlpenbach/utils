(asdf:defsystem :mb-gnuplot
  :name "mb-gnuplot"
  :author "Mats Bergstrrøm <mats@contango.no>"
  :version "0.9.1"
  :licence "GNU"
  :depends-on (:mb-utils :csv)
  :components
  ((:cl-source-file "mb-gnuplot")))
