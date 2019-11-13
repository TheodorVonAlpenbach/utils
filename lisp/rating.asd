(asdf:defsystem :rating
  :description "Calculate new estimates for Glicko rating"
  :name "rating"
  :author "Mats Bergstrrøm <mbe@progfab.no>"
  :version "1.0"
  :licence "BSD"
  :depends-on (:mb-utils)
  :components ((:cl-source-file "rating")))
