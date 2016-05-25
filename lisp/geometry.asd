(asdf:defsystem :geometry
  :name "geometry"
  :author "Mats Bergstrrøm <mats@contango.no>"
  :version "1.0"
  :licence "BSD"
  :depends-on (:mb-utils)
  :components
  ((:cl-source-file "geometry")
   (:cl-source-file "geometry-interval" :depends-on ("geometry"))))

