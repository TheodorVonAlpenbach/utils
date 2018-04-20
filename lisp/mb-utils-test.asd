(asdf:defsystem :mb-utils-test
  :name "mb-utils-test"
  :author "Mats Bergstrrøm <mbe@lightstructures.no>"
  :version "1.0.0"
  :depends-on (:mb-utils)
  :components
  ((:cl-source-file "tests")))
