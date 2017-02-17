(asdf:defsystem :topology-test
  :name "topology-test"
  :author "Mats Bergstrrøm <mbe@lightstructures.no>"
  :version "1.0.0"
  :depends-on (:topology :lisp-unit)
  :components
  ((:cl-source-file "tests")))
