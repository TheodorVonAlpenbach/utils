(asdf:defsystem :test-fifo
  :name "test-fifo"
  :author "Mats Bergstrøm <mbe@lightstructures.no>"
  :version "1.0.0"
  :depends-on (:fifo :lisp-unit)
  :components
  ((:cl-source-file "test-fifo")))
