(asdf:defsystem :elapsed
  :name "elapsed"
  :author "Mats Bergstrøm <mbe@lightstructures.no>"
  :version "1.0.0"
  :depends-on (:local-time)
  :components ((:cl-source-file "elapsed")))

(asdf:defsystem :test-elapsed
  :name "test-elapsed"
  :author "Mats Bergstrøm <mbe@lightstructures.no>"
  :version "1.0.0"
  :depends-on (:lisp-unit :elapsed)
  :components ((:cl-source-file "test-elapsed")))
