(asdf:defsystem :bezier
  :name "bezier"
  :author "Mats Bergstrrøm <mbe@lightstructures.no>"
  :version "1.0.0"
  :depends-on (:wave
	       :egina
	       :mb-gnuplot)
  :components
  ((:cl-source-file "test")))
