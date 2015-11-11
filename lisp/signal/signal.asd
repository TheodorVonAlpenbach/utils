(asdf:defsystem :signal
  :name "signal"
  :author "Mats Bergstrøm <mats@contango.no>"
  :version "1.0.0"
  :depends-on (:mb-utils :numerics-utils :mb-gnuplot :ptester)
  :components
  ((:cl-source-file "defpackage")
   (:cl-source-file "utils" :depends-on ("defpackage"))
   (:cl-source-file "signal" :depends-on ("utils"))
   (:cl-source-file "signals" :depends-on ("signal"))
   (:cl-source-file "filters" :depends-on ("signal"))))
