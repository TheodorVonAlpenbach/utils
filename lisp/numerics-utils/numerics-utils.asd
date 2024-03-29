(asdf:defsystem :numerics-utils
  :name "numerics-utils"
  :author "Mats Bergstrrøm <mats@contango.no>"
  :version "0.9.1"
  :licence "GNU"
  :depends-on (:mb-utils)
  :components
  ((:cl-source-file "defpackage")
   (:cl-source-file "safe-operators" :depends-on ("defpackage"))
   (:cl-source-file "gamma" :depends-on ("defpackage"))
   (:cl-source-file "statistics" :depends-on ("safe-operators"))
   (:cl-source-file "integration" :depends-on ("defpackage"))
   (:cl-source-file "matrix" :depends-on ("safe-operators"))))
