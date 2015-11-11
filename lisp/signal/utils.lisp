(in-package :signal)

(defparameter *default-db-unit* :voltage)

(defun db (x &optional (unit *default-db-unit*) (reference 1))
  (case unit
    (:power (* 10 (log x 10)))
    (:voltage (* 20 (log x (/ 10 reference))))
    (t (error "Illegal unit ~a" unit))))
;;(db 10)
