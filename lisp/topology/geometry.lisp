(in-package :topology)

(defclass geometry () ())

(defun geometry-p (x) (typep x 'geometry))
;;(mapcar #'geometry-p (list '(1 2) (make-point '(1 2))))

