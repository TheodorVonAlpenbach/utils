(in-package :topology)

(defclass multi-geometry (geometry)
  ((elements :initarg :elements :accessor elements :type list)))

(defmethod make-multi-geometry (elements)
  (make-instance 'multi-geometry :elements elements))
;;(make-multi-geometry (list (make-point '(1 2)) (make-point '(2 4)))
