(in-package :topology)

(defmethod boundary ((x geometry))) ;;default is nil

(defmethod boundary ((x segment))
  (make-instance 'multi-geometry :elements (list (start x) (end x))))

(defmethod boundary ((x path))
  (make-instance 'multi-geometry
    :elements (list (start (first (segments x)))
		    (end (last-elt (segments x))))))





