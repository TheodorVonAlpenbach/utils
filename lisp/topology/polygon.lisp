(in-package :topology)

(defclass polygon (geometry)
  ((boundary :initarg :boundary :accessor boundary :type path)))

;; Seems strange to define a copy of superclass method
(defmethod print-object ((x polygon) stream)
  (print-unreadable-object (x stream :type t)
    (princ (mapcar #'coordinates (points (boundary x))) stream)))

(defmethod make-polygon (points)
  (make-instance 'polygon :boundary (make-path (rcons points (first points)))))
;;(make-polygon '((0 0) (1 0) (0 1)))

(defmethod segments ((x polygon))
    "Shortcut for (segments (boundary x))"
  (segments (boundary x)))
;;(segments (make-polygon '((0 0) (1 0) (0 1))))
