(in-package :topology)

(defclass convex-path (path) ())

(defmethod make-convex-path ((x cons) &optional verify-convex-p)
  (when verify-convex-p
    (error "Not implemented convex verication"))
  (when (= (length x) 1)
    (error "A path must contain at least two points!"))
  (make-instance 'convex-path
    :segments (loop for (start end) in (pairs x)
		    collect (make-segment start end))))
;;(make-convex-path (list (make-point '(0 0)) (make-point '(0 0))))

(defmethod make-convex-path ((x path) &optional verify-convex-p)
  (when verify-convex-p
    (error "Not implemented convex verication"))
  (make-instance 'convex-path :segments (segments x)))
;;(make-convex-path (mpl 0 0 1 0))
