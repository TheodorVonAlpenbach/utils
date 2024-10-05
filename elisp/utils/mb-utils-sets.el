(provide 'mb-utils-sets)

(cl-defstruct
  (interval
    (:constructor interval-oo (l r &aux (open-left t) (open-right t)))
    (:constructor interval-oc (l r &aux (open-left t) (open-right nil)))
    (:constructor interval-co (l r &aux (open-left nil) (open-right t)))
    (:constructor interval-cc (l r &aux (open-left nil) (open-right nil))))
  l r (open-left t) (open-right t))

(cl-defun open-left-p (intv) (interval-open-left intv))
(cl-defun open-right-p (intv) (interval-open-right intv))
(cl-defun closed-left-p (intv) (not (open-left-p intv)))
(cl-defun closed-right-p (intv) (not (open-right-p intv)))
;(interval-l (make-interval :l 1 :r 2))
;(open-left-p (interval-oo 1 1))

(cl-defun interval-list (interval)
  (list (interval-l interval) (interval-r interval)))
;;(interval-list (interval-oo 1 2))

(cl-defun 1-sphere (radius &optional (center 0))
  (interval-cc (- center radius) (+ center radius)))
;;(within 1 (1-sphere 1))

(cl-defun within (x interval &key (test #'<))
  "Returns T if X is within INTERVAL. This could be made a macro
later."
  (cl-labels ((= (a b) (and (not (funcall test a b)) 
			 (not (funcall test b a)))))
    (not (or (funcall test x (interval-l interval))
	     (funcall test (interval-r interval) x)
	     (and (open-left-p interval) (= x (interval-l interval)))
	     (and (open-right-p interval) (= x (interval-r interval)))))))
;;(contained 1 (interval-oo 1 2))

;;; Functions that operate on lists interpreted as sets
;;; They are are supplement to the existing set functions like
;;; cl-set-difference, cl-intersection, cl-subsetp, cl-set-exclusive-or, cl-union,
(cl-defun set-equalp (list1 list2 &rest args)
  (and (apply #'cl-subsetp list1 list2 args)
       (apply #'cl-subsetp list2 list1 args)))
;;(set-equal '(a) '(a a))
