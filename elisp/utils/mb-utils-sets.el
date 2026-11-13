;;; mb-utils-sets.el --- Interval and set helpers -*- lexical-binding: t; -*-

(require 'cl-lib)

;;; Code:

(cl-defstruct
  (interval
    (:constructor interval-oo (l r &aux (open-left t) (open-right t)))
    (:constructor interval-oc (l r &aux (open-left t) (open-right nil)))
    (:constructor interval-co (l r &aux (open-left nil) (open-right t)))
    (:constructor interval-cc (l r &aux (open-left nil) (open-right nil))))
  l r (open-left t) (open-right t))

(cl-defun open-left-p (intv)
  "Return non-nil when INTV excludes its left endpoint."
  (interval-open-left intv))

(cl-defun open-right-p (intv)
  "Return non-nil when INTV excludes its right endpoint."
  (interval-open-right intv))

(cl-defun closed-left-p (intv)
  "Return non-nil when INTV includes its left endpoint."
  (not (open-left-p intv)))

(cl-defun closed-right-p (intv)
  "Return non-nil when INTV includes its right endpoint."
  (not (open-right-p intv)))
;(interval-l (make-interval :l 1 :r 2))
;(open-left-p (interval-oo 1 1))

(cl-defun interval-list (interval)
  "Return INTERVAL's left and right endpoints as a list."
  (list (interval-l interval) (interval-r interval)))
;;(interval-list (interval-oo 1 2))

(cl-defun 1-sphere (radius &optional (center 0))
  "Return the closed interval with RADIUS around CENTER."
  (cl-check-type radius number)
  (cl-check-type center number)
  (unless (>= radius 0)
    (user-error "Sphere radius must be nonnegative: %S" radius))
  (interval-cc (- center radius) (+ center radius)))
;;(within 1 (1-sphere 1))

(cl-defun within (x interval &key (test #'<))
  "Return non-nil when X belongs to INTERVAL under ordering TEST.
Reversed intervals are empty.  A degenerate interval contains its
single endpoint only when both sides are closed."
  (cl-labels ((equivalent-p
	       (a b)
	       (and (not (funcall test a b))
		    (not (funcall test b a)))))
    (not (or (funcall test x (interval-l interval))
	     (funcall test (interval-r interval) x)
	     (and (open-left-p interval)
		  (equivalent-p x (interval-l interval)))
	     (and (open-right-p interval)
		  (equivalent-p x (interval-r interval)))))))
;;(contained 1 (interval-oo 1 2))

;;; Functions that operate on lists interpreted as sets
;;; They are are supplement to the existing set functions like
;;; cl-set-difference, cl-intersection, cl-subsetp, cl-set-exclusive-or, cl-union,
(cl-defun set-equalp (list1 list2 &rest args)
  "Return non-nil when LIST1 and LIST2 represent equal sets.
ARGS are forwarded to `cl-subsetp', including `:key' and `:test'."
  (and (apply #'cl-subsetp list1 list2 args)
       (apply #'cl-subsetp list2 list1 args)))
;;(set-equal '(a) '(a a))

(provide 'mb-utils-sets)

;;; mb-utils-sets.el ends here
