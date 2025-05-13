;;;; closed intervals: '(a b) is [a b]
;;;; empty intervals are represented as nil 
;;;; parameter symbols X represent either a number or an interval 
(cl-defun i-make-interval (a b)
  "Return the closed interval [A B]."
  (list a b))

(cl-defun i-left (interval) (first interval))
(cl-defun i-right (interval) (second interval))

(cl-defun i-interval-p (interval)
  "Return not nil if INTERVAL is a valid interval object."
  (and interval
       (consp interval)
       (= (length interval) 2)
       (numberp (first interval))
       (numberp (second interval))
       (<= (first interval) (second interval))
       interval))

(cl-defun i-contain-p (interval x &optional strictly-p) 
  "Return not nil if number x is within interval"
  (if strictly-p
    (if (listp x)
      (and (< (first interval) (first x) (second x) (second interval)))
      (< (first interval) x (second interval)))
    (if (listp x)
      (and (<= (first interval) (first x) (second x) (second interval)))
      (<= (first interval) x (second interval)))))

(cl-defun i-within-p (x interval &optional strictly-p) 
  (i-contain-p interval x strictly-p))

(cl-defun i-touch-p (interval1 interval2)
  (cl-destructuring-bind ((a b) (c d)) (list interval1 interval2)
    (if (<= a c) (= b c) (= a d))))
;;(i-touch-p '(0 2) '(-1 0))

(cl-defun i-overlap-p (interval1 interval2 &optional strictly-p)
  (and (i-interval-p interval1)
       (i-interval-p interval2)
       (cl-destructuring-bind ((a b) (c d)) (list interval1 interval2)
	 (if strictly-p
	   (or (< a c b) (< c a d))
	   (or (<= a c b) (<= c a d))))))

(cl-defun i-disjoint-p (interval1 interval2 &optional allow-touch-p) 
  (or (not (i-overlap-p interval1 interval2))
      (and allow-touch-p
	   (i-touch-p interval1 interval2))))
;;(i-disjoint-p '(0 2) '(3 5))

(cl-defun i-intersection (interval1 interval2)
  (and (i-interval-p interval1)
       (i-interval-p interval2)
       (cl-destructuring-bind ((a b) (c d)) (list interval1 interval2)
	 (cond ((<= a c b) (i-make-interval c (min b d)))
	       ((<= c a d) (i-make-interval a (min b d)))))))

(cl-defun i-union (interval1 interval2)
  ;; Need to handle the fact that I ∪ ∅ ≍ I
  (if (i-interval-p interval1)
    (if (i-interval-p interval2)
      (i-make-interval (min (first interval1) (first interval2))
		       (max (second interval1) (second interval2)))
      interval1)
    (i-interval-p interval2)))

(provide 'list-interval)
