;;;; closed intervals: '(a b) is [a b]
;;;; empty intervals are represented as nil 
;;;; parameter symbols X represent either a number or an interval 
(defun i-make-interval-p (a b)
  (list a b))

(defun i-interval-p (interval)
  (and interval
       (consp interval)
       (= (length interval) 2)
       (numberp (first interval))
       (numberp (second interval))
       (<= (first interval) (second interval))
       interval))

(defun i-contain-p (interval x &optional strictly-p) 
  (if strictly-p
    (if (listp x)
      (and (< (first interval) (first x) (second x) (second interval)))
      (< (first interval) x (second interval)))
    (if (listp x)
      (and (<= (first interval) (first x) (second x) (second interval)))
      (<= (first interval) x (second interval)))))

(defun i-within-p (x interval &optional strictly-p) 
  (i-contain-p interval x strictly-p))

(defun i-touch-p (interval1 interval2)
  (destructuring-bind ((a b) (c d)) (list interval1 interval2)
    (if (<= a c) (= b c) (= a d))))
;;(i-touch-p '(0 2) '(-1 0))

(defun i-overlap-p (interval1 interval2 &optional strictly-p)
  (and (i-interval-p interval1)
       (i-interval-p interval2)
       (destructuring-bind ((a b) (c d)) (list interval1 interval2)
	 (if strictly-p
	   (or (< a c b) (< c a d))
	   (or (<= a c b) (<= c a d))))))

(defun i-disjoint-p (interval1 interval2 &optional allow-touch-p) 
  (or (not (i-overlap-p interval1 interval2))
      (and allow-touch-p
	   (i-touch-p interval1 interval2))))
;;(i-disjoint-p '(0 2) '(3 5))

(defun i-intersection (interval1 interval2)
  (and (i-interval-p interval1)
       (i-interval-p interval2)
       (destructuring-bind ((a b) (c d)) (list interval1 interval2)
	 (cond ((<= a c b) (i-make-interval-p c (min b d)))
	       ((<= c a d) (i-make-interval-p a (min b d)))))))

(defun i-union (interval1 interval2)
  ;; Need to handle the fact that I ∪ ∅ ≍ I
  (if (i-interval-p interval1)
    (if (i-interval-p interval2)
      (i-make-interval-p (min (first interval1) (first interval2))
			 (max (second interval1) (second interval2)))
      interval1)
    (i-interval-p interval2)))

(provide 'list-interval)
