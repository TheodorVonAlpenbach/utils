(in-package :topology)

(defun binary-search (value array
		      &key (low 0) (high (1- (length array)))
			key (test #'<) debug)
  (when debug (print (list :value value :low low :high high)))
  (if (< high low)
    (values nil nil high low)
    (let ((middle (floor (+ low high) 2)))
      (flet ((a (i)
	       (if key (funcall key (aref array i)) (aref array i)))
	     (bs (l h)
	       (binary-search value array :low l :high h :test test :key key)))
	(cond ((funcall test value (a middle)) (bs low (1- middle)))
	      ((funcall test (a middle) value) (bs (1+ middle) high))
	      (t (values (aref array middle) middle low high)))))))
;;(binary-search 11/2 (0-n 100 :key #'list :type 'array) :key #'car)


(defun min3 (a b c test)
  (if (funcall test a b)
    (if (funcall test a c) a c)
    (if (funcall test b c) b c)))

(defun min2 (a b test)
  (if (funcall test a b) a b))
;;(min3 1 -2 3 #'>)

(defun binary-max-1 (seq low high test key max theoretical-max debug)
  "Skip key in this version"
  (when debug (print (list :low low :high high :max max)))
  (if (= low high)
    (funcall key (elt seq low))
    (if (and max theoretical-max
	     (let ((tm (funcall theoretical-max seq low high)))
	       (when debug (print (list :tm tm low high)))
	       (funcall test max tm)))
      ;; max is better than any values in range [vec low .. high]
      max
      ;; otherwise we split and divide low .. high
      (let ((middle (floor (+ low high) 2)))
	(min2 (binary-max-1 seq low middle test key max theoretical-max debug)
	      (binary-max-1 seq (1+ middle) high test key max theoretical-max debug)
	      test)))))
;;(binary-max (0-n 100 :type 'vector) 0 99 :max 10 :debug t)
;;(trace binary-max-1)

(defun binary-max (seq low high
		   &key (test #'<)
		     (key #'identity)
		     (max (funcall key (elt seq low)))
		     (theoretical-max (constantly (1+ max)))
		     debug)
  (unless max (setf max (elt seq low)))
  (binary-max-1 seq low high test key max theoretical-max debug))
;;(binary-max (0-n 100 :type 'vector) 0 99 :max 10 :debug t)
;;(binary-max (0-n 100 :type 'vector) 0 99 (constantly t))

(defun zero-modulo10-impossible-p (a b)
  "Return nil iff no number in [a b] can be zero modulo 10."
  (let ((a-m10 (mod a 10)))
    (and (plusp a-m10)
	 (< (- b a) (- 10 a-m10)))))
;;(loop with a = 9 for b from a to (+ a 9) collect (zero-modulo10-possible-p a b))
