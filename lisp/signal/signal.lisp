(in-package :signal)

(defclass discrete-signal ()
  ((data :accessor data
	 :initarg :data
	 :initform '()
	 :type list
	 :documentation "The elements in the support of the signal")
   (start :accessor start
	  :initarg :start
	  :initform 0
	  :type integer
	  :documentation "The start index of the support of the signal"))
  (:documentation "An implementaion of a discrete-time signal."))

(defun sig (support-start &rest args)
  "Initializes a DISCRETE-SIGNAL"
  (make-instance 'discrete-signal :data args :start support-start))
;;(sig -1 -1 0 1)

(defun sequal ((x discrete-signal) (y discrete-signal))
  (and (eql (start x) (start y))
       (equal (data x) (data y))))
;;(sequal (sig 0 1 2) (sig 0 1 2 0))

(defun sequal* ((x discrete-signal) (y discrete-signal))
  (loop for n from (min (start x) (start y)) to (max (stop x) (stop y))
	always (= (sref x n) (sref y n))))
;;(sequal* (sig 0 1 2) (sig 0 1 2 0))

(defmethod zero ((x discrete-signal))
  "Returns the position of the zeroeth index of the signal X"
  (- (start x)))
;;(zero (sig -1 -1 0 1))

(defmethod len ((x discrete-signal)) (length (data x)))

(defmethod end ((x discrete-signal))
  "Returns the end index of the support of the signal X. END is equivalent with LAST + 1"
  (+ (start x) (len x)))
;;(end x1)

(defmethod stop ((x discrete-signal)) (1- (end x)))
;;(stop (sig 1 2 3 4))

(defun signal->tree (x)
  (if (plusp (start x))
    (concatenate 'list
      (list (list 0))
      (make-list (1- (start x)) :initial-element 0)
      (data x))
    (if (plusp (end x))
      (concatenate 'list
	(subseq (data x) 0 (zero x))
	(list (list (elt (data x) (zero x))))
	(subseq (data x) (1+ (zero x))))
      (concatenate 'list
	(data x)
	(make-list (- (end x)) :initial-element 0)
	(list (list 0))))))
;;(signal->tree (sig -6 1 2 3 4 5))
;;(end (sig -6 1 2 3 4 5))
;;(loop for s from -6 to 6 collect (signal->tree (sig s 1 2 3 4 5)))

(defmethod print-object ((x discrete-signal) stream)
  (print-unreadable-object (x stream :type t)
    (format stream "~a" (signal->tree x))))
;;(sig -5 1 2 3 4 5)

(defun pend-list (list value end &optional (start 0))
  "Appends END elements and prepends START elements of VALUE to LIST."
  (concatenate 'list
    (make-list start :initial-element value)
    list
    (make-list end :initial-element value)))
;;(pend-list '(a b c) 0 2 3)

(defun pend (sequence value start &optional (end 0))
  (mb-utils::with-list (x sequence) (pend-list x value end start)))
;;(pend #(a b c) 0 2 3)

(defmethod extend-support ((x discrete-signal) new-start new-end)
  (make-instance 'discrete-signal
    :data (pend (data x) 0
		(max 0 (- (start x) new-start))
		(max 0 (- new-end (end x))))
    :start new-start))
;;(extend-support (sig 0 0 1 2) -1 5)

(defmethod normalize ((x discrete-signal) &rest signals)
  (if signals
    (let ((min-start (min (start x) (reduce #'min signals :key #'start)))
	(max-end (reduce #'max (cons x signals) :key #'end)))
      (loop for s in (cons x signals)
	    collect (extend-support s min-start max-end)))
    x))
;;(normalize (sig -2 1 2 3) (sig 0 1 2 3) (sig 2 1 2 3))

(defmethod add-normalized ((x discrete-signal) (y discrete-signal))
  (make-instance 'discrete-signal
    :data (mapcar #'+ (data x) (data y))
    :start (start x)))
;;(add-normalized (sig -2 1 2 3) (sig -2 1 2 3))

(defmethod multiply ((x discrete-signal) (y discrete-signal))
  (destructuring-bind (x y) (normalize x y)
    (make-instance 'discrete-signal
      :data (mapcar #'* (data x) (data y))
      :start (start x))))

(defmethod multiply ((x number) (y discrete-signal))
  (make-instance 'discrete-signal
    :data (mapcar (mb-utils::bind #'* x) (data y)) :start (start y)))

(defmethod multiply ((y discrete-signal) (x number))
  (multiply x y))
;;(multiply (sig -2 1 2 3 4) 2)

(defmethod s+ ((x discrete-signal) &rest args)
  (if args
    (reduce #'add-normalized (apply #'normalize x args))
    x))
;;(s+ (sig -2 1 2 3) (sig 0 1 2 3) (sig 2 1 2 3))

(defun s* (&rest args) (reduce #'multiply args))
;;(s* (sig -1 1 2 3) 2 (sig 0 1 2 3) (sig 1 1 2 3))

(defmethod s- ((x discrete-signal) &rest args)
  (if args
    (s+ x (s* -1 (apply #'s+ args)))
    (s* -1 x)))
;;(s- (sig -2 1 2 3) (sig 0 1 2 3))
;;(s- (sig -2 1 2 3) (sig 0 1 2 3) (sig 2 1 2 3))
;;(s- (sig -2 1 2 3))

(defmethod support-p ((x discrete-signal) (i integer))
  (<= (start x) i (1- (end x))))
;;(loop with x = (sig 0 1 2 3) for i from -1 to 3 collect (support-p x i))
;;(support-p (shift (delta) 5) 1)

(defmethod sref ((x discrete-signal) (i integer))
  (if (support-p x i)
    (nth (- i (start x)) (data x))
    0))
;;(loop with x = (sig 0 1 2 3) for i from -1 to 3 collect (sref x i))
;;(sref qwe 5)

;;(data qwe)
;;(support-p qwe 5)
;;(setf qwe (shift (delta) 5))
;;(stop qwe)
;;(setf (sref qwe 100) 'ewq)

(defmethod set-sref ((x discrete-signal) i value)
  (unless (support-p x i)
    (setf (data x)
	  (if (< i (start x))
	    (append (make-list (- (start x) i)) (data x))
 	    (append (data x) (make-list (- i (stop x)))))))
  (setf (elt (data x) (- i (start x))) value))

(defmethod convolve ((x discrete-signal) (y discrete-signal))
  (let ((y (sig (+ (start x) (start y))))
	(xf (fold x)))
    (loop for i from (start y) below (- (+ (end xf) (end y)) 1)
	  do (setf (sref y i)
		   (loop for k from (start y) below (end xf))))))

(defun csum (x y k n-min n-max)
  (loop for n from n-min to n-max
	sum (* (sref x n) (sref y (- k n)))))

(defmethod convolve ((x discrete-signal) (y discrete-signal))
  "Implementation avoids unnecessary inner loops"
  (let* ((k-min (+ (start x) (start y)))
	 (k-mid (+ k-min (len y)))
	 (k-max (+ (len x) (len y)))
	 (zl (loop for k from k-min below k-mid
		   collect (csum x y k (start x) (- k (start y)))))
	 (zr (loop for k from k-mid to k-max
		   collect (csum x y k (- k (stop y)) (stop x)))))
    (apply #'sig k-min (nconc zl zr))))
;;(convolve (sig 0 1 2 3) (sig 0 1 1))
;;(convolve (sig 0 1 2) (sig 0 3 1 4))
;;(convolve (sig 0 1 0 0 2) (sig 0 3 0 0 1 0 0 4))
;;(convolve (sig 0 0 0 0 1 2 0 0 0) (sig 0 3 1 4))


(defun chebyshev-signal (order)
  "Returns a signal of order order consisting of the coefficients in the Chebyshev polynom of the same order"
  (case order
    (0 (sig 0 1))
    (1 (sig 1 1))
    (otherwise (s- (s* 2 (shift (chebyshev-signal (- order 1)) 1))
		   (chebyshev-signal (- order 2))))))
;;(chebyshev-signal 6)

(defmethod polynomial ((s discrete-signal))
  (lambda (x)
    (loop for i to (len s)
	  for x^i = 1 then (* x^i x)
	  for part = (* (sref s i) x^i)
;;	  do (print (list i part))
	  sum part)))
;;(gp:plot `(:l (:d ,(polynomial (chebyshev-signal 20)) :x-values (-1 1) :resolution 100)))
;;(gp:plot `((:l (:d ,(polynomial (chebyshev-signal 5)) :x-values (-1 1))) (:l (:d ,#'(lambda (x) (cos (* 5 pi 1/2 x))) :x-values (-1 1)))))
