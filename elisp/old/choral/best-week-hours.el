;;; Problemet er egentlig å finne den beste fordelingen av 100 timer i
;;; en uke (på 168 timer), der hver time representerer en verdi, slik
;;; at summen av timeverdiene blir størst mulig. I tillegg kommer
;;; skrankene at det alltid må være minst fire sammenhengende timer,
;;; og når det er hull må det være minst fire sammenhengende timehull.

;;; Generelt blir problemet. Gitt n, m <= n, k <= m, l < n in N, m indekser i_1, ..., i_m. Finn
;;; Max(Sum(x_i_j, 1<=j<=m), alle sett av i_j slik at, ørk...

;;; Løsningen er blir å finne alle mulige lovlige kombinasjoner av de
;;; m timene innen intervallet 1, ..., n. For hver kombinasjon
;;; beregnes summen og sammenlignes med midlertidig maks.

;;; Det er litt uklart om "hullene" ved randen også må være minst l
;;; store. Antar at de IKKE må være det.

(require 'cl)

(defun best-week-max (values m k l)
  "VALUES is as list of N floats. M, K, L are positive integers
such that, M <= N, K <= M, L <= N."
  (first (sort (mapcar #'(lambda (list) (scalar-product values list)) 
		       (best-week-legal-combinations (length values) m k l))
	       #'>)))
;;(best-week-max (make-random-list 40 300) 15 4 4)

(defun make-random-list (length max-value)
  (loop for i below length collect (random max-value)))
;;(make-random-list 168 300)

(defun scalar-product (list1 list2)
  (apply #'+ (mapcar* #'* list1 list2)))
;;(scalar-product '(1 2 3) '(1 0 1))

(defun best-week-legal-combinations (n m k l)
  "Returns a list of all legal distributions of N elements with M
ones and N-M zeroes"
  (if (or (< n m) (< m k) (< l 1))	;stop criteria
      ()
    ;; else
    (append (loop with ms = (make-list m 1)
		  for i from 0 to (- n m)
		  collect (append (make-list i 0) ms (make-list (- n m i) 0)))

	    (loop for p from k to (- n l k) 
		  append
		  (loop with ps = (make-list p 1)
			for i from 0 to (- n l k p)
			for prefix = (append (make-list i 0) ps (make-list l 0))
			for prefix-size = (length prefix)
			append 
			(mapcar #'(lambda (suffix) (append prefix suffix))
				(best-week-legal-combinations (- n prefix-size) (- m p) k l)))))))
;;(best-week-legal-combinations 6 4 2 2)((1 1 1 1 0 0) (0 1 1 1 1 0) (0 0 1 1 1 1) (1 1 0 0 1 1))

(defun best-week-2 (values m k l)
  "Returns a list of all legal distributions of N elements with M
ones and N-M zeroes"
  (let ((n (length values)))
    (if (or (null values) (< n m) (< m k) (< l 1)) ;stop criteria
      0 
      (max (loop for i from 0 to (- n m)
		 maximize (list-sum values i m))
	   (clean-nil
	    (loop for p from k to (- n l k) 
		  maximize
		  (clean-nil
		   (loop for i from 0 to (- n l k p)
			 maximize (+ (list-sum values i p)
				     (best-week-2 (nthcdr (+ i p l) values) (- m p) k l))))))))))
;;(best-week-2 '(7 1 2 3 0 5 6) 4 2 2)
(best-week-2 (make-random-list 168 300) 8 4 4)

(defun list-sum (list start length)
  (apply #'+ (subseq list start (+ start length))))
;;(list-sum '(1 2 3 4) 1 2)

(defun* clean-nil (x &optional (value 0)) 
  (if (null x) value x))

(defun best-week-count (n m k l)
  (if (or (< n m) (< m k)) ;stop criteria
    0 
    (+ (- n m -1)
	 (loop for p from k to (- n l k) 
		sum
		(loop for i from 0 to (- n l k p)
		       sum (best-week-count (- n i p l) (- m p) k l))))))
;;(best-week-count 168 11 4 4)
