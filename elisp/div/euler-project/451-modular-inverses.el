(defun modular-inverse-p (i n) (= (mod (* i i) n) 1))
;;(modular-inverse-p 11 15)

(defun find-max-modular-inverse (n)
  (loop for i from (- n 2) downto 1
	if (modular-inverse-p i n) return i))

(defun find-max-modular-inverse (n)
  (loop for i from (- n 2) downto 1
	if (= (mod (* i i) n) 1) return i))
;;(time (find-modular-inverse (round 2E7)))

(defun find-min-modular-inverse (n)
  (or (loop for i from 2 to (floor (/ n 2))
	    if (= (mod (* i i) n) 1) return i)
      1))
;;(find-min-modular-inverse 20)
;;(time(find-min-modular-inverse 2E7))

;; (setf eval-expression-print-length 20)
;; (time (all-factors 20000001))
;; (time (length (mapcar #' factorize (make-list 1000 5000000001))))
;; (factorize 5 '(2 3))
;;(setf eval-expression-print-length 20)
;;(log 3 10)
;;(/ (log 3) (log 10))

;;(last 10000-first-primes)
;;(loop for i from 10000 to 100000 if (primep i) return i)
;;(primep 104743)
;;(aref +fvec+ 104743)
;;(aref +fvec+ 100007)
;;(aref +fvec+ 11)

(defun cl-make-factor-vector (n primes)
  (let ((res (make-array (list n 2) :initial-element 1)))
    (loop for i below n do (setf (aref res i 0) i))
    (loop for p in primes do
	  (loop for i from 1
		for ip = (* i p)
		while (< ip n) do
		(when (= 1 (aref res ip 1))
		  (setf (aref res ip 0) i)
		  (setf (aref res ip 1) p))))
    res))
;;(cl-make-factor-vector 20 10000-first-primes)
;;(time (progn (setf +fvec+ (cl-make-factor-vector (round 2E7) 10000-first-primes)) (array-dimensions +fvec+)))

(defun make-factor-vector (n primes)
  (let ((res (coerce (loop for i below n collect (list i 1)) 'vector)))
    (loop for p in primes do
	  (loop for i from 1
		for ip = (* i p)
		while (< ip n) do
		(when (null (aref res ip))
		  (push i (aref res ip))
		  (push p (aref res ip)))))
    res))
;;(make-factor-vector 20 10000-first-primes)
(time (progn (setf +fvec+ (make-factor-vector 10000000 10000-first-primes)) (length +fvec+)))
;(time (loop for n from 2 below 1000000 do (factorize-rec n +fvec+)))
;;(position nil +fvec+ :start 2)

(defun cl-factorize-rec (n fvec)
  (when (> n 1) (cons (aref fvec n 1) (cl-factorize-rec (aref fvec n 0) fvec))))
;;(cl-factorize-rec 2345 +fvec+)
;;(aref +fvec+ 2345 0)
;;(time (loop for n from 2 below 10 do (cl-factorize-rec n +fvec+)))
(defun factorize-rec (n fvec)
  (when (> n 1)
    (destructuring-bind (p rest) (aref fvec n)
      (cons p (factorize-rec rest fvec)))))
;;(factorize-rec 2345 +fvec+)
;;(time (loop for n from 2 below 1000000 do (cl-factorize-rec n +fvec+)))

(defun factorize-all (n primes)
  (let* ((res (make-vector n nil)))
    (loop for p in primes do
	  (loop for i from 2
		for ip = (* i p)
		while (< ip n) do
		(push i (aref res ip))))
    res))
;;(factorize-all 20 10000-first-primes)
;;(factorize-all 2000 10000-first-primes)

(defun factorize-all-el (n)
  (factorize-all n 10000-first-primes))
;;(setf +factors+ (factorize-all-el (round 1E6)))
;;(aref +factors+ 12)

(defun combine-factors (fs1 fs2 a b)
  "Return all products of integers in fs1 and fs1 from a below b"
  (remove-duplicates
      (loop for f1 in fs1 append
	 (loop for f2 in fs2
	       for p = (* f1 f2)
	       while (< p b)
	       if (not (< p a))
	       collect p))))
;;(combine-factors (cons 4 (all-factors-2 4)) (cons 6 (all-factors-2 6)) 7 20)
;;(combine-factors '(2 3 4 5) '(2 3 4 5) 10 20)

;; (length +factors+)
;; (setf +min-modular-inverses+ (make-vector (length +factors+) nil))

(defun find-min-modular-inverses (n)
  (loop with factors = (factorize-all n 10000-first-primes)
	with min-modular-inverses = (make-vector n 1)
	for i from 3 below (1- n)
	for a = (1- i)
	for b = (1+ i)
	for fs1 = (all-factors-3 a factors)
	for fs2 = (all-factors-3 b factors)
	for fs = (combine-factors fs1 fs2 (1+ b) n) do
	(loop for f in fs do
	      (when (= 1 (aref min-modular-inverses f))
		(setf (aref min-modular-inverses f) i)))
	finally return min-modular-inverses))
;;(time (find-min-modular-inverses 100000))
;;(time (map 'vector #'find-min-modular-inverse (a-b 0 100000)))
;;(equal (find-min-modular-inverses 11111) (map 'vector #'find-min-modular-inverse (a-b 0 11110)))

(defun all-factors-3 (n factors)
  (sort (remove-duplicates
	    (cons n (loop for f in (aref factors n)
			  for subfactors = (all-factors-3 f factors)
			  append (cons f subfactors))))
    #'<))
;;(all-factors-3 20 +factors+)

(defun all-factors-2 (n)
  (loop for f in (aref +factors+ n)
	for subfactors = (all-factors-2 f)
	append (cons f subfactors)))
;;(sort (remove-duplicates (all-factors-2 100111)) #'<)
;; (2 (2 1)) spawns (4 (2 2)), (8 (2 4)), (2^k2 ...), where 2^k2 < N
;; (3 (3 1)) spawns (9 (3 3)), (27 (3 9)),..., (6 (3 2))
;; (4 (2 2)) spawns (8 (2 4)) and (12 (3 4))

(defun find-min-modular-inverses (ns)
  (loop for i in ns
	for x = (find-min-modular-inverse i)
	if (> x 1) collect (list i x)))
;;(length (find-min-modular-inverses (0-n 10000)))

(defun sum-max-modular-inverse (a b)
  (loop for n from a to b sum (find-modular-inverse n)))

;;(sum-max-modular-inverse 3 15)

;; (mapcar #'find-min-modular-inverse (a-b 21 30))

;; (mapcar #'find-min-modular-inverse '(2400
;; 				     1200 600 150
;; 				     800 400 200 100
;; 				     480 ; from (* 30 32) / 2
;; 				     360 ; from (* 18 20)
;; 				     ))

;; (mapcar #'find-min-modular-inverse '(960
;; 				     480 240
;; 				     120 ; from (* 10 12)
;; 				     320 160
;; 				     80 ; from (* 8 10)
;; 				     192
;; 				     96 ; from (* 16 18) / 3
;; 				     ))

(defun pvecs-combine (pvec1 pvec2)
  (loop for f1 in pvec1 append
	(loop for f2 in pvec2 collect (* f1 f2))))
;;(pvecs-combine '(2 4) '(3 9))

(defun all-divisors-1 (pvecs)
  (when pvecs
    (destructuring-bind (divisors . pvecs) pvecs
      (loop for pvec in pvecs
	    do (setf divisors (append divisors (pvecs-combine divisors pvec))))
      divisors)))
;;(all-divisors-1 '((2 4 8 16 32) (3) (5 25)))

(defun all-divisors (pfs primes)
  (all-divisors-1 (cl-mapcar #'pvec pfs primes)))
;;(all-divisors '(5 1 2) '(2 3 5))

(defun pvec (pmax prime)
  (loop for i from 1 to pmax collect (expt prime i)))
;;(pvec 5 2)

;; (let ((n (expt 10 7)))
;;   (loop for a from n to (+ n 500)
;; 	maximize (length (factorize (* (1- a) (1+ a))))))

;; (pp (all-factors 120))

;; '(1 2 3 4 5 6 8 10 12 15 20 24 30 40 60 120)

;; '(1 (2 (4 (8 (12 (24 (120)) (60 (120)))
;; 	    (20 (40 (120)) (60 (120)))))
;;       (6 (12 (24 (120)) (60 (120)))
;; 	 (30 (60 (120))))
;;       (10 (20 (40 (120))) (30 (60 (120)))))) 

;; (setf 6l (list 6))
;; (setf 2-6l (list 2 6l))
;; (setf 3-6l (list 3 6l))
;; (setf 1l (list 1 2-6l 3-6l))
;; (setf (car 6l) 6)
;; (setf (cdr 2-6l) 5)
