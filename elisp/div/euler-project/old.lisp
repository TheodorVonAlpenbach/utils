(defun max-modular-inverse (n)
  (loop for i from (- n 2) downto 1
	if (modular-inverse-p i n) return i))
;;(time (max-modular-inverse 20))

(defun max-modular-inverses (n)
  (loop for i from 3 to n collect (max-modular-inverse i)))
;;(time (max-modular-inverses 100))

(defun reset-factor-table (ft)
  (destructuring-bind (n m) (array-dimensions ft)
    (loop for i below n do
	  (loop for j below m do
		(setf (aref ft i j) 0)))
    ft))
;;(array-dimensions (reset-factor-table *ft*))

(defun ft-next-prime (p ft)
  (loop for i from p below (array-dimension ft 0) if (zerop (aref ft i 0)) return i))

(defun remake-factor-table-new (ft &optional (first-prime 2))
  (loop with n = (array-dimension ft 0)
	for p = first-prime then (ft-next-prime p ft)
	while (<= p n) do
	(loop for i from 2
	      for ip = (* i p)
	      while (< ip n) do
	      (setf (aref ft ip (factor-table-available-column-index ft ip)) i)))
  ft)
;;(setf *ft* (remake-factor-table *ft*))

(defparameter *visited-ht* (make-hash-table))
(defun visited-ht-p (x)
  (gethash x *visited-ht*))
(defun set-visited-ht (x) (setf (gethash x *visited-ht*) t))

(defun factor-table-row-factors-raw (ft i)
  (let ((m (array-dimension ft 1)))
    (loop for j below m
	  for f = (aref ft i j)
	  while (> f 1) collect f)))
;;(factor-table-row-factors-raw *ft* 7920)
;;(factor-table-row-factors-raw *ft* 0)

(defun all-factors-raw (n)
  (cons n (loop for f in (factor-table-row-factors-raw *ft* n)
		append (all-factors f))))

(defun min-modular-inverses-semirec-1 (i fi m)
  (let ((k (* fi m)))
    (when (or (> k *n*) (and (= (svref *inverses* k) 1) (< (* 2 i) k)))
      (when (<= k *n*)
	(setf (svref *inverses* k) i))
      (loop for cfm in (factor-table-row-factors *ft* m)
	    ;; unless (visited-p cfm) 
	    do (min-modular-inverses-semirec-1 i fi cfm)
	    ;; and do (set-visited cfm)
	    ))))

(defun min-modular-inverses-semirec (n)
  (make-inverses)
  (loop for i from 3 below n
	;; do (setf *visited-ht* (make-hash-table))
	do (setf *visited* (make-visited))
	do (loop for fi in (all-factors (1- i)) ;including 1
		 ;; do (setf *visited-ht* (make-hash-table))
		 ;; do (setf *visited* (make-visited))
		 do (min-modular-inverses-semirec-1 i fi (1+ i)))))
;;(time (min-modular-inverses-semirec 100))

(defun min-modular-inverses-rec-test (n)
  (min-modular-inverses-rec n)
  (let ((res-brute-force (min-modular-inverses n)))
    (loop for i to n
	  for mi-rec across *inverses*
	  for mi-brute-force in res-brute-force
	  if (not (zerop (- mi-rec mi-brute-force)))
	  collect (list i mi-rec mi-brute-force))))
;;(time (min-modular-inverses-rec-test 10000))
;;(time (min-modular-inverses-rec 10000))
;; This is tooooo slow. I revert to finding all factors for both and then 

;; (defun max-inverses (n) (loop for i from 3 to n collect (max-modular)))

(defun test-make-array (n)
  (loop repeat n do (make-visited))
  'qwe)
;;(time (test-make 100000))

(defun test-make-ht (n)
  (loop repeat n do (make-hash-table))
  'qwe)
;;(time (test-make-ht 10000000))

(defun desc-row-p (ft i)
  (apply #'>= (row-list ft i)))
;;(loop for i below (array-dimension *ft* 0) always (desc-row-p *ft* i))

(defun row-list (arr2 i)
  (loop for j below (array-dimension arr2 1)
	collect (aref arr2 i j)))
;;(row-list *ft* 3)

(defun min-modular-inverses-a-b (i a b)
  (let ((ab (* a b)))
    (when (or (>= ab *n*) (= (svref *inverses* ab) 1) (> ab 2))
      (when (and (< ab *n*) (= (svref *inverses* ab) 1))
	;; (print (list a b (svref *inverses* ab) i))
	(when (< (* 2 i) ab)
	  (setf (svref *inverses* ab) i)))
      (loop for f in (factor-table-row-factors *ft* b)
	    do (min-modular-inverses-a-b i a f)))))
;;(min-modular-inverses-a-b 3 2 4)

(defun min-modular-inverses-b (i n b)
  (let ((nb (* n b)))
    (when (or (>= nb *n*) (= (svref *inverses* nb) 1) (>= (svref *inverses* nb) i))
      (min-modular-inverses-a-b i n b)
      ;; (when (< nb *n*) (setf (svref *inverses* nb) i))
      (loop for f in (factor-table-row-factors *ft* n)
	    do (min-modular-inverses-b i f b)))))
;;(min-modular-inverses-b 8 7 9)
;;(time (setf *inverses* (make-array *n* :initial-element 1)))
;;(setf *visited* (make-visited))

(defun reset-visited ()
  (loop for i across *visited* do (setf (bit *visited* i) 0)))

(defun reset-inverses ()
  (loop for i across *inverses* do (setf (svref *inverses* i) 0)))

(defun min-modular-inverses-2 (i n m)
  (let ((nm (* n m)))
    (unless (or (and (< nm *n*)
		     (> (svref *inverses* nm) 1))
		(>= (* 2 i) nm))
      (loop for fn in (factor-table-row-factors *ft* n)
	    do (min-modular-inverses-2 i fn m))
      (loop for fm in (factor-table-row-factors *ft* m)
	    do (min-modular-inverses-2 i n fm)))
    (when (and (< nm *n*) (= (svref *inverses* nm) 1) (< (* 2 i) nm))
      (setf (svref *inverses* nm) i))))
;;(time (min-modular-inverses-rec 10))

(defun min-modular-inverses-rec (n)
  (setf *inverses* (make-inverses))
  (loop for i from 3 below n
	;; do (setf *visited* (make-visited))
	;; do (min-modular-inverses-2 i (1- i) (1+ i))))
	do (min-modular-inverses-2 i (1- i) (1+ i))))
;;(time (min-modular-inverses-rec 100))
;;(time (min-modular-inverses-rec *n*))
;;(time (min-modular-inverses-rec 10))

(defun min-modular-inverses (n)
  (setf *inverses* (make-inverses))
  (loop for i from 3 below n
	for fs- = (all-factors (1- i)) 
	for fs+ = (all-factors (1+ i)) 
	;; do (setf *visited* (make-visited))
	;; do (min-modular-inverses-2 i (1- i) (1+ i))))
	do (min-modular-inverses-2 i (1- i) (1+ i))))
;;(time (min-modular-inverses-rec 10))

(defun min-modular-inverses-single-seed (n)
  (setf *visited* (make-visited))
  (setf *inverses* (make-inverses))
  (min-modular-inverses-2 n (1- n) (1+ n)))
;;(min-modular-inverses-single-seed 5)

(defun inner-loop (i)
  (setf *inverses* (make-inverses))
  (let ((p-max (* i i)))
    (reset-visited)
    (loop for f- in (all-factors (1- i))
	  do (reset-visited)
	  do (loop for f+ in (all-factors (1+ i))
		   for p = (* f- f+)
		   do (princ p)
		   do (princ " ")
		   while (< p p-max)
		   if (and (= (svref *inverses* p) 1)
			   (> p (* 2 i)))
		   do (setf (svref *inverses* p) i)))))
;;(inner-loop 249)
