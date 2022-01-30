(require '10000-first-primes "~/git/utils/elisp/div/euler-project/10000-first-primes.lisp")

(defun number-of-factor-table-columns (n &optional (primes 10000-first-primes))
  (let ((product 1))
    (position-if #'(lambda (x) (> (setf product (* x product)) n)) primes)) )
;;(number-of-factor-table-columns 100)
;;(subseq 10000-first-primes 0 3)

(defun factor-table-available-column-index (ft i)
  (let ((m (array-dimension ft 1)))
    (loop for j below m
	  if (< (aref ft i j) 2) return j)))
;;(loop for i below 20 collect (factor-table-available-column-index *ft* i))

(defun remake-factor-table (ft &optional (primes 10000-first-primes))
  (loop with n = (array-dimension ft 0)
	for p in primes
	while (<= p n) do
	(loop for i from 2
	      for ip = (* i p)
	      while (< ip n) do
	      (setf (aref ft ip (factor-table-available-column-index ft ip)) i)))
  ft)
;;(setf *ft* (remake-factor-table *ft*))

(defun make-factor-table (n &optional factor-table (primes 10000-first-primes))
  (let ((ft (or factor-table
		(make-array (list n (number-of-factor-table-columns n primes))
		  :element-type 'integer :initial-element 0))))
    (remake-factor-table ft primes)))
;;(make-factor-table 100 *ft*)

(defun factor-table-row-factors (ft i)
  (let ((m (array-dimension ft 1)))
    (loop for j below m
	  for f = (aref ft i j)
	  while (> f 1) unless (visited-p f) collect f)))
;;(factor-table-row-factors *ft* 7920)

(defun factor-table-row-factors-raw (ft i)
  (let ((m (array-dimension ft 1)))
    (loop for j below m
	  for f = (aref ft i j)
	  while (> f 1) collect f)))

(defun ft-count-columns (ft)
  (destructuring-bind (n m) (array-dimensions ft)
    (loop for j below m collect
	  (loop for i below n count (> (aref ft i j) 1)))))
;;(ft-count-columns *ft*)

(defun ft-count-zeros (ft)
  (destructuring-bind (n m) (array-dimensions ft)
    (let ((z (loop for j below m sum
		   (loop for i below n count (zerop (aref ft i j)))))
	  (n (apply #'* (array-dimensions ft))))
      (list n z (coerce (/ z n) 'float)))))
;;(ft-count-zeros *ft*)

;;; visited
(defun make-visited (n)
  (make-array (+ 2 n) :element-type 'bit))

(defparameter *visited* (make-visited 0))

(defun reset-visited (&optional (n *n*))
  (setf *visited* (make-visited n))
  (length *visited*))

(defun visited-p (x) (= (bit *visited* x) 1))
(defun unvisited-p (x) (= (bit *visited* x) 0))
(defun set-visited (x) (setf (bit *visited* x) 1))

(defun all-factors-1 (n)
  (unless (visited-p n)
    (set-visited n)
    (cons n (loop for f in (factor-table-row-factors *ft* n)
		  append (all-factors-1 f)))))

(defun all-factors (n &optional reset-visisted-p)
  (when reset-visisted-p
    (reset-visisted n)
    (all-factors-1 n)))
;;(all-factors 4)
;;(all-factors 80)

(provide 'factor-table)
