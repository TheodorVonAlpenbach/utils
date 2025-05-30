(require 'mb-lists)

(defconst 2pi (* 2 pi))

(cl-defun 2* (number) (* 2 number))
(cl-defun /2 (number) (/ number 2))
(cl-defun inv (number &optional float-p) (/ (if float-p 1.0 1) number))

(defmacro mincf (place &optional x)
  "Increment PLACE geometrically by X (2 by default).
PLACE may be a symbol, or any generalized variable allowed by `setf'.
The return value is the incremented value of PLACE."
  (declare (debug (place &optional form)))
  (if (symbolp place)
    (list 'setq place (if x (list '* place x) (list '2* place)))
    (list 'cl-callf '* place (or x 2))))
;;(let ((x 3)) (mincf x) x)

(cl-defun sq (x) (* x x))

(cl-defun between= (x a b)
  (and (<= a x) (<= x b)))
;;(between= 5 1 5)

(cl-defun modp (a b n)
  "Returns t iff x and y are identical modulo n"
  (= (mod a n) (mod b n)))
;;(modp 3 6 4)

(cl-defun modb (x n base)
  (+ (cl-mod (- x base) n) base))
;;(abs (modb 6 (* 2 pi) (- pi)))
;;(cl-loop for i from -4 to 4 collect (modb i 4 -2))

(cl-defun floor-to (arg divisor)
  (* divisor (first (cl-floor arg divisor))))
;;(floor-to 99 25)

(cl-defun ceiling-to (arg divisor)
  (* divisor (first (cl-ceiling arg divisor))))
;;(ceiling-to 99 25)

(cl-defun next-greater-multiple (x multiple &optional (n 1))
  (floor-to (+ x (* n multiple)) multiple))
;;(next-greater-multiple 100 25 1)

(cl-defun next-smaller-multiple (x multiple &optional (n 1))
  (ceiling-to (- x (* n multiple)) multiple))
;;(next-smaller-multiple 101 25 1)

(cl-defun sum (sequence &rest cl-keys)
  "Sum all elements in SEQUENCE.
Keywords supported: :operator :start :end :from-end
:initial-value :key. Default value for :operator is #'+. See
`cl-reduce' for a description of the other keywords"
  (let ((op (or (plist-pop cl-keys :operator) #'+)))
    (apply #'cl-reduce op sequence cl-keys)))
;;(sum '(1 2 3 4) :operator #'* :start 1 :initial-value 1)
;;(sum '(1 2 3))

(cl-defun Ln-sum (sequence order &rest cl-keys)
  "Return the Ln-sum of order ORDER for SEQUENCE. 
See `sum' for a descriptions of the keywords."
  (apply #'sum (mapcar (bind #'expt order) sequence) cl-keys))
;;(L-sum '(1 2 3) 2)

(cl-defun Ln-norm (sequence order &rest cl-keys)
  "Return the Ln-norm of order ORDER for SEQUENCE.
See `sum' for a descriptions of the keywords."
  (expt (apply #'Ln-sum sequence order cl-keys)
	(/ 1.0 order)))
;;(Ln-norm '(3 4) 2)

(cl-defun cumsum-list (list &optional (key #'+) (initial-value 0))
  "Return the cumulative sum of LIST.
Optional argument KEY specifies the operator and INITIAL-VALUE
the start value for the cumulation."
  (cl-loop for x in list
	   collect (setf initial-value (funcall key initial-value x))))
;;(cumsum-list (0-n 3))

(cl-defun cumsum (sequence &key (key #'+) (initial-value 0))
  "Return the cumulative sum of SEQUENCE.
For the key arguments, see `cumsum-list'."
  (as-list (l sequence) (cumsum-list l key initial-value)))
;;(cumsum (cl-coerce (0-n 3) 'vector))

(cl-defun product (sequence &key (key #'identity) (initial-value 1))
  (cl-reduce #'* sequence :key key :initial-value initial-value))
;;(product '(1 2 3) :initial-value 2)

(cl-defun product-safe-list (list &key (initial-value 1))
  "Return the product of LIST's elements,
converting to float if the product is large for an integer."
  (cl-loop with res = initial-value
	   for (first . rest) on list
	   for y = first then (* first res)
	   if (< y res) return (product list
				 :key #'float :initial-value initial-value)
	   else do (setf res y)
	   finally return res))
;;(product-safe-list (list 1 2 3))

(cl-defun product-safe (sequence &rest args)
  "Return the product of SEQUENCE's elements,
converting to float if the product is large for an integer."
  (apply #'product-safe-list (cl-coerce sequence 'list) args))
;;(product-safe (make-list 19 10))

(cl-defun product* (sequence &key (method :auto) (initial-value 1))
  (cl-case method
    (:auto (product-safe sequence :initial-value initial-value))
    (:float (product sequence :key #'float :initial-value initial-value))
    (:integer (product sequence :initial-value initial-value))
    (t (error "Unknown method %S" method))))
;;(product* (vector 1 2 3))

(cl-defun product-a-b (a b)
  (product (a-b a b)))
;;(product-a-b 2 4)

(defmacro avg (&rest args)
  `(/ (+ ,@args) ,(length args)))
;;(avg 1 2 3)

(cl-defun distance (x y)
  (abs (- x y)))
;;(distance 2 1)

(cl-defun average (list &key key)
  (/ (apply #'+ (if key (mapcar key list) list))
     (float (length list))))
;;(average (list 1 2 3) :key #'1+)

(defmacro negatef (n)
  "Negates N"
  `(setf ,n (- ,n)))
;;(let ((a (list 1))) (negatef (first a)) a)

(defmacro absf (n)
  "Makes N an absolute intege"
  `(setf ,n (abs ,n)))

(cl-defun plusp* (&rest factors)
  (cl-oddp (count-if #'plusp factors)))
;;(plusp* 1 -1)

(cl-defun minusp* (&rest factors)
  (cl-oddp (count-if #'minusp factors)))
;;(minusp* -1 -2 -3 4)

(cl-defun signum* (&rest factors)
  (apply #'* (mapcar #'cl-signum factors)))
;;(signum* -1 -2)

(cl-defun unsignum (&rest factors)
  (- (apply #'signum* factors)))

(cl-defun num-digits (n &optional (base 10))
  "Returns number of digits needed to represent N > 1 in N-ary format"
  (when (< n 1) (error "N must be a positive integer!"))
  (1+ (truncate (log n base))))
;;(num-digits 10 2) ==> 4 ("1010")

(cl-defun fixnum-floor (n m)
  (* m (first (floor* n m))))
;;(mapcar #'(lambda (x) (list x (fixnum-floor x 8))) (0-n 10))

(cl-defun fixnum-ceiling (n m)
  (* m (first (ceiling* n m))))
;;(mapcar #'(lambda (x) (list x (fixnum-ceiling x 8))) (0-n 10))

(cl-defun eq-parity (x y)
  (eq (cl-oddp x) (cl-oddp y)))
;;(eq-parity 2 'a)

;;chess math
(cl-defun elo-expected-score-ratio (elo1 elo2)
  (/ 1 (1+ (expt 10 (/ (- elo2 elo1) 400.0)))))

(cl-defun elo-expected-score (elo1 elo2 num-games)
  (* num-games (elo-expected-score-ratio elo1 elo2)))
;;(elo-expected-score 2713 2606 9)

(cl-defun elo-relative-performance (score-percentage)
  (* -400 (log (1- (/ 1.0 score-percentage)))))
;;(elo-relative-performance 1)

(cl-defun elo-performance (score-percentage elo-average)
  (+ elo-average (elo-relative-performance score-percentage)))
;;(elo-performance 1 (avg 2578 2435))
;;(+ 2558 748)

(cl-defun elo-coefficient-fide (elo &optional has-played-less-than-30-games)
  (if has-played-less-than-30-games
    25
    (if (< elo 2400) 15 10)))

(cl-defun elo-new (elo-old score num-games elo-average
		   &optional has-played-less-than-30-games)
  (+ elo-old (* (elo-coefficient-fide elo-old has-played-less-than-30-games)
		(- score (elo-expected-score elo-old elo-average num-games)))))
;;(elo-new 2713 5.5 9 2713)

(cl-defun celsius-to-fahrenheit (x &optional reverse)
  "Returns X degrees Celsius in Fahrenheit. If REVERSE is non-nil, the
reverse operation is performed. "
  (if reverse (* (- x 32) (/ 5 9.0)) (+ (* 1.8 x) 32)))
;;(celsius-to-fahrenheit 100 t)

(cl-defun pound-to-kg (x &optional reverse)
  "Returns X degrees Celsius in Fahrenheit. If REVERSE is non-nil, the
reverse operation is performed. "
  (if reverse
    (/ x 0.45359237)
    (* x 0.45359237)))
;;(mapcar #'pound-to-kg '(215 160 178))

(defconst +inch->cm+ 2.54)
(cl-defun foot-to-cm (feets inches)
  (* +inch->cm+ (+ (* feets 12) inches)))
;;(format "Leonardo was 194 cm high, while Michelangelo was just %s cm" (foot-to-cm 5 2))

(cl-defun personnummer-check (personnummer)
  "Checks that personnummer is valid according to its check digits
\(the last two digits\). The formulas are taken from
http://www.matematikk.org/pub/mattetekst/Persnr/."
  (let* ((parts (butlast (mapcar #'string-to-number
			   (apply #'split-at-position
			     personnummer (1-n 10))) 2))
	 (k1 (asetf (second (floor* (scalar-product
				     '(3 7 6 1 8 9 4 5 2) parts) 11)) 
		    (if (zerop it) 0 (- 11 it))))
	 (k2 (asetf (second (floor* (scalar-product
				     '(5 4 3 2 7 6 5 4 3 2)
				     (append parts (list k1))) 11))
		    (if (zerop it) 0 (- 11 it)))))
    (list (int-to-string (+ (* 10 k1) k2)) (substring personnummer 9))))
;;(personnummer-check "06017229573")

;;; Combinatorics:  head method is MERGE-N-LISTS
(cl-defun make-distributed-list (m n &optional (symbol-a '1) (symbol-b '0))
  "Returns a list where there are M occurrences of SYMBOL-A and N-M
occurrences of SYMBOL-B and the symbols are evenly distributed." 
  (cl-loop with denominator = 0
	   for i below n
	   for numerator = (* i m)
	   if (>= numerator denominator)
	   collect symbol-a and
	   do (cl-incf denominator n)
	   else
	   collect symbol-b))
;;(make-distributed-list 2 7) ==> '(1 0 0 0 1 0 0)
;;(make-distributed-list 3 7) ==> '(1 0 0 1 0 0 1 0)

(cl-defun merge-2-lists (list1 list2)
  "Merges contents of LIST1 and LIST2 and in an optimally distributed
way."
  (cl-loop with denominator = 0
	   with m = (length list1)
	   with n = (+ m (length list2 ))
	   for i below n
	   for numerator = (* i m)
	   if (>= numerator denominator)
	   collect (pop list1) and
	   do (cl-incf denominator n)
	   else
	   collect (pop list2)))
;;(merge-2-lists '(1 1 1) '(0 0))

(cl-defun merge-n-lists-sorted-by-length (lists)
  "Merges contents of the each list in LISTS and in an optimally distributed
way. Assumes that LISTS is sorted by the length of its elements (lists)."
  (if lists
    (if (rest lists)
      (merge-2-lists (first lists) (merge-n-lists (rest lists)))
      (first lists))))

(cl-defun merge-n-lists (lists)
  "Merges contents of the each list in LISTS and in an optimally distributed
way. Assumes that LISTS is sorted by the length of its elements (lists)."
  (merge-n-lists-sorted-by-length (sort* lists #'> :key #'length)))
;;(merge-n-lists (partition '(a b d a b c b a)))


(cl-defun extract (x list &key (test #'eq) (key #'identity))
  "Same as remove, but returns the extracted elements while LIST is altered"
  (let ((rest (cl-loop for elt in list
		       if (funcall test (funcall key elt) (funcall key x))
		       collect elt)))
    (setf list (delete* x list :test test :key key))
    rest))
;;(let ((list '(a b c d a))) (list (extract 'a list) list))

(cl-defun list-move (x list-from list-to &key (test #'eq) (key #'identity))
  "Not implemented"
  (o7tf))

(cl-defun partition-from-ordering (list &key (test #'<) (key #'identity))
  "Returns a partition of set LIST corresponding to the
equivalence relation based on the strict ordering TEST on LIST
\(Two elements x, y in LIST are equal iff neither x < y nor y <
x.)"
  (group (sort* list order-relation :key key)))
;;(partition-from-ordering '(a b d a b c b a) :order-relation #'symbol<)

(defmacro pushhash (key value hash-table)
  "Push VALUE on HASH-TABLE's KEY entry."
  `(puthash ,key (cons ,value (gethash ,key ,hash-table)) ,hash-table))

(cl-defun npartition (list &key (test #'eql) key)
  "Returns a partition of set LIST corresponding to the
equivalence relation TEST on LIST."
  (let ((ht (make-hash-table :test test)))
    (cl-loop with rlist = (reverse list) ; to preserve order inside classes
	     for x in rlist
	     for kx in (if key (mapcar key rlist) rlist)
	     do (pushhash kx x ht))
    (cl-loop for k being the hash-keys of ht using (hash-values v)
	     collect v)))
;;(npartition (0-n 10) :key (bind #'mod 3))

(cl-defun partition (sequence &rest args)
  "Return the list of equivalence classes in SEQUENCE.
The method regards the elements in SEQUENCE as an unordered set.
Each equivalence class is a sequence of the same type as SEQUENCE.
The equivalence relation is given by keyword :test.
\nKeywords supported:  :test :key
\n(fn SEQUENCE [KEYWORD VALUE]...)"
  (mapcar (bind #'cl-coerce (type-of sequence))
    (apply #'npartition (cl-coerce sequence 'list) args)))
;;(partition (cl-coerce '(a 1 b b "string" a) 'vector))
;;(partition (vector 'a 1 'b 'b "string" 'a))
;;(partition "abcdeafe")

;;(partition (0-n 10) #'(lambda (x y) (eq (cl-oddp x) (cl-oddp y))))
;;(partition (vector 1 2 3 4) #'(lambda (x y) (eq (cl-oddp x) (cl-oddp y))))

(cl-defun distribute (list &optional (test #'eq))
  (merge-n-lists (partition list test)))
;;(distribute '(a b c a d e f))

(cl-defun distribute-categories (distribution-list &optional categories-list)
  "Special application for Munkholmserien"
  (merge-n-lists 
   (cl-mapcar #'(lambda (n sym) (make-list n sym))
	    distribution-list
	    (or categories-list '(g h k n s d)))))
;;(prin1 (distribute-categories '(10 10 8 10 5 7)))

;;; cycles (copy to separate module)
(cl-defun cycle-rotate (cycle &optional (n 1))
  "Returns a rotated copy of CYCLE. Optional argument N specifies
the rotation degree."
  (rotate-list cycle n))
;;(cycle-rotate '(1 2 3))

(cl-defun cycle-rotate-until
    (cycle test &optional (max-number-of-rotations (length cycle)))
  "Returns a rotated copy of CYCLE. Optional argument N specifies
the rotation degree."
  (cl-loop for i below max-number-of-rotations
	   for rot = cycle then (cycle-rotate rot)
	   if (funcall test rot) return rot))
;;(cycle-rotate-until '(a b a a a) #'(lambda (x) (funcall 'neq (first x) (last-elt x))))

(cl-defun cycle-rotations (cycle)
  (cl-loop for x in cycle
	   for rot = cycle then (cycle-rotate rot)
	   collect rot))
;;(cycle-rotations '(a a b b a b a))

(cl-defun maprot (function &rest cycles)
  (cl-loop for i below (apply #'max (mapcar #'length cycles))
	   for args = cycles then (mapcar #'cycle-rotate args)
	   collect (apply function args)))
;;(maprot #'list '(1 2 3) '(1 2)) => (((1 2 3) (1 2)) ((2 3 1) (2 1)) ((2 3 1) (2 1)))

(cl-defun cycle-grouped-normal-form (list &optional (test #'eq))
  "One problem: the highest number of a's should come first"
  (let ((test test)) 
    (min-element
     (cycle-rotations
      (group (cycle-rotate-until 
	      list #'(lambda (x) (not (funcall test (first x) (last-elt x)))))
	:test test))
     :test #'(lambda (x y) (list> x y :key #'length)))))
;;(cycle-grouped-normal-form '(a a a b c d a e f))

(cl-defun cycle-normal-form (cycle &optional (test #'eq))
  (flatten (cycle-grouped-normal-form cycle test)))
;;(cycle-normal-form '(a a b b a b a))

(cl-defun 2cycle-lform (cycle &optional (test #'eq))
  (mapcar #'length (group cycle :test test)))
;;(2cycle-lform '(a b a a b b b a))

(cl-defun 2cycle-to-optimal-lform (cycle &optional (test #'eq))
  (let* ((n (cl-count (first cycle) cycle :test test)))
    (2cycle-lform (cycle-normal-form (make-distributed-list n (length cycle))))))
;;(2cycle-to-optimal-lform '(a a a b b a))

(cl-defun 2cycle-badness (cycle &optional (test #'eq))
  "Calculates how good the elements of CYCLE is distributed, assuming the number of non-EQ elements is 2"
  (let* ((n (length cycle))
	 (nfirst (cl-count (first cycle) cycle :test test))
	 (pos (positions (first cycle) cycle
			 :test (if (> (+ nfirst nfirst) n) 
				 (compose #'not test) test)))
	 (distances (cons (- (+ n (first pos)) (last-elt pos)) 
			  (mapcar (compose #'- (bind #'apply #'- 1))
			    (pairs pos))))
	 (optimal-distance (/ (float n) (length pos)))
	 (penalties (mapcar (compose #'sq (bind #'distance optimal-distance))
		      distances)))
    (if (> (length pos) 1) 
      (sum penalties)
      0)))
;;(2cycle-badness '(a a a a a b))

(cl-defun cycle-badness (cycle &optional (test #'eq))
  "Calculate how well the elements of CYCLE is distributed,
assuming the number of non-EQ elements is 2"
  (let ((test test))
    (cl-loop for elt in (cl-remove-duplicates cycle)
	     for badness = (2cycle-badness
			    cycle #'(lambda (x y) 
				      (xnor (funcall test elt x) 
					    (funcall test elt y))))))
  ;;(cycle-badness '(a b c a b c b a c a b c))

  (cl-defun cycle-best (cycles &optional (test #'eq))
    "Calculate the best distribution of the elements in CYCLE.
See `cycle-badness' for the measure of a good cycle."
    (cl-loop with min-cycle = (first cycles)
	     with min = (cycle-badness min-cycle test)
	     for c in cycles
	     for badness = min then (cycle-badness c test)
	     if (zerop badness) return c
	     if (< badness min) do (setf min badness min-cycle c)
	     finally return min-cycle))
  ;;(cycle-best '((a a b b) (a b a b)))

  (cl-defun distribute-rest (list prefix-cycle &optional (test #'eq))
    (let* ((test test))
      (cycle-best (mapcar (bind #'append prefix-cycle)
		    (cycle-rotations (distribute list test))))))
  ;;(cycle-badness (distribute-rest '(a c a b e b) '(a a b)))

  ;; energy
  (cl-defun p/th-to-NOK/MWh (p/th &optional (NOK/pound 11.5))
    (* p/th 0.01 NOK/pound 34.1))
  ;;(p/th-to-NOK/MWh 90)352.935

  ;; trondheim
  (cl-defun trondheim-percentage
      (n-games game-price n-qs-delivered received-price/q wanted-price/q)
    (/ (* n-qs-delivered (- wanted-price/q received-price/q))
       (* 1.0 n-games game-price)))
  ;;(trondheim-percentage 5000 500 1000 10 35)


  (defconst bilkollektivet-price-table 
    '((B 26 180 280 2.70 1.30)
      (C 28.50 200 300 2.90 1.50)
      (D 31.50 220 320 3.10 1.70))
    "Format: (type NOK/hour NOK/days� NOK/days� NOK/km� NOK/km�). where
�: 1-5 days, �: >6 days, �: 1-300 km, and �: >301 km")

  (cl-defun bilkollektivet-price-calculator (type num-km
					     &optional (days 0) (hours 0))
    ""
    (let* ((price-type (assoc type bilkollektivet-price-table))
	   (price-km (* num-km (if (< num-km 301)
				 (fifth price-type) (sixth price-type))))
	   (price-days (* days (if (< days 6)
				 (third price-type) (fourth price-type))))
	   (price-hours (* hours (second price-type))))
      (+ price-km price-days price-hours)))
  ;;(bilkollektivet-price-calculator 'C 2000 5)


;;; div
  (cl-defun is-divisible (n m)
    (zerop (mod n m)))
					;(mapcar #'(lambda (n) (is-divisible n 5)) (cl-loop for i below 11 collect i))

  (require 'mb-utils-10000-first-primes)
  (cl-defun primep (n)
    (not-null (cl-find n 10000-first-primes)))
  ;;(cl-remove-if nil (mapcar #'primep (1-n 20)))

  (cl-defun factorize (n &optional (primes 10000-first-primes))
    (let* ((max-prime (first (last primes)))
	   (max-argument (sq max-prime))
	   (factors '()))
      (when (> n max-argument)
	(error "Argument must be lower than %d" max-argument))
      (while (and primes 
		  (> n 1))
	(let ((p (pop primes)))
	  (while (is-divisible n p)
	    (push p factors)
	    (setq n (/ n p)))))
      (when (> n 1)
	(push n factors))
      factors))
  ;;(mapcar #'factorize (cl-loop for i from 2 to 100 collect i))
  ;;(mapcar #'factorize '(324 180))
  ;;(/ 180 36)
  ;;(apply #'* (factorize 1047300))
  ;;(/ 288 36)

  (cl-defun all-factors (n)
    (cl-sort (cl-remove-duplicates
		 (mapcar #'product (power-set (factorize n))))
      #'<)))
;;(all-factors 120)
;;(all-factors 284)

(cl-defun test-factorize (n)
  "Tests factorize for first N integers"
  (cl-loop for i from 2 to n
	   if (not (= i (apply #'* (factorize i))))
	   do (error "FACTORIZE failed for argument %d" i)))
					;(test-factorize 10000)

(cl-defun expand-factor (exponents primes)
  (product (cl-loop for p in primes
		    for e in exponents
		    collect (expt p e))))
;;(expand-factor '(1 2) '(3 2))

(cl-defun expand-factors (factors primes)
  (cl-loop for f in factors collect (expand-factor f primes)))
;;(expand-factors '((1 2) (0 0)) '(3 2))

;;; Number conversions
(cl-defun calculate-n-ary (root coefficients)
  "COEFFICIENTS is a list of integers a0, a1, a2... where ai < ROOT"
  (cl-loop for coefficient in coefficients
	   for i from 0
	   sum (* (expt root i) coefficient)))
;;(calculate-n-ary 128 (list 0 0 64))

(cl-defun int-to-hex (n &optional (length nil))
  (let ((format-string
	 (if length 
	   (concat "%" (format "0%dx" length))
	   "%x")))
    (format format-string n)))
;;(int-to-hex 10 10) ==> "000000000a"

(cl-defun int-to-bin-array (n)
  (let ((res ()))
    (while (> n 0)
      (push (logand 1 n) res)
      (setq n (lsh n -1)))
    res))
;;(int-to-bin-array 1235)

(cl-defun int-to-bin (n)
  (apply #'concat (mapcar #'int-to-string (int-to-bin-array n))))
;;(int-to-bin 10450)

(cl-defun uint-length-1 (n &optional (base 10))
  "Return the number of digits in N in the BASE-number system.
BASE is 10 by default."
  (cl-assert (not (minusp n)))
  (ceiling (log (1+ n) base)))

(cl-defun uint-length (n &optional (base 10))
  "Return the number of digits in non-negative integer N.
By default, the function assumes a decimal representation of N,
but you can change this with the optional argument BASE.

Also, N can be list of non-negative integers. In this case, the
function returns the greatest digit length of elements in N."
  (if (listp n)
    (cl-loop for i in n maximize (uint-length i base))
    (uint-length-1 n base)))
;;(uint-length (0-n 111))

(cl-defun uint-to-n-base
    (n &optional (base 10) (min-length (uint-length n base)))
  "Divide non-negative integer N into its digits.
By default the decimal system is used. But you can use an
arbitrary number base with optional argument BASE. The function
returns by default a minimum number of digits. With the optional
MIN-LENGTH you can force another length. The result is then
prefixed by zeros."
  (let ((res (cl-loop for i in (nreverse (0-n min-length))
		      collect (cl-mod (/ n (expt base i)) base))))
    (append (make-list (- min-length (length res)) 0)
	    res)))
;;(cl-loop for i in (a-b 99 101) collect (uint-to-n-base i))

(cl-defun bin-to-int-array (binary-string)
  (mapcar #'string-to-int (split-string binary-string "" t)))
;;(bin-to-int-array "01101")

(cl-defun byte-to-2hex (byte) (format "%02x" byte))

(cl-defun byte-to-char (byte) (format "%c" byte))
;;(mapcar #'byte-to-2hex (read-bytes-from-buffer midi-buffer))

(cl-defun bytes-to-string (bytes)
  "Converts a list of 8-bits bytes to a unibyte STRING.
TODO: handle multibyte strings."
  (apply #'concat (mapcar #'byte-to-char bytes)))
;;(bytes-to-string '(65 66 67 68))

(cl-defun string-to-bytes (string)
  "Converts a unibyte STRING to a list of 8-bits bytes. 
TODO: handle multibyte strings."
  (mapcar #'string-to-char (split-string string "" t)))
;;(string-to-bytes "ABCD") ==> (65 66 67 68)

(cl-defun nth-bit (integer n)
  (logand 1 (lsh integer (- n))))
;;(mapcar (bind #'nth-bit 16 1) (b-a 7 0))

(cl-defun int-to-n-bit-bytes
    (integer &optional (number-of-bits 8) (number-of-bytes nil))
  "Converts INTEGER to a list of NUMBER-OF-BITS-bit bytes.
If NUMBER-OF-BYTES is nil, the result contains only the needed
bytes. If non-nil the result is either truncated to
NUMBER-OF-BYTES, or prepended by zero bytes until a length of
NUMBER-OF-BYTES is reached."
  (if (zerop integer)
    (make-list (or number-of-bytes 1) 0)
    (let* ((number-of-total-bits (log (+ 1.0 integer) 2))
	   (number-of-total-bytes
	    (ceiling (/ number-of-total-bits number-of-bits)))
	   (mask (1- (expt 2 number-of-bits)))
	   (bytes
	    (reverse
	     (cl-loop for i below number-of-total-bytes
		      for integer-i = (lsh integer (- (* i number-of-bits)))
		      collect (logand integer-i mask)))))
      
      (if number-of-bytes
	(if (> (length bytes) number-of-bytes)
	  (nthcdr bytes (- (length bytes) number-of-bytes)) ;;truncate
	  (append (make-list (- number-of-bytes (length bytes)) 0) bytes))
	bytes))))
;;(int-to-n-bit-bytes 0 7)
;;(int-to-n-bit-bytes #x0FFFFFFF 7)
;;(mapcar #'int-to-n-bit-bytes '(1 255 256 258 512))

(cl-defun int-to-bytes (integer n)
  "Converts INTEGER to a list of N 8-bit bytes"
  (int-to-n-bit-bytes integer 8 n))
;;(int-to-bytes #xf 1)

(cl-defun bytes-to-int (bytes &key (byte-size 8) (endianness :big))
  "Calculates the integer represented by BYTES of bit length BYTE-SIZE."
  (calculate-n-ary
   (expt 2 8) (cl-case endianness
     ((:big) (nreverse bytes))
     ((:little) bytes)
     (t (error "%s is not a correct :ENDIANNESS value" :endianness)))))
;;(bytes-to-int (list 1 0) :endianness :little)
;;(bytes-to-int (list 1 0) :endianness :big)

(cl-defun int-to-variable-length-quantity (integer)
  "Converts iNTEGER to a variable length integer.
See http://en.wikipedia.org/wiki/Variable-length_quantity for a
definition."
  (let ((7-bit-bytes (int-to-n-bit-bytes integer 7)))
    (cl-loop for i below (1- (length 7-bit-bytes)) 
	     do (setf (nth i 7-bit-bytes) (logior (nth i 7-bit-bytes) #x80)))
    7-bit-bytes))
;;(int-to-variable-length-quantity #x0FFFFFFF)
;;(int-to-variable-length-quantity 0)

(cl-defun signed-byte-positive-p (signed-byte &optional (sign-bit-postition 8))
  (zerop (logand (lsh signed-byte (- 1 sign-bit-postition)) 1)))
;;(signed-byte-positive-p 255)

(cl-defun signed-byte-to-int (signed-byte &optional (sign-bit-postition 8))
  (if (signed-byte-positive-p signed-byte) 
    signed-byte (- signed-byte (expt 2 sign-bit-postition))))
;;(signed-byte-to-int 127)

(cl-defun int-to-signed-byte (integer &optional (number-of-bits 8))
  (let ((mask (expt 2 (1- number-of-bits))))
    (unless (< integer mask)
      (error "Integer %d is too big for specified byte size %d" integer number-of-bits))
    (if (< integer 0)
      (+ (* mask 2) integer) integer)))
;;(int-to-signed-byte -1)

(cl-defun 2s-complement (integer &optional (number-of-bits 16))
  (if (> integer (expt 2 (1- number-of-bits)))
    (- integer (expt 2 number-of-bits))
    integer))
;;(time (mapcar #'2s-complement (0-n (expt 2 8))))

(cl-defun 2s-complement-function (number-of-bits
				  &key (with-non-integer :error) inverse)
  "Returns a function that converts a number to its 2's complement
of bit length NUMBER-OF-BITS.
Optional argument WITH-NON-INTEGER controls the functions
behavior when the argument is not an integer"
  (let ((2^n (expt 2 number-of-bits))
	(2^n-1 (expt 2 (1- number-of-bits)))
	(with-non-integer with-non-integer)
	(inverse inverse))
    (lambda (x)
      (if (integerp x)
	(if inverse
	  (if (minusp x) (+ x 2^n) x)
	  (if (> x 2^n-1) (- x 2^n) x))
	(cl-case with-non-integer
	  (:error (error "Argument %S is not an integer" x))
	  (:leave x)
	  (:discard nil))))))
;;(time (mapcar (compose (2s-complement-function 16 :inverse t) (2s-complement-function 16)) (0-n (expt 2 8))))

(cl-defun split-byte (byte &optional (pos 4))
  "Splits BYTE at bit position POS"
  (let ((mask (1- (expt 2 pos))))
    (list (lsh byte (- pos))
	  (logand mask byte))))
;;(split-byte 255 0)

(cl-defun join-ints (a b &optional (bit-length-b 4))
  (+ (* a (expt 2 bit-length-b))
     b))
;;(join-ints 15 15) ==> 255 

;;;random numbers
(cl-defun random-float-base (&optional seed)
  "Return a random number in [0 1]."
  (when seed (random t))
  (/ (- (cl-coerce (random) 'float) most-negative-fixnum)
     (- (cl-coerce most-positive-fixnum 'float)
	(cl-coerce most-negative-fixnum 'float))))
;;(cl-loop for i below 100000 count (< (random-float-base) .1)) should -> .1

(cl-defun random-float (&optional (a 0.0) (b 1.0) seed)
  "Return a random number in [a b]."
  (+ (* (random-float-base seed) (- b a)) a))
;;(random-float 1 4)

(cl-defun random-integer (&optional (a 0) (b 1) seed)
  "Return a random integer in [a b]." 
  (when seed (random t))
  (+ (random (1+ (- b a))) 
     a))
;;(cl-loop for i below 100000 count (= (random-integer 1 3) 3))

(cl-defun random-unique-integers-n (n m &optional seed)
  "Return a list of N unique random numbers in [0 M>."
  (cl-assert (<= n m))
  (when seed (random t))
  (cl-loop with list = (0-n m) repeat n collect (draw-random list)))
;;(random-unique-integers-n 5 5)

(cl-defun random-unique-integers (n &optional (a 0) (b 1) seed)
  "Return a list of N unique random numbers in [0 M]."
  (mapcar (bind #'+ a) (random-unique-integers-n n (1+ (- b a)) seed)))
;;(random-unique-integers 6 10 15)

(cl-defun random-integers (n &optional (a 0) (b 1) seed)
  "Return a list of N random numbers in [a b]."
  (when seed (random t))
  (cl-loop repeat n collect (random-integer a b)))
;;(random-integers 10 1 3 t)

(cl-defun random* (&optional (a 0) (b 1) (integerp nil) seed)
  "Return a random number in the interval [a b]. If optional
INTEGERP is nil then the number can take every floating point
value in the specified interval. Else, it will be an integer."
  (if integerp
    (random-integer a b seed)
    (random-float a b seed)))

(cl-defun random-weighted-index (weights)
  (let ((cumulative-weights (cumsum-list weights)))
    (cl-position-if (bind #'>= (random-float 0 (last-elt cumulative-weights)))
	cumulative-weights)))
;;(random-weighted-index '(5 4 3 2 1))
;;(cumsum-list '(5 4 3 2 1))

(cl-defun random-weights-given-probabilities (n p &optional (num-draws 1))
  "Return weights so the first element has probability P to be drawn"
  (let ((w0 (expt p (/ 1.0 num-draws))))
    (cons w0 (mapcar (bind #'* (/ (* 2 (- 1 w0)) (1- n) n))
	       (nreverse (1-n (1- n)))))))
;;(random-weights-given-probabilities 5 .5)

(cl-defun random-weighted-element-1 (elements)
  (first (nth (random-weighted-index (project-sequence elements 1)) elements)))

(cl-defun random-weighted-element (elements &optional inverse)
  (random-weighted-element-1
   (if inverse (mapcol (bind #'inv t) 1 elements) elements)))

(cl-defun random-log-parameters (&optional (h 1) (m 0.1))
  ""
  (cl-assert (< 0 m h))
  (let* ((d (- (/ m h)))
	 (x (apply #'max (quadratic-root (1+ d) 1 d)))
	 (k (* 2 (log x)))
	 (ek (sq x))
	 (b (/ h (1+ ek)))
	 (a (- h b)))
    (list a b k)))
;;(random-log-parameters)

(cl-defun random-log-fun (&optional (h 1) (m 0.1) seed)
  ""
  (cl-destructuring-bind (a b k) (random-log-parameters h m)
    (let ((a a) (b b) (k k))
      (lambda (x)
	(+ a (* b (exp (* k x))))))))
;;(funcall (random-log-fun) 0)



;;(random-weighted-element '((a 1) (b 5) (c 10)) t)
;;(accumulate-list (cl-loop repeat 100 collect (random-weighted-element '((a 1) (b 5) (c 10)))) #'symbol<)

(cl-defun interval-floor (n interval)
  (* (floor n interval) interval))
;;(interval-floor 30 10)

;;pool utils
(cl-defun fractional-ball-angle (fraction
				 &optional with-object-ball-throw-correction)
  "Optional argument WITH-OBJECT-BALL-THROW-CORRECTION is not implemented"
  (cl-assert (between= fraction 0 1))
  (radians-to-degrees (asin (- 1 fraction))))
;;(fractional-ball-angle .5)


;;map utils
(cl-defun geo-distance (x y &optional (radius earth-radius-mean))
  "Calculates the distance between X and Y over a sphere surface.
Default sphere dimension corresponds to Earth's.
The algorithm used is the old Haversine formula, see
http://en.wikipedia.org/wiki/Haversine_formula

For a better distance approximation, use for instance Lambert's
formulae:
http://en.wikipedia.org/wiki/Geographical_distance#Lambert.27s_formulae"
  (let* ((diff-lat-rad (degrees-to-radians (- (first y) (first x))))
	 (diff-lon-rad (degrees-to-radians (- (second y) (second x))))
	 (x-lat-rad (degrees-to-radians (first x)))
	 (y-lat-rad (degrees-to-radians (first y)))
	 (a (+ (sq (sin (/ diff-lat-rad 2)))
	       (* (sq (sin (/ diff-lon-rad 2)))
		  (cos x-lat-rad)
		  (cos x-lat-rad))))
	 (c (* 2 (atan (sqrt a) (sqrt (- 1 a)))))
	 (d (* radius c)))
    d))
;;(geo-distance '(90.0 0.0) '(0.0 0.0))
;;(geo-distance '(59.925536 10.759177) '(59.925555 10.76113))

(cl-defun generate-addends-fixed (n k)
  "Returns a LIST of K numbers that adds to N, and so that the
numbers are `almost' equal (ie. (- (max LIST) (min LIST)) is
either 0 or 1)"
  (cl-destructuring-bind (q r) (cl-floor n k)
    (append (make-list r (1+ q))
	    (make-list (- k r) q))))
;;(generate-addends-fixed 271 3)

(cl-defun generate-addends (n)
  (if (zerop n)
    (list nil)		    ; resulting list consists of the empty set
    (cl-loop for i below n
	     append (mapcar (bind #'cons (- n i) 1) (generate-addends i)))))
;;(generate-addends 4) => ((4) (3 1) (2 2) (2 1 1) (1 3) (1 2 1) (1 1 2) (1 1 1 1))

(cl-defun aggregate (numbers &optional (init 0))
  (when numbers
    (let ((head (+ init (first numbers))))
      (cons head (aggregate (rest numbers) head)))))
;;(aggregate '(1 2 3 4))

(cl-defun normalize (numbers)
  (map (type-of numbers) (bind #'/ (float (sum numbers))) numbers))
;;(normalize (vector 1 2 3))

(cl-defun waverage (list)
  "Returns the Weighted average of the number and weight pairs in LIST: ((x1 w1) (x2 w2) ...)"
  (/ (sum (mapcar (bind #'apply #'* 1) list))
     (float (sum (mapcar #'second list)))))
;;uio grades
;;(waverage '((1 10) (1 10) (3 10) (1 10) (3 15) (4 10) (1 10) (1 5) (2 5) (2 10) (1 5) (1 5)))

(cl-defun hourly-from-yearly-salary (yearly-salary &optional (num-hours-per-year 1650) (arbeidsgiveravgift .14) (holiday-money .12) (company-costs .05))
  "For num-hours-per-year in Norway, see http://no.wikipedia.org/wiki/�rsverk.
"
  (let* ((hourly-salary-raw (/ (float yearly-salary) num-hours-per-year))
	 (hourly-salary-with-holiday-money (* hourly-salary-raw (1+ holiday-money)))
	 (hourly-salary-with-company-costs (* hourly-salary-with-holiday-money (1+ company-costs)))
	 (hourly-salary (* hourly-salary-with-company-costs (1+ arbeidsgiveravgift))))
    (list hourly-salary hourly-salary-with-company-costs hourly-salary-with-holiday-money hourly-salary-raw)))
;;(hourly-from-yearly-salary 567875)

(cl-defun quadratic-solver (a b c)
  (let ((r (/ (- b) (* 2.0 a)))
	(q (/ (sqrt (- (* b b) (* 4.0 a c)))
	      2.0)))
    (list (+ r q) (- r q))))
;;(quadratic-solver 1 1 -6)

(cl-defun ceilx*x-solver (y)
  "Assume y and solution is positive"
  (let* ((min (first (quadratic-solver 1 1 (- y))))
	 (max (first (quadratic-solver 1 0 (- y))))
	 (range (mapcar 'float (a-b (ceiling min) (ceiling max))))
	 (solution-candidates (mapcar (bind #'/ y 1) range)))
    (cl-find y solution-candidates :test #'= :key #'(lambda (x) (* (ceiling x) x)))))
;;(ceilx*x-solver 3.1)

;; financial
(cl-defun interest-annual (x interest year)
  (* x (expt (1+ interest) years)))
;;(interest-annual 1000 .06 5)

(cl-defun naaverdi-annual (x interest years)
  (/ x (expt (1+ interest) years)))

(cl-defun obligation-value (x yield years market-interest)
  (+ (naaverdi-annual x market-interest years)
     (cl-loop with premium = (* x yield)
	      for i below years
	      sum (naaverdi-annual premium market-interest i))))
;;(obligation-value 1000 .06 5 0.04)

;; swimming!
(defconst swimming-records
  '((46.91 crawl)
    (49.82 butterfly)
    (51.94 backstroke)
    (58.46 breaststroke)))

(cl-defun swimming-relative-distances
    (&optional (style-symbol 'crawl) (distance 100))
  (let ((style (cl-find style-symbol swimming-records :key #'second)))))
;;(swimming-relative-distances 'crawl 50)

;;; div js dates (TODO move this later)
;; This buffer is for notes you don't want to save, and for Lisp evaluation.
;; If you want to create a file, visit that file with C-x C-f,
;; then enter the text in that file's own buffer.

;;;;
(cl-defun div-sec-min (seconds) (floor* seconds 60.0))
(cl-defun div-min-hour (minutes) (floor* minutes 60.0))
(cl-defun div-hour-day (hours) (floor* hours 24.0))
;;(div-sec-min 72)

(cl-defun leap-year-p (year)
  (and (modp year 0 4)
       (or (modp year 0 400)
	   (not (modp year 0 100)))))
;;(mapcar #'leap-year-p '(1600 1700 1800 1900 2000 2001 2002 2003 2004))

(cl-defun days-in-year (year)
  (if (leap-year-p year) 366 365))
;;(mapcar #'days-in-year '(1600 1700 1800 1900 2000 2001 2002 2003 2004))

(cl-defun days-between-years (year1 year2)
  (cl-loop for y from year1 below year2 
	   sum (days-in-year y)))
;;(days-between-years 1970 1971)

(cl-defun current-year () (sixth (decode-time)))

(cl-defun days-after-year (&optional (year1 1970) (year2 (current-year)))
  (sum (mapcar #'days-in-year (a-b year1 year2))))
;;(days-after-year 2012)

(cl-defun div-day-year (days &optional (base-year 1970))
  (let* ((days-list (days-after-year base-year))
	 (pos (position days days-list :test #'<))
	 (val (if (zerop pos) 
		0 (nth (1- pos) days-list))))
    (list (+ base-year pos) (- days val))))
;;(div-day-year 3000)

(cl-defun days-in-month (month &optional (year (current-year)))
  (if (and (= month 1) (leap-year-p year))
    29 (nth month '(31 28 31 30 31 30 31 31 30 31 30 31))))
;;(mapcar (bind #'days-in-month 2001) (0-n 12))

(cl-defun div-day-month (days &optional (year (current-year)))
  (let* ((days-list (sum (mapcar (bind #'days-in-month year) (0-n 12))))
	 (pos (position days days-list :test #'<))
	 (val (if (zerop pos) 
		0 (nth (1- pos) days-list))))
    (list pos (- days val))))
;;(div-day-month 31)

(cl-defun jstime (seconds &optional (unix-base-year 1970))
  (let* ((minutes (div-sec-min seconds))
	 (hours (div-min-hour (first minutes)))
	 (days (div-hour-day (first hours)))
	 (years (div-day-year (first days)))
	 (months (div-day-month (second years))))
    (mapcar #'round (list (first years) (first months) (second years) 
			  (second days) (second hours) (second minutes)))))
;;(jstime 1199290500)
;;(floor* 1199290500 60.0)

(cl-defun linear-function-slope-point (slope x)
  "Returns a linear function with SLOPE going through point X.
The point is represented as a pair (X0 X1)."
  (let* ((x0 (first x))
		 (x1 (second x))
		 (a slope)
		 (b (- x1 (* a x0))))
    #'(lambda (x) (+ (* a x) b))))
;;(mapcar (linear-function-slope-point 2 '(1 2)) (a-b -3 3))

(cl-defun linear-function-point-to-point (x y)
  "Returns a linear function going through points X and Y. Both
  poings are represented as a pair of numbers. See
  `linear-function-slope-point'"
  (linear-function-slope-point (/ (float (- (second y) (second x)))
				  (float (- (first y) (first x))))
			       x))
;;(mapcar (linear-function-point-to-point '(0 0) '(1 2)) (a-b -3 3))

(cl-defun linear-function (slope-or-point point)
  (if (consp slope-or-point)
    (linear-function-point-to-point slope-or-point point)
    (linear-function-slope-point slope-or-point point)))
;;(mapcar (linear-function '(0 0) '(1 2)) (a-b -3 3))

(cl-defun nth-digit (number k &optional (base 10))
  "Returns the Kth digit of NUMBER. The return value is an integer symbol"
  (cl-assert (< k (log number base)))
  (floor (mod number (expt base (1+ k)))
	 (expt base k)))
;;(nth-digit 123 2)

(cl-defun digit-sum-1 (n &optional (base 10))
  "Inner sum function for `digit-sum'" 
  (cl-loop for i below (log n base)
	sum (nth-digit n i base)))

(cl-defun digit-sum (n &optional (base 10))
  "Returns the decimal digit sum of integer N" 
  (cl-loop for n* = n then res
	   for res = (digit-sum-1 n* base) 
	   until (< res base)
	   finally return res))
;;(digit-sum 123 2)

(cl-defun tall-navn (n &optional intetkj�nn-p)
  (cond ((= n 0) "null")
	((= n 1) (if intetkj�nn-p "ett" "en"))
	((< n 20) 
	 (symbol-name (nth (- n 2) '(to tre fire fem seks sju �tte ni ti elleve tolv tretten fjorten femten seksten sytten atten nitten))))
	((and (zerop (mod n 10))
	      (<= n 100))
	 (symbol-name (nth (- (/ n 10) 2) '(tjue tretti f�rti femti seksti sytti �tti nitti hundre))))
	((< n 100) (concat (tall-navn (* 10 (floor n 10))) (tall-navn (mod n 10))))
	((= n 100) "ett hundre")
	((< n 1000) (concat (tall-navn (floor n 100) t) " hundre og " (tall-navn (mod n 100))))
	((= n 1000) "ett tusen")
	((< n 1000000) (concat (tall-navn (floor n 1000) t) " tusen " (tall-navn (mod n 1000))))
	((= n 1000000) "en million")
	((< n 2000000) (concat (tall-navn (floor n 1000000)) " million " (tall-navn (mod n 1000000))))
	((< n 1000000000) (concat (tall-navn (floor n 1000000)) " millioner " (tall-navn (mod n 1000000))))
	((= n 1000000000) "milliard")
	(t (error))))
;;(tall-navn 221134923)

(cl-defun extended-gcd (a b)
  (let* ((s 0) (old-s 1)
	 (t* 1) (old-t* 0)
	 (r b) (old-r a)
	 quotient)
    (while (not (zerop r))
      (setf quotient (/ old-r r))
      (setf tmp r)
      (setf r (- old-r (* quotient tmp)))
      (setf old-r tmp)

      (setf tmp s)
      (setf s (- old-s (* quotient tmp)))
      (setf old-s tmp)

      (setf tmp t*)
      (setf t* (- old-t* (* quotient tmp)))
      (setf old-t* tmp))
    (message "B�zout coefficients: (%d, %d)
greatest common divisor: %d
quotients by the gcd: (%d, %d)"
	     old-s old-t* old-r t* s)
    (mod old-s b)))
;;(extended-gcd -7 -11)
;;(extended-gcd 19 12)
;;(/ (- 3 (* 2 (log 3 2))) (- (* 7 (log 3 2)) 11))
;;  

(cl-defun floor-test1 (k &optional (max-m k))
  "Tests that F(mlog3) = F(m(F(klog3))/k), for all m and k"
  (let ((log3 (log 3 2)))
    (cl-loop for m to max-m
	  for a = (floor (* m log3))
	  for b = (floor (/ (* m (floor (* k log3))) k))
	  always (= a b))))

(cl-defun floor-test (k &optional (max-m k))
  "Tests that F(mlog3) = F(m(F(klog3))/k), for all m and k"
  (let ((log3 (log 3 2)))
    (cl-loop for m to max-m
	     for a = (floor (* m k log3))
	     for b = (* m (floor (* k log3)))
	     always (= a b))))
;;(mapcar #'floor-test (a-b 20 30))
;;(cl-loop for i below 1000 if (floor-test i) collect i)(0 1 2 7 12 24 53 106 359 665)
;;(mapcar #'floor-test (list 7 12 24 36 48 63))
;;(list (log 3 2) (/ 11 7.0))

(cl-defun fractional (x) (- (float x) (floor x)))
;;(fractional 3.4)

(cl-defun test-fractional (k) (< (fractional (* k (log 3 2))) (/ 1.0 k)))
;;(cl-loop for i from 1 below 1000 if (test i) collect (list i (l-value i) (gcd i (l-value i))))

(cl-defun floor-test2 (max-k m)
  "Tests that F(mlog3) = F(m(F(klog3))/k), for all m and k"
  (let ((log3 (log 3 2)))
    (cl-loop for k from 1 to max-k collect
	     (list k
		   (floor (* m log3))
		   (floor (/ (* m (floor (* k log3))) k))))))
;;(floor-test2 7 1)
;;(floor (* 28 (log 3 2)))
;;(list (log 3 2) (/ 11 7.0))
;;(cl-loop for i to 5 collect (floor (* i (log 3 2))))
;;(cl-loop for i below 30 collect  (list i (floor (* i (log 3 2)))))

(cl-defun l-value (k) (floor (* k (log 3 2))))
;;(mapcar #'l-value (a-b 1 12))
;;(cl-loop for k from 1 to 12 collect (* (float k) (fractional (* k (log 3 2)))))

(cl-defun l-value-test (s &optional (k 12))
  (let ((l (l-value k))
	(l-inverse (extended-gcd (l-value k) k)))
    (list s
	  (* l-inverse s)
	  (mod (* l-inverse s) k)
	  (* l (mod (* l-inverse s) k))
	  (first (cl-floor (* l (mod (* l-inverse s) k)) k)))))
;;(cl-loop for s below 10 collect (test3 s))

(cl-defun factors (n)
  (cl-loop for i from 1 to (/ n 2)
	   if (zerop (mod n i))
	   collect i))
;;(factors 10)

(cl-defun perfect-number-p (n)
  (= (sum (factors n)) n))
;;(perfect-number-p 28)

(cl-defun perfect-numbers (&optional (n 10000))
  (cl-loop for i from 2 to n
	   if (perfect-number-p i) collect i))
;;(perfect-numbers 10000)

(cl-defun integer-ceiling (p q)
  "Return the ceiling of P/Q, where P and Q are positive integers.
This is an effective implementation of (cl-coerce (ceiling (/ (float
p) q)))"
  (/ (+ p q -1) q))
;;(integer-ceiling 10 3)

(cl-defun test-integer-ceiling (n &optional (max-integer 1000000))
  "Test `integer-ceiling'"
  (cl-loop repeat n
	   for (p q) = (random-integers 2 1 max-integer)
	   if (/= (cl-coerce (ceiling (/ (float p) q)) 'integer)
		  (integer-ceiling p q))
	   collect (list p q)))
;;(test-integer-ceiling 100000)

(cl-defun test-another-integer-operation (n &optional (max-integer 1000000))
  (cl-loop repeat n
	   for (p q r) = (random-integers 3 1 max-integer)
	   for res1 = (cl-coerce (floor (- (float r) (/ (float p) q))) 'integer)
	   for res2 = (- r (integer-ceiling p q))
	   if (/= res1 res2) collect (list r p q res1 res2)))
;;(test-another-integer-operation 100000)

(provide 'mb-utils-math)

(cl-defun problem (n)
  (/ (cl-loop with primes = (subseq 10000-first-primes 0 15)
	      repeat n
	      for s = (+ (random-integer 1 50) (random-integer 1 50))
	      count (and (cl-find s primes) (< s 50)))
     (float n)))
;;(* (problem 1000000) (/ 2500 313.0))

(cl-defun approximate-frequency (f &optional (eps 0.1) (max-m 100))
  "Return \(N M f_nm\), f_nm = 2^N * 3^M, for which |f_nm - f| < EPS.
If no such triple can be found for M < MAX-M, the function
instead returns nil.

TODO: move this to some yamal elisp module."
  (cl-flet ((fn (f eps m) (- (log (+ f eps) 2) (* m (log 3.0 2))))
	    (fz (n m) (expt 2 (+ n (* m (log 3.0 2))))))
    (cl-loop for m from 73 below max-m
	     for n-min = (ceiling (fn f (- eps) m))
	     for n-max = (floor (fn f eps m))
	     for n-best = (if (< (abs n-min) (abs n-max)) n-min n-max)
	     unless (< n-max n-min) return (list n-min n-max m (fz n-min m))
	     for nn-min = (ceiling (fn f (- eps) (- m)))
	     for nn-max = (floor (fn f eps (- m)))
	     unless (< nn-max nn-min)
	     return (list nn-min (- m) (fz nn-min (- m))))))
;;(approximate-frequency (* 100 pi) 1)

(cl-defun volume-ellipsoid (r1 &optional (r2 r1) (r3 r2))
  (/ (* 4 pi r1 r2 r3) 3))
;; Volume Himalia (Jupiter moon)
;;(volume-ellipsoid (/ 170 2))                     2572440.7845144426
;; Volume Amalthea (Jupiter moon) as an ellipsoid
;;(volume-ellipsoid (/ 250 2) (/ 146 2) (/ 128 2)) 2446253.479595252
;; Volume Amalthea as a prism
;;(* 250 146 128)                                  4672000
;; Volume Amalthea according to Wikipedia          2430000 (assuming that it is a true ellipsoid?)

(cl-defun area-secant (theta &optional (radius 1))
  (* (/ (sq radius) 2.0) (- theta (sin theta))))

(provide 'mb-utils-math)
