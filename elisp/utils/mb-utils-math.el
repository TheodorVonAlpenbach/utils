(defun sq (x) (* x x))

(defun between= (x a b)
  (and (<= a x) (<= x b)))
;;(between= 5 1 5)

(defun modp (a b n)
  "Returns t iff x and y are identical modulo n"
  (= (mod a n) (mod b n)))
;;(modp 3 6 4)

(defmacro avg (&rest args)
  `(/ (+ ,@args) ,(length args)))
;;(avg 1 2 3)

(defun distance (x y)
  (abs (- x y)))
;;(distance 2 1)

(defun average (list &key key)
  (/ (apply #'+ (if key (mapcar key list) list))
     (length list)))
;;(average (list 1 2 3) :key #'1+)

(defmacro negatef (n)
  "Negates N"
  `(setf ,n (- ,n)))
;;(let ((a (list 1))) (negatef (first a)) a)

(defmacro absf (n)
  "Makes N an absolute intege"
  `(setf ,n (abs ,n)))

(cl-defun plusp* (&rest factors)
  (oddp (count-if #'plusp factors)))
;;(plusp* 1 -1)

(cl-defun minusp* (&rest factors)
  (oddp (count-if #'minusp factors)))
;;(minusp* -1 -2 -3 4)

(cl-defun signum* (&rest factors)
  (apply #'* (mapcar #'signum factors)))
;;(signum* -1 -2)

(cl-defun num-digits (n &optional (base 10))
  "Returns number of digits needed to represent N > 1 in N-ary format"
  (when (< n 1) (error "N must be a positive integer!"))
  (1+ (truncate (log n base))))
;;(num-digits 10 2) ==> 4 ("1010")

(defun fixnum-floor (n m)
  (* m (first (floor* n m))))
;;(mapcar #'(lambda (x) (list x (fixnum-floor x 8))) (0-n 10))

(defun fixnum-ceiling (n m)
  (* m (first (ceiling* n m))))
;;(mapcar #'(lambda (x) (list x (fixnum-ceiling x 8))) (0-n 10))

(defun eq-parity (x y)
  (eq (oddp x) (oddp y)))
;;(eq-parity 2 'a)

;;chess math
(defun elo-expected-score-ratio (elo1 elo2)
  (/ 1 (1+ (expt 10 (/ (- elo2 elo1) 400.0)))))

(defun elo-expected-score (elo1 elo2 num-games)
  (* num-games (elo-expected-score-ratio elo1 elo2)))
;;(elo-expected-score 2713 2606 9)

(cl-defun elo-relative-performance (score-percentage)
  (* -400 (log (1- (/ 1.0 score-percentage)))))
;;(elo-relative-performance 1)

(defun elo-performance (score-percentage elo-average)
  (+ elo-average (elo-relative-performance score-percentage)))
;;(elo-performance 1 (avg 2578 2435))
;;(+ 2558 748)

(defun elo-coefficient-fide (elo &optional has-played-less-than-30-games)
  (if has-played-less-than-30-games
    25
    (if (< elo 2400) 15 10)))

(defun elo-new (elo-old score num-games elo-average &optional has-played-less-than-30-games)
  (+ elo-old (* (elo-coefficient-fide elo-old has-played-less-than-30-games)
		(- score (elo-expected-score elo-old elo-average num-games)))))
;;(elo-new 2713 5.5 9 2713)

(defun celsius-to-fahrenheit (x &optional reverse)
  "Returns X degrees Celsius in Fahrenheit. If REVERSE is non-nil, the
reverse operation is performed. "
  (if reverse (* (- x 32) (/ 5 9.0)) (+ (* 1.8 x) 32)))
;;(celsius-to-fahrenheit 10)

(defun pound-to-kg (x &optional reverse)
  "Returns X degrees Celsius in Fahrenheit. If REVERSE is non-nil, the
reverse operation is performed. "
  (if reverse
    (/ x 0.45359237)
    (* x 0.45359237)))
;;(mapcar #'pound-to-kg '(215 160 178))

(defconst +inch->cm+ 2.54)
(defun foot-to-cm (feets inches)
  (* +inch->cm+ (+ (* feets 12) inches)))
;;(format "Leonardo was 194 cm high, while Michelangelo was just %s cm" (foot-to-cm 5 2))

(defun personnummer-check (personnummer)
  "Checks that personnummer is valid according to its check digits
\(the last two digits\). The formulas are taken from
http://www.matematikk.org/pub/mattetekst/Persnr/."
  (let* ((parts (butlast (mapcar #'string-to-number (split-string-at-pos personnummer 1 2 3 4 5 6 7 8 9 10)) 2))
	 (k1 (asetf (second (floor* (scalar-product '(3 7 6 1 8 9 4 5 2) parts) 11)) 
	       (if (zerop it) 0 (- 11 it))))
	 (k2 (asetf (second (floor* (scalar-product '(5 4 3 2 7 6 5 4 3 2) (append parts (list k1))) 11))
	       (if (zerop it) 0 (- 11 it)))))
    (list (int-to-string (+ (* 10 k1) k2)) (substring personnummer 9))))
;;(personnummer-check "06017229573")

;;; Combinatorics:  head method is MERGE-N-LISTS
(cl-defun make-distributed-list (m n &optional (symbol-a '1) (symbol-b '0))
  "Returns a list where there are M occurrences of SYMBOL-A and N-M
occurrences of SYMBOL-B and the symbols are evenly distributed." 
  (loop with denominator = 0
        for i below n
	for numerator = (* i m)
	if (>= numerator denominator)
	  collect symbol-a and
	  do (incf denominator n)
	else
	  collect symbol-b))
;;(make-distributed-list 2 7) ==> '(1 0 0 0 1 0 0)
;;(make-distributed-list 3 7) ==> '(1 0 0 1 0 0 1 0)

(defun merge-2-lists (list1 list2)
  "Merges contents of LIST1 and LIST2 and in an optimally distributed
way."
  (loop with denominator = 0
	with m = (length list1)
	with n = (+ m (length list2 ))
	for i below n
	for numerator = (* i m)
	if (>= numerator denominator)
	  collect (pop list1) and
	  do (incf denominator n)
	else
	  collect (pop list2)))
;;(merge-2-lists '(1 1 1) '(0 0))

(defun merge-n-lists-sorted-by-length (lists)
  "Merges contents of the each list in LISTS and in an optimally distributed
way. Assumes that LISTS is sorted by the length of its elements (lists)."
  (if lists
    (if (rest lists)
      (merge-2-lists (first lists) (merge-n-lists (rest lists)))
      (first lists))))

(defun merge-n-lists (lists)
  "Merges contents of the each list in LISTS and in an optimally distributed
way. Assumes that LISTS is sorted by the length of its elements (lists)."
  (merge-n-lists-sorted-by-length (sort* lists #'> :key #'length)))
;;(merge-n-lists (partition '(a b d a b c b a)))


(cl-defun extract (x list &key (test #'eq) (key #'identity))
  "Same as remove, but returns the extracted elements while LIST is altered"
  (let ((rest (loop for elt in list
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

(cl-defun npartition (list &key (test #'eql) key)
  "Returns a partition of set LIST corresponding to the
EQUIVALENCE-RELATION on LIST. "
  (let ((res ())) 
    (while list
      (push (draw-if (bind test (if key (funcall key (car list)) (car list)))
		     list :key key)
	    res))
    (nreverse res)))

(cl-defun npartition (list &key (test #'eql) key)
  "Returns a partition of set LIST corresponding to the
EQUIVALENCE-RELATION on LIST. "
  (loop while list collect
	(draw-if (bind test (if key (funcall key (car list)) (car list))) list
		 :key key)))
;;(npartition '(a b e c d e))

(cl-defun partition (sequence &optional (equivalence-relation #'eql))
  "Returns a partition of set LIST corresponding to the
EQUIVALENCE-RELATION on LIST"
  (coerce (npartition (coerce sequence 'list) equivalence-relation)
	  (type-of sequence)))
;;(partition (0-n 10) #'(lambda (x y) (eq (oddp x) (oddp y))))
;;(partition (vector 1 2 3 4) #'(lambda (x y) (eq (oddp x) (oddp y))))

(cl-defun distribute (list &optional (test #'eq))
  (merge-n-lists (partition list test)))
;;(distribute '(a b c a d e f))

(defun distribute-categories (distribution-list &optional categories-list)
  "Special application for Munkholmserien"
  (merge-n-lists 
   (mapcar* #'(lambda (n sym) (make-list n sym))
	    distribution-list
	    (or categories-list '(g h k n s d)))))
;;(prin1 (distribute-categories '(10 10 8 10 5 7)))

;;; cycles (copy to separate module)
(cl-defun cycle-rotate (cycle &optional (n 1))
  "Returns a rotated copy of CYCLE. Optional argument N specifies
the rotation degree."
  (rotate-list cycle n))
;;(cycle-rotate '(1 2 3))

(cl-defun cycle-rotate-until (cycle test &optional (max-number-of-rotations (length cycle)))
  "Returns a rotated copy of CYCLE. Optional argument N specifies
the rotation degree."
  (loop for i below max-number-of-rotations
	for rot = cycle then (cycle-rotate rot)
	if (funcall test rot) return rot))
;;(cycle-rotate-until '(a b a a a) #'(lambda (x) (funcall 'neq (first x) (last-elt x))))

(defun cycle-rotations (cycle)
  (loop for x in cycle
	for rot = cycle then (cycle-rotate rot)
	collect rot))
;;(cycle-rotations '(a a b b a b a))

(defun maprot (function &rest cycles)
  (loop for i below (apply #'max (mapcar #'length cycles))
	for args = cycles then (mapcar #'cycle-rotate args)
	collect (apply function args)))
;;(maprot #'list '(1 2 3) '(1 2)) => (((1 2 3) (1 2)) ((2 3 1) (2 1)) ((2 3 1) (2 1)))

(cl-defun cycle-grouped-normal-form (list &optional (test #'eq))
  "One problem: the highest number of a's should come first"
  (lexical-let ((test test)) 
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
  (let* ((n (count (first cycle) cycle :test test)))
    (2cycle-lform (cycle-normal-form (make-distributed-list n (length cycle))))))
;;(2cycle-to-optimal-lform '(a a a b b a))

(cl-defun 2cycle-badness-old (cycle &optional (test #'eq))
  "Calculates how good the elements of CYCLE is distributed, assuming the number of non-EQ elements is 2"
  (let* ((lform (2cycle-lform (cycle-normal-form cycle test) test))
	 (loptimal-lform (2cycle-to-optimal-lform cycle test)))
    (sum (mapcar* #'distance lform loptimal-lform))))
;;(2cycle-badness-old '(a a a a a b b))

(cl-defun 2cycle-badness (cycle &optional (test #'eq))
  "Calculates how good the elements of CYCLE is distributed, assuming the number of non-EQ elements is 2"
  (let* ((n (length cycle))
	 (nfirst (count (first cycle) cycle :test test))
	 (pos (positions (first cycle) cycle :test (if (> (+ nfirst nfirst) n) 
						     (compose #'not test) test)))
	 (distances (cons (- (+ n (first pos)) (last-elt pos)) 
			  (mapcar (compose #'- (bind #'apply #'- 1)) (pairs pos))))
	 (optimal-distance (/ (float n) (length pos)))
	 (penalties (mapcar (compose #'sq (bind #'distance optimal-distance)) distances)))
    (if (> (length pos) 1) 
      (sum penalties)
      0)))
;;(2cycle-badness '(a a a a a b))

(cl-defun cycle-badness (cycle &optional (test #'eq))
  "Calculates how good the elements of CYCLE is distributed, assuming the number of non-EQ elements is 2"
  (lexical-let ((test test))
    (loop for elt in (remove-duplicates cycle)
	  for badness = (2cycle-badness cycle #'(lambda (x y) 
						  (xnor (funcall test elt x) 
							(funcall test elt y))))
	  sum badness)))
;;(cycle-badness '(a b c a b c b a c a b c))

(cl-defun cycle-best (cycles &optional (test #'eq))
  "Calculates how good the elements of CYCLE is distributed, assuming the number of non-EQ elements is 2"
  (loop with min-cycle = (first cycles)
	with min = (cycle-badness min-cycle test)
	for c in cycles
	for badness = min then (cycle-badness c test)
	if (zerop badness) return c
	if (< badness min) do (setf min badness min-cycle c)
	finally return min-cycle))
;;(cycle-best '((a a b b) (a b a b)))

(cl-defun distribute-rest (list prefix-cycle &optional (test #'eq))
  (lexical-let* ((rots )
		 (test test))
    (cycle-best (mapcar (bind #'append prefix-cycle) (cycle-rotations (distribute list test))))))
;;(cycle-badness (distribute-rest '(a c a b e b) '(a a b)))

;; energy
(cl-defun p/th-to-NOK/MWh (p/th &optional (NOK/pound 11.5))
  (* p/th 0.01 NOK/pound 34.1))
;;(p/th-to-NOK/MWh 90)352.935

;; trondheim
(defun trondheim-percentage (n-games game-price n-qs-delivered received-price/q wanted-price/q)
  (/ (* n-qs-delivered (- wanted-price/q received-price/q))
     (* 1.0 n-games game-price)))
;;(trondheim-percentage 5000 500 1000 10 35)

(cl-defun polylogarithm-abs-lt-1 (x &optional (n 2) (num-iterations 50))
  "http://mathworld.wolfram.com/Polylogarithm.html"
  (loop for i from 1 below num-iterations
	sum (/ (expt x i) (expt i n))))


;;; Standard math functions not supported by Emacs
(cl-defun dilog-lt-minus-1 (x &optional (num-iterations 50))
  "http://www.geocities.com/hjsmithh/Numbers/Dilog.html"
  (when (>= x -1) (error "Input"))
  (let ((y (/ 1 (- 1.0 x))))
    (- 0 
       (polylogarithm-abs-lt-1 (- 1 y) 2 num-iterations)
       (/ (sq (log y)) 2))))
;;(dilog-lt-minus-1 -2)

(cl-defun dilog-gt-1 (x &optional (num-iterations 50))
  (when (< x 1) (error "Input"))
  (- (dilog 1)
     (* (log x) (log (- x 1)))
     (dilog (- 1 x))))
;;(dilog-gt-1 3 60)

(cl-defun dilog (x &optional (num-iterations 50))
  "http://mathworld.wolfram.com/Dilogarithm.html. For testing, see
http://functions.wolfram.com/webMathematica/FunctionEvaluation.jsp?name=PolyLog2"
  (cond
   ((= x 0) 0)
   ((= x -1) (- (/ (sq pi) 12)))
   ((= x 1) (/ (sq pi) 6))
   ((< (abs x) 1) (polylogarithm-abs-lt-1 x 2 num-iterations))
   ((< x -1) (dilog-lt-minus-1 x num-iterations))
   ((> x 1) (dilog-gt-1 x num-iterations))))
;;(dilog 7)

(defun atan2 (x y)
  "Arctan with two arguments"
  ;; http://en.wikipedia.org/wiki/Atan2
  (numcond x
      (> (atan (/ (float y) x)))
      (< (numcond y
	   (>= (+ (atan (/ (float y) x)) float-pi))
	   (<  (- (atan (/ (float y) x)) float-pi))))
      (= (numcond y
	   (> (/ float-pi 2))
	   (< (- (/ float-pi 2)))
	   (= 'indeterminate)))))
;;(atan2 -1 0)

(defconst bilkollektivet-price-table 
  '((B 26 180 280 2.70 1.30)
    (C 28.50 200 300 2.90 1.50)
    (D 31.50 220 320 3.10 1.70))
  "Format: (type NOK/hour NOK/days¹ NOK/days² NOK/km³ NOK/km°). where
¹: 1-5 days, ²: >6 days, ³: 1-300 km, and °: >301 km")

(cl-defun bilkollektivet-price-calculator (type num-km &optional (days 0) (hours 0))
  ""
  (let* ((price-type (assoc type bilkollektivet-price-table))
	 (price-km (* num-km (if (< num-km 301) (fifth price-type) (sixth price-type))))
	 (price-days (* days (if (< days 6) (third price-type) (fourth price-type))))
	 (price-hours (* hours (second price-type))))
    (+ price-km price-days price-hours)))
;;(bilkollektivet-price-calculator 'C 2000 5)

;;; some physics
(defconst gravitational-constant 6.67428E-11)
(defconst GM-constant 3.986004418E14)
(defconst earth-mass  5.9736E24)
(defconst earth-radius-pole 6.356752E6)
(defconst earth-radius-equator 6.378137E6)
(defconst earth-radius-mean 6.371E6)
;;(sqrt (/ (* 2 gravitational-constant earth-mass) earth-radius-pole))
;;(sqrt (/ (* 2 gravitational-constant earth-mass) earth-radius-equator))


;;; div
(defun is-divisible (n m)
  (= (mod n m) 0))
;(mapcar #'(lambda (n) (is-divisible n 5)) (loop for i below 11 collect i))

(require 'mb-utils-10000-first-primes)
(defun primep (n) (find n 10000-first-primes))
;;(remove-if nil (mapcar #'primep (1-n 20)))

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
    factors))
;(mapcar #'factorize (loop for i from 2 to 100 collect i))
;(mapcar #'factorize '(324 180))
;(/ 180 36)
;(apply #'* (factorize 1047300))
;(/ 288 36)

(cl-defun test-factorize (n)
  "Tests factorize for first N integers"
  (loop for i from 2 to n
	if (not (= i (apply #'* (factorize i))))
	do (error "FACTORIZE failed for argument %d" i)))
;(test-factorize 10000)

;;; Number conversions
(defun calculate-n-ary (root coefficients)
  "COEFFICIENTS is a list of integers a0, a1, a2... where ai < ROOT"
  (loop for coefficient in coefficients
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

(defun int-to-bin-array (n)
  (let ((res ()))
    (while (> n 0)
      (push (logand 1 n) res)
      (setq n (lsh n -1)))
  res))
;;(int-to-bin-array 1235)

(defun int-to-bin (n)
  (apply #'concat (mapcar #'int-to-string (int-to-bin-array n))))
;;(int-to-bin 10450)

(defun bin-to-int-array (binary-string)
  (mapcar #'string-to-int (split-string binary-string "" t)))
;;(bin-to-int-array "01101")

(defun byte-to-2hex (byte) (format "%02x" byte))

(defun byte-to-char (byte) (format "%c" byte))
;;(mapcar #'byte-to-2hex (read-bytes-from-buffer midi-buffer))

(defun bytes-to-string (bytes)
  "Converts a list of 8-bits bytes to a unibyte STRING.
TODO: handle multibyte strings."
  (apply #'concat (mapcar #'byte-to-char bytes)))
;;(bytes-to-string '(65 66 67 68))

(defun string-to-bytes (string)
  "Converts a unibyte STRING to a list of 8-bits bytes. 
TODO: handle multibyte strings."
  (mapcar #'string-to-char (split-string string "" t)))
;;(string-to-bytes "ABCD") ==> (65 66 67 68)

(cl-defun nth-bit (integer n)
  (logand 1 (lsh integer (- n))))
;;(mapcar (bind #'nth-bit 16 1) (b-a 7 0))

(cl-defun int-to-n-bit-bytes (integer &optional (number-of-bits 8) (number-of-bytes nil))
  "Converts INTEGER to a list of NUMBER-OF-BITS-bit bytes.
If NUMBER-OF-BYTES is nil, the result contains only the needed
bytes. If non-nil the result is either truncated to
NUMBER-OF-BYTES, or prepended by zero bytes until a length of
NUMBER-OF-BYTES is reached."
  (if (zerop integer)
    (make-list (or number-of-bytes 1) 0)
    (let* ((number-of-total-bits (log (+ 1.0 integer) 2))
	   (number-of-total-bytes (ceiling (/ number-of-total-bits number-of-bits)))
	   (mask (1- (expt 2 number-of-bits)))
	   (bytes (reverse
		   (loop for i below number-of-total-bytes
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
  (calculate-n-ary (expt 2 8) (case endianness
				((:big) (nreverse bytes))
				((:little) bytes)
				(t (error "%s is not a correct :ENDIANNESS value" :endianness)))))
;;(bytes-to-int (list 1 0) :endianness :little)
;;(bytes-to-int (list 1 0) :endianness :big)

(defun int-to-variable-length-quantity (integer)
  "Converts iNTEGER to a variable length integer.
See http://en.wikipedia.org/wiki/Variable-length_quantity for a
definition."
  (let ((7-bit-bytes (int-to-n-bit-bytes integer 7)))
    (loop for i below (1- (length 7-bit-bytes)) 
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

(cl-defun 2s-complement-function (number-of-bits &key (with-non-integer :error) inverse)
  "Returns a function that converts a number to its 2's complement of bit length NUMBER-OF-BITS.
Optional argument WITH-NON-INTEGER controls the functions
behavior when the argument is not an integer"
  (lexical-let ((2^n (expt 2 number-of-bits))
		(2^n-1 (expt 2 (1- number-of-bits)))
		(with-non-integer with-non-integer)
		(inverse inverse))
    (lambda (x)
      (if (integerp x)
	(if inverse
	  (if (minusp x) (+ x 2^n) x)
	  (if (> x 2^n-1) (- x 2^n) x))
	(case with-non-integer
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
(defun random-float-base (&optional seed)
  "Returns a number in [0 1]"
  (when seed (random t))
  (/ (- (coerce (random) 'float) most-negative-fixnum)
     (- (coerce most-positive-fixnum 'float)
	(coerce most-negative-fixnum 'float))))
;;(loop for i below 100000 count (< (random-float-base) .1)) should -> .1

(cl-defun random-float (&optional (a 0.0) (b 1.0) seed)
  (+ (* (random-float-base seed) (- b a)) a))
;;(random-float 1 4)

(cl-defun random-integer (&optional (a 0) (b 1) seed)
  (when seed (random t))
  (+ (random (1+ (- b a))) 
     a))
;;(loop for i below 100000 count (= (random-integer 1 3) 3))

(cl-defun random* (&optional (a 0) (b 1) (integerp nil) seed)
  (if integerp
    (random-integer a b seed)
    (random-float a b seed)))

(defun interval-floor (n interval)
  (* (floor n interval) interval))
;;(interval-floor 30 10)


;;pool utils
(defun fractional-ball-angle (fraction &optional with-object-ball-throw-correction)
  "Optional argument WITH-OBJECT-BALL-THROW-CORRECTION is not implemented"
  (assert (between= fraction 0 1))
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

(provide 'mb-utils-math)

(cl-defun generate-addends-fixed (n k)
  "Returns a LIST of K numbers that adds to N, and so that the
numbers are `almost' equal (ie. (- (max LIST) (min LIST)) is
either 0 or 1)"
  (destructuring-bind (floor rest) (floor* n k)
    (let ((res (make-list k floor)))
      (loop for i below rest
	    for x in-ref res
	    do (incf x))
      res)))
;;(generate-addends-fixed 271 3)

(cl-defun generate-addends (n)
  (if (zerop n)
    (list nil)		    ; resulting list consists of the empty set
    (loop for i below n
	  append (mapcar (bind #'cons (- n i) 1) (generate-addends i)))))
;;(generate-addends 4) => ((4) (3 1) (2 2) (2 1 1) (1 3) (1 2 1) (1 1 2) (1 1 1 1))

(cl-defun aggregate (numbers &optional (init 0))
  (when numbers
    (let ((head (+ init (first numbers))))
      (cons head (aggregate (rest numbers) head)))))
;;(aggregate '(1 2 3 4))

(cl-defun fibonacci-numbers (n &optional (start-values '(1 1)))
  (let ((res (reverse start-values)))
    (loop for i below (- n 2)
	  do (push (+ (first res) (second res)) res))
    (nreverse res)))
;;(length (fibonacci-numbers 10))

(defun waverage (list)
  "Returns the Weighted average of the number and weight pairs in LIST: ((x1 w1) (x2 w2) ...)"
  (/ (sum (mapcar (bind #'apply #'* 1) list))
     (float (sum (mapcar #'second list)))))
;;uio grades
;;(waverage '((1 10) (1 10) (3 10) (1 10) (3 15) (4 10) (1 10) (1 5) (2 5) (2 10) (1 5) (1 5)))

(cl-defun hourly-from-yearly-salary (yearly-salary &optional (num-hours-per-year 1650) (arbeidsgiveravgift .14) (holiday-money .12) (company-costs .05))
  "For num-hours-per-year in Norway, see http://no.wikipedia.org/wiki/Årsverk.
"
  (let* ((hourly-salary-raw (/ (float yearly-salary) num-hours-per-year))
	 (hourly-salary-with-holiday-money (* hourly-salary-raw (1+ holiday-money)))
	 (hourly-salary-with-company-costs (* hourly-salary-with-holiday-money (1+ company-costs)))
	 (hourly-salary (* hourly-salary-with-company-costs (1+ arbeidsgiveravgift))))
    (list hourly-salary hourly-salary-with-company-costs hourly-salary-with-holiday-money hourly-salary-raw)))
;;(hourly-from-yearly-salary 567875)

(defun quadratic-solver (a b c)
  (let ((r (/ (- b) (* 2.0 a)))
	(q (/ (sqrt (- (* b b) (* 4.0 a c)))
	      2.0)))
    (list (+ r q) (- r q))))
;;(quadratic-solver 1 1 -6)

(defun ceilx*x-solver (y)
  "Assume y and solution is positive"
  (let* ((min (first (quadratic-solver 1 1 (- y))))
	 (max (first (quadratic-solver 1 0 (- y))))
	 (range (mapcar 'float (a-b (ceiling min) (ceiling max))))
	 (solution-candidates (mapcar (bind #'/ y 1) range)))
    (find y solution-candidates :test #'= :key #'(lambda (x) (* (ceiling x) x)))))
;;(ceilx*x-solver 3.1)

;; financial
(defun interest-annual (x interest year)
  (* x (expt (1+ interest) years)))
;;(interest-annual 1000 .06 5)

(defun naaverdi-annual (x interest years)
  (/ x (expt (1+ interest) years)))

(defun obligation-value (x yield years market-interest)
  (+ (naaverdi-annual x market-interest years)
     (loop with premium = (* x yield)
	   for i below years
	   sum (naaverdi-annual premium market-interest i))))
;;(obligation-value 1000 .06 5 0.04)

;; swimming!
(defconst swimming-records
  '((46.91 crawl)
    (49.82 butterfly)
    (51.94 backstroke)
    (58.46 breaststroke)))

(cl-defun swimming-relative-distances (&optional (style-symbol 'crawl) (distance 100))
  (let ((style (find style-symbol swimming-records :key #'second))
	(other-styles (remove* style-symbol swimming-records :key #'second)))
    (mapcar #'(lambda (x)
		(list (* distance
			 (/ (first style)
			    (first x)))
		      (second x)))
	    other-styles)))
;;(swimming-relative-distances 'crawl 50)

;;; div js dates (TODO move this later)
;; This buffer is for notes you don't want to save, and for Lisp evaluation.
;; If you want to create a file, visit that file with C-x C-f,
;; then enter the text in that file's own buffer.

;;;;
(defun div-sec-min (seconds) (floor* seconds 60.0))
(defun div-min-hour (minutes) (floor* minutes 60.0))
(defun div-hour-day (hours) (floor* hours 24.0))
;;(div-sec-min 72)

(defun leap-year-p (year)
  (and (modp year 0 4)
       (or (modp year 0 400)
	   (not (modp year 0 100)))))
;;(mapcar #'leap-year-p '(1600 1700 1800 1900 2000 2001 2002 2003 2004))

(defun days-in-year (year)
  (if (leap-year-p year) 366 365))
;;(mapcar #'days-in-year '(1600 1700 1800 1900 2000 2001 2002 2003 2004))

(defun days-between-years (year1 year2)
  (loop for y from year1 below year2 
	sum (days-in-year y)))
;;(days-between-years 1970 1971)

(defun current-year () (sixth (decode-time)))

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

(defun linear-function-slope-point (slope x)
  "Returns a linear function with SLOPE going through point X.
The point is represented as a pair (X0 X1)."
  (lexical-let* ((x0 (first x))
		 (x1 (second x))
		 (a slope)
		 (b (- x1 (* a x0))))
    #'(lambda (x) (+ (* a x) b))))
;;(mapcar (linear-function-slope-point 2 '(1 2)) (a-b -3 3))

(defun linear-function-point-to-point (x y)
  "Returns a linear function going through points X and Y. Both
  poings are represented as a pair of numbers. See
  `linear-function-slope-point'"
  (linear-function-slope-point (/ (float (- (second y) (second x)))
				  (float (- (first y) (first x))))
			       x))
;;(mapcar (linear-function-point-to-point '(0 0) '(1 2)) (a-b -3 3))

(defun linear-function (slope-or-point point)
  (if (consp slope-or-point)
    (linear-function-point-to-point slope-or-point point)
    (linear-function-slope-point slope-or-point point)))
;;(mapcar (linear-function '(0 0) '(1 2)) (a-b -3 3))

(cl-defun nth-digit (number k &optional (base 10))
  "Returns the Kth digit of NUMBER. The return value is an integer symbol"
  (assert (< k (log number base)))
  (floor (mod number (expt base (1+ k)))
	 (expt base k)))
;;(nth-digit 123 2)

(cl-defun digit-sum-1 (n &optional (base 10))
  "Inner sum function for `digit-sum'" 
  (loop for i below (log n base)
	sum (nth-digit n i base)))

(cl-defun digit-sum (n &optional (base 10))
  "Returns the decimal digit sum of integer N" 
  (loop for n* = n then res
	for res = (digit-sum-1 n* base) 
	until (< res base)
	finally return res))
;;(digit-sum 123 2)

(defun tall-navn (n &optional intetkjønn-p)
  (cond ((= n 0) "null")
	((= n 1) (if intetkjønn-p "ett" "en"))
	((< n 20) 
	 (symbol-name (nth (- n 2) '(to tre fire fem seks sju åtte ni ti elleve tolv tretten fjorten femten seksten sytten atten nitten))))
	((and (zerop (mod n 10))
	      (<= n 100))
	 (symbol-name (nth (- (/ n 10) 2) '(tjue tretti førti femti seksti sytti åtti nitti hundre))))
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

(defun extended-gcd (a b)
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
    (message "Bézout coefficients: (%d, %d)
greatest common divisor: %d
quotients by the gcd: (%d, %d)"
	     old-s old-t* old-r t* s)
    (mod old-s b)))
;;(extended-gcd -7 -11)
;;(extended-gcd 19 12)
;;(/ (- 3 (* 2 (log 3 2))) (- (* 7 (log 3 2)) 11))
;;  

(cl-defun floor-test (k &optional (max-m k))
  "Tests that F(mlog3) = F(m(F(klog3))/k), for all m and k"
  (let ((log3 (log 3 2)))
    (loop for m to max-m
       for a = (floor (* m log3))
       for b = (floor (/ (* m (floor (* k log3))) k))
       always (= a b))))

(cl-defun floor-test (k &optional (max-m k))
  "Tests that F(mlog3) = F(m(F(klog3))/k), for all m and k"
  (let ((log3 (log 3 2)))
    (loop for m to max-m
       for a = (floor (* m k log3))
       for b = (* m (floor (* k log3)))
       always (= a b))))
;;(mapcar #'floor-test (a-b 20 30))
;;(loop for i below 1000 if (floor-test i) collect i)(0 1 2 7 12 24 53 106 359 665)
;;(mapcar #'floor-test (list 7 12 24 36 48 63))
;;(list (log 3 2) (/ 11 7.0))

(defun fractional (x) (- (float x) (floor x)))
;;(fractional 3.4)

(defun test (k) (< (fractional (* k (log 3 2))) (/ 1.0 k)))
;;(loop for i from 1 below 1000 if (test i) collect (list i (l-value i) (gcd i (l-value i))))

(cl-defun floor-test2 (max-k m)
  "Tests that F(mlog3) = F(m(F(klog3))/k), for all m and k"
  (let ((log3 (log 3 2)))
    (loop for k from 1 to max-k collect
	 (list k
	       (floor (* m log3))
	       (floor (/ (* m (floor (* k log3))) k))))))
;;(floor-test2 7 1)
;;(floor (* 28 (log 3 2)))
;;(list (log 3 2) (/ 11 7.0))
;;(loop for i to 5 collect (floor (* i (log 3 2))))
;;(loop for i below 30 collect  (list i (floor (* i (log 3 2)))))

(defun l-value (k) (floor (* k (log 3 2))))
;;(mapcar #'l-value (a-b 1 12))
;;(loop for k from 1 to 12 collect (* (float k) (fractional (* k (log 3 2)))))

(cl-defun test3 (s &optional (k 12))
  (let ((l (l-value k))
	(l-inverse (extended-gcd (l-value k) k)))
    (list s
	  (* l-inverse s)
	  (mod (* l-inverse s) k)
	  (* l (mod (* l-inverse s) k))
	  (first (cl-floor (* l (mod (* l-inverse s) k)) k)))))
;;(loop for s below 10 collect (test3 s))

(defun test4 ())

(provide 'mb-utils-math)
