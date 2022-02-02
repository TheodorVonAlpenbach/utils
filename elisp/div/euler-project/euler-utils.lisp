(defun fill-prime-vector (p pv)
  "Set all factors of P in PV to 1 and return the position in PV of next prime"
  (loop for ip from p by p below (length pv) do (setf (bit pv ip) 1)))

(defun primes-below (n &key test (first-prime 2))
  "Return all prime numbers below N. If unary predicate TEST is false it stops collecting"
  (let ((pv (make-array (ceiling n) :element-type 'bit)))
    (loop for p = first-prime then (position 0 pv :start (1+ p))
	  while (and p (< p n))
	  do (fill-prime-vector p pv)
	  if (or (null test) (funcall test p)) collect p)))
;;(time (let ((ps (primes-below 1E8))) (length ps))) => 5761455
;;(primes-below 10.4 :test #'oddp)

(defun li-inverse (x)
  "Return an approximatino of the inverse logarithmic integral."
  (let* ((ln (log x))
	 (ln2 (log (log x))))
    (* x (+ ln ln2 -1 (/ (- ln 2)
			 ln)
	    (- (/ (+ (- (sq ln2) (* 6 ln2))
		     11)
		  (* 2 (sq ln))))))))
;;(li-inverse 25)
;;(primes-below)

(defun pi-test ()
  "Verify that li-inv(n) overshoots pi(n) for n > 49."
  (loop for p in (rest (rest (primes-below 100000)))
	for n from 3
	for pi_n = (li-inverse n)
	for ratio = (/ pi_n p)
	if (< ratio 1) collect (list n p pi_n ratio)))
;;(pi-test)

(defun nth-prime (n)
  "Return the Nth prime, zero-based"
  (nth n (primes-below (if (< n 50) 250 (li-inverse n)))))
;;(nth-prime 1)

(provide 'euler-utils)
