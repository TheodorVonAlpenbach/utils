;; 600851475143

;;; Utils
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

;;; Only code
(defun 003-solution (&optional (n 600851475143))
  (first (last (primes-below (sqrt n) :test #'(lambda (x) (zerop (mod n x)))))))
;;(time (003-solution))
