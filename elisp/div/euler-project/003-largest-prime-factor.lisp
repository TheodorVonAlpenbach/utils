;; 600851475143

;;; Only code

(defun 003-solution (&optional (n 600851475143))
  (first (last (primes-below (sqrt n) :test #'(lambda (x) (zerop (mod n x)))))))
;;(time (003-solution))
