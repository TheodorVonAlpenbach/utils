(defun fibonacci-sequence (n)
  "Return the terms in the Fibonacci sequence below N."
  (unless (< n 2)
    (let ((result '(1 1)))
      (loop for next-term = (+ (car result) (cadr result))
	    while (< next-term n)
	    do (setf result (cons next-term result)))
      result)))
;;(fibonacci-sequence 10)

(defun 002-solution (&optional (n 4000001))
  "Return the sum of even Fibonacci numbers below N"
  (loop for x in (fibonacci-sequence n) if (evenp x) sum x))
;;(time (002-solution))
;; => 4613732
