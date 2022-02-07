(defconstant +phi+ (coerce (/ (+ 1 (sqrt 5)) 2) 'double-float))
(defconstant +psi+ (coerce (/ (- 1 (sqrt 5)) 2) 'double-float))

(defun approx-fib-index (f)
  (round (+ (log f +phi+) (log (sqrt 5) +phi+))))
;;(approx-fib-index 21)

(defun nth-fibonacci-formula (n)
  (/ (- (expt +phi+ n) (expt +psi+ n))
     (sqrt 5)))

(defun nth-fibonacci (n)
  (loop for i from 2 below n
	for an-2 = 1 then an-1
	for an-1 = 1 then an
	for an = (+ an-1 an-2)
	finally (return (values an an-1 an-2 i))))
;;(nth-fibonacci 5000)
;;(loop for i below 20 collect (round (nth-fibonacci i)))

(defun 025-verify (n an an-1 an-2 limit)
  (if (< an limit)
    (if (< (+ an an-1) limit)
      (error "approx-fib-index not quite working")
      (1+ n))
    (if (< an-1 limit)
      n
      (if (< an-2 limit)
	(1- n)
	(error "approx-fib-index not quite working")))))

(defun 025-solution (&optional (digits 1000))
  (let* ((limit (expt 10 digits))
	(n (approx-fib-index limit)))
    (multiple-value-bind (an an-1 an-2 ignore) (nth-fibonacci n)
      (values n (025-verify n an an-1 an-2 limit)))))
;;(time (025-solution))
;; => 4787
