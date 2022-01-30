(require '10000-first-primes "~/git/utils/elisp/div/euler-project/10000-first-primes.lisp")

(defun make-prime-table (n)
  "Make a prime table for all integeres below N.
This not very efficient, and cannot handle bit N's. It is probably better with a assoc-like structure, e.g.
#[() () ((2 . 1)) ((3 . 1)) ((2 . 2)) ((5 . 1)) ((2 . 1))(3 . 1)]"
  (let ((pt (make-array n :initial-element ())))
    (loop for p in 10000-first-primes
	  while (< p n)
	  do (loop for i from 1 below n do (push 0 (svref pt i)))
	  (loop for e from 1
		for p^e = (expt p e)
		while (< p^e n) do
		(loop for x from p^e by p^e below n
		      do (incf (car (svref pt x))))))
    pt))
;;(make-prime-table 3)

(defun pt-product (fs)
  (reduce #'* (loop for p in 10000-first-primes
		    for e in (reverse fs)
		    collect (expt p e))))
;;(pt-product (svref (make-prime-table 21) 20))

(defun pt-lcm-fs (fs1 fs2) (mapcar #'max fs1 fs2))

(defun pt-lcm (pt) (reduce #'pt-lcm-fs pt :start 1))
;;(pt-product (pt-lcm (make-prime-table 21)))

(provide 'prime-table)
