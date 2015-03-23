(in-package :numerics-utils)

(defun integrate-simpson-sequence (f-values x0 xm)
  "Return the integral of a function F from X0 to XM, based on F-VALUES.
The f-values are the set [F(xi)] for xi in [X0, X0 + d, X0 + 2d, ...,
 XM], where d = (XM - X0)/M for some positive even integer M. This
 function is a variant of `integrate-simpson' in cl-math."
  (declare (float x0 xm sum-odd sum-even)
	   (fixnum i m))
  (let ((m (1- (length f-values)))) ; M is the number of partitions of the integration interval
    (when (oddp m) (error "Simpson requires an even number of steps, i.e. odd number of elements in input sequence"))
    (loop for i from 1 below m
	  if (oddp i) sum (aref f-values i) into sum-odd
	  else        sum (aref f-values i) into sum-even
	  finally (return (* (/ (- xm x0) 3 m)
			     (+ (aref f-values 0)
				(* 4 sum-odd)
				(* 2 sum-even)
				(aref f-values m)))))))

(defun generate-integrate-simpson-sequence (fn x0 xm m)
  "Converts input parameters to `integrate-simpson' to input parameters to `integrate-simpson-sequence'."
  (coerce (loop with step = (/ (- xm x0) (if (oddp m) (1+ m) m))
		for x from x0 to xm by step
		collect (funcall fn x)) 'vector))
;;(generate-integrate-simpson-sequence #'sqrt 0 1 3)

(defun integrate-simpson-emulate (fn x0 xm m)
  "This function is equivalent to `integrate-simpson', but is
implemented with the use of `integrate-simpson-sequence'. It is useful
for testing."
  (integrate-simpson-sequence (generate-integrate-simpson-sequence fn x0 xm m) x0 xm))
;;(integrate-simpson #'sqrt 0 1 10000)

(defun test-integrate-simpson-sequence (&key (fns (list #'sqrt)) (x0s '(0)) (x1s '(1)) (ms '(10 11)))
  (loop for fn in fns always
	(loop for x0 in x0s always
	      (loop for x1 in x1s always
		    (loop for m in ms
			  for fn1 = (integrate-simpson fn x0 x1 m)
			  for fn2 = (integrate-simpson-emulate fn x0 x1 m)
			  always (or (= fn1 fn2)
				     (error "Differences for arguments ~a: ~a and ~a"
					    (list fn x0 x1 m) fn1 fn2)))))))
;;(test-integrate-simpson-sequence :ms '(10000))

