(in-package :signal)

(defun delta (&optional (from 0) (to 0))
  (apply #'sig from (loop for n from from to to collect (if (zerop n) 1 0))))
;;(delta)

(defun unit (length) (apply #'sig 0 (make-list length :initial-element 1)))
;;(unit 10)

(defun rect (N)
  "Returns the signal rect[n/2N]"
  (apply #'sig (- N) (make-list (1+ (* 2 N)) :initial-element 1)))
;;(rect 2)

(defun tri (N)
  "Returns the signal tri[n/N]"
  (apply #'sig (- N)
	 (nconc (loop for i below N collect (/ i N))
		(loop for i from N downto 0 collect (/ i N)))))
;;(tri 5)

(defun ramp (&optional (length 20))
  "Returns the signal r[n]"
  (loop for i below length collect i))
;;(ramp)
;;(convolve (unit 100) (unit 100))
