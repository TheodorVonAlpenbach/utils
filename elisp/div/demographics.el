;; simple-demographics-simulator (sds)
(cl-defun sds-next-generation (population
			       average-number-of-children
			       &optional (premature-death-rate '(0.01 0.1)))
  "Calculates the next generation of POPULATION.
POPULATION is a triple (CHILDREN ADULTS ELDER)."
  (destructuring-bind (children adults elder)
      population
    (destructuring-bind (pdr-children pdr-adults)
	premature-death-rate
      (list (* average-number-of-children
	       (/ adults 2.0))
	    (* children (- 1 pdr-children))
	    (* adults (- 1 pdr-adults))))))
;;(sds-next-generation '(2 2 2) 7)

(cl-defun sds-simulate (num-generations
			generation-length
			start-population
			average-number-of-children
			&optional (premature-death-rate '(0.01 0.1)))
  (loop for x to num-generations
	for new-population = start-population
	    then (sds-next-generation
		  new-population
		  average-number-of-children
		  premature-death-rate)
	collect new-population))
;;(sds-simulate 3 30 '(0.05 0.1 0.05) 70)

(cl-defun sds-estimate-growth (num-generations
			       generation-length
			       &rest args)
  "See `sds-simulate' for ARGS"
  (let* ((populations (apply #'sds-simulate
			     num-generations
			     generation-length
			     args))
	 (pop-size (mapcar #'sum populations)))
    (expt (average (loop for (a b) in (pairs pop-size)
			 collect (/ (float b) a)))
	  (/ 1.0 generation-length))))
;;(sds-estimate-growth 10 30 '(3.8 2 2) 7 '(0.5 0.2))
;;(sds-estimate-growth 100 30 '(1 1 1) 71)
;;(sds-estimate-growth 100 30 '(1 1 1) 3.1)
;;(sds-estimate-growth 100 30 '(1 1 1) 1.7)
;;(pp (loop for y from 0 to 50 by 5 collect (* .2 (expt 1.08 y))))
;;(pp (loop for y from 0 downto -10 collect (* .2 (expt 1.08 y))))
