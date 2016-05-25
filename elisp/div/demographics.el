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
;;(sds-estimate-growth 10 30 '(2 2 2) 80)
;;(pp (loop for y from 0 to 50 by 5 collect (* .2 (expt 1.08 y))))
;;(pp (loop for y from 0 downto -10 collect (* .2 (expt 1.08 y))))
(* .2 (expt 1.08 24))
(0.2 0.18518518518518517 0.17146776406035663 0.1587664482040339 0.14700597055929063 0.1361166394067506 0.12603392537662092 0.11669807905242675 0.10805377690039515 0.10004979342629179 0.09263869761693684)

(0.2 0.2938656153600001 0.43178499945455767 0.6344338228396544 0.9321914287698617 1.3696950392438656 2.0125313778146903 2.9570688588641136 4.344904299359981 6.384089878058653 9.38032250264627)

