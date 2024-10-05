(cl-defun roll-dice (&optional base)
  (if base (random-integer base (+ base 5)) (random 6)))
;; (roll-dice 1)

(cl-defun roll-dices-no-sort (n)
  (cl-loop repeat n collect (roll-dice)))
;;(roll-dices-no-sort 3)

(cl-defun roll-dices (n)
  (sort (cl-loop repeat n collect (roll-dice)) #'>))
;;(roll-dices 3)

(cl-defun risk-roll-loss (blacks reds)
  "Returns the losses in a single roll as (BLACKS-LOST REDS-LOST).
T means a black win, NIL means a red win."
  (let* ((bd (roll-dices blacks))
	 (rd (roll-dices reds))
	 (bres (cl-mapcar #'> bd rd)))
    (list (count nil bres) (count t bres) bd rd)))
;;(risk-roll-loss 3 2)

(cl-defun risk-roll-loss-prediction (blacks reds &optional (n 100000))
  "Simulate N single rolls"
  (mapcar #'average
    (transpose (cl-loop repeat n collect (head 2 (risk-roll-loss blacks reds))))))
;;(risk-roll-loss-prediction 3 2)
;;(0.421116 0.578884)
;;(risk-roll-loss-prediction 1 1)
;;(/ 5.0 12)
;;(- 1 (/ 49.0 144))

(cl-defun risk-simulate (blacks reds)
  (cl-loop while (and (plusp blacks)
		   (plusp reds))
	for loss = (risk-roll-loss (min blacks 3) (min reds 2))
	for (blacks-lost reds-lost) = loss
	collect loss into losses
	do (decf blacks blacks-lost)
	do (decf reds reds-lost)
	finally return (list blacks reds losses)))
;;(risk-simulate 10 10)

(cl-defun risk-simulate-prediction (blacks reds &optional (n 10000))
  "Simulate N single rolls"
  (let* ((res (cl-loop repeat n collect (head 2 (risk-simulate blacks reds))))
	 (bwins (copy-if #'(lambda (x) (apply #'> x)) res)))
    (list (/ (length bwins) (float n))
	  (average bwins :key #'first))))
;;(risk-simulate-prediction 7 7 100)

(cl-defun risk-simulate-prediction-table (blacks reds &optional (n 1000))
  (combine (list (1-n blacks) (1-n reds))
	   :key #'(lambda (x y) (risk-simulate-prediction x y n))))
;;(time (risk-simulate-prediction-table 20 20))
