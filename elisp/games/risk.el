(defun roll-dice () (1+ (random 6)))
(defun roll-dices (n) (sort (loop repeat n collect (roll-dice)) #'>))
;;(roll-dices 3)

(defun risk-roll-1 (blacks reds)
  "Returns the result of a single risk roll a list of booleans.
T means a black win, NIL means a red win."
  (loop for b in (roll-dices blacks)
	for r in (roll-dices reds)
	collect (> b r)))
;;(risk-roll-1 3 2)

(defun risk-roll (blacks reds)
  "Returns the result of a single risk roll a list of booleans.
T means a black win, NIL means a red win."
  (let ((bres (risk-roll-1 blacks reds)))
    (list (decf blacks (count t bres))
	  (decf reds (count nil bres)))))
;;(risk-roll 3 4)

(defun risk-simulate-2 (blacks reds)
  (let ((bres (cl-sort (roll-dices blacks) #'>))
	(rres (cl-sort (roll-dices reds) #'>)))
    (list bres rres)))
;;(risk-simulate-2 3 2)

(defun risk-simulate-1 (blacks reds)
  )

(defun risk-simulate (blacks reds)
  )

(defconst *dice* (a-b 1 6))
(cl-defun possible-dices (n)
  (if (plusp n)
    (loop for config in (possible-dices (1- n))
	  append (loop for x in *dice* collect (cons x config)))
    (list nil)))
;;(length (possible-dices 5))
