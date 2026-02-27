(require 'glicko)
;;;; Simulate a converge from default ratings to actual ratings

;; Since these ratings are assumed to be the completely correct, RD is zero
(defconst +players+ '(890 1000 2000 1100 1500 0))

(defun init-players-1 (players)
  (cl-loop with average-rating = (average players)
	   for c in players collect `(((,average-rating 350)) (,c 0))))
;;(init-players-1 (list +player3+))

(defun init-players (&rest indices)
  (init-players-1
   (if indices (project +players+ indices) +players+)))
;;(init-players)

;; Note that expectation is dependent on RD!
;;(mapcar #'(lambda (x) (glicko-expected-score -169 -475 x)) '(0 1 10 100 1000))
;; => (0.853 0.853 0.853 0.842 0.629)
;; The higher RD the more the expectancy shifts towards 0.5 (i.e. the unknown)
;;(glicko-expected-score -169 -475 1E20) => .5

(defun simulate-result (player-ratings problem-ratings)
  ;; (message "exp: %S" (glicko-expected-score (car player-ratings) (car problem-ratings)))
  (if (< (random-float)
	 (glicko-expected-score (car player-ratings) (car problem-ratings)))
    1 0))
;;(simulate-result (list 1000 0) (list +problem-rating+ 0))

(defun glicko-rating-1 (player-ratings problem-ratings score)
  (glicko-rating player-ratings (list (list problem-ratings score))))
;;(glicko-rating-1 '(200 20) '(200 10) 0)

(defun simulate-match-1 (player-ratings problem-ratings score)
  (list (glicko-rating-1 player-ratings problem-ratings score)
	(glicko-rating-1 problem-ratings player-ratings (- 1 score))))
;;(simulate-match-1 (list 1000 10) (list +problem-rating+ 100) 1)

(defun simulate-match (player problem)
  (simulate-match-1 (caar player)
		    (caar problem)
		    (simulate-result (second player) (second problem))))
;;(simulate-match (list (list +default-ratings+) (player1-ratings)) (list (list +default-ratings+) (problem-ratings)))

(cl-defun simulate-glicko-rating (&optional (players (init-players)) (n 100))
  (cl-loop with size = (length players)
	   repeat n
	   for indices = (0-n size)
	   for (player1 player2) = (project players (draw-random l 2))
	   for (a b) = (simulate-match player1 player2)
	   ;; for da = (- (car a) (caaar player1))
	   ;; for db = (- (car b) (caaar player2))
	   ;; do (progn (push a (car player1))
	   ;; 	   (push b (car player2)))
	   do (progn (setf (car player1) (list a))
		     (setf (car player2) (list b)))
	   ;; do (message "drift: %S + %S = %S" da db (+ da db))
	   finally return
	   (let* ((res (mapcar #'(lambda (x) (rcons (caar x) (caadr x)))
			 players))
		  (drifts (mapcar #'(lambda (x) (- (third x) (first x))) res)))
	     (list
	      (last (car players))
	      (sum drifts)
	      drifts
	      res))))
;;(simulate-glicko-rating)


;;;; Two rating categories 

(defconst +players2+ '((890 0) (1000 1000) (2000 1500) (1100 2000) (0 1500)))

(defun init-players2-1 (players2)
  (cl-loop with average-ratings = (mapcar #'average (transpose players2))
	   for c in players2 collect
	   `((((,(first average-ratings) 350)
	       (,(second average-ratings) 350)))
	     ((,(first c) 0)
	      (,(second c) 0)))))
;;(init-players2-1 (last +players2+))

(defun init-players2 (&rest indices)
  (init-players2-1
   (if indices (project +players2+ indices) +players2+)))
;;(init-players2)

(- (car (glicko-rating '(1500 30) '((1400 30) 1))) 1500)1.8468580755247785
(- 1400 (car (glicko-rating '(1400 30) '((1500 30) 0))))1.8468580755247785

(+ (glicko-expected-score 1500 1400 30)
 (glicko-expected-score 1400 1500 30))
