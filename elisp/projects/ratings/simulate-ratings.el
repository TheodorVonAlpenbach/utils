(require 'glicko)
;;;; Simulate a converge from default ratings to actual ratings

;; Since these ratings are assumed to be the completely correct, RD is zero
(defconst +player1-rating+ 700)
(defconst +player2-rating+ 1000)
(defconst +player3-rating+ 2000)
(defconst +problem-rating+ 1100)
(defconst +default-ratings+ (list 0 350))

(defun player1-ratings () (list +player1-rating+ 0))
(defun player2-ratings () (list +player2-rating+ 0))
(defun player3-ratings () (list +player3-rating+ 0))
(defun problem-ratings () (list +problem-rating+ 0))

;; Note that expectation is dependent on RD!
;;(mapcar #'(lambda (x) (glicko-expected-score -169 -475 x)) '(0 1 10 100 1000))
;; => (0.853 0.853 0.853 0.842 0.629)
;; The higher RD the more the expectancy shifts towards 0.5 (i.e. the unknown)
;;(glicko-expected-score -169 -475 1E20) => .5

(defun simulate-result (player-ratings problem-ratings)
  (message "exp: %S" (glicko-expected-score (car player-ratings) (car problem-ratings)))
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

(let ((player1 (list (list (copy-tree +default-ratings+)) (player1-ratings)))
      (player2 (list (list (copy-tree +default-ratings+)) (player2-ratings)))
      (player3 (list (list (copy-tree +default-ratings+)) (player3-ratings)))
      (problem (list (list (copy-tree +default-ratings+)) (problem-ratings))))
  (cl-loop repeat 1000
	   for (new-user1 new-problem) = (simulate-match player1 problem)
	   do (push new-user1 (car player1))
	   do (push new-problem (car problem)))
  (list (caar player1)
	(caar problem)))

