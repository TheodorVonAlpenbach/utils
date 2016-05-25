(require 'mb-utils-math)
;;;; Implementation of the glicko rating
;;;; See  http://en.wikipedia.org/wiki/Glicko_rating_system for details.

(defconst +glicko-q-constant+ (/ (log 10) 400))
(defconst +glicko-q-squared+ (sq +glicko-q-constant+))
(defconst +glicko-P-constant+ (* 3 (sq (/ +glicko-q-constant+ pi))))
(defconst +glicko-max-RD+ 350)
(defconst +glicko-RD-restore-time+ 14 "Days")
(defconst +glicko-typical-RD+ 50)
(defconst +glicko-c-constant+ (sqrt (/ (- (sq +glicko-max-RD+) (sq +glicko-typical-RD+))
				     +glicko-RD-restore-time+)))
(defconst +glicko-init-rating+ (list 1500 +glicko-max-RD+))

(defun glicko-update-RD (old-RD &optional time-since-last-rating)
  "If TIME-SINCE-LAST-RATING is not nil, update OLD-RD. Else, just return OLD-RD.
This method guarantees to return a number of type float."
  (if time-since-last-rating
    (sqrt (+ (sq old-RD) (* (sq +glicko-c-constant+) time-since-last-rating)))
    (float old-RD)))
;;(glicko-update-RD +glicko-typical-RD+ +glicko-RD-restore-time+)
;;(glicko-update-RD +glicko-typical-RD+)

(defun glicko-g-function (RD)
  "Returns a float"
  (/ 1 (sqrt (1+ (* +glicko-P-constant+ (sq RD))))))
;;(mapcar #'glicko-g-function '(30 100 300))
;;(glicko-rating '(1500 200) '(((1400 30) 1) ((1550 100) 0) ((1700 300) 0)))

(defun glicko-expected-score (players-rating opponents-rating opponents-RD)
  "Returns a float"
  (/ 1 (1+ (expt 10 (/ (* (glicko-g-function opponents-RD)
			  (- players-rating opponents-rating))
		       -400)))))
;;(glicko-expected-score 1500 1300 50)

(defun glicko-d-squared (opponent-g-values expected-scores)
  "The lists opponent-g-values and expected-scores containts the
g-value and expected score, respectively, for the opponents"
  (/ 1 +glicko-q-squared+ (loop for g in opponent-g-values
			      for e in expected-scores
			      sum (* (sq g) e (- 1 e)))))
;;(glicko-d-squared '(.94) '(.5))

(defun glicko-expected-scores (original-rating opponent-ratings opponents-RDs)
  "Helper function"
  (cl-mapcar #'(lambda (r RD) (glicko-expected-score original-rating r RD))
	     opponent-ratings opponent-RDs))

(defun glicko-reorder (player matches)
  "Tranformes PLAYER and MATCHES to the list (player-rating player-RD ratings RDs scores)"
  (append player (transpose (cut (flatten matches) 3))))
;;(glicko-reorder '(1600 50) '(((1500 30) 1) ((1700 20) .5) ((2000 10) 0)))
;;(glicko-reorder '(1600 50) '((1500 30) 1))

(defun glicko-rating (player matches &optional time)
  "Returns a list consiting of PLAYERs new rating and RD after
playing MATCHES, which is either a single MATCH or a list of
MATCHes. A MATCH is a pair (OPPONENT SCORE). Both PLAYER and
OPPONENT is a pair (RATING RD) which indicates the PLAYER's or
opponent's strength. SCORE is a real number in the interval [0
1]. Optional TIME is the number of days since last time PLAYERs
rating was calculated. It is used to modify PLAYERs RD due to the
TIME passed since his last activity."
  (destructuring-bind (original-rating original-RD opponent-ratings opponent-RDs scores)
      (glicko-reorder player matches)
    (let* ((RD (glicko-update-RD original-RD time))
	   (g-values (mapcar #'glicko-g-function opponent-RDs))
	   (expected-scores (glicko-expected-scores original-rating opponent-ratings opponent-RDs))
	   (new-RD (sqrt (/ 1 (+ (/ 1 (sq RD)) (/ 1 (glicko-d-squared g-values expected-scores))))))
	   (new-rating (+ original-rating
			  (* +glicko-q-constant+
			     (sq new-RD)
			     (loop for g in g-values
				   for s in scores
				   for e in expected-scores
				   sum (* g (- s e)))))))
      (list new-rating new-RD))))
;;(glicko-rating '(1500 200) '(((1400 30) 1) ((1550 100) 0) ((1700 300) 0)))

(provide 'glicko)
