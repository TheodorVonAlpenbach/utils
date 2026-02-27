(require 'mb-utils-math)
;;;; Implementation of the glicko rating
;;;; See  http://en.wikipedia.org/wiki/Glicko_rating_system for details.

(defconst +glicko-q-constant+ (/ (log 10) 400))
(defconst +glicko-q-squared+ (sq +glicko-q-constant+))
(defconst +glicko-P-constant+ (* 3 (sq (/ +glicko-q-constant+ pi))))
(defconst +glicko-max-RD+ 350)
(defconst +glicko-RD-restore-time+ 100 "Days") ;; was 14 previously
(defconst +glicko-typical-RD+ 50)
(defconst +glicko-c-constant+
  (sqrt (/ (- (sq +glicko-max-RD+)
	      (sq +glicko-typical-RD+))
	   +glicko-RD-restore-time+)))
(defconst +glicko-init-rating+ (list 1500 +glicko-max-RD+))

(cl-defun glicko-update-RD (old-RD &optional time-since-last-rating)
  "Update the glicko ratings deviation.
If TIME-SINCE-LAST-RATING is not nil, update OLD-RD. Else, just
return OLD-RD. This method guarantees to return a number of type
float."
  (if time-since-last-rating
    (sqrt (+ (sq old-RD) (* (sq +glicko-c-constant+) time-since-last-rating)))
    (float old-RD)))
;;(glicko-update-RD +glicko-typical-RD+ +glicko-RD-restore-time+)
;;(glicko-update-RD +glicko-typical-RD+)

(cl-defun glicko-g-function (RD)
  "Returns a float"
  (/ 1 (sqrt (1+ (* +glicko-P-constant+ (sq RD))))))
;;(mapcar #'glicko-g-function '(30 100 300))
;;(glicko-rating '(1500 200) '(((1400 30) 1) ((1550 100) 0) ((1700 300) 0)))

(cl-defun glicko-expected-score (players-rating opponents-rating
				 &optional (opponents-RD 0))
  "Returns a float"
  (/ 1 (1+ (expt 10 (/ (* (glicko-g-function opponents-RD)
			  (- players-rating opponents-rating))
		       -400)))))
;;(glicko-expected-score -169 -475)
;;(glicko-expected-score -169 -475 3.5)
;;(glicko-expected-score -169 -470 3.5)
;;(glicko-expected-score -169 -360 14)
;;(glicko-expected-score -169 -300 3.5)
;;(* .88 .85)
;;(glicko-expected-score 29 487 3.5)
;;(glicko-expected-score 117.2 35.1 51)

(cl-defun glicko-expected-opponents-rating
    (players-rating opponents-rating opponents-RD)
  "Returns a float"
  (/ 1 (1+ (expt 10 (/ (* (glicko-g-function opponents-RD)
			  (- players-rating opponents-rating))
		       -400)))))
;;(glicko-expected-score 1500 1300 50)

(cl-defun glicko-d-squared (expected-scores opponent-g-values)
  "The lists opponent-g-values and expected-scores containts the
g-value and expected score, respectively, for the opponents"
  (/ 1 +glicko-q-squared+ (cl-loop for g in opponent-g-values
				   for e in expected-scores
				   sum (* (sq g) e (- 1 e)))))
;;(glicko-d-squared '(.5) '(.94))

(cl-defun glicko-expected-scores (original-rating opponent-ratings opponents-RDs)
  "Helper function"
  (cl-mapcar #'(lambda (r RD) (glicko-expected-score original-rating r RD))
	     opponent-ratings opponent-RDs))

(cl-defun glicko-reorder (player matches)
  "Tranforms PLAYER and MATCHES to a list
(player-rating player-RD ratings RDs scores)"
  (append player (transpose (cut (flatten matches) 3))))
;;(glicko-reorder '(1600 50) '(((1500 30) 1) ((1700 20) .5) ((2000 10) 0)))
;;(glicko-reorder '(1600 50) '((1500 30) 1))

(cl-defun glicko-rating (player matches &optional time)
  "Returns a list consiting of PLAYERs new rating and RD after
playing MATCHES, which is either a single MATCH or a list of
MATCHes. A MATCH is a pair (OPPONENT SCORE). Both PLAYER and
OPPONENT is a pair (RATING RD) which indicates the PLAYER's or
opponent's strength. SCORE is a real number in the interval [0
1]. Optional TIME is the number of days since last time PLAYERs
rating was calculated. It is used to modify PLAYERs RD due to the
TIME passed since his last activity."
  (cl-destructuring-bind (original-rating
			  original-RD
			  opponent-ratings
			  opponent-RDs scores)
      (glicko-reorder player matches)
    (let* ((RD (glicko-update-RD original-RD time))
	   (g-values (mapcar #'glicko-g-function opponent-RDs))
	   (expected-scores (glicko-expected-scores
			     original-rating opponent-ratings opponent-RDs))
	   (new-RD (sqrt (/ 1 (+ (/ 1 (sq RD))
				 (/ 1 (glicko-d-squared
				       expected-scores g-values))))))
	   (new-rating (+ original-rating
			  (* +glicko-q-constant+
			     (sq new-RD)
			     (cl-loop for g in g-values
				      for s in scores
				      for e in expected-scores
				      sum (* g (- s e)))))))
      (list new-rating new-RD))))
;;(glicko-rating '(1500 200) '(((1400 30) 1) ((1550 100) 0) ((1700 300) 0)))

(provide 'glicko)
