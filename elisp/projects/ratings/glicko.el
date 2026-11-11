;;; glicko.el --- Glicko-1 rating calculations -*- lexical-binding: t; -*-

;;; Code:

(require 'cl-lib)

(defun glicko--square (number)
  "Return NUMBER squared."
  (* number number))

(defconst +glicko-q-constant+ (/ (log 10.0) 400.0)
  "Glicko-1 scaling constant.")

(defconst +glicko-q-squared+
  (glicko--square +glicko-q-constant+)
  "Square of `+glicko-q-constant+'.")

(defconst +glicko-P-constant+
  (* 3
     (glicko--square
      (/ +glicko-q-constant+ float-pi)))
  "Constant used by the Glicko-1 attenuation function.")

(defconst +glicko-max-RD+ 350
  "Maximum supported rating deviation.")

(defconst +glicko-RD-restore-time+ 100
  "Days required to restore a typical deviation to the maximum.")

(defconst +glicko-typical-RD+ 50
  "Typical rating deviation used to calibrate restoration.")

(defconst +glicko-c-constant+
  (sqrt
   (/ (- (glicko--square +glicko-max-RD+)
         (glicko--square +glicko-typical-RD+))
      +glicko-RD-restore-time+))
  "Rating-deviation restoration constant.")

(defconst +glicko-init-rating+
  (list 1500 +glicko-max-RD+)
  "Initial rating and rating deviation.")

(defun glicko--rating-p (rating)
  "Return non-nil when RATING is a valid rating/deviation pair."
  (and (listp rating)
       (= (length rating) 2)
       (numberp (car rating))
       (numberp (cadr rating))
       (>= (cadr rating) 0)))

(defun glicko--match-p (match)
  "Return non-nil when MATCH has the form (OPPONENT SCORE)."
  (and (listp match)
       (= (length match) 2)
       (glicko--rating-p (car match))
       (numberp (cadr match))
       (<= 0 (cadr match) 1)))

(defun glicko--normalize-matches (matches)
  "Return MATCHES as a validated list of matches.
Accept either one match or a list of matches."
  (let ((normalized
         (cond
          ((null matches) nil)
          ((glicko--match-p matches) (list matches))
          ((and (listp matches)
                (cl-every #'glicko--match-p matches))
           matches)
          (t
           (user-error
            "Matches must contain ((RATING RD) SCORE) entries")))))
    normalized))

(cl-defun glicko-update-RD
    (old-RD &optional time-since-last-rating)
  "Restore OLD-RD after TIME-SINCE-LAST-RATING days.
Cap the result at `+glicko-max-RD+'."
  (unless (and (numberp old-RD) (>= old-RD 0))
    (user-error "Rating deviation must be non-negative"))
  (when (and time-since-last-rating
             (or (not (numberp time-since-last-rating))
                 (< time-since-last-rating 0)))
    (user-error "Elapsed rating time must be non-negative"))
  (min
   (float +glicko-max-RD+)
   (if time-since-last-rating
       (sqrt
        (+ (glicko--square old-RD)
           (* (glicko--square +glicko-c-constant+)
              time-since-last-rating)))
     (float old-RD))))

(defun glicko-g-function (RD)
  "Return the Glicko-1 attenuation for rating deviation RD."
  (unless (and (numberp RD) (>= RD 0))
    (user-error "Rating deviation must be non-negative"))
  (/ 1.0
     (sqrt
      (+ 1
         (* +glicko-P-constant+
            (glicko--square RD))))))

(cl-defun glicko-expected-score
    (player-rating opponent-rating &optional (opponent-RD 0))
  "Return PLAYER-RATING's expected score against OPPONENT-RATING.
OPPONENT-RD is the opponent's rating deviation."
  (/ 1.0
     (+ 1
        (expt
         10
         (/ (* (- (glicko-g-function opponent-RD))
               (- player-rating opponent-rating))
            400.0)))))

(define-obsolete-function-alias
  'glicko-expected-opponents-rating
  #'glicko-expected-score
  "2026-11-11")

(defun glicko-d-squared (expected-scores opponent-g-values)
  "Return the estimated variance for the supplied opponents."
  (unless (= (length expected-scores)
             (length opponent-g-values))
    (user-error
     "Expected scores and attenuation values must have equal lengths"))
  (let ((information
         (cl-loop
          for attenuation in opponent-g-values
          for expected in expected-scores
          sum
          (* (glicko--square attenuation)
             expected
             (- 1 expected)))))
    (unless (> information 0)
      (user-error "Matches provide no rating information"))
    (/ 1.0 +glicko-q-squared+ information)))

(defun glicko-expected-scores
    (player-rating opponent-ratings opponent-RDs)
  "Return PLAYER-RATING's expected scores against the opponents."
  (unless (= (length opponent-ratings)
             (length opponent-RDs))
    (user-error
     "Opponent ratings and deviations must have equal lengths"))
  (cl-mapcar
   (lambda (rating RD)
     (glicko-expected-score player-rating rating RD))
   opponent-ratings opponent-RDs))

(defun glicko-rating (player matches &optional time)
  "Return PLAYER's new rating/deviation pair after MATCHES.
PLAYER and each opponent are (RATING RD) pairs.  MATCHES may be
one match (OPPONENT SCORE), a list of such matches, or nil.  TIME
is the number of days since PLAYER was last rated."
  (unless (glicko--rating-p player)
    (user-error "Player must be a (RATING RD) pair"))
  (let* ((normalized (glicko--normalize-matches matches))
         (original-rating (car player))
         (RD (glicko-update-RD (cadr player) time)))
    (if (null normalized)
        (list (float original-rating) RD)
      (let* ((opponent-ratings
              (mapcar (lambda (match) (car (car match)))
                      normalized))
             (opponent-RDs
              (mapcar (lambda (match) (cadr (car match)))
                      normalized))
             (scores (mapcar #'cadr normalized))
             (g-values
              (mapcar #'glicko-g-function opponent-RDs))
             (expected-scores
              (glicko-expected-scores
               original-rating
               opponent-ratings
               opponent-RDs))
             (d-squared
              (glicko-d-squared expected-scores g-values))
             (new-RD
              (sqrt
               (/ 1.0
                  (+ (/ 1.0 (glicko--square RD))
                     (/ 1.0 d-squared)))))
             (new-rating
              (+ original-rating
                 (* +glicko-q-constant+
                    (glicko--square new-RD)
                    (cl-loop
                     for attenuation in g-values
                     for score in scores
                     for expected in expected-scores
                     sum
                     (* attenuation
                        (- score expected)))))))
        (list new-rating new-RD)))))

(provide 'glicko)

;;; glicko.el ends here
