(defpackage :rating
  (:use :cl :mb-utils)
  (:export :rating :make-rating :estimate :rd :new-rating :new-rd))

(in-package :rating)

(defclass rating ()
  ((estimate :initarg :estimate
	     :accessor estimate)
   (rd :initarg :rd
       :accessor rd)))

(defun make-rating (estimate rd)
  (make-instance 'rating :estimate estimate :rd rd))

(defmethod print-object ((x rating) stream)
  (print-unreadable-object (x stream :type t)
    (prin1 (list :estimate (estimate x) :rd (rd x)) stream)))

(defconstant +glicko-q-constant+ (/ (log 10) 400))
(defconstant +glicko-q-squared+ (sq +glicko-q-constant+))
(defconstant +glicko-p-constant+ (* 3 (sq (/ +glicko-q-constant+ pi))))
(defconstant +glicko-max-rd+ 350)
(defconstant +glicko-rd-restore-time+ 14 "Days")
(defconstant +glicko-typical-rd+ 50)
(defconstant +glicko-c-squared+
  (/ (- (sq +glicko-max-rd+)
	(sq +glicko-typical-rd+))
     +glicko-rd-restore-time+))

;;;; Update rating RD after elapsed TIME 
(defmethod new-rd (rd time)
  (sqrt (+ (sq rd) (* +glicko-c-squared+ time))))

(defun glicko-g-function (rd)
  "Helper function."
  (/ 1 (sqrt (1+ (* +glicko-p-constant+ (sq rd))))))

(defun glicko-expected-score (rating opponent-rating &aux (strange-constant -400))
  "Return the expected score for a player with RATING
facing a player with OPPONENT-RATING.
The score is a float in the interval [0 1].
TODO: why the hardcoded number -400? Check this out."
  (/ 1 (1+ (expt 10 (/ (* (glicko-g-function (rd opponent-rating))
			  (- (estimate rating)
			     (estimate opponent-rating)))
		       strange-constant)))))

(defun glicko-d-squared (expected-scores opponent-g-values)
  "The lists opponent-g-values and expected-scores containts the
g-value and expected score, respectively, for the opponents"
  (/ 1 +glicko-q-squared+ (loop for g in opponent-g-values
				for e in expected-scores
				sum (* (sq g) e (- 1 e)))))

(defun glicko-d-squared1 (expected-score opponent-g-value)
  (/ 1 +glicko-q-squared+ (* (sq opponent-g-value)
			     expected-score (- 1 expected-score))))
;;(glicko-d-squared '(.5) '(.94))

(defmethod new-rating ((rating rating) (opponent-rating rating) score)
  (let ((g-value (glicko-g-function (rd opponent-rating)))
	(expected-score (glicko-expected-score rating opponent-rating)))
    (let ((new-rd (sqrt (/ 1 (+ (/ 1 (sq (rd rating)))
				(/ 1 (glicko-d-squared1
				      expected-score g-value)))))))
      (make-rating (+ (estimate rating)
		      (* +glicko-q-constant+ (sq new-rd)
			 g-value (- score expected-score)))
		   new-rd))))

