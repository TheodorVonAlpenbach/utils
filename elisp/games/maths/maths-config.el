;;; maths-config.el --- Configuration for Maths -*- lexical-binding: t; -*-

;;; Code:

(require 'glicko)

(defconst +maths-buffer+ "Matematikkspill for Ludvik")

(defconst +maths-default-rating+ (car +glicko-init-rating+))
(defconst +maths-default-RD+ (cadr +glicko-init-rating+))
(defconst +maths-default-user-name+ "Mats")
(defconst +maths-default-rating-window+ 200)

(defvar *maths-auto-continue* nil)

(defvar *maths-task-range* ;(nilf *maths-task-range*)
  '((:addition (1 3))
    (:substraction (2 3) nil)
    (:division (2 3))
    (:multiplication (2 3)))
  "Possible task operators and their level ranges.
Each entry has the form (TYPE (FROM-LEVEL UPTO-LEVEL)
ALLOW-NEGATIVE-RESULT-P).")

(provide 'maths-config)

;;; maths-config.el ends here
