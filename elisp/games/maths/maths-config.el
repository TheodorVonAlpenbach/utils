(defconst +maths-buffer+ "Matematikkspill for Ludvik")

(defvar *maths-auto-continue* nil)

(defvar *maths-task-range* ;(nilf *maths-task-range*)
  '((:addition (1 3))
    (:substraction (2 3) nil)
    (:division (2 3))
    (:multiplication (2 3)))
  "List of possible task operators to be asked about. Each task
type is entered as a pair \(TYPE \(FROM-LEVEL UPTO-LEVEL\) ALLOW-NEGATIVE-RESULT-P\), where
TYPE denotes the task type and the pair a closed integer interval
that specifies the possible levels for that TYPE.")

(provide 'maths-config)

