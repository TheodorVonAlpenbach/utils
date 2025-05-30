(cl-defun sm-incorrect-p (grade)
  (< grade 3))

(cl-defun sm-new-state ()
  (list 0 2.5 1))

(cl-defun sm-2 (grade state)
  "Return new state from user GRADE and old STATE \(N EASY-FACTOR INTERVAL\).
INTERVAL unit is days by default
TODO: add interval unit"
  (cl-destructuring-bind (n easy-factor interval) state
    (if (sm-incorrect-p grade)
      (setf n 0 interval 1)
      (setf interval (cl-case n
		       (0 1)
		       (1 6)
		       (t (round (* interval easy-factor)))))
      (cl-incf n))
    (let ((nq (- 5 grade)))
      (cl-incf easy-factor (- 0.1 (* nq (+ 0.08 (* nq 0.02))))))
    (list n (max easy-factor 1.3) interval)))

(cl-defun sm-2-1 (grade state)
  "SM-2 with float INTERVAL"
  (cl-destructuring-bind (n easy-factor interval) state
    (if (sm-incorrect-p grade)
      (setf n 0 interval 1)
      (mincf interval easy-factor)
      (cl-incf n))
    (let ((nq (- 5 grade)))
      (cl-incf easy-factor (- 0.1 (* nq (+ 0.08 (* nq 0.02))))))
    (list n (max easy-factor 1.3) interval)))
;;(cl-loop for grade below 6 collect (sm-2 grade (list 2 2.7 1)))


(provide 'sm-2)

