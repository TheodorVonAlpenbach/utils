;;;; Calculate day of week by John Conway's Doomsday Rule
;;;; Sunday = 0, Monday = 1, ..., Saturday = 6.
;;;;
;;;; See https://en.wikipedia.org/wiki/Doomsday_rule

(cl-defun anchor-day (century)
  (mod (+ 2 (* 5 (mod century 4))) 7))
;;(mapcar #'anchor-day (a-b 16 24))

(cl-defun doomsday (y)
  (destructuring-bind (cc yy) (cl-floor y 100)
    (+ (anchor-day cc)
       (destructuring-bind (a b) (cl-floor yy 12)
	 (mod (+ a b (/ b 4)) 7)))))
;;(doomsday 2023)

(cl-defun doomsday-reference (y m)
  (let ((ref (nth (1- m) '(3 28 0 4 9 6 11 8 5 10 7 12))))
    (if (and (< m 3) (zerop (mod y 4))) (1+ ref) ref)))
;;(doomsday-reference 2023 1)

(cl-defun doomsday-day-of-week (y m d)
  (mod (+ (doomsday y) (- d (doomsday-reference y m))) 7))
;;(doomsday-day-of-week 2006 11 2)
