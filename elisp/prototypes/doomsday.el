;;;; Calculate day of week by John Conway's Doomsday Rule
;;;; Sunday = 0, Monday = 1, ..., Saturday = 6.
;;;;
;;;; See https://en.wikipedia.org/wiki/Doomsday_rule

(defun anchor-day (century)
  (mod (+ 2 (* 5 (mod century 4))) 7))
;;(mapcar #'anchor-day (a-b 16 24))

(defun doomsday (y)
  (destructuring-bind (cc yy) (cl-floor y 100)
      (+ (anchor-day cc)
      (destructuring-bind (a b) (cl-floor yy 12)
	(mod (+ a b (/ b 4)) 7)))))
;;(doomsday 1985)

(defun doomsday-reference (y m)
  (let ((ref (nth (1- m) '(3 28 0 4 9 6 11 8 5 10 7 12))))
    (if (and (< m 3) (zerop (mod y 4))) (1+ ref) ref)))
;;(doomsday-reference 2020 1)

(defun doomsday-day-of-week (y m d)
  (mod (+ (doomsday y) (- d (doomsday-reference y m))) 7))
;;(doomsday-day-of-week 1972 1 6)
