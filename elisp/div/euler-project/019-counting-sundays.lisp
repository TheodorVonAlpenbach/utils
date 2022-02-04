(require 'date-utils "~/git/utils/elisp/div/euler-project/date-utils.lisp")

(defun first-sundays-month (year first-weekday-year)
  "Return number of Sundays in YEAR given that the first day in YEAR
is FIRST-WEEKDAY-YEAR (0 is Sunday, 1 is Monday and so forth)."
  (count 0 (first-weekday-in-month year first-weekday-year)))
;;(first-sundays-month 2022 6)

(defun 019-solution (&optional (a 1900) (b 2001) (first-weekday-year 0))
  (loop for y from a to b
	sum (first-sundays-month y first-weekday-year)
	do (setf first-weekday-year
		 (mod7 (+ first-weekday-year (days-in-year y))))))
;;(time (019-solution))
;; => 176
