(require 'cl-utils "~/git/utils/elisp/div/euler-project/cl-utils.lisp")

(defun leap-year-p (year)
  (or (zerop (mod year 400))
      (and (zerop (mod year 4))
	   (not (zerop (mod year 100))))))
;;(mapcar #'leap-year-p '(1900 1901 1996 1999 2000))

(defun mod7 (x) (mod x 7))

(defparameter *days-in-month* '(31 28 31 30 31 30 31 31 30 31 30 31))
(defparameter *days-in-month-ly* '(31 29 31 30 31 30 31 31 30 31 30 31))
(defparameter *first-weekday-in-month*
  (mapcar #'mod7 (cons 0 (accumulate (butlast *days-in-month*)))))
(defparameter *first-weekday-in-month-ly*
  (mapcar #'mod7 (cons 0 (accumulate (butlast *days-in-month-ly*)))))
(defparameter *days-in-year* (reduce #'+ *days-in-month*))
(defparameter *days-in-leap-year* (reduce #'+ *days-in-month-ly*))

(defun days-in-year (year)
  (if (leap-year-p year) *days-in-leap-year* *days-in-year*))
;;(mapcar #'days-in-year '(1999 2000))

(defun first-weekday-in-month (year &optional first-weekday-year)
  (if first-weekday-year
    (loop for d in (first-weekday-in-month year)
	  collect (mod7 (+ first-weekday-year d)))
    (if (leap-year-p year)
      *first-weekday-in-month-ly* *first-weekday-in-month*)))
;;(loop for y in '(1900 1904) collect (first-weekday-in-month y 6))

(provide 'date-utils)
