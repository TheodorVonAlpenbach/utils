(defpackage :elapsed
  (:use :cl :local-time)
  (:export :elapsed :format-elapsed :iso-week-number-of :midnight :weekstart))

(in-package :elapsed)


(defun iso-week-number-of (time)
  "Return an integer that equals the week number of TIME according to ISO 8601."
  (multiple-value-bind (week)
      (parse-integer (format-timestring nil time :format '(:iso-week-number)))
    week))
;;(iso-week-number-of (now))

(defun midnight (&optional (time (now)))
  "Return the earliest UTC timestamp in the day of TIME.
Note that the result will be converted to the local time."
  (make-instance 'timestamp :day (day-of time)))

(defun weekstart (time &optional (week-start-day :monday))
  "Return the timestamp of first day in the week of TIME.
WEEK-START-DAY defines the weekday starting the week. Possible values
for WEEK-START-DAY are :MONDAY, :TUESDAY, :WEDNESDAY, :THURSDAY, :FRIDAY,
:SATURDAY, and :SUNDAY. The default is :MONDAY."
  (midnight (adjust-timestamp time
	      (offset :day-of-week week-start-day)
	      (offset :day -7))))

(defun elapsed (time &optional (unit :sec) (reference (now)))
  "Return the amount of time in UNITs from REFERENCE to TIME.
Supported UNITs are :SEC, :MIN, :HOUR, :DAY, :WEEK, :MONTH, :YEAR,
:DECADE, :CENTURY, and :MILLENNIUM."
  (case unit
    (:sec (- (timestamp-to-unix time) (timestamp-to-unix reference)))
    (:min (floor (elapsed time :sec reference) 60))
    (:hour (floor (elapsed time :sec reference) 3600))
    (:day (- (day-of time) (day-of reference)))
    (:week (/ (- (day-of (weekstart time))
		 (day-of (weekstart reference)))
	      7))
    (:month (+ (- (timestamp-month time) (timestamp-month reference))
	       (* 12 (elapsed time :year reference))))
    (:year (- (timestamp-year time) (timestamp-year reference)))
    (:century (- (timestamp-century time) (timestamp-century reference)))
    (:decade (- (timestamp-decade time) (timestamp-decade reference)))
    (:millennium (- (timestamp-millennium time) (timestamp-millennium reference)))
    (t (error "Unknown unit ~a" unit))))

(defun format-elapsed (time &key (reference (now)) stream)
  (cond ((timestamp< time reference)
	 (error "TIME cannot be before REFERENCE"))
	((timestamp= time reference)
	 (format stream "Now"))
	((< (elapsed time :sec reference) 60)
	 (format stream "A few seconds ago"))
	((< (elapsed time :min reference) 60)
	 (format stream "~d minute~:p ago" (elapsed time :min reference)))
	((= (elapsed time :day reference) 0) ; today
	 (format stream "~d hour~:p ago" (elapsed time :hour reference)))
	((= (elapsed time :day reference) 1) ; yesterday
	 (format stream "Yesterday"))
	((= (elapsed time :month reference) 0) ; this month
	 (format stream "~d day~:p ago" (elapsed time :day reference)))
	((= (elapsed time :month reference) 1) ; last month
	 (format stream "Last month"))
	((= (elapsed time :year reference) 0) ; this year
	 (format stream "~d month~:p ago" (elapsed time :month reference)))
	((= (elapsed time :year reference) 1) ; last year
	 (format stream "Last year"))
	(t
	 (format stream "~d year~:p ago" (elapsed time :year reference)))))
;;(format-elapsed @3000000020-01-31T00:00:00 :reference @2018-02-16T12:00:00)
