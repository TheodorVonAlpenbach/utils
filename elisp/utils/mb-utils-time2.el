;;;; Time utils library --- A new approach 

;;;; The time utils in Emacs are more powerful than I thought. This
;;;; new approach will expolit that. Also, it will strive for a more
;;;; consistent handling of time representations.

;;;; ISO time. For parsing date-to-time will be used as the core
;;;; parser. Note the following:

;;;; 1. Date only is not allowed. I have to write a ISO date parser
;;;; for this.

;;;; 2. date-to-time is sensible to time zones: 
;;;; a. 2018-05-24 12:00  is interpreted as local time
;;;; b. 2018-05-24T12:00  is interpreted as UTC (!)
;;;; c. 2018-05-24 12:00Z is interpreted as UTC
;;;; d. 2018-05-24 12:00+0300 is interpreted as time zone +3
;;;;
;;;; Of these b. is doubtfull. The time zone interpretation should not
;;;; depend on whether the T separator is present or not.
;;;;
;;;; The mb-thing handling of dates should have its own regexps.
;;;; 
;;;; Note that leap seconds are not supported on some Emacs Lisp plattforms (for instance Win32/Win64)
;;;; It would rely on current-time, encode-time etc (check this!)
;;;; A correction for this would be easy to implement, just incorporate the data found 
;;;; http://maia.usno.navy.mil/ser7/tai-utc.dat
;;;; See also http://tycho.usno.navy.mil/leapsec.html
;;;; You can check if leap seconds are supported by invoking the method leap-seconds-supported-p

(require 'mb-utils-sets)
(require 'mb-utils-div)
(require 'mb-lists)

(defmacro with-timezone (timezone-name &rest body)
  "Temporarily set local timezone to TIMEZONE-NAME and evaluate BODY."
  `(prog1
       (condition-case ()
	   (progn
	     (setenv "TZ" ,timezone-name)
	     ,@body)
	 (error nil))
     (setenv "TZ" nil)))
(def-edebug-spec with-timezone t)

(defun --time-encode (dtime &optional universal)
  "Apply `encode-time' on DTIME. If UNIVERSAL is not nil, discard
time zone offset in DTIME."
  (if universal
    (time-add (apply #'encode-time dtime) (list 0 (ninth dtime)))
    (apply #'encode-time dtime)))
;;(--time-encode (decode-time) t)

(defun clean-dtime (dtime)
  (decode-time (--time-encode dtime)))

(cl-defun --add-ddate (dtime &key (year 0) (month 0) (week 0) (day 0))
  ;; Add time parts to decoded time DTIME. Adding years, months,
  ;; weeks, and days should not be affected by change in daylight
  ;; saving time status.
  (let ((ddtime (clean-dtime (append (subseq dtime 0 3)
				     (cl-mapcar #'+
				       (list (+ day (* 7 week)) month year)
				       (subseq dtime 3 6))
				     (subseq dtime 3)))))
    ;; Replace second, minute, hour by original values. Probably, it
    ;; is enough to modify hour, but what the heck...
    (setf ddtime (append (subseq dtime 0 3) (subseq ddtime 3)))
    ddtime))

(cl-defun --add-dtime (dtime &key (hour 0) (minute 0) (second 0))
  ;; Add time parts to decoded time DTIME. Helper function for this
  ;; module. Adding years, months, weeks, and days is not affected by
  ;; change in daylight saving time status.
  (append (cl-mapcar #'+
	    (list second minute hour)
	    (subseq dtime 0 3))
	  (subseq dtime 3)))
;;(--add-dtime (decode-time) :year 1)

(cl-defun add-etime-time (etime &rest args)
  "Add time parts to encoded time ETIME.

Keywords supported: :day :hour :minute :second
\n(fn ETIME [KEYWORD VALUE]...)"
  (--time-encode (apply #'--add-dtime (decode-time etime) args)))

(cl-defun add-etime-date (etime &rest args)
  "Add date parts to encoded time ETIME.
Even if the daylight saving time status in the result is
different from the status of etime, the time part of the latter
is not changed by the function.

Keywords supported: :year :month :week :day
\n(fn ETIME [KEYWORD VALUE]...)"
  (--time-encode (apply #'--add-ddate (decode-time etime) args)))

(cl-defun add-time (etime &key (year 0) (month 0) (week 0) (day 0)
			   (hour 0) (minute 0) (second 0))
  "Add date and time parts to encoded time ETIME.
This function is implemented for back compablity with the old
add-time function only. Otherwise, you should use either
add-etime-time or add-etime-date for clarity, see below.

The function is identical to first applying add-etime-date and
then applying add-etime-time to the result.

The problem with combining date and time additions, is that the
order is not arbitrary due to possible changes in daylight
savings status as a consequence of changing time. For instance,
the second after 2018-04-25T02:59:59CET is
2018-04-25T02:00:00CEST. Consequenly, 2018-04-25T02:00:00CET
becomes ofter 1 hour 2018-04-25T02:00:00CEST! On the other hand,
adding one day to 2018-04-24T12:00:00CET will bring you to
2018-04-25T12:00:00CET even if the time difference is in fact 25
hours in this case. Now, adding first one hour to
2018-04-25T02:00:00CET and then adding one whole day, results in
time 2018-04-26T02:00:00CEST. On the other hand, starting again
with 2018-04-25T02:00:00CET, adding first one whole day, and then
one hour leads to 2018-04-26T03:00:00CEST.

Hence the order of applying add-etime-date and add-etime-time is
not arbitrary. You should therefore clarify which order you
actually intend by using these two funcion instead of this.

Keywords supported: :year :month :week :day :hour :minute :second
\n(fn ETIME [KEYWORD VALUE]...)"
  (add-etime-time
   (add-etime-date etime :year year :month month :week week :day day)
   :hour hour :minute minute :second second))

(cl-indent 'add-etime 'prog1)
;;(iso-dttm (add-etime (parse-time "2018-03-24 12:00") :day 10))
;;(format-time-string "%T%z" (add-etime (date-to-time "2000-10-28 12:00") :hour 1))
;;(format-time-string "%T%z" (date-to-time "2000-10-28 12:00CET"))
;;(add-etime "13:12" :minute 1)
;;(add-etime '(0 0 0 28 10 2001 0 t 7200) :day 1)
;;(add-etime "1990-12-31T22:59:59" :second 2 :hour 2)
(format-time-string "%z" (date-to-time "2018-05-24T12:00:00"))

;;; Parser functions
(defalias 'copy-time #'copy-list)

(defun append-timezone-regexp (zone)
  "Create a regular expression matching a date ending with ZONE's code.
ZONE is a pair (ZONE-CODE . UTC-OFFSET). The result is on the form
\(ZONE-CODE UTC-OFFSET ZONE-REGEXP\)."
  (destructuring-bind (zone-code . zone-offset) zone
      (list zone-code zone-offset (format "\\(%s\\)$" zone-code))))

(require 'timezone)
(defvar *mb-time-zones*
  (mapcar #'append-timezone-regexp
    (append '(("Z"    .  +0000)
	      ("UTC"  .  +0000)
	      ("CET"  .  +0100)
	      ("CEST" .  +0200))
	    timezone-world-timezones)))

(defun clean-time-zone-suffix (string)
  (or (loop for (code offset regexp) in *mb-time-zones*
	    if (string-match regexp string)
	    return (concat (substring string 0 (- (length code)))
			   (format "%+05d" offset)))
      (awhen (string-match* (iso-time-zone-regexp) string :num '(0 1 2 3))
	(destructuring-bind (all sign h m) it
	  (format "%s%s%s%s"
	    (substring string 0 (- (length all)))
	    sign h (or m "00"))))
      ;; Code suffix not found
      string))
;;(clean-time-zone-suffix "1972-01-05T23:00+01:00")

(defun mb-parse-time-string (string)
  "Parse date STRING and convert it to an encoded time object."

  ;; Implementation note: The function relies on built-in function
  ;; `date-to-time'. This function, however, interprets an ISO 8601
  ;; timestring '<date>T<time>' as UTC and '<date> <time>' as local
  ;; time. According to English Wikipedia:

  ;;   \"If no UTC relation information is given with a time
  ;;   representation, the time is assumed to be in local time.\".

  ;; This function therefore replaces 'T' with a space before calling
  ;; `date-to-time'.
  (if (< (length string) 11)
    (mb-parse-time-string (concat string " 00:00"))
    (when (eql (char string 10) ?T)
      (setf (char string 10) ? ))
    (date-to-time (clean-time-zone-suffix string))))
;;(decode-time (mb-parse-time-string "2018-05-24T00:00+0100"))

(defun parse-time (time-designator)
  "Returns a new time object equal to TIME-DESIGNATOR.
Argument may be a time objects itself or a string."
  (typecase time-designator
    (cons (if (= (length time-designator) 9)
	    (encode-time time-designator)
	    (copy-time time-designator)))
    (string (mb-parse-time-string time-designator))
    (symbol (parse-time (symbol-name time-designator)))
    (error "%s is not a legar time designator")))
;;(mapcar #'parse-time (list "2005-01-18 09:15" '2014-04-02))
;;(parse-time "2000-01-18T22:31")

;;; Notion of time extension: time extension atoms are time points
;;; (points in timespace) or time intervals (intervals with time point
;;; limits). A time extension is a time extension atom or a list of
;;; time extensions.

;;; Eg. 
;;; (decode-time) - time point atom
;;; (interval-cc (decode-time t1) (decode-time t2)) - time interval atom
;;; '((decode-time) time-ext1 '(time-ext2 time-ext3)))

(defconst *the-creation* (encode-time 0 0 0 1 1 1970 0)
  "The earliest date that the Emacs manual guarantees will work
  on every system.")
;;(decode-time *the-creation*)

(defconst *the-apocalypse* (encode-time 0 0 0 31 12 2147485547 0)
  "The latest date that encode-time and decode-time can handle on
  my current system.")
;;(decode-time *the-apocalypse*)

(defun the-creation () (copy-time *the-creation*))
(defun the-apocalypse () (copy-time *the-apocalypse*))

;; Note that the leap second inserted as 1990-12-31T23:59:60 is not 
;; implemented in encode-time:
;;(encode-time 59 59 22 31 12 1990 0) ==> (10111 53503)
;;(encode-time 0 0 1 1 1 1991 0) ==> (10111 53504)
(defun leap-seconds-supported-p ()
  (let* ((decoded-time-before-leap-second '(59 59 23 31 12 1990 0))
	 (decoded-time-after-leap-second '(0 0 0 1 1 1991 0))
	 (encoded-time-before-leap-second (apply #'encode-time decoded-time-before-leap-second))
	 (encoded-time-after-leap-second (apply #'encode-time decoded-time-after-leap-second)))
    (eq 2 (apply #'- (mapcar #'second (list encoded-time-after-leap-second
					    encoded-time-before-leap-second))))))
;;(leap-seconds-supported-p)

;; reference times
(defmacro encode-now ()
  `(apply #'encode-time (decode-time)))
;;(encode-now)

(defun iso-dttm (etime &optional universal)
  (format-time-string "%Y-%m-%dT%H:%M:%S%Z" etime universal))
;;(iso-dttm (encode-now) t)

(cl-defun now (&rest args)
  "Return current time as an encoded time object."
  (if (null args)
    (encode-now)
    (warn "Calling `now' with arguments has been deprecated. Use `add-etime-date' or `add-etime-time' for modifying time, instead.")
    (apply #'add-etime (encode-now) args)))
;;(iso-dttm (now :year 1 :month 10 :day 3 :hour 16 :minute 21 :second 30))

(cl-defun midnight (&optional (time (now)) universal)
  "Return the first time point within the date of local TIME in local time.
If UNIVERSAL is not nil, return the `midnight' in UTC."
  (--time-encode (fill (decode-time time) 0 :end 3) universal))
;;(iso-dttm (midnight (parse-time "1972-01-06")))

(cl-defun midday (&optional (time (now)) universal)
  "Return the time point 12 hours into the date of TIME in local time.
If UNIVERSAL is not nil, return the `midday' in UTC."
  (add-etime-time (midnight time universal) :hour 12))
;;(iso-dttm (midday))

(defalias 'noon #'midday)
;;(iso-dttm (noon (parse-time "2000-09-07")))

(cl-defun morning (&optional (time (now)) universal)
  "Return the mean time point within the date of local TIME.
If UNIVERSAL is not nil, return the `midday' in UTC."
  (add-etime-time (midnight time universal) :hour 6))
;;(iso-dttm (morning (parse-time "1972-01-06")))

(cl-defun evening (&optional (time (now)) universal)
  (add-etime-time (midnight time universal) :hour 18))

(defun weekday-number (weekday-designator)
  "Return the weekday number corresponding to WEEKDAY-DESIGNATOR.
Possible values of WEEKDAY-DESIGNATOR
are :SUNDAY, :MONDAY, :TUESDAY, :WEDNESDAY,:THURSDAY, :FRIDAY,
and :SATURDAY."
  (position weekday-designator '(:sunday :monday :tuesday :wednesday
				 :thursday :friday :saturday)))

(cl-defun weekstart (&optional (etime (now)) (start-weekday :monday))
  "Return encoded time of the start of the week ETIME is in.
ETIME must be an encoded time object, see `encode-time'. Default
is current time. A second optional parameter defines the start
day of the week."
  (let ((dtime (decode-time etime)))
    (midnight (apply #'encode-time
		(--add-dtime dtime
			     :day (- (mod (- (seventh dtime)
					     (weekday-number start-weekday))
					  7)))))))
;;(iso-dttm (weekstart (now) :sunday))

(cl-defun monthstart (&optional (etime (now)) old-etime)
  "Return encoded time of the start of the month ETIME is in.
Optional argument OFFSET shifts the result OFFSET number of
months. Negative OFFSET shifts result backward in time. ETIME
must be an encoded time object, see `encode-time'. Default is
current time. A second optional parameter defines the start day
of the week."
  (if (integerp etime)
    ;; Old version (MONTHSTART OFFSET ETIME) is now deprecated
    (progn (warn "This way of calling monthstart is deprecated!")
	   (add-etime-date (monthstart (or old-etime) (now)) :month etime))
  (let ((res (midnight etime)))
    (destructuring-bind (year-offset month-offset)
	(cl-truncate (+ (mbt-month res) offset) 12)
      (setf (mbt-day res) 1)
      (setf (mbt-month res) month-offset)
      (incf (mbt-year res) year-offset)
      (setf (mbt-hour res) 0)) ;; due to DLS
    res)))
;;(pp (loop for i in (a-b -4 6) collect (iso-date-and-time :time (monthstart i))))

(cl-defun yearstart (&optional (time (now)))
  (let ((res (monthstart 0 time)))
    (setf (mbt-month res) 1)
    res))
;;(iso-date-and-time :time (yearstart))

;; Other utils
(defun etime< (encoded-time1 encoded-time2)
  "Returns non-nil iff ENCODED-TIME1 comes before ENCODED-TIME2."
  (if (= (first encoded-time1) (first encoded-time2))
    (< (second encoded-time1) (second encoded-time2))
    (< (first encoded-time1) (first encoded-time2))))

(defun time< (time-designator1 time-designator2)
  "Returns non-nil iff TIME1 comes before TIME2."
  (etime< (time-encode time-designator1)
	  (time-encode time-designator2)))
;;(time< (now :day 1 :second -1) (now :day 1 :second -1))

(defun time<= (time-designator1 time-designator2)
  "Returns non-nil iff TIME1 comes before TIME2."
  (let ((et1 (time-encode time-designator1))
	(et2 (time-encode time-designator2)))
    (or (etime< et1 et2)
	(equal et1 et2))))
;;(time<= (now) (now))

;; Time units
(lexical-let* ((2^16 (expt 2 16))
	       (s 1.0) (m (* 60 s)) (h (* 60 m)) (d (* 24 h)) 
	       (w (* 7 d)) (mo (* 30 d)) (y (* 365.24 d)))

  (defun unit-factor (unit)
    (case unit
      (:second s) (:minute m) (:hour h) (:day d) (:week w)
      (:month mo) (:year y) (:olympiad (* 4 y)) 
      (:decennium (* 10 y)) (:century (* 100 y))
      (:millenium (* 1000 y))
      (otherwise (error "Unknown unit."))))

  (cl-defun time- (time-designator1 time-designator2 &key (unit :day))
    "Converts TIME-DESIGNATOR to an integer counted from REFERANCE-TIME with
RESOLUTION as unit \(= 1\)."
    (let ((diff (mapcar* #'- (time-encode time-designator1) (time-encode time-designator2)))
	  (u (unit-factor unit)))
      (+ (* (/ 2^16 u) (first diff))
	 (/ (second diff) u))))
  ;;(* (time- (now :day 6 :hour 25) (now) :unit :week) 168)

  (cl-defun change-time-unit (time &key (from-unit :second) (to-unit :second))
    (* time (/ (unit-factor from-unit) (unit-factor to-unit)))))
;;(change-time-unit 3 :from-unit :minute)

(cl-defun time-map (time-designator &key (referance-time-designator (now)) (unit :day))
  "Converts TIME-DESIGNATOR to an integer counted from
REFERANCE-TIME-DESIGNATOR. UNIT is one of
\(:YEAR :MONTH :WEEK :DAY :MINUTE :SECOND\). Note that the time
is rounded. Eg. 3 :DAYs is 0 :WEEKs, and 4 :DAYs is 1:WEEK."
  (round (time- time-designator referance-time-designator :unit unit)))
;;(time-map (now :week 0 :day 4) :unit :week)
;;(time-map '2014-03-01 :unit :week)

;; Time formatting
(cl-defun iso-date (&optional (date (decode-time)))
  "Returns DATE (default is today) in iso string format"
  (format "%04d-%02d-%02d" (mbt-year date) (mbt-month date) (mbt-day date)))
;;(iso-date (now :year 1))

(cl-defun weekday (&optional (lang :no) (time-designator (decode-time)))
  "Returns weekday of TIME-DESIGNATOR (default is today) in language LANG"
    (nth (mbt-day-of-week (parse-time time-designator))
       (second (assoc lang *weekdays*))))
;;(loop for i below 7 collect (weekday :no (now :day i)))
;;(weekday :no '2014-04-02)

(cl-defun full-date (&optional (lang :no) (time-designator (decode-time)))
  "Returns TIME-DESIGNATOR (default is today) in full date format in language LANG
TODO: the iso-date part should be changed to corresond with the language"
  (let ((time (parse-time time-designator)))
    (format "%s %s" (weekday lang time) (iso-date time))))
;;(loop for lang in '(:en :no nil) collect (full-date lang '2015-01-09))

(cl-defun short-date (&optional (time-designator (decode-time)))
  (interactive)
  "Returns DATE (default is today) in short string format.
TODO: add lang parameter?"
  (let ((time (parse-time time-designator)))
    (format "%d.%d" (mbt-day time) (mbt-month time))))
;;(short-date '2014-04-02)

(cl-defun iso-time (&key (time (decode-time)) (with-seconds nil))
  "Returns time designator TIME (default is now) in iso string format"
  (let ((time (parse-time time)))
    (if with-seconds 
      (format "%02d:%02d:%02d" (mbt-hour time) (mbt-minute time) (mbt-second time))
      (format "%02d:%02d" (mbt-hour time) (mbt-minute time)))))
;;(iso-time :with-seconds t :time '2014-04-02)

(cl-defun iso-date-and-time (&key (time (decode-time)) (with-seconds nil))
  "Prints time designator TIME in full ISO date and time format"
  (let ((time (parse-time time)))
    (concat (iso-date time) "T" (iso-time :time time :with-seconds with-seconds))))
;;(iso-date-and-time :time '2014-04-02T22:25 :with-seconds t)

;;;; Time intervals (also called a periods)
(cl-defun period (&key (from (the-creation)) (to (the-apocalypse)))
  "Creates a time interval [FROM TO), where FROM and TO are
TIMEs, not time designators."
  (interval-co from to))
;;(period :from (today))

(defun period-to-string (p &optional format)
  (format "[%s %s)"
    (iso-date-and-time :time (interval-l p))
    (iso-date-and-time :time (interval-r p))))
;;(period-to-string (period))

(defconst *always* (period :from (the-creation) :to (the-apocalypse)))

(defun period-move (period &rest args)
  "Returns a copy of period. The period may be moved in time. See
  `time-add' for ARGS"
  (period :from (period)))

(defun today (&rest args)
  "Period from last midnight to next midnight. The period may be moved
using the keyword parameters of NOW."
  (let ((from (apply #'add-time (midnight) args)))
    (period :from from :to (add-time from :day 1))))
;;(period-to-string (today :week 1))

;;; Reference periods
(defun tomorrow () (today :day 1))
(defun yesterday () (today :day -1))

;;; Period queries
(defun within-period (time period) 
  (within time period))
;;(within-period (now) (period :from (midnight (now :year -1)) :to (now :second 1)))
;;(setf debug-on-error t)
;;(debug-on-entry 'within-period)

(defun within-time (time time-extension) 
  (if (interval-p time-extension)
    (within time time-extension :test #'time<)
    (and time-extension
	 (or (within-time time (first time-extension))
	     (within-time time (rest time-extension))))))
;;(within-time (now :hour 35) `(,(tomorrow) ,(today)))

;;;; Utils for debugging
(defun diff-current-time-msec (current-time1 current-time2)
  "Returns the difference between CURRENT-TIME1 and CURRENT-TIME2
in milliseconds. Note that arguments are not times in the (second
minute ... ) format, but in that same format as is returned by
`current-time'"
  (+ (* 1000 (+ (* (expt 2 16) (- (first current-time2) 
				  (first current-time1)))
	     (- (second current-time2) (second current-time1))))
     (/ (- (third current-time2) (third current-time1)) 1000)))
;;(let ((x (current-time))) (loop for i below (expt 10 6)) (diff-current-time-msec x (current-time)))

(defvar *mb-time-reference* 0
  "Time reference (ms). Used by `time-set-reference' and
`time-elapsed'.")

(defun time-set-reference ()
  "Resets reference time (ms) . Used in connection with
`time-elapsed.' Eg. `time-set-reference' is inserted before some body
of expressions, and `time-elapsed' after it."
  (setq *mb-time-reference* (current-time)))

(cl-defun time-elapsed (&optional (print-result-p t))
  "Returns time elapsed (ms) since `*mb-time-reference*'. If
PRINT-RESULT-P (default T) is T the time is also printed with
`princ'."
  (let ((res (diff-current-time-msec *mb-time-reference* (current-time))))
    (if print-result-p (princ res) res)))

(defmacro time (expression) 
  "Evaluates EXPRESSION and prints the evaluation time (ms) ."
  (let ((time-val (gensym)))
    `(let ((,time-val (time-val ,expression)))
       (princ (format "Time spent: %d (ms); Result: " (first ,time-val)))
       (second ,time-val))))

(defmacro time-val (expression) 
  "Returns '\(TIME VALUE\) where TIME is the time (ms) it takes to evaluate
EXPRESSION and VALUE is the corresponding evaluation value."
  (let ((current-time-start (gensym))
	(result (gensym)))
    `(let ((,current-time-start (current-time))
	   (,result ,expression)) ;unwind-?
       (values (diff-current-time-msec ,current-time-start
				       (current-time))
	       ,result))))
;;(fourth (first *dic-db*))

(cl-defun time-map-iv (time-iv &rest args)
  "Converts TIME to an integer counted from REFERANCE-TIME. UNIT
is one of \(:year :month :week :day :minute :second\). Default
UNIT is :day."
  (list (apply #'time-map (interval-l time-iv) args)
	(apply #'time-map (interval-r time-iv) args)))
;;(time-map-iv (tomorrow))

(cl-defun american-date-regexp (&optional (short-form nil))
  "Year matches named regexp 1, month 2, day 3"
  (let* ((y (iso-year-regexp))
	(m (concat* (loop for i in (0-n 12)
			  collect (month-name i :en short-form))
		    :in "\\|"))
	(d (regexp-opt (mapcar #'number-to-string (1-n 31))))
	(M-d-y (format "\\(?2:%s\\)[[:space:]]+\\(?3:%s\\)[[:space:],]+\\(?1:%s\\)" m d y))
	(d-M-y (format "\\(?3:%s\\)[[:space:]]+\\(?2:%s\\)[[:space:]]+\\(?1:%s\\)" d m y)))
    (format "%s\\|%s" M-d-y d-M-y)))
;;(string-match* (american-date-regexp) "May 5, 3333" :num '(1 2 3))
;;(string-match* (american-date-regexp) "5 May 3333" :num '(1 2 3))

(defun parse-american-date (s)
  "This is not quite right yet. It only converts the date to iso"
  (if (empty-string-p s)
    s
    (multiple-value-bind (y m d)
      (string-match* (american-date-regexp) s :num '(1 2 3))
    (format "%s-%02d-%02d" y (position m (second (assoc :en *months*)) :test #'equal) (string-to-number d)))))
;;(parse-american-date "December 29, 1995")
;;(parse-american-date "1 December 1973")
;;(parse-american-date "December 21, 1968")

(defun parse-american-date-lines (beg end)
  (interactive "r")
  (let* ((old-lines (string-to-lines (buffer-substring beg end)))
	 (new-lines (mapcar #'parse-american-date old-lines))
	 (new-region (concat* new-lines :in "\n")))
    (kill-region beg end)
    (insert new-region)))

(defun first-week-start (year)
  "Returns the date of the monday in the first week in YEAR.
This function is mainly a helper for `week-number'"
  (let ((jan-4 (make-time :date (list year 1 4))))
    (add-time jan-4 :day (- (weekday :m0 jan-4)))))
;;(iso-date (first-week-start 2014))

(defun week-number-1 (time-designator)
  (let* ((time (parse-time time-designator)) 
	 (year (mbt-year time))
	 (fws (first-week-start year)))
    (when (time< time fws)
      (setf fws (first-week-start (1- year)))
      (decf year))
    (let ((candidate (1+ (floor (time- time fws :unit :week)))))
      (if (< candidate 53)
	(values candidate year)
	(multiple-value-bind (week year) (week-number-1 (add-time time :week 1))
	  (if (= week 2)
	    (values 1 year)
	    (values 53 year)))))))
;;(week-number-1 '1993-01-03)

(cl-defun week-number (&optional (time-designator (now)))
  (first (week-number-1 time-designator)))
;;(week-number '2009-12-31)
;;(week-number)

(defun week-year (time-designator)
  (let* ((time (parse-time time-designator))
	 (week (week-number time))
	 (year (mbt-year time))
	 (month (mbt-month time)))
    (case month
      (1 (if (> week 32) (1- year) year))
      (12 (if (> week 32) year (1+ year)))
      (t year))))
;;(week-year '1958-12-31)

(defun test-week-number ()
  "Test function for `week-number'.
Test data are taken from http://no.wikipedia.org/wiki/Ukenummer"
 (let ((test-data '((1993-01-03 53 1992) (1993-01-04 01 1993) (1997-12-28 52 1997) (1997-12-29 01 1998) (1998-01-04 01 1998) (1998-01-01 01 1998)
		    (1998-12-31 53 1998) (1992-01-01 01 1992) (1992-02-29 09 1992) (1992-12-31 53 1992) (1975-01-01 01 1975) (1975-12-28 52 1975) (1975-12-29 01 1976)
		    (1958-12-10 50 1958) (1958-12-17 51 1958) (1958-12-24 52 1958) (1958-12-31 1 1959) (1959-01-01 1 1959))))
   (loop for x in test-data
	 for date = (first x)
	 for correct-week = (second x)
	 for correct-year = (third x)
	 do (assert (and (= (week-number date) correct-week)
			 (= (week-year date) correct-year)) 
		    t (format "Couldn't parse date %S" date))
	 finally (return 'success))))
;;(test-week-number)

(defun number-of-weeks (year)
  "Returns the number of weeks in YEAR"
  (let ((candidate (week-number (list year 12 31))))
    (if (= candidate 1)
      52 candidate)))

(defun parse-week (week-designator)
  "WEEK-DESIGNATOR is either a string on format 'yyyy-mm' or a list (year week)"
  (cl-etypecase week-designator
    (symbol (parse-week (symbol-name week-designator)))
    (string (mapcar #'string-to-number (split-string week-designator "-")))
    (list week-designator)))
;;(mapcar #'parse-week '("2000-02" "2000-22" (2000 32) 2000-42))

(defun week- (week1 week2)
  "Calculates the time difference, in :week units between week designators WEEK1 and WEEK2.
See `parse-week' for definition of week designator."
  (destructuring-bind ((y1 w1) (y2 w2))
      (list (parse-week week1) (parse-week week2))
    (round (+ (time- (first-week-start y1) (first-week-start y2) :unit :week)
	      (- w1 w2)))))
;;(week- '1996-12 '1995-37)

(defun clean-encoded-time (etime)
  "Cleans etime"
  (loop with floors = (cons '(0 0) (mapcar #'cl-floor etime))
	for floor1 in floors
	for floor2 in (rest floors)
	for expt-args in '((1 1) (2 16) (10 6) (10 6))
	collect (round (+ (* (apply #'expt expt-args)
			     (second floor1))
			  (first floor2)))))

(cl-defun interpolate-time (x period &key (a 0) (b 1))
  "Returns the time corresponding to X such that X in [A B)
  corresponds linearly to the result in PERIOD"
  (let ((eperiod (mapcar #'time-encode (interval-list period))))
    (decode-time 
     (clean-encoded-time
      (cl-mapcar #'+ (cl-mapcar (bind #'* (/ (- x a) (- b a))) 
				(apply #'cl-mapcar #'- eperiod))
		 (first eperiod))))))
;;(interpolate-time .5 (today))

(defun unix-time (time-designator)
  "Convert time-designator to the number of seconds since 1970-01-01 UTC.
This function was implemented before I knew about `float-time'.
TODO: use `float-time' directly on decoded time, instead of
calling `iso-to-unix-time'. The latter will then become
obsolete."
  (iso-to-unix-time (iso-date-and-time :time time-designator)))
;;(unix-time "2018-05-22")
;;(float-time (apply #'encode-time (subseq (parse-time "2018-05-22") 0 6)))

(defun iso-to-unix-time (utc-iso-time)
  "Convert UTC-ISO-TIME to the number of seconds since 1970-01-01 UTC.
This function was implemented before I knew about `float-time'.
TODO: use `float-time' instead of calling shell util."
  (string-to-number
   (call-process-shell-command* "date" "-d" utc-iso-time "+%s")))
;;(iso-to-unix-time "2018-05-22")

;;;; examples
;;(add-time "2000-10-29" :day 1)
;;(add-time (now) :day 1)
;;(now :iso-date "2000-10-31")
;;(iso-date (now :year 2 :month -12 :day -365))
;;(decode-time (apply #'encode-time '(0 0 0 1 1 2038 5 t 7200)))

(provide 'mb-utils-time)