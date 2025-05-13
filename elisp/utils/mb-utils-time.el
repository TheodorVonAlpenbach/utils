;; -*- lexical-binding: t; -*-

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

(defalias 'dtime-second 'first)
(defalias 'dtime-minute 'second)
(defalias 'dtime-hour 'third)
(defalias 'dtime-day 'fourth)
(defalias 'dtime-month 'fifth)
(defalias 'dtime-year 'sixth)
(defalias 'dtime-day-of-week 'seventh) ;;0 is Sunday ...
(defalias 'dtime-daylight-saving-time-p 'eighth)
(defalias 'dtime-time-zone 'ninth)

(cl-defun etime-second (x) (dtime-second (decode-time x)))
(cl-defun etime-minute (x) (dtime-minute (decode-time x)))
(cl-defun etime-hour (x) (dtime-hour (decode-time x)))
(cl-defun etime-day (x) (dtime-day (decode-time x)))
(cl-defun etime-month (x) (dtime-month (decode-time x)))
(cl-defun etime-year (x) (dtime-year (decode-time x)))
(cl-defun etime-day-of-week (x) (dtime-day-of-week (decode-time x)))
(cl-defun etime-daylight-saving-time-p (x) (dtime-daylight-saving-time-p (decode-time x)))
(cl-defun etime-time-zone (x) (dtime-time-zone (decode-time x)))

(defmacro with-timezone (timezone-name &rest body)
  "Temporarily set local timezone to TIMEZONE-NAME and evaluate BODY."
  `(let ((currentenv-tz (getenv "TZ")))
     (prog1
	 (condition-case ()
	     (progn
	       (setenv "TZ" ,timezone-name)
	       ,@body)
	   (error nil))
       (setenv "TZ" currentenv-tz))))
(def-edebug-spec with-timezone t)
;;(getenv "TZ")

(cl-defun set-timezone (timezone-name)
  (setenv "TZ" timezone-name))
;;(set-timezone nil)

(cl-defun current-timezone ()
  (or (getenv "TZ") (second (current-time-zone))))
;;(current-timezone)

(cl-defun --time-encode (dtime &optional universal-p)
  "Apply `encode-time' on DTIME. If UNIVERSAL-P is not nil, discard
time zone offset in DTIME."
  (if universal-p
    (time-add (apply #'encode-time dtime) (list 0 (dtime-time-zone dtime)))
    (apply #'encode-time dtime)))
;;(--time-encode (decode-time) t)

(cl-defun clean-dtime (dtime)
  (decode-time (--time-encode dtime)))

(cl-defmacro without-tz-modification ((dtime expr) &rest body)
  "Ensure time part of BODY's result is the same as for DTIME.

\(without-tz-modification (DTIME DTIME-EXPRESSION) BODY...)

DTIME is bound to the result of evaluating DTIME-EXPRESSION. BODY
is assumed to return a decoded time object. If the date is in
another time zone than DTIME, the `decode-time' will change the
time part accordingly. But for some time operations, like
`yearstart', `monthstart', `weekstart', `add-etime-date' these
time part changes are not wanted.

For instance, we usually want noon of yesterday to be 12:00 even
if there was a change in daylight saving time status during the
night. Without using something like this macro `decode-time' will
return the time exactly 24 hours ago, which in this case will not
be at noon.

Warning! As with all macros of this type, be careful to use
destructive functions where DTIME is involved. For instance, if
use set DTIME-EXPRESSION to

\(nconc '(0 0 0 1 1) (subseq dtime 5))

then most likely the result will be not what you inteded to since
`nonc' will probably alter DTIME, and the calulations in the
macro involving DTIME will produce unwanted results. Instead, in
this particular case, use the non-destructive `append', or wrap the expression with a `setf':
\(progn
   (setf (nconc '(0 0 0 1 1) (subseq dtime 5)))
   dtime)"
  (let ((gdtime-res (gensym)))
    `(let* ((,dtime ,expr)
	    (,gdtime-res (clean-dtime (progn ,@body))))
       (--add-dtime ,gdtime-res
		    :second (- (dtime-time-zone ,dtime)
			       (dtime-time-zone ,gdtime-res))))))
(def-edebug-spec without-tz-modification body)

(cl-defun --add-ddate (dtime &key (year 0) (month 0) (week 0) (day 0))
  ;; Add time parts to decoded time DTIME. Adding years, months,
  ;; weeks, and days should not be affected by change in daylight
  ;; saving time status.
  (without-tz-modification (dtime dtime)
    (let ((d (+ (+ day (* 7 week)) (dtime-day dtime)))
	  (m (+ month (dtime-month dtime)))
	  (y (+ year (dtime-year dtime))))
      (replace-sequence dtime (list d m y) 3 6))))
;;(--add-ddate (decode-time (parse-time "2018-03-25T12:00")) :day -1)

(cl-defun --add-dtime (dtime &key (hour 0) (minute 0) (second 0))
  ;; Add time parts to decoded time DTIME. Helper function for this
  ;; module. Adding years, months, weeks, and days is not affected by
  ;; change in daylight saving time status.
  (append (cl-mapcar #'+
	    (list second minute hour)
	    (subseq dtime 0 3))
	  (subseq dtime 3)))
;;(--add-dtime (decode-time) :hour 1)


(cl-defun add-etime-time (etime &rest args)
  "Add time parts to encoded time ETIME.

Keywords supported: :day :hour :minute :second
\n(fn ETIME [KEYWORD VALUE]...)"
  (--time-encode (apply #'--add-dtime (decode-time etime) args)))
;;(add-etime-time (now) :hour -1)

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
(defalias 'copy-time #'cl-copy-list)

(cl-defun append-timezone-regexp (zone)
  "Create a regular expression matching a date ending with ZONE's code.
ZONE is a pair (ZONE-CODE . UTC-OFFSET). The result is on the form
\(ZONE-CODE UTC-OFFSET ZONE-REGEXP\)."
  (cl-destructuring-bind (zone-code . zone-offset) zone
      (list zone-code zone-offset (format "\\(%s\\)$" zone-code))))

(require 'timezone)
(defvar *mb-time-zones*
  (mapcar #'append-timezone-regexp
    (append '(("Z"    .  +0000)
	      ("UTC"  .  +0000)
	      ("CET"  .  +0100)
	      ("CEST" .  +0200))
	    timezone-world-timezones)))

(cl-defun clean-time-zone-suffix (string)
  (or (cl-loop for (code offset regexp) in *mb-time-zones*
	    if (string-match regexp string)
	    return (concat (substring string 0 (- (length code)))
			   (format "%+05d" offset)))
      (awhen (string-match* (iso-time-zone-regexp) string :num '(0 1 2 3))
	(cl-destructuring-bind (all sign h m) it
	  (format "%s%s%s%s"
	    (substring string 0 (- (length all)))
	    sign h (or m "00"))))
      ;; Code suffix not found
      string))
;;(clean-time-zone-suffix "1972-01-05")
;;(date-to-time "1972-01-06T00:00+0000")
;;(date-to-time "1972-01-06T00:00+0100")
;;(date-to-time "1972-01-06")
;;(clean-time-zone-suffix "1972-01-05T23:00Z")
;;(clean-time-zone-suffix "1972-01-05T23:00+01:00")

(cl-defun mb-parse-date-string (string)
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
    (date-to-time string)
    (date-to-time (clean-time-zone-suffix string))))

(cl-defun parse-time (time-designator)
  "Returns a new time object equal to TIME-DESIGNATOR.
Argument may be a time objects itself or a string."
  (cl-typecase time-designator
    (cons (if (= (length time-designator) 9)
	    (--time-encode time-designator)
	    (copy-time time-designator)))
    (string (mb-parse-date-string time-designator))
    (symbol (parse-time (symbol-name time-designator)))
    (number (seconds-to-time time-designator))
    (otherwise (error "%s is not a legar time designator"))))
;;(mapcar #'parse-time (list 1527598870.823139 "2005-01-18 09:15" '2014-04-02))

(cl-defun parse-ms (ms)
  "Converts an integer representing unix milliseconds to new time object."
  (parse-time (/ (cl-coerce ms 'float) 1000)))
;;(iso-dttm (parse-ms 1643018173697))

;;; Notion of time extension: time extension atoms are time points
;;; (points in timespace) or time intervals (intervals with time point
;;; limits). A time extension is a time extension atom or a list of
;;; time extensions.

;;; Eg. 
;;; (decode-time) - time point atom
;;; (interval-cc (decode-time t1) (decode-time t2)) - time interval atom
;;; '((decode-time) time-ext1 '(time-ext2 time-ext3)))

(defconst +the-creation+ (encode-time 0 0 0 1 1 1970 0)
  "The earliest date that the Emacs manual guarantees will work
  on every system.")
;;(decode-time +the-creation+)

(defconst +the-apocalypse+
  (if (cygwin-emacs-p)
      (encode-time 0 0 0 31 12 2147483647 0)
      (encode-time 0 0 0 31 12 2147485547 0))
  "The latest date that encode-time and decode-time can handle on
  my current system.")
;;(decode-time +the-apocalypse+)

(cl-defun the-creation () (copy-time +the-creation+))
(cl-defun the-apocalypse () (copy-time +the-apocalypse+))

;; Note that the leap second inserted as 1990-12-31T23:59:60 is not 
;; implemented in encode-time:
;;(encode-time 59 59 22 31 12 1990 0) ==> (10111 53503)
;;(encode-time 0 0 1 1 1 1991 0) ==> (10111 53504)
(cl-defun leap-seconds-supported-p ()
  (let* ((decoded-time-before-leap-second '(59 59 23 31 12 1990 0))
	 (decoded-time-after-leap-second '(0 0 0 1 1 1991 0))
	 (encoded-time-before-leap-second (apply #'encode-time decoded-time-before-leap-second))
	 (encoded-time-after-leap-second (apply #'encode-time decoded-time-after-leap-second)))
    (eq 2 (apply #'- (mapcar #'second (list encoded-time-after-leap-second
					    encoded-time-before-leap-second))))))
;;(leap-seconds-supported-p)

(cl-defun --dtime-set-time (dtime second minute hour)
  "Set the time parameters in ETIME to SECOND, MINUTE, and HOUR,
regardless of this changes the status of daylight saving time
status."
  (without-tz-modification (dtime dtime)
    (replace-sequence dtime (list second minute hour) 0 3)))

(cl-defun --etime-set-time (etime second minute hour universal-p)
  "Set the time parameters in ETIME to SECOND, MINUTE, and HOUR,
regardless of this changes the status of daylight saving time
status. If UNIVERSAL-P is not nil, return result in UTC."
  (--time-encode (--dtime-set-time (decode-time etime) second minute hour)
		 universal-p))
;;(midnight (parse-time "1972-01-05T23:00CET"))
;;(decode-time (parse-time "1972-01-05T23:00CET"))
;;(decode-time (parse-time "1972-01-05T23:00Z"))

;; Reference times
(defmacro encode-now ()
  `(apply #'encode-time (decode-time)))
;;(encode-now)

(cl-defun iso-dttm (etime &optional universal-p)
  "Convert ETIME to an ISO 8601 time string.
If UNIVERSAL-P is not nil, return result in UTC."
  (format-time-string "%Y-%m-%dT%H:%M:%S%Z" etime universal-p))
;;(iso-dttm (list 968 61552))
;;(mapcar #'iso-dttm (list (parse-time (seconds-to-time (float-time))) (encode-now)))

(cl-defun etime-part (etime part &optional universal-p)
  "Return the time PART of ETIME. The supported parts are :YEAR,
:MONTH, :DAY, :HOUR, :MINUTE, :SECOND. If UNIVERSAL-P is not nil,
return result in UTC. This only affects the parts :HOUR and
:MINUTE."
  (string-to-number
   (format-time-string
    (cl-case part
      (:year "%Y") (:month "%m") (:day "%d")
      (:hour "%H") (:minute "%M") (:second "%S"))
    etime universal-p)))
;;(etime-part (parse-time "1972-01-06T08:15:17") :hour)

(cl-defun etime-round-1 (etime part quantity &optional universal-p)
  "Helper for `etime-round', one iteration only.

TODO: need to handle base 1 of the month and day cases.

\(Also, initutition tells me that there must be simpler
implementation.)"
  (let* ((etime-part (etime-part etime part universal-p))
	 (diff (- (funcall (if (minusp quantity)
			     #'next-smaller-multiple #'next-greater-multiple)
		    etime-part (abs quantity))
		  etime-part)))
    (message "%d" diff)
    (when (and (zerop diff) (plusp quantity))
      (cl-incf diff quantity))
    (cl-case part
      (:year (yearstart (add-etime-date etime part diff)))
      (:hour (hourstart (add-etime-time etime part diff)))
      (:minute (minutestart (add-etime-time etime part diff)))
      (:second (secondstart (add-etime-time etime part diff)))
      (t (error "Part %S is not supported" part)))))
;;(iso-dttm (etime-round-1 (parse-time "1972-01-06T08:15:10") :year 1))
;;(iso-dttm (etime-round-1 (parse-time "1972-01-06T08:15:10") :hour 1))
;;(iso-dttm (etime-round-1 (parse-time "1972-01-06T08:15:10") :minute -15))

(cl-defun time-part-p (part)
  (cl-case part
    ((:hour :minute :second) t)
    ((:year :month :day) nil)
    (otherwise (error "Unknown part %S" part))))

(cl-defun etime-round (etime part quantity &optional (n 1) universal-p)
  "Rounds ETIME up or down to the closest multiple of quantity if part
is positive or negative. For furhter information on PART and
UNIVERSAL-P, see `etime-part'.

Note that PARTs :MONTH and :DAY are currently not supported."
  (cl-assert (not (minusp n)) t "N cannot be negative")
  (if (zerop n)
    etime
    (if (time-part-p part)
      (add-etime-time
       (etime-round-1 etime part quantity universal-p)
       part (* (if (minusp n) n (1- n)) quantity))
      (add-etime-date
       (etime-round-1 etime part quantity universal-p)
       part (* (if (minusp n) n (1- n)) quantity)))))
;;(iso-dttm (etime-round (parse-time "1972-01-06T08:15:17") :hour -1 1))
;;(iso-dttm (etime-round (parse-time "1972-01-06T08:15:17") :minute 15 1))

(cl-defun now (&rest args)
  "Return current time as an encoded time object."
  (if (null args)
    (--time-encode (decode-time))
    (warn (concat "Calling `now' with arguments has been deprecated.\n "
		  "Use `add-etime-date' or `add-etime-time' "
		  "for modifying time, instead."))
    (apply #'add-etime-time (--time-encode (decode-time)) args)))
;;(now :hour -1)

(cl-defun midnight (&optional (etime (now)) universal-p)
  "Return the first time point within the date of ETIME in local time.
If UNIVERSAL-P is not nil, return the `midnight' in UTC."
  (--etime-set-time etime 0 0 0 universal-p))
;;(iso-dttm (midnight (parse-time "1972-01-06")))

(cl-defun midday (&optional (etime (now)) universal-p)
  "Return the time point 12 hours into the date of ETIME in local time.
If UNIVERSAL-P is not nil, return the `midday' in UTC."
  (--etime-set-time etime 0 0 12 universal-p))
;;(iso-dttm (midday))

(defalias 'noon #'midday)
;;(iso-dttm (noon (parse-time "2000-09-07")))

(cl-defun morning (&optional (etime (now)) universal-p)
  "Return the mean time point within the date of ETIME in local time.
If UNIVERSAL-P is not nil, return the `midday' in UTC."
  (--etime-set-time etime 0 0 6 universal-p))
;;(iso-dttm (morning (parse-time "1972-01-06")))

(cl-defun evening (&optional (etime (now)) universal-p)
  "Return the mean time point within the date of ETIME in local time.
If UNIVERSAL-P is not nil, return the `midday' in UTC."
  (--etime-set-time etime 0 0 18 universal-p))

(cl-defun next-midnight (&optional (etime (now)) universal-p)
  "Return the first time point after the date of ETIME in local time.
If UNIVERSAL-P is not nil, return the `next-midnight' in UTC."
  (--etime-set-time etime 0 0 24 universal-p))
;;(list (midnight (parse-time "2018-03-25")) (next-midnight (parse-time "2018-03-25")))
;;(next-midnight (parse-time "2018-03-25"))
;;(midnight (parse-time "2018-03-26"))

(cl-defun yearstart (&optional etime)
  (--time-encode (without-tz-modification (dtime (decode-time etime))
		   (message "%S" (subseq dtime 5))
		   (message "%S" dtime)
		   (append '(0 0 0 1 1) (subseq dtime 5)))))
;;(iso-dttm (yearstart (parse-time "1972-01-06")))

(cl-defun yearend (&optional etime)
  (add-etime-time (yearstart etime) :second -1))
;;(iso-dttm (yearend (parse-time "1972-01-06")))

(cl-defun monthstart (&optional etime old-etime)
  "Return encoded time of the start of the month ETIME is in.
Optional argument OFFSET shifts the result OFFSET number of
months. Negative OFFSET shifts result backward in time. ETIME
must be an encoded time object, see `encode-time'. Default is
current time. A second optional parameter defines the start day
of the week."
  (if (integerp etime)
    ;; Old version (MONTHSTART OFFSET ETIME) is now deprecated
    (progn (warn "This way of calling monthstart is deprecated!")
	   (add-etime-date (monthstart (or old-etime (now))) :month etime))
    (--time-encode (without-tz-modification (dtime (decode-time etime))
		     (append '(0 0 0 1) (subseq dtime 4))))))
;;(iso-dttm (monthstart (parse-time "1972-01-06")))
;;(iso-dttm (--time-encode (clean-dtime (decode-time (parse-time "1972-01-06")))))
;;(iso-dttm (monthstart (parse-time "2018-03-31")))

(cl-defun weekday-number (weekday-designator)
  "Return the weekday number corresponding to WEEKDAY-DESIGNATOR.
Possible cl-values of WEEKDAY-DESIGNATOR
are :SUNDAY, :MONDAY, :TUESDAY, :WEDNESDAY,:THURSDAY, :FRIDAY,
and :SATURDAY."
  (cl-position weekday-designator
    '(:sunday :monday :tuesday :wednesday :thursday :friday :saturday)))

(cl-defun weekstart (&optional (etime (now)) (start-weekday :monday))
  "Return encoded time of the start of the week ETIME is in.
ETIME must be an encoded time object, see `encode-time'. Default
is current time. A second optional parameter defines the start
day of the week."
  (let ((dtime (decode-time etime)))
    (midnight (--time-encode
		(--add-ddate dtime
			     :day (- (mod (- (dtime-day-of-week dtime)
					     (weekday-number start-weekday))
					  7)))))))
;;(iso-dttm (weekstart (now) :sunday))

(cl-defun daystart (&optional etime)
  (--time-encode (without-tz-modification (dtime (decode-time etime))
		   (append '(0 0 0) (subseq dtime 3)))))
;;(iso-dttm (daystart (parse-time "1972-01-06")))

(cl-defun hourstart (&optional etime)
  (--time-encode (without-tz-modification (dtime (decode-time etime))
		   (append '(0 0) (subseq dtime 2)))))
;;(iso-dttm (hourstart (parse-time "1972-01-06T08:15")))

(cl-defun minutestart (&optional etime)
  (--time-encode (without-tz-modification (dtime (decode-time etime))
		   (append '(0) (subseq dtime 1)))))
;;(iso-dttm (minutestart (parse-time "1972-01-06T08:15:17")))

(cl-defun secondstart (&optional etime)
  etime)
;;(iso-dttm (minutestart (parse-time "1972-01-06T08:15:17")))

;; Other utils
(cl-defun etime< (encoded-time1 encoded-time2)
  "Returns non-nil iff ENCODED-TIME1 comes before ENCODED-TIME2."
  (if (= (first encoded-time1) (first encoded-time2))
    (< (second encoded-time1) (second encoded-time2))
    (< (first encoded-time1) (first encoded-time2))))

(cl-defun time< (time-designator1 time-designator2)
  "Returns non-nil iff TIME1 comes before TIME2."
  (etime< (parse-time time-designator1)
	  (parse-time time-designator2)))

(cl-defun time<= (time-designator1 time-designator2)
  "Returns non-nil iff TIME1 comes before TIME2."
  (not (time< time-designator2 time-designator1)))

;;;; Time units
(let* ((2^16 (expt 2 16))
       (s 1.0) (m (* 60 s)) (h (* 60 m)) (d (* 24 h))
       (w (* 7 d)) (mo (* 30 d)) (y (* 365.24 d)))
  (defun unit-factor (unit)
    (cl-case unit
      (:second s) (:minute m) (:hour h) (:day d) (:week w)
      (:month mo) (:year y) (:olympiad (* 4 y))
      (:decennium (* 10 y)) (:century (* 100 y))
      (:millenium (* 1000 y))
      (otherwise (error "Unknown unit `%S'." unit))))

  (cl-defun etime/-1 (etime1 etime2 divisor)
    "Divide by DIVISOR the number of seconds represented by an encoded time object ETIME."
    (+ (* etime1 (/ 2^16 divisor))
       (/ etime2 divisor))))

(cl-defun etime/ (etime divisor)
    "Divide by DIVISOR the number of seconds represented by an encoded time object ETIME."
    (if (listp etime)
      (etime/-1 (first etime) (second etime) divisor)
      (etime/-1 0 etime divisor)))
;;(etime/ (now) 1000)

(cl-defun etime- (etime1 etime2 &optional (unit :day))
  "Return the number of days from ETIME2 to ETIME1.
With keyword :UNIT you can specify another time unit for the time
difference. Time units from :MONTH and higher are inambiguous,
and are not recommended."
  (etime/ (time-subtract etime1 etime2) (unit-factor unit)))
;;(etime- (now) 0 :year)

(cl-defun change-time-unit (time &key (from-unit :second) (to-unit :second))
  (* time (/ (unit-factor from-unit) (unit-factor to-unit))))
;;(change-time-unit 3 :from-unit :minute)

(cl-defun time- (time-designator1 &optional (time-designator2 (now)) (unit :day))
  "Return the number of days from TIME-DESIGNATOR2 to TIME-DESIGNATOR1.
If optional TIME-DESIGNATOR2 is ommitted the period is calculated
from `now'. With optional argument UNIT you can specify another
time unit for the time difference. Time units from :MONTH and
higher are inambiguous, and are not recommended."
  (etime- (parse-time time-designator1) (parse-time time-designator2) unit))
;;(time- (add-etime-date (now) :day 1))

;;;; Time intervals (also called a periods)
(cl-defun period (&key (from (the-creation)) (to (the-apocalypse)))
  "Creates a time interval [FROM TO), where FROM and TO are
TIMEs, not time designators."
  (interval-co from to))
;;(period :from (now) :to (add-etime-date (now) :day 1))

(cl-defmacro period-bind ((a b) period &rest body)
  `(let ((,a (interval-l ,period))
	 (,b (interval-r ,period)))
     ,@body))
;;(period-bind (a b) (today) (list a b))
(def-edebug-spec period-bind ((symbolp symbolp) form body))

(cl-defun iso-period (period)
  "Format time PERIOD in accordance with ISO 8601."
  (period-bind (a b) period
    (format "%s/%s" (iso-dttm a) (iso-dttm b))))

(cl-defun period-to-string (p &optional format)
  (warn "period-to-string is deprecated. Use `iso-period' instead."))

(cl-defun period-length (period &optional (unit :second))
  (period-bind (a b) period
    (etime- b a unit)))
;;(period-length (today))

;;; Some standard periods
(defconst +always+ (period :from (the-creation) :to (the-apocalypse)))

(cl-defun today (&optional etime)
  "Period from midnight of ETIME to the following midnight.
The default value of the optional parameter ETIME, is `(now)'."
  (period :from (midnight etime) :to (next-midnight etime)))
;;(period-length (today (parse-time "2018-03-25")) :hour)
;;(etime- (next-midnight (parse-time "2018-03-25")) (midnight (parse-time "2018-03-25")) :hour)

(cl-defun this-week (&optional etime)
  "Period from week start at ETIME to the following week start.
Week start is defined as in `weekstart'. The default value of the
optional parameter ETIME, is `(now)'."
  (let ((a (weekstart etime)))
    (period :from a :to (add-etime-date a :day 7))))
;;(period-to-string (this-week))

;;; Derived periods
(cl-defun tomorrow (&optional etime) (today (add-etime-date etime :day 1)))
(cl-defun yesterday (&optional etime) (today (add-etime-date etime :day -1)))

;;; Period queries
(cl-defun within-period-p (time period) 
  (within time period :test #'etime<))
;;(within-period-p (now) (period :from (midnight) :to (next-midnight)))
;;(setf debug-on-error t)
;;(debug-on-entry 'within-period-p)

(cl-defun within-time (time time-extension) 
  "Generalization of `within-period-p'.
TIME-EXTENSION is a list of either time points or time
intervals."
  (if (interval-p time-extension)
    (within time time-extension :test #'time<)
    (and time-extension
	 (or (within-time time (first time-extension))
	     (within-time time (rest time-extension))))))
;;(within-time (now :hour 35) `(,(tomorrow) ,(today)))

(cl-defun interpolate-time (x period &key (a 0) (b 1) (unit :second))
  "Returns the time corresponding to X such that X in [A B)
  corresponds linearly to the result in PERIOD"
  (add-time (interval-l period)
	    :second (round (* (/ (- x a) (- b a))
			      (period-length period :second)))))
;;(iso-dttm (interpolate-time .5 (today)))

;;;; Utils for debugging
(cl-defun diff-current-time-msec (current-time1 current-time2)
  "Returns the difference between CURRENT-TIME1 and CURRENT-TIME2
in milliseconds. Note that arguments are not times in the (second
minute ... ) format, but in that same format as is returned by
`current-time'"
  (+ (* 1000 (+ (* (expt 2 16) (- (first current-time2) 
				  (first current-time1)))
	     (- (second current-time2) (second current-time1))))
     (/ (- (third current-time2) (third current-time1)) 1000)))
;;(let ((x (current-time))) (cl-loop for i below (expt 10 6)) (diff-current-time-msec x (current-time)))

;;;; Elapsed time
(defvar *mb-time-reference* 0
  "Time reference (ms). Used by `time-set-reference' and
`time-elapsed'.")

(cl-defun time-set-reference ()
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
       (cl-values (diff-current-time-msec ,current-time-start
				       (current-time))
	       ,result))))
;;(fourth (first *dic-db*))

(cl-defun american-date-regexp (&optional (short-form nil))
  "Year matches named regexp 1, month 2, day 3"
  (let* ((y +iso-year-regexp+)
	(m (concat* (cl-loop for i in (0-n 12)
			  collect (month-name i :en short-form))
		    :in "\\|"))
	(d (regexp-opt (mapcar #'number-to-string (1-n 31))))
	(M-d-y (format "\\(?2:%s\\)[[:space:]]+\\(?3:%s\\)[[:space:],]+\\(?1:%s\\)" m d y))
	(d-M-y (format "\\(?3:%s\\)[[:space:]]+\\(?2:%s\\)[[:space:]]+\\(?1:%s\\)" d m y)))
    (format "%s\\|%s" M-d-y d-M-y)))
;;(string-match* (american-date-regexp) "May 5, 3333" :num '(1 2 3))
;;(string-match* (american-date-regexp) "5 May 3333" :num '(1 2 3))

(cl-defun parse-american-date (s)
  "This is not quite right yet. It only converts the date to iso"
  (if (empty-string-p s)
    s
    (cl-multiple-value-bind (y m d)
	(string-match* (american-date-regexp) s :num '(1 2 3))
      (format "%04d-%02d-%02d"
	(string-to-number y)
	(1+ (position m (second (assoc :en *months*)) :test #'equal))
	(string-to-number d)))))
;;(parse-american-date "December 29, 1995")
;;(parse-american-date "1 December 1973")
;;(parse-american-date "December 21, 1968")
;;(parse-american-date "January 21, 1968")

(cl-defun parse-american-date-lines (beg end)
  (interactive "r")
  (let* ((old-lines (string-lines (buffer-substring beg end)))
	 (new-lines (mapcar #'parse-american-date old-lines))
	 (new-region (concat* new-lines :in "\n")))
    (kill-region beg end)
    (insert new-region)))

;;;; Week numbers
(require 'mb-utils-iso-time)

(cl-defun first-week-start (year)
  "Returns the date of the monday in the first week in YEAR.
This function is mainly a helper for `week-number'"
  (let ((jan-4 (encode-time 0 0 0 4 1 year)))
    (add-etime-date jan-4 :day (- (weekday :m0 jan-4)))))
;;(iso-dttm (first-week-start 1972))

(cl-defun yearweek-number (&optional (etime (now)))
  "Return (YEAR WEEK) if ETIME is within YEAR's WEEKth week."
  (cl-flet ((weekno (y etime)
	      (list y (round (1+ (time- (weekstart etime) (first-week-start y)
					:week))))))
    (let ((year (etime-year etime)))
      (if (etime< etime (first-week-start year))
	(weekno (1- year) etime)
	(if (etime< (first-week-start (1+ year)) etime)
	  (weekno (1+ year) etime)
	  (weekno year etime))))))
;;(mapcar (compose #'yearweek-number #'parse-time) '(1972-01-01 1972-01-06 2001-12-31 2008-12-29))

(cl-defun week-number (&optional (etime (now)))
  "Return the week number of ETIME."
  (second (yearweek-number etime)))
;;(mapcar (compose #'week-number #'parse-time) '(1972-01-01 1972-01-06 2001-12-31 2008-12-29))

(cl-defun max-week-number (year &optional zone)
  "Returns the greatest week number in YEAR."
  (let* ((etime (encode-time 59 59 23 31 12 year zone))
	 (candidate (week-number etime)))
    (if (= candidate 1)
      (week-number (add-etime-date etime :day -4))
      candidate)))
;;(max-week-number 2001)

(cl-defun parse-week (week-designator)
  "WEEK-DESIGNATOR is either a string on format 'yyyy-mm' or a list (year week)"
  (cl-etypecase week-designator
    (symbol (parse-week (symbol-name week-designator)))
    (string (mapcar #'string-to-number (split-string week-designator "-")))
    (list week-designator)))
;;(mapcar #'parse-week '("2000-02" "2000-22" (2000 32) 2000-42))

(cl-defun week- (week-designator1 week-designator2)
  "Calculates the time difference in :WEEK units between week
designators WEEK-DESIGNATOR1 and WEEK-DESIGNATOR2. See
`parse-week' for definition of week designator."
 (cl-destructuring-bind ((y1 w1) (y2 w2))
      (list (parse-week week-designator1) (parse-week week-designator2))
    (round (+ (time- (first-week-start y1) (first-week-start y2) :unit :week)
	      (- w1 w2)))))
;;(week- '1996-12 '1995-37)

(cl-defun unix-time (&optional (time-designator (now)))
  "Convert time-designator to the number of seconds since 1970-01-01 UTC."
  (float-time (parse-time time-designator)))
;;(cons (unix-time) (mapcar #'unix-time '("2020-03-02T12:26:30")))

(cl-defun ms-unix-time (&optional (time-designator (now)))
  "Convert time-designator to the number of seconds since 1970-01-01 UTC."
  (* 1000 (unix-time time-designator)))
;;(ms-unix-time "2023-11-13T16:00:00")
;;(ms-unix-time)

(cl-defun parse-ms-unix-time (ms)
  "Convert unix-time in milliseconds to ETIME."
  (iso-dttm (parse-time (/ ms 1000))))
;;(parse-ms-unix-time 1681077600000)"2023-04-10T00:00:00CEST"
;;(weekday :no (now))

(cl-defun minutes-to-mmss (minutes)
  (cl-destructuring-bind (mm rest) (cl-floor minutes)
    (cons mm (cl-floor (* 60 rest)))))
;;(minutes-to-mmss )

(cl-defun mmss-to-minutes (mmss)
  (cl-destructuring-bind (mm ss rest) mmss
    (+ mm (/ (+ ss rest) 60.0))))
;;(mmss-to-minutes '(1 29 .99))

(cl-defun hours-to-hhmmss (hours)
  (cl-destructuring-bind (hh rest) (cl-floor hours)
    (cons hh (minutes-to-mmss (* 60 rest)))))
;;(mapcar #'hours-to-hhmmss '(59.9529019 10.7646353))((59 57 10 0.4468400000018846) (10 45 52 0.6870800000004635))
;;(mapcar #'hours-to-hhmmss '(59.914804 10.742518))((59 54 53 0.2943999999878315) (10 44 33 0.06480000000163955))
;;59:54:53N 10:44:33E
;;Taasen skole: 59:57:10N 10:45:53E
;;(cl-floor 59.9529019)

(cl-defun hhmmss-to-hours (hhmmss)
  (cl-destructuring-bind (hh &rest mmss) hhmmss
    (+ hh (/ (mmss-to-minutes mmss) 60.0))))
;;(hhmmss-to-hours '(59 59 59 0))
;;(hhmmss-to-hours '(1 29 59 .99))

(provide 'mb-utils-time)
