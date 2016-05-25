;;;; Time utils library 

;;;; Note that leap seconds are not supported on some Emacs Lisp plattforms (for instance Win32/Win64)
;;;; It would rely on current-time, encode-time etc (check this!)
;;;; A correction for this would be easy to implement, just incorporate the data found 
;;;; http://maia.usno.navy.mil/ser7/tai-utc.dat
;;;; See also http://tycho.usno.navy.mil/leapsec.html
;;;; You can check if leap seconds are supported by invoking the method leap-seconds-supported-p

(require 'mb-utils-sets)
(require 'mb-utils-div)
(require 'mb-lists)

(defalias 'mbt-second 'first)
(defalias 'mbt-minute 'second)
(defalias 'mbt-hour 'third)
(defalias 'mbt-day 'fourth)
(defalias 'mbt-month 'fifth)
(defalias 'mbt-year 'sixth)
(defalias 'mbt-day-of-week 'seventh) ;;0 is Sunday ...
(defalias 'mbt-day-light-saving-time-p 'eighth)
(defalias 'mbt-time-zone 'ninth)

;;;; Time points
(defun daylight-savings-p (time-designator)
  "Returns true iff TIME-DESIGNATOR is in a daylight savings
period of the year."
  (/= (first (current-time-zone (time-encode '(0 0 0 1 1 1970 0))))
      (first (current-time-zone (time-encode time-designator)))))
;;(daylight-savings-p '2014-03-31T01:00)

(defun parse-iso-time (iso-time)
  (reverse (mapcar #'string-to-number (split-string iso-time ":"))))
;;(parse-iso-time "12:39:01")

;(debug-on-entry #'add-time)
(cl-defun add-time (time-designator &key (year 0) (month 0) (week 0) (day 0) (hour 0) (minute 0) (second 0))
  "Creates an adjusted time object that equals TIME-DESIGNATOR added YEAR units
of years, MONTH units of months, etc. The units may be negative."
  (let ((time (parse-time time-designator)))
    (incf (mbt-year time) year)
    (incf (mbt-month time) month)
    (incf (mbt-day time) (+ day (* 7 week)))
    (incf (mbt-hour time) hour)
    (incf (mbt-minute time) minute)
    (incf (mbt-second time) second)
    (clean-time time)))
(cl-indent 'add-time 'prog1)
;;(add-time "2000-10-28" :hour 1)
;;(add-time "13:12" :minute 1)
;;(add-time '(0 0 0 28 10 2001 0 t 7200) :day 1)
;;(add-time "1990-12-31T22:59:59" :second 2 :hour 2)

(cl-defun add-time* (time &optional (year 0) (month 0) (week 0) (day 0) (hour 0) (minute 0) (second 0))
  (when time
    (add-time time :year year :month month :week week :day day :hour hour :minute minute :second second)))
;;(add-time* "2000-10-28" 0 0 0 1)

(cl-defun make-time (&key (new-time (the-creation)) date time (skip-dls-p nil))
  "Basic method for creating an mb-time object from years, months etc down to seconds."
  (when date (setq new-time (replace new-time (reverse date) 
				     :start1 (- 6 (length date))
				     :end1 6)))
  (when time (setq new-time (replace new-time (reverse time) 
				     :start1 (- 3 (length time))
				     :end1 3)))
  (clean-time
   (if (or skip-dls-p
	   (not (daylight-savings-p new-time)))
     new-time
     (progn
       (setf (mbt-day-light-saving-time-p new-time) t)
       (incf (mbt-time-zone new-time) 3600)
       new-time))))
;;(equal (parse-time (iso-date-and-time :with-seconds t)) (now))

;;; Parser functions
(defun numbers-regexp (a b)
  (regexp-opt (mapcar (bind #'format "%02d" 1) (a-b a b))))
;;(numbers-regexp 2 4)

(defun iso-year-regexp ()
  "[0-9][0-9][0-9][0-9]")
;;(iso-year-regexp)

(defun iso-date-regexp ()
  (let* ((y (iso-year-regexp))
	(m (numbers-regexp 1 12))
	(d (numbers-regexp 1 31)))
    (format "\\b\\(?1:%s\\)-\\(?2:%s\\)-\\(?3:%s\\)" y m d)))
;;(string-match* (iso-date-regexp) "2014-10-30")

(defun iso-time-regexp ()
  (let* ((ms "[0-9]*")
	 (s "[0-5][0-9]")
	 (m s)
	 (h (numbers-regexp 0 23))
	 (hm-0-23 (format "\\(?4:%s\\):\\(?5:%s\\)" h m))
	 (hm-24 "\\(?4:24\\):\\(?5:00\\)") ;; who the f*** allowed this in the ISO standard!?)
	 (hm (format "\\(?:%s\\|%s\\)" hm-0-23 hm-24)))
    (format "%s\\(?::\\(?6:%s\\)\\(?:.\\(?7:%s\\)\\)?\\)?Z?\\b" hm s ms)))

;;(string-match* (iso-date-regexp) "2013-04-12" :num '(1 2 3))
;;(string-match* (iso-time-regexp) "14:59:22.00Z" :num '(4 5 6 7))
;;(string-match* (format "%sT%s" (iso-date-regexp) (iso-time-regexp)) "2013-04-12T14:59:22.00Z" :num '(1 2 3 4 5 6 7))

(cl-defun iso-dttm-regexp (&optional (date-time-split-regexp "T"))
  (format "%s\\(?:%s%s\\)?\\|\\(?:%s\\)" (iso-date-regexp) date-time-split-regexp (iso-time-regexp) (iso-time-regexp)))

(defconst *iso-date* (iso-date-regexp))
(defconst *iso-time* (iso-time-regexp))
(defconst *iso-dttm* (iso-dttm-regexp))
(defconst *iso-dttm-lazy* (iso-dttm-regexp "[T ]"))

(cl-defun decode-iso-dttm (iso-dttm &optional (lazy-dttm nil) (noerror t))
  (multiple-value-bind (y mo d h mi s ms)
      (if (string-match* (if lazy-dttm *iso-dttm-lazy* *iso-dttm*) iso-dttm :num '(1 2 3 4 5 6 7))
	(let ((time (mapcar (compose #'string-to-number (bind #'sstring "0")) 
			    (list y mo d h mi s)))) ;;discard ms
	  (if (zerop (first time)) ;ie. we parse time only
	    (make-time :new-time (midnight) :time (nthcdr 3 time))
	    (if (zerop (fourth time)) ;ie. we parse date only
	      (make-time :date (subseq time 0 3))
	      (make-time :date time))))
	(unless noerror (error "%s is not a valid ISO date" iso-dttm)))))
;;(mapcar #'decode-iso-dttm (list "16:00" "2013-04-12" "2013-04-12T16:04" "2013-04-12T16:04Z" "qwe"))
;;(decode-iso-dttm "qwe")

(cl-defun decode-iso-date-old (iso-date &optional (lazy-dttm nil) (noerror t))
  "Parses an ISO-DATE returning a mb-time object. Format is `yyyy-mm-dd[ hh:mm[:ss]]'"
  (let* ((dttm (mapcar (compose #'string-to-number (bind #'sstring "0")) 
		       (string-match* (if lazy-dttm *iso-dttm-lazy* *iso-dttm*)
				      iso-date :num '(1 2 3 4 5 6))))
	 (date (or (subseq dttm 0 3)
		   (mapcar #'string-to-number 
			   (string-match* *iso-date* iso-date :num '(1 2 3)))))
	 (time (or (subseq dttm 3)
		   (mapcar #'string-to-number
			   (string-match* *iso-time* iso-date :num '(1 2 3) :subexpression-null "0")))))
    (if (or date time)
      (make-time :new-time (midnight) :date date :time time)
      (unless noerror (error "%s is not a valid ISO date" iso-date)))))

(cl-defun decode-iso-date (iso-date &optional (lazy-dttm nil) (noerror t))
  "Parses an ISO-DATE returning a mb-time object. Format is `yyyy-mm-dd[ hh:mm[:ss]]'"
  (let* ((dttm-parts (string-match* (if lazy-dttm *iso-dttm-lazy* *iso-dttm*)
				    iso-date :num '(1 2 3 4 5 6)))
	 (date-parts (subseq dttm-parts 0 3))
	 (time-parts (subseq dttm-parts 3))
	 (date (and (some #'identity date-parts) ;; not all nil
		    (mapcar (compose #'string-to-number (bind #'sstring "0")) date-parts)))
	 (time (and (some #'identity time-parts) ;; not all nil
		    (mapcar (compose #'string-to-number (bind #'sstring "0")) time-parts))))
    (cond ((and time date) (make-time :date date :time time))
	  (date (make-time :date date)) 
	  (time (make-time :time time :date (nreverse (subseq (midnight) 3 6))))
	  (t (unless noerror (error "%s is not a valid ISO date" iso-date))))))
;;(mapcar #'decode-iso-date '("13:21" "2013-04-12" "2014-04-02T23:16"))
;;(parse-time "1993-01-04")
;;(decode-iso-date-new-try-but-still-not-ok "22:54")

(defmacro copy-time (time)
  `(copy-list ,time))

(defun parse-time (time-designator)
  "Returns a new time object equal to TIME-DESIGNATOR.
Argument may be a time objects itself or a string."
  (typecase time-designator
    (cons (copy-time time-designator))
    (string (decode-iso-date time-designator))
    (symbol (parse-time (symbol-name time-designator)))
    (error "%s is not a legar time designator")))
;;(mapcar #'parse-time (list "2005-01-18 09:15" '2014-04-02))
;;(parse-time "2000-01-18T22:31")

(defconst *time-max-int* 134217727 "2^27 - 1")
(defconst *weekdays*
  '((:s0 (0 1 2 3 4 5 6) "Zero based weekday sequence starting on a Sunday")
    (:s1 (1 2 3 4 5 6 7) "One based weekday sequence starting on a Sunday")
    (:m0 (6 0 1 2 3 4 5) "Zero based weekday sequence starting on a Monday")
    (:m1 (7 1 2 3 4 5 6) "One based weekday sequence starting on a Monday")
    (:no ("søndag" "mandag" "tirsdag" "onsdag" "torsdag" "fredag" "lørdag") "Norwegian")
    (:en ("Sunday" "Monday" "Tuesday" "Wednesday" "Thursday" "Friday" "Saturday") "English")
    (:de ("Sonntag" "Montag" "Dienstag" "Mitwoch" "Donnerstag" "Freitag" "Samstag") "German")
    (:it ("domenica" "lunedi" "martedi" "mercoledi" "giovedi" "venerdi" "sabbato") "Italian")
    (:se ("söndag" "måndag" "tisdag" "onsdag" "torsdag" "fredag" "lördag") "Swedish")))

(cl-defun mb-weekday-languages (&optional (format :description) (skip-number-sequences-p t))
  "Returns a list of the supported week day languages."
  (mapcar (case format (:description #'third) (t #'first))
	  (if skip-number-sequences-p
	    (remove-if #'(lambda (x) (integerp (first (second x)))) *weekdays*)
	    *weekdays*)))
;;(mb-weekday-languages)

(defconst *months*
  '((:j0 (0 1 2 3 4 5 6 7 8 9 10 11)) ;; zero based months starting on January
    (:j1 (1 2 3 4 5 6 7 8 9 10 11 12)) ;; one based starting on January
    (:no ("januar" "februar" "mars" "april" "mai" "juni" "juli" "august" "september" "oktober" "november" "desember"))
    (:en ("January" "February" "March" "April" "May" "June" "July" "August" "September" "October" "November" "December"))))

(cl-defun month-name (n &optional (lang :no) (short-form nil) (short-form-size 3) (months *months*))
  "Returns the Nth month according table `*months'."
  (let ((res (nth n (second (assoc lang months)))))
    (if short-form
      (substring res 0 short-form-size)
      res)))
;;(month-name 1 :en t)

;;; Notion of time extension: time extension atoms are time points
;;; (points in timespace) or time intervals (intervals with time point
;;; limits). A time extension is a time extension atom or a list of
;;; time extensions.

;;; Eg. 
;;; (decode-time) - time point atom
;;; (interval-cc (decode-time t1) (decode-time t2)) - time interval atom
;;; '((decode-time) time-ext1 '(time-ext2 time-ext3)))

(defun time-encode (time-designator)
  "Encodes TIME (as a list)"
  (apply #'encode-time (parse-time time-designator)))
;;(time-encode '2014-04-02)

(defun clean-time (time-designator) 
  "Corrects time-zone and week-day in TIME-DESIGNATOR."
  (decode-time (time-encode time-designator)))
;;(add-time (the-apocalypse) :year 1)
;;(clean-time (now))

(defconst *the-creation* (clean-time '(0 0 0 1 1 1971 0)))
(defconst *the-apocalypse* (clean-time '(0 0 0 1 1 2038 0)))

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

(defun qwe (date)
  (add-time "2001-10-28" :day 1))
;;(qwe "2001-10-28")
;;(qwe '(0 0 0 28 10 2001 0 t 7200))

;; reference times
(cl-defun now (&rest args)
  (apply #'add-time (decode-time) args))
;;(now :hour 12)
(cl-defun midnight (&optional (time (now)))
  (fill (copy-time time) 0 :end 3))
;;(midnight)
(cl-defun midday (&optional (time (now)))
  (add-time (midnight time) :hour 12))
(defalias 'noon #'midday)
;;(iso-date-and-time :time (midday (decode-iso-date "2000-09-07")))
(cl-defun morning (&optional (time (now)))
  (add-time (midnight time) :hour 6))
(cl-defun evening (&optional (time (now)))
  (add-time (midnight time) :hour 18))
(cl-defun weekstart (&optional (time (now)) (start-weekday :monday))
  (loop for time* = (midnight time) then (add-time time* :day -1)
	if (= (mbt-day-of-week time*) 
	      (if (eq start-weekday :monday) 1 0))
	return time*))
;;(iso-date-and-time :time (weekstart))
(cl-defun monthstart (&optional (offset 0) (time (now)))
  (let ((res (midnight time)))
    (destructuring-bind (year-offset month-offset)
	(cl-truncate (+ (mbt-month res) offset) 12)
      (setf (mbt-day res) 1)
      (setf (mbt-month res) month-offset)
      (incf (mbt-year res) year-offset)
      (setf (mbt-hour res) 0)) ;; due to DLS
    res))
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

(defun week-number (time-designator)
  (first (week-number-1 time-designator)))
;;(week-number '2009-12-31)

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

;;;; examples
;;(add-time "2000-10-29" :day 1)
;;(add-time (now) :day 1)
;;(now :iso-date "2000-10-31")
;;(iso-date (now :year 2 :month -12 :day -365))
;;(decode-time (apply #'encode-time '(0 0 0 1 1 2038 5 t 7200)))

(provide 'mb-utils-time)
