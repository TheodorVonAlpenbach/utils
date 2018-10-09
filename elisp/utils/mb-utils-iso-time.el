(defun numbers-regexp (a b)
  (regexp-opt (mapcar (bind #'format "%02d" 1) (a-b a b))))
;;(numbers-regexp 1 31)

(defconst +iso-year-regexp+ "[0-9][0-9][0-9][0-9]")
(defconst +iso-month-regexp+ "\\(?:0[1-9]\\|1[0-2]\\)")
(defconst +iso-day-regexp+ "\\(?:0[1-9]\\|[12][0-9]\\|3[01]\\)")

(defun iso-date-regexp ()
  (let* ((y +iso-year-regexp+)
	(m +iso-month-regexp+)
	(d +iso-day-regexp+))
    (format "\\b\\(?1:%s\\)-\\(?2:%s\\)-\\(?3:%s\\)" y m d)))
;;(string-match* (iso-date-regexp) "2014-10-30")

(defun 0-23-regexp ()
  "Return a optimized regular expression matching 1, ... 23"
  "\\(?:\\(?:[0-1][0-9]\\)\\|\\(?:2[0-3]\\)\\)")

(defun 0-59-regexp ()
  "Return a optimized regular expression matching 1, ... 59."
  "\\(?:[0-5][0-9]\\)")

(defun iso-time-hm-regexp (h-index m-index)
  (let* ((hm-0-23 (format "\\(?h:%s\\):\\(?m:%s\\)" (0-23-regexp) (0-59-regexp)))
	 (hm-24 "\\(?h:24\\):\\(?m:00\\)")) ;; who the f*** allowed this in the ISO standard!?
    (string-replace-map
	(format "\\(?:%s\\|%s\\)" hm-0-23 hm-24)
      `(("h" ,(sstring h-index))
	("m" ,(sstring m-index))))))
;;(iso-time-hm-regexp 4 5)

(cl-defun iso-time-zone-regexp (&optional (sign-index 1)
					  (h-index (1+ sign-index))
					  (m-index (1+ h-index)))
  "Return a regular expression for UTC time zone.
Examples (<time> is added for showing the context, and is not
part of time zone):
<time>Z
<time>±hh:mm
<time>±hhmm
<time>±hh

Even if UTC does not list time zone hours below -10 nor above
+14:00, and only the minute parts 00, 30, 45, the resulting
regexp here allows every time from 00:00 to 23:59."
  (let ((Z "\\(?sign:Z\\)")
	(sign "\\(?sign:[+-]\\)")
	(hh (format "\\(?:\\(?h:%s\\)\\)" (0-23-regexp)))
	(mm (format "\\(?::?\\(?m:%s\\)\\)" (0-59-regexp))))
    (string-replace-map
	(format "\\(?:%s\\|%s\\)" Z (format "\\(?:%s%s%s?\\)$" sign hh mm))
      `(("sign" ,(sstring sign-index))
	("h" ,(sstring h-index))
	("m" ,(sstring m-index))))))
;;(iso-time-zone-regexp)

(defun iso-time-regexp ()
  "Return a regular expression for ISO 8601 time.
The most complete time is on the form 'hh:mm:ss.ms+HH:MM'. In
this case, the matching indices of the parts hh, mm, ss, ms, +,
HH, MM are 4, 5, 6, 7, 8, 9, 10, respectively. For abbreviated
forms, see below, some of the matching parts will be nil. Here
examples of abbreviated time forms:
12:00 // the shortes possible form
12:00:00
12:00:00.0001
12:00Z
12:00+01:00
12:00:00Z
12:00:00.01+00:01 // non-existing time zone, but it is allowed 
"
  (format "%s\\(?::\\(?6:%s\\)\\(?:.\\(?7:%s\\)\\)?\\)?%s?\\b"
    (iso-time-hm-regexp 4 5) (0-59-regexp) "[0-9]*" (iso-time-zone-regexp 8)))
;;(string-match* (iso-time-regexp) "14:59:22.001Z" :num '(4 5 6 7))

(defconst *iso-date* (iso-date-regexp))
(defconst *iso-time* (iso-time-regexp))

(defun iso-dttm-regexp (date-time-split-regexp)
  (format "%s\\(?:%s%s\\)?\\|\\(?:%s\\)"
    *iso-date* date-time-split-regexp *iso-time* *iso-time*))

(defconst *iso-dttm* (iso-dttm-regexp "T"))
(defconst *iso-dttm-lazy* (iso-dttm-regexp "[T ]"))

(cl-defun decode-iso-dttm (iso-dttm &optional (lazy-dttm nil) (noerror t))
  "Parse an ISO 8601 formatted time string.
Note! This version does not handle time zones.
See https://common-lisp.net/project/local-time/manual.html
for inspiration."
  ())

(cl-defun decode-iso-dttm (iso-dttm &optional (lazy-dttm nil) (noerror t))
  "Parse an ISO 8601 formatted time string.
Note! This version does not handle time zones.
See https://common-lisp.net/project/local-time/manual.html
for inspiration."
  (multiple-value-bind (y mo d h mi s ms tz-sign tz-h tz-m)
      (if (string-match* (if lazy-dttm *iso-dttm-lazy* *iso-dttm*)
	    iso-dttm :num '(1 2 3 4 5 6 7 8 9 10))
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

;; Time formatting
(cl-defun iso-date (&optional (date (decode-time)))
  "Returns DATE (default is today) in iso string format"
  (format "%04d-%02d-%02d"
    (etime-year date) (etime-month date) (etime-day date)))
;;(iso-date (now))

(cl-defun weekday (&optional (lang :no) (time-designator (decode-time)))
  "Returns weekday of TIME-DESIGNATOR (default is today) in language LANG"
    (nth (etime-day-of-week (parse-time time-designator))
       (second (assoc lang *weekdays*))))
;;(loop for i below 7 collect (weekday :no (add-etime-date (now) :day i)))

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
    (format "%d.%d" (etime-day time) (etime-month time))))
;;(short-date '2014-04-02)

(cl-defun iso-time (&key (time (decode-time)) (with-seconds nil))
  "Returns time designator TIME (default is now) in iso string format"
  (let ((etime (parse-time time)))
    (if with-seconds 
      (format "%02d:%02d:%02d"
	(etime-hour etime) (etime-minute etime) (etime-second etime))
      (format "%02d:%02d" (etime-hour etime) (etime-minute etime)))))
;;(iso-time :with-seconds t :time '2014-04-02)

(cl-defun iso-date-and-time (&key (time (decode-time)) (with-seconds nil))
  "Prints time designator TIME in full ISO date and time format"
  (let ((time (parse-time time)))
    (concat (iso-date time) "T" (iso-time :time time :with-seconds with-seconds))))
;;(iso-date-and-time :time '2014-04-02T22:25 :with-seconds t)

(provide 'mb-utils-iso-time)
