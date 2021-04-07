(require 'ert)

(ert-deftest test-add-time ()
  "Test of `add-etime'"
  (should (equal (iso-dttm (add-time (parse-time "1972-01-06T07:15Z")) t)
		 "1972-01-06T07:15:00GMT"))
  (should (equal (iso-dttm (add-time (parse-time "1972-01-06T07:15Z") :year 1) t)
		 "1973-01-06T07:15:00GMT"))
  (should (equal (iso-dttm (add-time (parse-time "1972-01-06T07:15Z") :month 13) t)
		 "1973-02-06T07:15:00GMT"))
  (should (equal (iso-dttm (add-time (parse-time "1972-01-06T07:15Z") :day 100) t)
		 "1972-04-15T07:15:00GMT"))
  (should (equal (iso-dttm (add-time (parse-time "1972-01-06T07:15Z") :week 1) t)
		 "1972-01-13T07:15:00GMT"))
  (should (equal (iso-dttm (add-time (parse-time "1972-01-06T07:15Z") :hour -1) t)
		 "1972-01-06T06:15:00GMT"))
  (should (equal (iso-dttm (add-time (parse-time "1972-01-06T07:15Z") :minute -15) t)
		 "1972-01-06T07:00:00GMT"))
  (should (equal (iso-dttm (add-time (parse-time "1972-01-06T07:15Z") :second 60) t)
		 "1972-01-06T07:16:00GMT"))
  ;; Not a recommended practice. 
  (should (equal (iso-dttm (add-time (parse-time "1972-01-06T08:15CET")
			     :year 46 :month 4 :day 19))
		 "2018-05-25T08:15:00CEST"))
  ;; This should fail since UTC does not have daylight savings and the
  ;; inner workings of elisp time calculations are based on locale.
  ;; Only proper way to handle these problems is to set the locale to
  ;; something else while running the code
  (should-not (equal (iso-dttm (add-time (parse-time "1972-01-06T07:15GMT")
				 :year 46 :month 4 :day 19) t)
		     "2018-05-25T07:15:00GMT"))
  (should (equal (with-timezone "GMT"
		   (iso-dttm (add-time (parse-time "1972-01-06T07:15GMT")
			       :year 46 :month 4 :day 19) t))
		 "2018-05-25T07:15:00GMT")))

(ert-deftest test-clean-time-zone-suffix ()
  "Test of `clean-time-zone-suffix'"
  (should (equal (clean-time-zone-suffix "qweCET")
		 "qwe+0100"))
  (should (equal (clean-time-zone-suffix "qweCEST")
		 "qwe+0200"))
  (should (equal (clean-time-zone-suffix "qweZ")
		 "qwe+0000"))
  (should (all-equal (clean-time-zone-suffix "qwe+01:00")
		     (clean-time-zone-suffix "qwe+0100")
		     (clean-time-zone-suffix "qwe+01")
		     "qwe+0100"))
  (should (all-equal (clean-time-zone-suffix "qwe-01:00")
		     (clean-time-zone-suffix "qwe-0100")
		     (clean-time-zone-suffix "qwe-01")
		     "qwe-0100"))
  (should (all-equal (clean-time-zone-suffix "1972-01-05T23:00+01:00")
		     (clean-time-zone-suffix "1972-01-05T23:00+0100")
		     (clean-time-zone-suffix "1972-01-05T23:00CET")
		     "1972-01-05T23:00+0100"))
  (should (equal (clean-time-zone-suffix "qweNOTAKNOWNCODE")
		 "qweNOTAKNOWNCODE")))

(ert-deftest test-mb-parse-date-string ()
  "Test of `mb-parse-date-string'"
  (should (all-equal (mb-parse-date-string "1972-01-05T23:00+00:00")
		     (mb-parse-date-string "1972-01-05T23:00+0000")
		     (mb-parse-date-string "1972-01-05T23:00+00")
		     (mb-parse-date-string "1972-01-05T23:00Z")
		     (mb-parse-date-string "1972-01-05T23:00GMT")
		     (mb-parse-date-string "1972-01-05T23:00UTC")
		     (list 968 61552)))
  (should (all-equal (mb-parse-date-string "1972-01-05T23:00+01:00")
		     (mb-parse-date-string "1972-01-05T23:00+0100")
		     (mb-parse-date-string "1972-01-05T23:00+01")
		     (mb-parse-date-string "1972-01-05T23:00CET")
		     (list 968 57952)))
  (should (all-equal (mb-parse-date-string "1972-01-05T23:00+02:00")
		     (mb-parse-date-string "1972-01-05T23:00+0200")
		     (mb-parse-date-string "1972-01-05T23:00+02")
		     (mb-parse-date-string "1972-01-05T23:00CEST")
		     (list 968 54352))))

(ert-deftest test-iso-dttm ()
  "Test of `iso-dttm'"
  (should (equal (iso-dttm '(0 0) t) "1970-01-01T00:00:00GMT")))


(ert-deftest test---add-ddate ()
  "Test of `--add-ddate'"
  (should (equal (--add-ddate (decode-time (parse-time "2018-03-25T12:00")) :day -1)
		 '(3600 0 11 24 3 2018 6 nil 3600))))

(ert-deftest test-add-etime-date ()
  "Test of `add-etime-date'"
  (should (equal (iso-dttm (add-etime-date (parse-time "2018-03-25T12:00") :day -1))
		 "2018-03-24T12:00:00CET"))
  (should (equal (iso-dttm (add-etime-date (parse-time "2018-03-25T01:00") :day -1))
		 "2018-03-24T01:00:00CET")))

(ert-deftest test-midnight ()
  "Test of `midnight'"
  (should (equal (subseq (iso-dttm (midnight)) 11 19)
		 "00:00:00"))
  (should (equal (iso-dttm (midnight (parse-time "1972-01-06")))
		 "1972-01-06T00:00:00CET"))
  (should (equal (iso-dttm (midnight (parse-time "1972-01-06T00:00Z") t) t)
		 "1972-01-06T00:00:00GMT"))
  (should (equal (iso-dttm (midnight (parse-time "1972-01-05T23:00") t) t)
		 "1972-01-05T00:00:00GMT"))
  (should (equal (iso-dttm (midnight (parse-time "1972-01-05T23:00cet") t) t)
		 "1972-01-05T00:00:00GMT"))
  (should (equal (iso-dttm (midnight (parse-time "1972-01-06T00:00") t) t)
		 "1972-01-06T00:00:00GMT"))
  (should (equal (iso-dttm (midnight (parse-time "2018-03-25T12:00")))
		 "2018-03-25T00:00:00CET"))
  (should (equal (iso-dttm (midnight (parse-time "2018-10-28T12:00")))
		 "2018-10-28T00:00:00CEST")))

(ert-deftest test-midday ()
  "Test of `midday'"
  (should (equal (subseq (iso-dttm (midday)) 11 19) "12:00:00"))
  (should (equal (iso-dttm (midday (parse-time "1972-01-06T00:00Z") t) t)
		 "1972-01-06T12:00:00GMT"))
  ;; This test will fail outside CET locale
  (should (equal (iso-dttm (midday (parse-time "1972-01-06T00:00Z")) t)
		 "1972-01-06T11:00:00GMT"))
  (should (equal (iso-dttm (midday (parse-time "1972-01-06T00:00Z")))
		 "1972-01-06T12:00:00CET"))
  (should (all-equal (iso-dttm (midday (parse-time "2018-03-25T18:00")))
		     (iso-dttm (midday (parse-time "2018-03-25T01:00")))
		     "2018-03-25T12:00:00CEST"))
  (should (all-equal (iso-dttm (midday (parse-time "2018-10-28T18:00")))
		     (iso-dttm (midday (parse-time "2018-10-28T01:00")))
		     "2018-10-28T12:00:00CET")))

(ert-deftest test-morning ()
  "Test of `morning'"
  (should (equal (subseq (iso-dttm (morning)) 11 19) "06:00:00"))
  (should (equal (iso-dttm (morning (parse-time "1972-01-06T00:00Z") t) t)
		 "1972-01-06T06:00:00GMT"))
  ;; This test will fail outside CET locale
  (should (equal (iso-dttm (morning (parse-time "1972-01-06")))
		 "1972-01-06T06:00:00CET"))
  (should (all-equal (iso-dttm (morning (parse-time "2018-03-25T18:00")))
		     (iso-dttm (morning (parse-time "2018-03-25T01:00")))
		     "2018-03-25T06:00:00CEST"))
  (should (all-equal (iso-dttm (morning (parse-time "2018-10-28T18:00")))
		     (iso-dttm (morning (parse-time "2018-10-28T01:00")))
		     "2018-10-28T06:00:00CET")))

(ert-deftest test-evening ()
  "Test of `evening'"
  (should (equal (iso-dttm (evening (parse-time "1972-01-06T00:00Z") t) t)
		 "1972-01-06T18:00:00GMT"))
  (should (all-equal (iso-dttm (evening (parse-time "2018-03-25T18:00")))
		     (iso-dttm (evening (parse-time "2018-03-25T01:00")))
		     "2018-03-25T18:00:00CEST"))
  (should (all-equal (iso-dttm (evening (parse-time "2018-10-28T18:00")))
		     (iso-dttm (evening (parse-time "2018-10-28T01:00")))
		     "2018-10-28T18:00:00CET")))

(ert-deftest test-weekday-number ()
  "Test of `weekday-number'"
  (should (equal (weekday-number :sunday) 0))
  (should (equal (weekday-number :tuesday) 2))
  (should (equal (weekday-number :saturday) 6)))

(ert-deftest test-weekstart ()
  "Test of `weekstart'"
  (should (equal (iso-dttm (weekstart (parse-time "1972-01-06")))
		 "1972-01-03T00:00:00CET"))
  (should (all-equal (iso-dttm (weekstart (parse-time "2018-10-28T03:00")))
		     (iso-dttm (weekstart (parse-time "2018-10-28T02:30")))
		     (iso-dttm (weekstart (parse-time "2018-10-28T02:00")))
		     "2018-10-22T00:00:00CEST"))
  (should (all-equal (iso-dttm (weekstart (parse-time "2018-03-25T03:00")))
		     (iso-dttm (weekstart (parse-time "2018-03-25T02:30")))
		     (iso-dttm (weekstart (parse-time "2018-03-25T02:00")))
		     "2018-03-19T00:00:00CET")))

(ert-deftest test-append-timezone-regexp ()
  "Test of `append-timezone-regexp'"
  (should (equal (append-timezone-regexp '("CEST" . 0))
		 '("CEST" 0 "\\(CEST\\)$"))))

(ert-deftest test-monthstart ()
  "Test of `monthstart'"
  (should (all-equal (iso-dttm (monthstart (parse-time "1972-01-01")))
		     (iso-dttm (monthstart (parse-time "1972-01-06")))
		     "1972-01-01T00:00:00CET"))
  (should (all-equal (iso-dttm (monthstart (parse-time "2018-07-01")))
		     (iso-dttm (monthstart (parse-time "2018-07-06")))
		     "2018-07-01T00:00:00CEST")))

(ert-deftest test-yearstart ()
  "Test of `yearstart'"
  (should (all-equal (iso-dttm (yearstart (parse-time "1972-01-06")))
		     (iso-dttm (yearstart (parse-time "1972-07-06")))
		 "1972-01-01T00:00:00CET"))
  (should (all-equal (iso-dttm (yearstart (parse-time "2018-01-06")))
		     (iso-dttm (yearstart (parse-time "2018-07-06")))
		 "2018-01-01T00:00:00CET")))


(ert-deftest test-parse-time ()
  "Test of `parse-time'"
  (should (all-equal (parse-time "2018-01-06")
		     (parse-time '2018-01-06)
		     (parse-time '(23120 880))
		     (parse-time '(0 0 0 6 1 2018 6 nil 3600))
		     '(23120 880))))

(ert-deftest test-time< ()
  "Test of `time<'"
  (should (time< '2018-05-28T00:00CET '2018-05-28T00:00Z))
  (should (time< '2018-05-28T00:00Z '2018-05-28T00:00EST))
  (should (time< '2018-05-28T00:00Z '2018-05-28T00:00:01Z))
  (should-not (time< '2018-05-28T00:00Z '2018-05-28T00:00Z))
  (should-not (time< '2018-05-28T00:00EST '2018-05-28T00:00EST))
  (should-not (time< '2018-05-28T00:00:01Z '2018-05-28T00:00:01Z)))

(ert-deftest test-time<= ()
"Test of `time<='"
  (should (time<= '2018-05-28T00:00CET '2018-05-28T00:00Z))
  (should (time<= '2018-05-28T00:00Z '2018-05-28T00:00EST))
  (should (time<= '2018-05-28T00:00Z '2018-05-28T00:00:01Z))
  (should (time<= '2018-05-28T00:00Z '2018-05-28T00:00Z))
  (should (time<= '2018-05-28T00:00EST '2018-05-28T00:00EST))
  (should (time<= '2018-05-28T00:00:01Z '2018-05-28T00:00:01Z)))

(ert-deftest test-time- ()
  "Test of `time<='"
  (should (equal (time- '2018-05-28T00:00Z '2018-05-28T00:00CET :second)
		 3600.0))
  (should (equal (time- '2018-05-28T00:00Z '2018-05-28T00:00EST :second)
		 -18000.0))
  (should (equal (time- '2018-05-28T00:00Z '2018-05-28T00:00EST :minute)
		 -300.0))
  (should (equal (time- '2018-05-28T00:00Z '2018-05-28T00:00EST :hour)
		 -5.0))
  (should (equal (time- '2018-05-28T00:00Z '2018-05-27T00:00Z : :day)
		 1.0))
  (should (equal (time- '2018-05-28 '2018-05-21 :week)
		 1.0))
  (should (equal (* (time- (add-etime-time (add-etime-date (now) :day 6) :hour 25)
			   (now)
			   :week)
		    168)
		 169.0)))
(ert-deftest test-time- ()
  "Test of `time-'"
  (should (equal (time- '2018-05-28T00:00Z '2018-05-28T00:00CET :second)
		 3600.0))
  (should (equal (time- '2018-05-28T00:00Z '2018-05-28T00:00EST :second)
		 -18000.0))
  (should (equal (time- '2018-05-28T00:00Z '2018-05-28T00:00EST :minute)
		 -300.0))
  (should (equal (time- '2018-05-28T00:00Z '2018-05-28T00:00EST :hour)
		 -5.0))
  (should (equal (time- '2018-05-28T00:00Z '2018-05-27T00:00Z :day)
		 1.0))
  (should (equal (time- '2018-05-28 '2018-05-21 :week)
		 1.0))
  (should (equal (time- (add-etime-date (now) :day 6))
		 6.0)))

(ert-deftest test-today ()
  "Test of `today'"
  (should (<= 23 (period-length (today) :hour) 25))
  (should (equal (period-length (today (parse-time "2018-03-25")) :hour)
		 23.0))
  (should (equal (period-length (today (parse-time "2018-10-28")) :hour)
		 25.0))
  (should (equal (period-length (today (parse-time "2018-06-01")) :hour)
		 24.0)))

(ert-deftest test-tomorrow ()
  "Test of `tomorrow'"
  (should (<= 23 (period-length (tomorrow) :hour) 25))
  (should (equal (period-length (tomorrow (parse-time "2018-03-23")) :hour)
		 24.0))
  (should (equal (period-length (tomorrow (parse-time "2018-03-24")) :hour)
		 23.0))
  (should (equal (period-length (tomorrow (parse-time "2018-03-26")) :hour)
		 24.0))
  (should (equal (period-length (tomorrow (parse-time "2018-10-26")) :hour)
		 24.0))
  (should (equal (period-length (tomorrow (parse-time "2018-10-27")) :hour)
		 25.0))
  (should (equal (period-length (tomorrow (parse-time "2018-10-28")) :hour)
		 24.0)))

(ert-deftest test-yesterday ()
  "Test of `tomorrow'"
  (should (<= 23 (period-length (yesterday) :hour) 25))
  (should (equal (period-length (yesterday (parse-time "2018-03-25")) :hour)
		 24.0))
  (should (equal (period-length (yesterday (parse-time "2018-03-26")) :hour)
		 23.0))
  (should (equal (period-length (yesterday (parse-time "2018-03-27")) :hour)
		 24.0))
  (should (equal (period-length (yesterday (parse-time "2018-10-28")) :hour)
		 24.0))
  (should (equal (period-length (yesterday (parse-time "2018-10-29")) :hour)
		 25.0))
  (should (equal (period-length (yesterday (parse-time "2018-10-30")) :hour)
		 24.0)))

(ert-deftest test-unix-time ()
  "Test of `unix-time'"
  (should (equal "Europe/Oslo" (current-timezone)))
  (should (equal (unix-time "1972-01-06T08:15CET") 63530100.0))
  (should (equal (unix-time "1970-01-01T00:00Z") 0.0))
  (should (equal (unix-time "1970-01-01T00:00") -3600.0))
  (should (equal (unix-time "1970-01-01T00:00CET") -3600.0)))

(ert-deftest test-interpolate-time ()
  "Test of `interpolate-time'"
  (should (all-equal (iso-dttm (interpolate-time 0.5 (today (parse-time '1972-01-06))))
		     (iso-dttm (interpolate-time 1.5 (today (parse-time '1972-01-06))
						 :a 1 :b 2))
		     "1972-01-06T12:00:00CET"))
  (should (all-equal (iso-dttm (interpolate-time 0.25 (today (parse-time '1972-01-06))))
		     "1972-01-06T06:00:00CET"))
  (should (all-equal (iso-dttm (interpolate-time
				(/ 3.0 7) (this-week (parse-time '1972-01-06))))
		     (iso-dttm (interpolate-time
				(/ 3.0 7) (this-week (parse-time '1972-01-06))))
		     "1972-01-06T00:00:00CET")))

(ert-deftest test-iso-period ()
  "Test of `iso-period'"
  (should (equal (iso-period (today (parse-time '1972-01-06)))
		 "1972-01-06T00:00:00CET/1972-01-07T00:00:00CET")))

(ert-deftest test-first-week-start ()
  "Test of `first-week-start'"
  (should (equal (mapcar (compose #'iso-dttm #'first-week-start) (a-b 2008 2020))
		 ;; Checked with Goolge Calendar
		 '("2007-12-31T00:00:00CET" "2008-12-29T00:00:00CET" "2010-01-04T00:00:00CET"
		   "2011-01-03T00:00:00CET" "2012-01-02T00:00:00CET" "2012-12-31T00:00:00CET"
		   "2013-12-30T00:00:00CET" "2014-12-29T00:00:00CET" "2016-01-04T00:00:00CET"
		   "2017-01-02T00:00:00CET" "2018-01-01T00:00:00CET" "2018-12-31T00:00:00CET"
		   "2019-12-30T00:00:00CET"))))

(ert-deftest test-max-week-number ()
  "Test of `max-week-number'"
  (should (equal (mapcar #'max-week-number (a-b 2001 2030))
		 ;; Checked with Goolge Calendar
		 ;; 1  2  3  4  5  6  7  8  9  0
		 '(52 52 52 53 52 52 52 52 53 52
		   52 52 52 52 53 52 52 52 52 53
		   52 52 52 52 52 53 52 52 52 52))))

(ert-deftest test-within-period-p ()
  "Test of `within-period-p'"
  (should (within-period-p (now) (period :from (midnight) :to (next-midnight))))
  (should (within-period-p (now) (today)))
  (should-not (within-period-p (now) (tomorrow))))

(ert-deftest test-daystart ()
  "Test of `daystart'"
  (should (equal (iso-dttm (daystart (parse-time "1972-01-06")))
		 "1972-01-06T00:00:00CET")))

(ert-deftest test-etime-part ()
  "Test of `etime-part'"
  (should (equal (etime-part (parse-time "1972-01-06T08:15:17") :year) 1972))
  (should (equal (etime-part (parse-time "1972-01-06T08:15:17") :month) 1))
  (should (equal (etime-part (parse-time "1972-01-06T08:15:17") :day) 6))
  (should (equal (etime-part (parse-time "1972-01-06T08:15:17") :hour) 8))
  (should (equal (etime-part (parse-time "1972-01-06T08:15:17") :minute) 15))
  (should (equal (etime-part (parse-time "1972-01-06T08:15:17") :second) 17)))

(ert-deftest test-etime-round ()
  "Test of `etime-round'"
 (should (equal
	  (iso-dttm (etime-round (parse-time "1972-01-06T08:15:17") :year -10))
	  "1970-01-06T08:15:17CET"))
 (should (equal
	  (iso-dttm (etime-round (parse-time "1972-01-06T08:15:17") :year 10))
	  "1980-01-06T08:15:17CET"))
 (should (equal
	  (iso-dttm (etime-round (parse-time "1972-01-06T08:15:17") :year 10))
	  "1980-01-06T08:15:17CET")))

(provide 'test-mb-utils-time.el)
