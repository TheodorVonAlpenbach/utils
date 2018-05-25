(require 'ert)
(require 'lsconf-sensors)

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

(ert-deftest test-mb-parse-time-string ()
  "Test of `mb-parse-time-string'"
  (should (all-equal (mb-parse-time-string "1972-01-05T23:00+00:00")
		     (mb-parse-time-string "1972-01-05T23:00+0000")
		     (mb-parse-time-string "1972-01-05T23:00+00")
		     (mb-parse-time-string "1972-01-05T23:00Z")
		     (mb-parse-time-string "1972-01-05T23:00GMT")
		     (mb-parse-time-string "1972-01-05T23:00UTC")
		     (list 968 61552)))
  (should (all-equal (mb-parse-time-string "1972-01-05T23:00+01:00")
		     (mb-parse-time-string "1972-01-05T23:00+0100")
		     (mb-parse-time-string "1972-01-05T23:00+01")
		     (mb-parse-time-string "1972-01-05T23:00CET")
		     (list 968 57952)))
  (should (all-equal (mb-parse-time-string "1972-01-05T23:00+02:00")
		     (mb-parse-time-string "1972-01-05T23:00+0200")
		     (mb-parse-time-string "1972-01-05T23:00+02")
		     (mb-parse-time-string "1972-01-05T23:00CEST")
		     (list 968 54352))))

(ert-deftest test-iso-dttm ()
  "Test of `iso-dttm'"
  (should (equal (iso-dttm '(0 0) t) "1970-01-01T00:00:00GMT")))

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
		 "1972-01-06T00:00:00GMT")))

(ert-deftest test-midday ()
  "Test of `midday'"
  (should (equal (subseq (iso-dttm (midday)) 11 19) "12:00:00"))
  (should (equal (iso-dttm (midday (parse-time "1972-01-06T00:00Z") t) t)
		 "1972-01-06T12:00:00GMT"))
  ;; This test will fail outside CET locale
  (should (equal (iso-dttm (midday (parse-time "1972-01-06T00:00Z")) t)
		 "1972-01-06T11:00:00GMT"))
  (should (equal (iso-dttm (midday (parse-time "1972-01-06T00:00Z")))
		 "1972-01-06T12:00:00CET")))

(ert-deftest test-morning ()
  "Test of `morning'"
  (should (equal (subseq (iso-dttm (morning)) 11 19) "06:00:00"))
  (should (equal (iso-dttm (morning (parse-time "1972-01-06T00:00Z") t) t)
		 "1972-01-06T06:00:00GMT"))
  ;; This test will fail outside CET locale
  (should (equal (iso-dttm (morning (parse-time "1972-01-06")))
		 "1972-01-06T06:00:00CET")))

(ert-deftest test-evening ()
  "Test of `evening'"
  (should (equal (iso-dttm (evening (parse-time "1972-01-06T00:00Z") t) t)
		 "1972-01-06T18:00:00GMT")))

(ert-deftest test-weekday-number ()
  "Test of `weekday-number'"
  (should (equal (weekday-number :sunday) 0))
  (should (equal (weekday-number :tuesday) 2))
  (should (equal (weekday-number :saturday) 6)))

(ert-deftest test-weekstart ()
  "Test of `weekstart'"
  (should (equal (iso-dttm (weekstart (parse-time "1972-01-06")))
		 "1972-01-03T00:00:00CET"))
  (should (equal (iso-dttm (weekstart (parse-time "1972-01-06")))
		 "1972-01-03T00:00:00CET")))

(ert-deftest test-append-timezone-regexp ()
  "Test of `append-timezone-regexp'"
  (should (equal (append-timezone-regexp '("CEST" . 0))
		 '("CEST" 0 "\\(CEST\\)$"))))

(provide 'test-mb-utils-time2.el)
