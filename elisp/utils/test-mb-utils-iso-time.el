;;; test-mb-utils-iso-time.el --- Tests for ISO time helpers -*- lexical-binding: t; -*-

(require 'ert)
(require 'mb-utils-iso-time)

(defun test-mb-utils-iso-time--exact-match-p (regexp string)
  "Return non-nil when REGEXP matches all of STRING."
  (string-match-p (concat "\\`\\(?:" regexp "\\)\\'") string))

(defun test-mb-utils-iso-time--groups (regexp string groups)
  "Return GROUPS captured by REGEXP in STRING."
  (when (string-match regexp string)
    (mapcar (lambda (group) (match-string group string)) groups)))

(ert-deftest test-iso-date-regexp ()
  "Test ISO date matching and capture groups."
  (should (equal (test-mb-utils-iso-time--groups
                  (iso-date-regexp) "2014-10-30" '(1 2 3))
                 '("2014" "10" "30")))
  (should-not (test-mb-utils-iso-time--exact-match-p
               (iso-date-regexp) "2014-13-30")))

(ert-deftest test-iso-time-regexp ()
  "Test ISO time matching, boundaries, and capture groups."
  (should (equal (iso-time-regexp) *iso-time*))
  (dolist (time '("00:00" "14:59:22.001Z" "23:59+23:59" "24:00"))
    (should (test-mb-utils-iso-time--exact-match-p *iso-time* time)))
  (dolist (time '("24:01" "14:60" "14:59:22." "14:59:22x001"))
    (should-not (test-mb-utils-iso-time--exact-match-p *iso-time* time)))
  (should (equal (test-mb-utils-iso-time--groups
                  *iso-time* "14:59:22.001+02:30"
                  '(4 5 6 7 8 9 10))
                 '("14" "59" "22" "001" "+" "02" "30"))))

(ert-deftest test-iso-time-zone-regexp ()
  "Test ISO time-zone matching and custom capture indices."
  (let ((regexp (iso-time-zone-regexp 2)))
    (dolist (zone '("Z" "+00:00" "-00:00" "+0000" "-0000"
                    "+00" "-00" "+23:59"))
      (should (test-mb-utils-iso-time--exact-match-p regexp zone)))
    (dolist (zone '("+24:00" "Z00" "00" "+"))
      (should-not (test-mb-utils-iso-time--exact-match-p regexp zone)))
    (should (equal (test-mb-utils-iso-time--groups
                    regexp "+23:59" '(2 3 4))
                   '("+" "23" "59")))
    (should (equal (test-mb-utils-iso-time--groups
                    (iso-time-zone-regexp 3 11 19)
                    "+23:59" '(3 4 11 19 20))
                   '("+" nil "23" "59" nil)))))

(ert-deftest test-weekday-and-month-data ()
  "Test localized weekday and month lookup."
  (should (equal (weekday :no '2009-01-01) "torsdag"))
  (should (equal (month-name 0 :en) "January"))
  (should (equal (month-name 0 :en t) "Jan"))
  (should (member "Norwegian" (mb-weekday-languages)))
  (should (member :no (mb-weekday-languages :key))))

(ert-deftest test-iso-date-and-time ()
  "Test ISO date-time formatting."
  (let ((time (encode-time 0 25 22 2 4 2014)))
    (should (equal (iso-date-and-time :time time)
                   "2014-04-02T22:25"))
    (should (equal (iso-date-and-time :time time :simple-p t)
                   "20140402T2225"))
    (should (equal (iso-date-and-time
                    :time time :with-seconds t :simple-p t)
                   "20140402T222500"))
    (should (equal (short-date time) "2.4"))
    (should (equal (full-date :en time) "Wednesday 2014-04-02"))))

(ert-deftest test-iso-time-designators ()
  "Test supported encoded, decoded, symbolic, and numeric time values."
  (should (equal (iso-date '2014-04-02) "2014-04-02"))
  (should (equal (iso-date '(0 0 12 2 4 2014 nil nil nil))
                 "2014-04-02"))
  (should (equal (iso-date 0)
                 (format-time-string "%Y-%m-%d" (seconds-to-time 0)))))

(provide 'test-mb-utils-iso-time)

;;; test-mb-utils-iso-time.el ends here
