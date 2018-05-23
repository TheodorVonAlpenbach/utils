(require 'ert)
(require 'lsconf-sensors)

(ert-deftest test-iso-time-regexp ()
  "Test of `iso-time-regexp'"
  (should (equal (iso-time-regexp) *iso-time*)))

(ert-deftest test-0-24-regexp ()
  "Test of `0-24-regexp'"
  (let ((re (0-24-regexp)))
    (cl-flet ((f (x) (string-match-exact re (format "%02d" x))))
      (should (cl-every #'f (a-b 0 24)))
      (should (cl-notany #'f (a-b -3 -1)))
      (should (cl-notany #'f (a-b 25 26)))
      (should (cl-notany #'f (a-b 100 100))))))

(ert-deftest test-iso-time-zone-regexp ()
  "Test of `iso-time-zone-regexp'"
  (let ((re (iso-time-zone-regexp 2)))
    (should (string-match-exact re "Z"))
    (should (string-match-exact re "+00:00"))
    (should (string-match-exact re "-00:00"))
    (should (string-match-exact re "+0000"))
    (should (string-match-exact re "-0000"))
    (should (string-match-exact re "+00"))
    (should (string-match-exact re "-00"))
    (should (string-match-exact re "+23:59"))
    (should-not (string-match-exact re "+24:00"))
    (should-not (string-match-exact re "Z00"))
    (should-not (string-match-exact re "00"))
    (should-not (string-match-exact re "+"))
    (should (equal (string-match* re "+23:59" :num '(2 3 4))
		   '("+" "23" "59")))
    (should (equal (string-match* (iso-time-zone-regexp 3 11 19)
		     "+23:59" :num '(3 4 11 19 20))
		   '("+" nil "23" "59" nil))))) 

(provide 'test-mb-utils-time.el)
