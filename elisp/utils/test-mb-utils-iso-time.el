(require 'ert)
(require 'lsconf-sensors)

(ert-deftest test-iso-time-regexp ()
  "Test of `iso-time-regexp'"
  (should (equal (iso-time-regexp) *iso-time*)))

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

(ert-deftest test-weekday ()
  "Test of `weekday'"
  (should (equal (weekday :no '2009-01-01) "torsdag")))

(provide 'test-mb-utils-iso-time.el)
