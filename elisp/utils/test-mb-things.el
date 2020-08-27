(require 'ert)
(require 'mb-things)

(ert-deftest test-inc-clock ()
  "Test of `inc-clock'"
  (should (equal (inc-clock "08:15" 1 1) "08:16"))
  (should (equal (inc-clock "08:15" 2 1) "08:17"))
  (should (equal (inc-clock "08:15" -1 1) "08:14"))
  (should (equal (inc-clock "08:15" 1 2) "09:15"))
  (should (equal (inc-clock "08:15" 2 2) "10:15"))
  (should (equal (inc-clock "08:15" -1 2) "07:15"))
  (should (equal (inc-clock "08:15" 1 3) "08:15:01"))
  (should (equal (inc-clock "08:15" 2 3) "08:15:02"))
  (should (equal (inc-clock "08:15" -1 3) "08:14:59")))

(provide 'test-mb-things)
