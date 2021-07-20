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

(ert-deftest test-inc-number ()
  "Test of `inc-number'"
  (should (equal (inc-number 2000 21 1) 2021))
  (should (equal (inc-number 2000 -21 1) 1979))
  (should (equal (inc-number 2000 3 2) 5000))
  (should (equal (inc-number 10 1 3) 100))
  (should (equal (inc-number 10 -1 3) 3)))

(provide 'test-mb-things)
