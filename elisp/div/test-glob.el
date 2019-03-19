(require 'ert)
(require 'glob)

(ert-deftest test-glob-up-digit ()
  "Test of `glob-up-digit'"
  (should (equal (glob-up-digit 2) "[2-9]"))
  (should (equal (glob-up-digit 9) "9"))
  (should (equal (glob-up-digit 7 (i-make-interval 6 8)) "[7-8]"))
  (should (equal (glob-up-digit 8 (i-make-interval 6 8)) "8")))

(ert-deftest test-glob-up-day ()
  "Test of `glob-up-day'"
  (should (equal (glob-up-day 1 7) '("1[7-9]" "[2-3][0-9]")))
  (should (equal (glob-up-day 3 0) '("3[0-9]"))))

(provide 'test-glob)
