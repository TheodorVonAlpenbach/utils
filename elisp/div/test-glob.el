(require 'ert)
(require 'glob)

(ert-deftest test-glob-up-digit ()
  "Test of `glob-up-digit'"
  (should (equal (glob-up-digit 2) "[2-9]"))
  (should (equal (glob-up-digit 9) "9"))
  (should (equal (glob-up-digit 7 (i-make-interval 6 8)) "[7-8]"))
  (should (equal (glob-up-digit 8 (i-make-interval 6 8)) "8")))

(provide 'test-glob)
