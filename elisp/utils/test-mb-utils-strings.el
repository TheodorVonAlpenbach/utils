(require 'ert)

(ert-deftest test-string-trim ()
  "Test of `string-trim'."
  (should (equal (string-trim " qwe ") "qwe")))

(ert-deftest test-read-whole-string ()
  "Test of `read-whole-string'."
  (should (equal (read-whole-string "a b (c d)") '(a b (c d)))))


