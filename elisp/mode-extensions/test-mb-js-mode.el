(require 'ert)
(require 'mb-js-mode)

(ert-deftest test-js-eval-string ()
  "Test of `js-eval-string'"
  (should (equal (js-eval-string "2+2") "4"))
  (should (equal (js-eval-string "'2+2'") "'2+2'"))
  (should (equal (js-eval-string "[1,2]") "[ 1, 2 ]")))

(provide 'test-mb-js-mode)
