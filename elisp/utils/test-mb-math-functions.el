(require 'ert)
(require 'mb-math-functions)

(ert-deftest test-geometric-series ()
  "Test of `geometric-series'"
  (should (equal (geometric-series 0.5) 2.0))
  (should (equal (geometric-series 0.5 :end 2) 1.5))
  (should (equal (geometric-series 0.5 :end 3) 1.75))
  (should (equal (geometric-series 0.5 :start 1 :end 3) 0.75)))

(provide 'test-mb-math-functions)
