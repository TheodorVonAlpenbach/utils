(require 'ert)
(require 'mb-utils-economy)

(ert-deftest test-growth ()
  "Test of `growth'"
  ;; (should (equal (growth 2 2 0.75 1.0) 0.0))
  (should (equalp (growth 0 2 10 1) 10))
  (should (equalp (growth 1 2 10 1) 22))
  (should (equalp (growth 2 2 10 1) 46)))


(ert-deftest test-deposit ()
  "Test of `deposit'"
  (should (< (abs (- (deposit 37 1.07 -1.40352077574121) 0.1)) 1e-12)))

(provide 'test-mb-utils-economy)
