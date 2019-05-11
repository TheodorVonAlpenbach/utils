(require 'ert)
(require 'mb-utils-economy)

(ert-deftest test-growth ()
  "Test of `growth'"
  (should (equal (growth 2 2 0.75 1.0) 0.0)))


(provide 'test-mb-utils-economy)
