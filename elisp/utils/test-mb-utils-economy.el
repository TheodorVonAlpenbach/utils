;;; test-mb-utils-economy.el --- Tests for mb-utils-economy -*- lexical-binding: t; -*-

(require 'ert)
(require 'mb-utils-economy)

(ert-deftest test-economy-basic-helpers ()
  "Test conversion, growth ratio, and selected-average difference."
  (should (= (to-double 2) 2.0))
  (should (= (grew 25 125) 0.25))
  (should (= (CFaR '(1 2 3 4) :from 1 :to 3) 0.0)))

(ert-deftest test-growth ()
  "Test of `growth'"
  (should (cl-equalp (growth 0 2 10 1) 10))
  (should (cl-equalp (growth 1 2 10 1) 22))
  (should (cl-equalp (growth 2 2 10 1) 46))
  (should (cl-equalp (growth 3 1 10 2) 16)))

(ert-deftest test-deposit ()
  "Test of `deposit'"
  (should (< (abs (- (deposit 37 1.07 -1.40352077574121) 0.1)) 1e-12))
  (let ((yearly-deposit (deposit 20 1.05 1000)))
    (should (< (abs (growth 20 1.05 1000 yearly-deposit)) 1e-10))))

(ert-deftest test-withdraw-to-zero ()
  "Test that the calculated starting balance is fully withdrawn."
  (dolist (parameters '((1 1.045 100) (2 2 1) (24 1.07 1000)))
    (cl-destructuring-bind (n factor withdrawal) parameters
      (let ((balance (withdraw-to-zero n factor withdrawal)))
        (should (< (abs (growth n factor balance (- withdrawal)))
                   1e-9))))))

(ert-deftest test-profit-to-interest ()
  "Test conversion from accumulated profit to yearly growth."
  (should (= (profit-to-interest 1.21 2) 1.1))
  (should (= (profit-to-interest 1.5) 1.5)))

(provide 'test-mb-utils-economy)

;;; test-mb-utils-economy.el ends here
