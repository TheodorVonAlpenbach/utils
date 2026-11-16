;;; test-mb-utils-economy.el --- Tests for mb-utils-economy -*- lexical-binding: t; -*-

(require 'ert)
(require 'mb-utils-economy)

(ert-deftest mb-utils-economy-basic-helpers ()
  "Test growth ratio and selected-average difference."
  (should (= (grew 25 125) 0.25))
  (should (= (CFaR '(1 2 3 4) 1 3) 0.0))
  (should (= (CFaR '(1 2 3 4) 1) -0.5))
  (should-error (grew 1 1))
  (should-error (CFaR nil 0 0))
  (should-error (CFaR '(1 2) 2 2)))

(ert-deftest mb-utils-economy-growth ()
  "Test of `growth'"
  (should (cl-equalp (growth 0 2 10 1) 10))
  (should (cl-equalp (growth 1 2 10 1) 22))
  (should (cl-equalp (growth 2 2 10 1) 46))
  (should (cl-equalp (growth 3 1 10 2) 16))
  (should (= (growth 0 0 10 2) 10))
  (should-error (growth -1 1.05 100 10))
  (should-error (growth 1 -1 100 10)))

(ert-deftest mb-utils-economy-deposit ()
  "Test of `deposit'"
  (should (< (abs (- (deposit 37 1.07 -1.40352077574121) 0.1)) 1e-12))
  (let ((yearly-deposit (deposit 20 1.05 1000)))
    (should (< (abs (growth 20 1.05 1000 yearly-deposit)) 1e-10)))
  (should-error (deposit 0 1.05 100))
  (should-error (deposit 2 0 100)))

(ert-deftest mb-utils-economy-withdraw-to-zero ()
  "Test that the calculated starting balance is fully withdrawn."
  (dolist (parameters '((1 1.045 100) (2 2 1) (24 1.07 1000)))
    (cl-destructuring-bind (n factor withdrawal) parameters
      (let ((balance (withdraw-to-zero n factor withdrawal)))
        (should (< (abs (growth n factor balance (- withdrawal)))
                   1e-9)))))
  (should (= (withdraw-to-zero 0 1.05 100) 0.0))
  (should-error (withdraw-to-zero -1 1.05 100))
  (should-error (withdraw-to-zero 2 0 100)))

(ert-deftest mb-utils-economy-profit-to-interest ()
  "Test conversion from accumulated profit to yearly growth."
  (should (= (profit-to-interest 1.21 2) 1.1))
  (should (= (profit-to-interest 1.5) 1.5))
  (should-error (profit-to-interest -1 2))
  (should-error (profit-to-interest 1.21 0)))

(provide 'test-mb-utils-economy)

;;; test-mb-utils-economy.el ends here
