(require 'ert)
(require 'lsconf-sensors)

(ert-deftest test-cube-revert-sexp ()
  "Test of `cube-revert-sexp'"
  (should (equal (cube-revert-sexp 'U) 'Uw))
  (should (equal (cube-revert-sexp 'Uw) 'U))
  (should (equal (cube-revert-sexp '()) nil)))

(provide 'test-cube-utils.el)
