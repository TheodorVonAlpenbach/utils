(require 'ert)

(ert-deftest test-cram-current-user ()
  "Test of `cram-current-user'. Expects a current db."
  (should *current-database*)
  ;; (should (cram-current-user))
  ;; (should (cram-current-user t))
  )


