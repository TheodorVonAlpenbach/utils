;;; test-cram-config.el --- Tests for Cram configuration -*- lexical-binding: t; -*-

(require 'ert)
(require 'cram-config)

(ert-deftest cram-config-has-rating-defaults ()
  (should (equal +cram-default-rating+ +glicko-init-rating+))
  (should (= +cram-default-rating-window+ 200))
  (should (equal +cram-default-user-name+ "Mats")))

(ert-deftest cram-config-has-problem-selection-defaults ()
  (should (= *cram-same-problem-limit* 2))
  (should (stringp +cram-default-ref-filter+))
  (should (stringp *cram-ref-filter*)))

(ert-deftest cram-config-initializes-cache-timestamp ()
  (should (equal *cram-last-update* +the-creation+))
  (should-not (eq *cram-last-update* +the-creation+))
  (should-not *cram-match-cache*))

(provide 'test-cram-config)

;;; test-cram-config.el ends here
