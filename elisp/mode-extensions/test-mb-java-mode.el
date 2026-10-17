;;; test-mb-java-mode.el --- Tests for mb-java-mode -*- lexical-binding: t; -*-

(require 'ert)
(require 'mb-java-mode)

(ert-deftest mb-java-mode-feature-and-local-settings ()
  (should (featurep 'mb-java-mode))
  (let ((default-c-basic-offset (default-value 'c-basic-offset))
        (default-tab-width (default-value 'tab-width))
        (default-indent-tabs-mode (default-value 'indent-tabs-mode)))
    (with-temp-buffer
      (java-mode)
      (should (= c-basic-offset 4))
      (should (= tab-width 4))
      (should-not indent-tabs-mode)
      (should-not parens-require-spaces)
      (should electric-pair-mode)
      (should (symbol-value 'display-line-numbers-mode))
      (mb-java-mode-hook-function)
      (should electric-pair-mode)
      (should (symbol-value 'display-line-numbers-mode)))
    (should (equal (default-value 'c-basic-offset)
                   default-c-basic-offset))
    (should (= (default-value 'tab-width) default-tab-width))
    (should (eq (default-value 'indent-tabs-mode)
                default-indent-tabs-mode))))

(provide 'test-mb-java-mode)

;;; test-mb-java-mode.el ends here
