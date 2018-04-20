(require 'ert)
(require 'lsconf-sensors)

(ert-deftest test-buffer-major-mode ()
  "Test of `buffer-major-mode'"
  (should (equal (buffer-major-mode "*Messages*") 'messages-buffer-mode)))

(ert-deftest test-major-mode-p ()
  "Test of `major-mode-p'"
  (should (major-mode-p 'messages-buffer-mode "*Messages*")))

(provide 'test-mb-utils-buffer.el)
