(require 'ert)

(ert-deftest test-buffer-major-mode ()
  "Test of `buffer-major-mode'"
  (should (equal (buffer-major-mode "*Messages*") 'messages-buffer-mode)))

(ert-deftest test-major-mode-p ()
  "Test of `major-mode-p'"
  (should (major-mode-p 'messages-buffer-mode "*Messages*")))

(ert-deftest test-transpose-string ()
  "Test of `transpose-string'"
  (should (equal (transpose-string "abc") "a
b
c")))

(provide 'test-mb-utils-buffer.el)
