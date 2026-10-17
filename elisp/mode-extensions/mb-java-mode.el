;;; mb-java-mode.el --- Personal Java mode extensions -*- lexical-binding: t; -*-

;;; Code:

(require 'cc-mode)

(defvar parens-require-spaces)

(defun mb-java-mode-hook-function ()
  "Apply personal buffer-local settings for `java-mode'."
  (display-line-numbers-mode 1)
  (electric-pair-local-mode 1)
  (setq-local c-basic-offset 4)
  (setq-local tab-width 4)
  (setq-local indent-tabs-mode nil)
  (setq-local parens-require-spaces nil))

(add-hook 'java-mode-hook #'mb-java-mode-hook-function)

(provide 'mb-java-mode)

;;; mb-java-mode.el ends here
