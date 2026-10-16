;;; mb-ruby.el --- Personal Ruby mode extensions -*- lexical-binding: t; -*-

;;; Code:

(require 'ruby-mode)

(defvar evil-symbol-word-search)
(defvar parens-require-spaces)

(defun mb-ruby-init ()
  "Apply personal buffer-local settings for `ruby-mode'."
  (setq-local parens-require-spaces nil)
  (setq-local evil-symbol-word-search t))

(add-hook 'ruby-mode-hook #'mb-ruby-init)

(provide 'mb-ruby)

;;; mb-ruby.el ends here
