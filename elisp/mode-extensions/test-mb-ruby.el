;;; test-mb-ruby.el --- Tests for mb-ruby -*- lexical-binding: t; -*-

(require 'ert)
(require 'mb-ruby)

(ert-deftest mb-ruby-buffer-local-settings ()
  (let ((evil-was-bound (boundp 'evil-symbol-word-search))
        (default-evil-symbol-word-search
         (and (boundp 'evil-symbol-word-search)
              (default-value 'evil-symbol-word-search))))
    (with-temp-buffer
      (ruby-mode)
      (should (local-variable-p 'parens-require-spaces))
      (should-not parens-require-spaces)
      (should (local-variable-p 'evil-symbol-word-search))
      (should (symbol-value 'evil-symbol-word-search))
      (should (eq (char-syntax ?_) ?_))
      (mb-ruby-init)
      (should (symbol-value 'evil-symbol-word-search)))
    (should (eq (boundp 'evil-symbol-word-search) evil-was-bound))
    (when evil-was-bound
      (should (eq (default-value 'evil-symbol-word-search)
                  default-evil-symbol-word-search)))))

(provide 'test-mb-ruby)

;;; test-mb-ruby.el ends here
