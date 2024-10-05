(require 'ruby-mode)

(cl-defun mb-ruby-init ()
  (make-local-variable 'parens-require-spaces)
  (setf parens-require-spaces nil)
  (setf evil-symbol-word-search t)
  (modify-syntax-entry ?_  "_"))

(add-hook 'ruby-mode-hook 'mb-ruby-init)   ; with AUCTeX LaTeX mode

(provide 'mb-ruby)

