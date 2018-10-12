(require 'ruby-mode)

(defun mb-ruby-init ()
  (setf evil-symbol-word-search t)
  (modify-syntax-entry ?_  "_"))

(add-hook 'ruby-mode-hook 'mb-ruby-init)   ; with AUCTeX LaTeX mode

(provide 'mb-ruby)

