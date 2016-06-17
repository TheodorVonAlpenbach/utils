(defun set-mb-python-locals ()
  "My local modifications of PYTHON-MODE"
  ;; Disable intraspace for INSERT-PARENTHESES
  (setq-local parens-require-spaces nil))

(add-hook 'python-mode-hook #'set-mb-python-locals)

(provide 'mb-python)
