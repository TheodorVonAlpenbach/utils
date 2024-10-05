(require 'cc-mode)
;;java-mode

(cl-defun mb-java-mode-hook-function ()
  (linum-mode)
  (electric-pair-local-mode)
  (setq java-indent-level 4)
  (setq tab-width 4)
  (setq indent-tabs-mode nil)
  (setq-local parens-require-spaces nil))
(add-hook 'java-mode-hook 'mb-java-mode-hook-function)

(provide 'mb-js-mode)
