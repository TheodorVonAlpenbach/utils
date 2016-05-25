;;;; This module contains extensions of and advices to functions defined in simple.el
(require 'simple)

(cl-defun mb-forward-word (&optional (arg 1))
  "Like `forward-word', but treats /all/ contiguous groups of
word constituent characters as words. For instance, "
  (interactive "^p")
  (re-search-forward "\\w\\W" nil nil arg)
  (backward-char (signum arg)))
(advice-add #'forward-word :override #'mb-forward-word)
;;(advice-remove #'forward-word #'mb-forward-word)

(provide 'mb-simple)
