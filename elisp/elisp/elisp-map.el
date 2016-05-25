(require 'mb-insert)
;(require 'mb-gnus)

(defun escape-parenthesis (begin end)
  "Escapes all parentheses in region."
  (interactive "*r")
  (save-excursion
    (goto-char begin)
    (while (re-search-forward "[()]" end t)
      (backward-char 1)
      (insert ?\\)
      (forward-char 1)
      (incf end))))

;; elisp macros
(define-key lisp-mode-shared-map "\C-cp" 'escape-parenthesis)
(define-key lisp-mode-shared-map "\C-ce" 'eval-buffer)
(define-key lisp-mode-shared-map "\C-cr" 'eval-region)
(define-key lisp-mode-shared-map "\C-c\C-c" 'comment-region)

(provide 'elisp-map)