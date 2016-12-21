(defun set-mb-python-locals ()
  "My local modifications of PYTHON-MODE"
  ;; Disable intraspace for INSERT-PARENTHESES
  (setq-local parens-require-spaces nil))
   
(defun mb-python-last-sexp-region ()
  (let ((end (point)))
    (re-search-backward "[][:alnum:])]")
    (forward-char 1)
    (backward-sexp 1)
    (when (looking-at "(")
      (backward-sexp 1))
    (list (point) end)))

(defun mb-python-defun-region ()
  "Return the region of the current Python defun."
  (list (bod*) (eod*)))

(add-hook 'python-mode-hook #'set-mb-python-locals)

(provide 'mb-python)
