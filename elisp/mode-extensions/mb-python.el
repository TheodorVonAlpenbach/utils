(defun set-mb-python-locals ()
  "My local modifications of PYTHON-MODE"
  ;; Disable intraspace for INSERT-PARENTHESES
  (setq-local parens-require-spaces nil))
   
(defun mb-python-last-sexp-region ()
  (save-excursion
    (nreverse
     (list (point)
	   (1+ (re-search-backward "[[:space:]][^[:space:]]+"))))))

(add-hook 'python-mode-hook #'set-mb-python-locals)

(provide 'mb-python)
