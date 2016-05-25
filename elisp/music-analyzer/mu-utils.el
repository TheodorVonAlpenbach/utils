(defun eshell/mbgrep (string)
  "Apply egrep on string"
  (apply #'eshell/egrep string (file-expand-wildcards "c:/emacs-22.1/site-lisp/mb-lisp/*/*.el")))

(provide 'mu-utils)