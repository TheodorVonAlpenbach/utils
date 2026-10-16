;;; mb-python.el --- Personal Python mode extensions -*- lexical-binding: t; -*-

;;; Code:

(require 'python)

(defvar evil-symbol-word-search)
(defvar parens-require-spaces)

(defun set-mb-python-locals ()
  "Apply personal buffer-local settings for `python-mode'."
  (setq-local parens-require-spaces nil)
  (setq-local evil-symbol-word-search t)
  (display-line-numbers-mode 1))

(defun mb-python-last-sexp-region ()
  "Return the bounds of the Python expression preceding point.
Keep point unchanged.  Include the callable or indexed expression
when the final sexp is an adjacent parenthesized call or subscript."
  (let ((end (point)))
    (save-excursion
      (condition-case nil
          (backward-sexp 1)
        (scan-error
         (user-error "No complete Python expression before point")))
      (when (= (point) end)
        (user-error "No complete Python expression before point"))
      (when (and (memq (char-after) '(?\( ?\[))
                 (let ((previous (char-before)))
                   (and previous
                        (not (memq previous '(?\s ?\t ?\n ?\r))))))
        (condition-case nil
            (backward-sexp 1)
          (scan-error nil)))
      (list (point) end))))

(defun mb-python-defun-region ()
  "Return the bounds of the current Python def or class."
  (let ((start
         (save-excursion
           (when (python-nav-beginning-of-defun)
             (point))))
        (end
         (save-excursion
           (when (python-nav-end-of-defun)
             (point)))))
    (unless (and start end)
      (user-error "Point is not inside a Python def or class"))
    (list start end)))

(add-hook 'python-mode-hook #'set-mb-python-locals)

(provide 'mb-python)

;;; mb-python.el ends here
