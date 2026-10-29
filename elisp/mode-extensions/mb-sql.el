;;; mb-sql.el --- Personal SQL mode extensions -*- lexical-binding: t; -*-

;;; Code:

(require 'sql)
(require 'subr-x)

(defun mb-sql-send-expression ()
  "Send the current SQL line to the inferior SQL process.
Ignore a leading SQL comment marker, trim surrounding whitespace,
and preserve the leading newline used to avoid prompt alignment
issues."
  (interactive)
  (let* ((line
          (buffer-substring-no-properties
           (line-beginning-position) (line-end-position)))
         (expression
          (string-trim
           (replace-regexp-in-string
            "\\`[[:space:]]*--[[:space:]]*" "" line))))
    (when (string-empty-p expression)
      (user-error "No SQL expression on current line"))
    (sql-send-string (concat "\n" expression))))

(defun mb-sql-mode-setup ()
  "Install personal SQL bindings in the current buffer."
  (use-local-map (copy-keymap (current-local-map)))
  (local-set-key (kbd "C-c C-e") #'mb-sql-send-expression))

(add-hook 'sql-mode-hook #'mb-sql-mode-setup)

(provide 'mb-sql)

;;; mb-sql.el ends here
