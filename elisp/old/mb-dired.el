;;;; MB additions to Dired Mode.

(defun dired-query-replace-filenames ()
  ""
  (interactive)
  (when (eq major-mode 'dired-mode)
    (message "defun active!")))

