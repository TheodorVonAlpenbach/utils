(defvar *buffer-column-base* 1)

(defun read-from-string-safe (string &optional start end)
  "Same as `read-from-string' but returns (nil . nil) if reaching the end of STRING."
  (condition-case nil
      (read-from-string string start end)
    (error (list nil))))
;;(read-from-string-safe "")

(cl-defun line-column->point (line column &optional (buffer (current-buffer)))
  (with-buffer buffer
    (goto-char (point-min))
    (forward-line (1- line))
    (forward-char (- column *buffer-column-base*))
    (point)))
;;(line-column->point 2 1)

(provide 'ee-utils)
