(defvar *buffer-column-base* 1)

(cl-defun line-column->point (line column &optional (buffer (current-buffer)))
  (with-buffer buffer
    (goto-char (point-min))
    (forward-line (1- line))
    (forward-char (- column *buffer-column-base*))
    (point)))
;;(line-column->point 2 1)

(provide 'ee-utils)
