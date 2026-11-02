;;; mb-buffer.el --- Small buffer and window commands -*- lexical-binding: t; -*-

;;; Code:

(defun mb-buffer-back-to-indentation-and-indent ()
  "Normalize whitespace at indentation and indent the current line."
  (interactive)
  (back-to-indentation)
  (just-one-space)
  (if (derived-mode-p 'text-mode)
      (indent-relative)
    (indent-for-tab-command)))

(defalias 'back-to-indentation-and-indent
  #'mb-buffer-back-to-indentation-and-indent)

(defun mb-buffer-insert-at-point (text &optional point buffer)
  "Insert TEXT at POINT in BUFFER.
BUFFER defaults to the current buffer.  POINT defaults to that
buffer's current point."
  (with-current-buffer (or buffer (current-buffer))
    (save-excursion
      (goto-char (or point (point)))
      (insert text))))

(defalias 'insert-at-point #'mb-buffer-insert-at-point)

(defun mb-buffer-replace-region (string beginning end)
  "Replace text from BEGINNING through END with STRING.
Point remains at the beginning of the replacement, and the kill
ring is not modified."
  (delete-region beginning end)
  (save-excursion
    (goto-char beginning)
    (insert string)))

(defalias 'buffer-replace-region #'mb-buffer-replace-region)

(defun mb-buffer-sort-characters (beginning end &optional reverse)
  "Sort characters between BEGINNING and END.
With REVERSE non-nil, sort in descending order."
  (interactive "r\nP")
  (let* ((predicate (if reverse #'> #'<))
         (characters
          (sort (string-to-list
                 (delete-and-extract-region beginning end))
                predicate)))
    (insert (apply #'string characters))))

(defalias 'sort-chars #'mb-buffer-sort-characters)

(defun mb-buffer-window-size (&optional horizontal window)
  "Return WINDOW's height, or width when HORIZONTAL is non-nil."
  (if horizontal
      (window-width window)
    (window-height window)))

(defalias 'get-window-size #'mb-buffer-window-size)

(defun mb-buffer-set-window-size (size &optional horizontal)
  "Resize the selected window to SIZE.
Resize horizontally when HORIZONTAL is non-nil."
  (shrink-window
   (- (mb-buffer-window-size horizontal) size)
   horizontal))

(defalias 'set-window-size #'mb-buffer-set-window-size)

(provide 'mb-buffer)

;;; mb-buffer.el ends here
