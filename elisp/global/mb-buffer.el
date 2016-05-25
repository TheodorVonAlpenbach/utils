(defun back-to-indentation-and-indent ()
  (interactive)
  (back-to-indentation)
  (just-one-space)
  (if (string= major-mode "text-mode")
    (indent-relative)
    (indent-for-tab-command)))

(cl-defun insert-at-point (text &optional (point (point)) (buffer (current-buffer)))
  "Inserts TEXT at buffers POINT"
  (save-excursion
    (set-buffer buffer)
    (goto-char point)
    (insert text)))
;;(insert-at-point "qwe" 10 "*scratch*")

(defun buffer-replace-region (string beg end)
  "Replaces all content in region from point BEG to point END
with STRING"
  (kill-region beg end)
  (insert-at-point string beg))

(defun sort-chars (beg end &optional reverse)
  "Sort characters alphabetically in region."
  (interactive "r")
  (insert (cl-sort (delete-and-extract-region beg end) #'<)))

(defun get-window-size (&optional side window)
  (if side (window-width window) (window-height window)))

(defun set-window-size (arg &optional side)
  (shrink-window (- (get-window-size side) arg) side))
;;(set-window-size 12 t)

(provide 'mb-buffer)
