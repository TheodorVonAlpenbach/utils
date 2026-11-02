;;; mb-frames.el --- Commands for resizing frames -*- lexical-binding: t; -*-

;;; Code:

(defun mb-frames-resize (amount &optional horizontal frame)
  "Shrink FRAME by AMOUNT rows or columns.
Resize horizontally when HORIZONTAL is non-nil.  Negative AMOUNT
enlarges the frame.  FRAME defaults to the selected frame."
  (interactive "P")
  (let* ((target (or frame (selected-frame)))
         (delta (prefix-numeric-value (or amount 1)))
         (current (if horizontal
                      (frame-width target)
                    (frame-height target)))
         (size (- current delta)))
    (when (< size 1)
      (user-error "Frame size must be positive"))
    (if horizontal
        (set-frame-width target size)
      (set-frame-height target size))))

(defalias 'shrink-frame #'mb-frames-resize)

(defun mb-frames-enlarge (amount &optional frame)
  "Enlarge FRAME vertically by AMOUNT rows."
  (interactive "P")
  (mb-frames-resize (- (prefix-numeric-value (or amount 1)))
                    nil frame))

(defalias 'enlarge-frame #'mb-frames-enlarge)

(defun mb-frames-shrink-horizontally (amount &optional frame)
  "Shrink FRAME horizontally by AMOUNT columns."
  (interactive "P")
  (mb-frames-resize amount t frame))

(defalias 'shrink-frame-horizontally
  #'mb-frames-shrink-horizontally)

(defun mb-frames-enlarge-horizontally (amount &optional frame)
  "Enlarge FRAME horizontally by AMOUNT columns."
  (interactive "P")
  (mb-frames-resize (- (prefix-numeric-value (or amount 1)))
                    t frame))

(defalias 'enlarge-frame-horizontally
  #'mb-frames-enlarge-horizontally)

(provide 'mb-frames)

;;; mb-frames.el ends here
