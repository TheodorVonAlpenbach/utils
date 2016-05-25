;;; Swap files. For older versions, see archive.

(defconst +smart-swaps+
  '((tex pdf)
    (ly pdf)
    (gp pdf)
    (cpp h :search 1)
    ("Lilypond-compile" ("LilyPond started at .*\n\nlilypond \\(.*\\)$" 1)))
  "TODO: enable :search")

(defun regexp-swap (file-regexp)
  (destructuring-bind (regexp num) file-regexp
    (awhen (string-match* regexp (buffer-string-no-properties) :num 1)
      (find-file it))))

(defun substitute-extension (filename extension)
  (format "%s.%s" (file-name-sans-extension filename) (sstring extension)))
;;(substitute-extension (buffer-file-name) 'ly)

(defun expand-swap-target (expr)
  (cond	((symbolp expr) (substitute-extension (buffer-file-name) expr))
	((consp expr)
	 (string-match* (first expr) (buffer-string-no-properties) :num (second expr)))))
;;(swap-target '("filename='\\(.*\\)'" 1))

(defun swap-target (expr)
  (awhen (expand-swap-target expr)
    (when (file-exists-p it) it)))

(defun match-current-buffer-p (expr)
  (cond ((symbolp expr)
	 (when (string= (file-name-extension (buffer-file-name))
			(symbol-name expr))
	   (buffer-file-name)))
	((stringp expr)
	 (when (string-match expr (buffer-file-name))
	   (buffer-file-name)))))

(cl-defun smart-swap-find-target (&optional (swaps +smart-swaps+))
  (loop for (from to) in swaps
	when (or (when (match-current-buffer-p from) (swap-target to))
		 (when (match-current-buffer-p to) (swap-target from)))
     return it))

(cl-defun smart-swap (&optional (swaps +smart-swaps+))
  "Swaps to file as defined in SWAPS"
  (interactive)
  (aif (smart-swap-find-target swaps)
      (find-file it)
      (message "Couldn't find swap target for current file")))

(provide 'mb-files)

