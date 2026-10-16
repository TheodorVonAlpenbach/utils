;;; mb-utils-search.el --- Buffer search utility functions -*- lexical-binding: t; -*-

;;; Code:

(defun exists-match-p (regexp &optional buffer)
  "Return the end position of the first REGEXP match in BUFFER.
Return nil when there is no match.  BUFFER defaults to the current buffer.
Respect its current narrowing and preserve point and match data."
  (with-current-buffer (or buffer (current-buffer))
    (save-excursion
      (save-match-data
	(goto-char (point-min))
	(re-search-forward regexp nil t)))))
;;(exists-match-p "regexp")

(provide 'mb-utils-search)

;;; mb-utils-search.el ends here
