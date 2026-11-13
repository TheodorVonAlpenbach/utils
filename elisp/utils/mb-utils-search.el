;;; mb-utils-search.el --- Buffer search utility functions -*- lexical-binding: t; -*-

;;; Code:

(defun exists-match-p (regexp &optional buffer)
  "Return the end position of the first visible REGEXP match in BUFFER.
Return nil when there is no match.  BUFFER may be a live buffer or the
name of an existing buffer and defaults to the current buffer.  Search
from `point-min' to `point-max', respecting narrowing, while preserving
the current buffer, point, and match data."
  (with-current-buffer (or buffer (current-buffer))
    (save-excursion
      (save-match-data
	(goto-char (point-min))
	(re-search-forward regexp nil t)))))

(provide 'mb-utils-search)

;;; mb-utils-search.el ends here
