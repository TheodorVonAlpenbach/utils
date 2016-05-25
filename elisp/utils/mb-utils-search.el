(defun* exists-match-p (regexp &optional (buffer (current-buffer)))
  "Returns posistion of match of string in BUFFER with REGEXP. If no
match, NIL is returned."
  (save-excursion
    (goto-char (point-min))
    (re-search-forward regexp nil t)))
;;(exists-match-p "regexpt")

(provide 'mb-utils-search)