(require 'json-mode)

(defun json-pretty-print-string ()
  "Pretty print buffer on form '\"{<json-string-with-escaped-double-quotes>}\"."
  (interactive)
  (json-mode) ; ensure that buffer is in json-mode
  (save-excursion
    (eob)
    (delete-backward-char 1)
    (bob)
    (delete-char 1)
    (while (re-search-forward "\\\\\"" nil t)
      (replace-match "\""))
    (json-pretty-print (point-min) (point-max))))
;;(json-pretty-print-string)

(provide 'mb-json)
