(require 'json-mode)

(cl-defun json-remove-outer-quotes ()
  (save-excursion
    (bob)
    (when (looking-at "\\\\")
      (delete-char 1)
      (eob)
      (delete-backward-char 1)
      (while (re-search-forward "\\\\\"" nil t)
	(replace-match "\"")))))

(cl-defun json-quote-iso-time ()
  (save-excursion
    (bob)
    (while (re-search-forward "....-..-..T..:..:..\\.[^,]*Z" nil t)
      (replace-match "\"\\&\""))))

(cl-defun json-pretty-print-string ()
  "Pretty print buffer on form '\"{<json-string-with-escaped-double-quotes>}\"."
  (interactive)
  (json-mode)			  ; ensure that buffer is in json-mode
  (save-excursion
    (json-remove-outer-quotes)
    (json-quote-iso-time)
    (json-pretty-print (point-min) (point-max))))
;;(json-pretty-print-string)

(cl-defun object-id-to-string ()
  (interactive)
  (save-excursion
    (bob)
    (while (re-search-forward "\\(ObjectId\\|ISODate\\)(\"\\([^\"]+\\)\")" nil t)
       (replace-match "\"\\1:\\2\""))))

(provide 'mb-json)
