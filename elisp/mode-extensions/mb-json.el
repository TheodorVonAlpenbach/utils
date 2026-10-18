;;; mb-json.el --- Helpers for nonstandard JSON input -*- lexical-binding: t; -*-

;;; Code:

(require 'cl-lib)
(require 'json)

(defun mb-json--replace-buffer (string)
  "Replace the current buffer contents with STRING."
  (erase-buffer)
  (insert string))

(defun json-remove-outer-quotes ()
  "Decode a JSON string containing serialized JSON.
Leave ordinary JSON objects and arrays unchanged.  Return non-nil
when the buffer was decoded."
  (interactive)
  (let ((source (buffer-substring-no-properties (point-min) (point-max))))
    (when (and (> (length source) 1)
               (eq (aref source 0) ?\")
               (eq (aref source (1- (length source))) ?\"))
      (let ((decoded (json-parse-string source)))
        (unless (stringp decoded)
          (error "Outer JSON value is not a string"))
        (save-excursion
          (mb-json--replace-buffer decoded))
        t))))

(defconst mb-json--unquoted-iso-time-regexp
  (concat
   "\\(^[[:space:]]*\\|\\(?:\\[\\|[:,]\\)[[:space:]]*\\)"
   "\\([0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}"
   "T[0-9]\\{2\\}:[0-9]\\{2\\}:[0-9]\\{2\\}"
   "\\(?:\\.[0-9]+\\)?Z\\)"
   "\\([[:space:]]*\\(?:\\]\\|}\\|,\\|\\'\\)\\)")
  "Regexp matching an unquoted UTC timestamp in a JSON value position.")

(defun json-quote-iso-time ()
  "Quote unquoted ISO UTC timestamps in the current buffer.
Return the number of timestamps quoted.  Already quoted strings
are left unchanged."
  (interactive)
  (let ((count 0))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward mb-json--unquoted-iso-time-regexp nil t)
        (unless (nth 3 (syntax-ppss (match-beginning 2)))
          (replace-match "\\1\"\\2\"\\3" nil nil)
          (cl-incf count))))
    count))

(defun json-pretty-print-string ()
  "Pretty-print JSON in the current buffer.
If the buffer contains a JSON string whose value is serialized
JSON, decode that outer string first.  Also quote bare UTC
timestamps used as values before formatting."
  (interactive)
  (save-excursion
    (json-remove-outer-quotes)
    (json-quote-iso-time)
    (ignore
     (json-parse-string
      (buffer-substring-no-properties (point-min) (point-max))))
    (json-pretty-print (point-min) (point-max))))

(defun object-id-to-string ()
  "Convert Mongo shell ObjectId and ISODate forms to JSON strings.
Return the number of values converted."
  (interactive)
  (let ((count 0))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward
              "\\(ObjectId\\|ISODate\\)(\"\\([^\"]+\\)\")"
              nil t)
        (replace-match "\"\\1:\\2\"" nil nil)
        (cl-incf count)))
    count))

(provide 'mb-json)

;;; mb-json.el ends here
