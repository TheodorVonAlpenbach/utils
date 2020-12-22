(defvar es-url "http://localhost:9200/ada-portal/_search?pretty")
;;(setf es-url "http://localhost:9200/ada-portal/_search?pretty")
;;(setf url-request-extra-headers '(("Content-Type" . "application/json")))

(defun mycallback (cbargs)
	(print cbargs)
	;; (switch-to-buffer-other-window buffer)
	(kill-whole-line 5)
	(json-mode))

(defun elastic-search (json-string)
  "Display content from URL in other window"
  (let* ((url-request-data json-string)
	 (url-request-method "GET"))
    (switch-to-buffer-other-window (url-retrieve es-url 'mycallback))))
;;(elastic-search "")

(defun elastic-search (json-string)
  "Display content from URL in other window"
  (let* ((url-request-data json-string)
	 (url-request-method "GET"))
    (lexical-let ((buffer (url-retrieve es-url 'mycallback)))
      (print buffer))))
;;(elastic-search "")
;;(elastic-search "{\"from\":0,\"size\":1}")
;;(switch-to-buffer-other-window (url-retrieve-synchronously es-url))

(defun es-region (beg end)
  (interactive "r")
  (elastic-search (buffer-substring-no-properties beg end)))

(defun es-at-point ()
  (interactive)
  (elastic-search (paragraph-string)))

(provide 'elastic-search)

(defun regexp-replace (regexp replacement)
  (save-excursion
    (bob)
    (while (re-search-forward regexp nil t)
     (replace-match replacement))))

(defun sql-clean ()
  (interactive)
  (regexp-replace "(" " ")
  (regexp-replace ")" "")
  (regexp-replace "\\." " ")
  (regexp-replace "eq" "="))
