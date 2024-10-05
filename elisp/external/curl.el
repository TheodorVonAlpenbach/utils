(require 'curl-for-url)

(cl-defun curl-super-basic (url)
  "Display content from URL in other window"
  (let*
      ((buffer (url-retrieve url 'mycallback))
       (url-request-data "{\"from\":0,\"size\":1}")
       (url-request-method "GET")
       (url-request-extra-headers "Content-Type: application/json"))
    (cl-defun mycallback (cbargs)
      (switch-to-buffer-other-window buffer))))
;;(curl-super-basic "http://localhost:9200/ada-portal/_doc/_search?pretty")

(cl-defun copy-jwt-token ()
  "Extract JWT token values from cURL-header at the bottom of buffer to all other occurrences.

Note, this version assumes that the token is the last property in string, so that it is immediately followed by the single quote character. This might change."
  (interactive)
  ;; 1. find token
  (eob)
  (re-search-backward "jwt_token=")
  (re-search-forward "=")
  (let ((token (message (substring (buffer-substring-no-properties
				    (point) (re-search-forward "'"))
				   0 -1))))
    (bob)
    ;; 2. replace
    (while (re-search-forward "\\(jwt_token=\\)[^']*" nil t)
      (replace-match (concat "\\1" token))))
  ;; 3. delete cURL-header at the bottom of buffer
  (eob)
  (backward-kill-paragraph 1)
  (newline))



(provide 'curl)
