;;; logs
(defun ee-log (string)
  (with-buffer* "*mb*"
    (goto-char (point-max))
    (insert (format "%s\n" string))
    (goto-char (point-max))))

(defun ee-role-log (string id send-p signal-p)
  (ee-log (format "%s %s %s `%s'"
	    id
	    (if send-p "sent" "received")
	    (if signal-p "signal" "message")
	    string)))
(defun ee-server-log (string send-p signal-p)
  (ee-role-log string "server" send-p signal-p))
(defun ee-client-log (string send-p signal-p)
  (ee-role-log string "client" send-p signal-p))

(defun ee-server-send-log (string &optional signal-p)
  (ee-server-log string t signal-p))
(defun ee-server-receive-log (string &optional signal-p)
  (ee-server-log string nil signal-p))
(defun ee-client-send-log (string &optional signal-p)
  (ee-client-log string t signal-p))
(defun ee-client-receive-log (string &optional signal-p)
  (ee-client-log string nil signal-p))

;;; some test calls
;;(getf (process-contact (first *ee-clients*) t) :local)

(provide 'ee-log)
