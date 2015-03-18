;; This buffer is for notes you don't want to save, and for Lisp evaluation.
;; If you want to create a file, visit that file with C-x C-f,
;; then enter the text in that file's own buffer.

(defvar echo-server-port 10000
  "port of the echo server")

(defvar echo-server-clients '()
  "alist where KEY is a client process and VALUE is the string")


(defun echo-server-start nil
  "starts an emacs echo server"
  (interactive)
  (unless (process-status "echo-server")
    (make-network-process :name "echo-server" :buffer "*echo-server*" :family 'ipv4 :service echo-server-port :sentinel 'echo-server-sentinel :filter 'echo-server-filter :server 't)
    (setq echo-server-clients '())
    )
  )

(defun echo-server-stop nil
  "stop an emacs echo server"
  (interactive)
  (while  echo-server-clients
    (delete-process (car (car echo-server-clients)))
    (setq echo-server-clients (cdr echo-server-clients)))
  (delete-process "echo-server")
  )

(find-file-noselect "emacs-ee.el")

(switch-to-buffer (find-file-noselect "/home/ack/src/tmp/repl.fs"))
(switch-to-buffer (find-file-other-window "/home/ack/src/tmp/repl.fs"))
(switch-to-buffer (find-file-other-window "/home/ack/tmp/repl.fs"))

(defun echo-server-filter (proc string)
  (let ((pending (assoc proc echo-server-clients))
	message
	index)
    ;;create entry if required
    (unless pending
      (setq echo-server-clients (cons (cons proc "") echo-server-clients))
      (setq pending  (assoc proc echo-server-clients)))
    (setq message (concat (cdr pending) string))
    (while (setq index (string-match "\n" message))
      (setq index (1+ index))
      (setq msg (create-response (substring message 0 index)))
      (process-send-string proc msg)
      (echo-server-log  msg proc)
      (setq message (substring message index)))
    (setcdr pending message))
  )

(create-response "get-caret")

(buffer-list)

(defun create-response (message)
  (pcase message
    ("get-caret" (get-caret))))

(defun get-caret nil
    (format "%s|%d|%d\n%s" buffer-file-name (1+ (count-lines 1 (point))) (current-column) (buffer-substring-no-properties 1 (buffer-size))))

(defun echo-server-sentinel (proc msg)
  (when (string= msg "connection broken by remote peer\n")
    (setq echo-servers-clients (assq-delete-all proc echo-server-clients))
    (echo-server-log (format "client %s has quit" proc))))

;;from server.el
(defun echo-server-log (string &optional client)
  "If a *echo-server* buffer exists, write STRING to it for logging purposes."
  (if (get-buffer "*echo-server*")
      (with-current-buffer "*echo-server*"
	(goto-char (point-max))
	(insert (current-time-string)
		(if client (format " %s:" client) " ")
		string)
	(or (bolp) (newline)))))

(echo-server-start)
(echo-server-stop)

