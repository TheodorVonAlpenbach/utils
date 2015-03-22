(require 'ee-log)

(defvar *ee-server* nil)
(defvar *ee-server-port* 1000)
(defvar *ee-server-clients* nil)

(defun ee-server-start ()
  "starts an emacs echo server"
  (unless *ee-server*
    (setf *ee-server*
	  (make-network-process
	   :name "mb"
	   :buffer "*mb*"
	   :family 'ipv4
	   :service *ee-server-port*
	   :sentinel #'ee-server-sentinel
	   :filter #'ee-server-filter
	   :server t))))
;;(ee-server-start)

(defun ee-server-stop ()
  (delete-process *ee-server*)
  (setf *ee-server* nil)
  (setf *ee-server-clients* nil))
;;(ee-server-stop)

(defun ee-server-sentinel (proc string)
  "Receives incoming client connection requests."
  (ee-server-receive-log string :signal)
  (push proc *ee-server-clients*))

(defun ee-server-filter (proc string)
  "Receives incoming data from PROC."
  (ee-server-receive-log string))

(defun ee-server-send (msg)
  (ee-server-send-log msg)
  (aif (first *ee-server-clients*)
    (process-send-string it msg)
    (message "Warning! No clients are currently connected.")))

;;(ee-server-send "ping")
;;(ee-server-send "can-insert-for")
;;(ee-server-send "can-remove-for")
;;(ee-server-send "get-dirty-buffers")
;;(ee-server-send "get-buffer-content")
;;(ee-server-send "caret")
;;(ee-server-send "get-windows")

;;(ee-server-send (format "goto %s %s %s" (url-encode-url "/cygdrive/c/Users/eier/Google Drive/site-lisp/mb-lisp/EditorEngine.Emacs/ee-log.el") 3 3))

(provide 'ee-server)
