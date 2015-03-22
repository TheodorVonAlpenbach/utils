;;;; The ee client. In this version only one client is allowed in an
;;;; emacs session.
(require 'ee-server)

(defvar *ee-clients* nil)

(defun kill-ee-clients ()
  (loop for x in *ee-clients*
     do (delete-process x))
  (setf *ee-clients* nil))
;;(kill-ee-clients)

(defun ee-connect ()
  (if *ee-clients*
    (error "A client is already connected. This version only supports one client at a time")
    (let ((proc (open-network-stream
		 "ee-client"
		 nil ;;"*ee-client*"
		 "localhost"
		 *ee-server-port*)))
      (push proc *ee-clients*)
      (set-process-sentinel proc #'ee-client-sentinel)
      (set-process-filter proc #'ee-client-filter))))
;;(ee-connect)

(defun ee-client-sentinel (proc string)
  (ee-client-receive-log string :signal))

(defun ee-string (string)
  (substitute ))

(defun ee-client-send (msg)
  (ee-client-send-log msg)
  (process-send-string (first *ee-clients*) msg))

;;; utils
(defun ee-list (list) (concat* list :in "|")) ;must be reimplemented if not providing mb-utils
(defun ee-bool (x) (if x "true" "false"))

;;; Protocol implementation
(defun ee-buffers () (remove-if-not #'buffer-file-name (buffer-list)))
(defun dirty-buffers () (remove-if-not #'buffer-modified-p (ee-buffers)))
(defun dirty-buffer-files ()
  (remove nil (mapcar #'buffer-file-name (dirty-buffers))))
(defun ee-dirty-buffer-files () (ee-list (dirty-buffer-files)))
;;(ee-dirty-buffer-files)
(defun ee-windows () (ee-list (mapcar #'buffer-file-name (ee-buffers))))

(defun ee-parse-message (string)
  (loop for (x . pos) = (read-from-string-safe string)
                        then (read-from-string-safe string pos)
        while pos collect (if (symbolp x) (symbol-name x) x)))
;;(ee-parse-message "\"goto\" qwe 1")
;;(read " goto x y z")

;;; TODO: resolve this hack when we know how to handle spaces in filenames
(defun ee-goto-file (filename line column)
  (find-file (url-unhex-string filename))
  (goto-char (line-column->point line column)))
;;(apply #'ee-goto-file (list "/cygdrive/c/Users/eier/Google%20Drive/site-lisp/mb-lisp/EditorEngine.Emacs/ee-log.el" 3 3))

(defun ee-client-filter (proc string)
  (ee-client-receive-log string)
  (let ((args (ee-parse-message string)))
    (ee-client-send
     (string-case (first args)
       ("ping" "pong")
       ("get-dirty-buffers" (ee-dirty-buffer-files))
       (("can-insert-for" "can-remove-for") (ee-bool (not buffer-read-only)))
       ("get-buffer-content" (replace-regexp-in-string "\n" "||newline||" (buffer-string-no-properties)))
       ("caret" (format "%s|%s|%s" (buffer-file-name) (current-column) (line-number-at-pos)))
       ("get-windows" (ee-windows))
       ;; other messages are not replied to
       (t (string-case (first args)
	    ("goto" (apply #'ee-goto-file (rest args)))))))))

(defun ee-report-change ()
  (awhen (buffer-file-name)
    (ee-client-send (format "editor buffer-changed %s" it))))
(add-hook 'first-change-hook #'ee-report-change)

(provide 'ee-client)


