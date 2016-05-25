(defvar ftp-default-directory "ftps8.brinkster.com:")

(defun ftp-file-relative-name (url-path &optional url-dir)
  "Refinement of `file-relative-name'. Avoids filenames like '.'"
  (let ((res (file-relative-name (ftp-remote-path url-path) 
				 (ftp-remote-path (or url-dir ftp-default-directory)))))
    (cond  ((string= res "./") "")
	   ((string= res ".") "")
	   (t res))))
;;(ftp-file-relative-name "ftps8.brinkster.com:wer" nil)

(defun ftp-completion (url-string predicate flag)
  "Pretty much reimplementation of `read-file-name''s \(assumed\)
version of `try-completion'"
  (let* ((dir (or (file-name-directory url-string) ftp-default-directory)))
    (if flag
      (ftp-all-completions url-string dir)
      (ftp-try-completion url-string dir))))
;;(ftp-completion "ftps8.brinkster.com:mats/" nil nil)
;;(completing-read "FTP find file: " #'ftp-completion nil nil "ftps8.brinkster.com:mats/")

(defun ftp-all-completions (url-string dir)
  "Refinement of base `all-completions'"
  (ftp-file-name-all-completions (ftp-file-relative-name url-string dir) (or dir ftp-default-directory)))
;;(ftp-all-completions "" "ftps8.brinkster.com:")

(defun ftp-try-completion (string dir)
  "Refinement of base `try-completion'"
  (let ((res (ftp-file-name-completion (ftp-file-relative-name string (or dir ftp-default-directory))
				      (or (not-empty dir) ftp-default-directory))))
    (or (eq res t) (concat dir res))))
;;(ftp-try-completion "" "ftps8.brinkster.com:")

(defun ftp-read-file-name (&optional prompt remote-dir refresh)
  "FTP version of `read-file-name'. Value is not expanded---you must
call `ftp-expand-file-name' yourself. Prefix argument forces ftp
interactive."
  (if (or (local-variable-p 'ftp-url)
	  current-prefix-arg
	  remote-dir 
	  refresh)
    (let ((url (if (local-variable-p 'ftp-url) ftp-url ftp-default-directory))) 
      (when (or refresh (eql current-prefix-arg -1)) 
	(nilf ftp-dir-results))
      (completing-read (or prompt "FTP find file: ")
		       #'ftp-completion nil nil 
		       (or (not-empty remote-dir) ftp-default-directory)))
    (read-file-name "Find file: ")))
;;(ftp-read-file-name "FTP find file: " "")

(defun ftp-set-auto-mode (buffer-name)
  (funcall (first (loop for item in auto-mode-alist
			for regexp = (car item)
			for mode = (cdr item)
			if (string-match regexp buffer-name) collect mode))))
;;(ftp-set-auto-mode "qwe.el")
