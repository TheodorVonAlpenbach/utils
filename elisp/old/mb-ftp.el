;;;; Updated by Mats BergstrÅ¯m 2005-02-08
;;;;
;;;; For FTP manual, see http://www.ucalgary.ca/~physed/mmi/ftp-manual.html

;;;; o step 1: establish primitive GUI: find-file, save-buffer, write-file
;;;; o step 2: make primitives stable concerning connection termination
;;;; o step 3: add completion
;;;; * step 4: Optional. Implement primitives into the `magic
;;;; filename''s regime.

(require 'comint) ;;obsolete?

(defconst netrc-filename "c:/.netrc")
(defconst mb-ftp-process "ftp")
(defconst mb-ftp-process-args (list "-i" "-n" "-v" "-g" "-d"))
(defconst mb-ftp-tmp-file (concat *mb-lisp-path* "ftp/tmp/ftp-tmpfile"))

;;; FTP comint process settings
(add-hook 'comint-output-filter-functions #'comint-strip-ctrl-m) ;; doesn't seem to help, though

(defun ftp-get-process (hostname)
  (or (get-process hostname)
      (let ((process (apply #'start-process hostname hostname mb-ftp-process mb-ftp-process-args)))
;;           (set-process-sentinel process #'ftp-sentinel)
             (set-process-filter process #'ftp-process-filter)
             process)))
;;(ftp-get-process "ftps8.brinkster.com")

;;; .netrc and host utils
(defun parse-netrc ()
  "Simplified. Looks for `machine <domain> login <username> password
<password>'. Supports only the \"machine\", \"login\", \"password\"
keywords."
  (mapcar #'(lambda (x) (mapcar #'second x))
               (group-list (group-list (split-string (file-string netrc-filename)) 
				       2) 
			   3)))
;;(parse-netrc)

(defun ftp-find-host (hostname)
  "Returns tuple \(hostname user password\) for host HOSTNAME."
  (let ((host (assoc hostname (parse-netrc))))
    (unless host (unless  (error "Couldn't find host %s" host)))
    host))

(defun ftp-url-p (url) (consp (assoc (ftp-domain url) (parse-netrc))))
(defun ftp-parse-url (url) (split-string url ":"))
(defun ftp-domain (url) (first (ftp-parse-url url)))
(defun ftp-remote-path (url) (or (second (ftp-parse-url url)) ""))
(defun ftp-process (url) (ftp-get-process (ftp-domain url)))
(defun ftp-url (process path) (format "%s:%s" (ftp-hostname process) path))
(defun ftp-hostname (process) (process-name process))
(defun ftp-user (process) (second (ftp-find-host (ftp-hostname process))))
(defun ftp-password (process) (third (ftp-find-host (ftp-hostname process))))
(defun ftp-sentinel (process state))

;;; FTP commands
(defun ftp-ensure-login (process)
  (when (ftp-disconnected-p process)
    (ftp-open process)
    (ftp-login process)))

(defun ftp-disconnected-string-p (process-string)
  (or (string-match "Not connected" process-string)
      (string-match "421 Timeout" process-string)))

(defun* ftp-wait (process &optional (command "UNKNOWN") (n -1) (timeout 5)
(timeout-msecs 0))
  "Same as `accept-process-output' but with debug info in process buffer."
  (goto-char (point-max))
  (insert (message "%s for %s (%d)\n" "WAIT" (upcase command) n))
  (accept-process-output process timeout timeout-msecs))

(defun ftp-process-filter-old (process output)
  (message "ftp-process-filter received output: '%s'" output)
  (with-buffer* (process-buffer process)
    (goto-char (point-max))
    (insert (format "--- %s ---\n" (iso-time :with-seconds t)))
    (insert output)))

(defun ftp-process-filter (process output)
  ;;(message "ftp-process-filter received output: '%s'" output)
  (with-buffer* (process-buffer process)
    (goto-char (point-max))
    (insert (format "--- %s ---\n" (iso-time :with-seconds t)))
    (insert output)
    (insert "\n")
;;     (when (or (string-match "Connected to ftps8.brinkster.com" output)
;;          (string-match "--->" output)
;;          (string-match "150 Opening ASCII mode data connection" output))
;;       (ftp-wait process))
    (unless (or (string-match "^Not connected" output)
            (string-match "^220 Microsoft FTP Service" output)
            (string-match "^230 User brinkster/quizpark logged in" output)
            (string-match "^226 Transfer complete" output)
            (string-match "Connection closed by remote host\\.$" output)
            (string-match "421 Terminating connection\\." output)
            (string-match "^550" output))
      (ftp-wait process))))

(defun* ftp-send-command* (process command &optional argument-list (num-outputs 1)
				   (auto-relogin-p t) (timeout nil) (timeout-msecs 1000))
  "Sends COMMAND with optional ARGUMENT-LIST to running ftp PROCESS.
For TIMEOUT and TIMEOUT-MSECS, see `accept-process-output'. TODO: make
sure ftp process is logged in."
  ;;(when ensure-login-p (ftp-ensure-login process))
  (with-buffer (process-buffer process)
    (goto-char (point-max))
    (insert (format "%s\n" (upcase command)))
    (let* ((command-string (format "%s %s" command (concat* argument-list :in " ")))
	   (process-string (concat command-string "\n"))
	   (point (point-max)))
      (process-send-string process process-string)
      (ftp-wait process command-string 1)
      ;;
      (when (and auto-relogin-p
		 (ftp-disconnected-string-p (buffer-substring point (point))))
	(ftp-relogin process)
	(process-send-string process process-string)
	(ftp-wait process command))
      ;;
      (buffer-substring point (point)))))

(defun* ftp-send-command (process command &optional argument-list (num-outputs 1) 
				  (auto-relogin-p t) (timeout nil) (timeout-msecs 1000))
  "Sends COMMAND with optional ARGUMENT-LIST to running ftp PROCESS.
For TIMEOUT and TIMEOUT-MSECS, see `accept-process-output'. TODO: make
sure ftp process is logged in."
  (ftp-send-command* process command argument-list num-outputs auto-relogin-p timeout timeout-msecs))

(defun ftp-status (process)
  "Returns 'disconnected, 'connected, 'logged-in. Assumes no-on"
  (let ((return-string (ftp-send-command process "status" () 1 nil)))
    ;;(message return-string)
    (cond ((string-match "^Not connected" return-string) 'disconnected)
        ((string-match "use disconnect first" return-string) 'logged-out)
        ((string-match "^Connected to" return-string) 'connected)
        (t 'unknown))))

(defun ftp-disconnected-p (process) (eq (ftp-status process) 'disconnected))
(defun ftp-logged-out-p (process) (eq (ftp-status process) 'logged-out))

(defun* ftp-disconnect (process &optional (auto-relogin-p t))
  (ftp-send-command process "disconnect" (list (ftp-hostname process)) 1 auto-relogin-p))
(defun* ftp-open (process &optional (auto-relogin-p t)) 
  (ftp-send-command process "open" (list (ftp-hostname process)) 1 auto-relogin-p))
(defun* ftp-login (process &optional (auto-relogin-p t))
  (ftp-send-command process "user" (list (ftp-user process) (ftp-password process)) 1 auto-relogin-p))

(defun ftp-relogin (process)
  (ftp-disconnect process nil)
  (ftp-open process nil)
  (ftp-login process nil))

(defun* ftp-dir (process &optional (remote-path "./"))
  (when (string= remote-path "/") (setq remote-path "./"))
  (ftp-send-command process "dir" (list remote-path) 3)
  (with-buffer (process-buffer process)
    (goto-char (point-max))
    (and (re-search-backward "150 Opening.*\n\nW.*\n")
       (re-search-forward "--- .* ---\n")
       (buffer-substring (point)
                     (re-search-forward "\n$")))))
;;(ftp-dir (ftp-get-process "ftps8.brinkster.com") "mats")

(defvar ftp-dir-results () ;(nilf ftp-dir-results)
  "Stores DIR results. This is safe in this version, since we never
change working remote directory.")

(defconst ftp-dir-time-regexp
  "\\([0-9][0-9]\\)-\\([0-9][0-9]\\)-\\([0-9][0-9]\\)\\s-*\\([0-9][0-9]\\):\\([0-9][0-9]\\)\\([AP]M\\)")
(defconst ftp-dir-output-regexp
  (concat ftp-dir-time-regexp "\\s-*\\(\\([0-9]+\\)\\|\\(<DIR>\\)\\)\\s-*\\(.*\\)$"))
(setq sdir "01-18-05  03:33AM                  102 ftps8.brinkster.com.url")
(string-match* ftp-dir-output-regexp sdir '(1 2 3 4 5 6 7 10))

(defun ftp-parse-dir-output (line)
  (let ((res (string-match* ftp-dir-output-regexp line))
	(time (format ) ))))

(defun ftp-dir-list* (url &optional refresh)
  "Returns a list of directories"
  (when refresh (nilf ftp-dir-results))
  (or (second (assoc url ftp-dir-results))
      (let* ((res (ftp-dir (ftp-process url) (ftp-remote-path url)))
           (list (if (string-match "^550 .*: The system cannot find the file specified" res)
                 -1
                 (mapcar #'(lambda (x)
                         (if (string-match "<DIR>" x)
			   (concat (substring-intv x (interval-oo "<DIR>[ \t\n]*" "$")) "/")
			   (substring-intv x (interval-oo "M[ \t\n]*[0-9]*[ \t\n]*" "$"))))
                       (string-as-line-strings res)))))
      (push (list url list) ftp-dir-results)
      list)))
;;(ftp-dir-list* "ftps8.brinkster.com:" t)

(defun ftp-dir-alist (url)
  "Returns an alist of directories at remote URL"
  (let ((list (ftp-dir-list* url)))
    (and (listp list)
       (mapcar #'list list))))
;;(ftp-dir-alist "ftps8.brinkster.com:doc/referat")

(defun ftp-file-name-completion (string url)
  "FTP extension of `file-name-completion', which is a refinement of
`try-completion'."
  (let ((alist (ftp-dir-alist url)))
    (try-completion string alist)))
;;(ftp-file-name-completion "mats" "ftps8.brinkster.com:")

(defun* ftp-file-name-all-completions (string url)
  (all-completions string (ftp-dir-alist url)))

(defun* ftp-file-completion (string url)
  `(,(let* ((completion-ignore-case t)
         (hostname-and-path (ftp-parse-url url))
         (process (ftp-get-process (first hostname-and-path)))
         (remote-path (or (second hostname-and-path) ""))
         (remote-dir (file-name-directory remote-path)))
       (completing-read string (mapcar #'list (ftp-dir-list process remote-path)) nil nil))))
;;(ftp-file-completion "Find ftp file: " "ftps8.brinkster.com:")

(defun ftp-get (process path local-path) (ftp-send-command process "get" (list path local-path) 5))
(defun ftp-put (process local-path path) (ftp-send-command process "put" (list local-path path)))
(defun ftp-put-1 (process local-path) (ftp-send-command process "put" (list local-path)))

(defun ftp-find-file* (process path)
  (ftp-get process path mb-ftp-tmp-file)
  (let ((url (ftp-url process path))
      (buffer-name (file-name-nondirectory path)))
    (switch-to-buffer (generate-new-buffer-name buffer-name))
    (insert-file mb-ftp-tmp-file)
    (setq ftp-default-directory (file-name-directory url))
    (ftp-set-auto-mode buffer-name)
    (set (make-local-variable 'ftp-url) url)
    (not-modified)))

(defun ftp-find-file (url)
  "TODO: check login, otherwise we risk waiting in wain by giving the
3 arg. Check buffer is not already open"
  (interactive `(,(let ((completion-ignore-case t)) (ftp-read-file-name))))
  (if (ftp-url-p url)
    (let ((hostname-and-path (ftp-parse-url url)))
      (ftp-find-file* (ftp-get-process (first hostname-and-path)) (second hostname-and-path)))
    (find-file url)))
;;(ftp-find-file "ftps8.brinkster.com:mats/ftp-completions.el")

(defun ftp-save-buffer ()
  "TODO: see ftp-find-file"
  (interactive "")
  (if (local-variable-p 'ftp-url)
    (when (buffer-modified-p)
      (let ((hostname-and-path (ftp-parse-url ftp-url))
          (local-filename buffer-file-name))
      (write-file-silent mb-ftp-tmp-file)
      (ftp-put (ftp-get-process (first hostname-and-path))
             mb-ftp-tmp-file
             (second hostname-and-path))
      (not-modified)))
    (save-buffer)))

(defun ftp-write-file (url)
  "Writes file to URL. For prefix argument, see `ftp-read-file-name'."
  (interactive `(,(let ((completion-ignore-case t)) (ftp-read-file-name))))
  (if (ftp-url-p url)
    ;;ftp magic
    (let* ((hostname-and-path (ftp-parse-url url))
        (local-filename buffer-file-name)
        (remote-path (second hostname-and-path)))
      (when (ftp-file-directory-p remote-path)
	(setq remote-path (concat remote-path buffer-file-name)))
      (message "Saving buffer %s to %s" (buffer-name) remote-path)
      (ftp-put (ftp-get-process (first hostname-and-path)) (buffer-file-name) remote-path)
      (not-modified))
    ;;else the usual boring stuff
    (write-file url)))

(provide 'mb-ftp)

;;; Some utils that should have been standard elisp stuff
(defun write-file-interactive ()
  (list (if buffer-file-name
        (read-file-name "Write file: " nil nil nil nil)
        (read-file-name "Write file: " default-directory
                    (expand-file-name (file-name-nondirectory (buffer-name)) default-directory)
                    nil nil))
      (not current-prefix-arg)))

(defun write-file-silent (filename)
  "Like `write-file' but does not change current buffer. TODO: move
this to mb-utils."
  (write-region (point-min) (point-max) filename))
;;(write-file-silent "c:/unix/emacs-21.2/site-lisp/mb-lisp/ftp/qwe")
