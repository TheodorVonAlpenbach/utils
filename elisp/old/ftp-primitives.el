(defun ftp-set-auto-mode (buffer-name)
  "TODO: make this a general util"
  (funcall (first (loop for item in auto-mode-alist
			for regexp = (car item)
			for mode = (cdr item)
			if (string-match regexp buffer-name) collect mode))))

(defmacro* push-unique (x place &optional test)
  "Like `push', but pushes X on PLACE only if PLACE does not contain X."
  `(if (member* ,x ,place :test ,(or test `#'eql))
     ,place
     ,(if (symbolp place)
      `(setq ,place (cons ,x ,place))
      `(callf2 cons ,x ,place))))
;;(push-unique (1 1) #'equal)

(defconst ftp-handler-path-regexp (cons "ftp.*brinkster[^:]*:" #'ftp-hook-function)
  "If a path argument matches this regexp, then the ftp handler will
be invoked for 'find-file")

(push-unique ftp-handler-path-regexp file-name-handler-alist)
;;(find-file "ftps8.brinkster.com:mats/old.el")

(defun ftp-hook-function (operation &rest args)
  "Primitives that should be unhandled are: `substitute-in-file-name',
`file-name-nondirectory', `file-name-directory',
`directory-file-name', file-name-all-completions, file-name-completion"
  (cond 
   ((eq operation 'file-writable-p) (apply #'ftp-file-writable-p args))
   ((eq operation 'file-name-completion) (apply #'ftp-file-name-completion args))
   ((eq operation 'file-name-all-completions) (apply #'ftp-file-name-all-completions args))
   ((eq operation 'file-attributes) (apply #'ftp-file-attributes args))
   ((eq operation 'directory-file-name) (apply #'ftp-directory-file-name args))
   ((eq operation 'file-name-directory) (apply #'ftp-file-name-directory args))
   ((eq operation 'file-name-nondirectory) (apply #'ftp-file-name-nondirectory args))
   ((eq operation 'expand-file-name) (apply #'ftp-expand-file-name args))
   ((eq operation 'file-exists-p) (apply #'ftp-file-exists-p args))
   ((eq operation 'file-directory-p) (apply #'ftp-file-directory-p args))
   ((eq operation 'file-readable-p) (apply #'ftp-file-readable-p args))
   ((eq operation 'insert-file-contents) (apply #'ftp-insert-file-contents args))
   (t (message "Primitive `(%s %s)'" operation (concat* args :in " " :key #'(lambda (x) (format "%S" x))))
      (ftp-run-real-handler operation args))))

(defun ftp-write-region (url start end filename &optional append visit lockname mustbenew)
  (when (or append visit lockname mustbenew) 
    (error "ftp-write-region doesn't support optional parameters")))

(defun ftp-file-writable-p (url)
  (eq (sref (ninth (file-attributes url)) 2) ?w))
;;(file-writable-p "ftps8.brinkster.com:mats/ftp-completions.el")

(defun ftp-find-file (url &optional wildcards)
  "TODO: check login, otherwise we risk waiting in wain by giving the
3 arg. Check buffer is not already open"
  (interactive `(,(let ((completion-ignore-case t)) (ftp-read-file-name))))
  (if (ftp-url-p url)
    (let ((hostname-and-path (ftp-parse-url url)))
      (ftp-find-file* (ftp-get-process (first hostname-and-path)) (second hostname-and-path)))
    (find-file url wildcards)))
;;(find-file "ftps8.brinkster.com:mats/ftp-completions.el")

(defun ftp-file-name-directory (url)
  (concat (ftp-domain url) ":" (file-name-directory (ftp-remote-path url))))
;;(file-name-directory "ftps8.brinkster.com:mats")
;;(file-name-directory "t")

(defun ftp-file-name-nondirectory (url)
  (file-name-nondirectory (ftp-remote-path url)))
;;(file-name-nondirectory "ftps8.brinkster.com:mats")

(defun ftp-directory-file-name (url)
  (concat (ftp-domain url) ":" (directory-file-name (ftp-remote-path url))))
;;(directory-file-name "ftps8.brinkster.com:mats/mats/")

(defun ftp-real-expand-file-name (name &optional default-directory)
  (ftp-run-real-handler 'expand-file-name name default-directory))

(defun ftp-run-real-handler (operation args)
  ""
  (let ((inhibit-file-name-handlers
	 (cons #'ftp-hook-function
	       (cons #'ftp-completion-hook-function
		     (and (eq inhibit-file-name-operation operation)
			  inhibit-file-name-handlers))))
	(inhibit-file-name-operation operation))
    (apply operation args)))

(defun ftp-expand-file-name (name &optional default-directory) 
  name)

(defun ftp-file-exists-p (url)
  (not-nil-p (ftp-path-info url)))
;;(file-exists-p "ftps8.brinkster.com:mats/RCS/ftp-completions.el,v")
;;(find-file "ftps8.brinkster.com:mats/ftp-completions.el")

(defun ftp-file-directory-p (url)
  (and (ftp-file-exists-p url)
       (nil-p (third (ftp-path-info url)))))
;;(file-directory-p "ftps8.brinkster.com:mats/ftp-completions.el")

(defun ftp-file-attributes (url)
  "Returns non-nil iff URL exists and is not a directory."
  (let* ((res (ftp-path-info url))
	 (time (time-encode (second res)))
	 (file-modes (if (third res) "-rw-rw-rw-" "drw-rw-rw-")))
    (and (not (eq res -1)) ;; URL does not exists, returns nil
	 (list (eql (third res) t) 1 0 0 time time time (or (third res) 0) file-modes nil 0 0))))
;;(list (ftp-) 1 0 0 (16919 50415) (16920 51297) (16920 42124) 6805 "-rw-rw-rw-" nil 0 0))
;;(file-attributes "ftps8.brinkster.com:mats/")
;;(file-attributes "c:/unix/emacs-21.2/site-lisp/mb-lisp/ftp/")

(defun ftp-insert-file-contents (url &optional visit beg end replace)
  (ftp-get (ftp-process url) (ftp-remote-path url) mb-ftp-tmp-file)
  (let ((buffer-file-name-exists-p buffer-file-name)
	(bytes (second (insert-file-contents mb-ftp-tmp-file visit beg end replace))))
    (or buffer-file-name-exists-p (setq buffer-file-name url))
    (delete-file mb-ftp-tmp-file)
    (list url bytes)))
;;(ftp-insert-file-contents "ftps8.brinkster.com:mats/old.el" nil 1 10 nil)
;;(insert-file-contents "ftps8.brinkster.com:mats/old.el" nil 1 10 nil)

(defun ftp-file-readable-p (url)
  "TODO: add mode stuff"
  (file-exists-p url))

(file-local-copy "c:/cygwin/usr/libs/emacs-21.3/site-lisp/mb-lisp/ftp/mb-ftp.el")

;;;; ange implemented 32+2 primitives of a total of 46

;;; ANGE
;;backup-buffer U
;;copy-file
;;delete-directory
;;delete-file
;;directory-file-name
;;directory-files
;;dired-compress-file
;;dired-uncache
;;expand-file-name
;;file-attributes
;;file-directory-p
;;file-executable-p
;;file-exists-p
;;file-local-copy
;;file-name-all-completions
;;file-name-as-directory
;;file-name-completion
;;file-name-directory
;;file-name-nondirectory
;;file-name-sans-versions
;;file-newer-than-file-p
;;file-readable-p
;;file-symlink-p
;;file-writable-p
;;find-backup-file-name
;;insert-directory
;;insert-file-contents
;;load
;;make-directory
;;read-file-name-internal U
;;rename-file
;;unhandled-file-name-directory
;;verify-visited-file-modtime
;;write-region

;;; ALL
;;add-name-to-file U ok, symbolic links stuff
;;copy-file
;;delete-directory
;;delete-file
;;diff-latest-backup-file U doesn't exist anymore
;;directory-file-name
;;directory-files
;;dired-call-process U doesn't exist anymore
;;dired-compress-file
;;dired-uncache
;;expand-file-name
;;file-accessible-directory-p U shouldn't this be implemented
;;file-attributes
;;file-directory-p
;;file-executable-p
;;file-exists-p
;;file-local-copy
;;file-modes U ok, modes are not FTP
;;file-name-all-completions
;;file-name-as-directory
;;file-name-completion
;;file-name-directory
;;file-name-nondirectory
;;file-name-sans-versions
;;file-newer-than-file-p
;;file-ownership-preserved-p U ok, modes are not FTP
;;file-readable-p
;;file-regular-p U ok, there are probably no non-regular FTP files, whatever be such a file
;;file-symlink-p
;;file-truename U ok, symbolic links stuff
;;file-writable-p
;;find-backup-file-name
;;get-file-buffer U strange, we'll try implement this
;;insert-directory
;;insert-file-contents
;;load
;;make-directory
;;make-symbolic-link U ok, symbolic links stuff
;;rename-file
;;set-file-modes U ok, modes
;;set-visited-file-modtime U
;;shell-command U
;;unhandled-file-name-directory
;;vc-registered U
;;verify-visited-file-modtime
;;write-region

(provide 'ftp-primitives)

;; `expand-file-name' `file-attributes' `file-exists-p'
;; `file-name-directory' `file-name-nondirectory'
;; `file-name-sans-versions' `file-newer-than-file-p' `file-writable-p'
;; `vc-registered'
