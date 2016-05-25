;; Put these two lines in .emacs:
;;(add-to-list 'load-path (concat *site-lisp-dir* "geben-0.26/"))
;;(autoload 'geben "geben" "PHP Debugger on Emacs" t) ;;see ../geben*

;; add some find-file-at-point magic for geben output
(defun ffap-php-geben (name)
  (replace-regexp-in-string "/mnt/shared/" "/cygdrive/c/Users/mat_ber/Documents/WinShare/" name))
(push '("/mnt/shared/" . ffap-php-geben) ffap-alist)

(defun mb-geben-on-file-visit (session &optional buffer)
  (message "MB: visited buffer %s" (buffer-name buffer))
  (evil-emacs-state 1))
(setf geben-source-visit-hook 'mb-geben-on-file-visit)
;;(setf geben-source-visit-hook nil)

(defun get-buffer-regexp (regexp)
  "Similar to `get-buffer', but finds buffer by matching regular
  expressin REGEXP with buffer name"
  (find regexp (buffer-list) :key #'buffer-name :test #'string-match))
;;(get-buffer-regexp "GEBEN.+process")

(defun mb-geben-get-process-buffer ()
  "Returns the current geben process buffer. Note! The method
  cannot handle multiple geben processes."
  (get-buffer-regexp "GEBEN.+process"))
;;(mb-geben-get-process-buffer)

(defun mb-geben-current-session ()
  "Returns the current geben session. Note! The method
  cannot handle multiple geben processes."
  (with-buffer (mb-geben-get-process-buffer)
    geben-current-session))
;;(mb-geben-current-session)

(defun mb-geben-set-breakpoint-line ()
  "Sets breakpoint at current line, even if the current file has not yet been loaded by geben"
  (interactive)
  (let* ((fileuri (replace-regexp-in-string "/cygdrive/c/Users/mat_ber/Documents/WinShare/" "file:///mnt/shared/" (buffer-file-name)))
	 (lineno (line-number-at-pos)))
    (let ((geben-current-session (mb-geben-current-session)))
      (geben-set-breakpoint-line fileuri lineno))))
;;(mb-geben-set-breakpoint-line)

(provide 'mb-geben)
