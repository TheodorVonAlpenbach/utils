;; This sec is "copied" from dict, maybe a macro later?
(defvar *lynx-favorites* ())
(defconst *lynx-favorites-path* (expand-file-name *lynx-favorites-file* *lynx-proxy-dir*))

(cl-defun lynx-add-favorite (&optional (url *lynx-current-url*))
  "Add an url to favorites. Default is current url."
  (interactive)
  (when (y-or-n-p (format "Add %s to favorites? " *lynx-current-url*))
    (when (not (member* url *lynx-favorites* :test #'string=))
      (setq *lynx-favorites* (nconc *lynx-favorites* (list url))))
    (lynx-print-favorites)))

(defun lynx-goto-favorite (&optional n)
  "Goes to first favorite in *lynx-favorites* or to the N-th if
prefix argument is given."
  (interactive "p")
  (lynx-browse (nth (1- n) *lynx-favorites*)))

(cl-defun lynx-delete-favorite (&optional n)
  "Deletes first favorite in *lynx-favorites* or the N-th if prefix
argument is given."
  (interactive "p")
  (if *lynx-favorites*
    (when (y-or-n-p (format "Delete %s from favorites? "
			    (nth (1- n) *lynx-favorites*)))
      (setq *lynx-favorites* (remove-nth (1- n) *lynx-favorites*))
      (lynx-print-favorites))
    (message "No current favorites")))

(defun lynx-print-favorites ()
  "Prints favorite urls in temporary buffer *lynx-info*."
  (interactive)
  (with-output-to-temp-buffer "*lynx-info*"
    (loop for f in *lynx-favorites*
	  for i from 1 do
	  (princ (format "%d:  %s\n" i f))))
  (message "Invoke lynx-goto-favorite ARG to enter an url."))

(defun lynx-save-favorites () (print* *lynx-favorites* *lynx-favorites-path*))
(defun lynx-read-favorites () (setq *lynx-favorites* (read* *lynx-favorites-path*)))

(unless *lynx-favorites* (lynx-read-favorites)) ;(nil! *lynx-favorites*)
(add-hook 'kill-emacs-hook 'lynx-save-favorites)
(push (cons *lynx-favorites-path* 'iso-8859-1) file-coding-system-alist)

(provide 'lynx-favorites)
