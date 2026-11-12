;;; mb-tex.el --- Personal AUCTeX extensions -*- lexical-binding: t; -*-

;;; Code:

(require 'cl-lib)
(require 'latex)
(require 'reftex)

(defconst mb-tex--viewer-expansion
  '("%V" (lambda () "mb-tex-open-pdf")))

(defun mb-tex-master-prefix ()
  "Return the absolute path of the current TeX master without extension."
  (expand-file-name
   (file-name-sans-extension (TeX-master-file))
   (TeX-master-directory)))

(defun mb-tex--display-pdf (path)
  "Display and refresh the PDF at PATH when it exists."
  (when (file-exists-p path)
    (switch-to-buffer-other-window (find-file-noselect path t))
    (revert-buffer t t t)))

(defun mb-tex-open-pdf (&optional prefix)
  "Display the PDF for PREFIX, the active region, or the TeX master.
PREFIX is an extensionless path.  When the last AUCTeX command
operated on a region, use its region PDF instead of the master."
  (cond
   (prefix
    (mb-tex--display-pdf (concat prefix ".pdf")))
   (TeX-current-process-region-p
    (mb-tex--display-pdf (TeX-region-file "pdf")))
   (t
    (let* ((master-prefix (mb-tex-master-prefix))
	   (tex-path (concat master-prefix ".tex"))
	   (pdf-path (concat master-prefix ".pdf")))
      (if (file-newer-than-file-p tex-path pdf-path)
	  (message "Sentinel called too early!")
	(mb-tex--display-pdf pdf-path))))))

(cl-defun mode-help-follow-mode-advice ()
  (save-excursion
    (other-window 1)
    (re-search-forward ":override advice: " nil t)
    (forward-char 1)
    (push-button)))

(defun mb-tex-setup ()
  "Activate personal AUCTeX and RefTeX configuration."
  (setq TeX-auto-save t
	TeX-parse-self t
	TeX-electric-math '(?$ . ?$)
	reftex-plug-into-AUCTeX t
	reftex-insert-label-flags '("s" "sfte"))
  (setq-default TeX-master nil)
  (add-hook 'LaTeX-mode-hook #'turn-on-reftex)
  (cl-pushnew mb-tex--viewer-expansion TeX-expand-list :test #'equal)
  (advice-add #'describe-mode :after #'mode-help-follow-mode-advice))

(provide 'mb-tex)

;;; mb-tex.el ends here
