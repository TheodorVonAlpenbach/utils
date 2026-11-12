;;; mb-tex.el --- Personal AUCTeX extensions -*- lexical-binding: t; -*-

;;; Code:

(require 'cl-lib)
(require 'latex)
(require 'reftex)

(defconst mb-tex--viewer-expansion
  '("%V" (lambda () "open-my-pdf")))

(cl-defun master-prefix  ()
  (expand-file-name
   (file-name-sans-extension (TeX-master-file))
   (TeX-master-directory)))
;;(master-prefix)

(cl-defun open-my-pdf (&optional x)
  "Find master file and view the corresponding PDF file in a buffer.
Need to include this function in some view parameter. Check this
at work."
  (if (and x (file-exists-p (concat x ".pdf")))
    (progn (switch-to-buffer-other-window
	    (find-file-noselect (concat x ".pdf") t))
	   (revert-buffer t t t))
    (let* ((master-prefix (master-prefix))
	   (tex-path (concat master-prefix ".tex"))
	   (pdf-path (concat master-prefix ".pdf")))
      (if (eql (cadr (backtrace-frame 19)) 'TeX-command-run-all-region)
	(when (file-exists-p "_region.pdf_")
	  (switch-to-buffer-other-window (find-file-noselect "_region.pdf_" t))
	  (revert-buffer t t t))
	(if (file-newer-than-file-p tex-path pdf-path)
	  (message "Sentinel called too early!")
	  (when (file-exists-p pdf-path)
	    (switch-to-buffer-other-window (find-file-noselect pdf-path t))
	    (revert-buffer t t t)))))))

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
