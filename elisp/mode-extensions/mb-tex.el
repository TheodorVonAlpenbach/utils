(load "auctex.el" nil t t)
(load "preview-latex.el" nil t t)
(require 'latex)

(require 'reftex)

;;; LaTeX
(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq-default TeX-master nil)

;;; AUCTeX
(setq TeX-electric-math '(?$ . ?$))

;; reftex
(add-hook 'LaTeX-mode-hook 'turn-on-reftex)   ; with AUCTeX LaTeX mode
;;(add-hook 'latex-mode-hook 'turn-on-reftex)   ; with Emacs latex mode
(setf reftex-plug-into-AUCTeX t)
(setf reftex-insert-label-flags '("s" "sfte"))


(defun master-prefix ()
  (expand-file-name
   (file-name-sans-extension (TeX-master-file))
   (TeX-master-directory)))

(defun open-my-pdf (&optional x)
  "Find master file and view the corresponding PDF file in a buffer.
Need to include this function in some view parameter. Check this
at work."
  (let* ((master-prefix (master-prefix))
	 (tex-path (concat master-prefix ".tex"))
	 (pdf-path (concat master-prefix ".pdf")))
    (if (file-newer-than-file-p tex-path pdf-path)
      (message "Sentinel called too early!")
      (when (file-exists-p pdf-path)
	(switch-to-buffer-other-window (find-file-noselect pdf-path t))
	(revert-buffer t t t)))))

;;; Add this viewer to TeX-expand-list. (Check if lambda is necessary.)
(pushnew '("%V" (lambda () "open-my-pdf")) TeX-expand-list :test #'equal)
;;(nilf TeX-expand-list)

(defun mb-open-pdf (fn)
  "First version of 'open pdf'"
  (warn "This function is deprecated")
  (find-file-other-window (format "%s.pdf" fn) t))

(defun mb-describe-mode (&optional buffer)
  (interactive "@")
  (describe-mode buffer)
  (save-excursion
    (other-window 1)
    (re-search-forward ":override advice: ")
    (forward-char 1)
    (push-button)))

(defun mode-help-follow-mode-advice ()
  (save-excursion
    (other-window 1)
    (re-search-forward ":override advice: ")
    (forward-char 1)
    (push-button)))

(advice-add #'describe-mode :after #'mode-help-follow-mode-advice)
;;(advice-remove #'describe-mode #'mode-help-follow-mode-advice)

(provide 'mb-tex)
