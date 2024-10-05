(defvar *meta-compilation-buffer-name* "*Metafont-compilation*")

(cl-defun gftodvi (filename)
  (let ((default-directory (file-name-directory filename)))
    ;; Iff gftodvi is successful call-process returns 0
    (when (zerop (call-process "gftodvi" nil "*GFtoDVI-compilation*" nil filename))
      (file-name-change-extension filename "dvi"))))
;;(gftodvi "~/projects/mf/alberti/A.600gf")


(cl-defun meta-compilation-buffer ()
  (get-buffer *meta-compilation-buffer-name*))
;;(display-buffer (meta-compilation-buffer))

(cl-defun meta-check-compilation ()
  (with-buffer (meta-compilation-buffer)
    (save-excursion
      (let ((end (eob)))
	(if (re-search-backward "This is METAFONT" nil t)
	  (string-match* "Output written on \\([^ ]*\\)"
	    (buffer-substring-no-properties (point) end) :num 1)
	  ;; Now warn returns T, so we have to negated
	  (not (warn "METAFONT is not running")))))
    ))
;;(meta-check-compilation)

(cl-defun meta-compile-file (filename &optional (show-buffer-p t))
  (call-process "mf" nil "*Metafont-compilation*" nil
		filename)
		;; (format "\\mode=ljfour; mode_setup; input %s" filename)
  (prog1 (meta-check-compilation)
    (when show-buffer-p
      (switch-to-buffer-other-window (meta-compilation-buffer))
      (eob)
      (other-window 1))))
;;(meta-compile-file "~/projects/mf/alberti/A.mf")

(cl-defun meta-eval-buffer (&optional buffer)
  (with-buffer (or buffer (current-buffer))
    (save-buffer)
    (aif (meta-compile-file (buffer-file-name) nil)
      (let ((dir (file-name-directory (buffer-file-name))))
	(awhen (gftodvi (expand-file-name it dir))
	  (awhen (get-file-buffer it) (kill-buffer it))
	  (find-file-other-window it)
	  (other-window 1)))
      (switch-to-buffer-other-window (meta-compilation-buffer)))))
;;(meta-eval-buffer (get-buffer "A.mf"))

(provide 'mb-metafont)
