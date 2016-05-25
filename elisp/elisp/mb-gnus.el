(require 'gnus)
(require 'gnus-art)
(require 'mb-utils-file)

(defun gnus-win32-attachment-press-button ()
  "Invoke this method when point is on HANDLE's region, then win32's
`open' is invoked on the HANDLE file. Typically, HANDLE points to a
mail attachment. If prefix is given, the method just prompts to save
attachment as file."
  (interactive)
  (let* ((handle (get-text-property (point) 'gnus-data))
	 (filename (gnus-mb-get-part-filename handle))
	 (media-type (mm-handle-media-type handle)))

    ;;(pr handle)
    (if current-prefix-arg 
      ;; just `save as...'
      (mm-save-part handle)
      ;; else attempt to invoke a program on attachment
      (progn ;; special attachments
	(cond ((and (string= media-type "application/msword")
		    (string= (file-extension filename) ""))
	       (setq filename (concat filename ".doc"))))
	(mm-save-part-to-file handle filename)
	(condition-case nil
	    ;; first try to open with DOS command `open'
	    (w32-shell-execute "open" filename)
	  (error (condition-case nil
		     ;; then try to open in Emacs
		     (find-file-read-only filename)
		   (error (condition-case nil
			      ;; finally offer to save as
			      (copy-file filename (read-file-name "Save attachment as: "))
			    (error "Couldn't open file %s!" filename)))))))))) 

(defun qwe ()
  (read-file-name "Save attachment as: ")
  (message* "Saved file as %s" fn))

(defun gnus-mb-get-part-filename (handle)
  "Used by gnus-win32-attachment-press-button."
  (let* ((filename (mail-content-type-get (mm-handle-disposition handle) 'filename)))
    (string-replace-map
	(expand-file-name
	 (or (when filename (file-name-nondirectory filename))
	     (mail-content-type-get (mm-handle-type handle) 'name)
	     "qwe.html")
	 "c:/WINNT/Temp/")
      (assoc-project *iso-latin1-encoding* 0 1))))

(defun message-tab (refresh)
  "Expand group names in Newsgroups and Followup-To headers.
Do a `tab-to-tab-stop' if not in those headers."
  (interactive "P")
  (if (let ((mail-abbrev-mode-regexp message-newgroups-header-regexp))
	(mail-abbrev-in-expansion-header-p))
    (message-expand-group)
    (if (let ((mail-abbrev-mode-regexp "^\\(Bcc\\|From\\|To\\|Cc\\):"))
	  (mail-abbrev-in-expansion-header-p))
      (insert (mb-query-address nil refresh))
      (tab-to-tab-stop))))
;;(mb-query-address)

;; changed key map
(define-key gnus-mime-button-map "\r" #'gnus-win32-attachment-press-button)

(provide 'mb-gnus)
