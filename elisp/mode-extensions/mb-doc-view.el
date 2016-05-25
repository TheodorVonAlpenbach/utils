(defun doc-view-end-of-document ()
  (interactive)
  (doc-view-last-page)
  (image-eob))

(defun doc-view-beginning-of-document ()
  (interactive)
  (doc-view-first-page)
  (image-bob))

(defun doc-view-goto-page ())

;;; Additional key bindings
(define-key doc-view-mode-map (kbd "C-M->") #'doc-view-end-of-document)
(define-key doc-view-mode-map (kbd "C-M-<") #'doc-view-beginning-of-document)
