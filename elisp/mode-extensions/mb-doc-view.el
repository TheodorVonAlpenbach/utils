;;; mb-doc-view.el --- Personal DocView navigation -*- lexical-binding: t; -*-

;;; Code:

(require 'doc-view)
(require 'image-mode)

(defun doc-view-end-of-document ()
  "Move to the bottom of the final document page."
  (interactive)
  (doc-view-last-page)
  (image-eob))

(defun doc-view-beginning-of-document ()
  "Move to the top of the first document page."
  (interactive)
  (doc-view-first-page)
  (image-bob))

(define-key doc-view-mode-map
            (kbd "C-M->")
            #'doc-view-end-of-document)
(define-key doc-view-mode-map
            (kbd "C-M-<")
            #'doc-view-beginning-of-document)

(provide 'mb-doc-view)

;;; mb-doc-view.el ends here
